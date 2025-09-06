//! AST to HIR lowering pass
//!
//! This module provides the lowering pass that transforms AST nodes into HIR nodes.
//! The lowering process:
//! 1. Assigns stable NodeIds to all HIR nodes
//! 2. Preserves span information through a span mapping table
//! 3. Normalizes AST structures to be more type-checker friendly
//! 4. Converts string-based references to a form ready for symbol resolution

use crate::ids::{IdGenerator, ModuleId, NodeId};
use crate::nodes::*;
use veil_ast::{self as ast};

/// Context for lowering AST to HIR
pub struct LoweringContext {
    id_gen: IdGenerator,
    span_map: SpanMap,
    _module_id: ModuleId,
}

impl LoweringContext {
    pub fn new(module_id: ModuleId) -> Self {
        Self {
            id_gen: IdGenerator::new(),
            span_map: SpanMap::new(),
            _module_id: module_id,
        }
    }

    fn next_node_id(&mut self) -> NodeId {
        self.id_gen.next_node_id()
    }

    fn record_span(&mut self, node_id: NodeId, span: codespan::Span) {
        self.span_map.insert(node_id, span);
    }

    fn with_span<T>(&mut self, span: codespan::Span, f: impl FnOnce(&mut Self, NodeId) -> T) -> T {
        let node_id = self.next_node_id();
        self.record_span(node_id, span);
        f(self, node_id)
    }
}

/// Lower an AST program to HIR
pub fn lower_program(
    ast_program: &ast::Program,
    module_id: ModuleId,
) -> Result<HirProgram, LoweringError> {
    let mut ctx = LoweringContext::new(module_id);

    let mut items = Vec::new();

    // Lower imports
    for import in &ast_program.imports {
        let hir_import = ctx.lower_import(import)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::Import(hir_import),
            visibility: HirVisibility::Private, // Imports are always private
        });
    }

    // Lower functions
    for function in &ast_program.functions {
        let hir_function = ctx.lower_function(function)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::Function(hir_function),
            visibility: ctx.lower_visibility(&function.visibility),
        });
    }

    // Lower structs
    for struct_def in &ast_program.structs {
        let hir_struct = ctx.lower_struct(struct_def)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::Struct(hir_struct),
            visibility: ctx.lower_visibility(&struct_def.visibility),
        });
    }

    // Lower enums
    for enum_def in &ast_program.enums {
        let hir_enum = ctx.lower_enum(enum_def)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::Enum(hir_enum),
            visibility: ctx.lower_visibility(&enum_def.visibility),
        });
    }

    // Lower impl blocks
    for impl_block in &ast_program.impls {
        let hir_impl = ctx.lower_impl(impl_block)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::Impl(hir_impl),
            visibility: HirVisibility::Private, // Impl blocks don't have visibility
        });
    }

    // Lower FFI functions
    for ffi_function in &ast_program.ffi_functions {
        let hir_ffi_function = ctx.lower_ffi_function(ffi_function)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::FfiFunction(hir_ffi_function),
            visibility: HirVisibility::Private, // FFI items need explicit pub
        });
    }

    // Lower FFI variables
    for ffi_variable in &ast_program.ffi_variables {
        let hir_ffi_variable = ctx.lower_ffi_variable(ffi_variable)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::FfiVariable(hir_ffi_variable),
            visibility: HirVisibility::Private, // FFI items need explicit pub
        });
    }

    // Lower tests
    for test in &ast_program.tests {
        let hir_test = ctx.lower_test(test)?;
        items.push(HirItem {
            id: ctx.next_node_id(),
            kind: HirItemKind::Test(hir_test),
            visibility: HirVisibility::Private, // Tests are always private
        });
    }

    Ok(HirProgram {
        module_id,
        items,
        span_map: ctx.span_map,
    })
}

impl LoweringContext {
    fn lower_visibility(&self, visibility: &ast::Visibility) -> HirVisibility {
        match visibility {
            ast::Visibility::Private => HirVisibility::Private,
            ast::Visibility::Internal => HirVisibility::PublicCrate,
            ast::Visibility::Public => HirVisibility::Public,
        }
    }

    fn lower_import(
        &mut self,
        import: &ast::ImportDeclaration,
    ) -> Result<HirImport, LoweringError> {
        // Handle different import declaration types
        let (path, alias, items) = match import {
            ast::ImportDeclaration::ImportAll {
                module_path, alias, ..
            } => (module_path.clone(), alias.clone(), None),
            ast::ImportDeclaration::ImportSpecifiers {
                module_path,
                specifiers,
                ..
            } => {
                let items = specifiers.iter().map(|s| s.name.clone()).collect();
                (module_path.clone(), None, Some(items))
            }
            ast::ImportDeclaration::ExportImportAll {
                module_path, alias, ..
            } => (module_path.clone(), alias.clone(), None),
            ast::ImportDeclaration::ExportImportSpecifiers {
                module_path,
                specifiers,
                ..
            } => {
                let items = specifiers.iter().map(|s| s.name.clone()).collect();
                (module_path.clone(), None, Some(items))
            }
        };

        Ok(HirImport {
            id: self.next_node_id(),
            path,
            resolved_module_id: None, // Will be filled during resolution
            alias,
            items,
            resolved_symbols: Vec::new(), // Will be filled during resolution
        })
    }

    fn lower_function(&mut self, function: &ast::Function) -> Result<HirFunction, LoweringError> {
        Ok(self.with_span(function.span, |ctx, id| {
            let params = function
                .params
                .iter()
                .map(|(name, ty)| {
                    let param_id = ctx.next_node_id();
                    HirParam {
                        id: param_id,
                        name: name.clone(),
                        symbol_id: None, // Will be filled during resolution
                        ty: ctx.lower_type(ty),
                    }
                })
                .collect();

            let body = ctx.lower_stmts_to_block(&function.body);

            let generic_params = function
                .generic_params
                .iter()
                .map(|p| HirGenericParam {
                    name: p.clone(),
                    bounds: vec![],
                })
                .collect();

            HirFunction {
                id,
                name: function.name.clone(),
                symbol_id: None, // Will be filled during resolution
                generic_params,
                params,
                return_type: ctx.lower_type(&function.return_type),
                body,
            }
        }))
    }

    fn lower_struct(&mut self, struct_def: &ast::StructDef) -> Result<HirStruct, LoweringError> {
        Ok(self.with_span(struct_def.span, |ctx, id| {
            let fields = struct_def
                .fields
                .iter()
                .map(|field| {
                    let field_id = ctx.next_node_id();
                    ctx.record_span(field_id, field.span);
                    HirStructField {
                        id: field_id,
                        name: field.name.clone(),
                        symbol_id: None, // Will be filled during resolution
                        ty: ctx.lower_type(&field.ty),
                        visibility: HirVisibility::Private, // Default to private, no field-level visibility in AST
                    }
                })
                .collect();

            let generic_params = struct_def
                .generic_params
                .iter()
                .map(|p| HirGenericParam {
                    name: p.clone(),
                    bounds: vec![],
                })
                .collect();

            HirStruct {
                id,
                name: struct_def.name.clone(),
                symbol_id: None, // Will be filled during resolution
                generic_params,
                fields,
                repr: struct_def.repr.clone(),
            }
        }))
    }

    fn lower_enum(&mut self, enum_def: &ast::EnumDef) -> Result<HirEnum, LoweringError> {
        Ok(self.with_span(enum_def.span, |ctx, id| {
            let variants = enum_def
                .variants
                .iter()
                .map(|variant| {
                    let variant_id = ctx.next_node_id();
                    ctx.record_span(variant_id, variant.span);

                    let data = variant.data.as_ref().map(|data| match data {
                        ast::EnumVariantData::Tuple(types) => HirEnumVariantData::Tuple(
                            types.iter().map(|ty| ctx.lower_type(ty)).collect(),
                        ),
                        ast::EnumVariantData::Struct(fields) => {
                            let hir_fields = fields
                                .iter()
                                .map(|field| {
                                    let field_id = ctx.next_node_id();
                                    ctx.record_span(field_id, field.span);
                                    HirStructField {
                                        id: field_id,
                                        name: field.name.clone(),
                                        symbol_id: None, // Will be filled during resolution
                                        ty: ctx.lower_type(&field.ty),
                                        visibility: HirVisibility::Private, // Default to private, no field-level visibility in AST
                                    }
                                })
                                .collect();
                            HirEnumVariantData::Struct(hir_fields)
                        }
                    });

                    HirEnumVariant {
                        id: variant_id,
                        name: variant.name.clone(),
                        symbol_id: None, // Will be filled during resolution
                        data,
                        discriminant: variant.value.map(|v| {
                            let expr_id = ctx.next_node_id();
                            HirExpr {
                                id: expr_id,
                                kind: Box::new(HirExprKind::Int(v as i64)),
                            }
                        }),
                    }
                })
                .collect();

            let generic_params = enum_def
                .generic_params
                .iter()
                .map(|p| HirGenericParam {
                    name: p.clone(),
                    bounds: vec![],
                })
                .collect();

            HirEnum {
                id,
                name: enum_def.name.clone(),
                symbol_id: None, // Will be filled during resolution
                generic_params,
                variants,
            }
        }))
    }

    fn lower_impl(&mut self, impl_block: &ast::ImplBlock) -> Result<HirImpl, LoweringError> {
        let methods = impl_block
            .methods
            .iter()
            .map(|method| self.lower_function(method))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(self.with_span(impl_block.span, |ctx, id| {
            let target_type = if let Some(parsed_type) = &impl_block.target_type_parsed {
                ctx.lower_type(parsed_type)
            } else {
                HirType::Unresolved(impl_block.target_type.clone())
            };

            let _trait_ref: Option<HirType> = None;

            HirImpl {
                id,
                target_type,
                trait_ref: None, // AST ImplBlock doesn't support trait implementations yet
                trait_symbol_id: None, // Will be filled during resolution
                methods,
            }
        }))
    }

    fn lower_ffi_function(
        &mut self,
        ffi_function: &ast::FfiFunction,
    ) -> Result<HirFfiFunction, LoweringError> {
        // FFI functions don't have spans in the current AST
        let ffi_id = self.next_node_id();
        Ok({
            let params = ffi_function
                .params
                .iter()
                .enumerate()
                .map(|(i, ty)| {
                    let param_id = self.next_node_id();
                    HirParam {
                        id: param_id,
                        name: format!("param_{}", i), // FFI functions don't have named params
                        symbol_id: None,              // Will be filled during resolution
                        ty: self.lower_type(ty),
                    }
                })
                .collect();

            HirFfiFunction {
                id: ffi_id,
                name: ffi_function.name.clone(),
                params,
                return_type: self.lower_type(&ffi_function.return_type),
                variadic: false,
            }
        })
    }

    fn lower_ffi_variable(
        &mut self,
        ffi_variable: &ast::FfiVariable,
    ) -> Result<HirFfiVariable, LoweringError> {
        // FFI variables don't have spans in the current AST
        Ok(HirFfiVariable {
            id: self.next_node_id(),
            name: ffi_variable.name.clone(),
            ty: self.lower_type(&ffi_variable.ty),
        })
    }

    fn lower_test(&mut self, test: &ast::Test) -> Result<HirTest, LoweringError> {
        Ok(self.with_span(test.span, |ctx, id| HirTest {
            id,
            name: test.name.clone(),
            body: ctx.lower_stmts_to_block(&test.stmts),
        }))
    }

    #[allow(clippy::only_used_in_recursion)]
    fn lower_type(&self, ast_type: &ast::Type) -> HirType {
        match ast_type {
            ast::Type::I8 => HirType::I8,
            ast::Type::I16 => HirType::I16,
            ast::Type::I32 => HirType::I32,
            ast::Type::I64 => HirType::I64,
            ast::Type::U8 => HirType::U8,
            ast::Type::U16 => HirType::U16,
            ast::Type::U32 => HirType::U32,
            ast::Type::U64 => HirType::U64,
            ast::Type::F32 => HirType::F32,
            ast::Type::F64 => HirType::F64,
            ast::Type::Bool => HirType::Bool,
            ast::Type::String => HirType::String,
            ast::Type::Void => HirType::Void,
            ast::Type::Pointer(inner) => HirType::Pointer(Box::new(self.lower_type(inner))),
            ast::Type::Array(inner) => HirType::Array(Box::new(self.lower_type(inner))),
            ast::Type::SizedArray(inner, size) => {
                HirType::SizedArray(Box::new(self.lower_type(inner)), *size)
            }
            ast::Type::Optional(inner) => HirType::Optional(Box::new(self.lower_type(inner))),
            ast::Type::Function(params, ret) => {
                let param_types = params.iter().map(|p| self.lower_type(p)).collect();
                HirType::Function(param_types, Box::new(self.lower_type(ret)))
            }
            ast::Type::Generic(name) => HirType::Generic(HirGenericRef::new(name.clone())),
            ast::Type::GenericInstance(name, args) => {
                let arg_types = args.iter().map(|arg| self.lower_type(arg)).collect();
                HirType::GenericInstance(HirGenericRef::new(name.clone()), arg_types)
            }
            ast::Type::Struct(name) => HirType::Unresolved(name.clone()),
            ast::Type::Enum(name) => HirType::Unresolved(name.clone()),
            ast::Type::RawPtr => HirType::Pointer(Box::new(HirType::Void)),
            ast::Type::CChar => HirType::I8,  // C char maps to i8
            ast::Type::CInt => HirType::I32,  // C int maps to i32
            ast::Type::CSize => HirType::U64, // C size_t maps to u64
            ast::Type::Unknown => HirType::Unknown,
            ast::Type::Any => HirType::Unresolved("any".to_string()),
            ast::Type::NoneType => HirType::Void,
            ast::Type::Ellipsis => HirType::Unresolved("...".to_string()),
        }
    }

    fn lower_stmts_to_block(&mut self, stmts: &[ast::Stmt]) -> HirBlock {
        let block_id = self.next_node_id();

        let mut hir_stmts = Vec::new();
        let mut last_expr = None;
        let mut has_return = false;

        // First pass: check if there's an explicit return statement
        for stmt in stmts {
            if matches!(stmt, ast::Stmt::Return(_, _)) {
                has_return = true;
                break;
            }
        }

        for stmt in stmts {
            match self.lower_stmt(stmt) {
                Ok(hir_stmt) => {
                    // Only treat the last expression as the block's value if:
                    // 1. It's the last statement
                    // 2. It's an expression statement
                    // 3. There's no explicit return statement in the block
                    if let HirStmtKind::Expr(ref expr) = hir_stmt.kind
                        && stmts.last().map(|s| std::ptr::eq(s, stmt)).unwrap_or(false)
                        && !has_return
                    {
                        last_expr = Some(Box::new(expr.clone()));
                        continue; // Don't add as statement if it's the last expression
                    }
                    hir_stmts.push(hir_stmt);
                }
                Err(err) => {
                    // Log lowering errors for diagnostics - in production this would
                    // be collected in a proper error reporting system
                    if std::env::var("VEIL_DEBUG_LOWERING").is_ok() {
                        eprintln!("Warning: Failed to lower statement: {:?}", err);
                    }
                    continue;
                }
            }
        }

        HirBlock {
            id: block_id,
            stmts: hir_stmts,
            expr: last_expr,
        }
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> Result<HirStmt, LoweringError> {
        let stmt_id = self.next_node_id();
        let span = self.get_stmt_span(stmt);
        self.record_span(stmt_id, span);

        let kind = match stmt {
            ast::Stmt::Expr(expr, _) => {
                // Special-case assignment expressions as statement-level assignments
                if let ast::Expr::Assign(lhs, rhs, _) = expr {
                    HirStmtKind::Assign {
                        lhs: self.lower_expr(lhs)?,
                        rhs: self.lower_expr(rhs)?,
                    }
                } else {
                    HirStmtKind::Expr(self.lower_expr(expr)?)
                }
            }

            ast::Stmt::Const(name, ty, value, _) => HirStmtKind::Const {
                name: name.clone(),
                ty: ty.as_ref().map(|t| self.lower_type(t)),
                init: self.lower_expr(value)?,
            },
            ast::Stmt::Var(name, ty, value, is_mutable, _) => HirStmtKind::Var {
                name: name.clone(),
                ty: ty.as_ref().map(|t| self.lower_type(t)),
                init: self.lower_expr(value)?,
                is_mutable: *is_mutable,
            },
            ast::Stmt::Return(expr, _) => {
                // Check if this is a void expression (empty return)
                match expr {
                    ast::Expr::Void(_) => HirStmtKind::Return(None),
                    _ => HirStmtKind::Return(Some(self.lower_expr(expr)?)),
                }
            }
            ast::Stmt::Break(expr, _) => {
                HirStmtKind::Break(expr.as_ref().map(|e| self.lower_expr(e)).transpose()?)
            }
            ast::Stmt::Continue(_) => HirStmtKind::Continue,
            // While statement: emit as expression node
            ast::Stmt::While(cond, body, _) => {
                // Create an expression node with the while construct
                let expr_id = self.next_node_id();
                self.record_span(expr_id, self.get_stmt_span(stmt));
                let while_expr = HirExpr {
                    id: expr_id,
                    kind: Box::new(HirExprKind::While {
                        condition: Box::new(self.lower_expr(cond)?),
                        body: self.lower_stmts_to_block(body),
                    }),
                };
                HirStmtKind::Expr(while_expr)
            }
            // Loop statement: emit as expression node
            ast::Stmt::Loop(body, _) => {
                let expr_id = self.next_node_id();
                self.record_span(expr_id, self.get_stmt_span(stmt));
                let loop_expr = HirExpr {
                    id: expr_id,
                    kind: Box::new(HirExprKind::Loop {
                        body: self.lower_stmts_to_block(body),
                    }),
                };
                HirStmtKind::Expr(loop_expr)
            }
            // For statement: lower pattern, iter expr, and body to a For expression
            ast::Stmt::For(var_name, _index_var, iter_expr, _step, body, _) => {
                // Build a simple variable pattern
                let pat_id = self.next_node_id();
                self.record_span(pat_id, self.get_stmt_span(stmt));
                let pattern = HirPattern {
                    id: pat_id,
                    kind: Box::new(HirPatternKind::Variable(var_name.clone())),
                };

                // Lower iterator expression and body
                let iter_hir = self.lower_expr(iter_expr)?;
                let body_hir = self.lower_stmts_to_block(body);

                // Wrap as an expression statement
                let expr_id = self.next_node_id();
                self.record_span(expr_id, self.get_stmt_span(stmt));
                let for_expr = HirExpr {
                    id: expr_id,
                    kind: Box::new(HirExprKind::For {
                        pattern,
                        iter: Box::new(iter_hir),
                        body: body_hir,
                    }),
                };
                HirStmtKind::Expr(for_expr)
            }
            _ => return Err(LoweringError::UnsupportedConstruct("statement".to_string())),
        };

        Ok(HirStmt { id: stmt_id, kind })
    }

    fn get_stmt_span(&self, stmt: &ast::Stmt) -> codespan::Span {
        match stmt {
            ast::Stmt::Const(_, _, _, span) => *span,
            ast::Stmt::Var(_, _, _, _, span) => *span,
            ast::Stmt::Expr(_, span) => *span,
            ast::Stmt::Return(_, span) => *span,
            ast::Stmt::Break(_, span) => *span,
            ast::Stmt::Continue(span) => *span,
            ast::Stmt::If(_, _, _, span) => *span,
            ast::Stmt::While(_, _, span) => *span,
            ast::Stmt::Loop(_, span) => *span,
            ast::Stmt::For(_, _, _, _, _, span) => *span,
            ast::Stmt::Defer(_, span) => *span,
            ast::Stmt::Block(_, span) => *span,
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> Result<HirExpr, LoweringError> {
        let expr_id = self.next_node_id();
        self.record_span(expr_id, expr.span());

        let kind = match expr {
            ast::Expr::Int(value, _) => HirExprKind::Int(*value as i64),
            ast::Expr::Int64(value, _) => HirExprKind::Int(*value),
            ast::Expr::F32(value, _) => HirExprKind::Float(*value as f64),
            ast::Expr::F64(value, _) => HirExprKind::Float(*value),
            ast::Expr::Bool(value, _) => HirExprKind::Bool(*value),
            ast::Expr::Str(value, _) => HirExprKind::String(value.clone()),
            ast::Expr::None(_) => HirExprKind::None,
            ast::Expr::Void(_) => HirExprKind::Void,

            ast::Expr::Var(name, _) => HirExprKind::Variable(name.clone()),

            ast::Expr::BinOp(left, op, right, _) => {
                // Handle pipeline operator specially
                if matches!(op, ast::BinOp::Pipeline) {
                    HirExprKind::Pipeline {
                        expr: Box::new(self.lower_expr(left)?),
                        func: Box::new(self.lower_expr(right)?),
                    }
                } else {
                    HirExprKind::Binary {
                        op: self.lower_binary_op(op),
                        lhs: Box::new(self.lower_expr(left)?),
                        rhs: Box::new(self.lower_expr(right)?),
                    }
                }
            }

            ast::Expr::UnaryOp(op, expr, _) => HirExprKind::Unary {
                op: self.lower_unary_op(op),
                expr: Box::new(self.lower_expr(expr)?),
            },

            ast::Expr::Call(func_name, args, _) => {
                let hir_args = args
                    .iter()
                    .map(|arg| self.lower_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                // Create a variable expression for the function name
                let func_expr = HirExpr {
                    id: self.next_node_id(),
                    kind: Box::new(HirExprKind::Variable(func_name.clone())),
                };

                HirExprKind::Call {
                    func: Box::new(func_expr),
                    args: hir_args,
                }
            }

            ast::Expr::FieldAccess(obj, field, _) => HirExprKind::FieldAccess {
                base: Box::new(self.lower_expr(obj)?),
                field: field.clone(),
            },

            ast::Expr::ArrayAccess(base, index, _) => HirExprKind::Index {
                base: Box::new(self.lower_expr(base)?),
                index: Box::new(self.lower_expr(index)?),
            },

            ast::Expr::Cast(expr, ty, _) => HirExprKind::Cast {
                expr: Box::new(self.lower_expr(expr)?),
                ty: self.lower_type(ty),
            },

            ast::Expr::ArrayInit(elements, _) => {
                let hir_elements = elements
                    .iter()
                    .map(|elem| self.lower_expr(elem))
                    .collect::<Result<Vec<_>, _>>()?;
                HirExprKind::ArrayLiteral(hir_elements)
            }

            ast::Expr::Match(expr, arms, _) => {
                let hir_arms = arms
                    .iter()
                    .map(|arm| self.lower_match_arm(arm))
                    .collect::<Result<Vec<_>, _>>()?;

                HirExprKind::Match {
                    expr: Box::new(self.lower_expr(expr)?),
                    arms: hir_arms,
                }
            }

            ast::Expr::StructInit(name, fields, _) => {
                let hir_fields = fields
                    .iter()
                    .map(|(field_name, field_expr)| {
                        Ok((field_name.clone(), self.lower_expr(field_expr)?))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                HirExprKind::StructLiteral {
                    name: name.clone(),
                    fields: hir_fields,
                }
            }

            ast::Expr::TemplateStr(parts, _) => {
                let hir_parts = parts
                    .iter()
                    .map(|part| match part {
                        ast::TemplateStrPart::Literal(s) => {
                            Ok(HirTemplateStringPart::String(s.clone()))
                        }
                        ast::TemplateStrPart::Expression(expr) => {
                            Ok(HirTemplateStringPart::Expr(self.lower_expr(expr)?))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                HirExprKind::Template { parts: hir_parts }
            }

            // If expression
            ast::Expr::If(cond, then_stmts, else_stmts, _) => HirExprKind::If {
                condition: Box::new(self.lower_expr(cond)?),
                then_branch: self.lower_stmts_to_block(then_stmts),
                else_branch: else_stmts.as_ref().map(|s| self.lower_stmts_to_block(s)),
            },
            // Loop expression
            ast::Expr::Loop(body, _) => HirExprKind::Loop {
                body: self.lower_stmts_to_block(body),
            },

            // Range expressions
            ast::Expr::Range(start, end, range_type, _) => {
                let op = match range_type {
                    ast::RangeType::Exclusive => HirBinaryOp::Range,
                    ast::RangeType::Inclusive => HirBinaryOp::RangeInclusive,
                    _ => HirBinaryOp::Range,
                };
                HirExprKind::Binary {
                    op,
                    lhs: Box::new(self.lower_expr(start)?),
                    rhs: Box::new(self.lower_expr(end)?),
                }
            }

            ast::Expr::InfiniteRange(range_type, _) => {
                // For infinite ranges, create unary operations or special patterns
                let none_expr_id = self.next_node_id();
                let none_expr = HirExpr {
                    id: none_expr_id,
                    kind: Box::new(HirExprKind::None),
                };
                match range_type {
                    ast::RangeType::InfiniteUp => {
                        // ..> becomes range from nothing to infinity
                        HirExprKind::Binary {
                            op: HirBinaryOp::RangeFrom,
                            lhs: Box::new(none_expr.clone()),
                            rhs: Box::new(none_expr),
                        }
                    }
                    ast::RangeType::InfiniteDown => {
                        // ..< becomes range from negative infinity to nothing
                        HirExprKind::Binary {
                            op: HirBinaryOp::RangeExclusive,
                            lhs: Box::new(none_expr.clone()),
                            rhs: Box::new(none_expr),
                        }
                    }
                    ast::RangeType::Infinite => {
                        // .. becomes full infinite range
                        HirExprKind::Binary {
                            op: HirBinaryOp::Range,
                            lhs: Box::new(none_expr.clone()),
                            rhs: Box::new(none_expr),
                        }
                    }
                    _ => {
                        // Fallback for any other range types
                        HirExprKind::Binary {
                            op: HirBinaryOp::Range,
                            lhs: Box::new(none_expr.clone()),
                            rhs: Box::new(none_expr),
                        }
                    }
                }
            }

            _ => {
                return Err(LoweringError::UnsupportedConstruct(
                    "expression".to_string(),
                ));
            }
        };

        Ok(HirExpr {
            id: expr_id,
            kind: Box::new(kind),
        })
    }

    fn lower_match_arm(&mut self, arm: &ast::MatchArm) -> Result<HirMatchArm, LoweringError> {
        let arm_id = self.next_node_id();
        self.record_span(arm_id, arm.span);

        let pattern = self.lower_pattern(&arm.pattern)?;
        let guard = arm.guard.as_ref().map(|g| self.lower_expr(g)).transpose()?;
        let body = match &arm.body {
            ast::MatchArmBody::Expr(expr) => HirMatchArmBody::Expr(self.lower_expr(expr)?),
            ast::MatchArmBody::Block(stmts) => {
                HirMatchArmBody::Block(self.lower_stmts_to_block(stmts))
            }
        };

        Ok(HirMatchArm {
            id: arm_id,
            pattern,
            guard,
            body,
        })
    }

    fn lower_pattern(&mut self, pattern: &ast::Pattern) -> Result<HirPattern, LoweringError> {
        let pattern_id = self.next_node_id();
        let span = match pattern {
            ast::Pattern::Wildcard(span) => *span,
            ast::Pattern::Variable(_, span) => *span,
            ast::Pattern::Literal(_, span) => *span,
            ast::Pattern::EnumVariant(_, _, _, span) => *span,
        };
        self.record_span(pattern_id, span);

        let kind = match pattern {
            ast::Pattern::Wildcard(_) => HirPatternKind::Wildcard,
            ast::Pattern::Variable(name, _) => HirPatternKind::Variable(name.clone()),
            ast::Pattern::Literal(expr, _) => {
                HirPatternKind::Literal(Box::new(self.lower_expr(expr)?))
            }
            ast::Pattern::EnumVariant(enum_name, variant_name, patterns, _) => {
                let hir_patterns = patterns
                    .iter()
                    .map(|p| self.lower_pattern(p))
                    .collect::<Result<Vec<_>, _>>()?;

                HirPatternKind::EnumVariant {
                    name: enum_name.clone(),
                    enum_symbol_id: None, // Will be filled during resolution
                    variant: variant_name.clone(),
                    variant_symbol_id: None, // Will be filled during resolution
                    patterns: hir_patterns,
                }
            }
        };

        Ok(HirPattern {
            id: pattern_id,
            kind: Box::new(kind),
        })
    }

    fn lower_binary_op(&self, op: &ast::BinOp) -> HirBinaryOp {
        match op {
            ast::BinOp::Add => HirBinaryOp::Add,
            ast::BinOp::Sub => HirBinaryOp::Sub,
            ast::BinOp::Mul => HirBinaryOp::Mul,
            ast::BinOp::Div => HirBinaryOp::Div,
            ast::BinOp::IDiv => HirBinaryOp::IDiv,
            ast::BinOp::Mod => HirBinaryOp::Mod,
            ast::BinOp::And => HirBinaryOp::And,
            ast::BinOp::Or => HirBinaryOp::Or,
            ast::BinOp::Eq => HirBinaryOp::Eq,
            ast::BinOp::NotEq => HirBinaryOp::Ne,
            ast::BinOp::Gt => HirBinaryOp::Gt,
            ast::BinOp::GtEq => HirBinaryOp::Ge,
            ast::BinOp::Lt => HirBinaryOp::Lt,
            ast::BinOp::LtEq => HirBinaryOp::Le,
            ast::BinOp::Pow => HirBinaryOp::Pow,
            ast::BinOp::Pow2 => HirBinaryOp::Pow, // Treat Pow2 same as Pow
            ast::BinOp::Range => HirBinaryOp::Range,
            ast::BinOp::RangeInclusive => HirBinaryOp::RangeInclusive,
            ast::BinOp::RangeFrom => HirBinaryOp::RangeFrom,
            ast::BinOp::RangeTo => HirBinaryOp::RangeExclusive,
            ast::BinOp::Pipeline => HirBinaryOp::Add, // Should not be called - handled specially above
        }
    }

    fn lower_unary_op(&self, op: &ast::UnOp) -> HirUnaryOp {
        match op {
            ast::UnOp::Not => HirUnaryOp::Not,
            ast::UnOp::Neg => HirUnaryOp::Minus,
            ast::UnOp::Plus => HirUnaryOp::Plus,
            ast::UnOp::PreInc => HirUnaryOp::PreInc,
            ast::UnOp::PostInc => HirUnaryOp::PostInc,
            ast::UnOp::PreDec => HirUnaryOp::PreDec,
            ast::UnOp::PostDec => HirUnaryOp::PostDec,
        }
    }
}

/// Errors that can occur during AST to HIR lowering
#[derive(Debug, Clone)]
pub enum LoweringError {
    UnsupportedConstruct(String),
    InvalidSpan,
    MissingRequiredField(String),
}

impl std::fmt::Display for LoweringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoweringError::UnsupportedConstruct(construct) => {
                write!(f, "Unsupported construct during lowering: {}", construct)
            }
            LoweringError::InvalidSpan => write!(f, "Invalid span information"),
            LoweringError::MissingRequiredField(field) => {
                write!(f, "Missing required field: {}", field)
            }
        }
    }
}

impl std::error::Error for LoweringError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ids::ModuleId;

    #[test]
    fn test_lowering_context_creation() {
        let module_id = ModuleId::new(0);
        let ctx = LoweringContext::new(module_id);
        assert_eq!(ctx._module_id, module_id);
    }

    #[test]
    fn test_type_lowering() {
        let module_id = ModuleId::new(0);
        let ctx = LoweringContext::new(module_id);

        assert_eq!(ctx.lower_type(&ast::Type::I32), HirType::I32);
        assert_eq!(ctx.lower_type(&ast::Type::Bool), HirType::Bool);
        assert_eq!(ctx.lower_type(&ast::Type::String), HirType::String);
        assert_eq!(ctx.lower_type(&ast::Type::Void), HirType::Void);

        let pointer_type = ast::Type::Pointer(Box::new(ast::Type::I32));
        assert_eq!(
            ctx.lower_type(&pointer_type),
            HirType::Pointer(Box::new(HirType::I32))
        );

        let array_type = ast::Type::Array(Box::new(ast::Type::Bool));
        assert_eq!(
            ctx.lower_type(&array_type),
            HirType::Array(Box::new(HirType::Bool))
        );
    }

    #[test]
    fn test_visibility_lowering() {
        let module_id = ModuleId::new(0);
        let ctx = LoweringContext::new(module_id);

        assert_eq!(
            ctx.lower_visibility(&ast::Visibility::Private),
            HirVisibility::Private
        );
        assert_eq!(
            ctx.lower_visibility(&ast::Visibility::Public),
            HirVisibility::Public
        );
        assert_eq!(
            ctx.lower_visibility(&ast::Visibility::Internal),
            HirVisibility::PublicCrate
        );
    }
}
