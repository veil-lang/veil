use crate::ast;
use crate::ast::{BinOp, Expr, Type};
use crate::typeck::TypeChecker;
use codespan::FileId;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::collections::HashMap;

impl TypeChecker {
    fn unify_types(
        &self,
        pattern: &crate::ast::Type,
        concrete: &crate::ast::Type,
        subst: &mut std::collections::HashMap<String, crate::ast::Type>,
    ) -> bool {
        use crate::ast::Type;
        match (pattern, concrete) {
            (Type::Generic(name), _) => {
                if let Some(existing) = subst.get(name) {
                    existing == concrete
                } else {
                    subst.insert(name.clone(), concrete.clone());
                    true
                }
            }
            (Type::Array(p), Type::Array(c)) => self.unify_types(p, c, subst),
            (Type::Pointer(p), Type::Pointer(c)) => self.unify_types(p, c, subst),
            (Type::SizedArray(p, n1), Type::SizedArray(c, n2)) => {
                n1 == n2 && self.unify_types(p, c, subst)
            }
            (Type::GenericInstance(name_p, args_p), Type::GenericInstance(name_c, args_c)) => {
                if name_p != name_c || args_p.len() != args_c.len() {
                    return false;
                }
                for (ap, ac) in args_p.iter().zip(args_c.iter()) {
                    if !self.unify_types(ap, ac, subst) {
                        return false;
                    }
                }
                true
            }
            (Type::Struct(n1), Type::Struct(n2)) => n1 == n2,
            (Type::Enum(n1), Type::Enum(n2)) => n1 == n2,
            (Type::I32, Type::I32)
            | (Type::I64, Type::I64)
            | (Type::I8, Type::I8)
            | (Type::I16, Type::I16)
            | (Type::U8, Type::U8)
            | (Type::U16, Type::U16)
            | (Type::U32, Type::U32)
            | (Type::U64, Type::U64)
            | (Type::F32, Type::F32)
            | (Type::F64, Type::F64)
            | (Type::Bool, Type::Bool)
            | (Type::String, Type::String)
            | (Type::Void, Type::Void)
            | (Type::RawPtr, Type::RawPtr)
            | (Type::Unknown, Type::Unknown) => true,
            _ => false,
        }
    }

    fn try_match_impl_target(
        &self,
        impl_target: &str,
        obj_type: &crate::ast::Type,
    ) -> Option<std::collections::HashMap<String, crate::ast::Type>> {
        let pattern = self.parse_type_name(impl_target);
        let mut subst: std::collections::HashMap<String, crate::ast::Type> =
            std::collections::HashMap::new();
        if self.unify_types(&pattern, obj_type, &mut subst) {
            Some(subst)
        } else {
            None
        }
    }
    pub fn check_expr(&mut self, expr: &mut Expr) -> Result<Type, Vec<Diagnostic<FileId>>> {
        if let Expr::FieldAccess(obj, field_name, span_info) = expr
            && let Expr::Var(name, _) = obj.as_ref()
        {
            if let Some(var_type) = self.context.variables.get(name) {
                if let Type::Enum(enum_name) = var_type {
                    let enum_type = Type::Enum(enum_name.clone());
                    *expr = Expr::EnumConstruct(
                        enum_name.clone(),
                        field_name.clone(),
                        vec![],
                        ast::ExprInfo {
                            span: span_info.span,
                            ty: enum_type.clone(),
                            is_tail: span_info.is_tail,
                        },
                    );
                    return Ok(enum_type);
                }
            } else if self.context.enum_defs.contains_key(name) {
                let enum_type = Type::Enum(name.clone());
                *expr = Expr::EnumConstruct(
                    name.clone(),
                    field_name.clone(),
                    vec![],
                    ast::ExprInfo {
                        span: span_info.span,
                        ty: enum_type.clone(),
                        is_tail: span_info.is_tail,
                    },
                );
                return Ok(enum_type);
            }
        }

        match expr {
            Expr::Int(value, info) => {
                let expected_type = &self.context.current_return_type;
                if *value >= 0 {
                    match expected_type {
                        Type::U8 => {
                            if *value > u8::MAX as i32 {
                                self.report_error(
                                    &format!(
                                        "Value {} exceeds maximum for u8 ({})",
                                        value,
                                        u8::MAX
                                    ),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::U8;
                            Ok(Type::U8)
                        }
                        Type::U16 => {
                            if *value > u16::MAX as i32 {
                                self.report_error(
                                    &format!(
                                        "Value {} exceeds maximum for u16 ({})",
                                        value,
                                        u16::MAX
                                    ),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::U16;
                            Ok(Type::U16)
                        }
                        Type::U32 => {
                            if *value > u32::MAX as i32 {
                                self.report_error(
                                    &format!(
                                        "Value {} exceeds maximum for u32 ({})",
                                        value,
                                        u32::MAX
                                    ),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::U32;
                            Ok(Type::U32)
                        }
                        Type::U64 => {
                            info.ty = Type::U64;
                            Ok(Type::U64)
                        }
                        _ => {
                            info.ty = Type::I32;
                            Ok(Type::I32)
                        }
                    }
                } else {
                    match expected_type {
                        Type::I8 => {
                            if *value < i8::MIN as i32 || *value > i8::MAX as i32 {
                                self.report_error(
                                    &format!(
                                        "Value {} is out of range for i8 ({}..{})",
                                        value,
                                        i8::MIN,
                                        i8::MAX
                                    ),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::I8;
                            Ok(Type::I8)
                        }
                        Type::I16 => {
                            if *value < i16::MIN as i32 || *value > i16::MAX as i32 {
                                self.report_error(
                                    &format!(
                                        "Value {} is out of range for i16 ({}..{})",
                                        value,
                                        i16::MIN,
                                        i16::MAX
                                    ),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::I16;
                            Ok(Type::I16)
                        }
                        Type::I32 => {
                            info.ty = Type::I32;
                            Ok(Type::I32)
                        }
                        Type::I64 => {
                            info.ty = Type::I64;
                            Ok(Type::I64)
                        }
                        Type::U8 | Type::U16 | Type::U32 | Type::U64 => {
                            self.report_error(
                                &format!(
                                    "Cannot assign negative value {} to unsigned type {}",
                                    value, expected_type
                                ),
                                info.span,
                            );
                            info.ty = Type::Unknown;
                            Ok(Type::Unknown)
                        }
                        _ => {
                            info.ty = Type::I32;
                            Ok(Type::I32)
                        }
                    }
                }
            }
            Expr::Bool(_, _) => Ok(Type::Bool),
            Expr::Str(_, _) => Ok(Type::String),
            Expr::Int64(_, _) => Ok(Type::I64),
            Expr::F64(_, _) => Ok(Type::F64),
            Expr::F32(_, _) => Ok(Type::F32),
            Expr::Void(_) => Ok(Type::Void),
            Expr::Var(
                name,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let ty = match name.as_str() {
                    "true" | "false" => Type::Bool,
                    _ => match self.context.variables.get(name).cloned() {
                        Some(ty) => ty,
                        None => {
                            self.report_error(&format!("Undefined variable '{}'", name), *span);
                            Type::Unknown
                        }
                    },
                };
                *expr_type = ty.clone();
                Ok(ty)
            }
            Expr::BinOp(
                left,
                op,
                right,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;
                let result_ty = match op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Pow
                    | BinOp::Pow2
                    | BinOp::Mod => {
                        if (left_ty == Type::I8 && right_ty == Type::I8)
                            || (left_ty == Type::I16 && right_ty == Type::I16)
                            || (left_ty == Type::I32 && right_ty == Type::I32)
                            || (left_ty == Type::I64 && right_ty == Type::I64)
                            || (left_ty == Type::U8 && right_ty == Type::U8)
                            || (left_ty == Type::U16 && right_ty == Type::U16)
                            || (left_ty == Type::U32 && right_ty == Type::U32)
                            || (left_ty == Type::U64 && right_ty == Type::U64)
                            || (left_ty == Type::F32 && right_ty == Type::F32)
                            || (left_ty == Type::F64 && right_ty == Type::F64)
                            || (matches!(left_ty, Type::Struct(ref name) if name == "size_t")
                                && right_ty == Type::I32)
                            || (left_ty == Type::I32
                                && matches!(right_ty, Type::Struct(ref name) if name == "size_t"))
                            || (matches!(left_ty, Type::Struct(ref name) if name == "size_t")
                                && matches!(right_ty, Type::Struct(ref name2) if name2 == "size_t"))
                            || (left_ty == Type::CSize && right_ty == Type::I32)
                            || (left_ty == Type::I32 && right_ty == Type::CSize)
                            || (left_ty == Type::CSize && right_ty == Type::CSize)
                        {
                            if left_ty == Type::F64 || right_ty == Type::F64 {
                                Type::F64
                            } else if left_ty == Type::F32 || right_ty == Type::F32 {
                                Type::F32
                            } else if left_ty == Type::I64 || right_ty == Type::I64 {
                                Type::I64
                            } else if left_ty == Type::U64 || right_ty == Type::U64 {
                                Type::U64
                            } else if left_ty == Type::I32 || right_ty == Type::I32 {
                                Type::I32
                            } else if left_ty == Type::U32 || right_ty == Type::U32 {
                                Type::U32
                            } else if left_ty == Type::I16 || right_ty == Type::I16 {
                                Type::I16
                            } else if left_ty == Type::U16 || right_ty == Type::U16 {
                                Type::U16
                            } else if left_ty == Type::I8 || right_ty == Type::I8 {
                                Type::I8
                            } else if left_ty == Type::U8 || right_ty == Type::U8 {
                                Type::U8
                            } else if matches!(left_ty, Type::Struct(ref name) if name == "size_t")
                                || matches!(right_ty, Type::Struct(ref name) if name == "size_t")
                            {
                                Type::Struct("size_t".to_string())
                            } else if left_ty == Type::CSize || right_ty == Type::CSize {
                                Type::CSize
                            } else {
                                left_ty
                            }
                        } else if matches!(op, BinOp::Add)
                            && (left_ty == Type::String || right_ty == Type::String)
                        {
                            if left_ty == Type::String && right_ty == Type::String {
                                Type::String
                            } else if left_ty == Type::String
                                && Self::is_convertible(&right_ty, &Type::String)
                            {
                                Type::String
                            } else if right_ty == Type::String
                                && Self::is_convertible(&left_ty, &Type::String)
                            {
                                Type::String
                            } else {
                                self.report_error(
                                    &format!(
                                        "Cannot apply {:?} to {} and {}",
                                        op, left_ty, right_ty
                                    ),
                                    *span,
                                );
                                Type::Unknown
                            }
                        } else {
                            self.report_error(
                                &format!("Cannot apply {:?} to {} and {}", op, left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    BinOp::Gt
                    | BinOp::Eq
                    | BinOp::Lt
                    | BinOp::NotEq
                    | BinOp::GtEq
                    | BinOp::LtEq => {
                        // Strings: only == and != supported
                        if left_ty == Type::String || right_ty == Type::String {
                            match op {
                                BinOp::Eq | BinOp::NotEq => Type::Bool,
                                _ => {
                                    self.report_error(
                                        "Only == and != are supported for string comparisons",
                                        *span,
                                    );
                                    Type::Unknown
                                }
                            }
                        // Optionals/none: only == and != supported
                        } else if matches!(left_ty, Type::Optional(_) | Type::NoneType)
                            || matches!(right_ty, Type::Optional(_) | Type::NoneType)
                        {
                            match op {
                                BinOp::Eq | BinOp::NotEq => Type::Bool,
                                _ => {
                                    self.report_error(
                                        "Only == and != are supported when comparing optionals/none",
                                        *span,
                                    );
                                    Type::Unknown
                                }
                            }
                        } else if Self::is_convertible(&left_ty, &right_ty) {
                            // Numeric and interoperable comparisons (includes size_t/CSize)
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!("Cannot compare {} and {}", left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    BinOp::And | BinOp::Or => {
                        if left_ty == Type::Bool && right_ty == Type::Bool {
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!(
                                    "Logical {} requires bool operands, got {} and {}",
                                    op, left_ty, right_ty
                                ),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                };

                *expr_type = result_ty.clone();

                Ok(result_ty)
            }
            Expr::Deref(inner_expr, info) => {
                let inner_ty = self.check_expr(inner_expr)?;
                let result_ty = match inner_ty {
                    Type::Pointer(t) => *t,
                    Type::RawPtr => Type::Unknown,
                    _ => {
                        self.report_error(
                            &format!("Cannot dereference type {}", inner_ty),
                            info.span,
                        );
                        Type::Unknown
                    }
                };
                info.ty = result_ty.clone();
                Ok(result_ty)
            }
            Expr::UnaryOp(
                op,
                operand,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let operand_ty = self.check_expr(operand)?;
                let result_ty = match op {
                    ast::UnOp::Neg => {
                        if operand_ty == Type::I8
                            || operand_ty == Type::I16
                            || operand_ty == Type::I32
                            || operand_ty == Type::I64
                            || operand_ty == Type::F32
                            || operand_ty == Type::F64
                        {
                            operand_ty
                        } else {
                            self.report_error(&format!("Cannot negate type {}", operand_ty), *span);
                            Type::Unknown
                        }
                    }
                    ast::UnOp::Plus => {
                        if operand_ty == Type::I8
                            || operand_ty == Type::I16
                            || operand_ty == Type::I32
                            || operand_ty == Type::I64
                            || operand_ty == Type::U8
                            || operand_ty == Type::U16
                            || operand_ty == Type::U32
                            || operand_ty == Type::U64
                            || operand_ty == Type::F32
                            || operand_ty == Type::F64
                        {
                            operand_ty
                        } else {
                            self.report_error(
                                &format!("Cannot apply unary plus to type {}", operand_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    ast::UnOp::Not => {
                        if operand_ty == Type::Bool {
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!("Cannot apply logical NOT to type {}", operand_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                };
                *expr_type = result_ty.clone();
                Ok(result_ty)
            }
            Expr::Assign(
                target,
                value,
                ast::ExprInfo {
                    span,
                    ty: _expr_type,
                    is_tail: _,
                },
            ) => {
                let target_ty = self.check_expr(target)?;
                let value_ty = self.check_expr(value)?;

                if !Self::is_convertible(&value_ty, &target_ty) {
                    self.report_error(
                        &format!("Cannot assign {} to {}", value_ty, target_ty),
                        *span,
                    );
                }

                Ok(Type::Void)
            }
            Expr::Call(
                name,
                args,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                if let Some(method_name) = name.strip_prefix("<method>.")
                    && let Some(obj_expr) = args.first_mut()
                {
                    let (obj_type, is_static_call) = match obj_expr {
                        Expr::Var(var_name, var_info) => {
                            if self.context.struct_defs.contains_key(var_name) {
                                var_info.ty = Type::Struct(var_name.clone());
                                (Type::Struct(var_name.clone()), true)
                            } else {
                                (self.check_expr(obj_expr)?, false)
                            }
                        }
                        _ => (self.check_expr(obj_expr)?, false),
                    };

                    let type_name = obj_type.to_string();

                    let impls = self.impls.clone();
                    let mut method_found = false;
                    let mut method_return_type = Type::Unknown;
                    let mut method_param_types = Vec::new();

                    for impl_block in &impls {
                        if let Some(subst) =
                            self.try_match_impl_target(&impl_block.target_type, &obj_type)
                        {
                            for method in &impl_block.methods {
                                if method.name == method_name {
                                    method_found = true;
                                    // substitute generics in return type and param types
                                    method_return_type =
                                        crate::typeck::pattern::substitute_generics(
                                            &method.return_type,
                                            &subst.iter().collect(),
                                        );
                                    method_param_types = method
                                        .params
                                        .iter()
                                        .map(|(_, ty)| {
                                            crate::typeck::pattern::substitute_generics(
                                                ty,
                                                &subst.iter().collect(),
                                            )
                                        })
                                        .collect();
                                    break;
                                }
                            }
                            if method_found {
                                break;
                            }
                        }
                    }

                    if !method_found && let Type::Struct(ref simple_name) = obj_type {
                        for impl_block in &impls {
                            if impl_block.target_type == *simple_name
                                && let Some(method) =
                                    impl_block.methods.iter().find(|m| m.name == method_name)
                            {
                                method_found = true;
                                method_return_type = method.return_type.clone();
                                method_param_types =
                                    method.params.iter().map(|(_, ty)| ty.clone()).collect();
                                break;
                            }
                        }
                        if !method_found {
                            let synthetic_key = format!("{}.{}", simple_name, method_name);
                            if let Some((params, ret)) = self.functions.get(&synthetic_key).cloned()
                            {
                                method_found = true;
                                method_param_types = params;
                                method_return_type = ret;
                            }
                        }
                    }

                    if !method_found {
                        self.report_error(
                            &format!(
                                "Method '{}' not found for type '{}'",
                                method_name, type_name
                            ),
                            *span,
                        );
                        *expr_type = Type::Unknown;
                        return Ok(Type::Unknown);
                    }

                    let actual_args = if is_static_call {
                        args.len() - 1
                    } else {
                        args.len()
                    };
                    let expected_args = method_param_types.len();

                    if actual_args != expected_args {
                        self.report_error(
                            &format!(
                                "Method '{}' expects {} arguments, got {}",
                                method_name, expected_args, actual_args
                            ),
                            *span,
                        );
                    }

                    let start_idx = if is_static_call { 1 } else { 1 };
                    for (i, param_type) in method_param_types.iter().enumerate() {
                        if let Some(arg) = args.get_mut(start_idx + i) {
                            self.check_expr_with_expected(arg, param_type)?;
                        }
                    }

                    *expr_type = method_return_type.clone();
                    return Ok(method_return_type);
                }

                let mut found = self.functions.get(name).cloned();
                if found.is_none() {
                    found = self
                        .functions
                        .iter()
                        .find(|(k, _)| k.as_str() == name)
                        .map(|(_, v)| v.clone());
                }
                let (param_types, return_type) = match found {
                    Some(types) => types,
                    None => {
                        self.report_error(&format!("Undefined function '{}'", name), *span);
                        *expr_type = Type::Unknown;
                        return Ok(Type::Unknown);
                    }
                };

                let has_ellipsis = param_types.last() == Some(&Type::Ellipsis);
                let min_args = if has_ellipsis {
                    param_types.len() - 1
                } else {
                    param_types.len()
                };

                if args.len() < min_args || (!has_ellipsis && args.len() > min_args) {
                    self.report_error(
                        &format!(
                            "Expected {}{} arguments, got {}",
                            min_args,
                            if has_ellipsis { "+" } else { "" },
                            args.len()
                        ),
                        *span,
                    );
                }

                for (arg, param_ty) in args.iter_mut().zip(param_types.iter()).take(min_args) {
                    let arg_ty = self.check_expr(arg).unwrap_or(Type::Unknown);
                    if !Self::is_convertible(&arg_ty, param_ty) {
                        self.report_error(
                            &format!("Expected {}, got {}", param_ty, arg_ty),
                            arg.span(),
                        );
                    }
                }
                *expr_type = return_type.clone();

                Ok(return_type.clone())
            }
            Expr::New(
                struct_name,
                args,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                if let Some(_struct_def) = self.context.struct_defs.get(struct_name) {
                    let constructor_name = format!("{}.constructor", struct_name);
                    let constructor_info = self.functions.get(&constructor_name).cloned();

                    if let Some((param_types, return_type)) = constructor_info {
                        let has_ellipsis = param_types.last() == Some(&Type::Ellipsis);
                        let min_args = if has_ellipsis {
                            param_types.len() - 1
                        } else {
                            param_types.len()
                        };

                        if args.len() < min_args || (!has_ellipsis && args.len() > min_args) {
                            self.report_error(
                                &format!(
                                    "Expected {}{} arguments for constructor, got {}",
                                    min_args,
                                    if has_ellipsis { "+" } else { "" },
                                    args.len()
                                ),
                                *span,
                            );
                        }

                        for (arg, param_ty) in
                            args.iter_mut().zip(param_types.iter()).take(min_args)
                        {
                            let arg_ty = self.check_expr(arg).unwrap_or(Type::Unknown);
                            if !Self::is_convertible(&arg_ty, param_ty) {
                                self.report_error(
                                    &format!("Expected {}, got {}", param_ty, arg_ty),
                                    arg.span(),
                                );
                            }
                        }

                        *expr_type = return_type.clone();
                        Ok(return_type.clone())
                    } else {
                        self.report_error(
                            &format!("No constructor found for struct '{}'", struct_name),
                            *span,
                        );
                        *expr_type = Type::Unknown;
                        Ok(Type::Unknown)
                    }
                } else {
                    self.report_error(&format!("Unknown struct '{}'", struct_name), *span);
                    *expr_type = Type::Unknown;
                    Ok(Type::Unknown)
                }
            }
            Expr::SafeBlock(stmts, _) => {
                let old_in_safe = self.context.in_safe;
                self.context.in_safe = true;
                let result = self.check_block(stmts);

                self.context.in_safe = old_in_safe;
                result?;
                Ok(Type::Void)
            }
            Expr::Cast(
                expr,
                target_ty,
                ast::ExprInfo {
                    span,
                    ty: _expr_type,
                    is_tail: _,
                },
            ) => {
                let source_ty = self.check_expr(expr)?;

                match (&source_ty, &target_ty) {
                    (Type::RawPtr, Type::Pointer(_)) => Ok(target_ty.clone()),
                    (Type::Pointer(_), Type::RawPtr) => Ok(target_ty.clone()),
                    (Type::Pointer(_), Type::I32) => Ok(target_ty.clone()),
                    (Type::I32, Type::Pointer(_)) => Ok(target_ty.clone()),
                    (Type::I32, Type::I32) => Ok(source_ty),
                    (Type::I32, Type::Bool) => Ok(target_ty.clone()),
                    (Type::F32, Type::F32) => Ok(source_ty),
                    (Type::F32, Type::I32) => Ok(target_ty.clone()),
                    (Type::I32, Type::F32) => Ok(target_ty.clone()),
                    (Type::F64, Type::F64) => Ok(source_ty),
                    (Type::F64, Type::I32) => Ok(target_ty.clone()),
                    (Type::I32, Type::F64) => Ok(target_ty.clone()),
                    (Type::I32, Type::U32) => Ok(target_ty.clone()),
                    (Type::U32, Type::I32) => Ok(target_ty.clone()),
                    (Type::String, Type::I32) => Ok(target_ty.clone()),
                    (Type::String, Type::RawPtr) => Ok(target_ty.clone()),
                    (Type::F32, Type::String) => Ok(target_ty.clone()),
                    _ => {
                        if !self.is_cast_allowed(&source_ty, target_ty) {
                            self.report_error(
                                &format!("Invalid cast from {} to {}", source_ty, target_ty),
                                *span,
                            );
                            Ok(Type::Unknown)
                        } else {
                            Ok(target_ty.clone())
                        }
                    }
                }
            }
            Expr::Range(start, end, _, _) => {
                let start_ty = self.check_expr(start)?;
                let end_ty = self.check_expr(end)?;

                if start_ty != Type::I32 {
                    self.report_error("Range start must be an integer", start.span());
                }

                if end_ty != Type::I32 {
                    self.report_error("Range end must be an integer", end.span());
                }

                Ok(Type::Unknown)
            }
            Expr::InfiniteRange(_, info) => {
                info.ty = Type::I32;
                Ok(Type::I32)
            }
            Expr::StructInit(
                name,
                fields,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let struct_name = name.clone();
                let struct_fields = match self.context.struct_defs.get(&struct_name) {
                    Some(fields) => fields.clone(),
                    None => {
                        self.report_error(&format!("Undefined struct '{}'", name), *span);
                        *expr_type = Type::Unknown;
                        return Ok(Type::Unknown);
                    }
                };

                let mut seen_fields = HashMap::new();
                for (field_name, field_expr) in fields {
                    let field_name = field_name.clone();

                    let expected_ty =
                        match struct_fields.iter().find(|(name, _)| name == &field_name) {
                            Some((_, expected_ty)) => expected_ty.clone(),
                            None => Type::Unknown,
                        };

                    let field_ty = self.check_expr_with_expected(field_expr, &expected_ty)?;

                    match struct_fields.iter().find(|(name, _)| name == &field_name) {
                        Some((_, expected_ty)) => {
                            if !Self::is_convertible(&field_ty, expected_ty) {
                                self.report_error(
                                    &format!(
                                        "Type mismatch for field '{}': expected {}, got {}",
                                        field_name, expected_ty, field_ty
                                    ),
                                    field_expr.span(),
                                );
                            }
                        }
                        None => {
                            self.report_error(
                                &format!("Unknown field '{}' in struct '{}'", field_name, name),
                                field_expr.span(),
                            );
                        }
                    }

                    seen_fields.insert(field_name, ());
                }

                for (field_name, _) in struct_fields {
                    if !seen_fields.contains_key(&field_name) {
                        self.report_error(
                            &format!("Missing field '{}' in struct initialization", field_name),
                            *span,
                        );
                    }
                }

                let ty = Type::Struct(name.clone());
                *expr_type = ty.clone();
                Ok(ty)
            }
            Expr::FieldAccess(
                obj,
                field_name,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let obj_ty = self.check_expr(obj)?;
                match obj_ty {
                    Type::Struct(struct_name) => match self.context.struct_defs.get(&struct_name) {
                        Some(fields) => match fields.iter().find(|(name, _)| name == field_name) {
                            Some((_, field_ty)) => {
                                *expr_type = field_ty.clone();
                                Ok(field_ty.clone())
                            }
                            None => {
                                self.report_error(
                                    &format!(
                                        "No field '{}' in struct '{}'",
                                        field_name, struct_name
                                    ),
                                    *span,
                                );
                                Ok(Type::Unknown)
                            }
                        },
                        None => {
                            self.report_error(&format!("Unknown struct '{}'", struct_name), *span);
                            Ok(Type::Unknown)
                        }
                    },
                    _ => {
                        self.report_error(
                            &format!("Cannot access field '{}' on type {}", field_name, obj_ty),
                            *span,
                        );
                        Ok(Type::Unknown)
                    }
                }
            }
            Expr::ArrayInit(
                elements,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                if elements.is_empty() {
                    self.report_error("Cannot infer type of empty array", *span);
                    *expr_type = Type::Unknown;
                    return Ok(Type::Unknown);
                }

                let mut element_type: Option<Type> = None;

                for (i, element) in elements.iter_mut().enumerate() {
                    match element {
                        Expr::Spread(spread_expr, _) => {
                            let spread_type = self.check_expr(spread_expr)?;
                            if let Type::Array(inner_type) = spread_type {
                                if let Some(ref expected) = element_type {
                                    if !Self::is_convertible(&inner_type, expected) {
                                        self.report_error(
                                            &format!(
                                                "Spread array element type {} does not match expected {}",
                                                inner_type, expected
                                            ),
                                            element.span(),
                                        );
                                    }
                                } else {
                                    element_type = Some(*inner_type);
                                }
                            } else {
                                self.report_error(
                                    &format!("Cannot spread non-array type {}", spread_type),
                                    element.span(),
                                );
                            }
                        }
                        _ => {
                            let el_type = self.check_expr(element)?;
                            if let Some(ref expected) = element_type {
                                if !Self::is_convertible(&el_type, expected) {
                                    self.report_error(
                                        &format!(
                                            "Array element {} has type {}, but expected {}",
                                            i, el_type, expected
                                        ),
                                        element.span(),
                                    );
                                }
                            } else {
                                element_type = Some(el_type);
                            }
                        }
                    }
                }

                let final_element_type = element_type.unwrap_or(Type::Unknown);
                let array_type = Type::Array(Box::new(final_element_type));
                *expr_type = array_type.clone();
                Ok(array_type)
            }
            Expr::ArrayAccess(
                array,
                index,
                ast::ExprInfo {
                    span: _,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let array_type = self.check_expr(array)?;
                let index_type = self.check_expr(index)?;
                if !Self::is_convertible(&index_type, &Type::I32)
                    && !matches!(index_type, Type::Struct(ref name) if name == "size_t")
                    && index_type != Type::CSize
                {
                    self.report_error(
                        &format!("Array index must be i32 or size_t, got {}", index_type),
                        index.span(),
                    );
                }

                match array_type {
                    Type::Array(element_type) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    Type::SizedArray(element_type, _) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    Type::Pointer(element_type) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    Type::RawPtr => {
                        let element_type = Type::U8;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    _ => {
                        self.report_error(
                            &format!("Cannot index non-array type: {}", array_type),
                            array.span(),
                        );
                        Ok(Type::Unknown)
                    }
                }
            }
            Expr::TemplateStr(parts, info) => {
                for part in parts {
                    if let ast::TemplateStrPart::Expression(expr) = part {
                        let ty = self.check_expr(expr)?;

                        match expr.as_mut() {
                            ast::Expr::ArrayAccess(array, index, array_info) => {
                                let array_ty = self.check_expr(array)?;
                                let index_ty = self.check_expr(index)?;

                                if !Self::is_convertible(&index_ty, &Type::I32) {
                                    self.report_error(
                                        &format!("Array index must be i32, got {}", index_ty),
                                        index.span(),
                                    );
                                }

                                match array_ty {
                                    Type::Array(element_type) => {
                                        array_info.ty = *element_type.clone();
                                        if !Self::is_convertible(&element_type, &Type::String)
                                            && !matches!(
                                                *element_type,
                                                Type::I8
                                                    | Type::I16
                                                    | Type::I32
                                                    | Type::I64
                                                    | Type::U8
                                                    | Type::U16
                                                    | Type::U32
                                                    | Type::U64
                                                    | Type::F32
                                                    | Type::F64
                                                    | Type::Bool
                                                    | Type::Generic(_)
                                                    | Type::Optional(_)
                                            )
                                        {
                                            self.report_error(
                                                &format!("Cannot convert array element type {} to string", element_type),
                                                array.span()
                                            );
                                        }
                                    }
                                    Type::SizedArray(element_type, _) => {
                                        array_info.ty = *element_type.clone();
                                        if !Self::is_convertible(&element_type, &Type::String)
                                            && !matches!(
                                                *element_type,
                                                Type::I8
                                                    | Type::I16
                                                    | Type::I32
                                                    | Type::I64
                                                    | Type::U8
                                                    | Type::U16
                                                    | Type::U32
                                                    | Type::U64
                                                    | Type::F32
                                                    | Type::F64
                                                    | Type::Bool
                                                    | Type::Generic(_)
                                                    | Type::Optional(_)
                                            )
                                        {
                                            self.report_error(
                                                &format!("Cannot convert array element type {} to string", element_type),
                                                array.span()
                                            );
                                        }
                                    }
                                    _ => {
                                        self.report_error(
                                            &format!("Cannot index non-array type: {}", array_ty),
                                            array.span(),
                                        );
                                    }
                                }
                            }
                            ast::Expr::Var(name, var_info) => {
                                let ty = if name == "true" || name == "false" {
                                    Type::Bool
                                } else {
                                    match self.context.variables.get(name).cloned() {
                                        Some(ty) => ty,
                                        None => {
                                            self.report_error(
                                                &format!("Undefined variable '{}'", name),
                                                var_info.span,
                                            );
                                            Type::Unknown
                                        }
                                    }
                                };
                                var_info.ty = ty.clone();
                                if !matches!(
                                    ty,
                                    Type::String
                                        | Type::I8
                                        | Type::I16
                                        | Type::I32
                                        | Type::I64
                                        | Type::U8
                                        | Type::U16
                                        | Type::U32
                                        | Type::U64
                                        | Type::F32
                                        | Type::F64
                                        | Type::Bool
                                        | Type::Array(_)
                                        | Type::SizedArray(_, _)
                                        | Type::Generic(_)
                                        | Type::Optional(_)
                                ) {
                                    let msg = format!("Cannot convert type {:?} to string", ty);
                                    let span = expr.span();
                                    self.errors.push(
                                        Diagnostic::error()
                                            .with_message(msg)
                                            .with_labels(vec![Label::primary(self.file_id, span)]),
                                    );
                                }
                            }
                            _ => {
                                if !matches!(
                                    ty,
                                    Type::String
                                        | Type::I8
                                        | Type::I16
                                        | Type::I32
                                        | Type::I64
                                        | Type::U8
                                        | Type::U16
                                        | Type::U32
                                        | Type::U64
                                        | Type::F32
                                        | Type::F64
                                        | Type::Bool
                                        | Type::Array(_)
                                        | Type::SizedArray(_, _)
                                        | Type::Generic(_)
                                        | Type::Optional(_)
                                ) {
                                    let msg = format!("Cannot convert type {:?} to string", ty);
                                    let span = expr.span();
                                    self.errors.push(
                                        Diagnostic::error()
                                            .with_message(msg)
                                            .with_labels(vec![Label::primary(self.file_id, span)]),
                                    );
                                }
                            }
                        }
                    }
                }

                info.ty = Type::String;
                Ok(Type::String)
            }
            Expr::FfiCall(_name, _args, _info) => Ok(Type::Unknown),
            Expr::EnumConstruct(enum_name, variant_name, args, info) => {
                for arg in args.iter_mut() {
                    self.check_expr(arg)?;
                }

                if let Some(enum_def) = self.context.enum_def_map.get(enum_name).cloned()
                    && let Some(variant) =
                        enum_def.variants.iter().find(|v| &v.name == variant_name)
                {
                    let mut expected_types: Vec<Type> = vec![];
                    if let Some(data) = &variant.data {
                        match data {
                            crate::ast::EnumVariantData::Tuple(types) => {
                                expected_types = types.clone();
                            }
                            crate::ast::EnumVariantData::Struct(fields) => {
                                expected_types = fields.iter().map(|f| f.ty.clone()).collect();
                            }
                        }
                    }

                    let enum_name_clone = enum_name.clone();
                    let variant_name_clone = variant.name.clone();

                    for (i, arg) in args.iter_mut().enumerate() {
                        let arg_ty = arg.get_type();
                        let expected_ty = expected_types.get(i).cloned().unwrap_or(Type::Unknown);
                        if !Self::is_convertible(&arg_ty, &expected_ty) {
                            self.report_error(
                                &format!(
                                    "Enum variant '{}' of '{}' expects {}, got {}",
                                    variant_name_clone, enum_name_clone, expected_ty, arg_ty
                                ),
                                arg.span(),
                            );
                        }
                    }
                }

                let ty = if let Some(enum_def) = self.context.enum_def_map.get(enum_name) {
                    if !enum_def.generic_params.is_empty() {
                        let mut context_ty = None;
                        if let Type::GenericInstance(n, params) = &self.context.current_return_type
                            && n == enum_name
                            && params.len() == enum_def.generic_params.len()
                        {
                            context_ty = Some(Type::GenericInstance(n.clone(), params.clone()));
                        }
                        if context_ty.is_none()
                            && let Some(last_var_ty) = self.context.variables.values().last()
                            && let Type::GenericInstance(n, params) = last_var_ty
                            && n == enum_name
                            && params.len() == enum_def.generic_params.len()
                        {
                            context_ty = Some(Type::GenericInstance(n.clone(), params.clone()));
                        }
                        if let Some(t) = context_ty {
                            t
                        } else {
                            let arg_types: Vec<Type> = args.iter().map(|e| e.get_type()).collect();
                            Type::GenericInstance(enum_name.clone(), arg_types)
                        }
                    } else {
                        Type::Enum(enum_name.clone())
                    }
                } else {
                    Type::Enum(enum_name.clone())
                };
                info.ty = ty.clone();
                Ok(ty)
            }
            Expr::Match(expr, arms, info) => {
                // First, typecheck the expression being matched
                let matched_ty = self.check_expr(expr)?;
                let mut arm_types = Vec::new();
                for arm in arms.iter_mut() {
                    let original_variables = self.context.variables.clone();
                    match &arm.pattern {
                        ast::Pattern::EnumVariant(enum_name, _variant_name, _subpatterns, _) => {
                            let expected = if let Type::GenericInstance(name, args) = &matched_ty {
                                if name == enum_name {
                                    Type::GenericInstance(name.clone(), args.clone())
                                } else {
                                    Type::Enum(enum_name.clone())
                                }
                            } else {
                                Type::Enum(enum_name.clone())
                            };
                            self.check_pattern(&arm.pattern, &expected)?;
                        }
                        _ => {
                            self.check_pattern(&arm.pattern, &matched_ty)?;
                        }
                    }

                    if let Some(guard) = &mut arm.guard {
                        let guard_ty = self.check_expr(guard)?;
                        if !matches!(guard_ty, Type::Bool) {
                            self.report_error(
                                "Guard condition must be a boolean expression",
                                guard.span(),
                            );
                        }
                    }

                    let arm_ty = match &mut arm.body {
                        ast::MatchArmBody::Expr(expr) => self.check_expr(expr)?,
                        ast::MatchArmBody::Block(stmts) => {
                            let last_ty = Type::Void;
                            for stmt in stmts.iter_mut() {
                                self.check_stmt(stmt)?;
                            }
                            last_ty
                        }
                    };
                    arm_types.push(arm_ty);
                    self.context.variables = original_variables;
                }
                let result_ty = arm_types.first().cloned().unwrap_or(Type::Unknown);
                if !arm_types
                    .iter()
                    .all(|t| Self::is_convertible(t, &result_ty))
                {
                    self.report_error("All match arms must return the same type", info.span);
                }
                info.ty = result_ty.clone();
                Ok(result_ty)
            }
            Expr::If(condition, then_branch, else_branch, info) => {
                let condition_ty = self.check_expr(condition)?;
                if !Self::is_convertible(&condition_ty, &Type::Bool) {
                    self.report_error("If condition must be boolean", info.span);
                }

                let original_variables = self.context.variables.clone();

                for stmt in then_branch.iter_mut() {
                    self.check_stmt(stmt)?;
                }
                let then_ty = if let Some(last_stmt) = then_branch.last() {
                    match last_stmt {
                        ast::Stmt::Expr(expr, _) => expr.get_type(),
                        _ => Type::Void,
                    }
                } else {
                    Type::Void
                };

                let else_ty = if let Some(else_stmts) = else_branch {
                    self.context.variables = original_variables.clone();
                    for stmt in else_stmts.iter_mut() {
                        self.check_stmt(stmt)?;
                    }
                    if let Some(last_stmt) = else_stmts.last() {
                        match last_stmt {
                            ast::Stmt::Expr(expr, _) => expr.get_type(),
                            _ => Type::Void,
                        }
                    } else {
                        Type::Void
                    }
                } else {
                    Type::Void
                };

                self.context.variables = original_variables;

                let result_ty = if Self::is_convertible(&then_ty, &else_ty) {
                    then_ty
                } else if Self::is_convertible(&else_ty, &then_ty) {
                    else_ty
                } else if then_ty == Type::Void && else_ty == Type::Void {
                    Type::Void
                } else {
                    then_ty
                };

                info.ty = result_ty.clone();
                Ok(result_ty)
            }
            Expr::Loop(body, info) => {
                self.context.in_loop = true;

                let previous_break_types = std::mem::take(&mut self.context.break_types);

                for stmt in body.iter_mut() {
                    self.check_stmt(stmt)?;
                }

                let break_types = std::mem::take(&mut self.context.break_types);
                self.context.break_types = previous_break_types;
                self.context.in_loop = false;

                let loop_type = if break_types.is_empty() {
                    Type::Void
                } else {
                    let mut common_type = break_types[0].clone();
                    for break_type in &break_types[1..] {
                        if !Self::is_convertible(&common_type, break_type)
                            && !Self::is_convertible(break_type, &common_type)
                            && common_type != *break_type
                        {
                            self.report_error(
                                &format!(
                                    "Inconsistent types in break statements: {:?} vs {:?}",
                                    common_type, break_type
                                ),
                                info.span,
                            );
                            common_type = Type::Unknown;
                            break;
                        }
                    }
                    common_type
                };

                info.ty = loop_type.clone();
                Ok(loop_type)
            }
            Expr::Spread(expr, info) => {
                let expr_ty = self.check_expr(expr)?;
                info.ty = expr_ty.clone();
                Ok(expr_ty)
            }
            Expr::None(info) => {
                info.ty = Type::NoneType;
                Ok(Type::NoneType)
            }
        }
    }

    pub fn check_expr_with_expected(
        &mut self,
        expr: &mut Expr,
        expected_ty: &Type,
    ) -> Result<Type, Vec<Diagnostic<FileId>>> {
        if let Expr::ArrayInit(elements, ast::ExprInfo { ty: expr_type, .. }) = expr
            && elements.is_empty()
            && let Type::Array(_element_ty) = expected_ty
        {
            *expr_type = expected_ty.clone();
            return Ok(expected_ty.clone());
        }

        self.check_expr(expr)
    }
}
