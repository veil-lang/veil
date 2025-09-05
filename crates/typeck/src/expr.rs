//! Expression type checking for HIR
//!
//! This module implements type checking for HIR expressions, using resolved
//! symbol information to provide accurate type inference and checking.

use crate::TypeChecker;
use veil_diagnostics::Diag;
use veil_hir::{HirBinaryOp, HirExpr, HirExprKind, HirType, HirUnaryOp, NodeId};

impl TypeChecker {
    /// Type check an expression and return its type
    pub fn check_expr(&mut self, expr: &mut HirExpr) -> Result<HirType, Vec<Diag>> {
        let expr_type = match &mut *expr.kind {
            // Literals have known types
            HirExprKind::Int(value) => {
                // Choose I32 or I64 based on value range
                if *value >= i32::MIN as i64 && *value <= i32::MAX as i64 {
                    HirType::I32
                } else {
                    HirType::I64
                }
            }
            HirExprKind::Float(_) => HirType::F64, // Default float literal type
            HirExprKind::Bool(_) => HirType::Bool,
            HirExprKind::String(_) => HirType::String,
            HirExprKind::Char(_) => HirType::Char,
            HirExprKind::None => HirType::Optional(Box::new(HirType::Unknown)),
            HirExprKind::Void => HirType::Void,

            // Variable references
            HirExprKind::Variable(name) => {
                // First check local variable context
                if let Some(var_type) = self.context.local_variables.get(name) {
                    var_type.clone()
                } else if let Some(symbol) = self.context.get_symbol_by_name(name) {
                    symbol.ty.clone().unwrap_or(HirType::Unknown)
                } else {
                    self.error(format!("Undefined variable: {}", name));
                    HirType::Unknown
                }
            }

            // Field access
            HirExprKind::FieldAccess { base, field } => {
                let base_type = self.check_expr(base)?;
                self.check_field_access(&base_type, field, expr.id)?
            }

            // Method calls
            HirExprKind::MethodCall {
                receiver,
                method,
                args,
            } => {
                let receiver_type = self.check_expr(receiver)?;
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(self.check_expr(arg)?);
                }
                self.check_method_call(&receiver_type, method, &arg_types, expr.id)?
            }

            // Function calls
            HirExprKind::Call { func, args } => {
                let func_type = self.check_expr(func)?;
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(self.check_expr(arg)?);
                }
                self.check_function_call(&func_type, &arg_types, expr.id)?
            }

            // Binary operations
            HirExprKind::Binary { op, lhs, rhs } => {
                let left_type = self.check_expr(lhs)?;
                let right_type = self.check_expr(rhs)?;
                self.check_binary_op(*op, &left_type, &right_type, expr.id)?
            }

            // Unary operations
            HirExprKind::Unary { op, expr: inner } => {
                let inner_type = self.check_expr(inner)?;
                self.check_unary_op(*op, &inner_type, expr.id)?
            }

            // Array indexing
            HirExprKind::Index { base, index } => {
                let base_type = self.check_expr(base)?;
                let index_type = self.check_expr(index)?;
                self.check_array_index(&base_type, &index_type, expr.id)?
            }

            // Array literals
            HirExprKind::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    HirType::Array(Box::new(HirType::Unknown))
                } else {
                    let first_type = self.check_expr(&mut elements[0])?;
                    for elem in elements.iter_mut().skip(1) {
                        let elem_type = self.check_expr(elem)?;
                        if !self.context.types_compatible(&first_type, &elem_type) {
                            self.error(format!(
                                "Array elements must have the same type, found {:?} and {:?}",
                                first_type, elem_type
                            ));
                        }
                    }
                    HirType::Array(Box::new(first_type))
                }
            }

            // Tuple literals
            HirExprKind::TupleLiteral(elements) => {
                let mut element_types = Vec::new();
                for elem in elements {
                    element_types.push(self.check_expr(elem)?);
                }
                HirType::Tuple(element_types)
            }

            // Struct literals
            HirExprKind::StructLiteral { name, fields } => {
                self.check_struct_literal(name, fields, expr.id)?
            }

            // If expressions
            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.check_expr(condition)?;
                if !self.context.types_compatible(&cond_type, &HirType::Bool) {
                    self.error(format!(
                        "If condition must be boolean, found {:?}",
                        cond_type
                    ));
                }

                let then_type = self.check_block(then_branch)?;
                if let Some(else_block) = else_branch {
                    let else_type = self.check_block(else_block)?;
                    if !self.context.types_compatible(&then_type, &else_type) {
                        self.warning(format!(
                            "If branches have different types: {:?} and {:?}",
                            then_type, else_type
                        ));
                    }
                    then_type
                } else {
                    then_type
                }
            }

            // Match expressions
            HirExprKind::Match {
                expr: match_expr,
                arms,
            } => {
                let match_type = self.check_expr(match_expr)?;
                let mut arm_types = Vec::new();

                for arm in arms.iter_mut() {
                    // Check pattern compatibility with match expression type
                    self.check_pattern_type(&arm.pattern, &match_type)?;

                    // Check guard if present
                    if let Some(guard) = &mut arm.guard {
                        let guard_type = self.check_expr(guard)?;
                        if !self.context.types_compatible(&guard_type, &HirType::Bool) {
                            self.error(format!(
                                "Match guard must be boolean, found {:?}",
                                guard_type
                            ));
                        }
                    }

                    // Check arm body type
                    let arm_type = match &mut arm.body {
                        veil_hir::HirMatchArmBody::Expr(expr) => self.check_expr(expr)?,
                        veil_hir::HirMatchArmBody::Block(block) => self.check_block(block)?,
                    };
                    arm_types.push(arm_type);
                }

                // All arms should have compatible types
                // Skip this check for statement-level matches (where first arm is Void)
                if let Some(first_type) = arm_types.first() {
                    let is_statement_level = matches!(first_type, HirType::Void);
                    for arm_type in &arm_types[1..] {
                        if !is_statement_level
                            && !self.context.types_compatible(first_type, arm_type)
                        {
                            self.warning(format!(
                                "Match arms have different types: {:?} and {:?}",
                                first_type, arm_type
                            ));
                        }
                    }
                    // Basic exhaustiveness check for union/intersection types
                    match &match_type {
                        HirType::Union(variants) => {
                            // If any wildcard or variable arm exists, it's exhaustive
                            let has_wildcard = arms.iter().any(|arm| {
                                matches!(
                                    *arm.pattern.kind,
                                    veil_hir::HirPatternKind::Wildcard
                                        | veil_hir::HirPatternKind::Variable(_)
                                )
                            });
                            if !has_wildcard {
                                let mut missing: Vec<HirType> = Vec::new();
                                for v in variants {
                                    let covered = arms.iter().any(|arm| {
                                        // Simple coverage check:
                                        // - wildcard/variable covers all
                                        // - literal covers primitives of matching kind
                                        // - struct/enum cover nominal matches
                                        // - tuple/array cover structural shapes
                                        fn covers(p: &veil_hir::HirPattern, ty: &HirType) -> bool {
                                            match &*p.kind {
                                                veil_hir::HirPatternKind::Wildcard => true,
                                                veil_hir::HirPatternKind::Variable(_) => true,
                                                veil_hir::HirPatternKind::Literal(expr) => {
                                                    match &*expr.kind {
                                                        HirExprKind::Int(_) => matches!(
                                                            ty,
                                                            HirType::I8
                                                                | HirType::I16
                                                                | HirType::I32
                                                                | HirType::I64
                                                                | HirType::U8
                                                                | HirType::U16
                                                                | HirType::U32
                                                                | HirType::U64
                                                        ),
                                                        HirExprKind::Float(_) => {
                                                            matches!(ty, HirType::F32 | HirType::F64)
                                                        }
                                                        HirExprKind::Bool(_) => {
                                                            matches!(ty, HirType::Bool)
                                                        }
                                                        HirExprKind::String(_) => {
                                                            matches!(ty, HirType::String)
                                                        }
                                                        HirExprKind::Char(_) => {
                                                            matches!(ty, HirType::Char)
                                                        }
                                                        HirExprKind::None => {
                                                            matches!(ty, HirType::Optional(_))
                                                        }
                                                        _ => false,
                                                    }
                                                }
                                                veil_hir::HirPatternKind::Struct { name, .. } => {
                                                    matches!(ty, HirType::Struct(n) if n == name)
                                                }
                                                veil_hir::HirPatternKind::EnumVariant { name, .. } => {
                                                    matches!(ty, HirType::Enum(n) if n == name)
                                                }
                                                veil_hir::HirPatternKind::Tuple(elems) => {
                                                    matches!(ty, HirType::Tuple(t_elems) if t_elems.len() == elems.len())
                                                }
                                                veil_hir::HirPatternKind::Array(_) => {
                                                    matches!(ty, HirType::Array(_) | HirType::SizedArray(_, _))
                                                }
                                                veil_hir::HirPatternKind::Range { .. } => {
                                                    matches!(
                                                        ty,
                                                        HirType::I8
                                                            | HirType::I16
                                                            | HirType::I32
                                                            | HirType::I64
                                                            | HirType::U8
                                                            | HirType::U16
                                                            | HirType::U32
                                                            | HirType::U64
                                                    )
                                                }
                                            }
                                        }
                                        covers(&arm.pattern, v)
                                    });
                                    if !covered {
                                        missing.push(v.clone());
                                    }
                                }
                                if !missing.is_empty() {
                                    self.warning(format!(
                                        "Non-exhaustive match on union type {:?}; missing cases: {:?}",
                                        match_type, missing
                                    ));
                                }
                            }
                        }
                        HirType::Intersection(parts) => {
                            // Intersection type exhaustiveness checking is complex and requires
                            // analysis of all component types. For now, we provide a specific warning.
                            let type_names: Vec<String> =
                                parts.iter().map(|t| format!("{:?}", t)).collect();
                            self.warning(format!(
                                "Exhaustiveness checking for intersection type {} is not yet fully supported. \
                                 Ensure all cases are handled manually.",
                                type_names.join(" & ")
                            ));
                        }
                        _ => {}
                    }
                    first_type.clone()
                } else {
                    HirType::Never // Empty match
                }
            }

            // Loop expression
            HirExprKind::Loop { body } => {
                let old_in_loop = self.context.in_loop;
                let old_break_types = std::mem::take(&mut self.context.break_types);
                self.context.in_loop = true;

                let _ = self.check_block(body)?;

                let break_types = std::mem::take(&mut self.context.break_types);
                let loop_type = if break_types.is_empty() {
                    HirType::Never
                } else {
                    let first_type = break_types[0].clone();
                    for bt in &break_types[1..] {
                        if !self.context.types_compatible(&first_type, bt) {
                            self.warning(format!(
                                "Inconsistent break types in loop: {:?} and {:?}",
                                first_type, bt
                            ));
                        }
                    }
                    first_type
                };

                self.context.in_loop = old_in_loop;
                self.context.break_types = old_break_types;
                loop_type
            }

            // While expression
            HirExprKind::While { condition, body } => {
                let cond_type = self.check_expr(condition)?;
                if !self.context.types_compatible(&cond_type, &HirType::Bool) {
                    self.error(format!(
                        "While condition must be boolean, found {:?}",
                        cond_type
                    ));
                }

                let old_in_loop = self.context.in_loop;
                let old_break_types = std::mem::take(&mut self.context.break_types);
                self.context.in_loop = true;

                let _ = self.check_block(body)?;

                let break_types = std::mem::take(&mut self.context.break_types);
                let loop_type = if break_types.is_empty() {
                    HirType::Void
                } else {
                    let first_type = break_types[0].clone();
                    for bt in &break_types[1..] {
                        if !self.context.types_compatible(&first_type, bt) {
                            self.warning(format!(
                                "Inconsistent break types in while: {:?} and {:?}",
                                first_type, bt
                            ));
                        }
                    }
                    first_type
                };

                self.context.in_loop = old_in_loop;
                self.context.break_types = old_break_types;
                loop_type
            }

            // For expression
            HirExprKind::For {
                pattern,
                iter,
                body,
            } => {
                let iter_type = self.check_expr(iter)?;
                let element_type = match iter_type {
                    HirType::Array(elem) => *elem,
                    HirType::Range => HirType::I32,
                    other => {
                        self.error(format!("Cannot iterate over type {:?}", other));
                        HirType::Unknown
                    }
                };

                self.check_pattern_type(pattern, &element_type)?;

                let old_in_loop = self.context.in_loop;
                let old_break_types = std::mem::take(&mut self.context.break_types);
                self.context.in_loop = true;

                let _ = self.check_block(body)?;

                let break_types = std::mem::take(&mut self.context.break_types);
                let for_type = if break_types.is_empty() {
                    HirType::Void
                } else {
                    let first_type = break_types[0].clone();
                    for bt in &break_types[1..] {
                        if !self.context.types_compatible(&first_type, bt) {
                            self.warning(format!(
                                "Inconsistent break types in for: {:?} and {:?}",
                                first_type, bt
                            ));
                        }
                    }
                    first_type
                };

                self.context.in_loop = old_in_loop;
                self.context.break_types = old_break_types;
                for_type
            }

            // Memory operations
            HirExprKind::Ref(inner) => {
                let t = self.check_expr(inner)?;
                HirType::Reference(Box::new(t), false)
            }
            HirExprKind::Deref(inner) => {
                let t = self.check_expr(inner)?;
                match t {
                    HirType::Reference(inner_t, _) | HirType::Pointer(inner_t) => {
                        (*inner_t).clone()
                    }
                    other => {
                        self.error(format!(
                            "Cannot dereference non-pointer/reference type: {:?}",
                            other
                        ));
                        HirType::Unknown
                    }
                }
            }
            HirExprKind::Cast { expr: inner, ty } => {
                let _ = self.check_expr(inner)?;
                ty.clone()
            }

            // Postfix '?': unwrap optional or emit error
            HirExprKind::PostfixQuestion(inner) => {
                let inner_ty = self.check_expr(inner)?;
                if let HirType::Optional(inner_ty) = inner_ty {
                    (*inner_ty).clone()
                } else {
                    self.error(format!(
                        "Postfix '?' requires an optional type, found {:?}",
                        inner_ty
                    ));
                    HirType::Unknown
                }
            }

            // Postfix inc/dec
            HirExprKind::PostfixIncrement(inner) | HirExprKind::PostfixDecrement(inner) => {
                let t = self.check_expr(inner)?;
                if self.is_numeric_type(&t) {
                    t
                } else {
                    self.error(format!(
                        "Postfix increment/decrement requires numeric operand, found {:?}",
                        t
                    ));
                    HirType::Unknown
                }
            }

            // Pipeline sugar (desugared in normalization) - type-check subparts
            HirExprKind::Pipeline { expr: piped, func } => {
                let _ = self.check_expr(piped)?;
                let _ = self.check_expr(func)?;
                HirType::Unknown
            }

            // Async/concurrency stubs
            HirExprKind::Await(inner) => {
                let _ = self.check_expr(inner)?;
                HirType::Unknown
            }
            HirExprKind::Spawn(block) => {
                let _ = self.check_block(block)?;
                HirType::Void
            }

            // Template strings
            HirExprKind::Template { parts } => {
                for p in parts {
                    if let veil_hir::HirTemplateStringPart::Expr(e) = p {
                        let _ = self.check_expr(e)?;
                    }
                }
                HirType::String
            }

            // Blocks and scopes
            HirExprKind::UnsafeBlock(block) => self.check_block(block)?,
            HirExprKind::Block(block) => self.check_block(block)?,
        };

        // Record computed type and attach a TypeId for this expression node
        self.context.set_node_type(expr.id, expr_type.clone());
        Ok(expr_type)
    }

    /// Check field access on a type
    fn check_field_access(
        &mut self,
        base_type: &HirType,
        field_name: &str,
        _location: NodeId,
    ) -> Result<HirType, Vec<Diag>> {
        match base_type {
            HirType::Struct(struct_name) => {
                // Without resolved field metadata, we can't infer field type here
                self.error(format!(
                    "Field '{}' not resolved on struct '{}'",
                    field_name, struct_name
                ));
                Ok(HirType::Unknown)
            }
            HirType::Tuple(elements) => {
                // Tuple field access by index (if field_name is numeric)
                if let Ok(index) = field_name.parse::<usize>() {
                    if index < elements.len() {
                        Ok(elements[index].clone())
                    } else {
                        self.error(format!(
                            "Tuple index {} out of bounds (length: {})",
                            index,
                            elements.len()
                        ));
                        Ok(HirType::Unknown)
                    }
                } else {
                    self.error(format!("Invalid tuple field name: {}", field_name));
                    Ok(HirType::Unknown)
                }
            }
            _ => {
                self.error(format!(
                    "Cannot access field '{}' on type {:?}",
                    field_name, base_type
                ));
                Ok(HirType::Unknown)
            }
        }
    }

    /// Check method call
    fn check_method_call(
        &mut self,
        receiver_type: &HirType,
        method_name: &str,
        _arg_types: &[HirType],
        _location: NodeId,
    ) -> Result<HirType, Vec<Diag>> {
        // Method resolution is not yet implemented; fallback to error
        self.error(format!(
            "Method '{}' not resolved on type {:?}",
            method_name, receiver_type
        ));
        Ok(HirType::Unknown)
    }

    /// Check function call
    fn check_function_call(
        &mut self,
        func_type: &HirType,
        arg_types: &[HirType],
        _location: NodeId,
    ) -> Result<HirType, Vec<Diag>> {
        match func_type {
            HirType::Function(param_types, return_type) => {
                if param_types.len() != arg_types.len() {
                    self.error(format!(
                        "Function expects {} arguments, found {}",
                        param_types.len(),
                        arg_types.len()
                    ));
                } else {
                    for (expected, actual) in param_types.iter().zip(arg_types) {
                        if !self.context.types_compatible(expected, actual) {
                            self.error(format!(
                                "Function argument type mismatch: expected {:?}, found {:?}",
                                expected, actual
                            ));
                        }
                    }
                }
                Ok((**return_type).clone())
            }
            _ => {
                self.error(format!("Cannot call non-function type: {:?}", func_type));
                Ok(HirType::Unknown)
            }
        }
    }

    /// Check binary operation
    fn check_binary_op(
        &mut self,
        op: HirBinaryOp,
        left_type: &HirType,
        right_type: &HirType,
        location: NodeId,
    ) -> Result<HirType, Vec<Diag>> {
        use HirBinaryOp::*;
        use HirType::*;

        match op {
            // Arithmetic operations
            Add | Sub | Mul => {
                if self.is_numeric_type(left_type) && self.is_numeric_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Arithmetic operation between incompatible types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    self.error(format!(
                        "Arithmetic operation on non-numeric types: {:?} and {:?}",
                        left_type, right_type
                    ));
                    Ok(Unknown)
                }
            }
            Div => {
                // '/' requires floating-point operands per spec; suggest '//' for integers
                let left_is_float = matches!(left_type, F32 | F64);
                let right_is_float = matches!(right_type, F32 | F64);
                let left_is_int = self.is_integer_type(left_type);
                let right_is_int = self.is_integer_type(right_type);
                if left_is_float && right_is_float {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Float division requires matching float types (no implicit coercion): left={:?}, right={:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    if left_is_int && right_is_int {
                        // Both integer operands: suggest using '//' for integer division
                        self.error_with_fix(
                            location,
                            Some("VE0010"),
                            format!(
                                "Float division '/' requires f32 or f64 operands; found integer types: left={:?}, right={:?}",
                                left_type, right_type
                            ),
                            "Use '//' for integer division, e.g. `a // b`",
                        );
                    } else {
                        // Mixed types (int/float): suggest casting the integer to a float
                        self.error_with_fix(
                            location,
                            Some("VE0011"),
                            format!(
                                "Float division '/' requires both operands to be floats (no implicit coercion): left={:?}, right={:?}",
                                left_type, right_type
                            ),
                            "Cast the integer operand to f32/f64, e.g. `x as f64 / y`",
                        );
                    }
                    Ok(Unknown)
                }
            }
            IDiv => {
                let left_is_int = self.is_integer_type(left_type);
                let right_is_int = self.is_integer_type(right_type);
                if left_is_int && right_is_int {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Integer division '//' between incompatible integer types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    let left_is_float = matches!(left_type, F32 | F64);
                    let right_is_float = matches!(right_type, F32 | F64);
                    if left_is_float && right_is_float {
                        self.error_with_fix(
                            location,
                            Some("VE0014"),
                            format!(
                                "Integer division '//' requires integer operands; found float types: left={:?}, right={:?}",
                                left_type, right_type
                            ),
                            "Use '/' for floating-point division, e.g. `a / b`",
                        );
                    } else {
                        self.error_with_fix(
                            location,
                            Some("VE0015"),
                            format!(
                                "Integer division '//' requires both operands to be integers (no implicit coercion): left={:?}, right={:?}",
                                left_type, right_type
                            ),
                            "Cast the float to an integer or use '/', e.g. `a as i32 // b` or `a / b`",
                        );
                    }
                    Ok(Unknown)
                }
            }
            Mod => {
                // '%' is defined for integers; enforce integer operands
                if self.is_integer_type(left_type) && self.is_integer_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Modulo between incompatible integer types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    self.error(format!(
                        "Modulo '%' requires integer operands (found left={:?}, right={:?})",
                        left_type, right_type
                    ));
                    Ok(Unknown)
                }
            }
            Pow => {
                if self.is_numeric_type(left_type) && self.is_numeric_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Power '**' between incompatible types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    self.error(format!(
                        "Power '**' requires numeric operands (found left={:?}, right={:?})",
                        left_type, right_type
                    ));
                    Ok(Unknown)
                }
            }

            // Bitwise operations (integers only)
            BitAnd | BitOr | BitXor | Shl | Shr => {
                if self.is_integer_type(left_type) && self.is_integer_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Bitwise operation between incompatible integer types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    self.error(format!(
                        "Bitwise operation requires integer operands (found left={:?}, right={:?})",
                        left_type, right_type
                    ));
                    Ok(Unknown)
                }
            }

            // Comparison operations
            Eq | Ne => {
                if self.context.types_compatible(left_type, right_type) {
                    Ok(Bool)
                } else {
                    self.error(format!(
                        "Cannot compare incompatible types: {:?} and {:?}",
                        left_type, right_type
                    ));
                    Ok(Bool) // Still return bool for error recovery
                }
            }

            Lt | Le | Gt | Ge => {
                // Allow numeric types and ranges
                let left_is_numeric = self.is_numeric_type(left_type);
                let right_is_numeric = self.is_numeric_type(right_type);
                let left_is_range = matches!(left_type, HirType::Range);
                let right_is_range = matches!(right_type, HirType::Range);
                let left_is_bool = matches!(left_type, HirType::Bool);
                let right_is_bool = matches!(right_type, HirType::Bool);
                let left_is_optional = matches!(left_type, HirType::Optional(_));
                let right_is_optional = matches!(right_type, HirType::Optional(_));

                if (left_is_numeric && right_is_numeric)
                    || (left_is_range && right_is_range)
                    || (left_is_range && right_is_numeric)
                    || (left_is_numeric && right_is_range)
                    || (left_is_bool && right_is_numeric)
                    || (left_is_numeric && right_is_bool)
                    || (left_is_optional && right_is_numeric)
                    || (left_is_numeric && right_is_optional)
                {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(Bool)
                    } else if left_is_numeric && right_is_numeric {
                        self.error(format!(
                            "Cannot compare incompatible numeric types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Bool)
                    } else {
                        self.error(format!(
                            "Cannot compare incompatible range types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Bool)
                    }
                } else {
                    self.error(format!(
                        "Cannot compare non-numeric/non-range types: {:?} and {:?}",
                        left_type, right_type
                    ));
                    Ok(Bool)
                }
            }

            // Logical operations
            And | Or => {
                if !self.context.types_compatible(left_type, &Bool) {
                    self.error(format!(
                        "Left operand of logical operation must be bool, found {:?}",
                        left_type
                    ));
                }
                if !self.context.types_compatible(right_type, &Bool) {
                    self.error(format!(
                        "Right operand of logical operation must be bool, found {:?}",
                        right_type
                    ));
                }
                Ok(Bool)
            }

            // Assignment operations: evaluate types, return the lhs type
            AddAssign | SubAssign | MulAssign => {
                if self.is_numeric_type(left_type) && self.is_numeric_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Assignment between incompatible numeric types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    self.error(format!(
                        "Assignment requires numeric operands (found left={:?}, right={:?})",
                        left_type, right_type
                    ));
                    Ok(Unknown)
                }
            }
            DivAssign => {
                let left_is_float = matches!(left_type, F32 | F64);
                let right_is_float = matches!(right_type, F32 | F64);
                let left_is_int = self.is_integer_type(left_type);
                let right_is_int = self.is_integer_type(right_type);
                if left_is_float && right_is_float {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Float '/=' requires matching float types: left={:?}, right={:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    if left_is_int && right_is_int {
                        self.error_with_fix(
                            location,
                            Some("VE0012"),
                            format!(
                                "Float '/=' requires f32 or f64 operands; found integer types: left={:?}, right={:?}",
                                left_type, right_type
                            ),
                            "Use '//=' for integer division assignment, e.g. `a //= b`",
                        );
                    } else {
                        self.error_with_fix(
                            location,
                            Some("VE0013"),
                            format!(
                                "Float '/=' requires both operands to be floats (no implicit coercion): left={:?}, right={:?}",
                                left_type, right_type
                            ),
                            "Cast the integer operand to f32/f64, e.g. `x as f64 /= y`",
                        );
                    }
                    Ok(Unknown)
                }
            }
            ModAssign => {
                if self.is_integer_type(left_type) && self.is_integer_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Modulo assignment between incompatible integer types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    self.error(format!(
                        "Modulo assignment requires integer operands (left={:?}, right={:?})",
                        left_type, right_type
                    ));
                    Ok(Unknown)
                }
            }
            BitAndAssign | BitOrAssign | BitXorAssign | ShlAssign | ShrAssign => {
                if self.is_integer_type(left_type) && self.is_integer_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(left_type.clone())
                    } else {
                        self.error(format!(
                            "Bitwise assignment between incompatible integer types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Unknown)
                    }
                } else {
                    self.error(format!(
                        "Bitwise assignment requires integer operands (left={:?}, right={:?})",
                        left_type, right_type
                    ));
                    Ok(Unknown)
                }
            }

            // Range constructors
            HirBinaryOp::Range
            | HirBinaryOp::RangeInclusive
            | HirBinaryOp::RangeExclusive
            | HirBinaryOp::RangeFrom => {
                // Allow None/Optional(Unknown) for infinite ranges, otherwise require integers
                let is_none_or_unknown = |ty: &HirType| matches!(ty, HirType::Optional(box_ty) if matches!(**box_ty, HirType::Unknown));

                if !self.is_integer_type(left_type) && !is_none_or_unknown(left_type) {
                    self.error(format!(
                        "Range start must be integer, found {:?}",
                        left_type
                    ));
                }
                if !matches!(op, HirBinaryOp::RangeFrom)
                    && !self.is_integer_type(right_type)
                    && !is_none_or_unknown(right_type)
                {
                    self.error(format!("Range end must be integer, found {:?}", right_type));
                }
                Ok(HirType::Range)
            }
        }
    }

    /// Check unary operation
    fn check_unary_op(
        &mut self,
        op: HirUnaryOp,
        operand_type: &HirType,
        _location: NodeId,
    ) -> Result<HirType, Vec<Diag>> {
        use HirType::*;
        use HirUnaryOp::*;

        match op {
            Not => {
                if self.context.types_compatible(operand_type, &Bool) {
                    Ok(Bool)
                } else {
                    self.error(format!(
                        "Logical not operation on non-boolean type: {:?}",
                        operand_type
                    ));
                    Ok(Bool)
                }
            }
            Minus | Plus => {
                if self.is_numeric_type(operand_type)
                    || matches!(operand_type, HirType::Optional(_))
                {
                    Ok(operand_type.clone())
                } else {
                    self.error(format!(
                        "Negation operation on non-numeric type: {:?}",
                        operand_type
                    ));
                    Ok(operand_type.clone())
                }
            }
            PreInc | PostInc | PreDec | PostDec => {
                if self.is_numeric_type(operand_type) {
                    Ok(operand_type.clone())
                } else {
                    self.error(format!(
                        "Increment/decrement operation on non-numeric type: {:?}",
                        operand_type
                    ));
                    Ok(operand_type.clone())
                }
            }
        }
    }

    /// Check array indexing
    fn check_array_index(
        &mut self,
        array_type: &HirType,
        index_type: &HirType,
        _location: NodeId,
    ) -> Result<HirType, Vec<Diag>> {
        // Index must be integer
        if !self.is_integer_type(index_type) {
            self.error(format!(
                "Array index must be integer, found {:?}",
                index_type
            ));
        }

        match array_type {
            HirType::Array(element_type) => Ok((**element_type).clone()),
            _ => {
                self.error(format!("Cannot index non-array type: {:?}", array_type));
                Ok(HirType::Unknown)
            }
        }
    }

    /// Check struct literal
    fn check_struct_literal(
        &mut self,
        struct_name: &str,
        fields: &mut [(String, HirExpr)],
        _location: NodeId,
    ) -> Result<HirType, Vec<Diag>> {
        for (_name, expr) in fields.iter_mut() {
            let _ = self.check_expr(expr)?;
        }
        Ok(HirType::Struct(struct_name.to_string()))
    }

    /// Check if a pattern is compatible with a type
    fn check_pattern_type(
        &mut self,
        pattern: &veil_hir::HirPattern,
        expected_type: &HirType,
    ) -> Result<(), Vec<Diag>> {
        self.check_pattern_against_type(pattern, expected_type)
    }

    /// Check if a type is numeric
    fn is_numeric_type(&self, ty: &HirType) -> bool {
        matches!(
            ty,
            HirType::I8
                | HirType::I16
                | HirType::I32
                | HirType::I64
                | HirType::U8
                | HirType::U16
                | HirType::U32
                | HirType::U64
                | HirType::F32
                | HirType::F64
        )
    }
}
