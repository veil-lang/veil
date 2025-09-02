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
            HirExprKind::Int(_) => HirType::I32, // Default integer literal type
            HirExprKind::Float(_) => HirType::F64, // Default float literal type
            HirExprKind::Bool(_) => HirType::Bool,
            HirExprKind::String(_) => HirType::String,
            HirExprKind::Char(_) => HirType::Char,
            HirExprKind::None => HirType::Optional(Box::new(HirType::Unknown)),

            // Variable references
            HirExprKind::Variable(name) => {
                if let Some(symbol) = self.context.get_symbol_by_name(name) {
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

                for arm in arms {
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
                if let Some(first_type) = arm_types.first() {
                    for arm_type in &arm_types[1..] {
                        if !self.context.types_compatible(first_type, arm_type) {
                            self.warning(format!(
                                "Match arms have different types: {:?} and {:?}",
                                first_type, arm_type
                            ));
                        }
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

        Ok(expr_type)
    }

    /// Check field access on a type
    fn check_field_access(
        &mut self,
        base_type: &HirType,
        field_name: &str,
        location: NodeId,
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
        arg_types: &[HirType],
        location: NodeId,
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
        location: NodeId,
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
        _location: NodeId,
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
                    self.error(format!(
                        "Float division '/' requires f32 or f64 operands; use '//' for integer division (found left={:?}, right={:?})",
                        left_type, right_type
                    ));
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
                if self.is_numeric_type(left_type) && self.is_numeric_type(right_type) {
                    if self.context.types_compatible(left_type, right_type) {
                        Ok(Bool)
                    } else {
                        self.error(format!(
                            "Cannot compare incompatible numeric types: {:?} and {:?}",
                            left_type, right_type
                        ));
                        Ok(Bool)
                    }
                } else {
                    self.error(format!(
                        "Cannot compare non-numeric types: {:?} and {:?}",
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
                    self.error(format!(
                        "Float '/=' requires f32 or f64 operands; use '//=' for integers (left={:?}, right={:?})",
                        left_type, right_type
                    ));
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
                if !self.is_integer_type(left_type) {
                    self.error(format!(
                        "Range start must be integer, found {:?}",
                        left_type
                    ));
                }
                if !matches!(op, HirBinaryOp::RangeFrom) && !self.is_integer_type(right_type) {
                    self.error(format!("Range end must be integer, found {:?}", right_type));
                }
                Ok(HirType::Range)
            }

            // Fallback for not-yet-implemented operators
            _ => {
                self.warning(format!(
                    "Operator {:?} not yet fully supported for types {:?} and {:?}",
                    op, left_type, right_type
                ));
                Ok(Unknown)
            }
        }
    }

    /// Check unary operation
    fn check_unary_op(
        &mut self,
        op: HirUnaryOp,
        operand_type: &HirType,
        location: NodeId,
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
                if self.is_numeric_type(operand_type) {
                    Ok(operand_type.clone())
                } else {
                    self.error(format!(
                        "Negation operation on non-numeric type: {:?}",
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
        location: NodeId,
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
        fields: &mut Vec<(String, HirExpr)>,
        location: NodeId,
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
        // This would be implemented in pattern.rs
        // For now, just accept all patterns
        Ok(())
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
