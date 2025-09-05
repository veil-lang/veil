//! HIR visitor patterns for traversing HIR nodes
//!
//! This module provides visitor traits and implementations for traversing
//! HIR data structures. Visitors are useful for analysis passes, transformations,
//! and code generation that need to walk the HIR tree.

use crate::nodes::*;

/// Trait for visiting HIR nodes in a read-only manner
pub trait HirVisitor {
    type Output;

    fn visit_program(&mut self, program: &HirProgram) -> Self::Output {
        self.walk_program(program)
    }

    fn visit_item(&mut self, item: &HirItem) -> Self::Output {
        self.walk_item(item)
    }

    fn visit_function(&mut self, function: &HirFunction) -> Self::Output {
        self.walk_function(function)
    }

    fn visit_struct(&mut self, struct_def: &HirStruct) -> Self::Output {
        self.walk_struct(struct_def)
    }

    fn visit_enum(&mut self, enum_def: &HirEnum) -> Self::Output {
        self.walk_enum(enum_def)
    }

    fn visit_impl(&mut self, impl_block: &HirImpl) -> Self::Output {
        self.walk_impl(impl_block)
    }

    fn visit_stmt(&mut self, stmt: &HirStmt) -> Self::Output {
        self.walk_stmt(stmt)
    }

    fn visit_expr(&mut self, expr: &HirExpr) -> Self::Output {
        self.walk_expr(expr)
    }

    fn visit_pattern(&mut self, pattern: &HirPattern) -> Self::Output {
        self.walk_pattern(pattern)
    }

    fn visit_type(&mut self, ty: &HirType) -> Self::Output {
        self.walk_type(ty)
    }

    fn visit_block(&mut self, block: &HirBlock) -> Self::Output {
        self.walk_block(block)
    }

    // Default walking implementations
    fn walk_program(&mut self, program: &HirProgram) -> Self::Output {
        for item in &program.items {
            self.visit_item(item);
        }
        self.default_output()
    }

    fn walk_item(&mut self, item: &HirItem) -> Self::Output {
        match &item.kind {
            HirItemKind::Function(func) => self.visit_function(func),
            HirItemKind::Struct(struct_def) => self.visit_struct(struct_def),
            HirItemKind::Enum(enum_def) => self.visit_enum(enum_def),
            HirItemKind::Impl(impl_block) => self.visit_impl(impl_block),
            HirItemKind::FfiFunction(_) => self.default_output(),
            HirItemKind::FfiVariable(_) => self.default_output(),
            HirItemKind::Test(test) => self.visit_block(&test.body),
            HirItemKind::Import(_) => self.default_output(),
        }
    }

    fn walk_function(&mut self, function: &HirFunction) -> Self::Output {
        for param in &function.params {
            self.visit_type(&param.ty);
        }
        self.visit_type(&function.return_type);
        self.visit_block(&function.body)
    }

    fn walk_struct(&mut self, struct_def: &HirStruct) -> Self::Output {
        for field in &struct_def.fields {
            self.visit_type(&field.ty);
        }
        self.default_output()
    }

    fn walk_enum(&mut self, enum_def: &HirEnum) -> Self::Output {
        for variant in &enum_def.variants {
            if let Some(ref data) = variant.data {
                match data {
                    HirEnumVariantData::Tuple(types) => {
                        for ty in types {
                            self.visit_type(ty);
                        }
                    }
                    HirEnumVariantData::Struct(fields) => {
                        for field in fields {
                            self.visit_type(&field.ty);
                        }
                    }
                }
            }
            if let Some(ref discriminant) = variant.discriminant {
                self.visit_expr(discriminant);
            }
        }
        self.default_output()
    }

    fn walk_impl(&mut self, impl_block: &HirImpl) -> Self::Output {
        self.visit_type(&impl_block.target_type);
        for method in &impl_block.methods {
            self.visit_function(method);
        }
        self.default_output()
    }

    fn walk_stmt(&mut self, stmt: &HirStmt) -> Self::Output {
        match &stmt.kind {
            HirStmtKind::Expr(expr) => self.visit_expr(expr),
            HirStmtKind::Let { pattern, ty, init } => {
                self.visit_pattern(pattern);
                if let Some(ty) = ty {
                    self.visit_type(ty);
                }
                if let Some(init) = init {
                    self.visit_expr(init);
                }
                self.default_output()
            }
            HirStmtKind::Assign { lhs, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.default_output()
            }
            HirStmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
                self.default_output()
            }
            HirStmtKind::Break(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
                self.default_output()
            }
            HirStmtKind::Continue => self.default_output(),
        }
    }

    fn walk_expr(&mut self, expr: &HirExpr) -> Self::Output {
        match expr.kind.as_ref() {
            HirExprKind::Int(_)
            | HirExprKind::Float(_)
            | HirExprKind::Bool(_)
            | HirExprKind::String(_)
            | HirExprKind::Char(_)
            | HirExprKind::None
            | HirExprKind::Void
            | HirExprKind::Variable { .. } => self.default_output(),

            HirExprKind::FieldAccess { base, .. } => self.visit_expr(base),

            HirExprKind::Index { base, index } => {
                self.visit_expr(base);
                self.visit_expr(index);
                self.default_output()
            }

            HirExprKind::Call { func, args } => {
                self.visit_expr(func);
                for arg in args {
                    self.visit_expr(arg);
                }
                self.default_output()
            }

            HirExprKind::MethodCall { receiver, args, .. } => {
                self.visit_expr(receiver);
                for arg in args {
                    self.visit_expr(arg);
                }
                self.default_output()
            }

            HirExprKind::Unary { expr, .. } => self.visit_expr(expr),

            HirExprKind::Binary { lhs, rhs, .. } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.default_output()
            }

            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.visit_expr(condition);
                self.visit_block(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_block(else_branch);
                }
                self.default_output()
            }

            HirExprKind::Match { expr, arms } => {
                self.visit_expr(expr);
                for arm in arms {
                    self.visit_pattern(&arm.pattern);
                    if let Some(ref guard) = arm.guard {
                        self.visit_expr(guard);
                    }
                    match &arm.body {
                        HirMatchArmBody::Expr(expr) => {
                            self.visit_expr(expr);
                        }
                        HirMatchArmBody::Block(block) => {
                            self.visit_block(block);
                        }
                    }
                }
                self.default_output()
            }

            HirExprKind::Loop { body } => self.visit_block(body),

            HirExprKind::While { condition, body } => {
                self.visit_expr(condition);
                self.visit_block(body);
                self.default_output()
            }

            HirExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.visit_pattern(pattern);
                self.visit_expr(iter);
                self.visit_block(body);
                self.default_output()
            }

            HirExprKind::Ref(expr) | HirExprKind::Deref(expr) => self.visit_expr(expr),

            HirExprKind::Cast { expr, ty } => {
                self.visit_expr(expr);
                self.visit_type(ty);
                self.default_output()
            }

            HirExprKind::StructLiteral { fields, .. } => {
                for field in fields {
                    let expr = &field.1;
                    self.visit_expr(expr);
                }
                self.default_output()
            }

            HirExprKind::ArrayLiteral(exprs) | HirExprKind::TupleLiteral(exprs) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
                self.default_output()
            }

            HirExprKind::Block(block) | HirExprKind::UnsafeBlock(block) => self.visit_block(block),

            HirExprKind::Await(expr) => self.visit_expr(expr),

            HirExprKind::Spawn(block) => self.visit_block(block),

            HirExprKind::Pipeline { expr, func } => {
                self.visit_expr(expr);
                self.visit_expr(func);
                self.default_output()
            }

            HirExprKind::PostfixQuestion(expr)
            | HirExprKind::PostfixIncrement(expr)
            | HirExprKind::PostfixDecrement(expr) => self.visit_expr(expr),

            HirExprKind::Template { parts } => {
                for part in parts {
                    if let HirTemplateStringPart::Expr(expr) = part {
                        self.visit_expr(expr);
                    }
                }
                self.default_output()
            }
        }
    }

    fn walk_pattern(&mut self, pattern: &HirPattern) -> Self::Output {
        match pattern.kind.as_ref() {
            HirPatternKind::Wildcard | HirPatternKind::Variable { .. } => self.default_output(),
            HirPatternKind::Literal(expr) => self.visit_expr(expr),
            HirPatternKind::EnumVariant { patterns, .. } => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
                self.default_output()
            }
            HirPatternKind::Struct { fields, .. } => {
                for field in fields {
                    let pattern = &field.1;
                    self.visit_pattern(pattern);
                }
                self.default_output()
            }
            HirPatternKind::Tuple(patterns) | HirPatternKind::Array(patterns) => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
                self.default_output()
            }
            HirPatternKind::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.visit_expr(start);
                }
                if let Some(end) = end {
                    self.visit_expr(end);
                }
                self.default_output()
            }
        }
    }

    fn walk_type(&mut self, ty: &HirType) -> Self::Output {
        match ty {
            HirType::Pointer(inner) | HirType::Array(inner) | HirType::Optional(inner) => {
                self.visit_type(inner)
            }
            HirType::SizedArray(inner, _) => self.visit_type(inner),
            HirType::Function(params, ret) => {
                for param in params {
                    self.visit_type(param);
                }
                self.visit_type(ret);
                self.default_output()
            }
            HirType::GenericInstance(_, args) => {
                for arg in args {
                    self.visit_type(arg);
                }
                self.default_output()
            }
            HirType::Union(types) | HirType::Intersection(types) => {
                for ty in types {
                    self.visit_type(ty);
                }
                self.default_output()
            }
            _ => self.default_output(),
        }
    }

    fn walk_block(&mut self, block: &HirBlock) -> Self::Output {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(ref expr) = block.expr {
            self.visit_expr(expr);
        }
        self.default_output()
    }

    fn default_output(&self) -> Self::Output;
}

/// Trait for visiting HIR nodes in a mutable manner
pub trait HirVisitorMut {
    type Output;

    fn visit_program(&mut self, program: &mut HirProgram) -> Self::Output {
        self.walk_program(program)
    }

    fn visit_item(&mut self, item: &mut HirItem) -> Self::Output {
        self.walk_item(item)
    }

    fn visit_function(&mut self, function: &mut HirFunction) -> Self::Output {
        self.walk_function(function)
    }

    fn visit_struct(&mut self, struct_def: &mut HirStruct) -> Self::Output {
        self.walk_struct(struct_def)
    }

    fn visit_enum(&mut self, enum_def: &mut HirEnum) -> Self::Output {
        self.walk_enum(enum_def)
    }

    fn visit_impl(&mut self, impl_block: &mut HirImpl) -> Self::Output {
        self.walk_impl(impl_block)
    }

    fn visit_stmt(&mut self, stmt: &mut HirStmt) -> Self::Output {
        self.walk_stmt(stmt)
    }

    fn visit_expr(&mut self, expr: &mut HirExpr) -> Self::Output {
        self.walk_expr(expr)
    }

    fn visit_pattern(&mut self, pattern: &mut HirPattern) -> Self::Output {
        self.walk_pattern(pattern)
    }

    fn visit_type(&mut self, ty: &mut HirType) -> Self::Output {
        self.walk_type(ty)
    }

    fn visit_block(&mut self, block: &mut HirBlock) -> Self::Output {
        self.walk_block(block)
    }

    // Default walking implementations
    fn walk_program(&mut self, program: &mut HirProgram) -> Self::Output {
        for item in &mut program.items {
            self.visit_item(item);
        }
        self.default_output()
    }

    fn walk_item(&mut self, item: &mut HirItem) -> Self::Output {
        match &mut item.kind {
            HirItemKind::Function(func) => self.visit_function(func),
            HirItemKind::Struct(struct_def) => self.visit_struct(struct_def),
            HirItemKind::Enum(enum_def) => self.visit_enum(enum_def),
            HirItemKind::Impl(impl_block) => self.visit_impl(impl_block),
            HirItemKind::FfiFunction(_) => self.default_output(),
            HirItemKind::FfiVariable(_) => self.default_output(),
            HirItemKind::Test(test) => self.visit_block(&mut test.body),
            HirItemKind::Import(_) => self.default_output(),
        }
    }

    fn walk_function(&mut self, function: &mut HirFunction) -> Self::Output {
        for param in &mut function.params {
            self.visit_type(&mut param.ty);
        }
        self.visit_type(&mut function.return_type);
        self.visit_block(&mut function.body)
    }

    fn walk_struct(&mut self, struct_def: &mut HirStruct) -> Self::Output {
        for field in &mut struct_def.fields {
            self.visit_type(&mut field.ty);
        }
        self.default_output()
    }

    fn walk_enum(&mut self, enum_def: &mut HirEnum) -> Self::Output {
        for variant in &mut enum_def.variants {
            if let Some(ref mut data) = variant.data {
                match data {
                    HirEnumVariantData::Tuple(types) => {
                        for ty in types {
                            self.visit_type(ty);
                        }
                    }
                    HirEnumVariantData::Struct(fields) => {
                        for field in fields {
                            self.visit_type(&mut field.ty);
                        }
                    }
                }
            }
            if let Some(ref mut discriminant) = variant.discriminant {
                self.visit_expr(discriminant);
            }
        }
        self.default_output()
    }

    fn walk_impl(&mut self, impl_block: &mut HirImpl) -> Self::Output {
        self.visit_type(&mut impl_block.target_type);
        for method in &mut impl_block.methods {
            self.visit_function(method);
        }
        self.default_output()
    }

    fn walk_stmt(&mut self, stmt: &mut HirStmt) -> Self::Output {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => self.visit_expr(expr),
            HirStmtKind::Let { pattern, ty, init } => {
                self.visit_pattern(pattern);
                if let Some(ty) = ty {
                    self.visit_type(ty);
                }
                if let Some(init) = init {
                    self.visit_expr(init);
                }
                self.default_output()
            }
            HirStmtKind::Assign { lhs, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.default_output()
            }
            HirStmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
                self.default_output()
            }
            HirStmtKind::Break(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
                self.default_output()
            }
            HirStmtKind::Continue => self.default_output(),
        }
    }

    fn walk_expr(&mut self, expr: &mut HirExpr) -> Self::Output {
        match expr.kind.as_mut() {
            HirExprKind::Int(_)
            | HirExprKind::Float(_)
            | HirExprKind::Bool(_)
            | HirExprKind::String(_)
            | HirExprKind::Char(_)
            | HirExprKind::None
            | HirExprKind::Void
            | HirExprKind::Variable { .. } => self.default_output(),

            HirExprKind::FieldAccess { base, .. } => self.visit_expr(base),

            HirExprKind::Index { base, index } => {
                self.visit_expr(base);
                self.visit_expr(index);
                self.default_output()
            }

            HirExprKind::Call { func, args } => {
                self.visit_expr(func);
                for arg in args {
                    self.visit_expr(arg);
                }
                self.default_output()
            }

            HirExprKind::MethodCall { receiver, args, .. } => {
                self.visit_expr(receiver);
                for arg in args {
                    self.visit_expr(arg);
                }
                self.default_output()
            }

            HirExprKind::Unary { expr, .. } => self.visit_expr(expr),

            HirExprKind::Binary { lhs, rhs, .. } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.default_output()
            }

            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.visit_expr(condition);
                self.visit_block(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_block(else_branch);
                }
                self.default_output()
            }

            HirExprKind::Match { expr, arms } => {
                self.visit_expr(expr);
                for arm in arms {
                    self.visit_pattern(&mut arm.pattern);
                    if let Some(guard) = &mut arm.guard {
                        self.visit_expr(guard);
                    }
                    match &mut arm.body {
                        HirMatchArmBody::Expr(expr) => {
                            self.visit_expr(expr);
                        }
                        HirMatchArmBody::Block(block) => {
                            self.visit_block(block);
                        }
                    }
                }
                self.default_output()
            }

            HirExprKind::Loop { body } => self.visit_block(body),

            HirExprKind::While { condition, body } => {
                self.visit_expr(condition);
                self.visit_block(body);
                self.default_output()
            }

            HirExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.visit_pattern(pattern);
                self.visit_expr(iter);
                self.visit_block(body);
                self.default_output()
            }

            HirExprKind::Ref(expr) | HirExprKind::Deref(expr) => self.visit_expr(expr),

            HirExprKind::Cast { expr, ty } => {
                self.visit_expr(expr);
                self.visit_type(ty);
                self.default_output()
            }

            HirExprKind::StructLiteral { fields, .. } => {
                for field in fields {
                    let expr = &mut field.1;
                    self.visit_expr(expr);
                }
                self.default_output()
            }

            HirExprKind::ArrayLiteral(exprs) | HirExprKind::TupleLiteral(exprs) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
                self.default_output()
            }

            HirExprKind::Block(block) | HirExprKind::UnsafeBlock(block) => self.visit_block(block),

            HirExprKind::Await(expr) => self.visit_expr(expr),

            HirExprKind::Spawn(block) => self.visit_block(block),

            HirExprKind::Pipeline { expr, func } => {
                self.visit_expr(expr);
                self.visit_expr(func);
                self.default_output()
            }

            HirExprKind::PostfixQuestion(expr)
            | HirExprKind::PostfixIncrement(expr)
            | HirExprKind::PostfixDecrement(expr) => self.visit_expr(expr),

            HirExprKind::Template { parts } => {
                for part in parts {
                    if let HirTemplateStringPart::Expr(expr) = part {
                        self.visit_expr(expr);
                    }
                }
                self.default_output()
            }
        }
    }

    fn walk_pattern(&mut self, pattern: &mut HirPattern) -> Self::Output {
        match pattern.kind.as_mut() {
            HirPatternKind::Wildcard | HirPatternKind::Variable { .. } => self.default_output(),
            HirPatternKind::Literal(expr) => self.visit_expr(expr),
            HirPatternKind::EnumVariant { patterns, .. } => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
                self.default_output()
            }
            HirPatternKind::Struct { fields, .. } => {
                for field in fields {
                    let pattern = &mut field.1;
                    self.visit_pattern(pattern);
                }
                self.default_output()
            }
            HirPatternKind::Tuple(patterns) | HirPatternKind::Array(patterns) => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
                self.default_output()
            }
            HirPatternKind::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.visit_expr(start);
                }
                if let Some(end) = end {
                    self.visit_expr(end);
                }
                self.default_output()
            }
        }
    }

    fn walk_type(&mut self, ty: &mut HirType) -> Self::Output {
        match ty {
            HirType::Pointer(inner) | HirType::Array(inner) | HirType::Optional(inner) => {
                self.visit_type(inner)
            }
            HirType::SizedArray(inner, _) => self.visit_type(inner),
            HirType::Function(params, ret) => {
                for param in params {
                    self.visit_type(param);
                }
                self.visit_type(ret);
                self.default_output()
            }
            HirType::GenericInstance(_, args) => {
                for arg in args {
                    self.visit_type(arg);
                }
                self.default_output()
            }
            HirType::Union(types) | HirType::Intersection(types) => {
                for ty in types {
                    self.visit_type(ty);
                }
                self.default_output()
            }
            _ => self.default_output(),
        }
    }

    fn walk_block(&mut self, block: &mut HirBlock) -> Self::Output {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(expr) = &mut block.expr {
            self.visit_expr(expr);
        }
        self.default_output()
    }

    fn default_output(&self) -> Self::Output;
}

/// A simple visitor that collects all variable names
pub struct VariableCollector {
    pub variables: Vec<String>,
}

impl VariableCollector {
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
        }
    }
}

impl Default for VariableCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl HirVisitor for VariableCollector {
    type Output = ();

    fn visit_expr(&mut self, expr: &HirExpr) -> Self::Output {
        if let HirExprKind::Variable(name) = expr.kind.as_ref() {
            self.variables.push(name.clone());
        }
        self.walk_expr(expr)
    }

    fn visit_pattern(&mut self, pattern: &HirPattern) -> Self::Output {
        if let HirPatternKind::Variable(name) = pattern.kind.as_ref() {
            self.variables.push(name.clone());
        }
        self.walk_pattern(pattern)
    }

    fn default_output(&self) -> Self::Output {}
}

/// A visitor that counts different types of HIR nodes
#[derive(Debug, Default)]
pub struct NodeCounter {
    pub functions: usize,
    pub structs: usize,
    pub enums: usize,
    pub expressions: usize,
    pub statements: usize,
}

impl HirVisitor for NodeCounter {
    type Output = ();

    fn visit_function(&mut self, function: &HirFunction) -> Self::Output {
        self.functions += 1;
        self.walk_function(function)
    }

    fn visit_struct(&mut self, struct_def: &HirStruct) -> Self::Output {
        self.structs += 1;
        self.walk_struct(struct_def)
    }

    fn visit_enum(&mut self, enum_def: &HirEnum) -> Self::Output {
        self.enums += 1;
        self.walk_enum(enum_def)
    }

    fn visit_expr(&mut self, expr: &HirExpr) -> Self::Output {
        self.expressions += 1;
        self.walk_expr(expr)
    }

    fn visit_stmt(&mut self, stmt: &HirStmt) -> Self::Output {
        self.statements += 1;
        self.walk_stmt(stmt)
    }

    fn default_output(&self) -> Self::Output {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ids::NodeId;

    #[test]
    fn test_variable_collector() {
        let mut collector = VariableCollector::new();

        let expr = HirExpr {
            id: NodeId::new(0),
            kind: Box::new(HirExprKind::Variable("test_var".to_string())),
        };

        collector.visit_expr(&expr);
        assert_eq!(collector.variables, vec!["test_var"]);
    }

    #[test]
    fn test_node_counter() {
        let mut counter = NodeCounter::default();

        let expr = HirExpr {
            id: NodeId::new(0),
            kind: Box::new(HirExprKind::Int(42)),
        };

        let stmt = HirStmt {
            id: NodeId::new(1),
            kind: HirStmtKind::Expr(expr),
        };

        counter.visit_stmt(&stmt);
        assert_eq!(counter.statements, 1);
        assert_eq!(counter.expressions, 1);
    }
}
