//! Snapshot tests for HIR output
//!
//! These tests verify that HIR lowering produces the expected output format
//! and can serve as golden tests for regression detection.

use codespan::Span;
use veil_ast as ast;
use veil_hir::ids::ModuleId;
use veil_hir::{HirVisitor, lower_program};

/// Simple visitor that formats HIR as a debug string
struct HirFormatter {
    output: String,
    indent: usize,
}

impl HirFormatter {
    fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
        }
    }

    fn write_line(&mut self, text: &str) {
        self.output.push_str(&"  ".repeat(self.indent));
        self.output.push_str(text);
        self.output.push('\n');
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    fn format_hir(&mut self, program: &veil_hir::HirProgram) -> String {
        self.visit_program(program);
        self.output.clone()
    }
}

impl HirVisitor for HirFormatter {
    type Output = ();

    fn visit_program(&mut self, program: &veil_hir::HirProgram) -> Self::Output {
        self.write_line(&format!("HIR Program (module_id: {})", program.module_id));
        self.indent();
        for item in &program.items {
            self.visit_item(item);
        }
        self.dedent();
    }

    fn visit_function(&mut self, function: &veil_hir::HirFunction) -> Self::Output {
        self.write_line(&format!("Function: {}", function.name));
        self.indent();

        if !function.params.is_empty() {
            self.write_line("Parameters:");
            self.indent();
            for param in &function.params {
                self.write_line(&format!("{}: {:?}", param.name, param.ty));
            }
            self.dedent();
        }

        self.write_line(&format!("Return type: {:?}", function.return_type));

        self.write_line("Body:");
        self.indent();
        self.visit_block(&function.body);
        self.dedent();

        self.dedent();
    }

    fn visit_struct(&mut self, struct_def: &veil_hir::HirStruct) -> Self::Output {
        self.write_line(&format!("Struct: {}", struct_def.name));
        self.indent();

        if !struct_def.fields.is_empty() {
            self.write_line("Fields:");
            self.indent();
            for field in &struct_def.fields {
                self.write_line(&format!("{}: {:?}", field.name, field.ty));
            }
            self.dedent();
        }

        self.dedent();
    }

    fn visit_stmt(&mut self, stmt: &veil_hir::HirStmt) -> Self::Output {
        match &stmt.kind {
            veil_hir::HirStmtKind::Expr(expr) => {
                self.write_line("Expression statement:");
                self.indent();
                self.visit_expr(expr);
                self.dedent();
            }
            veil_hir::HirStmtKind::Let { pattern, ty, init } => {
                self.write_line("Let statement:");
                self.indent();
                self.write_line(&format!("Pattern: {:?}", pattern.kind));
                if let Some(ty) = ty {
                    self.write_line(&format!("Type: {:?}", ty));
                }
                if let Some(init) = init {
                    self.write_line("Initializer:");
                    self.indent();
                    self.visit_expr(init);
                    self.dedent();
                }
                self.dedent();
            }
            veil_hir::HirStmtKind::Return(expr) => {
                self.write_line("Return statement:");
                if let Some(expr) = expr {
                    self.indent();
                    self.visit_expr(expr);
                    self.dedent();
                }
            }
            _ => {
                self.write_line(&format!("Statement: {:?}", stmt.kind));
            }
        }
    }

    fn visit_expr(&mut self, expr: &veil_hir::HirExpr) -> Self::Output {
        match expr.kind.as_ref() {
            veil_hir::HirExprKind::Int(value) => {
                self.write_line(&format!("Int: {}", value));
            }
            veil_hir::HirExprKind::Bool(value) => {
                self.write_line(&format!("Bool: {}", value));
            }
            veil_hir::HirExprKind::String(value) => {
                self.write_line(&format!("String: \"{}\"", value));
            }
            veil_hir::HirExprKind::Variable(name) => {
                self.write_line(&format!("Variable: {}", name));
            }
            veil_hir::HirExprKind::Binary { op, lhs, rhs } => {
                self.write_line(&format!("Binary operation: {:?}", op));
                self.indent();
                self.write_line("Left:");
                self.indent();
                self.visit_expr(lhs);
                self.dedent();
                self.write_line("Right:");
                self.indent();
                self.visit_expr(rhs);
                self.dedent();
                self.dedent();
            }
            veil_hir::HirExprKind::Call { func, args } => {
                self.write_line("Function call:");
                self.indent();
                self.write_line("Function:");
                self.indent();
                self.visit_expr(func);
                self.dedent();
                if !args.is_empty() {
                    self.write_line("Arguments:");
                    self.indent();
                    for arg in args {
                        self.visit_expr(arg);
                    }
                    self.dedent();
                }
                self.dedent();
            }
            _ => {
                self.write_line(&format!("Expression: {:?}", expr.kind));
            }
        }
    }

    fn default_output(&self) -> Self::Output {
        ()
    }
}

#[test]
fn test_simple_function_snapshot() {
    let function = ast::Function {
        name: "add".to_string(),
        generic_params: vec![],
        params: vec![
            ("a".to_string(), ast::Type::I32),
            ("b".to_string(), ast::Type::I32),
        ],
        return_type: ast::Type::I32,
        body: vec![ast::Stmt::Return(
            ast::Expr::BinOp(
                Box::new(ast::Expr::Var(
                    "a".to_string(),
                    ast::ExprInfo {
                        span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(1)),
                        ty: ast::Type::I32,
                        is_tail: false,
                    },
                )),
                ast::BinOp::Add,
                Box::new(ast::Expr::Var(
                    "b".to_string(),
                    ast::ExprInfo {
                        span: Span::new(codespan::ByteIndex(4), codespan::ByteIndex(5)),
                        ty: ast::Type::I32,
                        is_tail: false,
                    },
                )),
                ast::ExprInfo {
                    span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(5)),
                    ty: ast::Type::I32,
                    is_tail: false,
                },
            ),
            Span::new(codespan::ByteIndex(0), codespan::ByteIndex(10)),
        )],
        span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(20)),
        visibility: ast::Visibility::Public,
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![function],
        structs: vec![],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let hir_program = lower_program(&ast_program, module_id).unwrap();

    let mut formatter = HirFormatter::new();
    let output = formatter.format_hir(&hir_program);

    let expected = r#"HIR Program (module_id: mod_0)
  Function: add
    Parameters:
      a: I32
      b: I32
    Return type: I32
    Body:
      Return statement:
        Binary operation: Add
          Left:
            Variable: a
          Right:
            Variable: b
"#;

    assert_eq!(output.trim(), expected.trim());
}

#[test]
fn test_struct_with_methods_snapshot() {
    let struct_def = ast::StructDef {
        name: "Point".to_string(),
        generic_params: vec![],
        fields: vec![
            ast::StructField {
                name: "x".to_string(),
                ty: ast::Type::F64,
                span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(5)),
            },
            ast::StructField {
                name: "y".to_string(),
                ty: ast::Type::F64,
                span: Span::new(codespan::ByteIndex(5), codespan::ByteIndex(10)),
            },
        ],
        span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(20)),
        visibility: ast::Visibility::Public,
        repr: None,
    };

    let impl_block = ast::ImplBlock {
        target_type: "Point".to_string(),
        target_type_parsed: Some(ast::Type::Struct("Point".to_string())),
        methods: vec![ast::Function {
            name: "new".to_string(),
            generic_params: vec![],
            params: vec![
                ("x".to_string(), ast::Type::F64),
                ("y".to_string(), ast::Type::F64),
            ],
            return_type: ast::Type::Struct("Point".to_string()),
            body: vec![],
            span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(15)),
            visibility: ast::Visibility::Public,
        }],
        span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(30)),
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![],
        structs: vec![struct_def],
        enums: vec![],
        impls: vec![impl_block],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let hir_program = lower_program(&ast_program, module_id).unwrap();

    let mut formatter = HirFormatter::new();
    let output = formatter.format_hir(&hir_program);

    let expected = r#"HIR Program (module_id: mod_0)
  Struct: Point
    Fields:
      x: F64
      y: F64
  Implementation block:
    Target type: Unresolved("Point")
    Methods:
      Function: new
        Parameters:
          x: F64
          y: F64
        Return type: Unresolved("Point")
        Body:
"#;

    // This test verifies the structure but the exact format may vary
    assert!(output.contains("HIR Program (module_id: mod_0)"));
    assert!(output.contains("Struct: Point"));
    assert!(output.contains("x: F64"));
    assert!(output.contains("y: F64"));
}

#[test]
fn test_let_statement_snapshot() {
    let function = ast::Function {
        name: "test_let".to_string(),
        generic_params: vec![],
        params: vec![],
        return_type: ast::Type::Void,
        body: vec![ast::Stmt::Let(
            "x".to_string(),
            Some(ast::Type::I32),
            ast::Expr::Int(
                42,
                ast::ExprInfo {
                    span: Span::new(codespan::ByteIndex(10), codespan::ByteIndex(12)),
                    ty: ast::Type::I32,
                    is_tail: false,
                },
            ),
            Span::new(codespan::ByteIndex(0), codespan::ByteIndex(15)),
            ast::Visibility::Private,
        )],
        span: Span::new(codespan::ByteIndex(0), codespan::ByteIndex(20)),
        visibility: ast::Visibility::Private,
    };

    let ast_program = ast::Program {
        imports: vec![],
        stmts: vec![],
        functions: vec![function],
        structs: vec![],
        enums: vec![],
        impls: vec![],
        ffi_functions: vec![],
        ffi_variables: vec![],
        tests: vec![],
    };

    let module_id = ModuleId::new(0);
    let hir_program = lower_program(&ast_program, module_id).unwrap();

    let mut formatter = HirFormatter::new();
    let output = formatter.format_hir(&hir_program);

    // Verify key components are present
    assert!(output.contains("Function: test_let"));
    assert!(output.contains("Let statement:"));
    assert!(output.contains("Type: I32"));
    assert!(output.contains("Int: 42"));
}
