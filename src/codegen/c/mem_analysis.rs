use crate::ast;
use crate::ast::Type;
use crate::codegen::c::CBackend;

impl CBackend {
    pub fn analyze_memory_requirements(&mut self, program: &ast::Program) {
        self.memory_analysis.total_functions = program.functions.len();
        for func in &program.functions {
            let depth = self.analyze_function_memory(&func.body);
            self.memory_analysis.max_function_depth =
                self.memory_analysis.max_function_depth.max(depth);
        }
        for stmt in &program.stmts {
            self.analyze_stmt_memory(stmt);
        }
        self.calculate_arena_size();
    }

    pub fn analyze_function_memory(&mut self, stmts: &[ast::Stmt]) -> usize {
        let mut depth = 0;
        for stmt in stmts {
            depth = depth.max(self.analyze_stmt_memory(stmt));
        }
        depth
    }

    pub fn analyze_stmt_memory(&mut self, stmt: &ast::Stmt) -> usize {
        match stmt {
            ast::Stmt::Let(_, ty, expr, _, _) => {
                if let Some(ty) = ty {
                    self.estimate_type_size(ty);
                }
                self.analyze_expr_memory(expr)
            }
            ast::Stmt::Expr(expr, _) => self.analyze_expr_memory(expr),
            ast::Stmt::Block(stmts, _) => {
                let mut max_depth = 0;
                for stmt in stmts {
                    max_depth = max_depth.max(self.analyze_stmt_memory(stmt));
                }
                max_depth + 1
            }
            ast::Stmt::While(_, body, _) | ast::Stmt::For(_, _, _, _, body, _) => {
                let mut max_depth = 0;
                for stmt in body {
                    max_depth = max_depth.max(self.analyze_stmt_memory(stmt));
                }
                max_depth + 1
            }
            ast::Stmt::Break(_, _) => 0,
            ast::Stmt::Continue(_) => 0,
            ast::Stmt::If(_, then_branch, else_branch, _) => {
                let then_depth = then_branch
                    .iter()
                    .map(|s| self.analyze_stmt_memory(s))
                    .max()
                    .unwrap_or(0);
                let else_depth = else_branch
                    .as_ref()
                    .map(|stmts| {
                        stmts
                            .iter()
                            .map(|s| self.analyze_stmt_memory(s))
                            .max()
                            .unwrap_or(0)
                    })
                    .unwrap_or(0);
                then_depth.max(else_depth) + 1
            }
            _ => 0,
        }
    }

    pub fn analyze_expr_memory(&mut self, expr: &ast::Expr) -> usize {
        match expr {
            ast::Expr::Str(s, _) => {
                self.memory_analysis.string_allocations += s.len() + 1;
                1
            }
            ast::Expr::ArrayInit(elements, _) => {
                self.memory_analysis.array_allocations += elements.len() * 8;
                for elem in elements {
                    self.analyze_expr_memory(elem);
                }
                1
            }
            ast::Expr::StructInit(name, fields, _) => {
                if let Some(struct_fields) = self.struct_defs.get(name) {
                    let size = struct_fields
                        .iter()
                        .map(|(_, ty)| self.get_type_size(ty))
                        .sum::<usize>();
                    self.memory_analysis.struct_allocations += size;
                }
                for (_, expr) in fields {
                    self.analyze_expr_memory(expr);
                }
                1
            }
            ast::Expr::TemplateStr(parts, _) => {
                let estimated_size = parts
                    .iter()
                    .map(|part| match part {
                        ast::TemplateStrPart::Literal(s) => s.len(),
                        ast::TemplateStrPart::Expression(_) => 32,
                    })
                    .sum::<usize>();
                self.memory_analysis.string_allocations += estimated_size;
                1
            }
            ast::Expr::BinOp(left, _, right, _) => self
                .analyze_expr_memory(left)
                .max(self.analyze_expr_memory(right)),
            ast::Expr::Call(_, args, _) => {
                args.iter()
                    .map(|arg| self.analyze_expr_memory(arg))
                    .max()
                    .unwrap_or(0)
                    + 1
            }
            _ => 0,
        }
    }

    pub fn estimate_type_size(&mut self, ty: &Type) {
        let size = self.get_type_size(ty);
        self.memory_analysis.estimated_arena_size += size;
    }

    pub fn get_type_size(&self, ty: &Type) -> usize {
        match ty {
            Type::I32 | Type::F32 | Type::Bool => 4,
            Type::I64 | Type::F64 => 8,
            Type::I8 | Type::U8 => 1,
            Type::I16 | Type::U16 => 2,
            Type::U32 => 4,
            Type::U64 => 8,
            Type::String => 256,
            Type::Pointer(_) | Type::RawPtr => 8,
            Type::Array(_) => 1024,
            Type::SizedArray(_inner, size) => size * 8,
            Type::Struct(name) => {
                if let Some(fields) = self.struct_defs.get(name) {
                    fields.iter().map(|(_, ty)| self.get_type_size(ty)).sum()
                } else {
                    64
                }
            }
            Type::Generic(_) => 8,
            Type::GenericInstance(_, _) => 8,
            _ => 8,
        }
    }

    pub fn calculate_arena_size(&mut self) {
        let base_size = 4 * 1024;
        let function_overhead = self.memory_analysis.total_functions * 1024;
        let string_overhead = self.memory_analysis.string_allocations;
        let array_overhead = self.memory_analysis.array_allocations;
        let struct_overhead = self.memory_analysis.struct_allocations;
        let depth_multiplier = (self.memory_analysis.max_function_depth + 1) * 2;

        let calculated_size = base_size
            + (function_overhead + string_overhead + array_overhead + struct_overhead)
                * depth_multiplier;

        self.memory_analysis.estimated_arena_size = calculated_size;

        self.memory_analysis.estimated_arena_size = self
            .memory_analysis
            .estimated_arena_size
            .next_power_of_two();

        if self.memory_analysis.estimated_arena_size > 64 * 1024 * 1024 {
            self.memory_analysis.estimated_arena_size = 64 * 1024 * 1024;
        }
    }
}
