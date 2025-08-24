use crate::ast;
use crate::ast::Type;
use crate::codegen::CompileError;
use crate::codegen::c::CBackend;

impl CBackend {
    pub fn emit_expr(&mut self, expr: &ast::Expr) -> Result<String, CompileError> {
        match expr {
            ast::Expr::Int(n, _) => Ok(n.to_string()),
            ast::Expr::Int64(n, _) => Ok(n.to_string()),
            ast::Expr::F32(f, _) => Ok(f.to_string()),
            ast::Expr::Bool(b, _) => {
                self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                Ok(if *b { "true" } else { "false" }.to_string())
            }
            ast::Expr::BinOp(left, op, right, _) => {
                let left_code = self.emit_expr(left)?;
                let right_code = self.emit_expr(right)?;

                let left_type = left.get_type();
                let right_type = right.get_type();
                let is_string_cmp =
                    matches!(left_type, Type::String) || matches!(right_type, Type::String);
                if is_string_cmp {
                    match op {
                        ast::BinOp::Eq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("(strcmp({}, {}) == 0)", left_conv, right_conv));
                        }
                        ast::BinOp::NotEq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("(strcmp({}, {}) != 0)", left_conv, right_conv));
                        }
                        _ => {
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("ve_concat({}, {})", left_conv, right_conv));
                        }
                    }
                }

                let result_type = expr.get_type();
                match result_type {
                    Type::String => match op {
                        ast::BinOp::Eq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                            let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                            Ok(format!("(strcmp({}, {}) == 0)", left_conv, right_conv))
                        }
                        ast::BinOp::NotEq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                            let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                            Ok(format!("(strcmp({}, {}) != 0)", left_conv, right_conv))
                        }
                        _ => {
                            let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                            let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                            Ok(format!("ve_concat({}, {})", left_conv, right_conv))
                        }
                    },
                    _ => {
                        let c_op = match op {
                            ast::BinOp::Add => "+",
                            ast::BinOp::Sub => "-",
                            ast::BinOp::Mul => "*",
                            ast::BinOp::Div => "/",
                            ast::BinOp::Mod => "%",
                            ast::BinOp::Eq => "==",
                            ast::BinOp::NotEq => "!=",
                            ast::BinOp::Gt => ">",
                            ast::BinOp::Lt => "<",
                            ast::BinOp::GtEq => ">=",
                            ast::BinOp::LtEq => "<=",
                            ast::BinOp::And => "&&",
                            ast::BinOp::Or => "||",
                            ast::BinOp::Pow | ast::BinOp::Pow2 => "",
                        };

                        if matches!(op, ast::BinOp::Pow | ast::BinOp::Pow2) {
                            self.includes.borrow_mut().insert("<math.h>".to_string());
                            return Ok(format!("pow({}, {})", left_code, right_code));
                        }

                        Ok(format!("({} {} {})", left_code, c_op, right_code))
                    }
                }
            }
            ast::Expr::Assign(target, value, _) => {
                let target_code = self.emit_expr(target)?;
                let value_code = self.emit_expr(value)?;
                Ok(format!("({} = {})", target_code, value_code))
            }
            ast::Expr::Str(s, _) => Ok(format!(
                "\"{}\"",
                s.replace("\n", "\\n").replace("\"", "\\\"")
            )),
            ast::Expr::Var(name, info) => {
                if name == "true" || name == "false" {
                    self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                    return Ok(name.clone());
                }

                let var_type = self
                    .variables
                    .borrow()
                    .get(name)
                    .cloned()
                    .unwrap_or(Type::Unknown);
                match var_type {
                    Type::I32
                    | Type::String
                    | Type::Pointer(_)
                    | Type::RawPtr
                    | Type::Struct(_)
                    | Type::Array(_)
                    | Type::SizedArray(_, _)
                    | Type::F32
                    | Type::F64
                    | Type::I8
                    | Type::I16
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::Unknown
                    | Type::Generic(_)
                    | Type::Optional(_) => Ok(name.clone()),
                    Type::Bool => {
                        self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                        Ok(name.clone())
                    }
                    _ => Err(CompileError::CodegenError {
                        message: format!("Cannot print type (in var) {:?}", var_type),
                        span: Some(info.span),
                        file_id: self.file_id,
                    }),
                }
            }
            ast::Expr::Call(name, args, _expr_info) => {
                if name.starts_with("<method>.") {
                    let method_name = &name[9..];
                    if let Some(obj_expr) = args.first() {
                        let obj_type = &obj_expr.get_type();
                        let type_name = self.type_to_c_name(obj_type);

                        let sanitized_type_name = type_name
                            .replace("[]", "_array")
                            .replace("[", "_")
                            .replace("]", "_");
                        let method_func_name =
                            format!("ve_method_{}_{}", sanitized_type_name, method_name);

                        let mut args_code = Vec::new();

                        let is_static_method = matches!(obj_expr, ast::Expr::Var(var_name, _) if self.struct_defs.contains_key(var_name.as_str()) || self.enum_defs.contains_key(var_name.as_str()) || matches!(var_name.as_str(), "Command" | "Array" | "Option"));

                        if !is_static_method {
                            let obj_code = self.emit_expr(obj_expr)?;
                            args_code.push(obj_code);
                        }

                        for arg in args.iter().skip(1) {
                            args_code.push(self.emit_expr(arg)?);
                        }

                        return Ok(format!("{}({})", method_func_name, args_code.join(", ")));
                    }
                }

                let final_name = if self.ffi_functions.contains(name) {
                    name.clone()
                } else {
                    format!("ve_fn_{}", name)
                };

                let mut args_code = Vec::new();
                for arg in args {
                    args_code.push(self.emit_expr(arg)?);
                }

                Ok(format!("{}({})", final_name, args_code.join(", ")))
            }
            ast::Expr::Void(_) => Ok("".to_string()),
            ast::Expr::SafeBlock(stmts, _) => {
                let mut code = String::new();
                code.push_str("{\n");
                let mut defers = Vec::new();

                for stmt in stmts {
                    match stmt {
                        ast::Stmt::Defer(expr, _) => {
                            let expr_code = self.emit_expr(expr)?;
                            defers.push(expr_code);
                        }
                        _ => {
                            let stmt_code = self.emit_stmt_to_string(stmt)?;
                            code.push_str(&stmt_code);
                        }
                    }
                }

                for deferred in defers.into_iter().rev() {
                    code.push_str(&format!("{};\n", deferred));
                }

                code.push_str("}\n");
                Ok(code)
            }
            ast::Expr::Deref(expr, _) => {
                let inner = self.emit_expr(expr)?;
                Ok(format!("(*{})", inner))
            }
            ast::Expr::Cast(expr, target_ty, _) => {
                let expr_code = self.emit_expr(expr)?;
                let expr_type = expr.get_type();
                match (&expr_type, target_ty) {
                    (Type::I32, Type::String) => Ok(format!("ve_int_to_str({})", expr_code)),
                    (Type::I64, Type::String) => Ok(format!("ve_i64_to_str({})", expr_code)),
                    (Type::F32, Type::String) => Ok(format!("ve_float_to_str({})", expr_code)),
                    (Type::F64, Type::String) => Ok(format!("ve_double_to_str({})", expr_code)),
                    (Type::Bool, Type::String) => Ok(format!("ve_bool_to_str({})", expr_code)),
                    (Type::Array(inner_type), Type::String) => match inner_type.as_ref() {
                        Type::I32 => Ok(format!("ve_array_i32_to_str({})", expr_code)),
                        Type::String => Ok(format!("ve_array_string_to_str({})", expr_code)),
                        Type::Bool => Ok(format!("ve_array_bool_to_str({})", expr_code)),
                        _ => Ok(format!("ve_array_to_str({})", expr_code)),
                    },
                    (Type::SizedArray(inner_type, _), Type::String) => match inner_type.as_ref() {
                        Type::I32 => Ok(format!("ve_array_i32_to_str({})", expr_code)),
                        Type::String => Ok(format!("ve_array_string_to_str({})", expr_code)),
                        Type::Bool => Ok(format!("ve_array_bool_to_str({})", expr_code)),
                        _ => Ok(format!("ve_array_to_str({})", expr_code)),
                    },
                    (Type::RawPtr, Type::String) => Ok(format!("(const char*)({})", expr_code)),
                    (Type::Pointer(inner), Type::String) => {
                        if matches!(**inner, Type::U8) {
                            Ok(format!("(const char*)({})", expr_code))
                        } else {
                            Ok(format!("ve_ptr_to_str({})", expr_code))
                        }
                    }
                    (Type::String, Type::I32) => {
                        self.includes.borrow_mut().insert("<stdlib.h>".to_string());
                        Ok(format!("atoi({})", expr_code))
                    }
                    (_, Type::Pointer(inner_ty)) => {
                        Ok(format!("({}*)({})", self.type_to_c(inner_ty), expr_code))
                    }
                    (_, Type::RawPtr) => Ok(format!("(void*)({})", expr_code)),
                    _ => Ok(format!("({})({})", self.type_to_c(target_ty), expr_code)),
                }
            }
            ast::Expr::Range(start, end, range_type, _) => {
                let start_code = self.emit_expr(start)?;
                let end_code = self.emit_expr(end)?;
                match range_type {
                    ast::RangeType::Inclusive => Ok(format!("{}..={}", start_code, end_code)),
                    ast::RangeType::Exclusive => Ok(format!("{}..{}", start_code, end_code)),
                    ast::RangeType::InfiniteUp => Ok(format!("{}..>", start_code)),
                    ast::RangeType::InfiniteDown => Ok(format!("{}..<", start_code)),
                    ast::RangeType::Infinite => Ok(format!("{}..", start_code)),
                }
            }
            ast::Expr::InfiniteRange(range_type, _) => match range_type {
                ast::RangeType::InfiniteUp => Ok("..>".to_string()),
                ast::RangeType::InfiniteDown => Ok("..<".to_string()),
                ast::RangeType::Infinite => Ok("..".to_string()),
                _ => Err(CompileError::CodegenError {
                    message: format!("Invalid infinite range type: {:?}", range_type),
                    span: None,
                    file_id: self.file_id,
                }),
            },
            ast::Expr::StructInit(name, fields, _) => {
                let mut field_inits = Vec::new();
                for (field_name, field_expr) in fields {
                    let field_code = self.emit_expr(field_expr)?;
                    field_inits.push(format!(".{} = {}", field_name, field_code));
                }

                Ok(format!("(ve_{}){{ {} }}", name, field_inits.join(", ")))
            }
            ast::Expr::FieldAccess(obj, field_name, _info) => {
                let obj_code = self.emit_expr(obj)?;
                Ok(format!("{}.{}", obj_code, field_name))
            }
            ast::Expr::New(name, args, _info) => {
                let mut arg_codes = Vec::new();
                for arg in args {
                    arg_codes.push(self.emit_expr(arg)?);
                }
                Ok(format!(
                    "ve_method_{}_constructor({})",
                    name,
                    arg_codes.join(", ")
                ))
            }
            ast::Expr::ArrayInit(elements, info) => {
                let element_type = match &info.ty {
                    Type::Array(inner) => inner.clone(),
                    _ => {
                        return Err(CompileError::CodegenError {
                            message: "Array initialization with non-array type".to_string(),
                            span: Some(info.span),
                            file_id: self.file_id,
                        });
                    }
                };

                let element_type_str = self.type_to_c(&element_type);
                self.includes.borrow_mut().insert("<stdlib.h>".to_string());

                let temp_var_ptr = format!("_array_ptr_{}", info.span.start());

                let mut has_spread = false;
                for elem in elements {
                    if let ast::Expr::Spread(_, _) = elem {
                        has_spread = true;
                        break;
                    }
                }

                if !has_spread {
                    let mut element_exprs = Vec::new();
                    for elem in elements {
                        element_exprs.push(self.emit_expr(elem)?);
                    }
                    let elements_str = element_exprs.join(", ");
                    let temp_var = format!("_array_stack_{}", info.span.start());
                    let size = elements.len();

                    if size == 0 {
                        Ok(format!(
                            "ve_array_create_empty(sizeof({}), 4)",
                            element_type_str
                        ))
                    } else {
                        Ok(format!(
                            "({{ \
                            {} {}[] = {{ {} }}; \
                            {}* {} = ve_arena_alloc({} * sizeof({})); \
                            if ({}) {{ \
                                for (int _i = 0; _i < {}; _i++) {{{}[_i] = {}[_i];}} \
                            }} \
                            ve_array_create({}, {}, sizeof({})); \
                        }})",
                            element_type_str,
                            temp_var,
                            elements_str,
                            element_type_str,
                            temp_var_ptr,
                            size,
                            element_type_str,
                            temp_var_ptr,
                            size,
                            temp_var_ptr,
                            temp_var,
                            temp_var_ptr,
                            size,
                            element_type_str
                        ))
                    }
                } else {
                    let base_id = info.span.start();
                    let temp_offset = format!("_offset_{}", base_id);
                    let temp_total_size = format!("_total_size_{}", base_id);

                    let mut code = format!(
                        "({{ \
                        size_t {} = 0; \
                        ",
                        temp_total_size
                    );

                    let mut spread_vars = Vec::new();
                    let mut spread_counter = 0;

                    for elem in elements {
                        match elem {
                            ast::Expr::Spread(spread_expr, _) => {
                                let temp_spread_array =
                                    format!("_spread_array_{}_{}", base_id, spread_counter);
                                let temp_spread_len =
                                    format!("_spread_len_{}_{}", base_id, spread_counter);
                                let spread_code = self.emit_expr(spread_expr)?;

                                code.push_str(&format!(
                                    "ve_Array* {} = {}; \
                                    size_t {} = ve_array_length({}); \
                                    {} += {}; \
                                    ",
                                    temp_spread_array,
                                    spread_code,
                                    temp_spread_len,
                                    temp_spread_array,
                                    temp_total_size,
                                    temp_spread_len
                                ));

                                spread_vars.push((temp_spread_array, temp_spread_len));
                                spread_counter += 1;
                            }
                            _ => {
                                code.push_str(&format!("{} += 1; ", temp_total_size));
                            }
                        }
                    }

                    code.push_str(&format!(
                        "{}* {} = ve_arena_alloc({} * sizeof({})); \
                        size_t {} = 0; \
                        ",
                        element_type_str,
                        temp_var_ptr,
                        temp_total_size,
                        element_type_str,
                        temp_offset
                    ));

                    let mut spread_var_index = 0;
                    for elem in elements {
                        match elem {
                            ast::Expr::Spread(_, _) => {
                                let (temp_spread_array, temp_spread_len) =
                                    &spread_vars[spread_var_index];
                                code.push_str(&format!(
                                    "for (size_t _i = 0; _i < {}; _i++) {{ \
                                        {}[{} + _i] = (({}*)ve_array_data({}))[_i]; \
                                    }} \
                                    {} += {}; \
                                    ",
                                    temp_spread_len,
                                    temp_var_ptr,
                                    temp_offset,
                                    element_type_str,
                                    temp_spread_array,
                                    temp_offset,
                                    temp_spread_len
                                ));
                                spread_var_index += 1;
                            }
                            _ => {
                                let elem_code = self.emit_expr(elem)?;
                                code.push_str(&format!(
                                    "{}[{}] = {}; \
                                    {} += 1; \
                                    ",
                                    temp_var_ptr, temp_offset, elem_code, temp_offset
                                ));
                            }
                        }
                    }

                    code.push_str(&format!(
                        "ve_array_create({}, {}, sizeof({})); \
                        }})",
                        temp_var_ptr, temp_total_size, element_type_str
                    ));

                    Ok(code)
                }
            }
            ast::Expr::ArrayAccess(array, index, _info) => {
                let array_expr = self.emit_expr(array)?;
                let index_expr = self.emit_expr(index)?;
                let array_type = array.get_type();

                match array_type {
                    Type::Pointer(inner_ty) => Ok(format!(
                        "(({}*){})[{}]",
                        self.type_to_c(&inner_ty),
                        array_expr,
                        index_expr
                    )),
                    Type::Array(inner_ty) => Ok(format!(
                        "(({}*)ve_array_data({}))[{}]",
                        self.type_to_c(&inner_ty),
                        array_expr,
                        index_expr
                    )),
                    Type::RawPtr => Ok(format!("((unsigned char*){})[{}]", array_expr, index_expr)),
                    _ => Ok(format!("{}[{}]", array_expr, index_expr)),
                }
            }
            ast::Expr::TemplateStr(parts, _info) => {
                if parts.is_empty() {
                    return Ok("\"\"".to_string());
                }

                let mut result = String::new();
                let mut is_first = true;

                for part in parts {
                    let part_code = match part {
                        ast::TemplateStrPart::Literal(text) => {
                            format!("\"{}\"", text.replace("\n", "\\n").replace("\"", "\\\""))
                        }
                        ast::TemplateStrPart::Expression(expr) => {
                            let expr_code = self.emit_expr(expr)?;
                            let expr_type = expr.get_type();

                            match &**expr {
                                ast::Expr::ArrayAccess(array, _, _) => {
                                    let array_type = array.get_type();
                                    match array_type {
                                        Type::Array(element_type)
                                        | Type::SizedArray(element_type, _) => {
                                            match *element_type {
                                                Type::I32 => {
                                                    format!("ve_int_to_str({})", expr_code)
                                                }
                                                Type::Bool => {
                                                    format!("ve_bool_to_str({})", expr_code)
                                                }
                                                Type::String => expr_code,
                                                _ => {
                                                    format!("\"[unsupported array element type]\"")
                                                }
                                            }
                                        }
                                        _ => format!("\"[not an array]\""),
                                    }
                                }
                                _ => self.convert_to_c_str(&expr_code, &expr_type),
                            }
                        }
                    };

                    if is_first {
                        result = part_code;
                        is_first = false;
                    } else {
                        result = format!("ve_concat({}, {})", result, part_code);
                    }
                }

                Ok(result)
            }
            ast::Expr::FfiCall(name, args, _) => {
                let mut args_code = Vec::new();
                for arg in args {
                    let arg_code = self.emit_expr(arg)?;
                    args_code.push(arg_code);
                }
                Ok(format!("{}({})", name, args_code.join(", ")))
            }
            ast::Expr::UnaryOp(op, expr, _) => {
                let inner_code = self.emit_expr(expr)?;
                match op {
                    ast::UnOp::Neg => Ok(format!("-{}", inner_code)),
                    ast::UnOp::Plus => Ok(format!("{}", inner_code)),
                    ast::UnOp::Not => Ok(format!("!{}", inner_code)),
                }
            }

            ast::Expr::EnumConstruct(enum_name, variant_name, args, info) => {
                if let Some(enum_def) = self.enum_defs.get(enum_name) {
                    let is_simple_enum = enum_def.variants.iter().all(|v| v.data.is_none());

                    if is_simple_enum && args.is_empty() {
                        let prefixed_enum = format!("ve_{}", enum_name);
                        return Ok(format!("{}_{}", prefixed_enum, variant_name));
                    }
                }
                let mut args_code = Vec::new();
                for arg in args {
                    args_code.push(self.emit_expr(arg)?);
                }

                let enum_type_name = match &info.ty {
                    Type::GenericInstance(_, _) => self.type_to_c_name(&info.ty),
                    _ => enum_name.clone(),
                };
                let prefixed_enum = format!("ve_{}", enum_type_name);
                if args_code.is_empty() {
                    Ok(format!("{}_{}_new()", prefixed_enum, variant_name))
                } else {
                    Ok(format!(
                        "{}_{}_new({})",
                        prefixed_enum,
                        variant_name,
                        args_code.join(", ")
                    ))
                }
            }

            ast::Expr::Match(pattern, arms, info) => {
                let matched_var = match pattern.as_ref() {
                    ast::Pattern::Variable(var_name, _) => var_name.clone(),
                    _ => {
                        return Err(CompileError::CodegenError {
                            message: "Only variable patterns supported in match".to_string(),
                            span: None,
                            file_id: self.file_id,
                        });
                    }
                };

                let matched_type = self
                    .variables
                    .borrow()
                    .get(&matched_var)
                    .cloned()
                    .unwrap_or(Type::Unknown);

                let switch_expr = match matched_type {
                    Type::Enum(enum_name) => {
                        if self.is_simple_enum(&enum_name) {
                            matched_var.clone()
                        } else {
                            format!("{}.tag", matched_var)
                        }
                    }
                    Type::GenericInstance(_, _) => format!("{}.tag", matched_var),
                    _ => matched_var.clone(),
                };

                let mut code = String::new();
                code.push_str(&format!("switch ({}) {{\n", switch_expr));

                for arm in arms {
                    match &arm.pattern {
                        ast::Pattern::EnumVariant(enum_name, variant_name, patterns, _) => {
                            let case_value = if let Some(matched_type) =
                                self.variables.borrow().get(&matched_var)
                            {
                                match matched_type {
                                    Type::GenericInstance(_, _) => {
                                        format!(
                                            "ve_{}_{}",
                                            self.type_to_c_name(matched_type),
                                            variant_name
                                        )
                                    }
                                    _ => format!("ve_{}_{}", enum_name, variant_name),
                                }
                            } else {
                                format!("ve_{}_{}", enum_name, variant_name)
                            };
                            code.push_str(&format!("case {}: {{\n", case_value));

                            for (i, pattern) in patterns.iter().enumerate() {
                                if let ast::Pattern::Variable(var_name, _) = pattern {
                                    let mut field_type = "int".to_string();
                                    if let Some(matched_type) =
                                        self.variables.borrow().get(&matched_var)
                                    {
                                        if let Type::GenericInstance(enum_name, args) = matched_type
                                        {
                                            if let Some(enum_def) = self.enum_defs.get(enum_name) {
                                                enum_def
                                                    .variants
                                                    .iter()
                                                    .find(|v| v.name == *variant_name)
                                                    .map(|variant| {
                                                        if let Some(data_types) = &variant.data {
                                                            if let Some(ty) = data_types.get(i) {
                                                                if let Type::Generic(param_name) =
                                                                    ty
                                                                {
                                                                    if let Some(generic_idx) =
                                                                        enum_def
                                                                            .generic_params
                                                                            .iter()
                                                                            .position(|p| {
                                                                                p == param_name
                                                                            })
                                                                    {
                                                                        if let Some(concrete_type) =
                                                                            args.get(generic_idx)
                                                                        {
                                                                            field_type = self
                                                                                .type_to_c(
                                                                                    concrete_type,
                                                                                );
                                                                        }
                                                                    }
                                                                } else {
                                                                    field_type = self.type_to_c(ty);
                                                                }
                                                            }
                                                        }
                                                    });
                                            }
                                        }
                                        code.push_str(&format!(
                                            "    {} {} = {}.data.{}.field{};\n",
                                            field_type,
                                            var_name,
                                            matched_var,
                                            variant_name.to_lowercase(),
                                            i
                                        ));
                                    }
                                }

                                let body_code = match &arm.body {
                                    ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                                    ast::MatchArmBody::Block(stmts) => {
                                        let mut block_code = String::new();
                                        for stmt in stmts {
                                            block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                        }
                                        block_code
                                    }
                                };

                                if info.is_tail {
                                    code.push_str(&format!("    return {};\n", body_code));
                                } else {
                                    code.push_str(&format!("    {};\n", body_code));
                                    code.push_str("    break;\n");
                                }
                                code.push_str("}\n");
                            }
                        }
                        ast::Pattern::Literal(expr, _) => {
                            let literal_code = self.emit_expr(expr)?;
                            code.push_str(&format!("case {}: {{\n", literal_code));
                            let body_code = match &arm.body {
                                ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                                ast::MatchArmBody::Block(stmts) => {
                                    let mut block_code = String::new();
                                    for stmt in stmts {
                                        block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                    }
                                    block_code
                                }
                            };
                            if info.is_tail {
                                code.push_str(&format!("    return {};\n", body_code));
                            } else {
                                code.push_str(&format!("    {};\n", body_code));
                                code.push_str("    break;\n");
                            }
                            code.push_str("}\n");
                        }
                        ast::Pattern::Wildcard(_) => {
                            code.push_str("default: {\n");
                            let body_code = match &arm.body {
                                ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                                ast::MatchArmBody::Block(stmts) => {
                                    let mut block_code = String::new();
                                    for stmt in stmts {
                                        block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                    }
                                    block_code
                                }
                            };
                            if info.is_tail {
                                code.push_str(&format!("    return {};\n", body_code));
                            } else {
                                code.push_str(&format!("    {};\n", body_code));
                                code.push_str("    break;\n");
                            }
                            code.push_str("}\n");
                        }
                        _ => {
                            return Err(CompileError::CodegenError {
                                message: "Unsupported pattern in match".to_string(),
                                span: Some(arm.span),
                                file_id: self.file_id,
                            });
                        }
                    }
                }
                code.push_str("}\n");
                Ok(code)
            }
            ast::Expr::If(condition, then_branch, else_branch, _info) => {
                let condition_code = self.emit_expr(condition)?;

                if let Some(else_stmts) = else_branch.as_ref() {
                    if then_branch.len() == 1 && else_stmts.len() == 1 {
                        if let (ast::Stmt::Expr(then_expr, _), ast::Stmt::Expr(else_expr, _)) =
                            (&then_branch[0], &else_stmts[0])
                        {
                            if !matches!(then_expr, ast::Expr::If(_, _, _, _))
                                && !matches!(else_expr, ast::Expr::If(_, _, _, _))
                            {
                                let then_code = self.emit_expr(then_expr)?;
                                let else_code = self.emit_expr(else_expr)?;
                                return Ok(format!(
                                    "({} ? {} : {})",
                                    condition_code, then_code, else_code
                                ));
                            }
                        }
                    }
                }

                let mut code = String::new();
                code.push_str(&format!("if ({}) {{\n", condition_code));

                for stmt in then_branch {
                    code.push_str(&self.emit_stmt_to_string(stmt)?);
                }

                if let Some(else_stmts) = else_branch {
                    code.push_str("} else {\n");
                    for stmt in else_stmts {
                        code.push_str(&self.emit_stmt_to_string(stmt)?);
                    }
                }

                code.push_str("}\n");
                Ok(code)
            }
            ast::Expr::Loop(body, info) => {
                let result_var = format!("_loop_result_{}", info.span.start());
                let loop_start = format!("_loop_start_{}", info.span.start());
                let loop_break = format!("_loop_break_{}", info.span.start());

                let result_type = self.type_to_c(&info.ty);

                let old_loop_result =
                    std::mem::replace(&mut self.current_loop_result, Some(result_var.clone()));
                let old_loop_break =
                    std::mem::replace(&mut self.current_loop_break, Some(loop_break.clone()));

                let mut code = String::new();
                code.push_str("({ ");
                code.push_str(&format!("{} {} = 0; ", result_type, result_var));
                code.push_str(&format!("{}: ", loop_start));
                code.push_str("while (1) {\n");

                let mut body_code = String::new();
                let old_body = std::mem::replace(&mut self.body, body_code);

                for stmt in body {
                    self.emit_stmt(stmt)?;
                }

                body_code = std::mem::replace(&mut self.body, old_body);
                code.push_str(&body_code);

                code.push_str("}\n");
                code.push_str(&format!("{}: {}; ", loop_break, result_var));
                code.push_str("})");

                self.current_loop_result = old_loop_result;
                self.current_loop_break = old_loop_break;

                Ok(code)
            }
            ast::Expr::Spread(expr, _) => self.emit_expr(expr),
            ast::Expr::None(_) => Ok("NULL".to_string()),
        }
    }

    pub fn emit_expr_with_optional_context(
        &mut self,
        expr: &ast::Expr,
        expected_type: &Type,
    ) -> Result<String, CompileError> {
        match (expr, expected_type) {
            (ast::Expr::None(_), Type::Optional(inner_type)) => {
                self.ensure_optional_type(inner_type);
                let type_name = self.type_to_c_name(inner_type);
                Ok(format!("ve_none_{}()", type_name))
            }
            (_, Type::Optional(inner_type)) => {
                let expr_type = expr.get_type();
                if self.is_type_convertible(&expr_type, inner_type) {
                    self.ensure_optional_type(inner_type);

                    let expr_code = if matches!(inner_type.as_ref(), Type::Optional(_)) {
                        self.emit_expr_with_optional_context(expr, inner_type)?
                    } else {
                        self.emit_expr(expr)?
                    };

                    let type_name = self.type_to_c_name(inner_type);
                    Ok(format!("ve_some_{}({})", type_name, expr_code))
                } else {
                    self.emit_expr(expr)
                }
            }
            _ => self.emit_expr(expr),
        }
    }

    pub fn convert_to_c_str(&mut self, code: &str, ty: &Type) -> String {
        self.includes.borrow_mut().insert("<string.h>".to_string());
        match ty {
            Type::I32 => format!("ve_int_to_str({})", code),
            Type::Bool => format!("ve_bool_to_str({})", code),
            Type::RawPtr => format!("(const char*)({})", code),
            Type::Pointer(_) => format!("ve_ptr_to_str({})", code),
            Type::String => code.to_string(),
            Type::Struct(name) => format!("ve_{}_to_str(&{})", name, code),
            Type::F32 => format!("ve_float_to_str({})", code),
            Type::F64 => format!("ve_double_to_str({})", code),
            Type::I8 => format!("ve_i8_to_str({})", code),
            Type::I16 => format!("ve_i16_to_str({})", code),
            Type::I64 => format!("ve_i64_to_str({})", code),
            Type::U8 => format!("ve_u8_to_str({})", code),
            Type::U16 => format!("ve_u16_to_str({})", code),
            Type::U32 => format!("ve_u32_to_str({})", code),
            Type::U64 => format!("ve_u64_to_str({})", code),
            Type::Optional(inner_type) => {
                format!(
                    "({}.has_value ? {} : \"None\")",
                    code,
                    self.convert_to_c_str(&format!("{}.value", code), inner_type)
                )
            }
            Type::Array(inner_type) => match inner_type.as_ref() {
                Type::I32 => format!("ve_array_i32_to_str({})", code),
                Type::String => format!("ve_array_string_to_str({})", code),
                Type::Bool => format!("ve_array_bool_to_str({})", code),
                _ => format!("ve_array_to_str({})", code),
            },
            Type::SizedArray(inner_type, _) => match inner_type.as_ref() {
                Type::I32 => format!("ve_array_i32_to_str({})", code),
                Type::String => format!("ve_array_string_to_str({})", code),
                Type::Bool => format!("ve_array_bool_to_str({})", code),
                _ => format!("ve_array_to_str({})", code),
            },
            Type::Any => "[any]".to_string(),
            Type::Unknown => {
                eprintln!("Warning: Unknown type in conversion to string. Check type inference.");
                "[unknown]".to_string()
            }
            Type::GenericInstance(_name, _) => format!("ve_ptr_to_str({})", code),
            _ => {
                eprintln!("Warning: Cannot convert type {:?} to string", ty);
                "[unsupported type]".to_string()
            }
        }
    }

    pub fn is_type_convertible(&self, from: &Type, to: &Type) -> bool {
        match (from, to) {
            (a, b) if a == b => true,
            (Type::I32, Type::I64) | (Type::I64, Type::I32) => true,
            (Type::I8, Type::I32) | (Type::I32, Type::I8) => true,
            (Type::I16, Type::I32) | (Type::I32, Type::I16) => true,
            (Type::I8, Type::I16) | (Type::I16, Type::I8) => true,
            (Type::I8, Type::I64) | (Type::I64, Type::I8) => true,
            (Type::I16, Type::I64) | (Type::I64, Type::I16) => true,
            (Type::F32, Type::F64) | (Type::F64, Type::F32) => true,
            (Type::Unknown, _) => true,
            (from_ty, Type::Optional(inner_ty)) => self.is_type_convertible(from_ty, inner_ty),
            _ => false,
        }
    }
}
