use crate::ast;
use crate::ast::Type;
use crate::typeck::TypeChecker;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;

impl TypeChecker {
    pub fn check_pattern(
        &mut self,
        pattern: &ast::Pattern,
        expected_ty: &Type,
    ) -> Result<(), Vec<Diagnostic<FileId>>> {
        match pattern {
            ast::Pattern::Wildcard(_) => Ok(()),
            ast::Pattern::Variable(name, _) => {
                self.context
                    .variables
                    .insert(name.clone(), expected_ty.clone());
                Ok(())
            }
            ast::Pattern::EnumVariant(enum_name, variant_name, patterns, _) => match expected_ty {
                Type::GenericInstance(expected_enum, generic_args)
                    if expected_enum == enum_name =>
                {
                    if let Some(enum_def) = self.enums.iter().find(|e| &e.name == enum_name) {
                        if let Some(variant) =
                            enum_def.variants.iter().find(|v| &v.name == variant_name)
                        {
                            let mut subst = std::collections::HashMap::new();
                            for (gp, arg) in enum_def.generic_params.iter().zip(generic_args.iter())
                            {
                                subst.insert(gp, arg);
                            }
                            let data_types = if let Some(data) = &variant.data {
                                match data {
                                    crate::ast::EnumVariantData::Tuple(types) => types
                                        .iter()
                                        .map(|t| substitute_generics(t, &subst))
                                        .collect::<Vec<_>>(),
                                    crate::ast::EnumVariantData::Struct(fields) => fields
                                        .iter()
                                        .map(|f| substitute_generics(&f.ty, &subst))
                                        .collect::<Vec<_>>(),
                                }
                            } else {
                                vec![]
                            };
                            for (i, subpat) in patterns.iter().enumerate() {
                                let ty = data_types.get(i).cloned().unwrap_or(Type::Unknown);
                                self.check_pattern(subpat, &ty)?;
                            }
                            return Ok(());
                        }
                    }
                    for (i, subpat) in patterns.iter().enumerate() {
                        let ty = generic_args.get(i).cloned().unwrap_or(Type::Unknown);
                        self.check_pattern(subpat, &ty)?;
                    }
                    Ok(())
                }
                Type::Enum(expected_enum) if expected_enum == enum_name => {
                    for pattern in patterns {
                        self.check_pattern(pattern, &Type::Unknown)?;
                    }
                    Ok(())
                }
                _ => {
                    self.report_error(
                        &format!(
                            "Pattern expects enum {}, but got {}",
                            enum_name, expected_ty
                        ),
                        pattern.span(),
                    );
                    Ok(())
                }
            },
            ast::Pattern::Literal(expr, span) => {
                let literal_ty = expr.get_type();
                let mut expected = expected_ty;
                if let Type::Unknown = expected_ty {
                    if let Some(var_ty) = self.context.variables.values().last() {
                        expected = var_ty;
                    }
                }
                if Self::is_convertible(&literal_ty, expected) {
                    Ok(())
                } else {
                    self.report_error(
                        &format!(
                            "Literal pattern type {} doesn't match expected type {}",
                            literal_ty, expected
                        ),
                        *span,
                    );
                    Ok(())
                }
            }
        }
    }

    pub fn is_convertible(from: &Type, to: &Type) -> bool {
        if from == to {
            return true;
        }
        match (from, to) {
            (Type::GenericInstance(n1, a1), Type::GenericInstance(n2, a2)) => {
                n1 == n2
                    && a1.len() == a2.len()
                    && a1.iter().zip(a2).all(|(a, b)| Self::is_convertible(a, b))
            }
            (Type::Generic(_), _) | (_, Type::Generic(_)) => true,
            (Type::I8, Type::I16 | Type::I32 | Type::I64)
            | (Type::I16, Type::I32 | Type::I64)
            | (
                Type::I32,
                Type::I64
                | Type::F32
                | Type::F64
                | Type::U32
                | Type::Bool
                | Type::Pointer(_)
                | Type::RawPtr
                | Type::CSize
                | Type::U8
                | Type::U16
                | Type::I8
                | Type::I16,
            )
            | (
                Type::I64,
                Type::U32 | Type::U64 | Type::U8 | Type::U16 | Type::I8 | Type::I16 | Type::I32,
            )
            | (Type::U8, Type::U16 | Type::U32 | Type::U64 | Type::I32)
            | (Type::U16, Type::U32 | Type::U64)
            | (Type::U32, Type::U64 | Type::I32)
            | (Type::Bool, Type::I32)
            | (Type::F32, Type::F64 | Type::I32 | Type::I64 | Type::String)
            | (Type::F64, Type::I32 | Type::I64 | Type::F32)
            | (Type::Pointer(_), Type::RawPtr | Type::I32)
            | (Type::RawPtr, Type::Pointer(_) | Type::I32)
            | (Type::CSize, Type::I32)
            | (Type::Void, Type::Void) => true,
            (Type::Struct(n1), Type::Struct(n2)) => n1 == "size_t" && n2 == "size_t",
            (Type::Struct(n1), Type::I32) => n1 == "size_t",
            (Type::I32, Type::Struct(n2)) => n2 == "size_t",
            (Type::Pointer(inner), Type::String) => matches!(&**inner, Type::U8),
            (Type::Array(_), Type::String) => true,
            (Type::SizedArray(_, _), Type::String) => true,
            (Type::Array(a), Type::Array(b)) | (Type::Pointer(a), Type::Pointer(b)) => {
                Self::is_convertible(a, b)
            }
            (Type::SizedArray(a, n1), Type::SizedArray(b, n2)) => {
                n1 == n2 && Self::is_convertible(a, b)
            }
            (Type::Function(a1, r1), Type::Function(a2, r2)) => {
                a1.len() == a2.len()
                    && a1.iter().zip(a2).all(|(a, b)| Self::is_convertible(a, b))
                    && Self::is_convertible(r1, r2)
            }
            (Type::Enum(n1), Type::Enum(n2)) => n1 == n2,
            (Type::Optional(inner1), Type::Optional(inner2)) => {
                Self::is_convertible(inner1, inner2)
            }
            (Type::Optional(inner), to) => Self::is_convertible(inner, to),
            (from, Type::Optional(inner)) => {
                if from == &Type::NoneType {
                    true
                } else {
                    Self::is_convertible(from, inner)
                }
            }
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            _ => false,
        }
    }

    pub fn expect_type(
        &mut self,
        actual: &Type,
        expected: &Type,
        span: Span,
    ) -> Result<(), Vec<Diagnostic<FileId>>> {
        if !Self::is_convertible(actual, expected) {
            self.report_error(&format!("Expected {}, got {}", expected, actual), span);
        }
        Ok(())
    }

    pub fn is_cast_allowed(&self, from: &Type, to: &Type) -> bool {
        match (from, to) {
            (Type::Bool, Type::String) => true,
            (Type::I32, Type::String) => true,
            (Type::I64, Type::String) => true,
            (Type::F32, Type::String) => true,
            (Type::F64, Type::String) => true,
            (Type::Generic(_), Type::String) => true,
            (Type::RawPtr, Type::Pointer(_)) => true,
            (Type::Pointer(_), Type::RawPtr) => true,
            (Type::Array(_), Type::RawPtr) => true,
            (Type::RawPtr, Type::Array(_)) => true,
            (Type::Pointer(_), Type::I32) => true,
            (Type::I32, Type::Pointer(_)) => true,
            (Type::I32, Type::Bool) => true,
            (Type::F32, Type::I32) => true,
            (Type::I32, Type::F32) => true,
            (Type::I32, Type::U32) => true,
            (Type::U32, Type::I32) => true,
            (Type::U8, Type::I32) => true,
            (Type::String, Type::I32) => true,
            (Type::String, Type::RawPtr) => true,
            (Type::RawPtr, Type::String) => true,
            (
                Type::Enum(enum_name),
                Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64,
            ) => {
                if let Some(enum_def) = self.context.enum_def_map.get(enum_name) {
                    enum_def.variants.iter().any(|v| v.value.is_some())
                } else {
                    false
                }
            }
            _ => Self::is_convertible(from, to),
        }
    }
}

pub fn substitute_generics(ty: &Type, subst: &std::collections::HashMap<&String, &Type>) -> Type {
    match ty {
        Type::Generic(name) => subst.get(name).cloned().cloned().unwrap_or(Type::Unknown),
        Type::Pointer(inner) => Type::Pointer(Box::new(substitute_generics(inner, subst))),
        Type::Array(inner) => Type::Array(Box::new(substitute_generics(inner, subst))),
        Type::SizedArray(inner, n) => {
            Type::SizedArray(Box::new(substitute_generics(inner, subst)), *n)
        }
        Type::Function(args, ret) => Type::Function(
            args.iter().map(|a| substitute_generics(a, subst)).collect(),
            Box::new(substitute_generics(ret, subst)),
        ),
        Type::GenericInstance(name, args) => Type::GenericInstance(
            name.clone(),
            args.iter().map(|a| substitute_generics(a, subst)).collect(),
        ),
        _ => ty.clone(),
    }
}
