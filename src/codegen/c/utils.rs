use crate::ast::Type;
use crate::codegen::c::CBackend;

impl CBackend {
    pub fn type_to_c_name(&self, ty: &Type) -> String {
        match ty {
            Type::I32 => "i32".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "string".to_string(),
            Type::Void => "void".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Struct(name) | Type::Enum(name) | Type::Generic(name) => name.clone(),
            Type::GenericInstance(name, args) => {
                let mut s = name.clone();
                for arg in args {
                    s.push('_');
                    s.push_str(&self.type_to_c_name(arg));
                }
                s
            }
            Type::Array(inner) => format!("array_{}", self.type_to_c_name(inner)),
            Type::Optional(inner) => format!("optional_{}", self.type_to_c_name(inner)),
            Type::NoneType => "none".to_string(),
            _ => "unknown".to_string(),
        }
    }

    pub fn type_to_c(&self, ty: &Type) -> String {
        match ty {
            Type::GenericInstance(_, _) | Type::Struct(_) | Type::Enum(_) => {
                format!("ve_{}", self.type_to_c_name(ty))
            }
            Type::Generic(name) => {
                if self.struct_defs.contains_key(name) {
                    format!("ve_{}", name)
                } else {
                    "void*".to_string()
                }
            }
            Type::I32 => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "const char*".to_string(),
            Type::Void => "void".to_string(),
            Type::F32 => "float".to_string(),
            Type::F64 => "double".to_string(),
            Type::I8 => "ve_i8".to_string(),
            Type::I16 => "ve_i16".to_string(),
            Type::I64 => "ve_i64".to_string(),
            Type::U8 => "ve_u8".to_string(),
            Type::U16 => "ve_u16".to_string(),
            Type::U32 => "ve_u32".to_string(),
            Type::U64 => "ve_u64".to_string(),
            Type::CChar => "char".to_string(),
            Type::CInt => "int".to_string(),
            Type::CSize => "ve_size_t".to_string(),
            Type::RawPtr => "void*".to_string(),
            Type::Any => "void*".to_string(),
            Type::Unknown => "void*".to_string(),
            Type::Optional(inner) => format!("ve_optional_{}", self.type_to_c_name(inner)),
            Type::NoneType => "void*".to_string(),
            Type::Ellipsis => "...".to_string(),
            Type::Function(args, ret) => {
                let args_str = args
                    .iter()
                    .map(|t| self.type_to_c(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret_str = self.type_to_c(ret);
                format!("{}(*)({})", ret_str, args_str)
            }
            Type::Pointer(inner) => {
                format!("{}*", self.type_to_c(inner))
            }
            Type::Array(_) => "ve_Array*".to_string(),
            Type::SizedArray(inner, _) => {
                format!("{}*", self.type_to_c(inner))
            }
        }
    }

    pub fn type_to_c_ffi(&self, ty: &Type) -> String {
        match ty {
            Type::GenericInstance(_, _) => format!("ve_{}", self.type_to_c_name(ty)),
            Type::Struct(_) | Type::Enum(_) => format!("ve_{}", self.type_to_c_name(ty)),
            Type::I32 => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "const char*".to_string(),
            Type::Void => "void".to_string(),
            Type::F32 => "float".to_string(),
            Type::F64 => "double".to_string(),
            Type::I8 => "int8_t".to_string(),
            Type::I16 => "int16_t".to_string(),
            Type::I64 => "int64_t".to_string(),
            Type::U8 => "uint8_t".to_string(),
            Type::U16 => "uint16_t".to_string(),
            Type::U32 => "uint32_t".to_string(),
            Type::U64 => "uint64_t".to_string(),
            Type::CChar => "char".to_string(),
            Type::CInt => "int".to_string(),
            Type::CSize => "size_t".to_string(),
            Type::RawPtr => "void*".to_string(),
            Type::Any => "void*".to_string(),
            Type::Unknown | Type::Generic(_) => "void*".to_string(),
            Type::Optional(inner) => format!("ve_optional_{}", self.type_to_c_name(inner)),
            Type::NoneType => "void*".to_string(),
            Type::Ellipsis => "...".to_string(),
            Type::Function(args, ret) => {
                let args_str = args
                    .iter()
                    .map(|t| self.type_to_c_ffi(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret_str = self.type_to_c_ffi(ret);
                format!("{}(*)({})", ret_str, args_str)
            }
            Type::Pointer(inner) | Type::Array(inner) | Type::SizedArray(inner, _) => {
                format!("{}*", self.type_to_c_ffi(inner))
            }
        }
    }
}
