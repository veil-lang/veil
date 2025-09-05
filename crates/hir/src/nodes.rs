//! HIR node definitions
//!
//! HIR (High-level Intermediate Representation) nodes are designed to be more
//! type-checker friendly than AST nodes while preserving all necessary information
//! for diagnostics and further compilation passes.
//!
//! Key differences from AST:
//! - Uses stable IDs instead of string names where possible
//! - More normalized structure for type checking
//! - Preserves spans through span mapping table
//! - Separates resolved vs unresolved name references

use crate::ids::{ModuleId, NodeId, SymbolId, TypeId};
use codespan::Span;

use std::collections::HashMap;

/// HIR representation of the entire program
#[derive(Debug, Clone)]
pub struct HirProgram {
    pub module_id: ModuleId,
    pub items: Vec<HirItem>,
    pub span_map: SpanMap,
}

/// Mapping from HIR node IDs to their original spans
#[derive(Debug, Clone, Default)]
pub struct SpanMap {
    pub spans: HashMap<NodeId, Span>,
}

impl SpanMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, node_id: NodeId, span: Span) {
        self.spans.insert(node_id, span);
    }

    pub fn get(&self, node_id: NodeId) -> Option<Span> {
        self.spans.get(&node_id).copied()
    }
}

/// Top-level items in a module
#[derive(Debug, Clone)]
pub struct HirItem {
    pub id: NodeId,
    pub kind: HirItemKind,
    pub visibility: HirVisibility,
}

#[derive(Debug, Clone)]
pub enum HirItemKind {
    Function(HirFunction),
    Struct(HirStruct),
    Enum(HirEnum),
    Impl(HirImpl),
    FfiFunction(HirFfiFunction),
    FfiVariable(HirFfiVariable),
    Test(HirTest),
    Import(HirImport),
}

/// HIR representation of visibility
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirVisibility {
    Private,
    Public,
    PublicCrate,
    PublicSuper,
    PublicIn(String),           // Path string, will be resolved to PublicInResolved
    PublicInResolved(ModuleId), // Resolved version after symbol resolution
}

/// HIR generic parameter
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirGenericParam {
    pub name: String,
    pub bounds: Vec<HirType>,
}

/// HIR function definition
#[derive(Debug, Clone)]
pub struct HirFunction {
    pub id: NodeId,
    pub name: String,
    pub symbol_id: Option<SymbolId>, // Resolved symbol ID
    pub generic_params: Vec<HirGenericParam>,
    pub params: Vec<HirParam>,
    pub return_type: HirType,
    pub body: HirBlock,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct HirParam {
    pub id: NodeId,
    pub name: String,
    pub symbol_id: Option<SymbolId>, // Resolved symbol ID
    pub ty: HirType,
}

/// HIR struct definition
#[derive(Debug, Clone)]
pub struct HirStruct {
    pub id: NodeId,
    pub name: String,
    pub symbol_id: Option<SymbolId>, // Resolved symbol ID
    pub generic_params: Vec<HirGenericParam>,
    pub fields: Vec<HirStructField>,
    pub repr: Option<String>,
}

/// Struct field
#[derive(Debug, Clone)]
pub struct HirStructField {
    pub id: NodeId,
    pub name: String,
    pub symbol_id: Option<SymbolId>, // Resolved symbol ID
    pub ty: HirType,
    pub visibility: HirVisibility,
}

/// HIR enum definition
#[derive(Debug, Clone)]
pub struct HirEnum {
    pub id: NodeId,
    pub name: String,
    pub symbol_id: Option<SymbolId>, // Resolved symbol ID
    pub generic_params: Vec<HirGenericParam>,
    pub variants: Vec<HirEnumVariant>,
}

/// Enum variant
#[derive(Debug, Clone)]
pub struct HirEnumVariant {
    pub id: NodeId,
    pub name: String,
    pub symbol_id: Option<SymbolId>, // Resolved symbol ID
    pub data: Option<HirEnumVariantData>,
    pub discriminant: Option<HirExpr>,
}

#[derive(Debug, Clone)]
pub enum HirEnumVariantData {
    Tuple(Vec<HirType>),
    Struct(Vec<HirStructField>),
}

/// HIR impl block
#[derive(Debug, Clone)]
pub struct HirImpl {
    pub id: NodeId,
    pub target_type: HirType,
    pub trait_ref: Option<HirType>, // Trait reference as HIR type
    pub trait_symbol_id: Option<SymbolId>, // Resolved trait symbol ID
    pub methods: Vec<HirFunction>,
}

/// HIR FFI function
#[derive(Debug, Clone)]
pub struct HirFfiFunction {
    pub id: NodeId,
    pub name: String,
    pub params: Vec<HirParam>,
    pub return_type: HirType,
    pub variadic: bool,
}

/// HIR FFI variable
#[derive(Debug, Clone)]
pub struct HirFfiVariable {
    pub id: NodeId,
    pub name: String,
    pub ty: HirType,
}

/// HIR test definition
#[derive(Debug, Clone)]
pub struct HirTest {
    pub id: NodeId,
    pub name: String,
    pub body: HirBlock,
}

/// HIR import declaration
#[derive(Debug, Clone)]
pub struct HirImport {
    pub id: NodeId,
    pub path: String,
    pub resolved_module_id: Option<ModuleId>, // Resolved module ID
    pub alias: Option<String>,
    pub items: Option<Vec<String>>, // None = import all, Some = specific items
    pub resolved_symbols: Vec<SymbolId>, // Resolved imported symbols
}

/// HIR type representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirType {
    /// Unresolved type (contains original string name)
    Unresolved(String),
    /// Resolved type (contains type ID)
    Resolved(TypeId),
    /// Unknown type (for error recovery)
    Unknown,
    /// Primitive types
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    String,
    Char,
    Void,
    Never,
    /// Complex/reference/aggregate types
    Pointer(Box<HirType>),
    Reference(Box<HirType>, bool), // (inner, is_mut)
    Array(Box<HirType>),
    SizedArray(Box<HirType>, usize),
    Optional(Box<HirType>),
    Tuple(Vec<HirType>),
    Function(Vec<HirType>, Box<HirType>),
    /// Nominal types
    Struct(String),
    Enum(String),
    Range,
    /// Generic types
    Generic(HirGenericRef),
    GenericInstance(HirGenericRef, Vec<HirType>),
    /// Union and intersection types (v2.0 features)
    Union(Vec<HirType>),
    Intersection(Vec<HirType>),
    /// Dynamic trait objects
    DynTrait(HirTraitRef),
}

impl HirType {
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
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
                | HirType::Bool
                | HirType::String
                | HirType::Char
                | HirType::Void
        )
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, HirType::Pointer(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, HirType::Array(_) | HirType::SizedArray(_, _))
    }

    pub fn is_optional(&self) -> bool {
        matches!(self, HirType::Optional(_))
    }
}

/// HIR statement
#[derive(Debug, Clone, PartialEq)]
pub struct HirStmt {
    pub id: NodeId,
    pub kind: HirStmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmtKind {
    Expr(HirExpr),
    Let {
        pattern: HirPattern,
        ty: Option<HirType>,
        init: Option<HirExpr>,
    },
    Assign {
        lhs: HirExpr,
        rhs: HirExpr,
    },
    Return(Option<HirExpr>),
    Break(Option<HirExpr>),
    Continue,
}

/// Reference to a generic type parameter
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirGenericRef {
    pub name: String,
    pub scope_id: Option<NodeId>, // ID of the scope that defines this generic
    pub resolved_index: Option<usize>, // Index in the generic parameter list
}

impl HirGenericRef {
    pub fn new(name: String) -> Self {
        Self {
            name,
            scope_id: None,
            resolved_index: None,
        }
    }
}

/// Reference to a trait for dynamic dispatch
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirTraitRef {
    pub name: String,
    pub symbol_id: Option<SymbolId>, // Resolved trait symbol
    pub type_args: Vec<HirType>,     // Type arguments for the trait
}

impl HirTraitRef {
    pub fn new(name: String) -> Self {
        Self {
            name,
            symbol_id: None,
            type_args: Vec::new(),
        }
    }
}

/// HIR expressions
#[derive(Debug, Clone, PartialEq)]
pub struct HirExpr {
    pub id: NodeId,
    pub kind: Box<HirExprKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExprKind {
    /// Literals
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    None,
    Void,

    /// Variables and paths
    Variable(String),
    FieldAccess {
        base: Box<HirExpr>,
        field: String,
    },
    Index {
        base: Box<HirExpr>,
        index: Box<HirExpr>,
    },

    /// Function calls
    Call {
        func: Box<HirExpr>,
        args: Vec<HirExpr>,
    },
    MethodCall {
        receiver: Box<HirExpr>,
        method: String,
        args: Vec<HirExpr>,
    },

    /// Operators
    Unary {
        op: HirUnaryOp,
        expr: Box<HirExpr>,
    },
    Binary {
        op: HirBinaryOp,
        lhs: Box<HirExpr>,
        rhs: Box<HirExpr>,
    },

    /// Control flow
    If {
        condition: Box<HirExpr>,
        then_branch: HirBlock,
        else_branch: Option<HirBlock>,
    },
    Match {
        expr: Box<HirExpr>,
        arms: Vec<HirMatchArm>,
    },
    Loop {
        body: HirBlock,
    },
    While {
        condition: Box<HirExpr>,
        body: HirBlock,
    },
    For {
        pattern: HirPattern,
        iter: Box<HirExpr>,
        body: HirBlock,
    },

    /// Memory operations
    Ref(Box<HirExpr>),
    Deref(Box<HirExpr>),
    Cast {
        expr: Box<HirExpr>,
        ty: HirType,
    },

    /// Constructors
    StructLiteral {
        name: String,
        fields: Vec<(String, HirExpr)>,
    },
    ArrayLiteral(Vec<HirExpr>),
    TupleLiteral(Vec<HirExpr>),

    /// Blocks and scopes
    Block(HirBlock),
    UnsafeBlock(HirBlock),

    /// Async and concurrency (v2.0 features)
    Await(Box<HirExpr>),
    Spawn(HirBlock),

    /// Sugar operators (to be desugared in normalization)
    Pipeline {
        expr: Box<HirExpr>,
        func: Box<HirExpr>,
    },
    PostfixQuestion(Box<HirExpr>),
    PostfixIncrement(Box<HirExpr>),
    PostfixDecrement(Box<HirExpr>),

    /// Template strings
    Template {
        parts: Vec<HirTemplateStringPart>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirTemplateStringPart {
    String(String),
    Expr(HirExpr),
}

/// HIR unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirUnaryOp {
    Not,
    Minus,
    Plus,
    PreInc,
    PostInc,
    PreDec,
    PostDec,
}

/// HIR binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirBinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    // Logical
    And,
    Or,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Assignment
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
    // Range operators
    Range,
    RangeInclusive,
    RangeExclusive,
    RangeFrom,
}

/// HIR block (sequence of statements)
#[derive(Debug, Clone, PartialEq)]
pub struct HirBlock {
    pub id: NodeId,
    pub stmts: Vec<HirStmt>,
    pub expr: Option<Box<HirExpr>>,
}

/// HIR pattern for destructuring
#[derive(Debug, Clone, PartialEq)]
pub struct HirPattern {
    pub id: NodeId,
    pub kind: Box<HirPatternKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirPatternKind {
    Wildcard,
    Variable(String),
    Literal(Box<HirExpr>),
    EnumVariant {
        name: String,
        enum_symbol_id: Option<SymbolId>, // Resolved enum symbol ID
        variant: String,
        variant_symbol_id: Option<SymbolId>, // Resolved variant symbol ID
        patterns: Vec<HirPattern>,
    },
    Struct {
        name: String,
        fields: Vec<(String, HirPattern)>,
    },
    Tuple(Vec<HirPattern>),
    Array(Vec<HirPattern>),
    Range {
        start: Option<Box<HirExpr>>,
        end: Option<Box<HirExpr>>,
        inclusive: bool,
    },
}

/// Field in a struct literal with resolved symbol information
///
/// Field in a struct pattern with resolved symbol information
///
/// HIR match arm
#[derive(Debug, Clone, PartialEq)]
pub struct HirMatchArm {
    pub id: NodeId,
    pub pattern: HirPattern,
    pub guard: Option<HirExpr>,
    pub body: HirMatchArmBody,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirMatchArmBody {
    Expr(HirExpr),
    Block(HirBlock),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hir_type_predicates() {
        assert!(HirType::I32.is_primitive());
        assert!(HirType::Bool.is_primitive());
        assert!(!HirType::Array(Box::new(HirType::I32)).is_primitive());

        assert!(HirType::Pointer(Box::new(HirType::I32)).is_pointer());
        assert!(!HirType::I32.is_pointer());

        assert!(HirType::Array(Box::new(HirType::I32)).is_array());
        assert!(HirType::SizedArray(Box::new(HirType::I32), 10).is_array());
        assert!(!HirType::I32.is_array());

        assert!(HirType::Optional(Box::new(HirType::I32)).is_optional());
        assert!(!HirType::I32.is_optional());
    }

    #[test]
    fn test_span_map() {
        let mut span_map = SpanMap::new();
        let node_id = NodeId::new(0);
        let span = Span::new(codespan::ByteIndex(0), codespan::ByteIndex(0));

        span_map.insert(node_id, span);
        assert_eq!(span_map.get(node_id), Some(span));
        assert_eq!(span_map.get(NodeId::new(1)), None);
    }
}
