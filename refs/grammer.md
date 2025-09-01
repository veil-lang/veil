(* Complete EBNF Grammar for the Veil Language edition,the full language specification v2.0.*)

(* -------------------------------------------------- *) (* 1. Top-Level Program Structure & Compilation Units *) (* -------------------------------------------------- *)

(* A Veil program is a collection of items within a compilation unit (a single file). The order of declarations generally does not matter, with the exception of some comptime metaprogramming scenarios. *) program ::= ( item )* ;

(* An 'item' is a top-level declaration or statement that can exist at the module scope. *) item ::= import_declaration | export_declaration | function_declaration | struct_declaration | enum_declaration | union_declaration | trait_declaration | impl_block | type_alias | ffi_declaration | statement ;

(* --------------------------------- *) (* 2. Module System & Visibility *) (* --------------------------------- *)

(* Note: The spec is inconsistent. Prose examples (e.g., sec 3.7) use Rust-style `use` and `mod`, while the formal grammar section (11.2) uses `import`/`export`. This grammar follows the formal specification in section 11.2, which is more aligned with languages like JavaScript or Python. *) import_declaration ::= 'import' ( module_path ( 'as' identifier )? | '{' import_list '}' 'from' module_path ) ';' ; import_list ::= import_item ( ',' import_item )* ; import_item ::= identifier ( 'as' identifier )? ; module_path ::= identifier ( ( '::' | '/' ) identifier )* ; (* Supports both `::` and `/` as path separators. *)

export_declaration ::= 'export' ( function_declaration | struct_declaration | enum_declaration | type_alias ) ;

(* Visibility modifiers control how items can be accessed from other modules.

- `pub`: Publicly visible to any module that imports this one.

- `pub(crate)`: Visible only within the same compilation crate.

- `pub(super)`: Visible to the parent module.

- `pub(in path)`: Visible within the specified module path. Default is private to the current module. *) visibility ::= 'pub' ( '(' ( 'crate' | 'super' | 'in' module_path ) ')' )? ;

(* --------------------------------- *) (* 3. Declarations *) (* --------------------------------- *)

(* A function declaration, which can be a standalone function or a method within an impl block. It can be generic, async, and specify a return type. *) function_declaration ::= attribute* visibility? ( 'async' )? 'fn' identifier generic_params? parameter_list ( '->' type )? ( where_clause )? ( block | ';' ) ; parameter_list ::= '(' ( parameter ( ',' parameter )* )? ')' ; parameter ::= ( '...' )? identifier ':' type ; (* `...` indicates a variadic parameter. *)

(* Structs define custom data structures. They come in three forms:

1. C-style structs with named fields: `struct Point { x: f64, y: f64 }`

2. Tuple structs with unnamed fields: `struct Color(u8, u8, u8);`

3. Unit-like structs with no fields: `struct Unit;` *) struct_declaration ::= attribute* visibility? 'struct' identifier generic_params? ( where_clause )? ( struct_body | ';' ) ; struct_body ::= '{' ( struct_field ( ',' struct_field )* )? '}' | '(' ( tuple_field ( ',' tuple_field )* )? ')' ';' ; struct_field ::= attribute* visibility? identifier ':' type ; tuple_field ::= attribute* visibility? type ;

(* Enums (or tagged unions) define a type that can be one of several variants. *) enum_declaration ::= attribute* visibility? 'enum' identifier generic_params? ( where_clause )? '{' ( enum_variant ( ',' enum_variant )* )? '}' ; enum_variant ::= identifier ( enum_data )? ( '=' expression )? ; (* Can have an associated value for C-style enums. *) enum_data ::= '(' ( type ( ',' type )* )? ')' ;

(* Unions are for unsafe memory sharing between different types, primarily for FFI. *) union_declaration ::= attribute* visibility? 'union' identifier generic_params? ( where_clause )? '{' ( struct_field ( ',' struct_field )* )? '}' ;

(* Traits define shared behavior. They can contain method signatures, associated types, and constants. *) trait_declaration ::= attribute* visibility? 'trait' identifier generic_params? ( ':' trait_bounds )? ( where_clause )? '{' ( trait_item )* '}' ; trait_item ::= 'fn' identifier generic_params? parameter_list ( '->' type )? ( where_clause )? ( block | ';' ) (* Can have default implementation *) | 'type' identifier ( ':' trait_bounds )? ';' | 'const' identifier ':' type ';' ;

(* Impl blocks provide implementations for structs, enums, or traits. *) impl_block ::= 'impl' generic_params? ( type | trait_bound 'for' type ) ( where_clause )? '{' ( function_declaration )* '}' ;

(* Type aliases provide a new name for an existing type. *) type_alias ::= 'type' identifier generic_params? '=' type ';' ;

(* Foreign Function Interface for calling C-compatible functions. *) ffi_declaration ::= 'extern' string_literal '{' ( ffi_item )* '}' ; ffi_item ::= attribute* 'fn' identifier parameter_list ( '->' type )? ';' ;

(* Attributes provide metadata to the compiler, e.g., for conditional compilation or code generation. *) attribute ::= '#[' identifier ( '(' literal ( ',' literal )* ')' )? ']' ;

(* --------------------------------- *) (* 4. Statements *) (* --------------------------------- *)

statement ::= variable_declaration | expression_statement | if_statement | while_statement | for_statement | loop_statement | match_statement | return_statement | break_statement | continue_statement | block ;

(* Variable bindings. `const` for immutable, `var` for mutable. Supports destructuring. *) variable_declaration ::= ( 'const' | 'var' ) identifier_list ( ':' type )? '=' expression_list ';' ; identifier_list ::= identifier ( ',' identifier )* ; expression_list ::= expression ( ',' expression )* ;

expression_statement ::= expression ';' ; block ::= '{' ( statement )* '}' ; (* A block is a sequence of statements, and is also an expression. *)

(* --------------------------------- *) (* 5. Control Flow *) (* --------------------------------- *)

(* `if let` provides a convenient way to match a pattern and execute a block. *) if_statement ::= 'if' ( 'let' pattern '=' )? expression block ( 'else' ( if_statement | block ) )? ; while_statement ::= 'while' ( 'let' pattern '=' )? expression block ; for_statement ::= 'for' pattern 'in' expression block ; loop_statement ::= ( identifier ':' )? 'loop' block ; (* Loops can be labeled. *)

match_statement ::= 'match' expression '{' ( match_arm ( ',' match_arm )* )? '}' ; match_arm ::= pattern ( 'if' expression )? '=>' ( expression | block ) ; (* Arms can have optional guards. *)

return_statement ::= 'return' ( expression )? ';' ; break_statement ::= 'break' ( identifier )? ( expression )? ';' ; continue_statement ::= 'continue' ( identifier )? ';' ;

(* --------------------------------- *) (* 6. Expressions (by precedence) *) (* --------------------------------- *)

expression ::= assignment_expression ;

assignment_expression ::= pipeline_expression ( ( '=' | '+=' | '-=' | '*=' | '/=' | '//=' | '%=' | '**=' | '<<=' | '>>=' | '&=' | '^=' | '|=' ) assignment_expression )? ;

pipeline_expression ::= logical_or_expression ( '|>' logical_or_expression )* ;

logical_or_expression ::= logical_and_expression ( '|' logical_and_expression )* ;

logical_and_expression ::= equality_expression ( '&' equality_expression )* ;

equality_expression ::= relational_expression ( ( '==' | '!=' ) relational_expression )* ;

relational_expression ::= shift_expression ( ( '<' | '>' | '<=' | '>=' | 'is' | 'is not' | 'in' | 'not in' ) shift_expression )* ;

shift_expression ::= additive_expression ( ( '<<' | '>>' ) additive_expression )* ;

additive_expression ::= multiplicative_expression ( ( '+' | '-' ) multiplicative_expression )* ;

multiplicative_expression ::= power_expression ( ( '*' | '/' | '//' | '%' ) power_expression )* ;

power_expression ::= cast_expression ( '**' power_expression )? ;

cast_expression ::= unary_expression ( 'as' type )* ;

unary_expression ::= ( '!' | '+' | '-' | '~' | '&' | '*' | 'await' | '++' | '--' ) unary_expression | postfix_expression ;

postfix_expression ::= primary_expression ( postfix_operator )* ; postfix_operator ::= '.' identifier (* Member access: obj.field *) | '[' expression ']' (* Indexing: arr[i] *) | '(' argument_list? ')' (* Function call: func(a, b) *) | '?' (* Error propagation: result? *) | '++' | '--' ; (* Postfix increment/decrement: x++ *)

primary_expression ::= literal | identifier | '(' expression ')' | array_literal | struct_literal | if_expression | match_expression | loop_expression | try_expression | 'async'? block | unsafe_block | closure_expression | 'comptime' block ;

argument_list ::= expression ( ',' expression )* ;

(* Control Flow Expressions *) if_expression ::= 'if' ( 'let' pattern '=' )? expression block ( 'else' ( if_expression | block ) ) ; match_expression ::= 'match' expression '{' ( match_arm ( ',' match_arm )* )? '}' ; loop_expression ::= ( identifier ':' )? 'loop' block ; try_expression ::= 'try' block ; unsafe_block ::= 'unsafe' block ; closure_expression ::= ( 'async' )? ( 'move' )? '|' ( parameter ( ',' parameter )* )? '|' ( '->' type )? ( expression | block ) ;

(* --------------------------------- *) (* 7. Patterns *) (* --------------------------------- *)

(* Patterns are used in match statements, let bindings, and function parameters to destructure data. *) pattern ::= literal | identifier ( '@' pattern )? (* `name @ Some(_)` binds the whole pattern to `name` *) | '_' (* Wildcard, ignores a value *) | '...' (* Rest pattern, for arrays and structs *) | type '{' ( field_pattern ( ',' field_pattern )* ( ',' '...' )? )? '}' (* Struct pattern *) | type '(' ( pattern ( ',' pattern )* )? ')' (* Tuple struct pattern *) | '[' ( pattern ( ',' pattern )* )? ']' (* Array/slice pattern *) | ( '&' )? pattern (* Reference pattern *) | range_pattern ;

field_pattern ::= identifier ':' pattern ; range_pattern ::= expression ( '..' | '..=' ) expression ; (* e.g., `1..10` or `1..=10` *)

(* --------------------------------- *) (* 8. Types & Generics *) (* --------------------------------- *)

type ::= type_expression ('?')? ; (* The trailing `?` makes the type optional, e.g., `string?` *)

(* A type expression can be a simple type, or a union/intersection of simple types. *) type_expression ::= simple_type ( ( '|' | '&' ) simple_type )* ;

simple_type ::= primitive_type | module_path ( '::' identifier )* generic_args? (* User-defined type *) | '[' type ( ';' expression )? ']' (* Array type: `[i32]` or `[i32; 5]` *) | '&' type (* Borrowed reference *) | '*' type (* Raw pointer *) | 'weak' type (* Weak reference for ARC *) | 'dyn' trait_bounds ; (* Dynamic dispatch trait object *)

primitive_type ::= 'bool' | 'ch' | 'str' | 'i8' | 'i16' | 'i32' | 'i64' | 'i128' | 'isize' | 'u8' | 'u16' | 'u32' | 'u64' | 'u128' | 'usize' | 'f32' | 'f64' | 'rawptr' | 'void' ;

generic_params ::= '<' ( generic_param ( ',' generic_param )* )? '>' ; generic_param ::= identifier ( ':' trait_bounds )? ; generic_args ::= '<' ( type ( ',' type )* )? '>' ;

where_clause ::= 'where' ( where_predicate ( ',' where_predicate )* )? ; where_predicate ::= type ':' trait_bounds ;

trait_bounds ::= trait_bound ( '+' trait_bound )* ; trait_bound ::= ( '?' )? module_path ( '::' identifier )* generic_args? ;

(* --------------------------------- *) (* 9. Literals & Terminals *) (* --------------------------------- *)

literal ::= integer_literal | float_literal | string_literal | template_string_literal | boolean_literal | 'none' ;

array_literal ::= '[' ( expression ( ',' expression )* )? ']' ; struct_literal ::= identifier '{' ( identifier ':' expression ( ',' identifier ':' expression )* )? '}' ;

identifier ::= ( 'a'..'z' | 'A'..'Z' | '*' ) ( 'a'..'z' | 'A'..'Z' | '*' | '0'..'9' )* ; integer_literal ::= ( '0'..'9' )+ ( '_' ( '0'..'9' )+ )* ; float_literal ::= ( '0'..'9' )+ '.' ( '0'..'9' )* ( ( 'e' | 'E' ) ( '+' | '-' )? ( '0'..'9' )+ )? ; string_literal ::= '"' ( [^"\] | '\' . )* '"' ; template_string_literal ::= '`' ( [^`\] | '\' . | '{' expression '}' )* '`' ; boolean_literal ::= 'true' | 'false' ;
