# Programming Language Specification

*Generated from parser analysis - Version 1.0*

## Table of Contents

1. [Language Overview](#language-overview)
2. [Lexical Structure](#lexical-structure)
3. [Grammar and Syntax](#grammar-and-syntax)
4. [Types and Values](#types-and-values)
5. [Expressions](#expressions)
6. [Statements](#statements)
7. [Declarations](#declarations)
8. [Import and Export System](#import-and-export-system)
9. [Foreign Function Interface (FFI)](#foreign-function-interface-ffi)
10. [Pattern Matching](#pattern-matching)
11. [Examples](#examples)
12. [Complete Grammar Reference](#complete-grammar-reference)

---

## 1. Language Overview

### 1.1 Introduction
This is a statically-typed, compiled programming language with modern features including pattern matching, generics, FFI support, and comprehensive type safety. The language combines imperative and functional programming paradigms with a focus on performance and safety.

### 1.2 Design Principles
- **Type Safety**: Strong static typing with type inference
- **Memory Safety**: Controlled memory access with pointer types
- **Interoperability**: FFI support for C-style function calls
- **Expressiveness**: Pattern matching and rich expression syntax
- **Modularity**: Comprehensive import/export system

### 1.3 Key Features
- Generic types and functions
- Pattern matching with guards
- Template string interpolation
- Array and range expressions
- Method calls and field access
- Foreign function interface
- Test framework integration
- Constructor functions

---

## 2. Lexical Structure

### 2.1 Character Set
The language uses UTF-8 encoding for source files.

### 2.2 Keywords
```
as        break     constructor continue  else      enum      export    
false     fn        for       foreign   from      if        impl      
import    in        let       loop      match     new       none      
rawptr    return    step      struct    test      true      var       
while
```

### 2.3 Identifiers
```
identifier ::= letter (letter | digit | '_')*
letter     ::= 'a'..'z' | 'A'..'Z'  
digit      ::= '0'..'9'
```

### 2.4 Literals

#### 2.4.1 Integer Literals
```
integer_literal ::= decimal_literal | hex_literal | binary_literal
decimal_literal ::= digit+
hex_literal     ::= '0x' hex_digit+
binary_literal  ::= '0b' ('0' | '1')+
```

#### 2.4.2 Floating Point Literals
```
float_literal ::= digit+ '.' digit+
```

#### 2.4.3 String Literals
```
string_literal    ::= '"' string_char* '"'
template_string   ::= '`' template_char* '`'
string_char       ::= ~['"' '\n' '\r' '\t'] | escape_sequence
template_char     ::= ~['`'] | '{' expression '}' | escape_sequence
escape_sequence   ::= '\' ('n' | 'r' | 't' | '\\' | '\'' | '"')
```

#### 2.4.4 Boolean Literals
```
bool_literal ::= 'true' | 'false'
```

#### 2.4.5 None Literal
```
none_literal ::= 'none'
```

### 2.5 Operators and Punctuation
```
Arithmetic:  + - * / % ** ^
Assignment:  =
Comparison:  == != > < >= <=
Logical:     && || !
Access:      . [] 
Range:       .. ..= ..> ..<
Pointer:     *
Cast:        as
Optional:    ?
Delimiters:  ( ) [ ] { }
Separators:  , ; :
Arrows:      -> =>
Special:     ... # @
```

### 2.6 Comments
```
line_comment  ::= '//' ~[\r\n]*
block_comment ::= '/*' (~'*' | '*' ~'/')* '*/'
```

---

## 3. Grammar and Syntax

### 3.1 Program Structure
```ebnf
program ::= item*

item ::= import_declaration
       | export_declaration
       | function_declaration
       | struct_declaration  
       | enum_declaration
       | impl_block
       | test_declaration
       | ffi_declaration
       | statement
```

### 3.2 Import/Export System
```ebnf
import_declaration ::= 'import' import_specifier ';'
                     | 'import' '{' import_list '}' 'from' module_path ';'

export_declaration ::= 'export' exportable_item
                     | 'export' '{' exportable_item* '}'

import_specifier ::= module_path ('as' identifier)?
import_list      ::= import_item (',' import_item)*
import_item      ::= identifier ('as' identifier)?
module_path      ::= identifier ('/' identifier)*

exportable_item ::= function_declaration
                  | struct_declaration
                  | enum_declaration
```

### 3.3 Foreign Function Interface
```ebnf
ffi_declaration ::= ('#[' metadata ']')? 'foreign' ffi_item

ffi_item ::= 'fn' identifier '(' parameter_list? ')' ('->' type)? ';'
           | 'var' identifier ':' type ';'

metadata ::= metadata_item (',' metadata_item)*
metadata_item ::= identifier ('=' metadata_value)?
metadata_value ::= string_literal | identifier | bool_literal
```

---

## 4. Types and Values

### 4.1 Primitive Types

#### 4.1.1 Integer Types
- **Signed**: `i8`, `i16`, `i32`, `i64`
- **Unsigned**: `u8`, `u16`, `u32`, `u64`

#### 4.1.2 Floating Point Types
- `f32`: 32-bit IEEE 754 floating point
- `f64`: 64-bit IEEE 754 floating point

#### 4.1.3 Boolean Type
- `bool`: `true` or `false`

#### 4.1.4 String Type
- `string`: UTF-8 encoded string

#### 4.1.5 Special Types
- `void`: No value (unit type)
- `any`: Dynamic type (type erasure)
- `rawptr`: Raw pointer type

### 4.2 Composite Types

#### 4.2.1 Arrays
```
[i32; 5]     // Fixed-size array of 5 i32 values
[i32]        // Dynamic array of i32 values
[]           // Empty array literal
```

#### 4.2.2 Optional Types
```
i32?         // Optional i32 (can be none)
```

#### 4.2.3 Pointer Types
```
*i32         // Pointer to i32
```

#### 4.2.4 Generic Types
```
Array<T>     // Generic array type
Option<T>    // Generic optional type
```

### 4.3 User-Defined Types

#### 4.3.1 Structures
```
struct Point {
    x: f64,
    y: f64,
}
```

#### 4.3.2 Enumerations
```
enum Color {
    Red,
    Green, 
    Blue,
    RGB(u8, u8, u8),
}
```

---

## 5. Expressions

### 5.1 Literal Expressions
```
42          // Integer literal
3.14        // Float literal  
"hello"     // String literal
`Hello {name}!` // Template string
true        // Boolean literal
none        // None literal
[]          // Empty array
```

### 5.2 Arithmetic Expressions
```
a + b       // Addition
a - b       // Subtraction  
a * b       // Multiplication
a / b       // Division
a % b       // Modulo
a ** b      // Power
a ^ b       // Alternative power
```

### 5.3 Comparison Expressions
```
a == b      // Equality
a != b      // Inequality
a > b       // Greater than
a < b       // Less than
a >= b      // Greater than or equal
a <= b      // Less than or equal
```

### 5.4 Logical Expressions
```
a && b      // Logical AND
a || b      // Logical OR
!a          // Logical NOT
```

### 5.5 Assignment Expressions
```
a = b       // Assignment
```

### 5.6 Access Expressions
```
obj.field   // Field access
obj.method()// Method call
array[index]// Array indexing
```

### 5.7 Range Expressions
```
1..10       // Exclusive range [1, 10)
1..=10      // Inclusive range [1, 10]
..>10       // Infinite up to 10
..<10       // Infinite down to 10
..          // Infinite range
```

### 5.8 Constructor Expressions
```
new Point(1.0, 2.0)     // Constructor call
Point { x: 1.0, y: 2.0 }// Struct initialization
[1, 2, 3, 4]            // Array initialization
```

### 5.9 Cast Expressions
```
value as i32    // Type casting
```

### 5.10 Unary Expressions
```
-value      // Negation
+value      // Unary plus
!value      // Logical not
*pointer    // Dereference
...spread   // Spread operator
```

### 5.11 Control Flow Expressions

#### 5.11.1 If Expressions
```
if condition {
    value1
} else {
    value2
}
```

#### 5.11.2 Match Expressions
```
match value {
    pattern1 => result1,
    pattern2 if guard => result2,
    _ => default_result,
}
```

#### 5.11.3 Loop Expressions
```
loop {
    // infinite loop body
}
```

---

## 6. Statements

### 6.1 Variable Declarations
```ebnf
let_statement ::= 'let' identifier_list (':' type)? '=' expression_list ';'
identifier_list ::= identifier (',' identifier)*
expression_list ::= expression (',' expression)*
```

**Examples:**
```
let x = 42;
let y: i32 = 100;
let a, b = 1, 2;
let name: string = "Alice";
```

### 6.2 Expression Statements
```ebnf
expression_statement ::= expression ';'?
```

### 6.3 Control Flow Statements

#### 6.3.1 If Statements
```ebnf
if_statement ::= 'if' expression block ('else' (if_statement | block))?
```

**Examples:**
```
if x > 0 {
    print("positive");
} else if x < 0 {
    print("negative");
} else {
    print("zero");
}
```

#### 6.3.2 While Loops
```ebnf
while_statement ::= 'while' expression block
```

**Example:**
```
while i < 10 {
    print(i);
    i = i + 1;
}
```

#### 6.3.3 For Loops
```ebnf
for_statement ::= 'for' for_pattern 'in' expression ('step' expression)? block
for_pattern ::= identifier (',' identifier)?
```

**Examples:**
```
for i in 0..10 {
    print(i);
}

for value, index in array {
    print(index, value);
}

for i in 0..100 step 2 {
    print(i);  // prints even numbers
}
```

#### 6.3.4 Loop Statements
```ebnf
loop_statement ::= 'loop' block
```

#### 6.3.5 Match Statements
```ebnf
match_statement ::= 'match' expression '{' match_arm* '}'
match_arm ::= pattern ('if' expression)? '=>' (expression | block) ','?
```

### 6.4 Jump Statements
```ebnf
return_statement ::= 'return' expression? ';'
break_statement ::= 'break' expression? ';'  
continue_statement ::= 'continue' ';'
```

---

## 7. Declarations

### 7.1 Function Declarations
```ebnf
function_declaration ::= 'fn' identifier generic_params? parameter_list 
                        ('->' type)? block

generic_params ::= '<' identifier (',' identifier)* '>'
parameter_list ::= '(' parameter (',' parameter)* ')'
parameter ::= identifier ':' type | '...' identifier? ':' type | '...'
```

**Examples:**
```
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn generic_func<T>(value: T) -> T {
    value
}

fn variadic_func(...args: any) {
    // handle variadic arguments
}
```

### 7.2 Struct Declarations
```ebnf
struct_declaration ::= 'struct' identifier generic_params? '{' struct_field* '}'
struct_field ::= identifier ':' type ','?
```

**Example:**
```
struct Point<T> {
    x: T,
    y: T,
}
```

### 7.3 Enum Declarations
```ebnf
enum_declaration ::= 'enum' identifier generic_params? '{' enum_variant* '}'
enum_variant ::= identifier enum_data? ('=' integer_literal)? ','?
enum_data ::= '(' type (',' type)* ')'
```

**Examples:**
```
enum Status {
    Pending,
    Running,
    Completed = 100,
}

enum Result<T, E> {
    Ok(T),
    Error(E),
}
```

### 7.4 Implementation Blocks
```ebnf
impl_block ::= 'impl' type '{' impl_item* '}'
impl_item ::= function_declaration | constructor_declaration

constructor_declaration ::= 'constructor' parameter_list '->' type block
```

**Example:**
```
impl Point {
    fn distance(self, other: Point) -> f64 {
        // calculate distance
    }
    
    constructor(x: f64, y: f64) -> Point {
        Point { x: x, y: y }
    }
}
```

### 7.5 Test Declarations
```ebnf
test_declaration ::= 'test' identifier block
```

**Example:**
```
test basic_math {
    assert(add(2, 3) == 5);
}
```

---

## 8. Import and Export System

### 8.1 Import Declarations

#### 8.1.1 Import All
```
import std/io;                    // Import all from standard library
import ./local_module;            // Import from local file
import ../parent_module;          // Import from parent directory
import external_crate;            // Import external crate
```

#### 8.1.2 Import with Alias
```
import std/collections as collections;
```

#### 8.1.3 Import Specific Items
```
import { HashMap, Vec } from std/collections;
import { add, subtract as sub } from ./math;
```

### 8.2 Export Declarations

#### 8.2.1 Export Individual Items
```
export fn public_function() { }
export struct PublicStruct { }
```

#### 8.2.2 Export Block
```
export {
    fn function1() { }
    fn function2() { }
    struct DataType { }
}
```

#### 8.2.3 Re-export
```
export import * from ./internal_module;
export import { specific_item } from ./other_module;
```

### 8.3 Module Types
- **Standard**: Modules from `std/` prefix (built-in standard library)
- **Local**: Modules starting with `./` or `../` (relative paths)
- **External**: Third-party modules (no prefix)

---

## 9. Foreign Function Interface (FFI)

### 9.1 FFI Function Declarations
```
foreign fn malloc(size: u64) -> rawptr;
foreign fn free(ptr: rawptr);

#[link_name = "custom_name"]
foreign fn system_call(code: i32) -> i32;
```

### 9.2 FFI Variable Declarations  
```
foreign var errno: i32;
foreign var global_counter: u64;
```

### 9.3 Metadata Attributes
FFI declarations can include metadata for linking:
```
#[link_name = "actual_symbol_name", calling_convention = "cdecl"]
foreign fn complex_function(a: i32, b: f64) -> bool;
```

---

## 10. Pattern Matching

### 10.1 Pattern Types

#### 10.1.1 Literal Patterns
```
match value {
    42 => "forty-two",
    "hello" => "greeting", 
    true => "boolean true",
    none => "no value",
}
```

#### 10.1.2 Variable Patterns
```
match value {
    x => x + 1,  // Binds value to x
}
```

#### 10.1.3 Wildcard Pattern
```
match value {
    _ => "default case",
}
```

#### 10.1.4 Enum Patterns
```
match color {
    Color.Red => "red color",
    Color.RGB(r, g, b) => format("rgb({}, {}, {})", r, g, b),
    Color.RGB(255, g, b) if g > 128 => "bright color",
}
```

### 10.2 Pattern Guards
Patterns can include conditional guards:
```
match value {
    x if x > 0 => "positive",
    x if x < 0 => "negative", 
    _ => "zero",
}
```

### 10.3 Match Arms
Match arms can contain either expressions or blocks:
```
match status {
    Status.Ok => simple_value,
    Status.Error => {
        log_error();
        handle_error()
    },
}
```

---

## 11. Examples

### 11.1 Hello World
```
import std/io;

fn main() {
    print("Hello, World!");
}
```

### 11.2 Factorial Function
```
fn factorial(n: u32) -> u32 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

test factorial_test {
    assert(factorial(5) == 120);
}
```

### 11.3 Generic Data Structure
```
struct Stack<T> {
    items: [T],
}

impl Stack<T> {
    constructor() -> Stack<T> {
        Stack { items: [] }
    }
    
    fn push(self, item: T) {
        self.items.push(item);
    }
    
    fn pop(self) -> T? {
        if self.items.length() > 0 {
            self.items.pop()
        } else {
            none
        }
    }
}
```

### 11.4 Pattern Matching Example
```
enum Result<T, E> {
    Ok(T),
    Error(E),
}

fn handle_result<T>(result: Result<T, string>) -> T {
    match result {
        Result.Ok(value) => value,
        Result.Error(msg) => {
            print(`Error: {msg}`);
            panic("Result was error")
        },
    }
}
```

### 11.5 FFI Usage
```
#[link_name = "libc"]
foreign fn strlen(s: rawptr) -> u64;
foreign fn malloc(size: u64) -> rawptr;
foreign fn free(ptr: rawptr);

fn string_length(s: string) -> u64 {
    unsafe {
        strlen(s.as_ptr())
    }
}
```

### 11.6 Template Strings
```
fn greet(name: string, age: i32) {
    let message = `Hello {name}! You are {age} years old.`;
    print(message);
}
```

### 11.7 Range and Array Operations
```
fn sum_range(start: i32, end: i32) -> i32 {
    let mut total = 0;
    for i in start..end {
        total = total + i;
    }
    total
}

fn process_array(arr: [i32]) {
    for value, index in arr {
        print(`Item {index}: {value}`);
    }
}
```

---

## 12. Complete Grammar Reference

### 12.1 Operator Precedence (Highest to Lowest)
1. Field access (`.`), Array access (`[]`) - Left associative
2. Type casting (`as`) - Left associative  
3. Range operators (`..`, `..=`, `..>`, `..<`) - Left associative
4. Power (`**`, `^`) - Right associative
5. Multiplication, Division, Modulo (`*`, `/`, `%`) - Left associative
6. Addition, Subtraction (`+`, `-`) - Left associative
7. Comparison (`>`, `<`, `>=`, `<=`) - Left associative
8. Equality (`==`, `!=`) - Left associative
9. Logical AND (`&&`) - Left associative
10. Logical OR (`||`) - Left associative  
11. Assignment (`=`) - Right associative

### 12.2 Statement Grammar
```ebnf
statement ::= let_statement
            | expression_statement
            | if_statement
            | while_statement
            | for_statement  
            | loop_statement
            | match_statement
            | return_statement
            | break_statement
            | continue_statement
            | block_statement

block_statement ::= '{' statement* '}'
```

### 12.3 Expression Grammar
```ebnf
expression ::= assignment_expression

assignment_expression ::= logical_or_expression
                        | logical_or_expression '=' assignment_expression

logical_or_expression ::= logical_and_expression ('||' logical_and_expression)*

logical_and_expression ::= equality_expression ('&&' equality_expression)*

equality_expression ::= relational_expression (('==' | '!=') relational_expression)*

relational_expression ::= additive_expression (('<' | '>' | '<=' | '>=') additive_expression)*

additive_expression ::= multiplicative_expression (('+' | '-') multiplicative_expression)*

multiplicative_expression ::= power_expression (('*' | '/' | '%') power_expression)*

power_expression ::= cast_expression (('**' | '^') power_expression)*

cast_expression ::= range_expression ('as' type)*

range_expression ::= postfix_expression (('..' | '..=' | '..>' | '..<') postfix_expression)*

postfix_expression ::= prefix_expression postfix_operator*

postfix_operator ::= '[' expression ']'
                   | '(' argument_list? ')'
                   | '.' identifier ('(' argument_list? ')')?

prefix_expression ::= ('*' | '+' | '-' | '!' | '...') prefix_expression
                    | primary_expression

primary_expression ::= literal
                     | identifier
                     | '(' expression ')'
                     | array_literal
                     | struct_literal
                     | 'new' identifier '(' argument_list? ')'
                     | if_expression
                     | match_expression
                     | loop_expression

literal ::= integer_literal
          | float_literal
          | string_literal
          | template_string_literal
          | bool_literal
          | none_literal
```

### 12.4 Type Grammar
```ebnf
type ::= base_type type_modifier*

type_modifier ::= '?'           // Optional type
                | '[' ']'       // Array type  
                | '[' integer_literal ']'  // Sized array

base_type ::= primitive_type
            | identifier generic_args?
            | '*' base_type
            | '[' type (';' integer_literal)? ']'
            | '...'

primitive_type ::= 'i8' | 'i16' | 'i32' | 'i64'
                 | 'u8' | 'u16' | 'u32' | 'u64'
                 | 'f32' | 'f64'
                 | 'bool' | 'string' | 'void' | 'any'
                 | 'rawptr'

generic_args ::= '<' type (',' type)* '>'
```

---

*This specification covers the complete syntax and semantics of the programming language as implemented in the provided parser. The language provides a modern, type-safe programming environment with comprehensive features for system programming, application development, and FFI integration.*