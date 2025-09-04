# Veil language test suite (v2.0, implemented subset)

Purpose
- This directory hosts .veil programs that validate every feature currently implemented in the parser/AST/HIR/typecheck pipeline.
- Tests are discovered by `test <name> { ... }` blocks in .veil files. Each test compiles independently; success = program compiles.

How to run
- List tests: veil test veil/tests --list
- Run all: veil test veil/tests
- Run one: veil test veil/tests --test-name <name>
- Verbose: add --verbose

Conventions
- One topic per file; multiple test blocks per file.
- Keep everything a test uses inside its block (no reliance on other tests).
- Prefer primitives and simple expressions (the runner compiles but does not execute assertions yet).
- Prelude is auto-injected. Avoid extra imports unless necessary.

Planned files and reference content
- Copy/paste each block below into the corresponding file under veil/tests/

```veil/tests/basics.veil#L1-200
/# Basics: declarations, expressions, return, comments #/

test decl_parse_smoke {
    let a: i32 = 1;
    let b: i32 = 2;
    let c = a + b;
    let d = c * 3;
    let e = d - 4;
    let f = e % 2;
    let g = f == 0;
    let h = !g;
    /# comments and extra semicolons #/
    let _sink = h;;
    return;
}

test blocks_and_shadowing {
    let x: i32 = 10;
    {
        let x: i32 = 20;
        let _ = x;
    }
    let _ = x;
    return;
}
```

```veil/tests/literals_and_operators.veil#L1-200
/# Literals and operators: ints, floats, strings, bools, comparisons, logical #/

test integer_and_float_literals {
    let a = 0;
    let b = 123;
    let c = 0xFF;
    let d = 0b1010;
    let f = 1.5;
    let big = 4294967296; /# i64 path in parser #/
    let _sink = a + b + c + d + (f as f64 as i64) + big;
    return;
}

test string_and_bool_literals {
    let s: string = "hello";
    let t: str = "world";
    let u = `hi {s} {1 + 2}`;
    let ok = true;
    let no = false;
    let _sink = ok | no;
    return;
}

test comparisons_and_equality {
    let a: i32 = 3;
    let b: i32 = 4;
    let _ = a < b;
    let _ = a <= b;
    let _ = a > b;
    let _ = a >= b;
    let _ = a == b;
    let _ = a != b;
    return;
}

test logical_ops_bool_only {
    let t = true;
    let f = false;
    let _ = t & f;
    let _ = t | f;
    /# Deprecated (still parsed; may warn) #/
    let _ = t && f;
    let _ = t || f;
    return;
}
```

```veil/tests/division.veil#L1-200
/# Division operators: / (float) and // (integer) #/

test float_division {
    let a = 5.0;
    let b = 2.0;
    let _ = a / b;
    return;
}

test integer_division {
    let x: i32 = 5 as i32;
    let y: i32 = 2 as i32;
    let _ = x // y;
    return;
}

test mixed_division_in_expression {
    let a: i32 = 10 as i32;
    let b: i32 = 3 as i32;
    let f = 9.0;
    let _ = (f / 2.0) + (a // b) as f64;
    return;
}
```

```veil/tests/functions_and_calls.veil#L1-200
/# Functions and calls #/

fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn noop() -> void {
    return;
}

test call_minimal_no_args {
    noop();
    return;
}

test call_with_args_and_nesting {
    let r = add(add(1 as i32, 2 as i32), 3 as i32);
    let _sink = r;
    return;
}
```

```veil/tests/control_flow.veil#L1-200
/# if/else, while, loop, for-ranges #/

test if_else_chains {
    let x: i32 = 5 as i32;
    if x > 10 {
        let _ = 1;
    } else if x == 10 {
        let _ = 2;
    } else {
        let _ = 3;
    }
    return;
}

test while_loop_counter {
    let i: i32 = 0 as i32;
    while i < 3 {
        let _tick = i;
        let i2 = i + 1;
        let i = i2; /# simple rebinding in block #/
    }
    return;
}

test loop_and_break_like_pattern {
    let i: i32 = 0 as i32;
    loop {
        let i2 = i + 1;
        let i = i2;
        if i >= 2 {
            return;
        }
    }
}

test for_over_range_exclusive {
    /# for pattern in expr (step expr)? #/
    for i in 0 .. 3 {
        let _ = i;
    }
    return;
}

test for_over_range_inclusive {
    for i in 0 ..= 2 {
        let _ = i;
    }
    return;
}
```

```veil/tests/ranges_and_arrays.veil#L1-200
/# Ranges and arrays (literal + index) #/

test array_literal_and_indexing {
    let arr = [1 as i32, 2 as i32, 3 as i32];
    let a0 = arr[0];
    let a1 = arr[1];
    let _sink = a0 + a1;
    return;
}

test sized_array_type_annotation {
    let xs: [i32; 3] = [1 as i32, 2 as i32, 3 as i32];
    let _ = xs[2];
    return;
}

test infinite_ranges_compile_only {
    let _up = ..>;
    let _down = ..<;
    let _inf = ..;
    return;
}
```

```veil/tests/casts_and_types.veil#L1-200
/# Casts and primitive types #/

test simple_as_casts {
    let i = 1 as i32;
    let j = 2 as i32;
    let sum = (i + j) as i64;
    let f = 3.25 as f64;
    let _sink = sum as i32 + (f as i32);
    return;
}

test primitive_type_annotations {
    let a: i8 = 1 as i8;
    let b: i16 = 2 as i16;
    let c: i32 = 3 as i32;
    let d: i64 = 4 as i64;
    let u: u32 = 10 as u32;
    let fl: f32 = 1.0 as f32;
    let dbl: f64 = 2.0;
    let s: string = "ok";
    let z: void = (return;); /# expression to void; ensure parse path #/
}
```

```veil/tests/template_strings.veil#L1-200
/# Template strings with embedded expressions #/

test template_string_basic {
    let who = "Veil";
    let msg = `Hello {who} {1 + 2}`;
    let _ = msg;
    return;
}

test template_string_nested_exprs {
    let a = 1 as i32;
    let b = 2 as i32;
    let s = `sum={a + b} both={a}{b}`;
    let _ = s;
    return;
}
```

```veil/tests/structs_and_enums.veil#L1-200
/# Struct and enum declarations (compilation targets, no instantiation) #/

pub struct Point {
    x: i32,
    y: i32,
}

pub(crate) enum Color {
    Red = 0,
    Green = 1,
    Blue = 2,
}

enum OptionI32 {
    Some(i32),
    Nothing,
}

test decls_compile {
    /# Declarations above should be sufficient for this test to compile #/
    return;
}
```

```veil/tests/match_expression.veil#L1-200
/# Match expression with literal patterns and wildcard #/

test match_on_int_minimal {
    let v: i32 = 2 as i32;
    match v {
        1 => { let _ = 10; }
        2 => { let _ = 20; }
        _ => { let _ = 0; }
    }
    return;
}

test match_with_guard_like_structure {
    let n: i32 = 5 as i32;
    match n {
        0 => { let _ = -1; }
        _ => { let _ = 1; }
    }
    return;
}
```

```veil/tests/imports_and_visibility.veil#L1-200
/# Imports (normalized :: vs /) and visibility #/

import std/io;
import std/prelude; /# redundant with auto-injection, but should still compile #/

pub fn public_function() -> void {
    return;
}

pub(crate) fn crate_function() -> void {
    return;
}

pub(super) fn super_function() -> void {
    return;
}

pub(in some::path) fn path_function() -> void {
    return;
}

test imports_and_visibility_compile {
    public_function();
    crate_function();
    super_function();
    path_function();
    return;
}
```

What’s covered (implemented subset)
- Lexical: identifiers, comments (/# ... #/), literals (int, float, string, bool, none), template strings
- Types: primitive aliases (i8/i16/i32/i64/u32/f32/f64/str/string/void/any), arrays [T] and [T; N], optional parsing path (T?) at grammar level
- Expressions: unary (!, +, -, ++/-- prefix and postfix), binary (+, -, *, /, //, %, ==, !=, <, >, <=, >=, &, |, &&, ||), casts (as), array indexing, field access shape, calls (f(...)), template strings, ranges (.., ..=, ..>, ..<), match, if, loop
- Statements: let/var, expr, return, while, for, loop, blocks
- Decls: fn, struct, enum (unit/tuple, discriminants), exports (pub, pub(crate), pub(super), pub(in path))
- Imports: import all and normalization (:: vs /), auto prelude injection

Notes
- Keep tests conservative to avoid unimplemented semantics (e.g., complex method dispatch, advanced trait objects, raw pointers).
- This suite validates end-to-end compilation in the current pipeline; functional assertions can be added once runtime execution is wired in the test runner.
