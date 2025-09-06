# Veil Language Specification: The Ultimate Guide (v2.0)

## Table of Contents

1. [Philosophy: Productive, Performant, Pragmatic Safety](#1-philosophy-productive-performant-pragmatic-safety)
2. [The Safety Core: Memory & Concurrency](#2-the-safety-core-memory--concurrency)
   - 2.1 [Memory Management: ARC + Generational References](#21-memory-management-arc--generational-references)
   - 2.2 [Task Orchestration: Structured Concurrency](#22-task-orchestration-structured-concurrency)
   - 2.3 [Shared State Synchronization: Mutex<T> & Atomics](#23-shared-state-synchronization-mutext--atomics)
   - 2.4 [Built-in Async Runtime](#24-built-in-async-runtime)
3. [Core Language Features](#3-core-language-features)
   - 3.1 [Metaprogramming: comptime](#31-metaprogramming-comptime)
     - 3.1.1 [comptime Limitations](#311-comptime-limitations)
   - 3.2 [Expressive & Safe Type System](#32-expressive--safe-type-system)
   - 3.3 [Integrated Data Handling](#33-integrated-data-handling)
   - 3.4 [Ergonomic Control Flow](#34-ergonomic-control-flow)
   - 3.5 [Error Handling & Result Types](#35-error-handling--result-types)
   - 3.6 [Trait System](#36-trait-system)
   - 3.7 [Module System & Visibility](#37-module-system--visibility)
   - 3.8 [Closures & First-Class Functions](#38-closures--first-class-functions)
   - 3.9 [Unsafe Operations](#39-unsafe-operations)
   - 3.10 [Advanced Type Features](#310-advanced-type-features)
     - 3.10.1 [Type Aliases and Newtype Pattern](#3101-type-aliases-and-newtype-pattern)
     - 3.10.2 [Associated Types and Type Families](#3102-associated-types-and-type-families)
     - 3.10.3 [Higher-Kinded Types and Generic Constraints](#3103-higher-kinded-types-and-generic-constraints)
     - 3.10.4 [Variance and Lifetime Parameters](#3104-variance-and-lifetime-parameters)
     - 3.10.5 [Type-Level Programming and Const Generics](#3105-type-level-programming-and-const-generics)
     - 3.10.6 [Advanced Trait Patterns](#3106-advanced-trait-patterns)
     - 3.10.7 [Type Erasure and Existential Types](#3107-type-erasure-and-existential-types)
     - 3.10.8 [Dependent Types and Refinement Types](#3108-dependent-types-and-refinement-types)
4. [The ve Toolchain & Ecosystem](#4-the-ve-toolchain--ecosystem)
   - 4.1 [Package Management](#41-package-management)
   - 4.2 [Build System & Targets](#42-build-system--targets)
   - 4.3 [Cross-Platform Compilation](#43-cross-platform-compilation)
   - 4.4 [Conditional Compilation](#44-conditional-compilation)
   - 4.5 [Attribute System](#45-attribute-system)
   - 4.6 [Linting & Formatting](#46-linting--formatting)
   - 4.7 [Self-Hosting Roadmap](#47-self-hosting-roadmap)
5. [Detailed Semantics](#5-detailed-semantics)
6. [Standard Library (std) Overview](#6-standard-library-std-overview)
   - 6.1 [std::sync::Mutex<T> API Reference](#61-stdsyncmutext-api-reference)
   - 6.2 [std::collections API Reference](#62-stdcollections-api-reference)
   - 6.3 [std::io API Reference](#63-stdio-api-reference)
   - 6.4 [std::fs API Reference](#64-stdfs-api-reference)
   - 6.5 [std::net API Reference](#65-stdnet-api-reference)
   - 6.6 [std::time API Reference](#66-stdtime-api-reference)
   - 6.7 [std::string API Reference](#67-stdstring-api-reference)
   - 6.8 [std::json API Reference](#68-stdjson-api-reference)
7. [Ownership & References](#7-ownership--references)
   - 7.1 [Ownership and ARC](#71-ownership-and-arc)
   - 7.2 [Borrowed References (&T)](#72-borrowed-references-t)
   - 7.3 [Weak References (weak T)](#73-weak-references-weak-t)
8. [Operators and Precedence](#8-operators-and-precedence)
9. [Comments and Documentation](#9-comments-and-documentation)
10. [Lexical Structure](#10-lexical-structure)
    - 10.1 [Character Set](#101-character-set)
    - 10.2 [Keywords](#102-keywords)
    - 10.3 [Identifiers](#103-identifiers)
    - 10.4 [Literals](#104-literals)
    - 10.5 [Operators and Punctuation](#105-operators-and-punctuation)
11. [Grammar and Syntax](#11-grammar-and-syntax)
    - 11.1 [Program Structure](#111-program-structure-ebnf)
    - 11.2 [Import/Export System](#112-importexport-system-ebnf)
    - 11.3 [Foreign Function Interface (FFI)](#113-foreign-function-interface-ffi-ebnf)
12. [Types and Values](#12-types-and-values)
    - 12.1 [Primitive Types](#121-primitive-types)
    - 12.2 [Composite Types](#122-composite-types)
    - 12.3 [User-Defined Types](#123-user-defined-types)
13. [Expressions](#13-expressions)
    - 13.1 [Literal Expressions](#131-literal-expressions)
    - 13.2 [Arithmetic](#132-arithmetic)
    - 13.3 [Comparison](#133-comparison)
    - 13.4 [Logical](#134-logical)
    - 13.5 [Assignment](#135-assignment)
    - 13.6 [Access](#136-access)
    - 13.7 [Ranges](#137-ranges)
    - 13.8 [Constructors](#138-constructors)
    - 13.9 [Cast](#139-cast)
    - 13.10 [Unary](#1310-unary)
    - 13.11 [Control Flow Expressions](#1311-control-flow-expressions)
14. [Statements](#14-statements)
    - 14.1 [Variable Declarations](#141-variable-declarations-ebnf)
    - 14.2 [Expression Statements](#142-expression-statements)
    - 14.3 [Control Flow Statements](#143-control-flow-statements)
    - 14.4 [Jump Statements](#144-jump-statements)
15. [Declarations](#15-declarations)
    - 15.1 [Functions](#151-functions-ebnf)
    - 15.2 [Structs](#152-structs-ebnf)
    - 15.3 [Enums](#153-enums-ebnf)
    - 15.4 [Impl Blocks](#154-impl-blocks-ebnf)
16. [Pattern Matching](#16-pattern-matching)
    - 16.1 [Basic Patterns](#161-basic-patterns)
    - 16.2 [Enum and Struct Patterns](#162-enum-and-struct-patterns)
    - 16.3 [Advanced Patterns](#163-advanced-patterns)
    - 16.4 [Pattern Matching in Different Contexts](#164-pattern-matching-in-different-contexts)
    - 16.5 [Exhaustiveness and Unreachable Patterns](#165-exhaustiveness-and-unreachable-patterns)
    - 16.6 [Pattern Matching Performance](#166-pattern-matching-performance)
17. [Examples](#17-examples)
    - 17.1 [Hello World](#171-hello-world)
    - 17.2 [Factorial](#172-factorial)
    - 17.3 [Generic Stack](#173-generic-stack)
    - 17.4 [Error Handling Example](#174-error-handling-example)
    - 17.5 [Async Web Server](#175-async-web-server)
    - 17.6 [Concurrent Data Processing Pipeline](#176-concurrent-data-processing-pipeline)
    - 17.7 [Generic Data Structures](#177-generic-data-structures)
    - 17.8 [FFI Usage](#178-ffi-usage)
    - 17.9 [Template Strings](#179-template-strings)
    - 17.10 [Range and Array](#1710-range-and-array)
18. [Performance Guidelines](#18-performance-guidelines)
    - 18.1 [Memory Management Optimization](#181-memory-management-optimization)
    - 18.2 [Async Runtime Tuning](#182-async-runtime-tuning)
    - 18.3 [Compile-Time Computation Strategies](#183-compile-time-computation-strategies)
    - 18.4 [Profiling and Benchmarking](#184-profiling-and-benchmarking)
    - 18.5 [Platform-Specific Optimizations](#185-platform-specific-optimizations)
19. [Interoperability Guide](#19-interoperability-guide)
    - 19.1 [C Foreign Function Interface (FFI)](#191-c-foreign-function-interface-ffi)
    - 19.2 [Advanced FFI Patterns](#192-advanced-ffi-patterns)
    - 19.3 [Memory Safety Across Language Boundaries](#193-memory-safety-across-language-boundaries)
    - 19.4 [Binding Generation Tools](#194-binding-generation-tools)
    - 19.5 [Platform-Specific Considerations](#195-platform-specific-considerations)
    - 19.6 [Best Practices for Interoperability](#196-best-practices-for-interoperability)
20. [Complete Grammar Reference](#20-complete-grammar-reference)
    - 20.1 [Operator Precedence](#201-operator-precedence-highest-to-lowest)
    - 20.2 [Statement Grammar](#202-statement-grammar)
    - 20.3 [Expression Grammar](#203-expression-grammar)
    - 20.4 [Type Grammar](#204-type-grammar)

---

## 1. Philosophy: Productive, Performant, Pragmatic Safety

Veil is a statically-typed, compiled language engineered for the modern era of software development. It is built to empower developers to create high-performance, safe, and maintainable software—from low-latency web services to cross-platform tooling—with less friction and cognitive overhead. Its design directly addresses the common pain points of contemporary programming, aiming to provide a development experience that is not only efficient but also enjoyable.

Our philosophy is the bedrock of every design decision, a set of principles that guide the language's evolution:

- **Simplicity Over Complexity**: Veil delivers the performance and safety of a systems language without the notoriously steep learning curve associated with concepts like manual memory management or complex lifetime analysis. The syntax is clean, expressive, and designed to be orthogonal, meaning features work together predictably. The memory model, in particular, is designed to be intuitive, allowing developers to focus on their application's logic rather than the intricacies of memory allocation.

- **Compiler as a Teacher**: The Veil compiler (vec) is your partner in development, not an obstacle. Its error messages are legendary: clear, actionable, and educational. Instead of a cryptic message, you get a detailed explanation of what went wrong, why it's a problem, and a concrete suggestion for a fix, often with a code snippet. It guides you to the correct solution, transforming errors from frustrating roadblocks into valuable learning opportunities about the language and safer coding practices.

- **Productivity by Default**: The entire developer experience is streamlined to maximize flow and minimize friction. A powerful, all-in-one toolchain (ve) handles project creation, dependency management, building, testing, and more with simple, consistent commands. Furthermore, the language itself automates common, error-prone tasks (like JSON serialization and deserialization), allowing
  developers to focus on unique business logic, not boilerplate.

- **Pragmatic Safety**: Veil provides robust, compile-time safety guarantees against entire classes of bugs—including null pointer dereferences, memory errors, data races, and concurrent task leaks. This is achieved through a system that is both powerful and easy to reason about, striking a pragmatic balance. It avoids the unrestricted risk of C/C++ and the runtime overhead of a garbage collector, while being significantly more approachable than Rust's borrow checker.

- **Cross-Platform Ahead-of-Time Compilation**: Veil uses ahead-of-time (AOT) compilation to generate optimized native code for any target platform. The compiler performs extensive analysis and optimization during compilation, producing efficient executables without runtime overhead. Cross-compilation is seamlessly supported—you can build for Windows, macOS, Linux, and other platforms from any host system. This approach eliminates the need for virtual machines or interpreters while ensuring consistent performance across all supported architectures.

- **Self-Hosting Vision**: Veil is designed to be sufficiently powerful and ergonomic for large-scale systems development. To prove this, the Veil compiler itself will be completely rewritten in Veil before reaching version 1.0. Currently implemented in another language, this self-hosting milestone demonstrates that Veil is not just suitable for application development, but capable of building the most demanding systems software—including compilers, operating systems, and runtime environments.

## 2. The Safety Core: Memory & Concurrency

Veil's most significant innovation lies in its unified approach to safety, which addresses the two primary challenges of modern systems programming: memory management and concurrency. These systems are designed to work in concert, providing a holistic safety net.

### 2.1 Memory Management: ARC + Generational References

Veil achieves deterministic, garbage-collector-free memory safety through a novel hybrid system that is far simpler to use than a borrow checker.

- **Automatic Reference Counting (ARC)**: The compiler automatically injects retain and release operations to count references to heap-allocated objects. These operations are atomic by default to ensure thread safety, though non-atomic versions can be used in single-threaded contexts for a performance boost. When an object's reference count drops to zero, its memory is freed instantly and predictably. This deterministic nature is crucial for resource management, as destructors run at a known time.

- **Generational References (Compile-Time Cycle Prevention)**: To defeat ARC's classic nemesis—reference cycles—the compiler performs a static analysis called Generational Referencing. It assigns a "generation" number to each scope; nested scopes receive higher numbers. The compiler then enforces a simple rule: a reference cannot be stored in an object belonging to an older (long-lived) generation if it points to an object in a younger (short-lived) generation. This makes the most common cycle patterns a compile-time error.

```veil
/# Example of a compile-time cycle rejection
const long_lived_obj = new MyObject(); /# Generation 1

spawn {
    const short_lived_obj = new MyObject(); /# Generation 2
    /# ERROR: Cannot assign a reference from a younger generation (2)
    /# to an object in an older generation (1).
    long_lived_obj.child = short_lived_obj;
}
```

- **weak References**: For the rare cases where a cycle is intentional and necessary (e.g., a cache implementation or a parent node pointing to a child that also points back), you can use a weak reference. A weak reference does not increase the reference count and is ignored by the generational analysis. Accessing a weak reference is safe, as it yields an Option<T>, which will be none if the underlying object has been deallocated.

This system provides the performance and determinism of manual memory management with the ease-of-use of a GC language.

### 2.2 Task Orchestration: Structured Concurrency

Veil makes asynchronous programming sane and safe by building structured concurrency and a high-performance async runtime directly into the language. This integrated approach ensures optimal performance and eliminates the need for external async libraries. The runtime is responsible for the lifetime and orchestration of concurrent tasks, ensuring no task outlives the scope it was created in.

- **async fn**: Marks a function as asynchronous, allowing it to be run concurrently on Veil's built-in runtime. Its return type is a Future<T>.

- **await**: Pauses the execution of an async function until another async operation (a Future) completes, yielding control to Veil's integrated scheduler.

- **spawn { ... }**: Creates a concurrency "nursery" managed by the built-in runtime. This block will not exit until all concurrent tasks started within it have completed, either successfully or with an error. This simple guarantee eliminates leaked tasks (like goroutines) and makes concurrent code easy to reason about. If any task within the spawn block fails with an error, all other tasks within that same block are immediately signaled for cancellation.

```veil
/# Structured Concurrency guarantees all tasks complete or are cancelled.
async fn run_jobs() -> Result<ProcessedData, Error> {
    spawn {
        const data_future = fetch_from_db(); /# Task 1
        const api_future = call_external_api(); /# Task 2

        /# `await` retrieves the result, propagating errors with `?`
        const data = await data_future?;
        const api_data = await api_future?;

        /# This code only runs if both futures succeed.
        return Ok(process(data, api_data));
    }
    /# This point is unreachable until both futures resolve.
    /# If one fails, the other is cancelled and the error is returned.
}
```

### 2.3 Shared State Synchronization: Mutex<T> & Atomics

While structured concurrency manages what tasks run and when they finish, it does not prevent data races if multiple tasks try to modify the same piece of memory. Veil solves this with classic synchronization primitives built into the runtime, responsible for safe access to shared data.

- **Mutex<T>**: The primary tool for managing shared state, integrated with Veil's async runtime. A Mutex (Mutual Exclusion) ensures that only one task can access the data it protects at any given time. To access the data, a task must await a lock. The lock is held via a MutexGuard object, which automatically releases the lock when it goes out of scope (RAII-style). This robust pattern prevents deadlocks from forgotten unlock calls.

- **RwLock<T>**: A Read-Write lock that allows for either multiple concurrent readers or a single exclusive writer. It is a performance optimization for data that is read far more often than it is modified. For example, a shared application configuration that is read on every request but only written to occasionally by an administrator.

- **Atomic<T>**: Provides lock-free atomic operations (e.g., fetch_add, compare_exchange) on primitive types. Atomics are for experts in performance-critical situations, such as implementing high-performance metrics counters or building other synchronization primitives, and require a deep understanding of memory ordering to use correctly.

```veil
import std::sync::Mutex;

/# This example is 100% safe from data races and runs on Veil's built-in async runtime.
async fn run_shared_counter() {
    const counter = Mutex::new(0); /# Wrap the shared state in a Mutex.

    spawn {
        /# Spawn 1,000 tasks to increment the counter.
        for _ in 0..1000 {
            increment(&counter);
        }
    }

    const final_value = await counter.lock();
    print("Final count: {final_value}"); /# Guaranteed to print "1000".
}

async fn increment(m: &Mutex<int>) {
    /# `await` the lock. This pauses the task until it gains exclusive access.
    const guard = await m.lock();

    /# The guard object gives us safe, mutable access to the data.
    *guard += 1;

} /# The `guard` goes out of scope here, and the mutex is automatically unlocked.
```

### 2.4 Built-in Async Runtime

Veil includes a high-performance async runtime built directly into the language, eliminating the need for external async libraries and providing optimal integration with structured concurrency.

#### 2.4.1 Runtime Architecture

The Veil async runtime is designed for maximum performance and seamless integration:

```veil
/# The runtime is automatically initialized - no setup required
async fn main() {
    /# Your async code runs immediately on the built-in runtime
    const result = await fetch_data();
    print("Data: {result}");
}

/# Multiple concurrent tasks are efficiently scheduled
async fn concurrent_example() {
    spawn {
        const task1 = fetch_user_data();
        const task2 = fetch_product_data();
        const task3 = fetch_analytics_data();

        /# All tasks run concurrently on the work-stealing scheduler
        const (users, products, analytics) = await (task1, task2, task3);

        process_combined_data(users, products, analytics);
    }
}
```

#### 2.4.2 Work-Stealing Scheduler

The runtime uses a sophisticated work-stealing thread pool optimized for async workloads:

- **Thread Pool**: Automatically sized to match available CPU cores
- **Work Stealing**: Idle threads steal work from busy threads for optimal load balancing
- **NUMA Awareness**: Thread and memory allocation respects NUMA topology
- **Adaptive**: Dynamically adjusts to workload characteristics

```veil
/# Runtime configuration (optional - sane defaults provided)
async fn configure_runtime() {
    /# Access runtime configuration if needed
    const config = runtime::Config {
        worker_threads: 8,
        max_blocking_threads: 512,
        thread_stack_size: 2 * 1024 * 1024, /# 2MB
        enable_io_uring: true, /# Linux
        enable_completion_ports: true, /# Windows
    };

    runtime::set_config(config);
}
```

#### 2.4.3 I/O Integration

The runtime provides zero-copy, high-performance I/O integrated with the async system:

```veil
import std::net;
import std::fs;

async fn io_examples() {
    /# Network I/O - automatically uses epoll/kqueue/io_uring
    const listener = await net::TcpListener::bind("127.0.0.1:8080")?;

    spawn {
        loop {
            const (stream, addr) = await listener.accept()?;
            spawn {
                handle_connection(stream).await;
            };
        }
    }

    /# File I/O - non-blocking and efficient
    const data = await fs::read_to_string("large_file.txt")?;
    const processed = process_data(data);
    await fs::write("output.txt", processed)?;
}

async fn handle_connection(stream: net::TcpStream) {
    const mut buffer = [0u8; 4096];

    loop {
        match await stream.read(&buffer) {
            Ok(0) => break, /# Connection closed
            Ok(n) => {
                const response = generate_response(&buffer[..n]);
                await stream.write_all(response.as_bytes())?;
            },
            Err(e) => {
                print("Connection error: {e}");
                break;
            }
        }
    }
}
```

#### 2.4.4 Timer and Sleep Integration

High-precision timers are built into the runtime:

```veil
import std::time;

async fn timer_examples() {
    /# High-precision sleep
    await time::sleep(time::Duration::from_millis(100));

    /# Timeout for operations
    const result = await time::timeout(
        time::Duration::from_secs(5),
        slow_operation()
    );

    match result {
        Ok(value) => print("Completed: {value}"),
        Err(time::TimeoutError) => print("Operation timed out"),
    }

    /# Periodic timers
    const mut interval = time::interval(time::Duration::from_secs(1));
    spawn {
        loop {
            await interval.tick();
            print("Heartbeat");
        }
    }
}
```

#### 2.4.5 Channel-based Communication

Built-in async channels for task communication:

```veil
import std::sync::channel;

async fn channel_example() {
    const (tx, rx) = channel::unbounded::<string>();

    /# Producer task
    spawn {
        for i in 0..10 {
            await tx.send(`Message {i}`);
            await time::sleep(time::Duration::from_millis(100));
        }
        tx.close();
    };

    /# Consumer task
    spawn {
        while const msg = await rx.recv() {
            match msg {
                Some(data) => print("Received: {data}"),
                None => break, /# Channel closed
            }
        }
    };
}

/# Multi-producer, single-consumer
async fn mpsc_example() {
    const (tx, rx) = channel::bounded::<WorkItem>(100);

    /# Multiple producers
    for worker_id in 0..4 {
        const sender = tx.clone();
        spawn {
            for task_id in 0..25 {
                const work = WorkItem::new(worker_id, task_id);
                await sender.send(work);
            }
        };
    }

    /# Single consumer
    spawn {
        var completed = 0;
        while const item = await rx.recv() {
            match item {
                Some(work) => {
                    process_work_item(work);
                    completed += 1;
                },
                None => break,
            }
        }
        print("Processed {completed} items");
    };
}
```

#### 2.4.6 Runtime Introspection

The runtime provides introspection capabilities for monitoring and debugging:

```veil
import std::runtime;

async fn runtime_monitoring() {
    const stats = runtime::stats();

    print("Active tasks: {stats.active_tasks}");
    print("Queued tasks: {stats.queued_tasks}");
    print("Worker threads: {stats.worker_threads}");
    print("Thread utilization: {stats.thread_utilization}%");
    print("I/O events/sec: {stats.io_events_per_sec}");

    /# Memory statistics
    print("Async stack memory: {stats.async_stack_memory} bytes");
    print("Channel buffer memory: {stats.channel_buffer_memory} bytes");

    /# Performance counters
    print("Context switches: {stats.context_switches}");
    print("Work steals: {stats.work_steals}");
    print("I/O operations: {stats.io_operations}");
}

/# Task-local storage
async fn task_local_example() {
    runtime::task_local! {
        static REQUEST_ID: string = "unknown";
    }

    REQUEST_ID.with(|id| {
        *id = generate_request_id();
    });

    await process_request(); /# Can access REQUEST_ID in any spawned task
}
```

#### 2.4.7 Zero-Cost Abstractions

The runtime is designed for zero-cost abstractions where async overhead is minimal:

- **Future Compilation**: Futures compile to efficient state machines
- **Inlining**: Small async functions are often inlined completely
- **Stack Optimization**: Async stacks are optimally sized and reused
- **Memory Pool**: Built-in memory pools for common async allocations

```veil
/# This async function has near-zero overhead
async fn lightweight_async(x: i32) -> i32 {
    x * 2 /# Compiles to a simple state machine
}

/# Complex async operations are still efficient
async fn complex_async() -> Result<Data, Error> {
    const step1 = await fetch_data()?;
    const step2 = await transform_data(step1)?;
    const step3 = await store_data(step2)?;
    Ok(step3)
} /# Compiles to optimized state machine with minimal allocations
```

The built-in runtime ensures that Veil async code achieves optimal performance without the complexity of choosing and configuring external async libraries. The tight integration with structured concurrency provides both safety and performance guarantees that external runtimes cannot match.

## 3. Core Language Features

### 3.1 Metaprogramming: comptime

Veil eschews complex, syntax-rewriting macro systems in favor of compile-time code execution. Any code within a comptime block is executed by the compiler. This enables powerful metaprogramming that feels like writing normal code, with the full power of the Veil type system at your disposal.

- **Static Assertions**: `comptime { assert(sizeof(T) < 256, "Type T is too large for cache line"); }`

- **Code Generation**: Generate functions, lookup tables, and even data structures at compile time. This is perfect for creating highly optimized state machines or parsers from a declarative definition.

- **Pre-computation**: Perform expensive calculations at compile time so they cost nothing at runtime. For example, pre-calculating a table of trigonometric values.

#### 3.1.1 `comptime` Limitations

The `comptime` environment is sandboxed and cannot perform I/O. Execution is also limited by a maximum number of computational steps to prevent the compiler from hanging on infinite loops.

### 3.2 Expressive & Safe Type System

- **Primitives**: Complete integer range from i8..i128 and u8..u128, architecture-dependent isize/usize, IEEE 754 floating-point f32/f64, bool, UTF-8 str, Unicode ch.

- **Increment/Decrement Operators**: Veil supports both prefix and postfix increment (++) and decrement (--) operators for integers only (i8..i64, u8..u64). The prefix version (++x, --x) modifies the variable and returns the new value, while the postfix version (x++, x--) returns the original value and then modifies the variable. These operators are syntactic sugar for x += 1 and x -= 1 respectively. Applying ++/-- to non-integer types is a compile-time error, and these operators require a var binding.

```veil
var mut counter = 0;
var a = ++counter;  /# counter becomes 1, a is 1
var b = counter++;  /# b is 1, counter becomes 2
var c = --counter;  /# counter becomes 1, c is 1
var d = counter--;  /# d is 1, counter becomes 0
```

- **Immutability by Default**: var creates immutable bindings by default, which encourages safer programming. const is for compile-time constants. var mut is used for explicit mutability when state changes are necessary, making these locations easy to spot and reason about.

- **Union Types (|)**: string | int can hold either type. The compiler enforces that you handle all cases, typically with a match statement, eliminating a whole category of runtime type errors.

- **Intersection Types (&)**: Loggable & Serializable represents a type that conforms to both traits (interfaces). This allows for powerful, flexible abstractions without complex inheritance hierarchies.

- **Optionals (?)**: T? is syntactic sugar for the Option<T> enum (enum Option<T> { Some(T), None }). The language literal is none; you must handle none before use, completely eliminating null pointer errors, one of the most common sources
  of crashes in other languages.

- **Arrays**: [T; N] for stack-allocated, fixed-size arrays where the size is known at compile time, offering maximum performance. [T] for heap-allocated dynamic arrays (vectors) that can grow as needed.

### 3.3 Integrated Data Handling

Veil drastically reduces boilerplate by building data format handling into the standard library. The process is seamless, type-safe, and highly performant.

```veil
/# Define a struct, and it can be used for serialization.
/# The `[Serializable]` attribute automatically generates the necessary code.
[Serializable]
struct User { name: str, id: int }

/# One-line, type-safe parsing. The `?` operator propagates any
/# parsing errors (e.g., missing field, wrong type).
fn parse_user(raw_json_string: str) -> Result<User, json::Error> {
    const user = json::parse<User>(raw_json_string)?;
    return Ok(user);
}

/# One-line serialization.
const user = User { name: "Alice", id: 42 };
const json_string = json::stringify(user); /# -> `{"name": "Alice", "id": 42}`
```

### 3.4 Ergonomic Control Flow

- **Pipeline Operator (|>)**: data |> clean |> process is equivalent to process(clean(data)), making data transformation pipelines highly readable and easy to compose, encouraging a functional style.

- **Advanced Pattern Matching**: match is an exhaustive expression that is a cornerstone of the language. It supports binding variables, conditional guards, matching on ranges, and destructuring complex types like structs and tuples, making complex logic flat, clear, and safe.

### 3.5 Error Handling & Result Types

Veil provides a comprehensive error handling system built around the Result<T, E> type and explicit error propagation, designed to make error handling both safe and ergonomic.

#### 3.5.1 Result Type and Basic Error Handling

```veil
/# Result type is built into the language
enum Result<T, E> {
    Ok(T),
    Err(E),
}

/# The ? operator for error propagation
fn parse_number(s: str) -> Result<i32, ParseError> {
    const trimmed = s.trim();
    if trimmed.is_empty() {
        return Err(ParseError::Empty);
    }

    /# ? automatically converts and propagates errors
    const num = trimmed.parse::<i32>()?;
    Ok(num)
}

/# Custom error types
enum ParseError {
    Empty,
    InvalidFormat(str),
    OutOfRange,
}

impl Error for ParseError {
    fn message(&self) -> str {
        match self {
            ParseError::Empty => "Input string is empty".to_string(),
            ParseError::InvalidFormat(s) => format!("Invalid format: {}", s),
            ParseError::OutOfRange => "Number is out of range".to_string(),
        }
    }
}

/# Error trait for common functionality
trait Error: Display + Debug + Send + Sync {
    fn message(&self) -> str;
    fn source(&self) -> Option<&dyn Error> { None }
    fn backtrace(&self) -> Option<&Backtrace> { None }
}

/# Result methods for error handling
impl<T, E> Result<T, E> {
    fn is_ok(&self) -> bool;
    fn is_err(&self) -> bool;
    fn ok(self) -> Option<T>;
    fn err(self) -> Option<E>;
    fn unwrap(self) -> T where E: Debug;  /# Panics on Err
    fn unwrap_or(self, default: T) -> T;
    fn unwrap_or_else<F>(self, f: F) -> T where F: FnOnce(E) -> T;
    fn expect(self, msg: &str) -> T where E: Debug;  /# Panics with custom message
    fn map<U, F>(self, f: F) -> Result<U, E> where F: FnOnce(T) -> U;
    fn map_err<F, O>(self, f: F) -> Result<T, O> where F: FnOnce(E) -> O;
    fn and_then<U, F>(self, f: F) -> Result<U, E> where F: FnOnce(T) -> Result<U, E>;
}
```

#### 3.5.2 Error Chaining and Context

```veil
/# Error chaining for providing context
trait Context<T, E> {
    fn context(self, msg: &str) -> Result<T, ContextError<E>>;
    fn with_context<F>(self, f: F) -> Result<T, ContextError<E>>
    where
        F: FnOnce() -> string;
}

impl<T, E> Context<T, E> for Result<T, E>
where
    E: Error + Send + Sync + 'static,
{
    fn context(self, msg: &str) -> Result<T, ContextError<E>> {
        self.map_err(|err| ContextError::new(msg, err))
    }

    fn with_context<F>(self, f: F) -> Result<T, ContextError<E>>
    where
        F: FnOnce() -> string,
    {
        self.map_err(|err| ContextError::new(&f(), err))
    }
}

/# Usage examples
fn read_config_file() -> Result<Config, ConfigError> {
    const content = std::fs::read_to_string("config.toml")
        .context("Failed to read configuration file")?;

    const config = toml::parse(&content)
        .context("Failed to parse TOML configuration")?;

    Ok(config)
}

/# Error with context and source chain
struct ContextError<E> {
    message: string,
    source: E,
}

impl<E: Error> Error for ContextError<E> {
    fn message(&self) -> string {
        self.message.clone()
    }

    fn source(&self) -> Option<&dyn Error> {
        Some(&self.source)
    }
}
```

#### 3.5.3 Application-Level Error Types

```veil
/# Application error enum combining multiple error types
enum AppError {
    Io(io::Error),
    Parse(ParseError),
    Network(NetworkError),
    Database(DatabaseError),
    Validation(ValidationError),
    Custom(str),
}

impl Error for AppError {
    fn message(&self) -> str {
        match self {
            AppError::Io(err) => format!("I/O error: {}", err.message()),
            AppError::Parse(err) => format!("Parse error: {}", err.message()),
            AppError::Network(err) => format!("Network error: {}", err.message()),
            AppError::Database(err) => format!("Database error: {}", err.message()),
            AppError::Validation(err) => format!("Validation error: {}", err.message()),
            AppError::Custom(msg) => msg.clone(),
        }
    }

    fn source(&self) -> Option<&dyn Error> {
        match self {
            AppError::Io(err) => Some(err),
            AppError::Parse(err) => Some(err),
            AppError::Network(err) => Some(err),
            AppError::Database(err) => Some(err),
            AppError::Validation(err) => Some(err),
            AppError::Custom(_) => None,
        }
    }
}

/# Automatic conversion from specific errors
impl From<io::Error> for AppError {
    fn from(err: io::Error) -> Self {
        AppError::Io(err)
    }
}

impl From<ParseError> for AppError {
    fn from(err: ParseError) -> Self {
        AppError::Parse(err)
    }
}

/# Application result type alias
type AppResult<T> = Result<T, AppError>;
```

#### 3.5.4 Error Handling Patterns

```veil
/# Pattern 1: Early return with ?
fn process_data(input: &str) -> AppResult<ProcessedData> {
    const parsed = parse_input(input)?;
    const validated = validate_data(&parsed)?;
    const processed = transform_data(validated)?;
    const stored = store_data(&processed)?;
    Ok(stored)
}

/# Pattern 2: Collecting multiple errors
fn validate_user(user: &User) -> Result<(), [ValidationError]> {
    var errors = [];

    if user.name.is_empty() {
        errors.push(ValidationError::EmptyName);
    }

    if !user.email.contains('@') {
        errors.push(ValidationError::InvalidEmail);
    }

    if user.age < 18 {
        errors.push(ValidationError::TooYoung);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/# Pattern 3: Error recovery and fallbacks
fn fetch_with_fallback(primary_url: &str, fallback_url: &str) -> AppResult<Data> {
    match fetch_data(primary_url) {
        Ok(data) => Ok(data),
        Err(NetworkError::Timeout) => {
            /# Retry with fallback for timeout errors
            fetch_data(fallback_url)
        },
        Err(NetworkError::NotFound) => {
            /# Return default data for 404 errors
            Ok(Data::default())
        },
        Err(err) => Err(err.into()), /# Propagate other errors
    }
}

/# Pattern 4: Async error handling
async fn async_operation() -> AppResult<ProcessedData> {
    const data = fetch_remote_data().await?;
    const processed = async_process(data).await?;
    const stored = async_store(processed).await?;
    Ok(stored)
}

/# Pattern 5: Error handling in loops
fn process_batch(items: &[Item]) -> Result<[ProcessedItem], BatchError> {
    var results = [];
    var errors = [];

    for (index, item) in items.iter().enumerate() {
        match process_item(item) {
            Ok(processed) => results.push(processed),
            Err(err) => errors.push((index, err)),
        }
    }

    if errors.is_empty() {
        Ok(results)
    } else {
        Err(BatchError::PartialFailure { results, errors })
    }
}
```

#### 3.5.5 Panic Handling

**Panics are for unrecoverable errors and programming bugs:**

```veil
/# Panic functions
fn panic(msg: &str) -> !;  /# Never returns
fn assert(condition: bool, msg: &str);
fn debug_assert(condition: bool, msg: &str);  /# Only in debug builds
fn unreachable() -> !;

/# Panic with formatting
macro panic! {
    /# panic!("Invalid state: {}", state);
}

/# Panic hooks for custom handling
fn set_panic_hook(hook: Box<dyn Fn(&PanicInfo) + Send + Sync>) {
    /# Custom panic behavior - logging, cleanup, etc.
}

struct PanicInfo {
    message: &str,
    location: Location,
    backtrace: Backtrace,
}

/# Catching panics (use sparingly)
fn catch_unwind<F, R>(f: F) -> Result<R, Box<dyn Any + Send>>
where
    F: FnOnce() -> R + UnwindSafe,
{
    /# Implementation catches panics and returns them as errors
}

/# Result unwrapping methods (panic on Err)
const value = result.unwrap();  /# Panics if Err with generic message
const value = result.expect("Expected valid configuration");  /# Custom panic message
const value = result.unwrap_or_default();  /# Use default value if Err
const value = result.unwrap_or_else(|| compute_fallback());  /# Compute fallback if Err

/# When to use panic vs Result:
/# - Use Result for expected error conditions
/# - Use panic for programming errors and invariant violations
/# - Use assert for debugging and contract validation
```

#### 3.5.6 Error Handling Best Practices

`````veil
/# 1. Use specific error types
enum DatabaseError {
    ConnectionFailed(str),
    QueryTimeout,
    ConstraintViolation { table: str, constraint: str },
    SerializationFailure,
}

/# 2. Provide helpful error messages
impl Error for DatabaseError {
    fn message(&self) -> str {
        match self {
            DatabaseError::ConnectionFailed(reason) =>
                format!("Failed to connect to database: {}", reason),
            DatabaseError::QueryTimeout =>
                "Database query timed out after 30 seconds".to_string(),
            DatabaseError::ConstraintViolation { table, constraint } =>
                format!("Constraint '{}' violated in table '{}'", constraint, table),
            DatabaseError::SerializationFailure =>
                "Transaction failed due to concurrent modification".to_string(),
        }
    }
}

/# 3. Use Result type aliases for common patterns
type DatabaseResult<T> = Result<T, DatabaseError>;
````Backtrace:\n{}", backtrace);
    }
}
.backtrace() {
        trace!("
    if let Some(backtrace) = error
    }
d by ({}): {}", depth, source.message());
        current_error = source; += 1;
        warn!("  Cause {
        depth = 0;

    while let Some(source) = current_error.source()    var depth &dyn Error = error;
: current_error{}' failed: {}", operation, error.message());

    var input: {:?}", input))?;

    Ok(processed)
}

/# 6. Error logging and monitoring
fn log_error<E: Error>(error: &E, operation: &str) {
    error!("Operation 'validated)
        .with_context(|| format!("Processing failed for
        .context("Input validation failed")?;

    const processed = process_data(")?;

    const validated = validate_input(&input) = read_input()
        .context("Failed to read input data> {
    const input_operation() -> AppResult<Dataacing
fn complexFailed(err.to_string()),
        }
    }
}

/# 5. Use context for error tr
            _ => DatabaseError::Connectionrap_or("unknown").to_string(),
                }
            },rap_or("unknown").to_string(),
                    constraint: db_err.constraint().unwViolation {
                    table: db_err.table().unw_violation() => {
                DatabaseError::Constraint db_err.is_constraint_err) if::RowNotFound => DatabaseError::QueryTimeout,
            sqlx::Error::Database(db From<sqlx::Error> for DatabaseError {
    fn from(err: sqlx::Error) -> Self {
        match err {
            sqlx::Errortype ApiResult<T> = Result<T, ApiError>;

/# 4. Implement error conversions judiciously
impl

### 3.6 Trait System

Traits define shared behavior and enable powerful generic programming patterns.

````veil
/# Basic trait definition
trait Display {
    fn fmt(self) -> string;

    /# Default implementation
    fn show(self) {
        print(self.fmt());
    }
}

/# Trait implementation
impl Display for User {
    fn fmt(self) -> string {
        return `User(name: {self.name}, id: {self.id})`;
    }
}

/# Generic constraints
fn print_item<T>(item: T) where T: Display {
    item.show();
}

/# Multiple trait bounds
fn serialize<T>(item: T) -> string
where T: Display & Serializable {
    /# Can use both Display and Serializable methods
    return item.fmt() + "|" + item.to_json();
}

/# Associated types
trait Iterator {
    type Item;

    fn next(self) -> Option<Self::Item>;

    fn collect<C>(self) -> C where C: FromIterator<Self::Item> {
        /# Default implementation using other methods
    }
}

/# Trait objects for dynamic dispatch
fn process_displays(items: [&Display]) {
    for item in items {
        item.show();  /# Dynamic dispatch
    }
}

/# Operator overloading through traits
trait Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

impl Add for Point {
    type Output = Point;
    fn add(self, other: Point) -> Point {
        Point { x: self.x + other.x, y: self.y + other.y }
    }
}
`````

### 3.7 Module System & Visibility

Veil provides a hierarchical module system with fine-grained visibility control that enables building large, maintainable codebases with clear API boundaries.

#### 3.7.1 Module Declaration and Structure

```veil
/# File-based modules: each .veil file is a module
/# Directory-based modules: directories with mod.veil files

/# lib.veil (crate root)
pub mod network;     /# References network.veil or network/mod.veil
pub mod database;    /# References database.veil or database/mod.veil
pub mod utils;       /# References utils.veil or utils/mod.veil

/# Inline module declaration
mod internal {
    pub fn helper() { }

    mod deeply_nested {
        pub fn specialized_function() { }
    }
}

/# Module paths follow filesystem structure
/# network/tcp.veil becomes network::tcp
/# network/protocols/http.veil becomes network::protocols::http
```

#### 3.7.2 Visibility Modifiers

Veil's visibility system provides precise control over API boundaries:

```veil
/# Public visibility - accessible from anywhere
pub struct PublicStruct {
    pub field: i32,           /# Public field
    pub(crate) internal: i32, /# Visible within current crate
    pub(super) parent: i32,   /# Visible to parent module only
    pub(in network) scoped: i32, /# Visible within 'network' module tree
    private_field: i32,       /# Private (default) - module-local only
}

pub fn public_function() { }          /# Public API
pub(crate) fn crate_function() { }    /# Internal to crate
pub(super) fn parent_function() { }   /# Parent module only
pub(in network) fn network_function() { } /# Scoped visibility
fn private_function() { }             /# Module-local

/# Visibility applies to all items
pub enum Status { Active, Inactive }
pub trait Processor { fn process(&self); }
pub type Result = std::result::Result<(), Error>;
```

#### 3.7.3 Import System

```veil
/# Basic imports
use std::collections::HashMap;
use network::tcp::Connection;

/# Import with aliasing
use std::collections::HashMap as Map;
use very::long::module::name as short;
use network::tcp::{Connection as TcpConn, Error as TcpError};

/# Multiple imports from same module
use std::collections::{HashMap, HashSet, BTreeMap};

/# Glob imports (use sparingly in production code)
use std::prelude::*;
use math::constants::*; /# Import all constants

/# Nested imports for clean organization
use network::{
    tcp::{Connection, Listener},
    udp::{Socket, Packet},
    protocols::{http, websocket}
};
```

#### 3.7.4 Re-exports and Public APIs

```veil
/# Re-exports create clean public APIs
pub use internal::tcp::Connection;      /# Re-export as public
pub use internal::errors::NetworkError; /# Expose internal error type

/# Selective re-exports
pub use database::{
    Connection as DbConnection,  /# Rename to avoid conflicts
    Transaction,                 /# Keep original name
    /# Note: Other database items remain private
};

/# Module-level re-exports in mod.veil
/# network/mod.veil
pub mod tcp;
pub mod udp;
pub use tcp::{Connection, Listener};  /# Convenience re-exports
pub use udp::Socket;

/# This allows users to write either:
/# use mylib::network::tcp::Connection;  /# Direct path
/# use mylib::network::Connection;       /# Via re-export
```

#### 3.7.5 Conditional Compilation and Feature Gates

```veil
/# Feature-gated imports
#[cfg(feature = "networking")]
use std::net;

#[cfg(feature = "async")]
use std::sync::channel;

/# Platform-specific modules
#[cfg(target_os = "linux")]
mod linux_specific;

#[cfg(target_os = "windows")]
mod windows_specific;

/# Conditional re-exports
#[cfg(feature = "serde")]
pub use serde_integration::*;

/# Complex conditions
#[cfg(all(feature = "networking", not(target_arch = "wasm32")))]
mod native_networking;
```

#### 3.7.6 Module Organization Best Practices

```veil
/# Recommended crate structure:
/# src/
/#   lib.veil           /# Crate root
/#   error.veil         /# Error types
/#   prelude.veil       /# Common imports
/#   config/
/#     mod.veil         /# Configuration module root
/#     parser.veil      /# Config parsing
/#     validation.veil  /# Config validation
/#   network/
/#     mod.veil         /# Network module root
/#     tcp.veil         /# TCP implementation
/#     udp.veil         /# UDP implementation
/#     protocols/
/#       mod.veil       /# Protocols submodule
/#       http.veil      /# HTTP implementation
/#       websocket.veil /# WebSocket implementation

/# lib.veil - clean public API
pub mod config;
pub mod network;
pub mod error;
pub mod prelude;

/# Re-export commonly used types
pub use error::{Error, Result};
pub use config::Config;

/# Prelude module for convenient imports
/# prelude.veil
pub use crate::{Error, Result, Config};
pub use crate::network::{Connection, Listener};
```

#### 3.7.7 Module Resolution Rules

1. **Absolute paths**: Start from crate root

   ```veil
   use crate::network::tcp::Connection;  /# From crate root
   ```

2. **Relative paths**: Relative to current module

   ```veil
   use super::utils;        /# Parent module's utils
   use self::internal;      /# Current module's internal submodule
   ```

3. **External crates**: Reference dependencies

   ```veil
   use tokio::net::TcpStream;     /# External crate
   use serde::{Serialize, Deserialize}; /# External with multiple items
   ```

#### 3.7.8 Advanced Module Patterns

```veil
/# Module facade pattern
pub mod api {
    /# Clean, stable public API
    pub use crate::internal::core::{Process, Result};
    pub use crate::internal::utils::Helper;

    /# Hide implementation details
    use crate::internal::complex_impl;

    pub fn high_level_operation() -> Result<()> {
        complex_impl::do_complex_work()
    }
}

/# Plugin architecture with modules
pub trait Plugin {
    fn name(&self) -> &str;
    fn execute(&self) -> Result<(), PluginError>;
}

/# Plugins as modules
mod plugins {
    pub mod auth;
    pub mod logging;
    pub mod metrics;

    pub use self::{auth::AuthPlugin, logging::LogPlugin, metrics::MetricsPlugin};
}

/# Type aliasing across modules
pub mod types {
    use std::collections::HashMap;

    pub type UserId = u64;
    pub type UserMap = HashMap<UserId, User>;
    pub type Result<T> = std::result::Result<T, crate::Error>;
}
```

### 3.8 Closures & First-Class Functions

Veil supports closures with explicit capture semantics and async closures.

```veil
/# Closure syntax
const add = |a: i32, b: i32| -> i32 { a + b };
const simple = |x| x * 2;  /# Type inference

/# Capture modes
fn closure_examples() {
    var counter = 0;
    const data = [1, 2, 3];

    /# Move capture
    const moved = move |x| {
        /# Takes ownership of captured variables
        data.len() + x  /# data is moved into closure
    };

    /# Reference capture (default)
    const borrowed = |x| {
        counter += 1;  /# Borrows counter mutably
        x + counter
    };

    /# Explicit capture list
    const explicit = [counter, &data] |x| {
        counter + data.len() + x
    };
}

/# Async closures - run on Veil's built-in async runtime
const async_processor = async |data: Vec<Data>| -> Result<Processed, Error> {
    const results = [];
    for item in data {
        const processed = await process_item(item)?;
        results.push(processed);
    }
    Ok(results)
};

/# Higher-order functions
fn map<T, U>(slice: [T], f: fn(T) -> U) -> [U] {
    const result = [];
    for item in slice {
        result.push(f(item));
    }
    result
}

/# Function pointers vs closures
type FnPtr = fn(i32) -> i32;        /# Function pointer
type Closure = |i32| -> i32;        /# Closure type
```

### 3.9 Unsafe Operations

Unsafe blocks allow low-level operations while maintaining safety boundaries. Veil's approach to unsafe code is designed to minimize risk while enabling necessary low-level operations for FFI, performance-critical code, and systems programming.

#### 3.9.1 Unsafe Block Fundamentals

```veil
/# Unsafe blocks contain operations that bypass safety checks
fn basic_unsafe_example() {
    unsafe {
        const ptr = malloc(1024);
        *ptr = 42;  /# Raw pointer dereference
        free(ptr);
    }
    /# Safety is restored outside the unsafe block
}

/# Unsafe functions must be called within unsafe blocks
unsafe fn raw_memory_copy(src: rawptr, dst: rawptr, len: usize) {
    /# Platform-specific memory copy implementation
    /# Caller must ensure memory regions are valid and non-overlapping
}

fn safe_wrapper(data: [u8]) {
    const dest = allocate_buffer(data.len());
    unsafe {
        /# Safety: both pointers are valid, regions don't overlap
        raw_memory_copy(data.as_ptr(), dest.as_mut_ptr(), data.len());
    }
}
```

#### 3.9.2 Raw Pointer Operations

```veil
/# Raw pointer types
fn pointer_operations() {
    const x = 42;
    const ptr: *const i32 = &x;     /# Immutable raw pointer
    var y = 100;
    const mut_ptr: *mut i32 = &mut y; /# Mutable raw pointer

    unsafe {
        /# Dereferencing requires unsafe
        const value = *ptr;
        *mut_ptr = 200;

        /# Pointer arithmetic
        const next_ptr = ptr.offset(1);
        const byte_ptr = ptr as *const u8;

        /# Null pointer checks (recommended)
        if !ptr.is_null() {
            const safe_value = *ptr;
        }
    }
}

/# Raw pointer creation from addresses
fn from_address() {
    unsafe {
        const ptr = 0x1000 as *mut u8;  /# Dangerous - arbitrary address
        /# Only use if you know the address is valid
    }
}
```

#### 3.9.3 Union Types

```veil
/# Unions share memory between fields - accessing is unsafe
union Value {
    int: i32,
    float: f32,
    bytes: [u8; 4],
}

fn union_example() {
    var val = Value { int: 0x42424242 };

    unsafe {
        /# Reading any field requires unsafe
        const as_int = val.int;
        const as_float = val.float;       /# Reinterpret bits as float
        const first_byte = val.bytes[0];  /# Access byte representation
    }

    /# Writing to union fields is safe
    val.float = 3.14;

    unsafe {
        /# But reading is still unsafe
        const reinterpreted = val.int;
    }
}

/# Tagged unions for safer alternatives
enum SafeValue {
    Int(i32),
    Float(f32),
    Bytes([u8; 4]),
}
```

#### 3.9.4 Foreign Function Interface (FFI)

```veil
/# External function declarations
extern "C" {
    fn malloc(size: usize) -> rawptr;
    fn free(ptr: rawptr);
    fn strlen(s: *const u8) -> usize;
    fn memcpy(dest: rawptr, src: rawptr, n: usize) -> rawptr;
}

/# Calling external functions requires unsafe
fn ffi_example() {
    unsafe {
        const ptr = malloc(1024);
        if !ptr.is_null() {
            /# Use the allocated memory
            *(ptr as *mut i32) = 42;
            free(ptr);
        }
    }
}

/# Safe wrappers for FFI functions
fn safe_malloc(size: usize) -> Option<rawptr> {
    unsafe {
        const ptr = malloc(size);
        if ptr.is_null() {
            None
        } else {
            Some(ptr)
        }
    }
}

/# C string handling
fn c_string_length(c_str: *const u8) -> usize {
    unsafe {
        strlen(c_str)
    }
}
```

#### 3.9.5 Unsafe Traits and Implementations

```veil
/# Marking traits as unsafe
unsafe trait RawData {
    fn as_raw_ptr(&self) -> *const u8;
    fn raw_len(&self) -> usize;
}

/# Implementing unsafe traits requires unsafe
unsafe impl RawData for [u8] {
    fn as_raw_ptr(&self) -> *const u8 {
        self.as_ptr()
    }

    fn raw_len(&self) -> usize {
        self.len()
    }
}

/# Using unsafe trait methods
fn process_raw_data<T: RawData>(data: &T) {
    unsafe {
        const ptr = data.as_raw_ptr();
        const len = data.raw_len();
        /# Process raw memory
    }
}
```

#### 3.9.6 Memory Layout and Transmutation

```veil
/# Transmuting between types of same size
fn transmute_example() {
    const x: f32 = 3.14;

    unsafe {
        /# Reinterpret bits as different type
        const bits: u32 = std::mem::transmute(x);
        const back: f32 = std::mem::transmute(bits);
    }
}

/# Controlling memory layout
#[repr(C)]
struct CCompatible {
    a: u32,
    b: u16,
    c: u8,
}

#[repr(packed)]
struct PackedStruct {
    a: u32,
    b: u8,    /# No padding
    c: u16,   /# May be misaligned
}

/# Accessing packed struct fields requires unsafe
fn packed_access(packed: &PackedStruct) {
    const a = packed.a;  /# Safe - naturally aligned

    unsafe {
        /# Potentially misaligned access
        const c = packed.c;
    }

    /# Safe alternative - copy to aligned location
    const c_safe = { packed.c };
}
```

#### 3.9.7 Unsafe Guidelines and Best Practices

```veil
/# 1. Minimize unsafe code scope
fn good_practice() {
    /# Safe code...

    const result = unsafe {
        /# Minimal unsafe block
        dangerous_operation()
    };

    /# More safe code...
}

/# 2. Document safety requirements
/#/ # Safety
/#/
/#/ `ptr` must point to valid memory of at least `len` bytes.
/#/ The memory must be properly aligned for type `T`.
/#/ The caller must ensure no other code accesses this memory concurrently.
unsafe fn read_raw<T>(ptr: *const T, len: usize) -> [T] {
    /# Implementation...
}

/# 3. Provide safe wrappers
struct SafeBuffer {
    data: rawptr,
    len: usize,
    capacity: usize,
}

impl SafeBuffer {
    fn new(capacity: usize) -> Option<SafeBuffer> {
        unsafe {
            const data = malloc(capacity);
            if data.is_null() {
                None
            } else {
                Some(SafeBuffer { data, len: 0, capacity })
            }
        }
    }

    fn push(&mut self, value: u8) -> Result<(), BufferError> {
        if self.len >= self.capacity {
            return Err(BufferError::Full);
        }

        unsafe {
            *((self.data as *mut u8).offset(self.len as isize)) = value;
        }
        self.len += 1;
        Ok(())
    }
}

impl Drop for SafeBuffer {
    fn drop(&mut self) {
        unsafe {
            free(self.data);
        }
    }
}

/# 4. Use assertions to document assumptions
fn careful_unsafe_operation(slice: &[u8]) {
    assert!(!slice.is_empty(), "Slice must not be empty");
    assert!(slice.len() <= 1024, "Slice too large");

    unsafe {
        /# Now we can safely perform the operation
        /# based on our documented assumptions
        dangerous_slice_operation(slice.as_ptr(), slice.len());
    }
}
```

#### 3.9.8 Common Unsafe Patterns

```veil
/# Pattern: Safe initialization of uninitialized memory
fn init_array<T: Default>(count: usize) -> [T] {
    var result = [];
    result.reserve(count);

    unsafe {
        /# Initialize uninitialized memory
        for i in 0..count {
            const ptr = result.as_mut_ptr().offset(i as isize);
            ptr.write(T::default());
        }
        result.set_len(count);
    }

    result
}

/# Pattern: Implementing custom collections
struct RingBuffer<T> {
    data: rawptr,
    capacity: usize,
    head: usize,
    tail: usize,
}

impl<T> RingBuffer<T> {
    fn push(&mut self, value: T) -> Result<(), T> {
        const next_tail = (self.tail + 1) % self.capacity;
        if next_tail == self.head {
            return Err(value); /# Buffer full
        }

        unsafe {
            const ptr = (self.data as *mut T).offset(self.tail as isize);
            ptr.write(value);
        }

        self.tail = next_tail;
        Ok(())
    }

    fn pop(&mut self) -> Option<T> {
        if self.head == self.tail {
            return None; /# Buffer empty
        }

        unsafe {
            const ptr = (self.data as *mut T).offset(self.head as isize);
            const value = ptr.read();
            self.head = (self.head + 1) % self.capacity;
            Some(value)
        }
    }
}
```

/# Transmutation between types
fn transmute_example() {
const value: i32 = 42;
unsafe {
const float_bits: f32 = transmute(value);
}
}

/# Raw pointer arithmetic
fn pointer_arithmetic(ptr: *i32, offset: isize) -> *i32 {
unsafe {
ptr.offset(offset)
}
}

````
### 3.10 Advanced Type Features

Advanced type system features for sophisticated type-level programming, enabling highly expressive and safe abstractions.

#### 3.10.1 Type Aliases and Newtype Pattern

```veil
/# Simple type aliases for readability
type UserId = u64;
type Result<T> = std::result::Result<T, AppError>;
type JsonValue = std::collections::HashMap<string, Value>;

/# Newtype pattern for type safety
struct Meters(f64);
struct Seconds(f64);
struct Feet(f64);

impl Meters {
    fn to_feet(self) -> Feet {
        Feet(self.0 * 3.28084)
    }

    fn new(value: f64) -> Meters {
        assert!(value >= 0.0, "Distance cannot be negative");
        Meters(value)
    }
}

/# This prevents mixing incompatible units
fn calculate_speed(distance: Meters, time: Seconds) -> f64 {
    distance.0 / time.0  /# Explicit access to inner value
}

/# Compiler prevents errors like:
/# calculate_speed(42.0, 10.0);  /# Error: expected Meters and Seconds
````

#### 3.10.2 Associated Types and Type Families

```veil
/# Traits with associated types
trait Iterator {
    type Item;          /# Associated type
    type Error = ();    /# Associated type with default

    fn next(&mut self) -> Option<Self::Item>;
    fn collect<C: FromIterator<Self::Item>>(self) -> C;
}

trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter;
}

/# Generic implementation using associated types
impl<T> Iterator for VecIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        /# Implementation
    }
}

/# Collection trait with multiple associated types
trait Collection {
    type Item;
    type Index;
    type Output;

    fn get(&self, index: Self::Index) -> Option<&Self::Output>;
    fn len(&self) -> usize;
}

impl<T> Collection for [T] {
    type Item = T;
    type Index = usize;
    type Output = T;

    fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() { Some(&self[index]) } else { None }
    }
}
```

#### 3.10.3 Higher-Kinded Types and Generic Constraints

```veil
/# Generic constraints with where clauses
fn process_data<T, E>(data: [T]) -> Result<ProcessedData, E>
where
    T: Clone + Send + Sync,
    E: Error + Send + 'static,
{
    /# Complex processing with multiple constraints
}

/# Multiple bounds on a single type
fn advanced_processing<T>(items: [T]) -> [T]
where
    T: Clone + PartialOrd + Hash + Display + Send + Sync,
{
    /# Implementation using all these traits
}

/# Associated type constraints
fn collect_results<I>(iter: I) -> Result<[I::Item], I::Error>
where
    I: Iterator,
    I::Item: Clone,
    I::Error: Display,
{
    /# Implementation
}

/# Higher-kinded type simulation
trait Functor<F<_>> {
    fn map<A, B, Func>(self, f: Func) -> F<B>
    where
        Self: F<A>,
        Func: Fn(A) -> B;
}

/# Phantom types for compile-time state tracking
struct State<T> {
    _phantom: PhantomData<T>,
}

struct Authenticated;
struct Unauthenticated;

impl State<Unauthenticated> {
    fn new() -> State<Unauthenticated> {
        State { _phantom: PhantomData }
    }

    fn authenticate(self, credentials: Credentials) -> Result<State<Authenticated>, AuthError> {
        /# Authentication logic
        Ok(State { _phantom: PhantomData })
    }
}

impl State<Authenticated> {
    fn access_secure_resource(&self) -> SecureData {
        /# Only available on authenticated state
    }
}
```

#### 3.10.4 Variance and Lifetime Parameters

```veil
/# Lifetime parameters for borrowed data
struct BorrowedData<'a> {
    data: &'a [u8],
    metadata: &'a str,
}

impl<'a> BorrowedData<'a> {
    fn new(data: &'a [u8], metadata: &'a str) -> BorrowedData<'a> {
        BorrowedData { data, metadata }
    }

    /# Multiple lifetime parameters
    fn combine<'b>(&self, other: &'b BorrowedData<'b>) -> CombinedData<'a, 'b> {
        CombinedData {
            first: self,
            second: other,
        }
    }
}

/# Variance annotations
struct Container<+T> {  /# Covariant in T
    value: T,
}

struct Function<-T, +R> {  /# Contravariant in T, covariant in R
    func: fn(T) -> R,
}

/# Lifetime bounds and constraints
fn process_with_lifetime<'a, T>(data: &'a T) -> ProcessedData<'a>
where
    T: 'a + Clone + Send,  /# T must live at least as long as 'a
{
    /# Implementation
}

/# Higher-ranked trait bounds (HRTB)
fn apply_to_all<F>(f: F)
where
    F: for<'a> Fn(&'a str) -> &'a str,  /# F works for any lifetime
{
    /# Implementation
}
```

#### 3.10.5 Type-Level Programming and Const Generics

```veil
/# Const generics for compile-time constants
struct FixedArray<T, const N: usize> {
    data: [T; N],
}

impl<T, const N: usize> FixedArray<T, N> {
    fn new() -> FixedArray<T, N>
    where
        T: Default,
    {
        FixedArray {
            data: [T::default(); N],  /# Array of size N
        }
    }

    fn len(&self) -> usize {
        N  /# Compile-time constant
    }
}

/# Type-level arithmetic with const generics
fn matrix_multiply<const M: usize, const N: usize, const P: usize>(
    a: [[f64; N]; M],
    b: [[f64; P]; N],
) -> [[f64; P]; M] {
    /# Matrix multiplication with compile-time size checking
}

/# Const generic constraints
fn process_small_array<T, const N: usize>(arr: [T; N]) -> ProcessedArray<T, N>
where
    [(); N]: Sized,  /# Ensure N is a valid array size
    const { N <= 1024 }: bool,  /# Compile-time assertion
{
    /# Only accepts arrays up to size 1024
}

/# Type-level computations
trait TypeLevelAdd<Rhs> {
    type Output;
}

struct Peano<const N: usize>;

impl<const N: usize, const M: usize> TypeLevelAdd<Peano<M>> for Peano<N> {
    type Output = Peano<{N + M}>;
}

/# Type-level state machines
trait State {
    type Next: State;
    fn transition(self) -> Self::Next;
}

struct Idle;
struct Processing;
struct Complete;

impl State for Idle {
    type Next = Processing;
    fn transition(self) -> Processing { Processing }
}

impl State for Processing {
    type Next = Complete;
    fn transition(self) -> Complete { Complete }
}
```

#### 3.10.6 Advanced Trait Patterns

```veil
/# Object-safe traits for dynamic dispatch
trait Drawable {
    fn draw(&self);
    fn bounds(&self) -> Rectangle;
    /# Cannot have generic methods for object safety
}

/# Trait objects
fn render_shapes(shapes: [&dyn Drawable]) {
    for shape in shapes {
        shape.draw();  /# Dynamic dispatch
    }
}

/# Blanket implementations
trait Display {
    fn fmt(&self) -> string;
}

/# Implement Display for all types that implement Debug
impl<T: Debug> Display for T {
    fn fmt(&self) -> string {
        format!("{:?}", self)
    }
}

/# Trait aliases for convenience
trait Printable = Display + Clone + Send;

/# Extension traits
trait SliceExt<T> {
    fn find_duplicates(&self) -> [&T];
    fn chunk_by<F>(&self, f: F) -> ChunkIterator<T, F>
    where
        F: Fn(&T, &T) -> bool;
}

impl<T> SliceExt<T> for [T]
where
    T: PartialEq,
{
    fn find_duplicates(&self) -> [&T] {
        /# Implementation
    }
}

/# Marker traits for compile-time properties
trait Send {}  /# Type can be sent between threads
trait Sync {}  /# Type can be shared between threads
trait Copy {}  /# Type can be copied bitwise

/# Auto traits (automatically implemented when safe)
auto trait Send {}
auto trait Sync {}

/# Negative reasoning with auto traits
struct NotSend(*const u8);
impl !Send for NotSend {}  /# Explicitly not Send
```

#### 3.10.7 Type Erasure and Existential Types

```veil
/# Type erasure with trait objects
struct AnyIterator<T> {
    iter: Box<dyn Iterator<Item = T>>,
}

impl<T> AnyIterator<T> {
    fn new<I: Iterator<Item = T> + 'static>(iter: I) -> AnyIterator<T> {
        AnyIterator {
            iter: Box::new(iter),
        }
    }
}

impl<T> Iterator for AnyIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }
}

/# Existential types (impl Trait)
fn make_iterator() -> impl Iterator<Item = i32> {
    (0..10).map(|x| x * x)  /# Return type is erased
}

/# Existential types in argument position
fn process_iterator(iter: impl Iterator<Item = string>) {
    for item in iter {
        println!("{}", item);
    }
}

/# Associated type existentials
trait AsyncDatabase {
    type Connection: Send + Sync;
    type Error: Error + Send + Sync;

    async fn connect(&self) -> Result<Self::Connection, Self::Error>;
}

/# Implementation hides concrete types
struct PostgresDb;

impl AsyncDatabase for PostgresDb {
    type Connection = PgConnection;  /# Concrete type hidden from users
    type Error = PgError;
}
```

#### 3.10.8 Dependent Types and Refinement Types

```veil
/# Refinement types with compile-time verification
struct NonEmpty<T> {
    inner: [T],
}

impl<T> NonEmpty<T> {
    fn new(vec: [T]) -> Option<NonEmpty<T>> {
        if vec.is_empty() {
            None
        } else {
            Some(NonEmpty { inner: vec })
        }
    }

    fn head(&self) -> &T {
        &self.inner[0]  /# Safe - guaranteed non-empty
    }

    fn tail(&self) -> &[T] {
        &self.inner[1..]
    }
}

/# Index bounds checking with dependent types
struct BoundedIndex<const MAX: usize> {
    value: usize,
}

impl<const MAX: usize> BoundedIndex<MAX> {
    fn new(value: usize) -> Option<BoundedIndex<MAX>> {
        if value < MAX {
            Some(BoundedIndex { value })
        } else {
            None
        }
    }

    fn get(&self) -> usize {
        self.value  /# Guaranteed < MAX
    }
}

/# Safe array access with bounded indices
fn safe_access<T, const N: usize>(
    array: &[T; N],
    index: BoundedIndex<N>,
) -> &T {
    &array[index.get()]  /# No bounds check needed
}

/# Liquid types for runtime verification
struct Positive(f64) where Self::0 > 0.0;
struct Even(i32) where Self::0 % 2 == 0;

impl Positive {
    fn new(value: f64) -> Result<Positive, ValueError> {
        if value > 0.0 {
            Ok(Positive(value))
        } else {
            Err(ValueError::NotPositive(value))
        }
    }

    fn sqrt(self) -> Positive {
        Positive(self.0.sqrt())  /# sqrt of positive is positive
    }
}
```

}

/# Const generics
struct FixedArray<T, const N: usize> {
data: [T; N],
}

impl<T, const N: usize> FixedArray<T, N> {
fn new() -> Self {
FixedArray { data: [T::default(); N] }
}

    fn len(self) -> usize { N }

}

/# Generic array processing
fn process_array<const SIZE: usize>(arr: [i32; SIZE]) -> [i32; SIZE] {
var result = [0; SIZE];
for i in 0..SIZE {
result[i] = arr[i] \* 2;
}
result
}

/# Higher-kinded types for advanced generic programming
trait Functor<F<\_>> {
fn map<A, B>(self: F<A>, f: fn(A) -> B) -> F<B>;
}

/# Phantom types for compile-time state tracking
struct Database<State> {
connection: Connection,
\_state: PhantomData<State>,
}

struct Connected;
struct Disconnected;

impl Database<Disconnected> {
fn connect(self) -> Database<Connected> { }
}

impl Database<Connected> {
fn query(self, sql: string) -> QueryResult { }
}

/# Associated const in traits
trait MathConstants {
const PI: f64;
const E: f64;
}

/# Generic associated types (GATs)
trait AsyncIterator {
type Item;
type Future<'a>: Future<Output = Option<Self::Item>>;

    fn next<'a>(self) -> Self::Future<'a>;

}

````
## 4. The ve Toolchain & Ecosystem

Productivity is built on tooling. Veil provides a single, cohesive, and blazing-fast tool, ve, that manages the entire development lifecycle from a consistent command-line interface.

### 4.1 Package Management

The ve.toml manifest defines project metadata, dependencies, and build configuration:

```toml
[package]
name = "my-app"
version = "1.0.0"
edition = "2024"
authors = ["Your Name <email@example.com>"]
license = "MIT"
description = "A sample Veil application"
repository = "https:/#github.com/user/my-app"
keywords = ["web", "api", "async"]

[dependencies]
http = "2.0"
json = { version = "1.0", features = ["derive"] }

[dev-dependencies]
test-utils = "0.1"
bench-tools = "2.0"

[build-dependencies]
build-script = "1.0"

[features]
default = ["std"]
std = []
networking = []
database = []
full = ["networking", "database"]

[profile.dev]
opt-level = 0
debug = true
debug-assertions = true

[profile.release]
opt-level = 3
debug = false
lto = true
codegen-units = 1
panic = "abort"

[profile.test]
opt-level = 1
debug = true

[target.'cfg(unix)']
dependencies = { unix-specific = "1.0" }

[[bin]]
name = "main"
path = "src/main.vl"

[[example]]
name = "demo"
path = "examples/demo.vl"

[workspace]
members = [".", "subcrate"]
````

**Package commands:**

- `ve new <project>`: Creates a new project with standard layout
- `ve add <package>`: Adds dependency to ve.toml
- `ve remove <package>`: Removes dependency
- `ve update`: Updates dependencies to latest compatible versions
- `ve tree`: Shows dependency tree
- `ve search <query>`: Searches package registry

### 4.2 Build System & Targets

Comprehensive build system supporting multiple targets and optimization levels:

```bash
# Development builds
ve build              # Debug build
ve run                # Build and run
ve check              # Check without building

# Release builds
ve build --release    # Optimized release build
ve build --profile bench  # Benchmark profile

# Cross-compilation
ve build --target wasm32-unknown-unknown
ve build --target x86_64-pc-windows-gnu
ve build --target aarch64-apple-darwin
ve build --target riscv64gc-unknown-linux-gnu

# Output options
ve build --bin main           # Build specific binary
ve build --example demo       # Build example
ve build --lib               # Build library only
ve build --workspace         # Build entire workspace

# Advanced options
ve build --features async,networking  # Enable features
ve build --no-default-features       # Disable default features
ve build --jobs 8                    # Parallel compilation
ve build --verbose                   # Verbose output
```

### 4.3 Cross-Platform Compilation

Veil provides seamless cross-compilation support, allowing you to build for any target platform from any host system:

```bash
# List available targets
ve targets list

# Cross-compile for different platforms
ve build --target x86_64-linux-gnu       # Linux 64-bit
ve build --target x86_64-windows-msvc    # Windows 64-bit
ve build --target x86_64-darwin          # macOS 64-bit
ve build --target aarch64-linux-gnu      # ARM64 Linux
ve build --target aarch64-darwin         # Apple Silicon
ve build --target wasm32-wasi            # WebAssembly
ve build --target i686-windows-msvc      # Windows 32-bit
ve build --target armv7-linux-gnueabi    # ARM Linux
ve build --target riscv64gc-linux-gnu    # RISC-V 64-bit

# Multiple targets at once
ve build --targets x86_64-linux-gnu,x86_64-windows-msvc,x86_64-darwin

# Release builds for distribution
ve build --release --target x86_64-windows-msvc
```

**Supported Compilation Targets**:

**Tier 1 Platforms** (Guaranteed to work):

- `x86_64-linux-gnu` - Linux 64-bit (GNU)
- `x86_64-windows-msvc` - Windows 64-bit (MSVC)
- `x86_64-darwin` - macOS 64-bit (Intel)
- `aarch64-darwin` - macOS ARM64 (Apple Silicon)

**Tier 2 Platforms** (Tested and supported):

- `aarch64-linux-gnu` - ARM64 Linux
- `i686-windows-msvc` - Windows 32-bit
- `x86_64-linux-musl` - Linux 64-bit (musl libc)
- `wasm32-wasi` - WebAssembly System Interface

**Tier 3 Platforms** (Best effort):

- `armv7-linux-gnueabi` - ARM Linux
- `riscv64gc-linux-gnu` - RISC-V 64-bit
- `s390x-linux-gnu` - IBM Z/Architecture
- `powerpc64le-linux-gnu` - PowerPC 64-bit LE

**Target Triple Format**: `<arch>-<vendor>-<sys>-<abi>`

- **arch**: x86_64, aarch64, arm, i686, wasm32, riscv64gc, etc.
- **vendor**: pc, apple, unknown, etc.
- **sys**: linux, windows, darwin, wasi, etc.
- **abi**: gnu, msvc, musl, etc.

**Cross-Compilation Features**:

```veil
/# Platform-specific code selection
#[cfg(target_os = "windows")]
fn get_home_dir() -> Option<PathBuf> {
    std::env::var("USERPROFILE").ok().map(PathBuf::from)
}

#[cfg(target_os = "linux")]
fn get_home_dir() -> Option<PathBuf> {
    std::env::var("HOME").ok().map(PathBuf::from)
}

#[cfg(target_arch = "wasm32")]
fn platform_init() {
    /# WebAssembly-specific initialization
}

#[cfg(not(target_arch = "wasm32"))]
fn platform_init() {
    /# Native platform initialization
}
```

**Toolchain Configuration** (ve.toml):

```toml
[build]
default-target = "x86_64-unknown-linux-gnu"

[targets]
"x86_64-windows-msvc" = { linker = "lld-link" }
"aarch64-apple-darwin" = { linker = "ld64.lld" }
"wasm32-wasi" = { linker = "wasm-ld" }

[cross]
# Automatic toolchain download
auto-install = true
# Custom sysroot paths
sysroot = "/opt/cross"
```

### 4.4 Conditional Compilation

Fine-grained conditional compilation based on target, features, and custom configs:

```veil
/# Target-based conditionals
#[cfg(target_os = "linux")]
fn linux_specific() { }

#[cfg(target_arch = "x86_64")]
const CACHE_LINE_SIZE: usize = 64;

#[cfg(target_pointer_width = "64")]
type PointerSized = u64;

/# Feature-based conditionals
#[cfg(feature = "networking")]
mod network {
    pub fn connect() { }
}

/# Debug/Release conditionals
#[cfg(debug_assertions)]
fn debug_only_function() { }

#[cfg(not(debug_assertions))]
const OPTIMIZED: bool = true;

/# Custom configuration
#[cfg(custom_config = "production")]
const API_URL: &str = "https:/#api.production.com";

#[cfg(custom_config = "development")]
const API_URL: &str = "http:/#localhost:3000";

/# Complex conditions
#[cfg(all(unix, target_arch = "x86_64", feature = "simd"))]
mod optimized_unix { }

#[cfg(any(windows, target_os = "macos"))]
fn desktop_only() { }
```

### 4.5 Attribute System

Comprehensive attribute system for metadata, code generation, and compiler directives:

```veil
/# Derive attributes for automatic trait implementation
#[derive(Clone, Debug, PartialEq, Hash)]
struct User {
    name: string,
    id: u64,
}

/# Serialization attributes
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct ApiUser {
    #[serde(rename = "userId")]
    id: u64,

    #[serde(skip_serializing_if = "Option::is_none")]
    email: Option<string>,

    #[serde(default)]
    active: bool,
}

/# Memory layout attributes
#[repr(C)]
struct CCompatible {
    x: i32,
    y: i32,
}

#[repr(packed)]
struct PackedStruct {
    a: u8,
    b: u32,
}

/# Compiler optimization hints
#[inline(always)]
fn always_inline() { }

#[inline(never)]
fn never_inline() { }

#[cold]
fn error_path() { }

#[hot]
fn performance_critical() { }

/# Deprecation and versioning
#[deprecated(since = "1.2.0", note = "use new_function instead")]
fn old_function() { }

#[must_use = "this function returns a value that must be used"]
fn important_result() -> Result<Data, Error> { }

/# Test attributes
#[test]
fn basic_test() {
    assert_eq!(2 + 2, 4);
}

#[test]
#[should_panic(expected = "division by zero")]
fn panic_test() {
    divide(1, 0);
}

#[bench]
fn benchmark_function(b: &Bencher) {
    b.iter(|| expensive_computation());
}

/# Custom attributes for macros/codegen
#[route(GET, "/users/{id}")]
async fn get_user(id: u64) -> Response { }

#[database_table("users")]
struct UserModel {
    #[primary_key]
    id: u64,

    #[unique]
    email: string,
}
```

### 4.7 Self-Hosting Roadmap

Veil is committed to becoming self-hosted before version 1.0, demonstrating the language's maturity and capability for systems programming:

**Current State**: The Veil compiler is implemented in [host language] and can successfully compile Veil programs to native code for all supported target platforms.

**Self-Hosting Milestones**:

1. **Phase 1: Core Compiler (Target: v0.8)**
   - Rewrite the lexer and parser in Veil
   - Implement type checking and semantic analysis
   - Basic code generation for primary targets

2. **Phase 2: Advanced Features (Target: v0.9)**
   - Complete optimization pipeline
   - Cross-compilation support
   - Full standard library implementation
   - Comprehensive error reporting system

3. **Phase 3: Bootstrap Complete (Target: v1.0)**
   - Self-hosting compiler can build itself
   - Performance parity with original implementation
   - Full toolchain (ve) rewritten in Veil
   - Comprehensive test suite passing

**Benefits of Self-Hosting**:

- **Language Validation**: Proves Veil is suitable for large, complex systems
- **Performance Testing**: The compiler serves as a comprehensive benchmark
- **Feature Completeness**: Ensures all language features work in practice
- **Developer Experience**: Validates tooling and IDE support
- **Community Confidence**: Demonstrates long-term viability

**Self-Hosting Architecture**:

```veil
/# The self-hosted compiler structure
mod compiler {
    pub mod lexer;      /# Tokenization
    pub mod parser;     /# Syntax analysis
    pub mod semantic;   /# Type checking and analysis
    pub mod codegen;    /# Code generation
    pub mod optimizer;  /# Optimization passes
    pub mod linker;     /# Platform-specific linking
}

mod toolchain {
    pub mod build;      /# Build system
    pub mod package;    /# Package manager
    pub mod test;       /# Test runner
    pub mod format;     /# Code formatter
    pub mod lint;       /# Static analysis
}

/# Bootstrap process
fn main() -> Result<(), CompilerError> {
    const args = parse_command_line();
    const config = load_compiler_config()?;

    match args.command {
        Command::Compile { input, output, target } => {
            const compiler = Compiler::new(config, target);
            compiler.compile_file(input, output)
        },
        Command::Build { project } => {
            const builder = Builder::new(config);
            builder.build_project(project)
        },
        _ => handle_other_commands(args),
    }
}
```

**Contributing to Self-Hosting**:
The self-hosting effort is a community-driven initiative. Key areas for contribution:

- Compiler frontend (lexing, parsing, type checking)
- Backend code generation for specific targets
- Optimization passes and analysis
- Standard library implementation
- Tooling and developer experience improvements

### 4.6 Linting & Formatting

Built-in code quality tools with configurable rules:

```bash
# Formatting
ve fmt                    # Format all files
ve fmt --check           # Check if files are formatted
ve fmt src/main.vl       # Format specific file

# Linting
ve lint                  # Run all lints
ve lint --fix           # Auto-fix lints where possible
ve lint --deny warnings # Treat warnings as errors

# Clippy-style suggestions
ve suggest              # Advanced lint suggestions
```

**Lint configuration in ve.toml:**

```toml
[lints]
warnings = "warn"
style = "warn"
performance = "warn"
correctness = "deny"

[lints.allow]
unused-variables = true
dead-code = true

[lints.deny]
unsafe-code = true
missing-docs = true

[lints.custom]
max-line-length = 120
max-function-length = 50
```

**In-code lint controls:**

````veil
#[allow(unused_variables)]
fn development_function() {
    const unused = 42;
}

#[warn(missing_docs)]
mod important_module { }

#[deny(unsafe_code)]
fn safe_only() { }

#[forbid(deprecated)]
fn no_deprecated_usage() { }

## 5. Detailed Semantics

This section defines the precise rules the compiler must enforce.

- **Type System**: Inference is based on a powerful Hindley-Milner algorithm. match statements must be exhaustive, a check enforced at compile time. No implicit numeric coercion is allowed; as is required for all conversions to prevent subtle bugs.

- **Primitive Type Sizes**: Veil provides a comprehensive set of primitive types:
  - **Fixed-size integers**: i8 (1 byte), i16 (2 bytes), i32 (4 bytes), i64 (8 bytes), i128 (16 bytes) for signed integers; u8, u16, u32, u64, u128 for unsigned equivalents
  - **Architecture-dependent integers**: isize and usize are pointer-sized integers (32-bit on 32-bit platforms, 64-bit on 64-bit platforms)
  - **Floating-point**: f32 (IEEE 754 single precision, 4 bytes), f64 (IEEE 754 double precision, 8 bytes)
  - **Boolean**: bool (1 byte, values true/false)
  - **Character**: ch represents a Unicode scalar value (4 bytes, UTF-32)
  - **String**: str is a UTF-8 encoded, growable string type
  - **Special types**: void (zero-sized type), any (dynamic type), rawptr (raw pointer type)

- **Memory Model**: retain is inserted on reference copy; release is inserted on scope exit. An assignment x.field = y is a compile-time error if scope_generation(x) < scope_generation(y) and y contains heap references, effectively preventing upwardly-pointing references in the scope tree.

- **Concurrency Model**: A spawn block creates a nursery that tracks all child tasks. When the block exits, it implicitly awaits all tasks. Veil includes a built-in async runtime with a work-stealing thread pool scheduler, optimized for high-performance I/O-bound tasks. No external async runtime dependencies are required.

- **Compilation Model**: Veil uses ahead-of-time (AOT) compilation to generate optimized native code for the target platform. The compiler performs extensive optimizations during compilation, producing efficient executables without runtime overhead. Cross-compilation is supported for all target platforms from any host platform.

- **Self-Hosting Goal**: The Veil compiler is currently implemented in another language, but will be completely rewritten in Veil itself before reaching version 1.0. This ensures the language is sufficiently powerful and ergonomic for large-scale systems development.

- **comptime Model**: Code is executed in a sandboxed interpreter inside the compiler. It has no I/O access and has a bounded execution time to prevent infinite loops from hanging the compiler. It has full access to the program's type information via introspection APIs.

## 6. Standard Library (std) Overview

The std library is curated to be powerful yet lean, providing a "batteries-included" experience for common tasks.

- **std::prelude**: Contains common types (Option, Result, str, [T]) and functions (print, assert) that are automatically imported into every module.

- **Core Modules**:
  - **std::io**: Asynchronous I/O operations, readers, writers, and buffering
  - **std::fs**: File system operations with async support
  - **std::net**: Network programming primitives (TCP, UDP, Unix sockets)
  - **std::json, std::toml**: Data serialization and deserialization
  - **std::collections**: HashMap<K, V>, HashSet<T>, BTreeMap<K, V>, VecDeque<T>, etc.
  - **std::sync**: Concurrency primitives optimized for Veil's async runtime
  - **std::time**: Time and duration handling, timers, and timeouts
  - **std::str**: String manipulation utilities and formatting
  - **std::mem**: Memory utilities, alignment, and low-level operations
  - **std::env**: Environment variable access and process information

### 6.1 std::sync::Mutex<T> API Reference

```veil
/# The Mutex<T> provides safe, exclusive access to shared data.
/# It is an essential tool for preventing data races in concurrent programs.
impl<T> Mutex<T> {
    /# Creates a new Mutex wrapping the given data.
    fn new(data: T) -> Mutex<T>;

    /# Asynchronously acquires the lock, returning a guard.
    /# This function will pause the current task until the lock is available.
    /# It is an `async` function because the wait may be long, and the scheduler
    /# should run other tasks in the meantime.
    async fn lock(self) -> MutexGuard<T>;

    /# Attempts to acquire the lock immediately without blocking.
    /# Returns an `Option<MutexGuard<T>>`. It returns `Some(guard)` if the lock
    /# was acquired, and `none` otherwise. This is useful for avoiding deadlocks
    /# or implementing more complex locking strategies.
    fn try_lock(self) -> Option<MutexGuard<T>>;
}

/# The MutexGuard provides access to the data and ensures the lock is released.
/# This is a RAII (Resource Acquisition Is Initialization) type.
impl<T> MutexGuard<T> {
    /# Allows mutable access to the wrapped data via dereferencing.
    /# e.g., `*guard = new_value;`
    /# The `*` operator is overloaded to provide this ergonomic access.
}
/# When a MutexGuard<T> value goes out of scope, its destructor
/# is called, which automatically and safely unlocks the Mutex.
/# This means you can never forget to unlock a mutex.
````

### 6.2 std::collections API Reference

```veil
/# HashMap<K, V> - Hash table implementation
impl<K, V> HashMap<K, V>
where
    K: Hash + Eq,
{
    fn new() -> HashMap<K, V>;
    fn with_capacity(capacity: usize) -> HashMap<K, V>;

    fn insert(&mut self, key: K, value: V) -> Option<V>;
    fn get(&self, key: &K) -> Option<&V>;
    fn get_mut(&mut self, key: &K) -> Option<&mut V>;
    fn remove(&mut self, key: &K) -> Option<V>;

    fn contains_key(&self, key: &K) -> bool;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;

    fn keys(&self) -> Keys<K, V>;
    fn values(&self) -> Values<K, V>;
    fn iter(&self) -> Iter<K, V>;
}

/# HashSet<T> - Hash set implementation
impl<T> HashSet<T>
where
    T: Hash + Eq,
{
    fn new() -> HashSet<T>;
    fn with_capacity(capacity: usize) -> HashSet<T>;

    fn insert(&mut self, value: T) -> bool;
    fn remove(&mut self, value: &T) -> bool;
    fn contains(&self, value: &T) -> bool;

    fn union(&self, other: &HashSet<T>) -> Union<T>;
    fn intersection(&self, other: &HashSet<T>) -> Intersection<T>;
    fn difference(&self, other: &HashSet<T>) -> Difference<T>;
}

/# VecDeque<T> - Double-ended queue
impl<T> VecDeque<T> {
    fn new() -> VecDeque<T>;
    fn with_capacity(capacity: usize) -> VecDeque<T>;

    fn push_front(&mut self, value: T);
    fn push_back(&mut self, value: T);
    fn pop_front(&mut self) -> Option<T>;
    fn pop_back(&mut self) -> Option<T>;

    fn front(&self) -> Option<&T>;
    fn back(&self) -> Option<&T>;
    fn len(&self) -> usize;
}
```

### 6.3 std::io API Reference

```veil
/# Async I/O traits
trait AsyncRead {
    async fn read(&mut self, buf: &mut [u8]) -> io::Result<usize>;
    async fn read_to_end(&mut self, buf: &mut [u8]) -> io::Result<usize>;
    async fn read_to_string(&mut self, buf: &mut string) -> io::Result<usize>;
}

trait AsyncWrite {
    async fn write(&mut self, buf: &[u8]) -> io::Result<usize>;
    async fn write_all(&mut self, buf: &[u8]) -> io::Result<()>;
    async fn flush(&mut self) -> io::Result<()>;
}

/# Buffered I/O
struct BufReader<R: AsyncRead> {
    inner: R,
    buffer: [u8],
}

impl<R: AsyncRead> BufReader<R> {
    fn new(inner: R) -> BufReader<R>;
    fn with_capacity(capacity: usize, inner: R) -> BufReader<R>;

    async fn read_line(&mut self, buf: &mut string) -> io::Result<usize>;
    async fn lines(&mut self) -> Lines<R>;
}

struct BufWriter<W: AsyncWrite> {
    inner: W,
    buffer: [u8],
}

impl<W: AsyncWrite> BufWriter<W> {
    fn new(inner: W) -> BufWriter<W>;
    fn with_capacity(capacity: usize, inner: W) -> BufWriter<W>;

    async fn flush(&mut self) -> io::Result<()>;
}

/# Standard streams
fn stdin() -> Stdin;
fn stdout() -> Stdout;
fn stderr() -> Stderr;

/# File operations are in std::fs
```

### 6.4 std::fs API Reference

```veil
/# File operations
struct File {
    /# File handle
}

impl File {
    async fn open<P: AsRef<Path>>(path: P) -> io::Result<File>;
    async fn create<P: AsRef<Path>>(path: P) -> io::Result<File>;
    async fn open_options(options: &OpenOptions) -> io::Result<File>;

    async fn metadata(&self) -> io::Result<Metadata>;
    async fn sync_all(&self) -> io::Result<()>;
    async fn sync_data(&self) -> io::Result<()>;
    async fn set_len(&self, size: u64) -> io::Result<()>;
}

impl AsyncRead for File { /* implementation */ }
impl AsyncWrite for File { /* implementation */ }

/# Directory operations
async fn read_dir<P: AsRef<Path>>(path: P) -> io::Result<ReadDir>;
async fn create_dir<P: AsRef<Path>>(path: P) -> io::Result<()>;
async fn create_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()>;
async fn remove_dir<P: AsRef<Path>>(path: P) -> io::Result<()>;
async fn remove_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()>;

/# File metadata and permissions
async fn metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata>;
async fn symlink_metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata>;

/# File operations
async fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<u64>;
async fn rename<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<()>;
async fn remove_file<P: AsRef<Path>>(path: P) -> io::Result<()>;

/# Convenience functions
async fn read<P: AsRef<Path>>(path: P) -> io::Result<[u8]>;
async fn read_to_string<P: AsRef<Path>>(path: P) -> io::Result<string>;
async fn write<P: AsRef<Path>, C: AsRef<[u8]>>(path: P, contents: C) -> io::Result<()>;
```

### 6.5 std::net API Reference

```veil
/# TCP networking
struct TcpListener {
    /# TCP listener
}

impl TcpListener {
    async fn bind<A: ToSocketAddrs>(addr: A) -> io::Result<TcpListener>;
    async fn accept(&self) -> io::Result<(TcpStream, SocketAddr)>;
    fn local_addr(&self) -> io::Result<SocketAddr>;
}

struct TcpStream {
    /# TCP stream
}

impl TcpStream {
    async fn connect<A: ToSocketAddrs>(addr: A) -> io::Result<TcpStream>;
    fn peer_addr(&self) -> io::Result<SocketAddr>;
    fn local_addr(&self) -> io::Result<SocketAddr>;
    fn nodelay(&self) -> io::Result<bool>;
    fn set_nodelay(&self, nodelay: bool) -> io::Result<()>;
}

impl AsyncRead for TcpStream { /* implementation */ }
impl AsyncWrite for TcpStream { /* implementation */ }

/# UDP networking
struct UdpSocket {
    /# UDP socket
}

impl UdpSocket {
    async fn bind<A: ToSocketAddrs>(addr: A) -> io::Result<UdpSocket>;
    async fn connect<A: ToSocketAddrs>(&self, addr: A) -> io::Result<()>;

    async fn send_to<A: ToSocketAddrs>(&self, buf: &[u8], addr: A) -> io::Result<usize>;
    async fn recv_from(&self, buf: &mut [u8]) -> io::Result<(usize, SocketAddr)>;

    async fn send(&self, buf: &[u8]) -> io::Result<usize>;
    async fn recv(&self, buf: &mut [u8]) -> io::Result<usize>;

    fn local_addr(&self) -> io::Result<SocketAddr>;
}
```

### 6.6 std::time API Reference

```veil
/# Duration type
struct Duration {
    /# Time duration
}

impl Duration {
    fn new(secs: u64, nanos: u32) -> Duration;
    fn from_secs(secs: u64) -> Duration;
    fn from_millis(millis: u64) -> Duration;
    fn from_micros(micros: u64) -> Duration;
    fn from_nanos(nanos: u64) -> Duration;

    fn as_secs(&self) -> u64;
    fn as_millis(&self) -> u128;
    fn as_micros(&self) -> u128;
    fn as_nanos(&self) -> u128;

    fn checked_add(&self, other: Duration) -> Option<Duration>;
    fn checked_sub(&self, other: Duration) -> Option<Duration>;
    fn checked_mul(&self, rhs: u32) -> Option<Duration>;
    fn checked_div(&self, rhs: u32) -> Option<Duration>;
}

/# Instant for measuring elapsed time
struct Instant {
    /# Point in time
}

impl Instant {
    fn now() -> Instant;
    fn elapsed(&self) -> Duration;
    fn checked_duration_since(&self, earlier: Instant) -> Option<Duration>;
}

/# SystemTime for wall clock time
struct SystemTime {
    /# System time
}

impl SystemTime {
    fn now() -> SystemTime;
    fn elapsed(&self) -> Result<Duration, SystemTimeError>;
    fn checked_add(&self, duration: Duration) -> Option<SystemTime>;
    fn checked_sub(&self, duration: Duration) -> Option<SystemTime>;
}

/# Async time utilities
async fn sleep(duration: Duration);
async fn timeout<F: Future>(duration: Duration, future: F) -> Result<F::Output, TimeoutError>;

struct Interval {
    /# Periodic timer
}

impl Interval {
    fn new(duration: Duration) -> Interval;
    async fn tick(&mut self) -> Instant;
}
```

### 6.7 std::string API Reference

```veil
/# String manipulation
impl str {
    fn new() -> str;
    fn with_capacity(capacity: usize) -> str;
    fn from_utf8(vec: [u8]) -> Result<str, FromUtf8Error>;
    fn from_utf8_lossy(v: &[u8]) -> Cow<str>;

    fn push(&mut self, ch: ch);
    fn push_str(&mut self, string: &str);
    fn pop(&mut self) -> Option<ch>;
    fn remove(&mut self, idx: usize) -> ch;
    fn insert(&mut self, idx: usize, ch: ch);
    fn insert_str(&mut self, idx: usize, string: &str);

    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn capacity(&self) -> usize;
    fn clear(&mut self);
    fn truncate(&mut self, new_len: usize);

    fn contains(&self, pat: &str) -> bool;
    fn starts_with(&self, pat: &str) -> bool;
    fn ends_with(&self, pat: &str) -> bool;
    fn find(&self, pat: &str) -> Option<usize>;
    fn rfind(&self, pat: &str) -> Option<usize>;

    fn replace(&self, from: &str, to: &str) -> str;
    fn replacen(&self, from: &str, to: &str, count: usize) -> str;

    fn split(&self, pat: &str) -> Split<&str>;
    fn split_whitespace(&self) -> SplitWhitespace;
    fn lines(&self) -> Lines;

    fn trim(&self) -> &str;
    fn trim_start(&self) -> &str;
    fn trim_end(&self) -> &str;

    fn to_lowercase(&self) -> str;
    fn to_uppercase(&self) -> str;
    fn repeat(&self, n: usize) -> str;

    fn parse<F: FromStr>(&self) -> Result<F, F::Err>;
}

/# String formatting
fn format(args: fmt::Arguments) -> str;

macro format! {
    /# String interpolation macro
    /# format!("Hello, {}!"str, name)
    /# format!("Value: {value}, Type: {type}"str)
}
```

### 6.8 std::json API Reference

```veil
/# JSON serialization and deserialization
enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(str),
    Array([Value]),
    Object(Map<str, Value>),
}

impl Value {
    fn is_null(&self) -> bool;
    fn is_bool(&self) -> bool;
    fn is_number(&self) -> bool;
    fn is_string(&self) -> bool;
    fn is_array(&self) -> bool;
    fn is_object(&self) -> bool;

    fn as_bool(&self) -> Option<bool>;
    fn as_f64(&self) -> Option<f64>;
    fn as_str(&self) -> Option<&str>;
    fn as_array(&self) -> Option<&[Value]>;
    fn as_object(&self) -> Option<&Map<string, Value>>;

    fn get(&self, key: &str) -> Option<&Value>;
    fn get_mut(&mut self, key: &str) -> Option<&mut Value>;
}

/# Serialization
trait Serialize {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error>;
}

trait Deserialize<'de> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error>;
}

/# JSON functions
fn to_string<T: Serialize>(value: &T) -> Result<str, Error>;
fn to_string_pretty<T: Serialize>(value: &T) -> Result<str, Error>;
fn to_vec<T: Serialize>(value: &T) -> Result<[u8], Error>;

fn from_str<T: Deserialize>(s: &str) -> Result<T, Error>;
fn from_slice<T: Deserialize>(v: &[u8]) -> Result<T, Error>;
fn from_reader<R: Read, T: Deserialize>(reader: R) -> Result<T, Error>;

/# Derive macros for automatic serialization
#[derive(Serialize, Deserialize)]
struct User {
    name: str,
    email: str,
    age: u32,
}
```

## 7. Ownership & References

### 7.1 Ownership and ARC

By default, variables are owning references to their data. For heap-allocated objects, this means the variable holds a "strong" reference that participates in Automatic Reference Counting.

- **Assignment**: `const new_ref = old_ref;` increments the object's reference count.

- **Scope Exit**: When an owning reference goes out of scope, the reference count is decremented.

### 7.2 Borrowed References (&T)

A borrowed reference, denoted by &T, is a temporary, non-owning pointer to a value. It allows functions to access data without taking ownership of it. This is a fundamental concept for writing efficient and correct Veil code.

- **Non-Owning**: Creating or passing a &T does not affect the ARC count. It is a lightweight pointer copy.

- **Non-Nullable**: References are guaranteed to point to a valid, allocated object. There is no "null reference." For optionality, you must use Option<&T> or, more commonly, an owning optional type T?.

- **Lifetime Enforcement**: The compiler uses the same generational analysis to ensure a reference does not outlive the data it points to. A function cannot return a reference to a locally defined variable, and a struct cannot store a reference to an object that may be deallocated before the struct is.

```veil
/# A function that accepts a borrowed reference
fn calculate_length(s: &str) -> int {
    /# We can read from `s` but we don't own it.
    /# No ARC traffic is generated here.
    return s.len();
}

fn main() {
    const my_string = "hello world".to_string(); /# Owning reference
    const length = calculate_length(&my_string); /# Pass a borrow
    print("Length is {length}");
}
} /# `my_string` goes out of scope, its memory is freed.
```

The use of &Mutex<int> in the concurrency examples indicates passing a non-owning, temporary reference to the Mutex itself, which is the correct and most efficient way to allow multiple tasks to access the same lock.

### 7.3 Weak References (weak T)

A weak reference, denoted by weak T, is a non-owning pointer that is used to break reference cycles.

**Syntax and API**:

- **Type**: weak T

- **Creation**: An object that supports weak references provides a .downgrade() method to create a weak T from an owning reference (T).

- **Access**: To safely access the underlying object, a weak reference must be upgraded back into a strong one using the .upgrade() method. This returns an Option<T> (Some(T) if the object still exists, None otherwise).

```veil
/# Example: Breaking a Parent-Child reference
```

cycle
struct Node {
parent: weak Node, /# Parent reference is weak
children: [Node], /# Child references are strong
}

fn main() {
const root = Node::new(parent: none);
const child = Node::new(parent: root.downgrade());
root.children.push(child);
} /# When `root` goes out of scope, its ARC count becomes 0 and it's freed.
/# Because the child's reference to the parent was weak, it did not
/# prevent the deallocation. The child is then also freed.

````
## 8. Operators and Precedence

This section defines the full operator set and their precedence/associativity. Unless stated otherwise, operators are left-associative. No implicit numeric coercions are performed; operands must be of the same type family where applicable.

**Highest to lowest precedence:**

1. **() [] {} . func()** index/slice, member access, function call
2. **\*\*** (exponentiation) — right-associative
3. **Unary**: +x, -x, ~x (bitwise not), !x (logical not), &x (borrow), \*x (dereference), await x, ++x, --x (integers only)
4. **Multiplicative**: \* / /# %
   - / requires floating-point operands; use /# for integer division
   - /# is floor division (a == (a /# b) \* b + (a % b))
5. **Additive**: + -
6. **Shifts**: << >>
7. **Bitwise AND**: &
8. **Bitwise XOR**: ^
9. **Bitwise OR**: |
10. **Comparisons**: < <= > >= == !=
    **Membership/identity**: in, not in, is, is not
    - in delegates to .contains for collections/strings
    - is compares reference identity for owning references; not allowed for plain value types
11. **Logical NOT**: !
12. **Logical AND**: &
13. **Logical OR**: |
14. **Pipeline**: |> (left-associative; lower than logical or, higher than assignment)
15. **Assignment**: = (right-associative to enable a = b = c)
    **Compound assignment**: += -= \*= /= /#= %= \*\*= <<= >>= &= ^= |=
16. **Commas** for tuple/array literals and function arguments

**Notes:**

- ++/-- are allowed only on integer types (i8..i64, u8..u64) and require a var binding. Using them on const bindings or non-integer types is a compile-time error.

- / requires float operands (f32 or f64). Use explicit as casts for conversions. Integer division must use /#.

- % follows Euclidean semantics: a == (a /# b) \* b + (a % b), and (a % b) has the sign of b.

- is/is not perform identity comparison on owning references (pointer identity under ARC). For value types, use ==/!=.

- in/not in work with strings and collections implementing Contains; users can implement Contains in std::collections for custom types.
- Logical operators (&, |, !) have lower precedence than bitwise operators to prevent confusion.

- |> applies the left expression as the last argument to the right function: x |> f is f(x); a |> b |> c is c(b(a)).

## 9. Comments and Documentation

### Comments

- **Line comments**: /# ... to end of line
- **Block comments**: /* ... */ can span multiple lines. Nested block comments are not allowed.

### Documentation

- **Item documentation** is written using block doc comments immediately preceding the item:

```veil
/**
 * # Title
 *
 * Some description...
 */
fn foo() { ... }
````

- Within documentation comments, Markdown headings (#, ##, ###) define sections and subsections. Content under a heading belongs to that section. If no headings are present, documentation is treated as a single section (Rust-like flat docgen).

- **Cross-references** use wiki-style linking:
  - [[]] for items within the same package
  - [[]] to link to items in another library; the generator resolves library names from ve.toml dependencies

- Inline code and fences are supported; links and images follow standard Markdown.

- **Docgen behavior**:
  - If sectioned docs are present, the generator preserves the hierarchy in the output navigation.
  - If not, it falls back to Rust-like docgen that attaches the entire comment to the item.
  - Examples inside docs are runnable tests when wrapped in ```test fences (honored by ve test).

## 10. Lexical Structure

### 10.1 Character Set

- Source files are UTF-8 encoded.

### 10.2 Keywords

The following are reserved keywords:
as, break, constructor, continue, else, enum, export,
false, fn, for, foreign, from, if, impl, import, in,
loop, match, new, none, rawptr, return, step, struct, test,
true, var, const, while, async, await, spawn, weak, comptime,
trait, where, move, unsafe, union, dyn, Self, super, crate,
pub, use, type, try, yield

**Notes:**

- const and var are used for bindings (immutability and mutability). let is not a keyword.
- Logical operators are symbols: !, &, | (operators, not keywords).

### 10.3 Identifiers

```ebnf
identifier ::= letter (letter | digit | '_')*
letter     ::= 'a'..'z' | 'A'..'Z'
digit      ::= '0'..'9'
```

### 10.4 Literals

- **Integer**: decimal, hex (0x...), binary (0b...), octal (0o...) with optional underscores for readability
- **Float**: digits '.' digits with optional exponent (e/E)
- **String**: "..." with escape sequences (\n, \r, \t, \\, \", \')
- **Template string**: `...` with {expression} interpolation (delimited by backticks, not single quotes; interpolation is not supported in double-quoted strings)
- **Boolean**: true | false
- **None**: none

### 10.5 Operators and Punctuation

**(summary, see Operators and Precedence section)**

- **Arithmetic**: + - \* / /# % \*\*
- **Bitwise**: ~ & | ^
- **Assignment**: = with compound ops (+=, -=, \*=, /=, /#=, %=, \*\*=, <<=, >>=, &=, ^=, |=)
- **Comparison**: == != > < >= <=
- **Logical**: ! & |
- **Access**: . [] ()
- **Range**: .. ..= ..> ..<
- **Pointer/Deref**: \*
- **Borrow**: &
- **Cast**: as
- **Optional**: ?
- **Pipeline**: |>
- **Delimiters**: ( ) [ ] { }
- **Separators**: , ; :
- **Arrows**: -> =>

## 11. Grammar and Syntax

### 11.1 Program Structure (EBNF)

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

### 11.2 Import/Export System (EBNF)

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

### 11.3 Foreign Function Interface (FFI) (EBNF)

```ebnf
ffi_declaration ::= ('#[' metadata ']')? 'foreign' ffi_item

ffi_item ::= 'fn' identifier '(' parameter_list? ')' ('->' type)? ';'
           | 'var' identifier ':' type ';'

metadata ::= metadata_item (',' metadata_item)*
metadata_item ::= identifier ('=' metadata_value)?
metadata_value ::= string_literal | identifier | bool_literal
```

## 12. Types and Values

### 12.1 Primitive Types

- **Integers**:
  - **Signed**: i8 (1 byte: -128 to 127), i16 (2 bytes: -32,768 to 32,767), i32 (4 bytes: -2³¹ to 2³¹-1), i64 (8 bytes: -2⁶³ to 2⁶³-1), i128 (16 bytes: -2¹²⁷ to 2¹²⁷-1)
  - **Unsigned**: u8 (1 byte: 0 to 255), u16 (2 bytes: 0 to 65,535), u32 (4 bytes: 0 to 2³²-1), u64 (8 bytes: 0 to 2⁶⁴-1), u128 (16 bytes: 0 to 2¹²⁸-1)
  - **Architecture-dependent**: isize and usize (pointer-sized: 32-bit or 64-bit depending on target architecture)

- **Floating-point**: f32 (IEEE 754 single precision, 4 bytes), f64 (IEEE 754 double precision, 8 bytes)

- **Boolean**: bool (1 byte, values: true, false)

- **Character**: ch (4 bytes, Unicode scalar value, UTF-32 encoding)

- **String**: str (UTF-8 encoded, heap-allocated, growable)

- **Special Types**:
  - **void**: Zero-sized type, used for functions that don't return a value
  - **any**: Dynamic type container with runtime type information, similar to `void*` with RTTI. Supports explicit downcasting with runtime checks
  - **rawptr**: Raw pointer type for unsafe operations and FFI

### 12.2 Composite Types

- **Arrays**: [T; N] (fixed), [T] (dynamic), [] (empty literal)
- **Optional**: T?
- **Pointer**: \*T
- **Borrowed**: &T (non-owning, non-null)
- **Generics**: Type<T>, Option<T>

### 12.3 User-Defined Types

```veil
struct Point {
    x: f64,
    y: f64,
}

enum Color {
    Red,
    Green,
    Blue,
    RGB(u8, u8, u8),
}
```

## 13. Expressions

### 13.1 Literal Expressions

```veil
42
3.14
"hello"
`Hello {name}!`
true
none
[]
```

### 13.2 Arithmetic

```veil
a + b
a - b
a * b
a / b /# float division
a /# b /# integer floor division
a % b
a ** b
```

### 13.3 Comparison

```veil
a == b
a != b
a > b
a < b
a >= b
a <= b
```

### 13.4 Logical

```veil
a & b
a | b
!a
```

### 13.5 Assignment

```veil
a = b
```

### 13.6 Access

```veil
obj.field
obj.method()
array[index]
```

### 13.7 Ranges

```veil
1..10
1..=10
..>10
..<10
..
```

### 13.8 Constructors

```veil
new Point(1.0, 2.0)
Point { x: 1.0, y: 2.0 }
[1, 2, 3, 4]
```

### 13.9 Cast

```veil
value as i32
```

### 13.10 Unary

```veil
-value
+value
!value
*pointer
...spread
```

### 13.11 Control Flow Expressions

```veil
if condition { value1 } else { value2 }

match value {
    pattern1 => result1,
    pattern2 if guard => result2,
    _ => default_result,
}

loop {
    /# infinite loop body
}

/# Labeled loops
'outer: for i in 0..10 {
    'inner: for j in 0..10 {
        if condition { break 'outer; }
        if other { continue 'inner; }
    }
}

/# Try expressions
const result = try {
    const a = risky_operation()?;
    const b = another_operation(a)?;
    b * 2
};

/# Async blocks
const future = async {
    const data = await fetch_data();
    process(data)
};
```

## 14. Statements

### 14.1 Variable Declarations (EBNF)

```ebnf
variable_declaration ::= (const_declaration | var_declaration) ';'
const_declaration    ::= 'const' identifier (':' type)? '=' expression
var_declaration      ::= 'var' ('mut')? identifier (':' type)? '=' expression
```

**Examples:**

```veil
const PI = 3.14159;                    // Compile-time constant
const MAX_USERS: i32 = 1000;          // Typed constant
var name = "Alice";                    // Immutable variable
var age: i32 = 25;                     // Typed immutable variable
var mut counter = 0;                   // Mutable variable
var mut buffer: [u8; 1024] = [0; 1024]; // Typed mutable variable
```

**Semantics:**

- `const` declarations create compile-time constants. The value must be computable at compile time.
- `var` declarations create immutable variables. The value is computed at runtime but cannot be modified.
- `var mut` declarations create mutable variables that can be modified after initialization.

### 14.2 Expression Statements

```ebnf
expression_statement ::= expression ';'?
```

### 14.3 Control Flow Statements

```ebnf
if_statement ::= 'if' expression block ('else' (if_statement | block))?

while_statement ::= 'while' expression block

for_statement ::= 'for' for_pattern 'in' expression ('step' expression)? block
for_pattern   ::= identifier (',' identifier)?

loop_statement ::= label? 'loop' block

match_statement ::= 'match' expression '{' match_arm* '}'
match_arm       ::= pattern ('if' expression)? '=>' (expression | block) ','?

try_statement ::= 'try' block ('catch' pattern block)*

label ::= identifier ':'
```

### 14.4 Jump Statements

```ebnf
return_statement   ::= 'return' expression? ';'
break_statement    ::= 'break' label? expression? ';'
continue_statement ::= 'continue' label? ';'
yield_statement    ::= 'yield' expression ';'
```

## 15. Declarations

### 15.1 Functions (EBNF)

```ebnf
function_declaration ::= 'fn' identifier generic_params? parameter_list ('->' type)? block

generic_params ::= '<' identifier (',' identifier)* '>'
parameter_list ::= '(' parameter (',' parameter)* ')'
parameter      ::= identifier ':' type | '...' identifier? ':' type | '...'
```

**Examples:**

```veil
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn generic_func<T>(value: T) -> T {
    value
}

fn variadic_func(...args: any) {
    /# handle variadic arguments
}
```

### 15.2 Structs (EBNF)

```ebnf
struct_declaration ::= visibility? 'struct' identifier generic_params? struct_body
struct_body        ::= '{' struct_field* '}' | '(' tuple_field* ')' | ';'
struct_field       ::= visibility? identifier ':' type ','?
tuple_field        ::= visibility? type ','?
visibility         ::= 'pub' ('(' visibility_restriction ')')?
visibility_restriction ::= 'crate' | 'super' | 'in' module_path
```

### 15.3 Enums (EBNF)

```ebnf
enum_declaration ::= 'enum' identifier generic_params? '{' enum_variant* '}'
enum_variant     ::= identifier enum_data? ('=' integer_literal)? ','?
enum_data        ::= '(' type (',' type)* ')'
```

### 15.4 Impl Blocks (EBNF)

```ebnf
impl_block ::= 'impl' generic_params? type '{' impl_item* '}'
            | 'impl' generic_params? trait_bound 'for' type where_clause? '{' impl_item* '}'

impl_item  ::= function_declaration
            | constructor_declaration
            | associated_type
            | associated_const

constructor_declaration ::= 'constructor' parameter_list '->' type block
associated_type ::= 'type' identifier generic_params? where_clause? '=' type ';'
associated_const ::= 'const' identifier ':' type '=' expression ';'

trait_declaration ::= visibility? 'trait' identifier generic_params? trait_bounds? where_clause? '{' trait_item* '}'
trait_item ::= function_signature | associated_type_decl | associated_const_decl | function_declaration
function_signature ::= 'fn' identifier generic_params? parameter_list ('->' type)? where_clause? ';'
associated_type_decl ::= 'type' identifier generic_params? trait_bounds? where_clause? ';'
associated_const_decl ::= 'const' identifier ':' type ';'

where_clause ::= 'where' where_predicate (',' where_predicate)*
where_predicate ::= type ':' trait_bounds
trait_bounds ::= trait_bound ('+' trait_bound)*
trait_bound ::= type | '?' type
```

## 16. Pattern Matching

Pattern matching in Veil provides a powerful and expressive way to destructure data and control program flow. The compiler enforces exhaustiveness checking to ensure all possible cases are handled.

### 16.1 Basic Patterns

#### 16.1.1 Literal Patterns

```veil
match value {
    0 => "zero",
    1 => "one",
    2 => "two",
    42 => "the answer",
    _ => "something else",  /# Wildcard pattern
}

/# String literal patterns
match text {
    "hello" => greet(),
    "goodbye" => farewell(),
    "" => handle_empty(),
    _ => handle_other(),
}

/# Boolean patterns
match flag {
    true => enable_feature(),
    false => disable_feature(),
}
```

#### 16.1.2 Variable Patterns and Binding

```veil
match result {
    Ok(value) => {
        /# 'value' is bound to the success value
        process_success(value)
    },
    Err(error) => {
        /# 'error' is bound to the error value
        handle_error(error)
    },
}

/# Multiple bindings in tuple patterns
match coordinates {
    (x, y) => {
        print("Point at ({}, {})", x, y);
    },
}

/# Nested patterns with bindings
match nested_result {
    Ok(Some(data)) => process_data(data),
    Ok(None) => handle_empty(),
    Err(error) => log_error(error),
}
```

#### 16.1.3 Wildcard and Placeholder Patterns

```veil
match tuple {
    (x, _) => x,  /# Ignore second element
    _ => panic("unreachable"),
}

/# Multiple wildcards
match complex_tuple {
    (first, _, _, last) => (first, last),  /# Only care about first and last
}

/# Placeholder with binding for debugging
match result {
    Ok(value @ _) => {  /# Bind value but indicate we might not use it
        debug_print("Got value: {:?}", value);
        value
    },
    Err(_) => default_value(),
}
```

### 16.2 Enum and Struct Patterns

#### 16.2.1 Enum Destructuring

```veil
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(string),
    ChangeColor(i32, i32, i32),
}

match msg {
    Message::Quit => {
        print("Quit message received");
        shutdown()
    },
    Message::Move { x, y } => {
        print("Move to ({}, {})", x, y);
        move_cursor(x, y)
    },
    Message::Write(text) => {
        print("Text message: {}", text);
        display_text(text)
    },
    Message::ChangeColor(r, g, b) => {
        print("Change color to RGB({}, {}, {})", r, g, b);
        set_color(r, g, b)
    },
}

/# Partial destructuring with wildcards
match msg {
    Message::Move { x, y: _ } => handle_x_movement(x),  /# Ignore y
    Message::ChangeColor(r, _, _) => handle_red_component(r),  /# Ignore g, b
    _ => handle_other_messages(),
}
```

#### 16.2.2 Struct Patterns

```veil
struct Point { x: f64, y: f64 }
struct Person { name: str, age: u32, active: bool }

match point {
    Point { x: 0.0, y: 0.0 } => "origin",
    Point { x: 0.0, y } => format("on y-axis at {}", y),
    Point { x, y: 0.0 } => format("on x-axis at {}", x),
    Point { x, y } => format("point at ({}, {})", x, y),
}

/# Shorthand field binding
match person {
    Person { name, age: 18..=65, active: true } => {
        /# Destructure with range pattern and literal
        welcome_adult_user(name)
    },
    Person { age, .. } if age < 18 => {
        /# Rest pattern (..) ignores remaining fields
        handle_minor()
    },
    Person { active: false, .. } => handle_inactive_user(),
    Person { name, .. } => handle_other_user(name),
}
```

### 16.3 Advanced Patterns

#### 16.3.1 Range Patterns

```veil
match age {
    0..=12 => "child",
    13..=19 => "teenager",
    20..=64 => "adult",
    65.. => "senior",  /# Open-ended range
}

/# Character ranges
match ch {
    'a'..='z' => "lowercase letter",
    'A'..='Z' => "uppercase letter",
    '0'..='9' => "digit",
    _ => "other character",
}

/# Multiple ranges
match score {
    90..=100 | 85..=89 => "excellent",  /# Multiple patterns with |
    70..=84 => "good",
    60..=69 => "passing",
    0..=59 => "failing",
}
```

#### 16.3.2 Array and Slice Patterns

```veil
match array {
    [] => "empty array",
    [single] => format("single element: {}", single),
    [first, second] => format("two elements: {}, {}", first, second),
    [first, .., last] => format("first: {}, last: {}", first, last),
    [first, middle @ .., last] => {
        /# Bind middle slice
        format("first: {}, middle: {:?}, last: {}", first, middle, last)
    },
}

/# Fixed-size array patterns
match fixed_array {
    [1, 2, 3] => "the sequence 1, 2, 3",
    [x, y, z] if x + y == z => "sum pattern",
    [a, b, c] => format("array with {}, {}, {}", a, b, c),
}

/# Head/tail patterns for lists
match list {
    [] => process_empty(),
    [head, ..tail] => {
        process_element(head);
        process_rest(tail)
    },
}
```

#### 16.3.3 Reference and Dereference Patterns

```veil
match &optional_value {
    Some(ref inner) => process_borrowed(inner),  /# Borrow inner value
    None => handle_none(),
}

/# Dereference patterns
match boxed_value {
    box Some(value) => process_unboxed(value),
    box None => handle_boxed_none(),
}

/# Reference patterns in function parameters
fn process_option(opt: &Option<i32>) {
    match opt {
        Some(value) => print("Value: {}", value),  /# Automatic dereference
        None => print("No value"),
    }
}
```

#### 16.3.4 Guard Expressions

```veil
match point {
    Point { x, y } if x == y => "diagonal",
    Point { x, y } if x > 0.0 && y > 0.0 => "first quadrant",
    Point { x, y } if x < 0.0 && y > 0.0 => "second quadrant",
    Point { x, y } if x < 0.0 && y < 0.0 => "third quadrant",
    Point { x, y } if x > 0.0 && y < 0.0 => "fourth quadrant",
    Point { x: 0.0, y } => format("on y-axis: {}", y),
    Point { x, y: 0.0 } => format("on x-axis: {}", x),
    Point { x: 0.0, y: 0.0 } => "origin",
}

/# Complex guards with function calls
match user {
    User { name, age, .. } if is_admin(&name) => grant_admin_access(),
    User { age, .. } if age >= 18 && has_permission() => grant_user_access(),
    _ => deny_access(),
}

/# Guards with bound variables
match number {
    n if n % 2 == 0 && n > 10 => "large even number",
    n if n % 2 == 1 && n < 0 => "negative odd number",
    n => format("other number: {}", n),
}
```

### 16.4 Pattern Matching in Different Contexts

#### 16.4.1 If-Let Expressions

```veil
/# Convenient for single-pattern matching
if let Some(value) = optional {
    process_value(value);
} else {
    handle_none();
}

/# Chaining if-let expressions
if let Ok(data) = parse_data(input) {
    if let Some(processed) = process_data(data) {
        return Ok(processed);
    }
}

/# If-let with guards
if let Some(x) = maybe_number && x > 0 {
    print("Positive number: {}", x);
}
```

#### 16.4.2 While-Let Loops

```veil
/# Process iterator until None
while let Some(item) = iterator.next() {
    process_item(item);
}

/# Process results until error
while let Ok(data) = receive_data() {
    handle_data(data);
}

/# Complex patterns in while-let
while let Ok(Message::Data(payload)) = channel.recv() {
    process_payload(payload);
}
```

#### 16.4.3 Function Parameters and Let Bindings

```veil
/# Destructuring in function parameters
fn distance((x1, y1): (f64, f64), (x2, y2): (f64, f64)) -> f64 {
    ((x2 - x1).powi(2) + (y2 - y1).powi(2)).sqrt()
}

/# Destructuring in let bindings
const Point { x, y } = get_point();
const (first, second, ..rest) = get_tuple();
const [head, ..tail] = get_array();

/# Nested destructuring
const User {
    name,
    address: Address { street, city, .. },
    ..
} = get_user();
```

#### 16.4.4 For Loop Patterns

```veil
/# Destructuring in for loops
for (index, value) in array.iter().enumerate() {
    print("Item {}: {}", index, value);
}

/# Destructuring complex structures
for User { name, age, .. } in users {
    if age >= 18 {
        print("Adult user: {}", name);
    }
}

/# Pattern matching in for loop filters
for Ok(data) in results.iter().filter_map(|r| r.as_ref().ok()) {
    process_data(data);
}
```

### 16.5 Exhaustiveness and Unreachable Patterns

#### 16.5.1 Exhaustiveness Checking

```veil
enum Status { Active, Inactive, Pending }

/# Compiler ensures all variants are covered
match status {
    Status::Active => handle_active(),
    Status::Inactive => handle_inactive(),
    Status::Pending => handle_pending(),
    /# No wildcard needed - all cases covered
}

/# Exhaustiveness with nested patterns
match result {
    Ok(Some(data)) => process_data(data),
    Ok(None) => handle_empty(),
    Err(error) => handle_error(error),
    /# Compiler verifies all Result<Option<T>, E> cases are handled
}
```

#### 16.5.2 Unreachable Pattern Detection

```veil
match number {
    1..=10 => "small",
    5..=15 => "medium",  /# Warning: overlaps with previous pattern
    11..=20 => "large",  /# Warning: some cases already handled
    _ => "other",
}

/# Compiler detects unreachable patterns
match value {
    x if x > 0 => "positive",
    x if x < 0 => "negative",
    0 => "zero",
    _ => "unreachable",  /# Warning: all cases already handled
}
```

### 16.6 Pattern Matching Performance

#### 16.6.1 Optimization Strategies

```veil
/# Compiler optimizes common patterns efficiently
match discriminant {
    0 => action_0(),     /# Jump table optimization
    1 => action_1(),
    2 => action_2(),
    3 => action_3(),
    _ => default_action(),
}

/# Decision tree optimization for complex patterns
match (x, y) {
    (0, 0) => origin(),
    (0, _) => y_axis(),
    (_, 0) => x_axis(),
    (a, b) if a == b => diagonal(),
    _ => general_case(),
}

/# Efficient string matching
match command {
    "quit" | "exit" => quit(),      /# Optimized string comparison
    "help" | "?" => show_help(),
    "save" => save_file(),
    cmd if cmd.starts_with("load ") => load_file(&cmd[5..]),
    _ => unknown_command(),
}
```

#### 16.6.2 Performance Best Practices

```veil
/# Order patterns by frequency (most common first)
match request_type {
    RequestType::Get => handle_get(),        /# Most common
    RequestType::Post => handle_post(),      /# Second most common
    RequestType::Put => handle_put(),        /# Less common
    RequestType::Delete => handle_delete(),  /# Least common
}

/# Use guards sparingly for performance-critical code
match value {
    /# Prefer specific patterns over guards when possible
    0 => handle_zero(),
    1..=100 => handle_small(),
    101..=1000 => handle_medium(),
    _ => handle_large(),
}

/# Rather than:
/# match value {
/#     n if n == 0 => handle_zero(),
/#     n if n <= 100 => handle_small(),
/#     n if n <= 1000 => handle_medium(),
/#     _ => handle_large(),
/# }
```

## 17. Examples

### 17.1 Hello World

```veil
import std/io;

fn main() {
    print("Hello, World!");
}
```

### 17.2 Factorial

```veil
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

### 17.3 Generic Stack

```veil
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

### 17.4 Error Handling Example

```veil
enum AppError {
    NetworkError(string),
    ParseError { line: u32, message: string },
    ValidationError(ValidationError),
}

impl Error for AppError {
    fn message(self) -> string {
        match self {
            AppError.NetworkError(msg) => `Network error: {msg}`,
            AppError.ParseError { line, message } => `Parse error at line {line}: {message}`,
            AppError.ValidationError(err) => err.message(),
        }
    }
}

fn process_user_data(json: str) -> Result<User, AppError> {
    const data = json::parse(json)
        .map_err(|e| AppError.ParseError { line: e.line, message: e.message })?;

    const user = User::from_json(data)
        .map_err(AppError.ValidationError)?;

    /# Validate user data
    if user.email.is_empty() {
        return Err(AppError.ValidationError(
            ValidationError::new("Email cannot be empty")
        ));
    }

    Ok(user)
}

/# Usage with error handling
fn main() -> Result<(), AppError> {
    const json_data = read_file("user.json")?;
    const user = process_user_data(json_data)?;

    print(`Processed user: {user.name}`);
    Ok(())
}
```

### 17.5 Async Web Server

```veil
import std::net::{TcpListener, TcpStream};
import std::io::{AsyncRead, AsyncWrite};
import std::collections::HashMap;

struct HttpRequest {
    method: str,
    path: str,
    headers: HashMap<str, str>,
    body: [u8],
}

struct HttpResponse {
    status: u16,
    headers: HashMap<str, str>,
    body: [u8],
}

impl HttpResponse {
    fn new(status: u16, body: &str) -> HttpResponse {
        var headers = HashMap::new();
        headers.insert("Content-Type".to_string(), "text/plain".to_string());
        headers.insert("Content-Length".to_string(), body.len().to_string());

        HttpResponse {
            status,
            headers,
            body: body.as_bytes().to_vec(),
        }
    }

    async fn send(&self, stream: &mut TcpStream) -> io::Result<()> {
        const status_line = format!("HTTP/1.1 {} OK\r\n", self.status);
        stream.write_all(status_line.as_bytes()).await?;

        for (key, value) in &self.headers {
            const header = format!("{}: {}\r\n", key, value);
            stream.write_all(header.as_bytes()).await?;
        }

        stream.write_all(b"\r\n").await?;
        stream.write_all(&self.body).await?;
        stream.flush().await?;

        Ok(())
    }
}

async fn handle_request(mut stream: TcpStream) -> io::Result<()> {
    const mut buffer = [0u8; 4096];
    const n = stream.read(&mut buffer).await?;

    const request_str = std::str::from_utf8(&buffer[..n])
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8"))?;

    const response = match parse_request(request_str) {
        Ok(request) => route_request(request).await,
        Err(_) => HttpResponse::new(400, "Bad Request"),
    };

    response.send(&mut stream).await?;
    Ok(())
}

async fn route_request(request: HttpRequest) -> HttpResponse {
    match (request.method.as_str(), request.path.as_str()) {
        ("GET", "/") => HttpResponse::new(200, "Welcome to Veil Web Server!"),
        ("GET", "/api/health") => HttpResponse::new(200, "OK"),
        ("POST", "/api/users") => create_user(request).await,
        ("GET", path) if path.starts_with("/api/users/") => {
            const user_id = &path[11..];  /# Extract user ID
            get_user(user_id).await
        },
        _ => HttpResponse::new(404, "Not Found"),
    }
}

async fn create_user(request: HttpRequest) -> HttpResponse {
    /# Parse JSON from request body
    match json::from_slice::<User>(&request.body) {
        Ok(user) => {
            /# Save user to database
            match save_user_to_db(user).await {
                Ok(saved_user) => {
                    const response_body = json::to_string(&saved_user).unwrap();
                    HttpResponse::new(201, &response_body)
                },
                Err(_) => HttpResponse::new(500, "Internal Server Error"),
            }
        },
        Err(_) => HttpResponse::new(400, "Invalid JSON"),
    }
}

async fn get_user(user_id: &str) -> HttpResponse {
    match user_id.parse::<u64>() {
        Ok(id) => {
            match fetch_user_from_db(id).await {
                Some(user) => {
                    const response_body = json::to_string(&user).unwrap();
                    HttpResponse::new(200, &response_body)
                },
                None => HttpResponse::new(404, "User not found"),
            }
        },
        Err(_) => HttpResponse::new(400, "Invalid user ID"),
    }
}

async fn main() -> io::Result<()> {
    const listener = TcpListener::bind("127.0.0.1:8080").await?;
    println!("Server running on http:/#127.0.0.1:8080");

    loop {
        const (stream, addr) = listener.accept().await?;
        println!("New connection from: {}", addr);

        /# Handle each connection concurrently
        spawn {
            if let Err(e) = handle_request(stream).await {
                eprintln!("Error handling request: {}", e);
            }
        };
    }
}
```

### 17.6 Concurrent Data Processing Pipeline

```veil
import std::sync::{Mutex, Arc, channel};
import std::fs::File;
import std::io::{AsyncRead, BufReader};

struct DataItem {
    id: u64,
    content: str,
    timestamp: SystemTime,
}

struct ProcessedItem {
    id: u64,
    result: str,
    processing_time: Duration,
}

struct ProcessingStats {
    total_processed: Arc<Mutex<u64>>,
    total_errors: Arc<Mutex<u64>>,
    start_time: Instant,
}

impl ProcessingStats {
    fn new() -> Self {
        ProcessingStats {
            total_processed: Arc::new(Mutex::new(0)),
            total_errors: Arc::new(Mutex::new(0)),
            start_time: Instant::now(),
        }
    }

    async fn increment_processed(&self) {
        *self.total_processed.lock().await += 1;
    }

    async fn increment_errors(&self) {
        *self.total_errors.lock().await += 1;
    }

    async fn print_stats(&self) {
        const processed = *self.total_processed.lock().await;
        const errors = *self.total_errors.lock().await;
        const elapsed = self.start_time.elapsed();

        println!("Processed: {}, Errors: {}, Time: {:?}, Rate: {:.2}/sec",
                processed, errors, elapsed, processed as f64 / elapsed.as_secs_f64());
    }
}

/# Data ingestion stage
async fn data_ingester(
    file_path: &str,
    tx: channel::Sender<DataItem>
) -> Result<(), Box<dyn Error>> {
    const file = File::open(file_path).await?;
    const mut reader = BufReader::new(file);
    const mut line = String::new();
    var id = 0u64;

    while reader.read_line(&mut line).await? > 0 {
        const item = DataItem {
            id,
            content: line.trim().to_string(),
            timestamp: SystemTime::now(),
        };

        tx.send(item).await?;
        id += 1;
        line.clear();
    }

    /# Signal end of input
    drop(tx);
    Ok(())
}

/# Processing stage
async fn data_processor(
    rx: channel::Receiver<DataItem>,
    tx: channel::Sender<ProcessedItem>,
    stats: Arc<ProcessingStats>
) {
    while let Some(item) = rx.recv().await {
        const start_time = Instant::now();

        match process_item(&item).await {
            Ok(result) => {
                const processed = ProcessedItem {
                    id: item.id,
                    result,
                    processing_time: start_time.elapsed(),
                };

                if tx.send(processed).await.is_ok() {
                    stats.increment_processed().await;
                }
            },
            Err(e) => {
                eprintln!("Error processing item {}: {}", item.id, e);
                stats.increment_errors().await;
            },
        }
    }

    drop(tx);
}

async fn process_item(item: &DataItem) -> Result<str, ProcessingError> {
    /# Simulate complex processing
    time::sleep(Duration::from_millis(10)).await;

    if item.content.is_empty() {
        return Err(ProcessingError::EmptyContent);
    }

    /# Transform the data
    const processed = item.content
        .to_uppercase()
        .chars()
        .rev()
        .collect::<String>();

    Ok(format!("Processed: {}", processed))
}

/# Output stage
async fn result_writer(
    rx: channel::Receiver<ProcessedItem>,
    output_path: &str
) -> Result<(), Box<dyn Error>> {
    const mut file = File::create(output_path).await?;
    const mut writer = BufWriter::new(file);

    while let Some(item) = rx.recv().await {
        const line = format!("{},{},{:?}\n",
                           item.id, item.result, item.processing_time);
        writer.write_all(line.as_bytes()).await?;
    }

    writer.flush().await?;
    Ok(())
}

async fn main() -> Result<(), Box<dyn Error>> {
    const num_processors = num_cpus::get();
    const stats = Arc::new(ProcessingStats::new());

    /# Create channels for pipeline stages
    const (input_tx, input_rx) = channel::bounded::<DataItem>(1000);
    const (output_tx, output_rx) = channel::bounded::<ProcessedItem>(1000);

    /# Start data ingestion
    const ingester_handle = spawn {
            data_ingester("input.txt", input_tx).await
        };

    /# Start multiple processors
    const processor_handles: Vec<_> = (0..num_processors)
        .map(|i| {
            const rx_clone = input_rx.clone();
            const tx_clone = output_tx.clone();
            const stats_clone = stats.clone();

            spawn {
                println!("Starting processor {}", i);
                data_processor(rx_clone, tx_clone, stats_clone).await
            }
        })
        .collect();

    /# Start result writer
    const writer_handle = spawn {
            result_writer(output_rx, "output.csv").await
        };

    /# Start stats reporting
    const stats_clone = stats.clone();
    const stats_handle = spawn {
        loop {
            time::sleep(Duration::from_secs(5)).await;
            stats_clone.print_stats().await;
        }
    };

    /# Wait for ingestion to complete
    ingester_handle.await?;
    println!("Data ingestion completed");

    /# Close input receiver to signal processors
    drop(input_rx);

    /# Wait for all processors to complete
    for handle in processor_handles {
        handle.await;
    }
    println!("Data processing completed");

    /# Close output sender to signal writer
    drop(output_tx);

    /# Wait for writer to complete
    writer_handle.await?;
    println!("Output writing completed");

    /# Print final stats
    stats.print_stats().await;

    Ok(())
}
```

### 17.7 Generic Data Structures

```veil
/# Generic tree structure with iterator support
struct Tree<T> {
    root: Option<Box<TreeNode<T>>>,
    size: usize,
}

struct TreeNode<T> {
    value: T,
    children: Vec<Box<TreeNode<T>>>,
}

impl<T> Tree<T> {
    fn new() -> Self {
        Tree { root: None, size: 0 }
    }

    fn insert_root(&mut self, value: T) -> Result<(), TreeError> {
        if self.root.is_some() {
            return Err(TreeError::RootExists);
        }

        self.root = Some(Box::new(TreeNode {
            value,
            children: Vec::new(),
        }));
        self.size = 1;

        Ok(())
    }

    fn add_child(&mut self, parent_path: &[usize], value: T) -> Result<(), TreeError> {
        const parent = self.find_node_mut(parent_path)?;
        parent.children.push(Box::new(TreeNode {
            value,
            children: Vec::new(),
        }));
        self.size += 1;

        Ok(())
    }

    fn find_node_mut(&mut self, path: &[usize]) -> Result<&mut TreeNode<T>, TreeError> {
        const root = self.root.as_mut().ok_or(TreeError::EmptyTree)?;

        var current = root.as_mut();
        for &index in path {
            if index >= current.children.len() {
                return Err(TreeError::InvalidPath);
            }
            current = current.children[index].as_mut();
        }

        Ok(current)
    }

    fn iter(&self) -> TreeIterator<T> {
        TreeIterator::new(self.root.as_ref())
    }

    fn iter_dfs(&self) -> DepthFirstIterator<T> {
        DepthFirstIterator::new(self.root.as_ref())
    }

    fn iter_bfs(&self) -> BreadthFirstIterator<T> {
        BreadthFirstIterator::new(self.root.as_ref())
    }
}

/# Depth-first iterator
struct DepthFirstIterator<T> {
    stack: Vec<&TreeNode<T>>,
}

impl<T> DepthFirstIterator<T> {
    fn new(root: Option<&TreeNode<T>>) -> Self {
        var stack = Vec::new();
        if let Some(node) = root {
            stack.push(node);
        }
        DepthFirstIterator { stack }
    }
}

impl<T> Iterator for DepthFirstIterator<T> {
    type Item = &T;

    fn next(&mut self) -> Option<&T> {
        const node = self.stack.pop()?;

        /# Add children in reverse order for correct DFS traversal
        for child in node.children.iter().rev() {
            self.stack.push(child);
        }

        Some(&node.value)
    }
}

/# Breadth-first iterator
struct BreadthFirstIterator<T> {
    queue: VecDeque<&TreeNode<T>>,
}

impl<T> BreadthFirstIterator<T> {
    fn new(root: Option<&TreeNode<T>>) -> Self {
        var queue = VecDeque::new();
        if let Some(node) = root {
            queue.push_back(node);
        }
        BreadthFirstIterator { queue }
    }
}

impl<T> Iterator for BreadthFirstIterator<T> {
    type Item = &T;

    fn next(&mut self) -> Option<&T> {
        const node = self.queue.pop_front()?;

        /# Add all children to the queue
        for child in &node.children {
            self.queue.push_back(child);
        }

        Some(&node.value)
    }
}

/# Cache implementation with LRU eviction
struct LruCache<K, V> {
    cache: HashMap<K, Box<CacheNode<K, V>>>,
    head: *mut CacheNode<K, V>,
    tail: *mut CacheNode<K, V>,
    capacity: usize,
}

struct CacheNode<K, V> {
    key: K,
    value: V,
    prev: *mut CacheNode<K, V>,
    next: *mut CacheNode<K, V>,
}

impl<K: Hash + Eq + Clone, V> LruCache<K, V> {
    fn new(capacity: usize) -> Self {
        assert!(capacity > 0, "Cache capacity must be greater than 0");

        /# Create dummy head and tail nodes
        const head = Box::into_raw(Box::new(CacheNode {
            key: unsafe { std::mem::zeroed() },
            value: unsafe { std::mem::zeroed() },
            prev: std::ptr::null_mut(),
            next: std::ptr::null_mut(),
        }));

        const tail = Box::into_raw(Box::new(CacheNode {
            key: unsafe { std::mem::zeroed() },
            value: unsafe { std::mem::zeroed() },
            prev: head,
            next: std::ptr::null_mut(),
        }));

        unsafe {
            (*head).next = tail;
        }

        LruCache {
            cache: HashMap::new(),
            head,
            tail,
            capacity,
        }
    }

    fn get(&mut self, key: &K) -> Option<&V> {
        if let Some(node_ptr) = self.cache.get(key) {
            const node = node_ptr.as_ref();

            /# Move to front
            unsafe {
                self.remove_node(node_ptr.as_ref());
                self.add_to_front(node_ptr.as_ref());
            }

            Some(&node.value)
        } else {
            None
        }
    }

    fn put(&mut self, key: K, value: V) {
        if let Some(existing) = self.cache.get_mut(&key) {
            /# Update existing node
            existing.value = value;
            unsafe {
                self.remove_node(existing.as_ref());
                self.add_to_front(existing.as_ref());
            }
        } else {
            /# Add new node
            const new_node = Box::new(CacheNode {
                key: key.clone(),
                value,
                prev: std::ptr::null_mut(),
                next: std::ptr::null_mut(),
            });

            const node_ptr = Box::into_raw(new_node);
            self.cache.insert(key, unsafe { Box::from_raw(node_ptr) });

            unsafe {
                self.add_to_front(&*node_ptr);
            }

            /# Check capacity
            if self.cache.len() > self.capacity {
                self.evict_lru();
            }
        }
    }

    unsafe fn remove_node(&mut self, node: &CacheNode<K, V>) {
        (*node.prev).next = node.next;
        (*node.next).prev = node.prev;
    }

    unsafe fn add_to_front(&mut self, node: &CacheNode<K, V>) {
        const first = (*self.head).next;

        node.prev = self.head;
        node.next = first;
        (*self.head).next = node;
        (*first).prev = node;
    }

    fn evict_lru(&mut self) {
        unsafe {
            const lru = (*self.tail).prev;
            self.remove_node(&*lru);

            const key = (*lru).key.clone();
            self.cache.remove(&key);
            Box::from_raw(lru); /# Deallocate
        }
    }
}

/# Example usage
fn main() {
    /# Tree example
    var tree = Tree::new();
    tree.insert_root("root").unwrap();
    tree.add_child(&[], "child1").unwrap();
    tree.add_child(&[], "child2").unwrap();
    tree.add_child(&[0], "grandchild1").unwrap();

    println!("DFS traversal:");
    for value in tree.iter_dfs() {
        println!("  {}", value);
    }

    println!("BFS traversal:");
    for value in tree.iter_bfs() {
        println!("  {}", value);
    }

    /# Cache example
    var cache = LruCache::new(2);
    cache.put("key1", "value1");
    cache.put("key2", "value2");

    println!("Get key1: {:?}", cache.get(&"key1"));

    cache.put("key3", "value3"); /# This evicts key2
    println!("Get key2: {:?}", cache.get(&"key2")); /# None
    println!("Get key3: {:?}", cache.get(&"key3")); /# Some("value3")
}
```

### 17.5 FFI Usage

```veil
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

### 17.6 Template Strings

Template strings are delimited by backticks and support inline expression interpolation with braces.

- Delimiter: backticks `…`
- Interpolation: `{expr}` inside a template string
- Not supported: interpolation inside double-quoted strings ("…") or single quotes ('…')
  - Single quotes are not valid string delimiters in Veil

Examples:

```veil
const name = "Alice";
print(`Hello, {name}!`);

const a: i32 = 6;
println(`The value of a is {a}`);
```

Incorrect (prints braces literally or will not parse):

```veil
// No interpolation in double-quoted strings:
print("Hello, {name}");

// Single quotes are not valid for strings or templates:
println('Value: {a}');
```

```veil
fn greet(name: string,
 age: i32) {
    const message = `Hello {name}! You are {age} years old.`;
    print(message);
}
```

### 17.7 Range and Array

```veil
fn sum_range(start: i32, end: i32) -> i32 {
    var total = 0;
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

## 18. Performance Guidelines

This section provides comprehensive guidance for writing high-performance Veil code, covering memory optimization, async runtime tuning, compile-time computation strategies, and profiling techniques.

### 18.1 Memory Management Optimization

#### 18.1.1 ARC Optimization Patterns

```veil
/# Minimize reference counting overhead
fn process_data(data: &[Item]) -> ProcessedData {
    /# Use borrowed references instead of cloning Arc<T>
    /# This avoids unnecessary reference count increments
    let results = data.iter()
        .map(|item| process_item(item))  /# Borrow, don't clone
        .collect();

    ProcessedData::new(results)
}

/# Break reference cycles with weak references
struct Node {
    children: Vec<Arc<Node>>,
    parent: Weak<Node>,  /# Prevents cycles
}

/# Use Cow (Clone on Write) for conditional cloning
fn maybe_modify(data: Cow<'_, str>) -> Cow<'_, str> {
    if needs_modification(&data) {
        Cow::Owned(data.to_uppercase())  /# Clone only when needed
    } else {
        data  /# Return borrowed data unchanged
    }
}
```

#### 18.1.2 Memory Layout Optimization

```veil
/# Use repr(C) for predictable layout
#[repr(C)]
struct OptimizedStruct {
    /# Order fields by size (largest first) to minimize padding
    large_field: u64,     /# 8 bytes
    medium_field: u32,    /# 4 bytes
    small_field: u16,     /# 2 bytes
    tiny_field: u8,       /# 1 byte
    /# Total: 15 bytes + 1 byte padding = 16 bytes
}

/# Use packed structs sparingly (can hurt performance)
#[repr(packed)]
struct PackedStruct {
    a: u32,
    b: u8,
    c: u16,  /# May be misaligned - slower access
}

/# Cache-friendly data structures
struct SoA {  /# Structure of Arrays - better for bulk operations
    x_coords: Vec<f32>,
    y_coords: Vec<f32>,
    z_coords: Vec<f32>,
}

struct AoS {  /# Array of Structures - better for individual access
    points: Vec<Point3D>,
}

/# Choose based on access patterns:
/# - Use SoA when processing all X coordinates together
/# - Use AoS when processing complete points together
```

#### 18.1.3 Allocation Strategies

```veil
/# Pre-allocate collections when size is known
fn process_large_dataset(items: &[Input]) -> Vec<Output> {
    let mut results = Vec::with_capacity(items.len());  /# Avoid reallocations

    for item in items {
        results.push(process_item(item));
    }

    results
}

/# Use object pools for frequently allocated objects
struct ObjectPool<T> {
    objects: Mutex<Vec<T>>,
    factory: fn() -> T,
}

impl<T> ObjectPool<T> {
    fn acquire(&self) -> PooledObject<T> {
        let mut pool = self.objects.lock().unwrap();
        let obj = pool.pop().unwrap_or_else(|| (self.factory)());
        PooledObject::new(obj, &self.objects)
    }
}

/# Use stack allocation for small, short-lived data
const SMALL_BUFFER_SIZE: usize = 1024;

fn process_small_data(input: &[u8]) -> Result<ProcessedData, Error> {
    if input.len() <= SMALL_BUFFER_SIZE {
        let mut buffer = [0u8; SMALL_BUFFER_SIZE];  /# Stack allocated
        /# Process using stack buffer
    } else {
        let mut buffer = vec![0u8; input.len()];  /# Heap allocated
        /# Process using heap buffer
    }
}
```

### 18.2 Async Runtime Tuning

#### 18.2.1 Runtime Configuration

```veil
/# Configure runtime for your workload
async fn configure_runtime() {
    let config = runtime::Config {
        /# CPU-bound work: threads = CPU cores
        worker_threads: num_cpus::get(),

        /# I/O-bound work: more threads than cores
        /# worker_threads: num_cpus::get() * 2,

        /# Blocking operation threads
        max_blocking_threads: 512,

        /# Stack size for tasks (smaller = more tasks)
        thread_stack_size: 2 * 1024 * 1024,  /# 2MB

        /# Platform-specific optimizations
        enable_io_uring: cfg!(target_os = "linux"),
        enable_completion_ports: cfg!(target_os = "windows"),

        /# Task queue configuration
        task_queue_size: 8192,
        steal_interval: Duration::from_micros(100),
    };

    runtime::set_config(config);
}
```

#### 18.2.2 Task Scheduling Optimization

```veil
/# Use spawn_local for single-threaded contexts
async fn single_threaded_work() {
    /# Avoids cross-thread synchronization overhead
    spawn_local(async {
        /# Work that doesn't need to be Send
        process_local_data().await
    }).await;
}

/# Batch small async operations
async fn batch_operations(items: Vec<Item>) -> Vec<Result> {
    const BATCH_SIZE: usize = 100;

    let mut results = Vec::new();

    for chunk in items.chunks(BATCH_SIZE) {
        let batch_futures: Vec<_> = chunk.iter()
            .map(|item| process_item_async(item))
            .collect();

        let batch_results = futures::join_all(batch_futures).await;
        results.extend(batch_results);
    }

    results
}

/# Use yield_now() for cooperative scheduling
async fn cpu_intensive_work(data: &[u8]) {
    for (i, chunk) in data.chunks(1024).enumerate() {
        process_chunk(chunk);

        /# Yield periodically to allow other tasks to run
        if i % 100 == 0 {
            tokio::task::yield_now().await;
        }
    }
}
```

#### 18.2.3 I/O Optimization

```veil
/# Use buffered I/O for better performance
async fn efficient_file_processing(path: &Path) -> io::Result<ProcessedData> {
    let file = File::open(path).await?;
    let mut reader = BufReader::with_capacity(64 * 1024, file);  /# 64KB buffer

    let mut processed = ProcessedData::new();
    let mut line = String::new();

    while reader.read_line(&mut line).await? > 0 {
        process_line(&line, &mut processed);
        line.clear();  /# Reuse string allocation
    }

    Ok(processed)
}

/# Use vectored I/O for multiple buffers
async fn write_multiple_buffers(
    writer: &mut impl AsyncWrite,
    buffers: &[&[u8]]
) -> io::Result<()> {
    /# More efficient than multiple write calls
    writer.write_vectored(buffers).await?;
    writer.flush().await?;
    Ok(())
}

/# Connection pooling for network operations
struct ConnectionPool {
    pool: Arc<Mutex<Vec<Connection>>>,
    max_size: usize,
}

impl ConnectionPool {
    async fn acquire(&self) -> Result<PooledConnection, Error> {
        {
            let mut pool = self.pool.lock().await;
            if let Some(conn) = pool.pop() {
                return Ok(PooledConnection::new(conn, self.pool.clone()));
            }
        }

        /# Create new connection if pool is empty
        let conn = Connection::new().await?;
        Ok(PooledConnection::new(conn, self.pool.clone()))
    }
}
```

### 18.3 Compile-Time Computation Strategies

#### 18.3.1 Effective Use of comptime

```veil
/# Pre-compute expensive calculations
comptime {
    const SINE_TABLE: [f64; 360] = {
        let mut table = [0.0; 360];
        for i in 0..360 {
            table[i] = (i as f64 * PI / 180.0).sin();
        }
        table
    };
}

fn fast_sine_degrees(degrees: u32) -> f64 {
    SINE_TABLE[degrees as usize % 360]  /# O(1) lookup
}

/# Generate code based on configuration
comptime {
    const FEATURES: [&str] = cfg_features!();  /# Get enabled features

    if FEATURES.contains("simd") {
        /# Generate SIMD-optimized code
    } else {
        /# Generate scalar fallback
    }
}

/# Type-level computations
fn matrix_multiply<const M: usize, const N: usize, const P: usize>(
    a: [[f64; N]; M],
    b: [[f64; P]; N]
) -> [[f64; P]; M]
where
    [(); M * P]: Sized,  /# Ensure result fits in memory
{
    comptime {
        assert!(M <= 1000 && N <= 1000 && P <= 1000, "Matrix too large");
        assert!(M * P <= 100_000, "Result matrix too large");
    }

    /# Implementation with compile-time size checks
}
```

#### 18.3.2 Code Generation Patterns

```veil
/# Generate optimized switch statements
macro_rules! generate_dispatch {
    ($($variant:ident => $handler:expr),*) => {
        comptime {
            /# Generate jump table at compile time
            const DISPATCH_TABLE: [fn(); COUNT] = [
                $($handler),*
            ];
        }

        match discriminant {
            $(
                Discriminant::$variant => DISPATCH_TABLE[discriminant as usize](),
            )*
        }
    };
}

/# Template specialization
trait Process<T> {
    fn process(&self, data: T) -> ProcessedData;
}

/# Specialized implementation for numeric types
impl Process<f64> for Processor {
    fn process(&self, data: f64) -> ProcessedData {
        comptime {
            /# Generate SIMD instructions for f64
            generate_simd_code!(f64)
        }
    }
}

impl Process<String> for Processor {
    fn process(&self, data: String) -> ProcessedData {
        comptime {
            /# Generate string-specific optimizations
            generate_string_code!()
        }
    }
}
```

### 18.4 Profiling and Benchmarking

#### 18.4.1 Built-in Profiling Support

```veil
/# Function-level profiling
#[profile]
fn expensive_function(data: &[u8]) -> ProcessedData {
    /# Automatically tracked by profiler
    process_data(data)
}

/# Manual profiling scopes
fn complex_operation() {
    profile_scope!("initialization");
    let setup = initialize_data();

    profile_scope!("processing");
    let processed = process_data(&setup);

    profile_scope!("cleanup");
    cleanup_resources(processed);
}

/# Async profiling
#[profile_async]
async fn async_operation() -> Result<Data, Error> {
    let data = fetch_data().await?;
    let processed = process_async(data).await?;
    Ok(processed)
}

/# Memory allocation tracking
#[track_allocations]
fn memory_intensive_function() {
    /# Track all allocations within this function
    let large_vec = vec![0; 1_000_000];
    process_large_data(&large_vec);
}
```

#### 18.4.2 Performance Testing

```veil
/# Benchmark tests
#[bench]
fn bench_string_processing(b: &mut Bencher) {
    let data = generate_test_data();

    b.iter(|| {
        black_box(process_strings(&data))  /# Prevent optimization
    });
}

/# Comparative benchmarks
#[bench_group]
mod string_benches {
    use super::*;

    #[bench]
    fn naive_approach(b: &mut Bencher) {
        b.iter(|| naive_string_processing(&TEST_DATA));
    }

    #[bench]
    fn optimized_approach(b: &mut Bencher) {
        b.iter(|| optimized_string_processing(&TEST_DATA));
    }

    #[bench]
    fn simd_approach(b: &mut Bencher) {
        b.iter(|| simd_string_processing(&TEST_DATA));
    }
}

/# Memory usage benchmarks
#[bench_memory]
fn bench_memory_usage() {
    let _data = allocate_large_structure();
    /# Memory usage is tracked automatically
}

/# Async benchmarks
#[bench_async]
async fn bench_async_operation(b: &mut AsyncBencher) {
    b.iter(|| async {
        let result = expensive_async_operation().await;
        black_box(result)
    }).await;
}
```

#### 18.4.3 Performance Monitoring in Production

```veil
/# Runtime performance metrics
struct PerformanceMonitor {
    metrics: Arc<Mutex<MetricsCollector>>,
}

impl PerformanceMonitor {
    fn record_latency(&self, operation: &str, duration: Duration) {
        let mut metrics = self.metrics.lock().unwrap();
        metrics.record_latency(operation, duration);
    }

    fn record_throughput(&self, operation: &str, count: u64) {
        let mut metrics = self.metrics.lock().unwrap();
        metrics.record_throughput(operation, count);
    }
}

/# Automatic performance monitoring
#[monitor_performance]
async fn monitored_operation() -> Result<Data, Error> {
    /# Latency and error rates automatically tracked
    let data = fetch_data().await?;
    process_data(data).await
}

/# Custom performance counters
static REQUESTS_PROCESSED: AtomicU64 = AtomicU64::new(0);
static AVERAGE_LATENCY: AtomicU64 = AtomicU64::new(0);

fn update_performance_metrics(latency: Duration) {
    REQUESTS_PROCESSED.fetch_add(1, Ordering::Relaxed);

    /# Update rolling average
    let current_avg = AVERAGE_LATENCY.load(Ordering::Relaxed);
    let new_avg = (current_avg + latency.as_micros() as u64) / 2;
    AVERAGE_LATENCY.store(new_avg, Ordering::Relaxed);
}
```

### 18.5 Platform-Specific Optimizations

#### 18.5.1 CPU Architecture Optimizations

```veil
/# SIMD optimizations
#[cfg(target_feature = "avx2")]
fn simd_process_array(data: &[f32]) -> Vec<f32> {
    use std::arch::x86_64::*;

    let mut result = Vec::with_capacity(data.len());

    unsafe {
        for chunk in data.chunks_exact(8) {
            let values = _mm256_loadu_ps(chunk.as_ptr());
            let processed = _mm256_mul_ps(values, _mm256_set1_ps(2.0));

            let mut output = [0.0f32; 8];
            _mm256_storeu_ps(output.as_mut_ptr(), processed);
            result.extend_from_slice(&output);
        }
    }

    result
}

/# CPU cache optimization
#[cold]  /# Hint that this function is rarely called
fn handle_error_case() {
    /# Error handling code
}

#[hot]  /# Hint that this function is frequently called
#[inline]
fn fast_path_function() {
    /# Hot path code
}

/# Branch prediction hints
fn process_data(data: &[Item]) {
    for item in data {
        if likely(item.is_valid()) {  /# Hint: this branch is usually taken
            process_valid_item(item);
        } else {
            handle_invalid_item(item);  /# Cold path
        }
    }
}
```

#### 18.5.2 Memory System Optimization

```veil
/# Cache-line alignment for performance-critical structures
#[repr(align(64))]  /# Align to cache line boundary
struct CacheAlignedData {
    hot_data: [u64; 8],  /# Frequently accessed data
}

/# False sharing prevention
struct ThreadLocalData {
    #[cache_line_pad]
    counter: AtomicU64,  /# Prevent false sharing

    #[cache_line_pad]
    state: AtomicU32,
}

/# Prefetching for predictable access patterns
fn process_linked_list(head: *const Node) {
    let mut current = head;

    unsafe {
        while !current.is_null() {
            let node = &*current;

            /# Prefetch next node while processing current
            if !node.next.is_null() {
                prefetch_read_data(node.next as *const u8);
            }

            process_node(node);
            current = node.next;
        }
    }
}

/# Memory mapping for large files
fn process_large_file(path: &Path) -> io::Result<ProcessedData> {
    let file = File::open(path)?;
    let mmap = unsafe { Mmap::map(&file)? };

    /# Process memory-mapped data directly
    process_bytes(&mmap[..])
}
```

## 19. Interoperability Guide

This section provides comprehensive guidance for integrating Veil with other programming languages and systems, focusing on C FFI, binding generation, memory safety across language boundaries, and platform-specific considerations.

### 19.1 C Foreign Function Interface (FFI)

#### 19.1.1 Basic C Interoperability

```veil
/# External C function declarations
extern "C" {
    fn malloc(size: usize) -> rawptr;
    fn free(ptr: rawptr);
    fn strlen(s: *const u8) -> usize;
    fn printf(format: *const u8, ...) -> i32;
    fn fopen(filename: *const u8, mode: *const u8) -> *mut FILE;
    fn fclose(file: *mut FILE) -> i32;
}

/# Calling C functions from Veil
fn allocate_memory(size: usize) -> Option<rawptr> {
    unsafe {
        let ptr = malloc(size);
        if ptr.is_null() {
            None
        } else {
            Some(ptr)
        }
    }
}

/# Exporting Veil functions to C
#[no_mangle]
pub extern "C" fn veil_add(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
pub extern "C" fn veil_process_string(
    input: *const u8,
    input_len: usize,
    output: *mut u8,
    output_capacity: usize
) -> i32 {
    /# Safe wrapper around unsafe C interface
    if input.is_null() || output.is_null() {
        return -1;
    }

    unsafe {
        let input_slice = std::slice::from_raw_parts(input, input_len);
        let input_str = match std::str::from_utf8(input_slice) {
            Ok(s) => s,
            Err(_) => return -2,
        };

        let processed = process_string_internal(input_str);
        let processed_bytes = processed.as_bytes();

        if processed_bytes.len() > output_capacity {
            return -3; /# Buffer too small
        }

        std::ptr::copy_nonoverlapping(
            processed_bytes.as_ptr(),
            output,
            processed_bytes.len()
        );

        processed_bytes.len() as i32
    }
}
```

#### 19.1.2 C Structure Interoperability

```veil
/# C-compatible struct layouts
#[repr(C)]
struct CPoint {
    x: f64,
    y: f64,
}

#[repr(C)]
struct CString {
    data: *mut u8,
    len: usize,
    capacity: usize,
}

#[repr(C)]
union CValue {
    int_val: i32,
    float_val: f32,
    ptr_val: *mut u8,
}

/# Opaque types for C API
#[repr(C)]
pub struct OpaqueHandle {
    _private: [u8; 0],
}

/# C API with opaque handles
#[no_mangle]
pub extern "C" fn create_processor() -> *mut OpaqueHandle {
    let processor = Box::new(Processor::new());
    Box::into_raw(processor) as *mut OpaqueHandle
}

#[no_mangle]
pub extern "C" fn destroy_processor(handle: *mut OpaqueHandle) {
    if !handle.is_null() {
        unsafe {
            let _processor = Box::from_raw(handle as *mut Processor);
            /# Processor is automatically dropped
        }
    }
}

#[no_mangle]
pub extern "C" fn process_data(
    handle: *mut OpaqueHandle,
    data: *const u8,
    len: usize
) -> i32 {
    if handle.is_null() || data.is_null() {
        return -1;
    }

    unsafe {
        let processor = &mut *(handle as *mut Processor);
        let input_slice = std::slice::from_raw_parts(data, len);

        match processor.process(input_slice) {
            Ok(_) => 0,
            Err(_) => -2,
        }
    }
}
```

### 19.2 Advanced FFI Patterns

#### 19.2.1 Callback Functions

```veil
/# C-style callbacks
type CCallback = extern "C" fn(data: *const u8, len: usize, user_data: *mut u8) -> i32;

#[no_mangle]
pub extern "C" fn register_callback(
    callback: CCallback,
    user_data: *mut u8
) -> i32 {
    /# Store callback for later use
    unsafe {
        GLOBAL_CALLBACK = Some((callback, user_data));
    }
    0
}

/# Invoking callbacks safely
fn invoke_callback(data: &[u8]) -> Result<(), CallbackError> {
    unsafe {
        if let Some((callback, user_data)) = GLOBAL_CALLBACK {
            let result = callback(data.as_ptr(), data.len(), user_data);
            if result == 0 {
                Ok(())
            } else {
                Err(CallbackError::CallbackFailed(result))
            }
        } else {
            Err(CallbackError::NoCallback)
        }
    }
}

/# Veil closures to C function pointers
fn create_c_compatible_closure() -> extern "C" fn(i32) -> i32 {
    extern "C" fn closure_wrapper(x: i32) -> i32 {
        /# Stateless function - safe to convert
        x * 2
    }
    closure_wrapper
}
```

#### 19.2.2 Error Handling Across Language Boundaries

```veil
/# C-compatible error codes
#[repr(C)]
#[derive(Copy, Clone)]
pub enum CErrorCode {
    Success = 0,
    InvalidInput = 1,
    OutOfMemory = 2,
    IoError = 3,
    ParseError = 4,
    UnknownError = 255,
}

/# Convert Veil errors to C error codes
impl From<VeilError> for CErrorCode {
    fn from(error: VeilError) -> Self {
        match error {
            VeilError::InvalidInput(_) => CErrorCode::InvalidInput,
            VeilError::OutOfMemory => CErrorCode::OutOfMemory,
            VeilError::Io(_) => CErrorCode::IoError,
            VeilError::Parse(_) => CErrorCode::ParseError,
            _ => CErrorCode::UnknownError,
        }
    }
}

/# Error handling with detailed messages
#[no_mangle]
pub extern "C" fn get_last_error_message(
    buffer: *mut u8,
    buffer_size: usize
) -> usize {
    unsafe {
        if let Some(ref error_msg) = LAST_ERROR_MESSAGE {
            let msg_bytes = error_msg.as_bytes();
            let copy_len = std::cmp::min(msg_bytes.len(), buffer_size - 1);

            if !buffer.is_null() && buffer_size > 0 {
                std::ptr::copy_nonoverlapping(
                    msg_bytes.as_ptr(),
                    buffer,
                    copy_len
                );
                *buffer.offset(copy_len as isize) = 0; /# Null terminator
            }

            msg_bytes.len()
        } else {
            0
        }
    }
}
```

### 19.3 Memory Safety Across Language Boundaries

#### 19.3.1 Safe Memory Management Patterns

```veil
/# RAII wrapper for C resources
pub struct CResource {
    handle: *mut c_void,
    deleter: fn(*mut c_void),
}

impl CResource {
    pub fn new(handle: *mut c_void, deleter: fn(*mut c_void)) -> Self {
        CResource { handle, deleter }
    }

    pub fn as_ptr(&self) -> *mut c_void {
        self.handle
    }
}

impl Drop for CResource {
    fn drop(&mut self) {
        if !self.handle.is_null() {
            (self.deleter)(self.handle);
            self.handle = std::ptr::null_mut();
        }
    }
}

/# Safe C string handling
pub struct CString {
    inner: std::ffi::CString,
}

impl CString {
    pub fn new(s: &str) -> Result<Self, std::ffi::NulError> {
        Ok(CString {
            inner: std::ffi::CString::new(s)?,
        })
    }

    pub fn as_ptr(&self) -> *const c_char {
        self.inner.as_ptr()
    }
}

/# Owned vs borrowed data across FFI
#[no_mangle]
pub extern "C" fn create_owned_data(size: usize) -> *mut u8 {
    if size == 0 {
        return std::ptr::null_mut();
    }

    let layout = std::alloc::Layout::array::<u8>(size).unwrap();
    unsafe {
        let ptr = std::alloc::alloc(layout);
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        ptr
    }
}

#[no_mangle]
pub extern "C" fn free_owned_data(ptr: *mut u8, size: usize) {
    if !ptr.is_null() && size > 0 {
        unsafe {
            let layout = std::alloc::Layout::array::<u8>(size).unwrap();
            std::alloc::dealloc(ptr, layout);
        }
    }
}
```

#### 19.3.2 Thread Safety Considerations

```veil
/# Thread-safe C API
use std::sync::{Arc, Mutex};

struct ThreadSafeProcessor {
    inner: Arc<Mutex<Processor>>,
}

#[no_mangle]
pub extern "C" fn create_thread_safe_processor() -> *mut ThreadSafeProcessor {
    let processor = ThreadSafeProcessor {
        inner: Arc::new(Mutex::new(Processor::new())),
    };
    Box::into_raw(Box::new(processor))
}

#[no_mangle]
pub extern "C" fn thread_safe_process(
    handle: *mut ThreadSafeProcessor,
    data: *const u8,
    len: usize
) -> i32 {
    if handle.is_null() || data.is_null() {
        return -1;
    }

    unsafe {
        let processor_wrapper = &*handle;
        let input_slice = std::slice::from_raw_parts(data, len);

        match processor_wrapper.inner.lock() {
            Ok(mut processor) => {
                match processor.process(input_slice) {
                    Ok(_) => 0,
                    Err(_) => -2,
                }
            },
            Err(_) => -3, /# Mutex poisoned
        }
    }
}

/# Async-safe FFI with channels
use std::sync::mpsc;

static mut COMMAND_SENDER: Option<mpsc::Sender<Command>> = None;

#[no_mangle]
pub extern "C" fn send_async_command(
    command_type: i32,
    data: *const u8,
    len: usize
) -> i32 {
    unsafe {
        if let Some(ref sender) = COMMAND_SENDER {
            let data_slice = if data.is_null() {
                &[]
            } else {
                std::slice::from_raw_parts(data, len)
            };

            let command = Command::new(command_type, data_slice.to_vec());

            match sender.send(command) {
                Ok(()) => 0,
                Err(_) => -1,
            }
        } else {
            -2 /# Not initialized
        }
    }
}
```

### 19.4 Binding Generation Tools

#### 19.4.1 Automatic Header Generation

```veil
/# Generate C headers from Veil code
#[cbindgen::export]
#[repr(C)]
pub struct ExportedStruct {
    pub field1: i32,
    pub field2: f64,
}

#[cbindgen::export]
pub extern "C" fn exported_function(
    input: *const ExportedStruct,
    output: *mut ExportedStruct
) -> i32 {
    /# Implementation
    0
}

/# Configuration for cbindgen (in cbindgen.toml)
/*
[export]
include = ["ExportedStruct", "exported_function"]

[parse]
parse_deps = true
include = ["dependency_crate"]

[fn]
rename_args = "PascalCase"

[struct]
rename_fields = "PascalCase"
*/
```

#### 19.4.2 Binding Other Languages

```veil
/# Python bindings with PyO3
#[pymodule]
fn veil_module(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(process_data_py, m)?)?;
    m.add_class::<PyProcessor>()?;
    Ok(())
}

#[pyfunction]
fn process_data_py(data: &[u8]) -> PyResult<Vec<u8>> {
    match process_data(data) {
        Ok(result) => Ok(result),
        Err(e) => Err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(
            format!("Processing failed: {}", e)
        )),
    }
}

#[pyclass]
struct PyProcessor {
    inner: Processor,
}

#[pymethods]
impl PyProcessor {
    #[new]
    fn new() -> Self {
        PyProcessor {
            inner: Processor::new(),
        }
    }

    fn process(&mut self, data: &[u8]) -> PyResult<Vec<u8>> {
        match self.inner.process(data) {
            Ok(result) => Ok(result),
            Err(e) => Err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(
                e.to_string()
            )),
        }
    }
}

/# Node.js bindings with napi-rs
#[js_function(1)]
fn process_data_js(ctx: CallContext) -> napi::Result<JsBuffer> {
    let input_buffer = ctx.get::<JsBuffer>(0)?;
    let input_data = input_buffer.into_value()?;

    match process_data(input_data.as_ref()) {
        Ok(result) => {
            let mut output_buffer = ctx.env.create_buffer(result.len())?;
            output_buffer.as_mut_slice().copy_from_slice(&result);
            Ok(output_buffer.into_raw())
        },
        Err(e) => Err(napi::Error::from_reason(e.to_string())),
    }
}
```

### 19.5 Platform-Specific Considerations

#### 19.5.1 Windows Integration

```veil
/# Windows API integration
#[cfg(target_os = "windows")]
mod windows {
    use winapi::um::winnt::HANDLE;
    use winapi::um::handleapi::CloseHandle;

    pub struct WindowsHandle(HANDLE);

    impl WindowsHandle {
        pub fn new(handle: HANDLE) -> Self {
            WindowsHandle(handle)
        }

        pub fn as_raw(&self) -> HANDLE {
            self.0
        }
    }

    impl Drop for WindowsHandle {
        fn drop(&mut self) {
            unsafe {
                CloseHandle(self.0);
            }
        }
    }

    /# COM interface implementation
    #[repr(C)]
    pub struct VeilComObject {
        vtable: *const VeilComVTable,
        ref_count: std::sync::atomic::AtomicU32,
        inner: Processor,
    }

    #[repr(C)]
    pub struct VeilComVTable {
        pub query_interface: extern "system" fn(
            this: *mut VeilComObject,
            riid: *const winapi::shared::guiddef::GUID,
            ppv: *mut *mut std::ffi::c_void
        ) -> winapi::shared::winerror::HRESULT,

        pub add_ref: extern "system" fn(
            this: *mut VeilComObject
        ) -> u32,

        pub release: extern "system" fn(
            this: *mut VeilComObject
        ) -> u32,

        pub process: extern "system" fn(
            this: *mut VeilComObject,
            data: *const u8,
            len: u32,
            result: *mut *mut u8,
            result_len: *mut u32
        ) -> winapi::shared::winerror::HRESULT,
    }
}
```

#### 19.5.2 Unix/Linux Integration

```veil
/# Unix-specific functionality
#[cfg(unix)]
mod unix {
    use std::os::unix::io::{AsRawFd, FromRawFd, RawFd};

    pub struct UnixFileDescriptor {
        fd: RawFd,
    }

    impl UnixFileDescriptor {
        pub fn new(fd: RawFd) -> Self {
            UnixFileDescriptor { fd }
        }

        pub fn as_raw_fd(&self) -> RawFd {
            self.fd
        }
    }

    impl Drop for UnixFileDescriptor {
        fn drop(&mut self) {
            unsafe {
                libc::close(self.fd);
            }
        }
    }

    /# Signal handling
    extern "C" fn signal_handler(sig: libc::c_int) {
        match sig {
            libc::SIGINT => {
                /# Handle Ctrl+C
                shutdown_gracefully();
            },
            libc::SIGTERM => {
                /# Handle termination request
                shutdown_gracefully();
            },
            _ => {},
        }
    }

    pub fn setup_signal_handlers() -> Result<(), std::io::Error> {
        unsafe {
            libc::signal(libc::SIGINT, signal_handler as libc::sighandler_t);
            libc::signal(libc::SIGTERM, signal_handler as libc::sighandler_t);
        }
        Ok(())
    }
}
```

#### 19.5.3 WebAssembly Integration

```veil
/# WebAssembly bindings
#[cfg(target_arch = "wasm32")]
mod wasm {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    pub struct WasmProcessor {
        inner: Processor,
    }

    #[wasm_bindgen]
    impl WasmProcessor {
        #[wasm_bindgen(constructor)]
        pub fn new() -> WasmProcessor {
            WasmProcessor {
                inner: Processor::new(),
            }
        }

        #[wasm_bindgen]
        pub fn process(&mut self, data: &[u8]) -> Result<Vec<u8>, JsValue> {
            self.inner.process(data)
                .map_err(|e| JsValue::from_str(&e.to_string()))
        }

        #[wasm_bindgen]
        pub fn process_string(&mut self, input: &str) -> Result<String, JsValue> {
            self.inner.process_string(input)
                .map_err(|e| JsValue::from_str(&e.to_string()))
        }
    }

    /# JavaScript interop
    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace = console)]
        fn log(s: &str);

        #[wasm_bindgen(js_namespace = console, js_name = log)]
        fn log_u32(a: u32);

        #[wasm_bindgen(js_namespace = Date, js_name = now)]
        fn date_now() -> f64;
    }

    #[wasm_bindgen]
    pub fn console_log(message: &str) {
        log(message);
    }

    #[wasm_bindgen]
    pub fn get_timestamp() -> f64 {
        date_now()
    }
}
```

### 19.6 Best Practices for Interoperability

#### 19.6.1 API Design Guidelines

```veil
/# Design C APIs that are easy to use correctly
/# and hard to use incorrectly

/# Good: Clear ownership semantics
#[no_mangle]
pub extern "C" fn create_object() -> *mut OpaqueObject {
    /# Caller owns the returned pointer
    Box::into_raw(Box::new(Object::new()))
}

#[no_mangle]
pub extern "C" fn destroy_object(obj: *mut OpaqueObject) {
    /# Caller transfers ownership back
    if !obj.is_null() {
        unsafe { Box::from_raw(obj); }
    }
}

/# Good: Explicit buffer management
#[no_mangle]
pub extern "C" fn get_data(
    obj: *const OpaqueObject,
    buffer: *mut u8,
    buffer_size: usize,
    actual_size: *mut usize
) -> i32 {
    /# Returns required buffer size even if buffer is too small
    if obj.is_null() || actual_size.is_null() {
        return -1;
    }

    unsafe {
        let object = &*obj;
        let data = object.get_data();

        *actual_size = data.len();

        if buffer.is_null() || buffer_size < data.len() {
            return -2; /# Buffer too small, but actual_size is set
        }

        std::ptr::copy_nonoverlapping(
            data.as_ptr(),
            buffer,
            data.len()
        );

        0 /# Success
    }
}

/# Version information for API evolution
#[no_mangle]
pub extern "C" fn get_api_version() -> u32 {
    (MAJOR_VERSION << 16) | (MINOR_VERSION << 8) | PATCH_VERSION
}

#[no_mangle]
pub extern "C" fn is_feature_supported(feature: u32) -> bool {
    match feature {
        FEATURE_ASYNC_PROCESSING => true,
        FEATURE_PARALLEL_EXECUTION => true,
        FEATURE_CUSTOM_ALLOCATORS => false,
        _ => false,
    }
}
```

#### 19.6.2 Testing Interoperability

```veil
/# Integration tests for C API
#[cfg(test)]
mod ffi_tests {
    use super::*;
    use std::ffi::CStr;

    #[test]
    fn test_c_api_lifecycle() {
        unsafe {
            /# Test object creation and destruction
            let obj = create_object();
            assert!(!obj.is_null());

            /# Test basic operations
            let result = process_data(obj, b"test".as_ptr(), 4);
            assert_eq!(result, 0);

            /# Test cleanup
            destroy_object(obj);
        }
    }

    #[test]
    fn test_error_handling() {
        unsafe {
            /# Test null pointer handling
            let result = process_data(std::ptr::null_mut(), std::ptr::null(), 0);
            assert_eq!(result, -1);

            /# Test error message retrieval
            let mut buffer = [0u8; 256];
            let len = get_last_error_message(buffer.as_mut_ptr(), buffer.len());
            assert!(len > 0);

            let error_msg = CStr::from_ptr(buffer.as_ptr() as *const i8);
            assert!(!error_msg.to_str().unwrap().is_empty());
        }
    }

    #[test]
    fn test_thread_safety() {
        use std::thread;
        use std::sync::Arc;

        let obj = unsafe { create_thread_safe_processor() };
        assert!(!obj.is_null());

        let handles: Vec<_> = (0..10).map(|i| {
            thread::spawn(move || {
                let data = format!("test{}", i);
                unsafe {
                    thread_safe_process(obj, data.as_ptr(), data.len())
                }
            })
        }).collect();

        for handle in handles {
            assert_eq!(handle.join().unwrap(), 0);
        }

        unsafe { destroy_thread_safe_processor(obj); }
    }
}
```

## 20. Complete Grammar Reference

### 20.1 Operator Precedence (Highest to Lowest)

1. **Member/call/index**: . [] () — Left
2. **Cast**: as — Left
3. **Ranges**: .. ..= ..> ..< — Left
4. **Power**: \*\* — Right
5. **Multiplicative**: \* / /# % — Left
6. **Additive**: + - — Left
7. **Shifts**: << >> — Left
8. **Bitwise AND**: & — Left
9. **Bitwise XOR**: ^ — Left
10. **Bitwise OR**: | — Left
11. **Comparison**: > < >= <= — Left
12. **Equality**: == != — Left
13. **Logical AND**: & — Left
14. **Logical OR**: | — Left
15. **Assignment**: = (and compound) — Right

### 19.2 Statement Grammar

```ebnf
statement ::= variable_declaration
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

### 19.3 Expression Grammar

```ebnf
expression ::= assignment_expression

assignment_expression ::= pipeline_expression
                        | pipeline_expression '=' assignment_expression

pipeline_expression ::= logical_or_expression ('|>' logical_or_expression)*

logical_or_expression ::= logical_and_expression ('|' logical_and_expression)*

logical_and_expression ::= equality_expression ('&' equality_expression)*

equality_expression ::= relational_expression (('==' | '!=') relational_expression)*

relational_expression ::= additive_expression (('<' | '>' | '<=' | '>=' | 'in' | 'not in' | 'is' | 'is not') additive_expression)*

additive_expression ::= multiplicative_expression (('+' | '-') multiplicative_expression)*

multiplicative_expression ::= power_expression (('*' | '/' | '/#' | '%') power_expression)*

power_expression ::= cast_expression (('**') power_expression)*

cast_expression ::= range_expression ('as' type)*

range_expression ::= postfix_expression (('..' | '..=' | '..>' | '..<') postfix_expression)*

postfix_expression ::= prefix_expression postfix_operator*

postfix_operator ::= '[' expression ']'
                   | '(' argument_list? ')'
                   | '.' identifier ('(' argument_list? ')')?
                   | '++'
                   | '--'

prefix_expression ::= ('*' | '&' | '++' | '--' | '+' | '-' | '...' | '!' | 'await' | 'move') prefix_expression
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
                     | try_expression
                     | async_block
                     | unsafe_block
                     | closure_expression

closure_expression ::= move? '|' parameter_list? '|' (expression | block)
try_expression ::= 'try' block
async_block ::= 'async' block
unsafe_block ::= 'unsafe' block

literal ::= integer_literal
          | float_literal
          | string_literal
          | template_string_literal
          | bool_literal
          | none_literal
```

### 19.4 Type Grammar

```ebnf
type ::= base_type type_modifier*

type_modifier ::= '?'           /# Optional type
                | '[' ']'       /# Array type
                | '[' integer_literal ']'  /# Sized array

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

_This specification covers the complete syntax and semantics of Veil._
