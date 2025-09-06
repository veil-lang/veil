<div align="center" style="display: grid; place-items: center; gap: 1rem;">
     <img src="https://avatars.githubusercontent.com/u/196317235?s=400&u=67bb160a766df001c98e70501ec7b496e90bf8b1&v=4" alt="Veil Logo" width="200">
</div>

# Veil

**A modern, statically-typed programming language with focus on safety, performance, and developer experience.**

Veil compiles to efficient C code while providing modern language features like type inference, pattern matching, generics, and async/await.

## Features

- 🚀 **Fast compilation** to optimized C code
- 🛡️ **Memory safety** without garbage collection
- 🔧 **Type inference** and strong static typing
- 🎯 **Pattern matching** and algebraic data types
- ⚡ **Async/await** support for concurrent programming
- 📦 **Built-in package management** (coming soon)
- 🔄 **Self-updating toolchain** via GitHub releases

## Installation

### Quick Install (Recommended)

**Unix (Linux/macOS):**

```bash
curl -sSf https://raw.githubusercontent.com/veil-lang/veil/main/scripts/install.sh | bash
```

**Windows (PowerShell):**

```powershell
iex (iwr -useb https://raw.githubusercontent.com/veil-lang/veil/main/scripts/install.ps1).Content
```

### Build from Source

Requirements:

- Rust 1.70+
- Clang 17+ (Unix) / Visual Studio Build Tools (Windows)
- Git

```bash
git clone https://github.com/veil-lang/veil
cd veil
cargo install --path crates/cli --force
```

### Verify Installation

```bash
ve --version
# veil-cli 0.2.0-beta.4
```

## Quick Start

### Hello World

```veil
import std/io;

fn main() {
    print("Hello, World!");
}
```

```bash
# Save as hello.veil and run:
ve hello.veil
```

### Create a New Project

```bash
ve init my_project
cd my_project
ve run
```

## Usage

The `ve` toolchain provides everything you need for Veil development:

### Compilation Commands

```bash
# Compile and run (one command)
ve program.veil

# Build only (generate executable)
ve build program.veil -o my_program

# Build with optimizations
ve build program.veil --optimize

# Generate C code only (no compilation)
ve build program.veil --no-cc
```

### Project Management

```bash
# Initialize new project
ve init my_project

# Run project from current directory
ve run

# Run with verbose output
ve run --verbose
```

### Testing

```bash
# Run all tests in a file
ve test tests.veil

# Run specific test
ve test tests.veil --test-name my_test

# List available tests
ve test tests.veil --list
```

### Performance

```bash
# Benchmark a program
ve benchmark program.veil --iterations 10

# Benchmark with detailed output
ve benchmark program.veil --iterations 5 --verbose
```

### Toolchain Management

```bash
# Update ve toolchain to latest version
ve update

# Update from nightly channel
ve update --channel canary

# Force update (reinstall current version)
ve update --force

# Package management (coming soon)
ve upgrade
```

### Advanced Options

```bash
# Show compiler internals
ve build program.veil --verbose --dump-norm-hir --pass-timings

# Target specific platform
ve build program.veil --target-triple x86_64-unknown-linux-gnu

# Show cache statistics
ve build program.veil --cache-stats
```

## Language Examples

### Variables and Types

```veil
fn main() {
    const name = "Alice";           /# String inference
    const age: i32 = 30;           /# Explicit type
    var score: u32 = 100;         /# Immutable variable
    score = score + 10;           /# Cannot modify Immutable variables XX
    var mut height = 5.6;              /# Immutable f64 inference
    height = height + 0.1;         /# Can modify mutable variables
    print(`Hello, {name}! Age: {age}, Height: {height}`);
}
```

### Functions and Generics

```veil
fn add<T>(a: T, b: T) -> T where T: Add {
    a + b
}

fn main() {
    const result = add(5, 3);      /# i32
    const sum = add(2.5, 1.5);     /# f64
    print(`Results: {result}, {sum}`);
}
```

### Structs and Enums

```veil
struct User {
    name: str,
    age: u32,
    email: str,
}

enum Shape {
    Circle(f64),
    Rectangle(f64, f64),
    Triangle(f64, f64, f64),
}

fn area(shape: Shape) -> f64 {
    match shape {
        Shape.Circle(radius) => 3.14159 * radius * radius,
        Shape.Rectangle(width, height) => width * height,
        Shape.Triangle(a, b, c) => {
            const s = (a + b + c) / 2.0;
            (s * (s - a) * (s - b) * (s - c)).sqrt()
        }
    }
}

fn main() {
    const user = User {
        name: "Alice",
        age: 30,
        email: "alice@example.com",
    };

    var circle = Shape.Circle(5.0);
    print(`Circle area: {area(circle)}`);

    /# Can reassign mutable variables
    circle = Shape.Rectangle(10.0, 20.0);
    print(`Rectangle area: {area(circle)}`);
}
```

### Async Programming

```veil
import std::net;

async fn fetch_data(url: &str) -> Result<str, Error> {
    const response = await http::get(url)?;
    await response.text()
}

async fn main() {
    const data = await fetch_data("https://api.example.com/data");
    match data {
        Ok(content) => print(`Data: {content}`),
        Err(e) => print(`Error: {e}`),
    }
}
```

## Architecture

Veil uses a modular compilation pipeline:

- **Parser** - Pest-based grammar with error recovery
- **AST** - Type-safe abstract syntax tree
- **HIR** - High-level IR with type information
- **Resolution** - Symbol resolution and scope analysis
- **Type Checker** - Type inference and checking
- **Normalization** - Desugaring and transformations
- **Monomorphization** - Generic instantiation
- **IR** - Low-level optimization IR
- **Codegen** - C code generation

All components are built into the single `ve` binary for easy distribution.

## Development

### Building from Source

```bash
git clone https://github.com/veil-lang/veil
cd veil
cargo build
./target/debug/ve --help
```

### Running Tests

```bash
cargo test              # Rust unit tests
cargo check            # Check compilation
./target/debug/ve test tests/  # Veil language tests
```

### Project Structure

```
veil/
├── crates/
│   ├── cli/           # ve toolchain (main binary)
│   ├── syntax/        # Parser and grammar
│   ├── ast/           # Abstract syntax tree
│   ├── hir/           # High-level IR
│   ├── resolve/       # Symbol resolution
│   ├── typeck/        # Type checking
│   ├── normalize/     # HIR transformations
│   ├── mono/          # Monomorphization
│   ├── ir/            # Low-level IR
│   ├── codegen-c/     # C code generation
│   └── compiler/      # Pass manager
├── tests/             # Language test suite
├── scripts/           # Installation scripts
└── refs/              # Documentation and specs
```

## Contributing

We welcome contributions!

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `cargo test && ./target/debug/ve test tests/`
6. Submit a pull request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Community

- **GitHub**: [veil-lang/veil](https://github.com/veil-lang/veil)
- **Discord**: [Join our Discord](https://dsc.gg/velang)
- **Documentation**: [docs.veil-lang.org](https://docs.veil-lang.org)

## Status

Veil is currently in beta (v0.2.0-beta.4). The language and toolchain are functional but still evolving. We're working towards a stable 1.0 release with:

- ✅ Core language features
- ✅ Self-updating toolchain
- ✅ C code generation
- 🚧 Package management
- 🚧 Standard library
- 🚧 Language server (LSP)
- 🚧 Comprehensive documentation

---

<div align="center">
Made with ❤️ by the Veil team
</div>
