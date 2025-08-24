
<div align="center" style="display: grid; place-items: center; gap: 1rem;">
     <img src="https://avatars.githubusercontent.com/u/196317235?s=400&u=67bb160a766df001c98e70501ec7b496e90bf8b1&v=4" alt="Veil Logo" width="200">
</div>



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

### Manual Installation

Requirements:
- Rust 1.70+
- Clang 17+ (Unix) / Visual Studio Build Tools (Windows)
- Git

```bash
git clone https://github.com/veil-lang/veil
cd ve
cargo install --path .
```

For detailed installation instructions and troubleshooting, see [`scripts/README.md`](scripts/README.md).

## Usage
```bash
# Compile and run a file
ve example.veil

# Initialize a new project
ve init my_project

# Run a project (from the current directory)
ve run

# Run benchmarks - two approaches
ve benchmark example.veil --iterations 5 --verbose
# or
ve example.veil --iterations 10 --verbose
```
## Contributing
Contributions are welcome! Please read the [contributing guidelines](CONTRIBUTING.md) first.