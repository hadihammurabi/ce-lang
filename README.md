# CE-Lang - A Good Programming Language

A modern compiled systems programming language.

## Features

- **Fast Compilation**: Compiles to optimized C code
- **Native Execution**: Links with GCC for native binaries
- **Zero-copy VM**: Stack-based bytecode interpreter
- **Type Safety**: Static type system
- **Modern Syntax**: Clean, readable code

## Installation

### From Source

```bash
git clone https://github.com/hadihammurabi/ce-lang.git
cd ce-lang
make dev-setup
make build
make install
```

### From OPAM

```bash
opam install ce
```

## Quick Start

Create `main.ce`:

```ce
fn main() {
  println("Hello, World!")
}
```

Run it:

```bash
ce main.ce
./main
```

## Usage

```
ce <file.ce>
```

## Examples

### Variables and Expressions

```ce
fn main() {
  println(x + y)
}
```

### Functions

```ce
fn add() {
}
```

## Development

```bash
make build       # Build project
make install     # Install
make clean       # Clean build
```

## Architecture

- **Lexer** (`lib/lexer/`) - Tokenization with Sedlex
- **Parser** (`lib/parser/`) - AST generation with Menhir
- **VM** (`lib/vm/`) - Bytecode compiler
- **CLI** (`bin/`) - Command-line interface

## Performance

- **Compilation**: Sub-100ms for typical files
- **Execution**: Near-native speed with GCC optimization
- **Memory**: < 1MB minimal runtime

## License

GPL-3.0 - See [LICENSE](./LICENSE) file

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run `make fmt && make lint`
5. Submit a pull request

## Author

[hadihammurabi](https://github.com/hadihammurabi)
