# NohType Compiler (Reverse Python Compiler)

## Overview
NohType Compiler is a custom programming language compiler that transforms code written in "Reverse Python" (where Python keywords are reversed) into standard Python code. This educational project demonstrates the core components of programming language implementation including lexical analysis, parsing, semantic analysis, and code generation.

## Features
- Complete compilation pipeline for the NohType language
- Transforms reversed Python keywords (`tnirp`, `fi`, `elihw`, etc.) into standard Python
- Supports all major Python constructs including:
  - Variable declarations and assignments
  - Control flow statements (if-else, loops)
  - Function definitions and calls
  - Basic input/output operations
- Includes both a compiler (generates Python code) and an interpreter (executes directly)
- Error detection and reporting at lexical, syntactic, and semantic levels

## Installation

### Prerequisites
- Python 3.6 or higher

### Setup
1. Clone this repository:
   ```
   git clone https://github.com/Dioninlei/nohtype-compiler.git
   cd nohtype-compiler
   ```

2. The compiler uses only Python standard libraries with no external dependencies.

## Usage

### Basic Compilation
Compile a NohType script (`.rpc` file) to Python:

```bash
python nohtype_compiler.py <input_file.rpc>
```

This will generate a Python file with the same base name as your input file.

### Specifying Output
You can specify an output file name:

```bash
python nohtype_compiler.py <input_file.rpc> <output_file.py>
```

## NohType Language Syntax

### Keywords
NohType reverses all Python keywords:

| Python | NohType |
|--------|---------|
| print  | tnirp   |
| if     | fi      |
| else   | esle    |
| while  | elihw   |
| for    | rof     |
| in     | ni      |
| def    | fed     |
| return | nruter  |
| and    | dna     |
| or     | ro      |
| break  | kaerb   |
| continue | eunitnoc |
| pass   | ssap    |

### Sample NohType Program
```
# Hello World in NohType

fed main():
    tnirp("Hello, World!")
    
    # Ask for user input
    tnirp("What's your name?")
    name = tupni()
    tnirp("Hello, " + name + "!")
    
    # Demonstrate a loop
    rof i ni range(5):
        tnirp(i)
    
    # Demonstrate conditional
    fi name == "Admin":
        tnirp("Welcome, administrator!")
    esle:
        tnirp("Welcome, user!")

main()
```

## Compiler Architecture

The NohType compiler consists of several major components:

1. **Lexer** (`Lexer` class): Breaks the source code into tokens
2. **Parser** (`Parser` class): Builds an Abstract Syntax Tree (AST) from tokens
3. **Semantic Analyzer** (`SemanticAnalyzer` class): Performs semantic checks on the AST
4. **Code Generator** (`CodeGenerator` class): Transforms the AST into standard Python code
5. **Interpreter** (`Interpreter` class): Executes the AST directly without generating code

## Project Structure
```
nohtype-compiler/
├── nohtype_compiler.py    # Main compiler implementation
├── examples/              # Example NohType programs
│   ├── hello_world.rpc    # Simple hello world example
│   ├── fibonacci.rpc      # Fibonacci sequence implementation
│   └── ...
├── tests/                 # Test cases
└── docs/                  # Documentation
    └── project_report.md  # Detailed project report
```

## Contributing
Contributions to this project are welcome. Please feel free to submit a pull request or open an issue for bugs or feature requests.

## License
None

## Author
Nino Ariel

## Acknowledgments
This project was created as an educational exercise in compiler design and implementation.
