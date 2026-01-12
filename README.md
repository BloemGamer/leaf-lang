<!-- [![Forks][forks-shield]][forks-url] -->
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Project_license][license-shield]][license-url]


# Leaf-lang
A language that is designed to be used with C, with modern features and a platform to learn compiler design.

> [!WARNING]
> It is still under construction and cannot be used yet.

## Code preview
```rs
fn main()
{
	var a: i64 = 0;
	var v: Vec<i64> = Vec::with_len!(5);
	print("{}", a);
}
```

# Features
- **Simple language**: No hidden control flow. Simple but powerfull preprocessor.
- **Explicit memory**: No hidden memory allocations, deallocations are predictable.
- **Fast by design**: No garbage collector. Strong comptime excecution and lazy evaluation.

## Roadmap
### Now busy with
- [x] Writing the lexer
- [x] Writing the parser
- [ ] Writing the desugarer
- [ ] Refining the syntax

### Short-term
- [ ] Finish first compiler version

### Long-term
- [ ] Adding tagged unions (variants)
- [ ] Adding templates
- [ ] Adding lambda functions
- [ ] Preprocessor
- [ ] Code optimisations
- [ ] C parser integration
- [ ] Design a build system
- [ ] Write compile time execution
- [ ] Adding a LLVM backend (very low priority)

## Progress of the stages of the compiler
### Frontend
- [x] Lexing
- [ ] Preprocessor (lower priority)
- [x] Parsing -> AST
- [ ] AST macro -> AST to AST
- [ ] Semantic Analysis
	- [ ] Desugaring / Lowering
	- [ ] Symbol Collection
	- [ ] Name Resolution + Type Analysis
	- [ ] AST -> HIR
	- [ ] Lifetime Analysis & Destructor Insertion
	- [ ] HIR -> MIR
- [ ] Optimizations (low priority)

### Backend
- [ ] Code Generator (to C)
- [ ] Invoke C Compiler



## Installation

### Prerequisites
- Compiler
	- **Cargo** that supports the newest rust version
- Generated C code
	- **CMake** >= 3.10
	- **C compiler** with full C23 support (fully tested with GCC 14.2.0)

Leaf-lang is currently under development, but you can build it from source.
The build is tested on **Linux** and **Windows**. macOS is **not currently supported**.

![Linux][Linux-shield] ![Linux Mint][Linux-Mint-shield] ![Windows][Windows-shield]

### Cloning the Repository
```sh
git clone https://github.com/BloemGamer/leaf-lang.git
cd leaf-lang
````

### Building the Project

```sh
cargo build --release
```

### Running the Compiler

```sh
cargo run --release
```


## License
Leaf-lang is released under the MIT License. See [LICENSE](LICENSE) for details.

## Contact
Feel free to reach out via [GitHub Discussions](https://github.com/BloemGamer/leaf-lang/discussions) or [GitHub Isues](https://github.com/BloemGamer/leaf-lang/issues).


<!-- MARKDOWN LINKS & IMAGES -->
[forks-shield]: https://img.shields.io/github/forks/BloemGamer/leaf-lang.svg?style=flat
[forks-url]: https://github.com/BloemGamer/leaf-lang/network/members
[stars-shield]: https://img.shields.io/github/stars/BloemGamer/leaf-lang.svg?style=flat
[stars-url]: https://github.com/BloemGamer/leaf-lang/stargazers
[issues-shield]: https://img.shields.io/github/issues/BloemGamer/leaf-lang.svg?style=flat
[issues-url]: https://github.com/BloemGamer/leaf-lang/issues
[license-shield]: https://img.shields.io/github/license/BloemGamer/leaf-lang.svg?style=flat
[license-url]: https://github.com/BloemGamer/leaf-lang/blob/main/LICENSE

[Linux-Mint-shield]: https://img.shields.io/badge/Linux%20Mint-87CF3E?logo=linuxmint&logoColor=fff
[Linux-shield]: https://img.shields.io/badge/Linux-FCC624?logo=linux&logoColor=black
[Windows-shield]: https://custom-icon-badges.demolab.com/badge/Windows-0078D6?logo=windows11&logoColor=white
