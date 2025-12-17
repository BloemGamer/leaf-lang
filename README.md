<!-- [![Forks][forks-shield]][forks-url] -->
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Project_license][license-shield]][license-url]

# S-lang
A language that is designed to be used with C, with modern features and a platform to learn compiler design.

> [!WARNING]
> It is still under construction and cannot be used yet.

## Why S-lang?
- **Explicit systems programming:** Memory, mutability, ownership, and unsafe operations are always visible, nothing happens implicitly.

- **Move-by-default semantics:** Values are moved on assignment and function calls unless explicitly borrowed, eliminating accidental copies and making performance predictable.

- **Automatic memory management without a garbage collector:** Resources are freed deterministically at scope end, with no runtime, no GC, and no hidden reference counting.

- **Lightweight borrow checking:** Borrows are tracked for correctness of intent, not proven with complex lifetime analysis. Multiple mutable borrows and cross-function borrows are allowed by design.

- **Trust-based safety model:** The compiler prevents common errors like use-after-move and invalid access while borrowed, while trusting the programmer to manage borrow lifetimes responsibly.

- **First-class C integration:** Directly import C headers, share ABI-compatible data layouts, and call C code with zero overhead.

- **Unsafe is explicit and contained:** Low-level operations such as raw pointers, manual allocation, and inline assembly are available in unsafe blocks only.

- **Predictable, high performance:** Clear cost models make it suitable for systems and performance-critical code.

- **Power without artificial limits:** The language does not restrict what you can do, it makes powerful operations explicit instead of forbidden.

- **Designed to be understood:** A small, learnable core language that prioritizes clarity over cleverness.


## Code previeuw
```rs
fn main()
{
	i64 a = 0;
	print("{}", a);
}
```


## Roadmap
### Now busy with
- [ ] Writing the lexer
- [ ] Refining the syntax

### Short-term
- [ ] Finish first compiler version

### Long-term
- [ ] Adding templates
- [ ] Code optimisations
- [ ] Preprocessor
- [ ] C parser integration

## Progress of the stages of the compiler
- [ ] Frontend
- [ ] Lexing
- [ ] Preprocessor (lower priority)
- [ ] Parsing → AST
- [ ] Semantic Analysis
    - [ ] Desugaring / Lowering
    - [ ] Symbol Collection
    - [ ] Name Resolution + Type Analysis
    - [ ] Lifetime Analysis & Destructor Insertion
- [ ] Optimizations (optional)

Backend
- Code Generator (to C)
- Invoke C Compiler



## Installation

### Prerequisites
- Compiler
	- **Cargo** that supports the newest rust version
- Generated C code
	- **CMake** >= 3.10
	- **C compiler** with full C23 support (fully tested with GCC 14.2.0)

S-lang is currently under development, but you can build it from source.
The build is tested on **Linux** and **Windows**. macOS is **not currently supported**.

![Linux][Linux-shield] ![Linux Mint][Linux-Mint-shield] ![Windows][Windows-shield]

### Cloning the Repository
```sh
git clone https://github.com/BloemGamer/S-lang.git
cd S-lang
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
S-lang is released under the MIT License. See [LICENSE](LICENSE) for details.

## Contact
Feel free to reach out via [GitHub Discussions](https://github.com/BloemGamer/S-lang/discussions) or [GitHub Isues](https://github.com/BloemGamer/S-lang/issues).


<!-- MARKDOWN LINKS & IMAGES -->
[forks-shield]: https://img.shields.io/github/forks/BloemGamer/S-lang.svg?style=flat
[forks-url]: https://github.com/BloemGamer/S-lang/network/members
[stars-shield]: https://img.shields.io/github/stars/BloemGamer/S-lang.svg?style=flat
[stars-url]: https://github.com/BloemGamer/S-lang/stargazers
[issues-shield]: https://img.shields.io/github/issues/BloemGamer/S-lang.svg?style=flat
[issues-url]: https://github.com/BloemGamer/S-lang/issues
[license-shield]: https://img.shields.io/github/license/BloemGamer/S-lang.svg?style=flat
[license-url]: https://github.com/BloemGamer/S-lang/blob/main/LICENSE

[Linux-Mint-shield]: https://img.shields.io/badge/Linux%20Mint-87CF3E?logo=linuxmint&logoColor=fff
[Linux-shield]: https://img.shields.io/badge/Linux-FCC624?logo=linux&logoColor=black
[Windows-shield]: https://custom-icon-badges.demolab.com/badge/Windows-0078D6?logo=windows11&logoColor=white
