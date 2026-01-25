<!-- [![Forks][forks-shield]][forks-url] -->
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Project_license][license-shield]][license-url]
[![Status](https://img.shields.io/badge/status-in%20development-orange)](https://github.com/BloemGamer/leaf-lang)


# Leaf-lang
A language that is designed to be used with C, with modern features and a platform to learn compiler design.

## Code preview
```leaf
// Note: Leaf-lang compiler is under development. This example is illustrative only.
fn main()
{
	var a: i64 = 0;
	var v: Vec<i64> = Vec::with_len!(5);
	print!("{}", a);
}
```

# Features
- **Simple language**: No hidden control flow. Simple but powerful preprocessor.
- **Explicit memory**: No hidden memory allocations, deallocations are predictable.
- **Fast by design**: No garbage collector. Strong compile-time execution and lazy evaluation.

<details>
<summary><strong>Roadmap</strong></summary>

<br/>

### Current focus

### Still actively being developed
- [x] Writing the lexer
- [x] Writing the parser
- [x] Writing the desugarer
- [ ] Refining the syntax

### Short-term
- [ ] Finish first compiler version

### Long-term
- [ ] Adding variants
- [ ] Adding templates
- [ ] Adding lambda functions
- [ ] Preprocessor
- [ ] Code optimisations
- [ ] C parser integration
- [ ] Design a build system
- [ ] Write compile-time execution
- [ ] Adding a LLVM backend (very low priority)

</details>


<details>
<summary><strong>Compiler Pipeline Progress</strong></summary>

<br/>

Leaf-lang uses a multi-stage compiler pipeline with explicit lowering steps.
Each stage transforms the program into a simpler or more constrained form.

### Frontend (Source -> AST)

- [x] **Lexing**
	- Source text -> tokens
- [ ] **Preprocessing** (low priority)
	- Macro expansion
- [x] **Parsing**
	- Tokens -> Abstract Syntax Tree (AST)
- [ ] **AST-macros** (low priority)
	- Another macro expansion round (AST -> AST)
- [x] **AST Normalization**
	- AST -> simplified AST
	- Syntax sugar removal (Desugaring)
	- Normalizes multiple ways to write the same thing

---

### Semantic Analysis (AST -> HIR)

- [ ] **Symbol Collection**
	- Scope creation
	- Symbol tables
- [ ] **Name Resolution**
	- Identifier binding
	- Shadowing rules
- [ ] **Type Analysis**
	- Type checking
- [ ] **Basic ownership validation**
	- Checking ownerships
- [ ] **Lowering to HIR**
	- AST -> High-Level IR (HIR)

---

### Lifetime Analysis + Optimization (HIR)

- [ ] **Lifetime Analysis**
	- Ownership rules
	- Destructor insertion
- [ ] **HIR Optimizations** (low priority)

---

### Lowering (HIR -> MIR)

- [ ] **Lowering to MIR**
	- Control-flow normalization


### Backend (MIR -> C)
- [ ] **Code Generation**
	- MIR -> C source code
- [ ] **C Compilation**
	- Invoke system C compiler (C23)

</details>




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
```

### Building the Project

```sh
cargo build --release
```

### Running the Compiler

```sh
cargo run --release
```

## Syntax
The full language grammar is in [syntax.ebnf](syntax.ebnf).

> [!NOTE]
> The syntax will most likely change in the future, and the `syntax.ebnf` is not always very up to date


## License
Leaf-lang is released under the Apache 2.0 License. See [LICENSE](LICENSE) for details.

## Contact
Feel free to reach out via [GitHub Discussions](https://github.com/BloemGamer/leaf-lang/discussions) or [GitHub Issues](https://github.com/BloemGamer/leaf-lang/issues).


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
