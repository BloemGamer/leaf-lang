<!-- [![Forks][forks-shield]][forks-url] -->
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Project_license][license-shield]][license-url]
[![Status](https://img.shields.io/badge/status-in%20development-orange)](https://github.com/BloemGamer/leaf-lang)


> [!NOTE]
> Leaf-lang is still in active development.
> It technically works, but is not ready to be used.

# Leaf-lang
Leaf-lang is a functional systems programming language designed for interoperability with C, inspired by Rust and Haskell.

## Code preview
```leaf
// Note: Leaf-lang compiler is under development. This example is illustrative only.
fn main :: ()
{
	let a: u64 = 10 :> fib(); // pipe 10 into fib(), equivalent to fib(10)
}

fn fib :: n: u64 -> u64 {
	return switch n {
		0 => 0,
		1 => 1,
		_ => fib(n - 1) + fib(n - 2),
	};
}
```

<!-- # Features -->
<!-- - **Simple language**: No hidden control flow. Simple but powerful preprocessor. -->
<!-- - **Explicit memory**: No hidden memory allocations, deallocations are predictable. -->
<!-- - **Fast by design**: No garbage collector. Strong compile-time execution and lazy evaluation. -->

<details>
<summary><strong>Roadmap</strong></summary>

<br/>

### Current focus
- [ ] Refining the syntax

### Still actively being developed

### Short-term
- [ ] Finish first compiler version

### Long-term

### Language features

### Compiler infrastructure

</details>


<details>
<summary><strong>Compiler Pipeline Progress</strong></summary>

<br/>

## Frontend (Source -> Mir)

### Pre-parsing (Source -> PreParsed)
- [x] **Lexing**
	- Source text -> tokens
- [ ] **Pre-parsing**
	- Tokens -> top-level definitions + map of all definitions
- [ ] **Reflection**
	- top-level definitions -> more top-level definitions
- [ ] **Pre-parser Collection**
	- top-level definitions -> top-level definitions with ID

---

### Parsing + Semantic Analysis (PreParsed -> TypedHIR)
- [ ] **Full Parsing + Collection**
	- collecting local symbols
	- mapping all locals
	- generating block-based UntypedHIR

- [ ] **Type Checking + Resolution**
	- UntypedHIR -> TypedHIR

- [ ] **Borrow Checking**
	- Lifetime resolution

---

### Optimisations + Lowering
- [ ] **Generic Sea of Nodes**
	- TypedHIR -> Generic Sea of Nodes (GSoNMIR)
- [ ] **Optimisations**
- [ ] **Monomorphisation**
	- Solving all generics in the SoNMIR (GSoNMIR -> MSoNMIR)
	- Removing unused functions
- [ ] **Optimisations**
- [ ] **MIR**
	- MSoNMIR -> block-based MIR

---

## Backend (MIR -> C)
- [ ] **Code Generation**
	- MIR -> C source code
- [ ] **C Compilation**
	- Invoke system C compiler (C23)


## Backend (MIR -> LLVM) (low priority)
- [ ] **Code Generation**
	- MIR -> LLVM IR
- [ ] **LLVM Compilation**
	- Invoke LLVM

</details>




## Installation

### Prerequisites
- Compiler
	- **Cargo** that supports the latest Rust version
- Generated C code
	- **C compiler** with full C23 support (currently tested and supported with GCC 16 and Clang 22)

Leaf-lang is currently under development, but you can build it from source.
The build is tested on **Linux** and **Windows**. MacOS is **not currently supported**.
<!-- I don't have a MacOS device, so I can't test MacOS, but probably the compiler works, but some functions of the stdlib not -->

![Linux][Linux-shield] ![Windows][Windows-shield]

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

[Linux-shield]: https://img.shields.io/badge/Linux-FCC624?logo=linux&logoColor=black
[Windows-shield]: https://custom-icon-badges.demolab.com/badge/Windows-0078D6?logo=windows11&logoColor=white
