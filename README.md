<!-- [![Forks][forks-shield]][forks-url] -->
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Project_license][license-shield]][license-url]

# S-lang
A C-based language with modern features and a platform to learn compiler design.

> [!WARNING]
> It is still under construction and cannot be used yet.

## Why S-lang?
* **Compatibility:** The language is designed to be compatible with C.
* **Clearer syntax:** The language tries to change some of the syntax to make the language more readable.
* **Extra features:** More features to make development easier.


## Code previeuw
```rs
@import "stdio.h" 							// #include <stdio.h> in C

fn main(i64 argc, char&& argc) -> int
{
	i64 a = 10;
	i64& b = i64&::give_clean_array(10);	// now you can use references, next to pointers
	mut i64 c = 0;
	c = { a + b[0] };
	print("{}\n", c);						// print got a new format version
}

pub fn i64&::give_clean_array(usize size) -> i64&
{
	return (i64&)calloc(size, sizeof(i64));	// you can still use every C function you want
}
```


## Roadmap
### Now busy with
- [ ] Rewriting code so it is more clean

### Short-term
- [ ] Finish first compiler version
- [ ] Adding the namespace like syntax
- [ ] Add the ability to use documenting comments
- [ ] Add tests
- [ ] Write documentation
- [x] Make the lexer threadsafe
- [ ] Add error handeling in the lexer
- [ ] Add error handeling in the parser
- [ ] Add error handeling in the code generation
- [ ] Add a switch or match statement

### Long-term
- [ ] Adding templates
- [ ] Code optimisations
- [ ] Preprocessor
- [ ] C parser integration

## Progress of the stages of the compiler
- [ ] Preprocessor (Low priority)
- [x] Lexing -> lex
- [x] Parsing -> parse
- [ ] Semantic analyser
- [ ] Optimisations (Low priority)
- [x] Code generator
- [ ] Calling C compiler

## Installation

### Prerequisites
- CMake >= 3.10
- Any C compiler that has C23 fully implemented (Fully tested with GCC 14.2.0)

Currently, S-lang is under development, but you can build it from source.
The build is tested for Linux and Windows. macOS is not currently supported.

![Linux][Linux-shield] ![Linux_mint][Linux-Mint-shield] ![Windows][Windows-shield]

```sh
git clone https://github.com/BloemGamer/S-lang.git
cd S-lang
cmake -S . -B cmake-build-debug -DCMAKE_BUILD_TYPE=Debug
cmake -S . -B cmake-build-release -DCMAKE_BUILD_TYPE=Release
```
```sh
cmake --build cmake-build-debug		# Debug
cmake --build cmake-build-release	# Release
```

## License
S-lang is released under the MIT License. See [LICENSE](LICENSE) for details.

## Contact
Feel free to reach out via [GitHub Discussions](https://github.com/BloemGamer/S-lang/discussions) or [GitHub Isues](https://github.com/BloemGamer/S-lang/issues).


<!-- MARKDOWN LINKS & IMAGES -->
[forks-shield]: https://img.shields.io/github/forks/BloemGamer/S-lang.svg?style=for-the-badge
[forks-url]: https://github.com/BloemGamer/S-lang/network/members
[stars-shield]: https://img.shields.io/github/stars/BloemGamer/S-lang.svg?style=for-the-badge
[stars-url]: https://github.com/BloemGamer/S-lang/stargazers
[issues-shield]: https://img.shields.io/github/issues/BloemGamer/S-lang.svg?style=for-the-badge
[issues-url]: https://github.com/BloemGamer/S-lang/issues
[license-shield]: https://img.shields.io/github/license/BloemGamer/S-lang.svg?style=for-the-badge
[license-url]: https://github.com/BloemGamer/S-lang/blob/main/LICENSE

[Linux-Mint-shield]: https://img.shields.io/badge/Linux%20Mint-87CF3E?logo=linuxmint&logoColor=fff
[Linux-shield]: https://img.shields.io/badge/Linux-FCC624?logo=linux&logoColor=black
[Windows-shield]: https://custom-icon-badges.demolab.com/badge/Windows-0078D6?logo=windows11&logoColor=white
