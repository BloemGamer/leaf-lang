<!-- [![Forks][forks-shield]][forks-url] -->
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Project_license][license-shield]][license-url]

# S-lang
S-lang is a language compiling to C, aiming to offer more complex features than C while serving as a platform for learning compiler design.

> [!WARNING]
> It is still under construction and cannot be used yet


## Roadmap
### Short-term
- [ ] Finish first compiler version
- [ ] Add tests
- [ ] Write documentation

### Long-term
- [ ] Code optimisations
- [ ] Preprocessor
- [ ] C parser integration

## Progress of the stages of the compiler
- [ ] Preprocessor (this will be done the last I think)
- [x] Lexing -> lex
- [ ] Parsing -> parse
- [ ] Semantic analyser
- [ ] Optimisations (maybe, so later)
- [ ] Code generator
- [ ] Calling C compiler

## Installation

### Prerequisites
- CMake >= 3.10
- Any C compiler that has C23 fully implemented (Fully tested with GCC 14.2.0)

Currently, S-lang is under development, but you can build it from source:
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
Feel free to reach out via issues or my GitHub profile.


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
