<!-- [![Forks][forks-shield]][forks-url] -->
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Project_license][license-shield]][license-url]

# S-lang
A language I made that compiles to C.

> [!WARNING]
> It is still under construction and cannot be used yet


## Roadmap
- [ ] finish the first version of the compiler
- [ ] add (automated) tests
- [ ] write proper documentation
- [ ] some example files, and hopefully a simple project to test and show a lot of features
- [x] write a very simple macro based variable length array type and use that instead of doing it every time by hand
- [ ] adding a C parser, for parsing at least the types, so you don't have to define it yourself
- [ ] make the code thread safe, aka rewrite the tokeniser with the error things

## Progress of the stages of the compiler
- [ ] preprocessor (this will be done the last I think)
- [x] lexing -> lex
- [ ] parsing -> parse
- [ ] semantic analyzer
- [ ] optimalisations (maybe, so later)
- [ ] code generator
- [ ] calling C compiler

## Installation
Currently, S-lang is under development, but you can build it from source:
The build is tested for Linux and Windows. macOS is not currently supported.
[![Linux][Linux-shield]] [![Linux_mint][Linux-Mint-shield]] [![Windows][Windows-shield]]

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
