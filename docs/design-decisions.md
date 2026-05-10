# Leaf-lang Design Decisions

This document explains key design decisions behind Leaf-lang, including language features, compiler architecture, memory management, and backend design.

---

## 1. Language Design

### Target Audience & Philosophy
- **Target users:** Mostly C, C++ and rust developers that want to try a new language
- **Design philosophy:** Everything is explicit while being mostly safe
- **Tradeoffs:** Some of the design choices will make writing very optimized code a little harder

---

## 2. Compiler Architecture

### Why C backend
- **Familiarity** I'm personally also a C dev, so I can better debug C IR than LLVM IR
- **Better compatibility** C compilers are developed for even more architectures and operating systems
- **Good C intergration** Because of that the compiler can generate C code, writing header files for C is very easy to implement
