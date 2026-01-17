# Leaf-lang Design Decisions

This document explains key design decisions behind Leaf-lang, including language features, compiler architecture, memory management, and backend design.

---

## 1. Language Design

### 1.1 Target Audience & Philosophy
- **Target users:** Mostly C, C++ and rust developers that want to try a new language
- **Design philosophy:** Everything is explicit while being mostly safe
- **Tradeoffs:** Some of the design choices will make writing very optimized code a little harder

---

### 1.2 Memory & Ownership
- **Decision:** If you pass a struct that does not have a copy, the function should own it
- **Rationale:**
	- No confusion about who owns the data
	- No unnecessary cloning of structures you don't need anymore
- **Impact:** Sometimes some references are used than would be optimal

---

### 1.3 Syntax Choices
- **Decision:** Very explicit IO and memory allocations (the allocations syntax still being developed)
- **Rationale:**
	- Every allocation is seen and can be optimised if you know what you need
- **Rationale:** Very explicit, and it's possible to have terminal like piping in the language

---

## 2. Compiler Architecture


### 2.1 Multi-Stage Pipeline
- **Stages:**
	1. **Lexing:** Convert raw source code into tokens
	2. **Parsing:** Tokens -> Concrete Syntax Tree (CST) -> Abstract Syntax Tree (AST)
	3. **Semantic Analysis / HIR Generation:** Perform symbol resolution, type checking, ownership/lifetime analysis, then lower AST -> High-Level IR (HIR)
	4. **MIR Generation:** Lower HIR -> Mid-level IR (MIR) for optimizations and backend-agnostic representation
	5. **Backend(s):** Generate target code from MIR. Currently supported:
		- **C Backend:** MIR -> C code -> compile with system C compiler
		- **LLVM Backend:** MIR -> LLVM IR -> compile with LLVM toolchain

- **Rationale:**
	- Separating the compiler into multiple stages keeps responsibilities clear.
	- MIR acts as a **shared backend intermediate representation**, enabling multiple backends without duplicating analyses or optimizations
	- Each stage provides a natural checkpoint for testing and diagnostics, which is very handy for tests

- **Tradeoffs:**
	- Multi-stage compilation increases initial implementation complexity compared to a simple direct compiler (AST -> C)
	- Each stage makes the design more difficult, but makes each stage very clear, and ease of adding things later

---

### 2.2 MIR as Unified IR
- **Decision:** MIR should be as low as possible
- **Rationale:** I have to implement as little backend specific code
- **Impact:** The backends will be smaller, so easier to maintain

---

### 2.3 Backend Design
- **C Backend:** Easier to implement, and debug, and have the best C support and runs on everything
	- **C version** C23, because C23 is almost fully supported, and has features like `constexpr` which are very usefull for libraries
- **LLVM Backend:** More freedom for the language, and I want to learn how LLVM works
- **Rationale:** It's fun to try to make both work, and I could add specefic features to each one
- **Tradeoffs:** 2 backends would be more work, so I will try to make the MIR as uniform and easy as possible so I don't have to change that one very often

---

<!--

### 2.4 Diagnostics & Error Handling
- **Decision:** [Structured errors / spans / codes]
- **Rationale:** [Improve developer experience / clarity]

---

## 3. Language Features & Limitations

### 3.1 Explicit Control Flow
- **Decision:** [No hidden control flow]
- **Rationale:** [Predictable behavior, easier reasoning]

### 3.2 Compile-Time Execution
- **Decision:** [Support lazy or compile-time execution]
- **Tradeoff:** [Extra analysis at compile time]

### 3.3 Planned But Low-Priority Features
- [List features e.g., tagged unions, templates, lambdas]
- **Rationale:** [Why deferred]

-->
