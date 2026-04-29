# naive-lab-compiler

A SysY language compiler written in Rust, part of the PKU compiler principle course project.

## Features

- **Frontend**: LALRPOP parser for the full SysY language (variables, constants, arrays, functions, control flow)
- **IR**: AST → Koopa IR translation with constant folding optimization
- **Register Allocation**: Linear scan register allocator with caller-saved-first strategy
- **Backends**:
  - `-koopa` : Koopa IR (textual intermediate representation)
  - (default) : RISC-V 32-bit assembly (RV32IM)
  - `-x64`    : x86-64 assembly (AT&T syntax, System V AMD64 ABI)
  - `-la64`   : LoongArch64 assembly (LP64D / New World ABI)

## Architecture

```
SysY Source (.c)
    |
    v
[parser] (LALRPOP)  →  AST
    |
    v
[irgen]  →  Koopa IR  (with constant folding)
    |
    +-- -koopa  →  Koopa IR text
    |
    +-- default →  [rvgen]   →  RISC-V asm  (structured → peephole → emit)
    |
    +-- -x64    →  [x86gen]  →  x86-64 asm   (structured → peephole → emit)
    |
    +-- -la64   →  [lagen]   →  LoongArch asm (structured → peephole → emit)
```

## Code Generation Pipeline

Each backend follows the same pipeline:

```
Koopa IR → liveness analysis → linear scan register allocation
         → build Vec<Inst> (structured instructions)
         → sliding window peephole optimization (2-3 instruction windows)
         → emit assembly text
```

### Peephole Optimizations

Common optimization rules applied during the sliding window pass:

| Rule | Pattern | Optimization |
|------|---------|-------------|
| Dead store elimination | `store X, N(sp)` → `store Y, N(sp)` | Keep only last store |
| Redundant load after store | `store X, N(sp)` → `load X, N(sp)` | Remove load |
| Constant folding | `li r, 0` → `add r, s, r` | Replace with `mv r, s` |
| Redundant move | `mv r, r` | Remove entirely |
| Move chain | `mv a, b` → `mv c, a` | Fold to `mv a, b` → `mv c, b` |
| Store-load cycle | `sw a0, N(sp)` → `mv t1, a0` → `lw a0, N(sp)` | Drop the reload |

## Usage

```bash
# Build
cargo build --release

# Koopa IR output
./target/release/compiler-principle-lab -koopa input.c -o output.koopa

# RISC-V assembly (default)
./target/release/compiler-principle-lab input.c -o output.S

# x86-64 assembly
./target/release/compiler-principle-lab -x64 input.c -o output.S

# LoongArch64 assembly
./target/release/compiler-principle-lab -la64 input.c -o output.S
```

## Register Allocation

Linear scan register allocation with a two-pool strategy:

| Backend | Caller-saved (priority) | Callee-saved (fallback) |
|---------|------------------------|------------------------|
| RISC-V  | t1-t6 (6 regs)         | s1-s11 (11 regs)        |
| x86-64  | r8-r11 (4 regs)        | r12-r15, rbx (5 regs)   |
| LoongArch | t0-t7 (8 regs)       | s0-s8 (9 regs)          |

Caller-saved registers are allocated first (zero overhead for leaf functions). Callee-saved registers are used when caller-saved pool is exhausted, with prologue save / epilogue restore.

## Project Structure

```
src/
  main.rs           — Entry point, CLI argument parsing
  parser/
    mod.rs           — Parser wrapper
    sysy.lalrpop     — LALRPOP grammar for SysY language
  types/
    ast/mod.rs       — AST types, symbol table, const folding
  irgen/
    mod.rs           — AST → Koopa IR code generation
  asm/
    mod.rs           — AsmEmit trait, peephole optimization framework
  rvgen/
    mod.rs           — RISC-V backend
  x86gen/
    mod.rs           — x86-64 backend
  lagen/
    mod.rs           — LoongArch64 backend
  y86gen/
    mod.rs           — Y86 backend (experimental, incomplete)
test/                 — SysY test programs
```

## Dependencies

- **koopa** (0.0.7): IR data structures and utilities
- **lalrpop** / **lalrpop-util** (0.20.2): Parser generator

## Copyright

Copyright 2024-2026 Li Bohai, licensed under GPLv3. See LICENSE for details.

## Academic Integrity

Due to libkoopa being licensed under GPLv3, this project must be licensed under GPLv3. However, please finish your course project independently. Use this as a reference for better style, features, and performance.

From the second commit, vibe coding was used and the code has not been fully tested yet, use at your own risks.
