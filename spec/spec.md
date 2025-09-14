# Black Magic Language Specification 

## Table of Contents

1. [Essence](#essence)  
2. [Lexical & Tokens](#lexical--tokens)  
3. [Modules & Visibility](#modules--visibility)  
4. [Modes & Effect Seeds](#modes--effect-seeds)  
5. [Circles (Transactions)](#circles-transactions)  
6. [Functions](#functions)  
   - [Spells (White-leaning)](#spells-white-leaning)  
   - [Rituals (Black)](#rituals-black)  
7. [Runes (Hygienic AST Rewriting)](#runes-hygienic-ast-rewriting)  
8. [Pacts & Linearity](#pacts--linearity)  
9. [Types & Generics](#types--generics)  
10. [Black-Mode Pointer Arcana](#black-mode-pointer-arcana)  
11. [Expressions, Statements, Control](#expressions-statements-control)  
12. [Effects (Wards)](#effects-wards)  
13. [Concurrency](#concurrency)  
14. [Standard Library Surface](#standard-library-surface)  
15. [Diagnostics](#diagnostics)  
16. [Grammar (EBNF)](#grammar-ebnf)  
17. [End-to-End Examples](#end-to-end-examples)  
18. [Implementation Notes](#implementation-notes)

---

## Essence

**Black Magic** is a dual-faced language:

- **White Mode** — deterministic, strict, effect-safe, no raw pointers by default.  
- **Black Mode** — ritualistic, rune-friendly, **raw pointer arcana** (C-like) gated by `hex::void`.

**Surface**: Rust-like paths (`std::io::println`), braces `{}`, semicolons `;`.  
**Modules**: `summon` only.  
**Runes**: AST rewrite macros invoked as `name!(...)`.  
**Semantics**: circles (transaction scopes), wards/effects, linear offerings, rune hygiene.

---

## Lexical & Tokens

- **Identifiers**: `[A-Za-z_][A-Za-z0-9_]*`
- **Literals**:  
  - Int: `0`, `42`, `1_000`, optional suffixes `42i32`, `7u64`  
  - Float: `3.14`, `6.02e23`, `1e-3`, optional `f32`, `f64`  
  - Bool: `true`, `false`  
  - String: `"hello"`, raw `r"no escapes"`  
  - `BOTTOM` (bottom/never), `Void` (unit-like return)
- **Collections**: Lists `[1,2]`, Tuples `(a,b)`, Maps `{ key: value }`
- **Comments**: `// line`, `/* block */`
- **Keywords** (reserved):
  ```
  circle spell ritual rune pact form when
  ward calls effects
  summon as seal banish
  let bind const offer consume scry
  attempt otherwise dispel checkpoint rewind wait tick
  if else while for match return break continue
  mode white black veil
  requires yield pass fail
  pub pub(crate) pub(super)
  struct enum type
  void
  BOTTOM Void
  ```
- **Operators**:
  - Arithmetic: `+ - * / %`
  - Comparison: `< <= > >= == !=`
  - Boolean: `and or not`
  - Bitwise: `& | ^ ~ << >>` (pure on integers; allowed in both modes)
  - **Pointer (black)**: unary `&expr` (address-of), unary `*expr` (deref), `ptr + n`, `ptr - n`

---

## Modules & Visibility

```blackmagic
summon std::io as io;
summon std::list as list;
summon io::{print, println};
seal io at "1.2.0";        // toolchain-enforced version pin
// banish io::{print};     // optional unimport
```

- **No star-imports.**  
- **Aliasing**: `as`.  
- **Visibility**: default private; `pub`, `pub(crate)`, `pub(super)` expose items.

**Resolution order**: local module/crate → summoned aliases → std registry → external registries.

---

## Modes & Effect Seeds

**Marking precedence** (nearest wins): Item attribute > circle header > file header > tool default (white).

```blackmagic
mode white;                    // file default (optional)
circle main black { ... }      // circle mode
#[white] spell f() { ... }     // per-item override
```

**Default allowed effect set `C`**:

- **White**: `{}` (pure)  
- **Black**: `{ hex::entropy }`

A **`ward { ... }`** (or black `calls { ... }`) at circle or function level **sets** the allowed effects for that scope (not additive).

**Call rule**: a call is legal iff `Ecallee ⊆ C(call-site)` where `Ecallee` is the callee’s required effects (its `ward/calls`), and `C(call-site)` is the current allowed set.

---

## Circles (Transactions)

```blackmagic
circle phase white {
    ward { hex::io };          // restrict whole circle (optional)

    attempt {
        // journal boundary
    } otherwise {
        // on fail: unwind boundary, then run this
    }

    checkpoint t0; rewind t0;  // time weaving
    wait 50ms; tick 1s;

    // exit: all linear offerings must be consumed
}
```

A **journal** is kept per circle: linear alloc/free, buffered IO (policy-defined), logical time, checkpoints.  
`dispel;` aborts current circle, rolling back its journal.

---

## Functions

### Spells (White-leaning)

```blackmagic
pub spell quicksort(xs: List<Int>) -> List<Int> {
    if (list::length(xs) <= 1) { return xs; }
    let p  = list::head(xs);
    let r  = list::tail(xs);
    let lo = list::filter(r, |x| x <= p);
    let hi = list::filter(r, |x| x >  p);
    return list::append(quicksort(lo), [p], quicksort(hi));
}
```

- Leading `ward { ... }` (optional) sets required effects for the body.

### Rituals (Black)

```blackmagic
#[black]
ritual cast(msg: String) calls { hex::io } {
    chant "let it echo";
    io::println(msg);
}
```

- `calls { ... }` (alias of `ward`) declares required effects (`Efn`).  
- `yield` is a synonym for `return` inside rituals.  
- Arcane keywords are permitted (`chant`, `conjure`, `banish`).

---

## Runes (Hygienic AST Rewriting)

```blackmagic
rune bubble {
    form [A, B | R] when (A > B) -> [B, A | R];
}

let xs2 = bubble!(xs);  // rune expansion, then evaluate
```

- **Invocation** uses `!` (macro-like) to distinguish from normal calls.  
- **Strategy**: top-down, leftmost-outermost, repeat-to-fixed-point.  
- **Fuel**: 1024 rewrites per rune call; exceeding fuel is an error.  
- **Hygiene**: new bindings in RHS are alpha-renamed; `when` guards must be pure.

---

## Pacts & Linearity

```blackmagic
pact Channel<T> requires spend_once {
    spell send(self, msg: T) -> Void;
    spell close(self) -> Void;
}
```

- `offer ^x: T;` introduces a **linear** value (LIVE=1).  
- `consume ^x;` consumes (LIVE=0).  
- Passing to a linear parameter consumes it.  
- **On circle exit**: any LIVE linear → error.

Values of a pact marked `requires spend_once` are **linear** by type.

---

## Types & Generics

- Built-ins: `Int, Float, Bool, String, Void, BOTTOM`
- Parametric types: `List<T>, Tuple<T1,T2,...>, Map<K,V>, Option<T>`
- User types:
  ```blackmagic
  pub struct Pair<T> { left: T; right: T; }
  pub enum Either<L,R> { Left(L); Right(R); }
  type Str = String;
  ```
- Generic functions: `spell id<T>(x: T) -> T { x }`  
- Inference: local HM; effects & linearity tracked orthogonally.

---

## Black-Mode Pointer Arcana

> **Black-only.** Requires `hex::void`. Illegal in white mode.  
> Enabled either by: (a) ritual with `calls { hex::void, ... }`, or (b) a `void { ... }` block.

### Pointer & Reference Types

- **Raw pointer**: `*T` (nullable; pointer arithmetic allowed; unsafe)  
- **References**: `&T` (shared), `&mut T` (exclusive mutable)  
  - References are non-linear and scoped; runtime may assert exclusivity for `&mut`.

### Pointer Operations (Black Only)

- Address-of: `&expr` → obtains pointer/reference  
- Dereference: `*ptr` → reads/writes through pointer (requires `hex::void`)  
- Arithmetic: `ptr + n`, `ptr - n` (scaled by `sizeof(T)`)  
- Comparison: `ptr1 == ptr2`, `<`, `>` (address order)  
- Size/Align: `sizeof(T)`, `alignof(T)`

### `void` Block

```blackmagic
void {
    // inside: hex::void is permitted
    let p: *Int = &x;
    let v: Int  = *p;        // deref
}
```

Syntactic sugar for a nested block where `hex::void` is locally allowed (merged with outer effects).

### Memory & Pointer Std Modules

```blackmagic
summon std::mem as mem;
summon std::ptr as ptr;
```

- `mem::alloc<T>(count: Int) -> *T` — allocates uninitialized memory
- `mem::free<T>(p: *T)` — frees memory allocated for `T`
- `mem::zero<T>(p: *T, count: Int)` — writes zeroes
- `mem::copy<T>(dst: *T, src: *T, count: Int)` — memmove semantics
- `mem::set_bytes(p: *u8, byte: Int, count: Int)` — memset
- `mem::transmute<U,T>(x: T) -> U` — bitcast (dangerous)

- `ptr::offset<T>(p: *T, i: Int) -> *T` — pointer offset
- `ptr::diff<T>(a: *T, b: *T) -> Int` — element distance

All above **require** `hex::void`.

### Casts

- Safe cast helper: `cast<T>(expr)` (library function, runtime-checked)  
- Bitcast: `mem::transmute<U,T>(x)` (no checks; black-only by effect)  
- Optional rune sugar: `hexcast!(U, expr)` → rewrite to `mem::transmute<U, typeof(expr)>(expr)`

---

## Expressions, Statements, Control

- **Statements**:  
  `let`, `bind`, `const`, `offer`, `consume`, `seal entropy;`, `scry expr;`, `attempt { } otherwise { }`, `dispel;`, `checkpoint id;`, `rewind id;`, `wait 2s;`, `tick 50ms;`, `return/break/continue;`
- **Closures**: `|x| x+1` or `|x,y| { x+y }`
- **Match**:
  ```blackmagic
  let out = match xs {
      []                      => 0,
      [x]                     => x,
      [x,y | rest] if x<y     => x+y,
      _                       => list::length(xs),
  };
  ```

**Operator precedence** (high → low):
1. Postfix: call `f(...)`, rune `name!(...)`, path `ns::id`
2. Unary: `-`, `!`, `not`, `*`(deref), `&`(addr-of)  *(pointer meanings: black only)*
3. Multiplicative: `* / %`
4. Additive: `+ -`
5. Shifts: `<< >>`
6. Bitwise: `& ^ |`
7. Comparisons: `< <= > >= == !=`
8. Boolean: `and or`
9. Assignment: `=`

---

## Effects (Wards)

- Effects live under `hex::…`: `hex::io`, `hex::time`, `hex::entropy`, `hex::void`
- **Allowed set `C`** at a point is computed by:
  1) Mode seed (white `{}`, black `{hex::entropy}`)  
  2) Enclosing circle’s `ward { … }` (sets `C`)  
  3) Inside a spell/ritual, that item’s `ward/calls` sets both its **required** set `Efn` and the body’s working `C = Efn`

- **Call legality**: require `Ecallee ⊆ C(call-site)`.

---

## Concurrency

```blackmagic
summon std::async as async;
summon std::io::{println};

circle tasks white {
    ward { hex::io };
    spell run() {
        let a = async::spawn(|| 1);
        let b = async::spawn(|| { println("hi"); return 2; });
        let xs = async::join_all([a, b]);   // -> List<Int>
        println(xs);
    }
}
```

- `spawn` requires the closure body’s effects to be allowed at the call site.  
- `join_all` is pure (waiting has no additional effects).

---

## Standard Library Surface

- `std::io::{print, println, read_line}` → `hex::io`  
- `std::list::{length, head, tail, append, map, filter, sort}` → pure  
- `std::math::{abs, sqrt, pow, min, max}` → pure  
- `std::rand::{random_int, random_bool}` → `hex::entropy`  
- `std::time::{wait, tick, now}` → `hex::time`  
- **Black memory**: `std::mem::{alloc, free, zero, copy, set_bytes, transmute}`, `std::ptr::{offset, diff}` → `hex::void`

---

## Diagnostics

- Effects
  ```
  error[E1102]: ward forbids effect hex::void here
  help: wrap in `void { ... }` or add `calls { hex::void }`
  ```
- Linearity
  ```
  error[L3001]: unspent offering ^res at circle exit
  note: values of pact 'Channel<T>' are spend_once; call 'close'
  ```
- Runes
  ```
  error[R1001]: rune 'bubble' exceeded rewrite fuel (1024)
  ```
- Mode
  ```
  error[M0003]: pointer dereference not allowed in white mode
  ```
- Syntax
  ```
  error[S0203]: 'bubble' is a rune; invoke as 'bubble!(...)'
  ```

---

## Grammar (EBNF)

```ebnf
Program      := { UseDecl | TopDecl } ;

UseDecl      := SummonDecl | SealDecl | BanishDecl ;
SummonDecl   := "summon" Path [ "as" Ident ] ";"
              | "summon" Path "::" "{" Ident { "," Ident } "}" ";" ;
SealDecl     := "seal" Ident "at" String ";" ;
BanishDecl   := "banish" Ident [ "::" "{" Ident { "," Ident } "}" ] ";" ;

ModeHeader   := "mode" ( "white" | "black" ) ";" ;
Attr         := "#[" ( "white" | "black" ) "]" ;

TopDecl      := Circle | Spell | Ritual | Rune | Pact | Struct | Enum | TypeAlias ;

Circle       := "circle" Ident ( "white" | "black" )? Block ;

Spell        := Attr? Visibility? "spell" Ident TypeParams? "(" Params? ")" Ret? Block ;
Ritual       := Attr? Visibility? "ritual" Ident TypeParams? "(" Params? ")" Calls? Block ;
Calls        := "calls" EffectSet ;
Ret          := "->" Type ;

Rune         := Visibility? "rune" Ident "{" { Form } "}" ;
Form         := "form" Pattern "->" Rewrite [ "when" Expr ] ";" ;

Pact         := Visibility? "pact" Ident TypeParams? Requires? "{" { Sig } "}" ;
Requires     := "requires" "spend_once" ;
Sig          := "spell" Ident "(" Params? ")" "->" Type ";" ;

Struct       := Visibility? "struct" Ident TypeParams? "{" { Field } "}" ;
Field        := Ident ":" Type ";" ;
Enum         := Visibility? "enum" Ident TypeParams? "{" { Variant } "}" ;
Variant      := Ident [ "(" Type { "," Type } ")" ] ";" ;
TypeAlias    := Visibility? "type" Ident "=" Type ";" ;

Params       := Param { "," Param } ;
Param        := Ident [ ":" Type ] ;
TypeParams   := "<" Ident { "," Ident } ">" ;

EffectSet    := "{" Effect { "," Effect } "}" ;
Effect       := "hex::" Ident ;

Stmt         := VarDecl | Offer | Consume | SealStmt | Scry
              | Attempt | Disp | Check | Rewind | Wait | Tick
              | VoidBlock | MatchStmt | ExprStmt ;
VarDecl      := ("let" | "bind") Ident ("=" Expr)? ";"
              | "const" Ident ":" Type "=" Expr ";" ;
Offer        := "offer" "^"Ident ":" Type ";" ;
Consume      := "consume" "^"Ident ";" ;
SealStmt     := "seal" "entropy" ";" ;
Scry         := "scry" Expr ";" ;
Attempt      := "attempt" Block "otherwise" Block ;
Disp         := "dispel" ";" ;
Check        := "checkpoint" Ident ";" ;
Rewind       := "rewind" Ident ";" ;
Wait         := "wait" Number TimeUnit ";" ;
Tick         := "tick" Number TimeUnit ";" ;
VoidBlock    := "void" Block ;                 // BLACK-ONLY, enables hex::void locally
MatchStmt    := "match" Expr Block ;
ExprStmt     := Expr ";" ;

Block        := "{" { Stmt } "}" ;
ArgList      := Expr { "," Expr } ;

Expr         := Assign ;
Assign       := OrExpr | Ident "=" Expr ;
OrExpr       := AndExpr { "or" AndExpr } ;
AndExpr      := BitOrExpr { "and" BitOrExpr } ;

BitOrExpr    := BitXorExpr { "|" BitXorExpr } ;
BitXorExpr   := BitAndExpr { "^" BitAndExpr } ;
BitAndExpr   := CmpExpr { "&" CmpExpr } ;

CmpExpr      := ShiftExpr { CmpOp ShiftExpr } ;
CmpOp        := "==" | "!=" | "<" | "<=" | ">" | ">=" ;

ShiftExpr    := AddExpr { ("<<" | ">>") AddExpr } ;
AddExpr      := MulExpr { ("+"|"-") MulExpr } ;
MulExpr      := Unary { ("*"|"/"|"%") Unary } ;

Unary        := ( "-" | "!" | "not" | "*" | "&" )? Primary ;  // '*' deref, '&' addr-of (BLACK pointer semantics)
Primary      := Literal | Ident | Path | Call | RuneCall | List | Tuple | Map
              | "(" Expr ")" | Closure ;

Call         := (Ident | Path) "(" ArgList? ")" ;
RuneCall     := Ident "!" "(" ArgList? ")" ;
Closure      := "|" ParamList? "|" (Expr | Block) ;
ParamList    := Ident { "," Ident } ;

Pattern      := ListPat | Ident | Literal | "_" ;
ListPat      := "[" [ PatItem { "," PatItem } ] "]" ;
PatItem      := Ident | Literal | ConsPat ;
ConsPat      := PatItem "|" Ident ;

Type         := PtrType | RefType | Path | Path "<" Type { "," Type } ">" ;
PtrType      := "*" Type ;                       // RAW pointer (BLACK usage only)
RefType      := "&" "mut"? Type ;                // references
Path         := Ident { "::" Ident } ;

Literal      := Number | String | "true" | "false" | "BOTTOM" ;
Number       := Int | Float ;
TimeUnit     := "ms" | "s" ;
```

---

## End-to-End Examples

### White quicksort

```blackmagic
mode white;
summon std::io as io;
summon std::list as list;
summon io::{println};
summon list::{length, head, tail, filter, append};

circle sort_demo white {
    spell quicksort(xs: List<Int>) -> List<Int> {
        if (length(xs) <= 1) { return xs; }
        let p  = head(xs);
        let r  = tail(xs);
        let lo = filter(r, |x| x <= p);
        let hi = filter(r, |x| x >  p);
        return append(quicksort(lo), [p], quicksort(hi));
    }
    spell run() { println(quicksort([5,3,8,1,4])); }
}
```

### Black rune + pointers

```blackmagic
summon std::io::{println};
summon std::mem as mem;
summon std::ptr as ptr;

rune bubble { form [A, B | R] when (A > B) -> [B, A | R]; }

circle rawplay black {
    ritual noisy_sort(xs: List<Int>) calls { hex::io } {
        println(bubble!(xs));
    }

    ritual demo_ptrs() calls { hex::io, hex::void } {
        let n: Int = 4;
        let p: *Int = mem::alloc<Int>(n);
        void {
            *(p + 0) = 10;
            *(p + 1) = 20;
            *(p + 2) = 30;
            *(p + 3) = 40;
        }
        println(*(p + 2)); // 30
        mem::free<Int>(p);
    }
}
```

---

## Implementation Notes

- **Parser**: brace language; attributes; `summon` forms; `rune!` calls; pointer tokens `*`/`&` in both type and expr positions (enforce legality at checker).  
- **Resolver**: alias table for `summon`; no wildcard imports; collision detection.  
- **Type/Effect checker**: compute `Efn` per item; compute `C` per call site from mode + wards; enforce `Ecallee ⊆ C`.  
- **Linearity**: per-scope counters for offerings and `spend_once` types; assert all consumed on circle exit.  
- **Rune engine**: AST rewrite with hygiene; fuel=1024 per invocation.  
- **Runtime/IR**: circle journal (linear, IO buffers, time, checkpoints); sealed RNG per circle; `attempt/otherwise` boundary tracking.  
- **Black pointer ops**: gate deref/arith behind `hex::void`; provide `std::mem`/`std::ptr` intrinsics.

---

*End of Black Magic v1.3 Specification.*
