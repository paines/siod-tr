# Octonion API Documentation - SIOD-TR

## Table of Contents

1. [Overview](#overview)
2. [Mathematical Background](#mathematical-background)
3. [C API Reference](#c-api-reference)
4. [Scheme API Reference](#scheme-api-reference)
5. [Examples](#examples)
6. [Common Pitfalls](#common-pitfalls)
7. [Performance Notes](#performance-notes)

---

## Overview

The SIOD-TR octonion library provides complete support for 8-dimensional octonion arithmetic. Octonions (𝕆) are the largest normed division algebra, extending the pattern:

```
ℝ (1D) → ℂ (2D) → ℍ (4D) → 𝕆 (8D)
```

### Key Properties

- **Non-associative**: `(a×b)×c ≠ a×(b×c)` in general
- **Non-commutative**: `a×b ≠ b×a` in general
- **Normed division algebra**: `|a×b| = |a| × |b|` (preserved!)
- **Alternative**: `(a×a)×b = a×(a×b)` and `a×(b×b) = (a×b)×b`

### Representation

An octonion is represented as 8 real components:

```
o = e₀ + e₁i + e₂j + e₃k + e₄l + e₅il + e₆jl + e₇kl
```

Where:
- `e₀` = real part
- `e₁, e₂, e₃` = quaternion-like imaginary parts (i, j, k)
- `e₄, e₅, e₆, e₇` = extended imaginary parts (l, il, jl, kl)

All basis elements square to -1 except the real unit: `i² = j² = k² = l² = il² = jl² = kl² = -1`

---

## Mathematical Background

### The Cayley-Dickson Construction

Octonions are constructed from quaternions using the Cayley-Dickson process. Given two quaternions `(a, b)` and `(c, d)`:

```
(a, b) × (c, d) = (a·c - conj(d)·b, d·a + b·conj(c))
```

This construction naturally leads to non-associativity.

### Multiplication Table

The basis element multiplication follows specific rules encoded in the 8×8 multiplication table. Key examples:

```
i × j = k        j × i = -k       (non-commutative)
i × l = il       l × i = -il
(i×j)×l ≠ i×(j×l)                 (non-associative)
```

### The Loss Cascade

```
ℝ: ordered field
ℂ: field, algebraically closed, lose ordering
ℍ: division algebra, lose commutativity
𝕆: normed division algebra, lose associativity
𝕊: lose division entirely (zero divisors appear)
```

**Hurwitz's Theorem (1898)**: Only 1, 2, 4, and 8 dimensional normed division algebras exist over ℝ.

---

## C API Reference

### Data Types

```c
typedef struct oct {
    double e[8];  /* e[0]=real, e[1..7]=imaginary components */
} octonion;
```

### Constructors

#### `oct_make`
```c
octonion oct_make(double e0, double e1, double e2, double e3,
                  double e4, double e5, double e6, double e7);
```
Create an octonion from 8 components.

**Parameters:**
- `e0` - Real part
- `e1` - i component
- `e2` - j component
- `e3` - k component
- `e4` - l component
- `e5` - il component
- `e6` - jl component
- `e7` - kl component

**Returns:** New octonion

**Example:**
```c
octonion o = oct_make(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
```

---

#### `oct_identity`
```c
octonion oct_identity(void);
```
Create the multiplicative identity (1, 0, 0, 0, 0, 0, 0, 0).

**Returns:** Identity octonion

---

#### `oct_zero`
```c
octonion oct_zero(void);
```
Create the zero octonion (0, 0, 0, 0, 0, 0, 0, 0).

**Returns:** Zero octonion

---

#### `array2oct`
```c
octonion array2oct(const double *values, int size);
```
Create an octonion from an array of 8 doubles.

**Parameters:**
- `values` - Array of 8 doubles
- `size` - Must be 8 (or returns zero octonion)

**Returns:** New octonion or zero octonion on error

---

#### `oct2array`
```c
double *oct2array(octonion q);
```
Convert an octonion to a dynamically allocated array.

**Parameters:**
- `q` - Octonion to convert

**Returns:** Pointer to array of 8 doubles (caller must free!)

**Warning:** Caller is responsible for freeing the returned array.

---

### Arithmetic Operations

#### `oct_add`
```c
octonion oct_add(octonion p, octonion q);
```
Add two octonions (component-wise).

**Associative:** Yes  
**Commutative:** Yes  

**Example:**
```c
octonion a = oct_make(1, 0, 0, 0, 0, 0, 0, 0);
octonion b = oct_make(0, 1, 0, 0, 0, 0, 0, 0);
octonion sum = oct_add(a, b);  // (1, 1, 0, 0, 0, 0, 0, 0)
```

---

#### `oct_sub`
```c
octonion oct_sub(octonion p, octonion q);
```
Subtract two octonions (component-wise).

**Associative:** No  
**Commutative:** No  

---

#### `oct_multiply`
```c
octonion oct_multiply(octonion p, octonion q);
```
Multiply two octonions using the Cayley multiplication table.

**Associative:** ❌ **NO** - This is the defining feature!  
**Commutative:** ❌ **NO**  
**Norm-preserving:** ✓ **YES** - `|p×q| = |p| × |q|`

**Warning:** Always use explicit parentheses when chaining multiplications!

**Example:**
```c
octonion i = oct_make(0, 1, 0, 0, 0, 0, 0, 0);
octonion j = oct_make(0, 0, 1, 0, 0, 0, 0, 0);

octonion ij = oct_multiply(i, j);   // k
octonion ji = oct_multiply(j, i);   // -k (different!)

// Non-associativity example:
octonion l = oct_make(0, 0, 0, 0, 1, 0, 0, 0);
octonion left = oct_multiply(oct_multiply(i, j), l);   // (i×j)×l
octonion right = oct_multiply(i, oct_multiply(j, l));  // i×(j×l)
// left ≠ right !
```

---

#### `oct_scale`
```c
octonion oct_scale(octonion q, double scalar);
```
Multiply an octonion by a scalar (all components).

**Parameters:**
- `q` - Octonion to scale
- `scalar` - Scalar multiplier

**Returns:** Scaled octonion

---

#### `oct_add_scalar`
```c
octonion oct_add_scalar(octonion q, double s);
```
Add a scalar to the real part of an octonion.

---

#### `oct_sub_scalar`
```c
octonion oct_sub_scalar(octonion q, double s);
```
Subtract a scalar from the real part of an octonion.

---

### Properties and Transformations

#### `oct_conjugate`
```c
octonion oct_conjugate(octonion q);
```
Compute the conjugate by negating all 7 imaginary components.

**Formula:** `conj(e₀ + v) = e₀ - v` where `v` is the imaginary part

**Properties:**
- `conj(conj(o)) = o`
- `conj(a + b) = conj(a) + conj(b)`
- `conj(a × b) = conj(b) × conj(a)` (note order reversal!)

---

#### `oct_negate`
```c
octonion oct_negate(octonion q);
```
Negate all components: `-o = -1 × o`

---

#### `oct_norm`
```c
double oct_norm(octonion q);
```
Compute the norm (magnitude) of an octonion.

**Formula:** `|o| = √(e₀² + e₁² + ... + e₇²)`

**Properties:**
- `|o| ≥ 0` with equality only when `o = 0`
- `|a × b| = |a| × |b|` (multiplicative norm)
- `|s × o| = |s| × |o|` for scalar `s`

---

#### `oct_norm_squared`
```c
double oct_norm_squared(octonion q);
```
Compute the squared norm (avoids sqrt, faster).

**Formula:** `|o|² = e₀² + e₁² + ... + e₇²`

**Use:** Faster than `oct_norm` when you only need to compare magnitudes.

---

#### `oct_normalise`
```c
octonion oct_normalise(octonion q);
```
Normalize to unit length: `o/|o|`

**Returns:** Unit octonion with `|result| = 1`, or zero octonion if `|q| = 0`

---

#### `oct_inverse`
```c
octonion oct_inverse(octonion q);
```
Compute the multiplicative inverse.

**Formula:** `o⁻¹ = conj(o) / |o|²`

**Returns:** Inverse octonion, or zero octonion if `|q| = 0`

**Properties:**
- `o × o⁻¹ = o⁻¹ × o = 1` (for non-zero `o`)
- `(a × b)⁻¹ = b⁻¹ × a⁻¹` (note order reversal!)

**Warning:** Division is well-defined but order-dependent due to non-commutativity.

---

### Accessors and Utilities

#### `oct_real`
```c
double oct_real(octonion q);
```
Extract the real part (e₀).

---

#### `oct_equal`
```c
int oct_equal(octonion p, octonion q, double epsilon);
```
Test equality within tolerance.

**Parameters:**
- `p`, `q` - Octonions to compare
- `epsilon` - Tolerance for floating-point comparison (e.g., `1e-10`)

**Returns:** 1 if equal within epsilon, 0 otherwise

---

#### `oct_print`
```c
void oct_print(octonion q);
```
Print an octonion to stdout in the format: `𝕆(e0, e1, e2, e3, e4, e5, e6, e7)`

---

## Scheme API Reference

### Constructors

#### `(make-octonion e0 e1 e2 e3 e4 e5 e6 e7)` / `(oct e0 e1 e2 e3 e4 e5 e6 e7)`
Create an octonion from 8 real numbers.

**Parameters:** 8 numbers (integers or floats)  
**Returns:** Octonion object

**Example:**
```scheme
(define o1 (oct 1 0 0 0 0 0 0 0))     ; Real unit
(define i  (oct 0 1 0 0 0 0 0 0))     ; i basis
(define j  (oct 0 0 1 0 0 0 0 0))     ; j basis
(define k  (oct 0 0 0 1 0 0 0 0))     ; k basis
(define l  (oct 0 0 0 0 1 0 0 0))     ; l basis
```

---

### Predicates

#### `(octonion? x)`
Test if `x` is an octonion.

**Returns:** `t` if `x` is an octonion, `nil` otherwise

**Example:**
```scheme
(octonion? (oct 1 2 3 4 5 6 7 8))  ; => t
(octonion? 42)                     ; => nil
(octonion? (quat 1 0 0 0))         ; => nil (it's a quaternion)
```

---

### Accessors

#### `(oct-real o)`
Extract the real part (e₀).

**Example:**
```scheme
(define o (oct 1 2 3 4 5 6 7 8))
(oct-real o)  ; => 1.0
```

---

#### `(oct-e1 o)` through `(oct-e7 o)`
Extract individual imaginary components.

- `(oct-e1 o)` - i component
- `(oct-e2 o)` - j component  
- `(oct-e3 o)` - k component
- `(oct-e4 o)` - l component
- `(oct-e5 o)` - il component
- `(oct-e6 o)` - jl component
- `(oct-e7 o)` - kl component

**Example:**
```scheme
(define o (oct 1 2 3 4 5 6 7 8))
(oct-e4 o)  ; => 5.0  (l component)
```

---

### Arithmetic

#### `(oct-add o1 o2)`
Add two octonions.

**Associative:** Yes  
**Commutative:** Yes

**Example:**
```scheme
(define a (oct 1 0 0 0 0 0 0 0))
(define b (oct 0 1 0 0 0 0 0 0))
(oct-add a b)  ; => #O(1 1 0 0 0 0 0 0)
```

---

#### `(oct-sub o1 o2)`
Subtract two octonions.

**Example:**
```scheme
(oct-sub (oct 5 0 0 0 0 0 0 0) 
         (oct 3 0 0 0 0 0 0 0))  ; => #O(2 0 0 0 0 0 0 0)
```

---

#### `(oct-multiply o1 o2)` / `(oct* o1 o2)`
Multiply two octonions.

**Associative:** ❌ **NO**  
**Commutative:** ❌ **NO**

**Example:**
```scheme
(define i (oct 0 1 0 0 0 0 0 0))
(define j (oct 0 0 1 0 0 0 0 0))

(oct* i i)  ; => #O(-1 0 0 0 0 0 0 0)  [i² = -1]
(oct* i j)  ; => #O(0 0 0 1 0 0 0 0)   [i×j = k]
(oct* j i)  ; => #O(0 0 0 -1 0 0 0 0)  [j×i = -k, different!]

;; Non-associativity demonstration
(define l (oct 0 0 0 0 1 0 0 0))
(define left  (oct* (oct* i j) l))  ; (i×j)×l
(define right (oct* i (oct* j l)))  ; i×(j×l)
;; left ≠ right !
```

---

#### `(oct-scale o scalar)`
Multiply an octonion by a scalar.

**Example:**
```scheme
(oct-scale (oct 1 2 3 4 5 6 7 8) 2)
; => #O(2 4 6 8 10 12 14 16)
```

---

### Properties

#### `(oct-conjugate o)`
Compute the conjugate.

**Example:**
```scheme
(define o (oct 1 2 3 4 5 6 7 8))
(oct-conjugate o)  ; => #O(1 -2 -3 -4 -5 -6 -7 -8)
```

---

#### `(oct-norm o)`
Compute the magnitude.

**Example:**
```scheme
(oct-norm (oct 1 2 3 4 5 6 7 8))  ; => 14.2828...
(oct-norm (oct 3 0 4 0 0 0 0 0))  ; => 5.0
```

---

#### `(oct-norm-squared o)`
Compute the squared magnitude (faster).

**Example:**
```scheme
(oct-norm-squared (oct 1 2 3 4 5 6 7 8))  ; => 204.0
```

---

#### `(oct-normalize o)`
Normalize to unit length.

**Example:**
```scheme
(define o (oct 3 0 4 0 0 0 0 0))
(define unit (oct-normalize o))
(oct-norm unit)  ; => 1.0
```

---

#### `(oct-inverse o)`
Compute the multiplicative inverse.

**Example:**
```scheme
(define o (oct 2 0 0 0 0 0 0 0))
(oct-inverse o)  ; => #O(0.5 0 0 0 0 0 0 0)

;; Verify: o × o⁻¹ = 1
(oct* o (oct-inverse o))  ; => #O(1 0 0 0 0 0 0 0)
```

---

#### `(oct-negate o)`
Negate all components.

**Example:**
```scheme
(oct-negate (oct 1 2 3 4 5 6 7 8))  
; => #O(-1 -2 -3 -4 -5 -6 -7 -8)
```

---

## Examples

### Example 1: Basis Element Multiplication

```scheme
;; Define basis octonions
(define one (oct 1 0 0 0 0 0 0 0))
(define i   (oct 0 1 0 0 0 0 0 0))
(define j   (oct 0 0 1 0 0 0 0 0))
(define k   (oct 0 0 0 1 0 0 0 0))
(define l   (oct 0 0 0 0 1 0 0 0))

;; Verify i² = j² = k² = l² = -1
(print (oct* i i))  ; #O(-1 0 0 0 0 0 0 0)
(print (oct* j j))  ; #O(-1 0 0 0 0 0 0 0)
(print (oct* k k))  ; #O(-1 0 0 0 0 0 0 0)
(print (oct* l l))  ; #O(-1 0 0 0 0 0 0 0)

;; Verify quaternion-like relations
(print (oct* i j))  ; #O(0 0 0 1 0 0 0 0) = k
(print (oct* j k))  ; #O(0 1 0 0 0 0 0 0) = i
(print (oct* k i))  ; #O(0 0 1 0 0 0 0 0) = j
```

### Example 2: Non-Commutativity

```scheme
(define i (oct 0 1 0 0 0 0 0 0))
(define j (oct 0 0 1 0 0 0 0 0))

(define ij (oct* i j))
(define ji (oct* j i))

(print ij)  ; #O(0 0 0 1 0 0 0 0)   = k
(print ji)  ; #O(0 0 0 -1 0 0 0 0)  = -k

;; They differ by a sign!
```

### Example 3: Non-Associativity

```scheme
(define i (oct 0 1 0 0 0 0 0 0))
(define j (oct 0 0 1 0 0 0 0 0))
(define l (oct 0 0 0 0 1 0 0 0))

;; Compute (i×j)×l
(define ij (oct* i j))
(define left (oct* ij l))

;; Compute i×(j×l)
(define jl (oct* j l))
(define right (oct* i jl))

(print left)   ; Different result!
(print right)  ; Different result!

;; This is THE defining feature of octonions
```

### Example 4: Norm Preservation

```scheme
(define o1 (oct 1 2 3 4 5 6 7 8))
(define o2 (oct 2 1 0 -1 3 -2 1 0))

(define product (oct* o1 o2))

(define norm1 (oct-norm o1))
(define norm2 (oct-norm o2))
(define norm-product (oct-norm product))

(print norm1)
(print norm2)
(print (* norm1 norm2))
(print norm-product)

;; norm-product = norm1 × norm2  (within floating-point error)
;; This property is preserved even though associativity is lost!
```

### Example 5: Inverse and Division

```scheme
(define o (oct 1 2 3 4 5 6 7 8))
(define o-inv (oct-inverse o))

;; Verify o × o⁻¹ = 1
(define result (oct* o o-inv))
(print result)  ; Should be approximately #O(1 0 0 0 0 0 0 0)

;; Manual division: a / b = a × b⁻¹
(define a (oct 10 0 0 0 0 0 0 0))
(define b (oct 2 0 0 0 0 0 0 0))
(define quotient (oct* a (oct-inverse b)))
(print quotient)  ; #O(5 0 0 0 0 0 0 0)
```

### Example 6: Building Complex Operations

```scheme
;; Compute o² = o × o (this is always well-defined)
(define (oct-square o)
  (oct* o o))

;; Compute o³ = (o × o) × o (left-associative by convention)
(define (oct-cube o)
  (oct* (oct* o o) o))

;; Unit octonion in a specific direction
(define (oct-unit e0 e1 e2 e3 e4 e5 e6 e7)
  (oct-normalize (oct e0 e1 e2 e3 e4 e5 e6 e7)))

;; Test
(define unit-i (oct-unit 0 1 0 0 0 0 0 0))
(print (oct-norm unit-i))  ; 1.0
```

---

## Common Pitfalls

### 1. Assuming Associativity ❌

```scheme
;; WRONG - Ambiguous!
(define bad-result (oct* (oct* (oct* a b) c) d))

;; RIGHT - Explicit parentheses
(define good-result (oct* (oct* (oct* a b) c) d))  ; Left-associative
```

**Rule:** Always use explicit parentheses. Never rely on implicit associativity.

### 2. Assuming Commutativity ❌

```scheme
;; These are DIFFERENT:
(oct* a b)  ≠  (oct* b a)

;; Order matters!
```

### 3. Confusing Conjugate Properties

```scheme
;; WRONG
(oct-conjugate (oct* a b))  ≠  (oct* (oct-conjugate a) (oct-conjugate b))

;; RIGHT - Order reverses!
(oct-conjugate (oct* a b))  =  (oct* (oct-conjugate b) (oct-conjugate a))
```

### 4. Division Order

```scheme
;; a / b and b \ a are different!
(define a-div-b (oct* a (oct-inverse b)))  ; a × b⁻¹
(define b-div-a (oct* (oct-inverse b) a))  ; b⁻¹ × a

;; These are NOT the same!
```

### 5. Floating-Point Comparison

```scheme
;; WRONG
(= (oct-norm o) 1.0)

;; RIGHT - Use tolerance
(< (abs (- (oct-norm o) 1.0)) 1e-10)

;; Or in C:
oct_equal(o1, o2, 1e-10)
```

---

## Performance Notes

### Computational Complexity

- **Addition/Subtraction:** O(8) - Simple component-wise operations
- **Multiplication:** O(64) - 8×8 multiplication table lookups
- **Norm:** O(8) - Sum of 8 squares + sqrt
- **Norm-squared:** O(8) - Sum of 8 squares (faster, no sqrt)
- **Inverse:** O(8) + norm-squared cost

### Optimization Tips

1. **Use `oct_norm_squared` when possible** - Avoid expensive sqrt when comparing magnitudes

```c
// Fast magnitude comparison
if (oct_norm_squared(a) > oct_norm_squared(b)) { ... }
```

2. **Reuse computed values**

```c
// Compute norm once
double norm = oct_norm(o);
octonion unit = oct_scale(o, 1.0 / norm);
```

3. **Batch operations in C** - Call C functions directly for tight loops rather than going through Scheme

4. **Use conjugate for inverse when normalized**

```c
// For unit octonions (|o| = 1):
// o⁻¹ = conj(o) / |o|² = conj(o) / 1 = conj(o)
if (fabs(oct_norm_squared(o) - 1.0) < 1e-10) {
    inverse = oct_conjugate(o);  // Faster!
}
```

### Memory

- Each octonion: 64 bytes (8 doubles)
- No dynamic allocation in core operations
- `oct2array` allocates - remember to free!

---

## Mathematical Properties Summary

### Satisfied Properties ✓

- **Closure:** 𝕆 × 𝕆 → 𝕆
- **Additive associativity:** (a + b) + c = a + (b + c)
- **Additive commutativity:** a + b = b + a
- **Additive identity:** 0
- **Additive inverse:** -a
- **Multiplicative identity:** 1
- **Multiplicative inverse:** a⁻¹ (for a ≠ 0)
- **Distributivity:** a × (b + c) = a×b + a×c
- **Norm-multiplicative:** |a × b| = |a| × |b|
- **Alternative:** (a×a)×b = a×(a×b), a×(b×b) = (a×b)×b

### Lost Properties ❌

- **Multiplicative associativity:** (a×b)×c ≠ a×(b×c)
- **Multiplicative commutativity:** a×b ≠ b×a
- **Power associativity:** aⁿ not well-defined for n > 2

### Special Cases

- **Flexible identity:** (a×b)×a = a×(b×a) for all a, b
- **Moufang identities:** Various special cases of partial associativity
- **Real multiples commute:** (ra) × b = r(a × b) = a × (rb) for r ∈ ℝ

---

## References

- Baez, J. C. (2002). "The Octonions". *Bulletin of the American Mathematical Society*.
- Conway, J. H., & Smith, D. A. (2003). *On Quaternions and Octonions*.
- Hurwitz, A. (1898). "Über die Composition der quadratischen Formen von beliebig vielen Variabeln".

---

## Version History

- **v0.1** (2026-01-09): Initial implementation
  - Complete arithmetic: add, subtract, multiply
  - Properties: conjugate, norm, inverse
  - C and Scheme APIs
  - Test suite demonstrating non-associativity

---

*Part of SIOD-TR - Baroque Number Systems (ℝ → ℂ → ℍ → 𝕆)*  
*Chris "Scáth" Ó Luanaigh, 2026*
