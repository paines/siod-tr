# SymEngine Integration for SIOD-TR - Phase 1

## 🎉 Getting Started with Symbolic Mathematics

This is the **Phase 1** implementation of SymEngine for SIOD-TR, providing core symbolic mathematics capabilities.

---

## Files Included

### Core Implementation
- **`siod_symengine.c`** - Main C bindings (650 lines)
- **`symengine.c`** - Module entry point
- **`Makefile.symengine`** - Build rules

### Documentation
- **`SYMENGINE_INSTALL.md`** - Installation guide & quickstart
- **`SYMENGINE_API_REFERENCE.md`** - Complete API documentation
- **`SYMENGINE_INTEGRATION_STUDY.md`** - Design rationale & feasibility study

### Scheme Utilities
- **`test-symengine.scm`** - Test suite (15 tests)
- **`symengine-utils.scm`** - Helper functions
- **`symengine-plplot.scm`** - PLplot integration examples

---

## Quick Installation (5 Minutes)

### 1. Install SymEngine

```bash
# Ubuntu 22.04
sudo apt install cmake g++ libgmp-dev git

git clone https://github.com/symengine/symengine
cd symengine
cmake -DCMAKE_INSTALL_PREFIX=/usr/local \
      -DCMAKE_BUILD_TYPE=Release .
make -j4
sudo make install
sudo ldconfig
```

### 2. Build SIOD-TR Module

```bash
cd /path/to/siod-tr

# Add these lines to your Makefile PROGS variable:
# symengine.so

# Add Makefile.symengine contents to your Makefile

# Build
make clean
make linux  # or make macosx

# Verify
ls -lh symengine.so
```

### 3. Test

```bash
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
./siod
```

```scheme
> (require-so "symengine.so")
done.
t

> (sym '(* x x))
"x**2"

> (sym-diff '(* x x) 'x)
"2*x"
```

**If this works, you're ready!** 🎊

---

## What Works Now (Phase 1)

### ✅ Expression Creation
```scheme
(sym 'x)                      ; Variables
(sym 42)                      ; Numbers
(sym '(+ (* x x) 1))          ; Expressions
(sym 'pi)                     ; Constants
```

### ✅ Arithmetic
```scheme
(sym+ '(* x 2) '(* x 3))      ; => "5*x"
(sym- 'x 1)                   ; => "x - 1"
(sym* 'x 'y)                  ; => "x*y"
(sym/ 'x 2)                   ; => "x/2"
(sym-pow 'x 2)                ; => "x**2"
```

### ✅ Differentiation
```scheme
(sym-diff '(* x x) 'x)        ; => "2*x"
(sym-diff '(sin x) 'x)        ; => "cos(x)"
(sym-diff '(exp x) 'x)        ; => "exp(x)"
```

### ✅ Expansion
```scheme
(sym-expand '(* (+ x 1) (+ x 2)))  ; => "x**2 + 3*x + 2"
```

### ✅ Special Functions
```scheme
(sym '(sin x))                ; Trig functions
(sym '(exp x))                ; Exponential
(sym '(log x))                ; Logarithm
```

---

## What's Coming Next (Phase 2)

### ⭕ Numerical Evaluation
```scheme
; Coming soon:
(sym-subs '(* x x) '((x . 5)))         ; => 25
(sym-evalf '(sin 1))                   ; => 0.8414...
```

### ⭕ Function Generation
```scheme
; Coming soon:
(define f (sym->lambda '(* x x) '(x)))
(f 5)                                   ; => 25
```

### ⭕ PLplot Integration
```scheme
; Coming soon:
(plot-symbolic '(sin x) 'x 0 6.28)
(plot-with-derivative '(* x x x) 'x -3 3)
```

### ⭕ Series Expansion
```scheme
; Coming soon:
(sym-series '(exp x) 'x 0 5)           ; Taylor series
```

---

## Usage Examples

### Example 1: Basic Symbolic Math

```scheme
> (require-so "symengine.so")
> (load "symengine-utils.scm")

> (define f '(+ (* x x) (* 2 x) 1))
> (sym-show "f(x)" f)
f(x): x**2 + 2*x + 1

> (sym-show "f'(x)" (sym-diff f 'x))
f'(x): 2*x + 2

> (sym-show "f''(x)" (sym-diff-2 f 'x))
f''(x): 2
```

### Example 2: Chain Rule

```scheme
> (define g '(sin (* 2 x)))
> (sym-show "g(x)" g)
g(x): sin(2*x)

> (sym-show "g'(x)" (sym-diff g 'x))
g'(x): 2*cos(2*x)
```

### Example 3: Product Rule

```scheme
> (define h '(* x (sin x)))
> (sym-show "h(x)" h)
h(x): x*sin(x)

> (sym-show "h'(x)" (sym-diff h 'x))
h'(x): x*cos(x) + sin(x)
```

### Example 4: Run Test Suite

```scheme
> (load "test-symengine.scm")
> (run-symengine-tests)

========================================
SIOD-TR SymEngine Test Suite
========================================

Test 1: Symbol creation ... PASS
Test 2: Number creation ... PASS
Test 3: Symbolic addition ... PASS
...
Test 15: Constants (pi, e) ... PASS

========================================
SymEngine Test Summary
========================================
Total:  15
Passed: 15
Failed: 0
All tests passed!
```

---

## Implementation Details

### Current Architecture

```
S-Expression → lisp2basic() → SymEngine → basic2lisp() → String
   (SIOD)                     (symbolic)                  (SIOD)
```

Phase 1 returns **strings** for simplicity. Phase 2 will return actual symbolic objects that can be evaluated numerically.

### Memory Management

All SymEngine `basic` objects are stack-allocated and freed properly. No leaks in Phase 1 implementation.

### Error Handling

Invalid expressions raise SIOD errors cleanly:
```scheme
> (sym '(+ 1 "not-a-number"))
ERROR: cannot convert to symbolic expression
```

---

## Development Roadmap

### Phase 1 ✅ (Current - Week 1)
- Core expression creation
- Basic arithmetic
- Differentiation
- Expression expansion
- Test suite

### Phase 2 (Week 2)
- Numerical evaluation (sym-subs)
- Function generation (sym->lambda)
- Series expansion
- PLplot integration
- Advanced derivatives

### Phase 3 (Week 3)
- Equation solving
- Matrix operations
- Simplification
- Pattern matching
- More special functions

### Phase 4 (Future)
- Quaternion integration
- Code generation
- Domain-specific languages
- Advanced visualization

---

## Contributing

To extend this module:

1. **Add primitives** in `siod_symengine.c`
2. **Register them** in `init_subr_symengine()`
3. **Test** in `test-symengine.scm`
4. **Document** in API reference
5. **Add examples** in utilities

---

## Resources

- **SymEngine**: https://github.com/symengine/symengine
- **C API Docs**: https://github.com/symengine/symengine/blob/master/symengine/cwrapper.h
- **SIOD-TR**: Your baroque mathematics platform

---

## Support

For issues:
1. Check `SYMENGINE_INSTALL.md` troubleshooting section
2. Run `make check-symengine` to verify installation
3. Test with `test-symengine.scm`
4. Review examples in `symengine-utils.scm`

---

## License

MIT (matching both SymEngine and SIOD)

---

## Next Steps

1. **Install SymEngine** following `SYMENGINE_INSTALL.md`
2. **Build the module** with provided Makefile
3. **Run tests** to verify everything works
4. **Explore examples** in the utilities
5. **Start symbolic computing!**

**Welcome to symbolic mathematics in SIOD-TR!** 🚀📐

The baroque numbers dream is becoming reality:
- Quaternions ✅
- PLplot ✅  
- SymEngine ✅ (Phase 1)
- → Complete mathematical computing environment! 🎯
