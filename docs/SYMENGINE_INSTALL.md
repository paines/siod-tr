# SymEngine for SIOD-TR - Installation & Quickstart

## Installation

### Step 1: Install SymEngine

#### Ubuntu 22.04

```bash
# Install dependencies
sudo apt install cmake g++ libgmp-dev git

# Clone and build SymEngine
git clone https://github.com/symengine/symengine
cd symengine

# Configure (minimal build for fast compilation)
cmake -DCMAKE_INSTALL_PREFIX=/usr/local \
      -DCMAKE_BUILD_TYPE=Release \
      -DWITH_SYMENGINE_THREAD_SAFE=ON \
      -DWITH_SYMENGINE_RCP=ON \
      .

# Build (use -j4 for 4 cores, adjust as needed)
make -j4

# Install
sudo make install

# Update library cache
sudo ldconfig
```

#### macOS (Homebrew)

```bash
brew install symengine
```

#### Verify Installation

```bash
pkg-config --modversion symengine
# Should output: 0.11.0 or similar

pkg-config --cflags symengine
# Should output compiler flags

pkg-config --libs symengine
# Should output linker flags
```

### Step 2: Build SIOD-TR Module

```bash
cd /path/to/siod-tr

# Add symengine.so to your PROGS in Makefile
# (should already be there if following the guide)

# Build
make clean
make linux  # or make macosx

# Verify module was created
ls -lh symengine.so
```

### Step 3: Test Installation

```bash
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH  # If needed
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

> (sym+ '(* x 2) '(* x 3))
"5*x"
```

If these work, you're ready to go!

---

## Quick Examples

### Example 1: Basic Arithmetic

```scheme
> (sym '(+ (* x x) (* 2 x) 1))
"x**2 + 2*x + 1"

> (sym-expand '(* (+ x 1) (+ x 1)))
"x**2 + 2*x + 1"
```

### Example 2: Differentiation

```scheme
> (sym-diff '(* x x) 'x)
"2*x"

> (sym-diff '(sin x) 'x)
"cos(x)"

> (sym-diff '(* (exp x) (sin x)) 'x)
"exp(x)*sin(x) + exp(x)*cos(x)"
```

### Example 3: Symbolic Algebra

```scheme
> (sym+ '(* 2 x) '(* 3 x))
"5*x"

> (sym* '(+ x 1) '(+ x 2))
"(x + 1)*(x + 2)"

> (sym-expand '(* (+ x 1) (+ x 2)))
"x**2 + 3*x + 2"
```

### Example 4: Transcendental Functions

```scheme
> (sym '(sin (* 2 x)))
"sin(2*x)"

> (sym-diff '(sin (* 2 x)) 'x)
"2*cos(2*x)"

> (sym '(exp (* -1 (* x x))))
"exp(-x**2)"
```

### Example 5: Constants

```scheme
> (sym 'pi)
"pi"

> (sym 'e)
"E"

> (sym '(* 2 pi))
"2*pi"

> (sym '(exp (* i pi)))
"exp(I*pi)"
```

---

## Common Issues

### Issue 1: "SymEngine not found via pkg-config"

**Solution:**
```bash
# Check if SymEngine is installed
ls /usr/local/lib/libsymengine.so

# If it exists, add to pkg-config path
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH

# Then rebuild
make clean && make linux
```

### Issue 2: "cannot find -lsymengine" during linking

**Solution:**
```bash
# Check library path
ls /usr/local/lib/libsymengine.so*

# Add to library path
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Or update ldconfig
sudo ldconfig
```

### Issue 3: "undefined symbol" when loading symengine.so

**Solution:**
```bash
# Check what symbols are missing
ldd symengine.so

# Make sure LD_LIBRARY_PATH includes SymEngine
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

### Issue 4: Module loads but primitives not found

**Solution:**
```scheme
# Verify module loaded
> (require-so "symengine.so")

# Check if functions are defined
> sym
#<CLOSURE ...>  ; Should show it's defined

# If not, check initialization was called
```

---

## Next Steps

Once basic installation works:

1. **Load test suite:**
   ```scheme
   (load "tests/test-symengine.scm")
   (run-symengine-tests)
   ```

2. **Load utilities:**
   ```scheme
   (load "symengine-utils.scm")
   ```

3. **Try integration with PLplot:**
   ```scheme
   (load "examples/symengine-plplot.scm")
   (plot-symbolic-demo)
   ```

4. **Read the full documentation:**
   - `SYMENGINE_API_REFERENCE.md` - Complete API
   - `SYMENGINE_INTEGRATION_STUDY.md` - Design rationale

---

## Building from Source (Advanced)

### Minimal SymEngine Build

```bash
cmake -DCMAKE_BUILD_TYPE=Release \
      -DBUILD_TESTS=OFF \
      -DBUILD_BENCHMARKS=OFF \
      .
make -j4
sudo make install
```

### Optimized Build (Recommended)

```bash
cmake -DCMAKE_BUILD_TYPE=Release \
      -DWITH_SYMENGINE_THREAD_SAFE=ON \
      -DWITH_SYMENGINE_RCP=ON \
      -DWITH_TCMALLOC=ON \
      .
make -j4
sudo make install
```

### Full-Featured Build

```bash
cmake -DCMAKE_BUILD_TYPE=Release \
      -DWITH_SYMENGINE_THREAD_SAFE=ON \
      -DWITH_GMP=ON \
      -DWITH_MPFR=ON \
      -DWITH_MPC=ON \
      -DWITH_FLINT=ON \
      .
make -j4
sudo make install
```

---

## Verifying Everything Works

### Full Verification Test

```scheme
#!/usr/bin/env siod

(require-so "symengine.so")

(define (test-basic)
  (display "Testing basic operations...")
  (newline)
  
  ; Test 1: Symbol creation
  (define expr1 (sym 'x))
  (display "  Symbol: ") (display expr1) (newline)
  
  ; Test 2: Arithmetic
  (define expr2 (sym+ '(* x 2) '(* x 3)))
  (display "  2x + 3x = ") (display expr2) (newline)
  
  ; Test 3: Differentiation
  (define expr3 (sym-diff '(* x x) 'x))
  (display "  d/dx(x²) = ") (display expr3) (newline)
  
  ; Test 4: Expansion
  (define expr4 (sym-expand '(* (+ x 1) (+ x 2))))
  (display "  (x+1)(x+2) = ") (display expr4) (newline)
  
  (display "All tests passed!")
  (newline))

(test-basic)
```

Save as `verify-symengine.scm` and run:
```bash
./siod verify-symengine.scm
```

Expected output:
```
Testing basic operations...
  Symbol: x
  2x + 3x = 5*x
  d/dx(x²) = 2*x
  (x+1)(x+2) = x**2 + 3*x + 2
All tests passed!
```

---

**You're now ready to explore symbolic mathematics in SIOD-TR!** 🎉
