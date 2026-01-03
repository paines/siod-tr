# Contributing to SIOD-TR

Thank you for your interest in SIOD-TR! We welcome contributions from students, researchers, and tinkerers.

---

## Quick Start

1. **Fork the repository**
2. **Create a branch** (`git checkout -b feature/amazing-thing`)
3. **Make your changes**
4. **Test thoroughly**
5. **Submit a pull request**

---

## Ways to Contribute

### 🎓 For Students

**Share Your Learning**
- Write tutorials explaining quaternion concepts
- Create example programs showing what you built
- Report confusing documentation
- Suggest better error messages

**Example contributions:**
- "I created a bouncing ball demo using quaternions"
- "This error message was confusing, here's better wording"
- "Tutorial on gimbal lock vs quaternions"

### 🔬 For Researchers

**Expand Mathematical Capabilities**
- Implement quaternion functions (see roadmap)
- Add octonion support
- Create benchmark tests
- Write academic examples

**Example contributions:**
- "Implemented quaternion sinh/cosh/tanh"
- "Added test suite for slice-regular derivatives"
- "Example: using quaternions for rigid body physics"

### 💻 For Developers

**Improve the System**
- Bug fixes
- Performance optimization
- Cross-platform support
- Better build system

**Example contributions:**
- "Fixed memory leak in quaternion allocation"
- "Ported to Windows/WSL"
- "Improved Makefile for M1 Macs"

---

## Good First Issues

We tag beginner-friendly tasks with `good-first-issue`. Current ones:

### Documentation
- [ ] Add more comments to quaternion code
- [ ] Write FAQ section
- [ ] Create installation troubleshooting guide
- [ ] Document all quaternion functions

### Examples
- [ ] Solar system orbit simulation
- [ ] Spinning top physics
- [ ] Quaternion Julia sets
- [ ] Camera controls tutorial

### Testing
- [ ] Add tests for edge cases
- [ ] Verify quaternion identities
- [ ] Cross-platform testing
- [ ] Performance benchmarks

### Bugs
- [ ] See current issues marked `bug`

---

## Development Guidelines

### Code Style

**C code:**
```c
/* Function comment explaining what it does */
static LISP lfunction_name(LISP arg) {
    /* Check types */
    if (!QUATERNIONP(arg)) {
        err("expected quaternion", arg);
    }
    
    /* Do work */
    double w = QUATW(arg);
    ...
    
    /* Return result */
    return make_quaternion(w, x, y, z);
}
```

**Scheme code:**
```scheme
;;; Function: descriptive-name
;;; Purpose: What this does
;;; Args: what arguments mean
;;; Returns: what it returns
(define (descriptive-name arg1 arg2)
  (let ((intermediate (compute-something arg1)))
    (final-result intermediate arg2)))
```

### Testing

**Every new function needs tests:**

```scheme
;;; In test-quaternions.scm
(test-case "quaternion-sinh of zero"
  (assert-equal (sinh (quat 0 0 0 0)) 0))

(test-case "quaternion-sinh identity"
  (let ((q (quat 1 2 3 4)))
    (assert-close (sinh q) 
                  (/ (- (exp q) (exp (- q))) 2)
                  1e-10)))
```

**Run all tests before submitting:**
```bash
make test
```

### Commit Messages

**Good:**
```
Add quaternion hyperbolic functions

Implements sinh, cosh, tanh for quaternions using exp-based
formulas. Includes comprehensive test suite verifying identities.

Fixes #42
```

**Bad:**
```
fixed stuff
```

**Format:**
```
<type>: <subject>

<body>

<footer>
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation
- `test`: Adding tests
- `refactor`: Code restructure
- `perf`: Performance improvement

---

## Pull Request Process

### Before Submitting

- [ ] Code compiles without warnings
- [ ] All tests pass
- [ ] New code has tests
- [ ] Documentation updated
- [ ] CHANGELOG.md updated

### PR Template

```markdown
## Description
Brief description of what this changes

## Motivation
Why is this change needed?

## Changes
- Item 1
- Item 2

## Testing
How was this tested?

## Screenshots (if applicable)
[Add screenshots of visual changes]

## Checklist
- [ ] Tests pass
- [ ] Documentation updated
- [ ] No compiler warnings
```

### Review Process

1. **Automated checks** run (build, tests)
2. **Maintainer review** (usually within 48 hours)
3. **Discussion/changes** if needed
4. **Merge** when approved

---

## Areas Needing Help

### High Priority

**1. Complete Quaternion Math**
Status: In progress
Files: `baroque.c`
Need: sqrt, log, trig functions implemented

**2. Cross-Platform Support**
Status: Works on Linux/macOS
Need: Windows (WSL), FreeBSD testing

**3. Documentation**
Status: Basic README exists
Need: API documentation, more tutorials

### Medium Priority

**4. Octonion Support**
Status: Planned
Need: Design, implementation, testing

**5. Visualization Examples**
Status: Basic examples exist
Need: More complex demos, educational value

**6. Performance**
Status: Good enough for interactive use
Need: Benchmarks, optimization where needed

### Future

**7. Jupyter Integration**
**8. WebAssembly Build**
**9. Package Manager Distribution**

---

## Community Guidelines

### Be Kind

This is a learning environment. We welcome:
- Questions from beginners
- "Dumb" questions (no such thing!)
- Learning in public
- Mistakes (we all make them)

We don't tolerate:
- Gatekeeping ("you should know X")
- Dismissiveness
- Harassment of any kind

### Give Credit

If you use someone's idea, code, or suggestion:
```c
/* Based on suggestion by @username in issue #123 */
```

### Ask Questions

**Before starting major work:**
1. Open an issue discussing it
2. Get feedback from maintainers
3. Agree on approach
4. Then implement

**This prevents:**
- Wasted effort on rejected approaches
- Duplicate work
- Architectural conflicts

---

## Development Setup

### Prerequisites

```bash
# Ubuntu/Debian
sudo apt-get install build-essential git libraylib-dev \
    libsqlite3-dev libsymengine-dev libcqrlib-dev \
    libreadline-dev

# macOS
brew install raylib sqlite symengine cqrlib readline
```

### Build from Source

```bash
git clone https://github.com/yourusername/siod-tr
cd siod-tr
make clean
make
make test
```

### Debug Build

```bash
make DEBUG=1
gdb ./siod
```

### Testing Changes

```bash
# Run specific test
./siod test-quaternions.scm

# Run all tests
make test

# Run with verbose output
./siod -v test-quaternions.scm
```

---

## Documentation

### Code Documentation

**Every function should have:**

```c
/* ============================================
 * quaternion_sinh - Hyperbolic sine
 * ============================================
 * 
 * Computes sinh(q) for quaternion q using:
 *   sinh(q) = (exp(q) - exp(-q)) / 2
 * 
 * Args:
 *   q - Quaternion (type tc_quaternion)
 * 
 * Returns:
 *   Quaternion result, auto-simplified if pure real
 * 
 * See Also:
 *   quaternion_cosh, quaternion_exp
 * ============================================ */
```

### Tutorial Documentation

**Example structure:**

```scheme
;;; ============================================
;;; TUTORIAL: Quaternion Rotations
;;; ============================================
;;; 
;;; This tutorial explains how to use quaternions
;;; for 3D rotations.
;;;
;;; Prerequisites: Basic quaternion arithmetic
;;; Time: 15 minutes
;;; ============================================

;;; PART 1: Why Quaternions for Rotations?
;;; [explanation]

;;; PART 2: Creating Rotations
(define q-rot (quat-from-axis-angle ...))
;;; [explanation]

;;; PART 3: Applying Rotations
;;; [examples]

;;; EXERCISES
;;; 1. ...
;;; 2. ...
```

---

## Getting Help

### Resources

- **Documentation:** `docs/` directory
- **Examples:** `examples/` directory
- **Tests:** `test/` directory
- **Discussions:** GitHub Discussions

### Ask Questions

**Good question:**
```
I'm trying to implement quaternion asin. I understand the formula
asin(q) = -i*log(i*q + sqrt(1-q²)), but I'm not sure how to handle
the sqrt of (1-q²) when it might not be real. Should I:

1. Always return quaternion?
2. Try to simplify?
3. Something else?

Here's my current code: [paste]
```

**Better to ask than to guess!**

---

## Recognition

Contributors are recognized in:
- `CONTRIBUTORS.md`
- Release notes
- Academic papers (if appropriate)

Significant contributions may result in co-authorship on publications.

---

## License

By contributing, you agree that your contributions will be licensed under the BSD License (same as SIOD-TR).

---

## Questions?

- **Email:** your.email@example.com
- **Discussions:** GitHub Discussions
- **Issues:** GitHub Issues

**Welcome to the community! Let's explore baroque mathematics together.** 🎯
