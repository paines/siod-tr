# Solar System Gravity Simulation Guide

## What You've Got

Two complete N-body gravity simulations written in **pure Scheme**!

### Files

**solar-system.scm** - Full solar system
- 8 bodies (Sun + 7 planets + asteroid)
- Realistic orbital mechanics
- Camera controls for pan/zoom
- Persistent orbit trails

**simple-orbit.scm** - Two-body test
- Just Sun and Earth
- Perfect for understanding the physics
- Easier to verify behavior

---

## Quick Start

```scheme
> (load "simple-orbit.scm")
Simple orbit test loaded!

> (simple-orbit)
```

Watch Earth orbit the Sun in a circular path!

**Controls:**
- **SPACE** - Pause/unpause
- **R** - Reset to initial conditions
- **ESC** - Exit

Then try the full system:

```scheme
> (load "solar-system.scm")
Solar System Simulation loaded!

> (solar-system)
```

**Controls:**
- **Arrow keys** - Pan camera
- **+/-** - Zoom in/out
- **SPACE** - Pause/unpause
- **R** - Reset simulation
- **ESC** - Exit

---

## How It Works

### 1. Vector Math (Pure Scheme)

```scheme
(define (vec x y) (cons x y))           ; Create vector
(define (vec+ v1 v2) ...)               ; Add vectors
(define (vec-scale v s) ...)            ; Multiply by scalar
(define (vec-mag v) ...)                ; Magnitude
```

All vector operations in clean, functional Scheme!

### 2. N-Body Gravity

For each planet:
1. Calculate force from every other planet
2. Sum all forces → total force
3. F = ma → calculate acceleration
4. Update velocity: v_new = v_old + a × dt
5. Update position: p_new = p_old + v × dt

**Euler integration** - simple and fast!

### 3. Persistent Trails

The brilliant trick:
```scheme
;; Create trail texture
(create-render-texture 800 600)
(begin-texture-mode)
  (clear-background 'black)  ; Clear ONCE at start
(end-texture-mode)

;; Each frame, ADD to trails (never clear!)
(begin-texture-mode)
  (draw-circle planet-x planet-y 1 'white)  ; Add dot
(end-texture-mode)

;; Display accumulated trails
(draw-render-texture 0 0 'white)
```

Trails accumulate forever, showing the complete orbital history!

### 4. Camera Controls

Pan and zoom to explore:
```scheme
(camera-move dx dy)      ; Pan
(camera-zoom-by factor)  ; Zoom
```

Follow planets, zoom out to see full system!

---

## The Physics

### Gravitational Force

```
F = G × m1 × m2 / r²
```

Where:
- G = gravitational constant (scaled to 1.0 for nice visuals)
- m1, m2 = masses of the two bodies
- r = distance between them

### Initial Conditions

**Sun:** Massive (1000), stationary at center

**Planets:** Circular orbits with:
```
v = sqrt(G × M_sun / r)
```

This gives stable, roughly circular orbits!

### Timestep

```scheme
(define dt 0.016)  ; ~60 FPS
```

Smaller = more accurate but slower  
Larger = faster but less stable

---

## Tweaking the Simulation

### Add More Planets

```scheme
(define (create-solar-system)
  (list
    (make-planet "Sun" 400 300 0 0 1000 15 '(255 255 0 255))
    (make-planet "Earth" 520 300 0 5.5 1.0 5 '(0 100 255 255))
    
    ;; Add your own!
    (make-planet "MyPlanet" 600 300 0 4.0 2.0 6 '(255 0 255 255))
  ))
```

### Change Initial Velocities

Make elliptical orbits:
```scheme
;; Circular orbit: velocity tangent to radius
(make-planet "Earth" 520 300 0 5.5 ...)

;; Elliptical: add radial component
(make-planet "Comet" 520 300 2.0 5.5 ...)
```

### Adjust Gravity

```scheme
(define G 2.0)  ; Stronger gravity (tighter orbits)
(define G 0.5)  ; Weaker gravity (wider orbits)
```

### Speed Up/Slow Down

```scheme
(define steps-per-frame 2)  ; Double speed
(define steps-per-frame 5)  ; 5x speed
(define dt 0.008)           ; Smaller timestep (more accurate)
```

---

## Performance

### Current Performance

**simple-orbit.scm:**
- 1 force calculation/frame
- ~200 floating point ops/frame
- 60 FPS easily

**solar-system.scm:**
- 8 bodies = 28 force calculations/frame
- ~2000 floating point ops/frame  
- Should still run at 60 FPS!

### If It's Slow

1. **Reduce planets** - fewer bodies = faster
2. **Increase timestep** - `(define dt 0.032)`
3. **Skip frames** - draw every 2nd frame
4. **Reduce trail quality** - don't draw trails every frame

### If You Want MORE

Add these to sliba.c for speed:

```c
// Fast vector operations in C
static LISP lvec_add(LISP v1, LISP v2) {
    double x1 = FLONM(car(v1));
    double y1 = FLONM(car(cdr(v1)));
    double x2 = FLONM(car(v2));
    double y2 = FLONM(car(cdr(v2)));
    return cons(flocons(x1 + x2),
                cons(flocons(y1 + y2), NIL));
}
```

But try pure Scheme first - it should be fast enough!

---

## Fun Experiments

### 1. Binary Star System

```scheme
(list
  (make-planet "Star1" 350 300 0 2.0 500 12 '(255 255 0 255))
  (make-planet "Star2" 450 300 0 -2.0 500 12 '(255 200 0 255))
  (make-planet "Planet" 400 250 3.0 0 1.0 5 '(0 100 255 255)))
```

Two stars orbit each other, planet orbits both!

### 2. Figure-8 Orbit

Three equal masses in a figure-8 pattern (very sensitive to initial conditions!)

### 3. Planetary Collision

Set two planets on collision course and watch!

### 4. Asteroid Swarm

Add 20 small asteroids with random velocities.

### 5. Extreme Zoom

Use camera to zoom WAY in on a single planet and watch it closely.

---

## What's Amazing

You just built a **real physics simulation** in **pure Scheme**!

✨ N-body gravity  
✨ Numerical integration  
✨ Real-time rendering  
✨ Interactive controls  
✨ 300 lines of elegant code  

**No C changes needed!** Everything SIOD-TR already had was sufficient!

This is the power of:
- Good math libraries (sqrt, sin, cos)
- Flexible data structures (lists)
- Render textures (persistent trails)
- Camera controls (exploration)

Built on a **1990s Scheme interpreter** running **2020s graphics**!

The "Scáthify" philosophy in action! 🎨

---

## Next Steps

Want to go further?

1. **3D orbits** - add Z coordinate
2. **Gravitational slingshots** - shoot a probe
3. **Orbital mechanics** - calculate aphelion/perihelion
4. **Roche limit** - tidal forces breaking planets
5. **Relativistic effects** - GR corrections
6. **N-body optimization** - Barnes-Hut tree

But first, just enjoy watching the planets orbit! 🪐

The trails will draw beautiful patterns as they go.

Press SPACE to pause and admire the orbits.

Press R to reset and try different initial conditions.

Have fun, my friendly winkybottom! 😄
