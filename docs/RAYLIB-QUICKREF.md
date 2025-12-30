# SIOD-TR Raylib Quick Reference

## Setup & Teardown
```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Title")
(set-target-fps 60)
; ... your code ...
(close-window)
```

## Basic Loop Pattern
```scheme
(while (not (window-should-close?))
  (begin-drawing)
    (clear-background 'raywhite)
    ; ... draw here ...
  (end-drawing))
```

## Drawing Functions
| Function | Example |
|----------|---------|
| `draw-pixel` | `(draw-pixel 100 100 'red)` |
| `draw-circle` | `(draw-circle 400 300 50 'blue)` |
| `draw-rectangle` | `(draw-rectangle 10 10 100 50 'green)` |
| `draw-line` | `(draw-line 0 0 800 600 'red)` |
| `draw-text` | `(draw-text "Hello" 10 10 20 'black)` |

## Colors
**Named:** `'red 'blue 'green 'yellow 'black 'white 'gray 'raywhite ...`

**RGB:** `'(255 0 0 255)` = (R G B Alpha)

## Keyboard Input
| Function | When to Use |
|----------|-------------|
| `(key-pressed? KEY_SPACE)` | Single tap detection |
| `(key-down? KEY_RIGHT)` | Held key (movement) |
| `(key-released? KEY_SHIFT)` | Key release event |

**Common Keys:** `KEY_UP KEY_DOWN KEY_LEFT KEY_RIGHT KEY_SPACE KEY_ENTER KEY_ESCAPE`  
**Letters:** `KEY_A` through `KEY_Z`  
**Numbers:** `KEY_0` through `KEY_9`

## Mouse Input
```scheme
(let ((pos (mouse-position)))
  (let ((x (car pos))
        (y (cdr pos)))
    (draw-circle x y 10 'red)))

(if (mouse-button-pressed? MOUSE_LEFT_BUTTON)
    (do-click))
```

## Timing
```scheme
(get-time)        ; Seconds since init
(get-frame-time)  ; Seconds for last frame
(get-fps)         ; Current FPS
```

## Complete Minimal Example
```scheme
(require-so (so-ext 'raylib))
(init-window 400 300 "Test")
(set-target-fps 60)

(while (not (window-should-close?))
  (begin-drawing)
  (clear-background 'raywhite)
  (draw-circle 200 150 50 'red)
  (draw-text "Press ESC" 10 10 20 'darkgray)
  (end-drawing))

(close-window)
```

## Common Patterns

### Movement
```scheme
(if (key-down? KEY_RIGHT) (set! x (+ x 5)))
(if (key-down? KEY_LEFT)  (set! x (- x 5)))
```

### Bouncing
```scheme
(if (or (< x 0) (> x 800))
    (set! vx (- 0 vx)))
```

### Animation
```scheme
(let ((angle (* (get-time) 2)))
  (let ((x (+ 400 (* 100 (sin angle))))
        (y (+ 300 (* 100 (cos angle)))))
    (draw-circle x y 20 'blue)))
```

### FPS Display
```scheme
(draw-text (number->string (get-fps)) 750 10 20 'green)
```
