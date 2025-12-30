# SIOD-TR Raylib Module Documentation

## Overview

The raylib module provides interactive 2D graphics capabilities for SIOD-TR. It follows an immediate-mode rendering model where you draw fresh each frame.

**Version:** Phase 1 - Basic 2D Graphics  
**Module File:** raylib.so  
**Raylib Version:** 5.0+

---

## Loading the Module

```scheme
(require-so (so-ext 'raylib))
```

This dynamically loads the raylib module into SIOD-TR.

---

## Core Concepts

### Immediate-Mode Rendering

Each frame follows this pattern:

```scheme
(while (not (window-should-close?))
  (begin-drawing)
    ;; All drawing happens here
    (clear-background 'raywhite)
    (draw-circle 100 100 50 'red)
  (end-drawing))
```

### Color Values

Colors can be specified as:

**1. Named color symbols:**
```scheme
'raywhite 'lightgray 'gray 'darkgray 'black
'red 'maroon 'orange 'yellow 'lime 'green 'darkgreen
'skyblue 'blue 'darkblue 'purple 'violet 'darkpurple
'beige 'brown 'darkbrown
'white 'magenta 'pink 'gold
```

**2. RGB/RGBA lists:**
```scheme
'(255 0 0 255)      ; Red - (R G B A)
'(0 255 0 255)      ; Green
'(0 0 255 128)      ; Blue, semi-transparent
```

Values are 0-255 for each channel.

---

## API Reference

### Window Management

#### `(init-window width height title)`
Create and open a window.

**Parameters:**
- `width` - Window width in pixels (integer)
- `height` - Window height in pixels (integer)  
- `title` - Window title (string)

**Returns:** NIL

**Example:**
```scheme
(init-window 800 600 "My SIOD-TR Graphics")
```

---

#### `(close-window)`
Close the window and free resources.

**Parameters:** None

**Returns:** NIL

**Note:** Always call this before exiting to clean up properly.

**Example:**
```scheme
(close-window)
```

---

#### `(window-should-close?)`
Check if the user requested to close the window (clicked X or pressed ESC by default).

**Parameters:** None

**Returns:** Boolean (true if should close, false otherwise)

**Example:**
```scheme
(while (not (window-should-close?))
  ; Main loop
  )
```

---

#### `(set-target-fps fps)`
Set the target frames per second.

**Parameters:**
- `fps` - Target frame rate (integer, typically 60)

**Returns:** NIL

**Example:**
```scheme
(set-target-fps 60)  ; 60 frames per second
```

---

#### `(get-fps)`
Get the current frames per second.

**Parameters:** None

**Returns:** Integer (current FPS)

**Example:**
```scheme
(let ((fps (get-fps)))
  (display fps))
```

---

### Frame Drawing

#### `(begin-drawing)`
Begin drawing for the current frame. Must be called before any draw operations.

**Parameters:** None

**Returns:** NIL

**Example:**
```scheme
(begin-drawing)
  ; All draw calls here
(end-drawing)
```

---

#### `(end-drawing)`
End drawing for the current frame. Swaps buffers and displays the frame.

**Parameters:** None

**Returns:** NIL

**Note:** Must match each `begin-drawing` call.

---

#### `(clear-background color)`
Clear the screen with a color.

**Parameters:**
- `color` - Color symbol or RGBA list

**Returns:** NIL

**Example:**
```scheme
(clear-background 'raywhite)
(clear-background '(50 50 50 255))  ; Dark gray
```

---

### Drawing Primitives

#### `(draw-pixel x y color)`
Draw a single pixel.

**Parameters:**
- `x` - X coordinate (integer)
- `y` - Y coordinate (integer)
- `color` - Color symbol or RGBA list

**Returns:** NIL

**Example:**
```scheme
(draw-pixel 100 100 'red)
(draw-pixel 200 200 '(0 255 0 255))
```

---

#### `(draw-circle x y radius color)`
Draw a filled circle.

**Parameters:**
- `x` - Center X coordinate (integer)
- `y` - Center Y coordinate (integer)
- `radius` - Circle radius (integer)
- `color` - Color symbol or RGBA list

**Returns:** NIL

**Example:**
```scheme
(draw-circle 400 300 50 'blue)
(draw-circle 100 100 30 '(255 0 0 128))  ; Semi-transparent red
```

---

#### `(draw-rectangle x y width height color)`
Draw a filled rectangle.

**Parameters:**
- `x` - Top-left X coordinate (integer)
- `y` - Top-left Y coordinate (integer)
- `width` - Rectangle width (integer)
- `height` - Rectangle height (integer)
- `color` - Color symbol or RGBA list

**Returns:** NIL

**Example:**
```scheme
(draw-rectangle 100 100 200 150 'green)
(draw-rectangle 0 0 800 50 '(100 100 100 255))  ; Gray bar
```

---

#### `(draw-line x1 y1 x2 y2 color)`
Draw a line between two points.

**Parameters:**
- `x1` - Start X coordinate (integer)
- `y1` - Start Y coordinate (integer)
- `x2` - End X coordinate (integer)
- `y2` - End Y coordinate (integer)
- `color` - Color symbol or RGBA list

**Returns:** NIL

**Example:**
```scheme
(draw-line 0 0 800 600 'red)
(draw-line 400 0 400 600 '(255 255 0 255))  ; Yellow vertical line
```

---

#### `(draw-text text x y size color)`
Draw text on the screen.

**Parameters:**
- `text` - Text to display (string)
- `x` - Top-left X coordinate (integer)
- `y` - Top-left Y coordinate (integer)
- `size` - Font size (integer)
- `color` - Color symbol or RGBA list

**Returns:** NIL

**Example:**
```scheme
(draw-text "Hello SIOD-TR!" 10 10 20 'darkgray)
(draw-text "Score: 100" 700 10 30 'red)
```

---

### Input - Keyboard

#### `(key-pressed? key)`
Check if a key was pressed this frame (single event).

**Parameters:**
- `key` - Key constant (see Key Constants below)

**Returns:** Boolean (true if just pressed)

**Example:**
```scheme
(if (key-pressed? KEY_SPACE)
    (fire-bullet))
```

---

#### `(key-down? key)`
Check if a key is currently held down.

**Parameters:**
- `key` - Key constant

**Returns:** Boolean (true if held down)

**Example:**
```scheme
(if (key-down? KEY_RIGHT)
    (set! x (+ x speed)))
```

---

#### `(key-released? key)`
Check if a key was released this frame.

**Parameters:**
- `key` - Key constant

**Returns:** Boolean (true if just released)

**Example:**
```scheme
(if (key-released? KEY_SHIFT)
    (set! running #f))
```

---

### Input - Mouse

#### `(mouse-button-pressed? button)`
Check if a mouse button was pressed this frame.

**Parameters:**
- `button` - Mouse button constant (MOUSE_LEFT_BUTTON, MOUSE_RIGHT_BUTTON, MOUSE_MIDDLE_BUTTON)

**Returns:** Boolean

**Example:**
```scheme
(if (mouse-button-pressed? MOUSE_LEFT_BUTTON)
    (place-object (mouse-position)))
```

---

#### `(mouse-button-down? button)`
Check if a mouse button is currently held down.

**Parameters:**
- `button` - Mouse button constant

**Returns:** Boolean

**Example:**
```scheme
(if (mouse-button-down? MOUSE_LEFT_BUTTON)
    (draw-trail (mouse-position)))
```

---

#### `(mouse-position)`
Get the current mouse position.

**Parameters:** None

**Returns:** Pair (cons cell) of (x . y) coordinates

**Example:**
```scheme
(let ((pos (mouse-position)))
  (let ((x (car pos))
        (y (cdr pos)))
    (draw-circle x y 10 'red)))
```

---

### Timing

#### `(get-frame-time)`
Get time elapsed for the last frame in seconds.

**Parameters:** None

**Returns:** Float (seconds)

**Example:**
```scheme
(let ((dt (get-frame-time)))
  (set! x (+ x (* velocity dt))))  ; Frame-rate independent movement
```

---

#### `(get-time)`
Get elapsed time since init-window in seconds.

**Parameters:** None

**Returns:** Float (seconds)

**Example:**
```scheme
(let ((t (get-time)))
  (let ((wave (sin t)))
    (draw-circle 400 (+ 300 (* 50 wave)) 20 'blue)))
```

---

## Key Constants

### Arrow Keys
- `KEY_UP` - Up arrow
- `KEY_DOWN` - Down arrow
- `KEY_LEFT` - Left arrow
- `KEY_RIGHT` - Right arrow

### Special Keys
- `KEY_SPACE` - Space bar
- `KEY_ENTER` - Enter/Return
- `KEY_ESCAPE` - Escape (closes window by default)
- `KEY_BACKSPACE` - Backspace
- `KEY_TAB` - Tab
- `KEY_SHIFT` - Shift (left or right)
- `KEY_CONTROL` - Control (left or right)
- `KEY_ALT` - Alt (left or right)

### Letters (A-Z)
- `KEY_A` through `KEY_Z`

### Numbers
- `KEY_0` through `KEY_9`

### Function Keys
- `KEY_F1` through `KEY_F12`

---

## Mouse Button Constants

- `MOUSE_LEFT_BUTTON` - Left mouse button
- `MOUSE_RIGHT_BUTTON` - Right mouse button
- `MOUSE_MIDDLE_BUTTON` - Middle mouse button

---

## Common Patterns

### Basic Animation Loop

```scheme
(init-window 800 600 "Animation")
(set-target-fps 60)

(let ((x 0))
  (while (not (window-should-close?))
    (set! x (+ x 2))
    (if (> x 800) (set! x 0))
    
    (begin-drawing)
    (clear-background 'raywhite)
    (draw-circle x 300 20 'red)
    (end-drawing)))

(close-window)
```

---

### Keyboard-Controlled Movement

```scheme
(init-window 800 600 "Controls")
(set-target-fps 60)

(let ((x 400)
      (y 300)
      (speed 5))
  (while (not (window-should-close?))
    (if (key-down? KEY_RIGHT) (set! x (+ x speed)))
    (if (key-down? KEY_LEFT)  (set! x (- x speed)))
    (if (key-down? KEY_DOWN)  (set! y (+ y speed)))
    (if (key-down? KEY_UP)    (set! y (- y speed)))
    
    (begin-drawing)
    (clear-background 'raywhite)
    (draw-circle x y 25 'blue)
    (end-drawing)))

(close-window)
```

---

### Mouse Tracking

```scheme
(init-window 800 600 "Mouse")
(set-target-fps 60)

(while (not (window-should-close?))
  (let ((pos (mouse-position)))
    (begin-drawing)
    (clear-background 'raywhite)
    (draw-circle (car pos) (cdr pos) 30 'green)
    (if (mouse-button-down? MOUSE_LEFT_BUTTON)
        (draw-circle (car pos) (cdr pos) 50 'red))
    (end-drawing)))

(close-window)
```

---

### Frame-Rate Independent Movement

```scheme
(init-window 800 600 "Smooth Movement")
(set-target-fps 60)

(let ((x 0)
      (velocity 100))  ; pixels per second
  (while (not (window-should-close?))
    (let ((dt (get-frame-time)))
      (set! x (+ x (* velocity dt)))
      (if (> x 800) (set! x 0))
      
      (begin-drawing)
      (clear-background 'raywhite)
      (draw-circle x 300 20 'blue)
      (end-drawing))))

(close-window)
```

---

### Pixel-Based Graphics

```scheme
(init-window 400 300 "Pixel Art")
(set-target-fps 60)

(begin-drawing)
(clear-background 'black)

; Draw gradient
(let ((x 0))
  (while (< x 400)
    (let ((y 0))
      (while (< y 300)
        (let ((r (floor (* x 0.6)))
              (g (floor (* y 0.8)))
              (b 128))
          (draw-pixel x y (list r g b 255)))
        (set! y (+ y 1))))
    (set! x (+ x 1))))

(end-drawing)

(while (not (window-should-close?))
  (begin-drawing)
  (end-drawing))

(close-window)
```

---

## Performance Tips

1. **Minimize draw calls** - Each primitive has overhead. Batch when possible.

2. **Clear once per frame** - Call `clear-background` at the start of each frame.

3. **Use appropriate FPS** - 60 FPS is standard, but 30 may be fine for some applications.

4. **Cache calculations** - Don't recalculate values that don't change each frame.

5. **Frame-rate independence** - Use `get-frame-time` for physics/movement.

---

## Limitations (Phase 1)

- No texture loading (coming in Phase 2)
- No camera/viewport control (coming in Phase 2)
- No text measurement
- No custom fonts
- No audio
- Single pixel drawing is slow for large operations (use sparingly)

---

## Examples

See `raylib-examples.scm` for complete working examples:
- `raylib-test` - Animated shapes
- `hello-world-1` - Bouncing ball
- `hello-world-interactive` - Keyboard controls
- `hello-world-mouse` - Mouse tracking
- `mandelbrot-static` - Fractal rendering

---

## Getting Help

If a function isn't working:
1. Check you called `begin-drawing` before drawing
2. Verify window is initialized with `init-window`
3. Check color format (symbol or list of 4 integers)
4. Ensure key/button constants are defined

For questions or bug reports, see the SIOD-TR documentation.
