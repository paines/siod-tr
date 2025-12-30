# SIOD-TR Raylib Tutorial

## Lesson 1: Your First Window

Let's start with the absolute basics - opening a window.

```scheme
; Load the raylib module
(require-so (so-ext 'raylib))

; Create a window 800 pixels wide, 600 tall
(init-window 800 600 "My First Window")

; Set target to 60 frames per second
(set-target-fps 60)

; Keep window open until user closes it
(while (not (window-should-close?))
  (begin-drawing)
    (clear-background 'raywhite)
  (end-drawing))

; Clean up
(close-window)
```

**What's happening:**
1. Load the raylib module
2. Create a window with dimensions and title
3. Set frame rate (how many times per second to update)
4. Loop: draw frames until user closes window
5. `begin-drawing` and `end-drawing` wrap each frame
6. `clear-background` fills the screen with a color
7. Close window when done

**Try it:** The window should appear white. Press ESC or click the X to close.

---

## Lesson 2: Drawing Shapes

Let's draw some shapes!

```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Shapes")
(set-target-fps 60)

(while (not (window-should-close?))
  (begin-drawing)
    (clear-background 'raywhite)
    
    ; Draw a red circle in the center
    (draw-circle 400 300 50 'red)
    
    ; Draw a blue rectangle
    (draw-rectangle 100 100 200 150 'blue)
    
    ; Draw a green line
    (draw-line 0 0 800 600 'green)
    
    ; Draw some text
    (draw-text "Hello SIOD-TR!" 10 10 30 'darkgray)
  (end-drawing))

(close-window)
```

**New functions:**
- `draw-circle x y radius color` - draws a filled circle
- `draw-rectangle x y width height color` - draws a filled rectangle
- `draw-line x1 y1 x2 y2 color` - draws a line
- `draw-text text x y size color` - draws text

**Try it:** You should see shapes and text. All drawing happens between `begin-drawing` and `end-drawing`.

---

## Lesson 3: Animation

Let's make something move!

```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Animation")
(set-target-fps 60)

; Position of our circle
(let ((x 0))
  (while (not (window-should-close?))
    ; Move the circle
    (set! x (+ x 3))
    
    ; Wrap around when it goes off screen
    (if (> x 800)
        (set! x 0))
    
    ; Draw
    (begin-drawing)
      (clear-background 'raywhite)
      (draw-circle x 300 30 'blue)
      (draw-text "Watch it move!" 10 10 20 'darkgray)
    (end-drawing)))

(close-window)
```

**Key concepts:**
- `let` creates a variable `x` that persists across frames
- `set!` updates the variable each frame
- The circle moves 3 pixels right each frame
- When `x > 800`, reset to `0` (wraparound)

**Try it:** The blue circle should slide across the screen and wrap around.

---

## Lesson 4: Bouncing Ball

Now let's make it bounce!

```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Bouncing Ball")
(set-target-fps 60)

(let ((x 400)
      (y 300)
      (vx 3)   ; velocity in x direction
      (vy 2))  ; velocity in y direction
  
  (while (not (window-should-close?))
    ; Move the ball
    (set! x (+ x vx))
    (set! y (+ y vy))
    
    ; Bounce off left/right edges
    (if (or (< x 0) (> x 800))
        (set! vx (- 0 vx)))
    
    ; Bounce off top/bottom edges
    (if (or (< y 0) (> y 600))
        (set! vy (- 0 vy)))
    
    ; Draw
    (begin-drawing)
      (clear-background 'raywhite)
      (draw-circle x y 20 'red)
    (end-drawing)))

(close-window)
```

**New concepts:**
- Velocity variables (`vx`, `vy`) control movement
- When hitting an edge, reverse velocity: `(set! vx (- 0 vx))`
- This makes the ball "bounce" off walls

**Try it:** The ball should bounce around the window forever!

---

## Lesson 5: Keyboard Control

Let's control something with the keyboard.

```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Controls")
(set-target-fps 60)

(let ((x 400)
      (y 300)
      (speed 5))
  
  (while (not (window-should-close?))
    ; Check keyboard input
    (if (key-down? KEY_RIGHT) (set! x (+ x speed)))
    (if (key-down? KEY_LEFT)  (set! x (- x speed)))
    (if (key-down? KEY_DOWN)  (set! y (+ y speed)))
    (if (key-down? KEY_UP)    (set! y (- y speed)))
    
    ; Draw
    (begin-drawing)
      (clear-background 'raywhite)
      (draw-circle x y 25 'blue)
      (draw-text "Use arrow keys to move" 10 10 20 'darkgray)
    (end-drawing)))

(close-window)
```

**New function:**
- `key-down? KEY_NAME` - returns true if key is held down

**Common keys:**
- `KEY_UP`, `KEY_DOWN`, `KEY_LEFT`, `KEY_RIGHT` - arrow keys
- `KEY_SPACE` - space bar
- `KEY_ENTER` - enter key
- `KEY_A` through `KEY_Z` - letter keys

**Try it:** Use arrow keys to move the circle around!

---

## Lesson 6: Mouse Interaction

Track the mouse position.

```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Mouse")
(set-target-fps 60)

(while (not (window-should-close?))
  ; Get mouse position
  (let ((pos (mouse-position)))
    (let ((mx (car pos))   ; x coordinate
          (my (cdr pos)))  ; y coordinate
      
      (begin-drawing)
        (clear-background 'raywhite)
        
        ; Draw circle at mouse position
        (draw-circle mx my 30 'green)
        
        ; Draw line from center to mouse
        (draw-line 400 300 mx my 'red)
        
        ; Change color if clicking
        (if (mouse-button-down? MOUSE_LEFT_BUTTON)
            (draw-circle mx my 50 'yellow))
        
        (draw-text "Move your mouse" 10 10 20 'darkgray)
      (end-drawing))))

(close-window)
```

**New functions:**
- `mouse-position` - returns `(x . y)` pair
- `car` - gets x from pair
- `cdr` - gets y from pair
- `mouse-button-down? MOUSE_LEFT_BUTTON` - check if clicking

**Try it:** Move the mouse - a circle should follow it. Click to make it bigger!

---

## Lesson 7: Colors

Different ways to specify colors.

```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Colors")
(set-target-fps 60)

(while (not (window-should-close?))
  (begin-drawing)
    (clear-background 'raywhite)
    
    ; Named colors
    (draw-circle 100 100 40 'red)
    (draw-circle 200 100 40 'blue)
    (draw-circle 300 100 40 'green)
    
    ; RGB colors (Red Green Blue Alpha)
    ; Values are 0-255 for each
    (draw-circle 100 200 40 '(255 0 0 255))     ; Pure red
    (draw-circle 200 200 40 '(0 255 0 255))     ; Pure green
    (draw-circle 300 200 40 '(255 255 0 255))   ; Yellow (red+green)
    
    ; Semi-transparent (low alpha)
    (draw-circle 100 300 40 '(255 0 0 128))     ; Half-transparent red
    (draw-circle 200 300 40 '(0 0 255 128))     ; Half-transparent blue
    
    (draw-text "Named vs RGB colors" 10 10 20 'black)
  (end-drawing))

(close-window)
```

**Color formats:**
1. Named: `'red`, `'blue`, `'green`, `'yellow`, etc.
2. RGB list: `'(R G B A)` where each is 0-255

**Alpha channel:** 255 = fully opaque, 0 = fully transparent

---

## Lesson 8: Simple Game Loop

Putting it all together - a simple game!

```scheme
(require-so (so-ext 'raylib))
(init-window 800 600 "Catch the Ball")
(set-target-fps 60)

(let ((player-x 400)
      (ball-x 400)
      (ball-y 0)
      (ball-speed 3)
      (score 0))
  
  (while (not (window-should-close?))
    ; Player controls
    (if (key-down? KEY_LEFT)  (set! player-x (- player-x 5)))
    (if (key-down? KEY_RIGHT) (set! player-x (+ player-x 5)))
    
    ; Ball falls
    (set! ball-y (+ ball-y ball-speed))
    
    ; Check if caught
    (if (> ball-y 550)
        (begin
          ; Did player catch it?
          (if (and (> ball-x (- player-x 30))
                   (< ball-x (+ player-x 30)))
              (set! score (+ score 1)))
          ; Reset ball
          (set! ball-y 0)
          (set! ball-x (+ 100 (* 600 (get-time))))))  ; Random-ish position
    
    ; Draw
    (begin-drawing)
      (clear-background 'raywhite)
      
      ; Player paddle
      (draw-rectangle (- player-x 30) 550 60 20 'blue)
      
      ; Falling ball
      (draw-circle ball-x ball-y 15 'red)
      
      ; Score
      (draw-text "Score: " 10 10 30 'darkgray)
      (draw-text (number->string score) 120 10 30 'green)
      (draw-text "Use arrow keys" 10 50 20 'gray)
    (end-drawing)))

(close-window)
```

**Game concepts:**
- Player paddle controlled with arrow keys
- Ball falls from top
- If player catches ball, score increases
- Ball resets after hitting bottom

**Try it:** Move the paddle with LEFT/RIGHT arrows and try to catch the red balls!

---

## Next Steps

You now know the basics! Try:

1. **Modify the examples** - Change colors, speeds, sizes
2. **Combine ideas** - Make a bouncing ball you can control
3. **Add features** - Keep track of high score, add obstacles
4. **Study raylib-examples.scm** - See the Mandelbrot fractal renderer

For full API reference, see **RAYLIB-API.md**.

Happy coding! 🎨
