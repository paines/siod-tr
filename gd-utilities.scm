;;; gd-utilities.scm - High-level utilities for GD Graphics Library
;;;
;;; Provides convenient wrapper functions for common GD operations

(require-so (so-ext "gd"))

;;; ============================================================================
;;; Image Creation Helpers
;;; ============================================================================

(define (gd:create-image width height)
  "Create a new image with standard white background"
  (let ((img (gdImageCreate width height)))
    ;; First color allocated is background
    (gdImageColorAllocate img 255 255 255)
    img))

(define (gd:create-image-with-bg width height r g b)
  "Create a new image with specified background color"
  (let ((img (gdImageCreate width height)))
    (gdImageColorAllocate img r g b)
    img))

;;; ============================================================================
;;; Color Helpers
;;; ============================================================================

(define (gd:rgb img r g b)
  "Allocate an RGB color, creating it if it doesn't exist"
  (let ((exact (gdImageColorExact img r g b)))
    (if (< exact 0)
        (gdImageColorAllocate img r g b)
        exact)))

;; Pre-defined common colors (functions that allocate on demand)
(define (gd:black img) (gd:rgb img 0 0 0))
(define (gd:white img) (gd:rgb img 255 255 255))
(define (gd:red img) (gd:rgb img 255 0 0))
(define (gd:green img) (gd:rgb img 0 255 0))
(define (gd:blue img) (gd:rgb img 0 0 255))
(define (gd:yellow img) (gd:rgb img 255 255 0))
(define (gd:cyan img) (gd:rgb img 0 255 255))
(define (gd:magenta img) (gd:rgb img 255 0 255))
(define (gd:gray img) (gd:rgb img 128 128 128))
(define (gd:dark-gray img) (gd:rgb img 64 64 64))
(define (gd:light-gray img) (gd:rgb img 192 192 192))

;;; ============================================================================
;;; Drawing Helpers
;;; ============================================================================

(define (gd:box img x y width height color)
  "Draw a filled rectangle (box) - more intuitive than x1,y1,x2,y2"
  (gdImageFilledRectangle img x y (+ x width) (+ y height) color))

(define (gd:box-outline img x y width height color)
  "Draw a rectangle outline - more intuitive parameter order"
  (gdImageRectangle img x y (+ x width) (+ y height) color))

(define (gd:circle img x y radius color)
  "Draw a circle outline"
  (let ((diameter (* radius 2)))
    (gdImageArc img x y diameter diameter 0 360 color)))

(define (gd:filled-circle img x y radius color)
  "Draw a filled circle using arc (GD doesn't have native filled circle)"
  (let ((diameter (* radius 2)))
    ;; Draw multiple concentric circles to fill
    (let loop ((r radius))
      (if (>= r 0)
          (begin
            (gdImageArc img x y (* r 2) (* r 2) 0 360 color)
            (loop (- r 1)))))))

(define (gd:cross img x y size color)
  "Draw a cross/plus symbol centered at x,y"
  (gdImageLine img (- x size) y (+ x size) y color)
  (gdImageLine img x (- y size) x (+ y size) color))

(define (gd:x-mark img x y size color)
  "Draw an X symbol centered at x,y"
  (gdImageLine img (- x size) (- y size) (+ x size) (+ y size) color)
  (gdImageLine img (- x size) (+ y size) (+ x size) (- y size) color))

;;; ============================================================================
;;; Text Helpers
;;; ============================================================================

(define (gd:text img x y text color . font)
  "Draw text with optional font (default: gdFontLarge)"
  (let ((f (if (null? font) gdFontLarge (car font))))
    (gdImageString img f x y text color)))

(define (gd:centered-text img y text color . font)
  "Draw text centered horizontally at height y"
  (let* ((f (if (null? font) gdFontLarge (car font)))
         (fw (gdFont.w f))
         (text-width (* (string-length text) fw))
         ;; Assume image is available - would need to store or pass width
         (x (- 200 (quotient text-width 2)))) ; Default to 400px wide image
    (gdImageString img f x y text color)))

(define (gd:text-size font text)
  "Calculate the width and height of text in pixels"
  (let ((w (gdFont.w font))
        (h (gdFont.h font)))
    (cons (* (string-length text) w) h)))

;;; ============================================================================
;;; Polygon Helpers
;;; ============================================================================

(define (gd:triangle img x1 y1 x2 y2 x3 y3 color filled?)
  "Draw a triangle - filled if filled? is true"
  (let ((points (gdPoint x1 y1 x2 y2 x3 y3)))
    (if filled?
        (gdImageFilledPolygon img points color)
        (gdImagePolygon img points color))))

(define (gd:regular-polygon img cx cy n radius color filled?)
  "Draw a regular polygon with n sides, centered at (cx,cy) with given radius"
  (let* ((angle-step (/ (* 2 3.14159265) n))
         (points (let loop ((i 0) (acc '()))
                   (if (>= i n)
                       (apply gdPoint (reverse acc))
                       (let* ((angle (+ (* i angle-step) (- (/ 3.14159265 2))))
                              (x (+ cx (* radius (cos angle))))
                              (y (+ cy (* radius (sin angle)))))
                         (loop (+ i 1) (cons (inexact->exact y) (cons (inexact->exact x) acc))))))))
    (if filled?
        (gdImageFilledPolygon img points color)
        (gdImagePolygon img points color))))

;;; ============================================================================
;;; Graph/Chart Helpers
;;; ============================================================================

(define (gd:axes img x0 y0 width height color)
  "Draw X and Y axes for a graph"
  (gdImageLine img x0 (- y0 height) x0 y0 color)  ; Y-axis
  (gdImageLine img x0 y0 (+ x0 width) y0 color))   ; X-axis

(define (gd:grid img x0 y0 width height step-x step-y color)
  "Draw a grid for graphing"
  ;; Vertical lines
  (let loop-x ((x x0))
    (if (<= x (+ x0 width))
        (begin
          (gdImageLine img x (- y0 height) x y0 color)
          (loop-x (+ x step-x)))))
  ;; Horizontal lines
  (let loop-y ((y y0))
    (if (>= y (- y0 height))
        (begin
          (gdImageLine img x0 y (+ x0 width) y color)
          (loop-y (- y step-y))))))

(define (gd:plot-points img points color)
  "Plot a series of points as small crosses. Points is a list of (x . y) pairs"
  (define (plot-point p)
    (let ((x (car p))
          (y (cdr p)))
      (gd:cross img x y 2 color)))
  (map plot-point points))

(define (gd:plot-line img points color)
  "Draw a line connecting a series of points. Points is a list of (x . y) pairs"
  (define (connect-points p1 p2)
    (gdImageLine img (car p1) (cdr p1) (car p2) (cdr p2) color))
  (if (> (length points) 1)
      (let loop ((pts points))
        (if (null? (cdr pts))
            #t
            (begin
              (connect-points (car pts) (cadr pts))
              (loop (cdr pts)))))))

;;; ============================================================================
;;; File I/O Helpers
;;; ============================================================================

(define (gd:save-gif img filename)
  "Save image to a GIF file (convenience wrapper)"
  (let ((f (fopen filename "wb")))
    (if f
        (begin
          (gdImageGif img f)
          (fclose f)
          #t)
        #f)))

(define (gd:load-gif filename)
  "Load image from a GIF file (convenience wrapper)"
  (let ((f (fopen filename "rb")))
    (if f
        (let ((img (gdImageCreateFromGif f)))
          (fclose f)
          img)
        #f)))

;;; ============================================================================
;;; Complete Example Wrapper
;;; ============================================================================

(define (gd:quick-image width height draw-func filename)
  "Create, draw on, and save an image in one call
   draw-func is called with (img) and should draw on the image"
  (let ((img (gd:create-image width height)))
    (draw-func img)
    (gd:save-gif img filename)
    img))

;;; ============================================================================
;;; Example Usage
;;; ============================================================================

(define (gd-utilities-demo)
  "Demonstration of the utility functions"
  (puts "Creating demonstration image with gd-utilities...\n")
  
  (gd:quick-image 400 300
    (lambda (img)
      ;; Use convenient color functions
      (let ((black (gd:black img))
            (red (gd:red img))
            (blue (gd:blue img))
            (green (gd:green img)))
        
        ;; Draw shapes with easier functions
        (gd:box img 50 50 100 80 red)
        (gd:box-outline img 200 50 100 80 blue)
        (gd:circle img 100 200 40 green)
        (gd:filled-circle img 250 200 30 blue)
        
        ;; Draw symbols
        (gd:cross img 320 100 10 black)
        (gd:x-mark img 350 100 10 red)
        
        ;; Draw text
        (gd:text img 10 10 "GD Utilities Demo" black gdFontGiant)
        
        ;; Draw a hexagon
        (gd:regular-polygon img 200 150 6 40 green #f)))
    "demo-utilities.gif")
  
  (puts "Saved to demo-utilities.gif\n"))

(puts "GD utilities loaded. Try (gd-utilities-demo) for a demo.\n")
