;;; mandelbrot-zoomer.scm (SIOD-compatible version)
;;; Universal Mandelbrot renderer
;;; Complex (ℂ) and Quaternions (ℍ) use polymorphic +, *
;;; Octonions (𝕆) use explicit oct-multiply, oct-add (safer for non-associative)
;;;
;;; Controls:
;;;   Arrow keys: Pan
;;;   = / -:      Zoom in / out
;;;   1:          Complex mode (2D)
;;;   2:          Quaternion mode (4D → 2D slice, w=0, z=0)
;;;   3:          Octonion mode (8D → 2D slice, e3-e7=0)
;;;   ESC:        Exit

;;; ============================================
;;; Mandelbrot Iterators (Type-Specific)
;;; ============================================

;;; Complex and Quaternion: Use polymorphic operators
;;; (Safe because they're associative)
(define (mandelbrot-iter-poly c max-iter)
  (let ((z 0)
        (iter 0))
    (while (and (< iter max-iter)
                (<= (magnitude z) 2.0))
      (set! z (+ (* z z) c))
      (set! iter (+ iter 1)))
    iter))

;;; Octonion: Use explicit functions
;;; (Necessary because non-associativity requires visible operations)
(define (mandelbrot-iter-oct c max-iter)
  (let ((o (oct 0 0 0 0 0 0 0 0))
        (iter 0))
    (while (and (< iter max-iter)
                (<= (oct-norm o) 2.0))
      ;; Explicit: makes non-associativity visible
      (set! o (oct-add (oct-multiply o o) c))
      (set! iter (+ iter 1)))
    iter))

;;; Dispatcher: Choose appropriate iterator based on type
(define (mandelbrot-iter c max-iter)
  (if (octonion? c)
      (mandelbrot-iter-oct c max-iter)
      (mandelbrot-iter-poly c max-iter)))

;;; ============================================
;;; Color Mapping (Pre-computed Palette)
;;; ============================================

;;; Build a palette at load time to avoid allocations in hot loop
(define color-palette
  (let ((palette '()))
    (let ((i 0))
      (while (< i 256)
        (let ((t (/ i 255.0)))
          (set! palette (cons (list (truncate (* t 255))
                                   (truncate (* t 200))
                                   255
                                   255)
                             palette)))
        (set! i (+ i 1))))
    (reverse palette)))

(define black-color '(0 0 0 255))

(define (iter->color iters max-iter)
  (if (>= iters max-iter)
      black-color
      (let ((index (truncate (* (/ iters max-iter) 255))))
        (if (< index 0)
            (car color-palette)
            (if (>= index 256)
                (car (reverse color-palette))
                (list-ref color-palette index))))))

;;; ============================================
;;; Coordinate Mapping
;;; ============================================

(define (map-range value in-min in-max out-min out-max)
  (+ out-min 
     (* (- out-max out-min)
        (/ (- value in-min) (- in-max in-min)))))

;;; ============================================
;;; Number Constructors for Each Mode
;;; ============================================

;;; Mode 1: Complex (ℂ) - Standard 2D Mandelbrot
(define (make-complex-point x y)
  (make-rectangular x y))

;;; Mode 2: Quaternion (ℍ) - 4D slice with w=0, z=0
;;;   Vary: x (real/w), y (i component)
;;;   Fixed: j=0, k=0
(define (make-quat-point x y)
  (quat x y 0 0))

;;; Mode 3: Octonion (𝕆) - 8D slice with e2-e7=0
;;;   Vary: e0 (real), e1 (i)
;;;   Fixed: e2=0, e3=0, e4=0, e5=0, e6=0, e7=0
(define (make-oct-point x y)
  (oct x y 0 0 0 0 0 0))

;;; ============================================
;;; Simple State Management (SIOD-compatible)
;;; ============================================

;;; Global state variables (simpler for SIOD)
(define current-mode 1)        ; 1=complex, 2=quat, 3=oct
(define view-x-min -2.5)
(define view-x-max 1.0)
(define view-y-min -1.0)
(define view-y-max 1.0)
(define max-iterations 100)
(define cols-per-frame 10)

(define (mode-name)
  (cond ((= current-mode 1) "Complex (ℂ)")
        ((= current-mode 2) "Quaternion (ℍ)")
        ((= current-mode 3) "Octonion (𝕆)")
        (else "Unknown")))

;;; ============================================
;;; Point Constructor Dispatch
;;; ============================================

(define (make-point x y)
  (cond ((= current-mode 1) (make-complex-point x y))
        ((= current-mode 2) (make-quat-point x y))
        ((= current-mode 3) (make-oct-point x y))
        (else (error "Invalid mode"))))

;;; ============================================
;;; Interactive Mandelbrot Renderer
;;; ============================================

(define (mandelbrot-zoomer)
  (let ((width 800)
        (height 600))
    
    (init-window width height "SIOD-TR: Universal Mandelbrot")
    (set-target-fps 60)
    
    (let ((need-redraw #t))
      
      (define (render-mandelbrot)
        ; Progressive render - draw cols-per-frame columns at a time
        (let ((x 0))
          (while (< x width)
            (begin-drawing)
            
            ; Render a batch of columns
            (let ((batch-end (min (+ x cols-per-frame) width))
                  (col x))
              (while (< col batch-end)
                (let ((y 0))
                  (while (< y height)
                    (let* ((cx (map-range col 0 width view-x-min view-x-max))
                           (cy (map-range y 0 height view-y-min view-y-max))
                           (c (make-point cx cy))
                           (iters (mandelbrot-iter c max-iterations))
                           (color (iter->color iters max-iterations)))
                      (draw-pixel col y color))
                    (set! y (+ y 1))))
                (set! col (+ col 1))))
            
            ; Show progress and mode
            (draw-text (string-append "Rendering " (mode-name) "... " 
                                     (number->string (truncate (* 100 (/ x width))))
                                     "%")
                      10 10 20 'white)
            (end-drawing)
            
            (set! x (+ x cols-per-frame)))))
      
      (define (handle-input)
        (let ((x-range (- view-x-max view-x-min))
              (y-range (- view-y-max view-y-min))
              (pan-factor 0.1)
              (zoom-factor 0.8))
          
          ; Pan
          (if (key-pressed? KEY_RIGHT)
              (begin
                (set! view-x-min (+ view-x-min (* x-range pan-factor)))
                (set! view-x-max (+ view-x-max (* x-range pan-factor)))
                (set! need-redraw #t)))
          
          (if (key-pressed? KEY_LEFT)
              (begin
                (set! view-x-min (- view-x-min (* x-range pan-factor)))
                (set! view-x-max (- view-x-max (* x-range pan-factor)))
                (set! need-redraw #t)))
          
          (if (key-pressed? KEY_UP)
              (begin
                (set! view-y-min (- view-y-min (* y-range pan-factor)))
                (set! view-y-max (- view-y-max (* y-range pan-factor)))
                (set! need-redraw #t)))
          
          (if (key-pressed? KEY_DOWN)
              (begin
                (set! view-y-min (+ view-y-min (* y-range pan-factor)))
                (set! view-y-max (+ view-y-max (* y-range pan-factor)))
                (set! need-redraw #t)))
          
          ; Zoom in (=)
          (if (key-pressed? KEY_EQUAL)
              (let ((x-center (/ (+ view-x-min view-x-max) 2))
                    (y-center (/ (+ view-y-min view-y-max) 2))
                    (new-x-range (* x-range zoom-factor))
                    (new-y-range (* y-range zoom-factor)))
                (set! view-x-min (- x-center (/ new-x-range 2)))
                (set! view-x-max (+ x-center (/ new-x-range 2)))
                (set! view-y-min (- y-center (/ new-y-range 2)))
                (set! view-y-max (+ y-center (/ new-y-range 2)))
                (set! need-redraw #t)))
          
          ; Zoom out (-)
          (if (key-pressed? KEY_MINUS)
              (let ((x-center (/ (+ view-x-min view-x-max) 2))
                    (y-center (/ (+ view-y-min view-y-max) 2))
                    (new-x-range (/ x-range zoom-factor))
                    (new-y-range (/ y-range zoom-factor)))
                (set! view-x-min (- x-center (/ new-x-range 2)))
                (set! view-x-max (+ x-center (/ new-x-range 2)))
                (set! view-y-min (- y-center (/ new-y-range 2)))
                (set! view-y-max (+ y-center (/ new-y-range 2)))
                (set! need-redraw #t)))
          
          ; Mode switches
          (if (key-pressed? KEY_ONE)
              (begin
                (set! current-mode 1)
                (set! need-redraw #t)))
          
          (if (key-pressed? KEY_TWO)
              (begin
                (set! current-mode 2)
                (set! need-redraw #t)))
          
          (if (key-pressed? KEY_THREE)
              (begin
                (set! current-mode 3)
                (set! need-redraw #t)))))
      
      ; Main loop
      (while (not (window-should-close?))
        (handle-input)
        
        (if need-redraw
            (begin
              (render-mandelbrot)
              (set! need-redraw #f))
            ; Just keep displaying current frame
            (begin
              (begin-drawing)
              (draw-text (string-append (mode-name) 
                                       " | Arrows:Pan | =/- :Zoom | 1/2/3:Mode")
                        10 10 20 'white)
              (end-drawing)))))
    
    (close-window)))

;;; ============================================
;;; Quick Test
;;; ============================================

(define (mandelbrot-test)
  (puts "Testing universal Mandelbrot iterator...\n")
  
  ; Test with complex number
  (let ((c1 (make-rectangular 0.0 0.0)))
    (puts "Complex (0, 0): ")
    (print (mandelbrot-iter c1 100))
    (puts "\n"))
  
  ; Test with quaternion
  (let ((c2 (quat 0.0 0.0 0.0 0.0)))
    (puts "Quaternion (0, 0, 0, 0): ")
    (print (mandelbrot-iter c2 100))
    (puts "\n"))
  
  ; Test with octonion
  (let ((c3 (oct 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)))
    (puts "Octonion (0, 0, 0, 0, 0, 0, 0, 0): ")
    (print (mandelbrot-iter c3 100))
    (puts "\n"))
  
  ; Test with complex in set
  (let ((c4 (make-rectangular -0.5 0.0)))
    (puts "Complex (-0.5, 0): ")
    (print (mandelbrot-iter c4 100))
    (puts " (inside set)\n"))
  
  ; Test with complex outside set
  (let ((c5 (make-rectangular 1.0 1.0)))
    (puts "Complex (1, 1): ")
    (print (mandelbrot-iter c5 100))
    (puts " (outside set)\n"))
  
  (puts "\nTest complete! Run (mandelbrot-zoomer) to start.\n"))

;;; To run:
;;; (load "mandelbrot-zoomer.scm")
;;; (mandelbrot-test)      ; Quick test
;;; (mandelbrot-zoomer)    ; Interactive renderer
