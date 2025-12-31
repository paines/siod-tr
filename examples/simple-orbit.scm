;;; Fixed Simple Orbit Test
;;; With debug output to see what's happening

(require-so (so-ext 'raylib))

(define (defined? sym)
  (symbol-bound? sym (the-environment)))

;;; Vector math
(define (vec x y) (cons x y))
(define (vec-x v) (car v))
(define (vec-y v) (cdr v))

(define (vec+ v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

(define (vec- v1 v2)
  (vec (- (vec-x v1) (vec-x v2))
       (- (vec-y v1) (vec-y v2))))

(define (vec-scale v s)
  (vec (* (vec-x v) s)
       (* (vec-y v) s)))

(define (vec-mag-squared v)
  (+ (* (vec-x v) (vec-x v))
     (* (vec-y v) (vec-y v))))

(define (vec-normalize v)
  (let ((mag (sqrt (vec-mag-squared v))))
    (if (< mag 0.0001)
        (vec 0 0)
        (vec-scale v (/ 1.0 mag)))))

;;; Simple orbit - FIXED VERSION
(define (simple-orbit-fixed)
  (define width 800)
  (define height 600)
  (define G 1.0)
  (define dt 0.016)
  
  ;; Sun at center
  (define sun-pos (vec 400 300))
  (define sun-mass 1000)
  
  ;; Earth orbiting - start at distance 120 pixels
  (define earth-pos (vec 520 300))
  (define earth-vel (vec 0 5.5))
  (define earth-mass 1.0)
  
  ;; Graphics
  (init-window width height "Simple Orbit Test - SPACE: Pause | ESC: Exit")
  (set-target-fps 60)
  
  ;; NO render texture for now - just draw directly
  (define paused #f)
  (define frame-count 0)
  
  (display "Starting simple orbit simulation...")
  (newline)
  (display "Sun at: (400, 300)")
  (newline)
  (display "Earth starts at: (520, 300)")
  (newline)
  (display "Initial velocity: (0, 5.5)")
  (newline)
  
  (while (not (window-should-close?))
    
    ;; Input
    (if (key-pressed? KEY_SPACE)
        (begin
          (set! paused (not paused))
          (if paused
              (display "PAUSED")
              (display "RESUMED"))
          (newline)))
    
    ;; Physics
    (if (not paused)
        (begin
          ;; Calculate force
          (let* ((r-vec (vec- sun-pos earth-pos))
                 (r-squared (vec-mag-squared r-vec))
                 (r-mag (sqrt r-squared)))
            (if (> r-mag 1.0)
                (let* ((f-mag (/ (* G sun-mass earth-mass) r-squared))
                       (r-hat (vec-normalize r-vec))
                       (force (vec-scale r-hat f-mag))
                       (accel (vec-scale force (/ 1.0 earth-mass))))
                  ;; Update
                  (set! earth-vel (vec+ earth-vel (vec-scale accel dt)))
                  (set! earth-pos (vec+ earth-pos (vec-scale earth-vel dt))))))
          
          ;; Debug output every 60 frames
          (if (= (% frame-count 60) 0)
              (begin
                (display "Earth at: (")
                (display (floor (vec-x earth-pos)))
                (display ", ")
                (display (floor (vec-y earth-pos)))
                (display ")")
                (newline)))))
    
    (set! frame-count (+ frame-count 1))
    
    ;; Draw
    (begin-drawing)
      (clear-background 'black)
      
      ;; Draw orbit circle for reference (radius = 120)
      (draw-circle 400 300 120 '(50 50 50 255))  ; Dark gray circle
      
      ;; Draw Sun - BIG and YELLOW
      (draw-circle (floor (vec-x sun-pos)) 
                   (floor (vec-y sun-pos)) 
                   20
                   '(255 255 0 255))
      
      ;; Draw Earth - BLUE
      (draw-circle (floor (vec-x earth-pos)) 
                   (floor (vec-y earth-pos)) 
                   8
                   '(0 150 255 255))
      
      ;; Draw line from Sun to Earth
      (draw-line (floor (vec-x sun-pos))
                 (floor (vec-y sun-pos))
                 (floor (vec-x earth-pos))
                 (floor (vec-y earth-pos))
                 '(100 100 100 255))
      
      ;; UI
      (draw-text "SPACE: Pause | ESC: Exit" 10 10 15 'white)
      
      (if paused
          (draw-text "PAUSED" 10 30 20 '(255 255 0 255))
          (draw-text "Running" 10 30 15 'green))
      
      ;; Show distance
      (let ((dist (floor (sqrt (vec-mag-squared (vec- earth-pos sun-pos))))))
        (draw-text (string-append "Distance: " (number->string dist) " pixels")
                   10 (- height 45) 15 'green))
      
      ;; Show position
      (draw-text (string-append "Earth: (" 
                               (number->string (floor (vec-x earth-pos)))
                               ", "
                               (number->string (floor (vec-y earth-pos)))
                               ")")
                 10 (- height 25) 15 'green)
      
      (draw-text (string-append "FPS: " (number->string (get-fps)))
                 (- width 100) 10 15 'green)
    (end-drawing))
  
  (close-window)
  (display "Simulation ended")
  (newline))

(if (not (defined? 'string-append))
    (define (string-append . strings)
      (if (null? strings)
          ""
          (if (null? (cdr strings))
              (car strings)
              (string-concat (car strings)
                           (apply string-append (cdr strings)))))))

(display "Fixed simple orbit test loaded!")
(newline)
(display "Run: (simple-orbit-fixed)")
(newline)
(display "This version:")
(newline)
(display "  - No render texture (simpler)")
(newline)
(display "  - Shows reference orbit circle")
(newline)
(display "  - Prints Earth position every second")
(newline)
(display "  - Big visible planets")
(newline)
