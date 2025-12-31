;;; Solar System with CORRECT Physics
;;; Properly calculated orbital velocities: v = sqrt(G * M / r)

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

;;; Calculate correct orbital velocity for distance r
;;; v = sqrt(G * M / r)
(define (orbital-velocity G M r)
  (sqrt (/ (* G M) r)))

;;; Create system with CORRECT velocities
(define (create-solar-system-correct)
  (define G 1.0)
  (define sun-mass 1000)
  
  ;; Helper: make planet with correct orbital velocity
  (define (make-orbiting-planet name radius-from-sun planet-mass display-radius color)
    (let* ((x (+ 400 radius-from-sun))  ; Position to RIGHT of sun
           (y 300)
           (r radius-from-sun)
           (v (orbital-velocity G sun-mass r))  ; CORRECT velocity!
           (vx 0)
           (vy (- 0 v)))  ; UP (negative Y) for counter-clockwise orbit
      (list (cons 'name name)
            (cons 'pos (vec x y))
            (cons 'vel (vec vx vy))
            (cons 'mass planet-mass)
            (cons 'radius display-radius)
            (cons 'color color))))
  
  (list
    ;; Sun at center (stationary)
    (list (cons 'name "Sun")
          (cons 'pos (vec 400 300))
          (cons 'vel (vec 0 0))
          (cons 'mass sun-mass)
          (cons 'radius 15)
          (cons 'color '(255 255 0 255)))
    
    ;; Planets with CORRECT velocities
    ;; (name, distance, mass, display-size, color)
    (make-orbiting-planet "Mercury" 50  0.3  3  '(180 180 180 255))
    (make-orbiting-planet "Venus"   80  0.8  4  '(255 200 100 255))
    (make-orbiting-planet "Earth"   120 1.0  5  '(0 100 255 255))
    (make-orbiting-planet "Mars"    170 0.5  4  '(255 100 50 255))
    (make-orbiting-planet "Jupiter" 250 15   10 '(255 200 150 255))
    (make-orbiting-planet "Saturn"  320 12   9  '(255 220 180 255))))

;;; Planet accessors
(define (planet-get planet key) (cdr (assoc key planet)))
(define (planet-set! planet key value) (set-cdr! (assoc key planet) value))
(define (planet-pos p) (planet-get p 'pos))
(define (planet-vel p) (planet-get p 'vel))
(define (planet-mass p) (planet-get p 'mass))
(define (planet-radius p) (planet-get p 'radius))
(define (planet-color p) (planet-get p 'color))
(define (planet-set-pos! p pos) (planet-set! p 'pos pos))
(define (planet-set-vel! p vel) (planet-set! p 'vel vel))

;;; Physics
(define G 1.0)

(define (gravity-force p1 p2)
  (let* ((r-vec (vec- (planet-pos p2) (planet-pos p1)))
         (r-squared (vec-mag-squared r-vec))
         (r-mag (sqrt r-squared)))
    (if (< r-mag 1.0)
        (vec 0 0)
        (let* ((f-mag (/ (* G (planet-mass p1) (planet-mass p2)) r-squared))
               (r-hat (vec-normalize r-vec)))
          (vec-scale r-hat f-mag)))))

(define (total-force-on planet all-planets)
  (define (sum-forces remaining acc)
    (if (null? remaining)
        acc
        (let ((other (car remaining)))
          (if (eq? planet other)
              (sum-forces (cdr remaining) acc)
              (sum-forces (cdr remaining)
                         (vec+ acc (gravity-force planet other)))))))
  (sum-forces all-planets (vec 0 0)))

(define (update-planet! planet all-planets dt)
  (let* ((force (total-force-on planet all-planets))
         (accel (vec-scale force (/ 1.0 (planet-mass planet))))
         (old-vel (planet-vel planet))
         (new-vel (vec+ old-vel (vec-scale accel dt)))
         (old-pos (planet-pos planet))
         (new-pos (vec+ old-pos (vec-scale new-vel dt))))
    (planet-set-vel! planet new-vel)
    (planet-set-pos! planet new-pos)))

(define (update-physics! planets dt)
  (define (update-each remaining)
    (if (not (null? remaining))
        (begin
          (update-planet! (car remaining) planets dt)
          (update-each (cdr remaining)))))
  (update-each planets))

;;; Main
(define (solar-system)
  (define width 800)
  (define height 600)
  (define dt 0.016)
  (define paused #f)
  (define frame 0)
  
  (init-window width height "Solar System")
  (set-target-fps 60)
  
  ;; No texture for now - just see if orbits are circular
  
  (define planets (create-solar-system-correct))
  
  (display "Solar System")
  (newline)
  (display "Velocities calculated as v = sqrt(G * M / r)")
  (newline)
  (display "Orbits should be perfectly circular now!")
  (newline)
  
  (while (not (window-should-close?))
    
    (if (key-pressed? KEY_SPACE)
        (set! paused (not paused)))
    
    (if (not paused)
        (update-physics! planets dt))
    
    (begin-drawing)
      (clear-background 'black)
      
      ;; Reference circles
      (draw-circle 400 300 50 '(20 20 20 255))
      (draw-circle 400 300 80 '(20 20 20 255))
      (draw-circle 400 300 120 '(20 20 20 255))
      (draw-circle 400 300 170 '(20 20 20 255))
      (draw-circle 400 300 250 '(20 20 20 255))
      (draw-circle 400 300 320 '(20 20 20 255))
      
      ;; Draw planets
      (let ((p-list planets))
        (while (not (null? p-list))
          (let* ((p (car p-list))
                 (pos (planet-pos p))
                 (x (floor (vec-x pos)))
                 (y (floor (vec-y pos)))
                 (r (floor (planet-radius p)))
                 (c (planet-color p)))
            (draw-circle x y r c))
          (set! p-list (cdr p-list))))
      
      (draw-text "SPACE: Pause | Correct orbital velocities!" 10 10 15 'white)
      (if paused
          (draw-text "PAUSED" 10 30 20 '(255 255 0 255)))
      (draw-text (string-append "FPS: " (number->string (get-fps)))
                 (- width 100) 10 15 'green)
    (end-drawing)
    
    (set! frame (+ frame 1)))
  
  (close-window))

(if (not (defined? 'string-append))
    (define (string-append . strings)
      (if (null? strings)
          ""
          (if (null? (cdr strings))
              (car strings)
              (string-concat (car strings)
                           (apply string-append (cdr strings)))))))

(display "Solar System with correct physics loaded!")
(newline)
(display "Run: (solar-system)")
(newline)
(display "Now uses v = sqrt(G * M / r) for each planet.")
(newline)
(display "Orbits should be perfectly circular!")
(newline)
