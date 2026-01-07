;;; symengine-utils.scm
;;; Utility functions for working with SymEngine in SIOD-TR
;;;
;;; Load after symengine.so: (load "symengine-utils.scm")

;;; ============================================
;;; Display Helpers
;;; ============================================

;;; Display expression with label
;;; Handles both quoted expressions and string results from symbolic ops
(define (sym-show label expr)
  (display label)
  (display ": ")
  (if (pair? expr)
      ;; It's a quoted expression like '(+ x x) - convert it
      (begin
        (display (sym expr))
        (newline))
      ;; It's already a string result - just display it
      (begin
        (display expr)
        (newline))))

;;; Pretty-print symbolic expression or string result  
(define (sym-display expr)
  (if (pair? expr)
      (display (sym expr))
      (display expr))
  (newline))

;;; ============================================
;;; Common Mathematical Operations
;;; ============================================

;;; Square root
(define (sym-sqrt expr)
  (sym '(sqrt ,expr)))

;;; Absolute value
(define (sym-abs expr)
  (sym '(abs ,expr)))

;;; Sine
(define (sym-sin expr)
  (sym '(sin ,expr)))

;;; Cosine
(define (sym-cos expr)
  (sym '(cos ,expr)))

;;; Tangent
(define (sym-tan expr)
  (sym '(tan ,expr)))

;;; Exponential
(define (sym-exp expr)
  (sym '(exp ,expr)))

;;; Natural logarithm
(define (sym-log expr)
  (sym '(log ,expr)))

;;; ============================================
;;; Higher-Order Derivatives
;;; ============================================

;;; NOTE: These don't work in Phase 1 because results are strings
;;; Phase 2 will add proper symbolic object types that can be chained

;;; Compute nth derivative (PHASE 2)
(define (sym-diff-n expr var n)
  (display "sym-diff-n requires Phase 2 (symbolic objects)")
  (newline)
  (display "For now, call sym-diff manually n times")
  (newline))

;;; Second derivative (PHASE 2)
(define (sym-diff-2 expr var)
  (display "sym-diff-2 requires Phase 2")
  (newline))

;;; Third derivative (PHASE 2)
(define (sym-diff-3 expr var)
  (display "sym-diff-3 requires Phase 2")
  (newline))

;;; ============================================
;;; Expression Analysis
;;; ============================================

;;; Check if expression contains a variable
(define (sym-has-var? expr var)
  ; Try differentiating; if result is "0", variable not present
  (not (string=? (sym-diff expr var) "0")))

;;; ============================================
;;; Common Expression Patterns
;;; ============================================

;;; Polynomial: a*x^n + b*x^(n-1) + ... + c
(define (sym-polynomial coeffs var)
  (define (build-term coef power)
    (if (= power 0)
        coef
        `(* ,coef (^ ,var ,power))))
  
  (let ((n (length coeffs)))
    (let loop ((cs coeffs) (power (- n 1)) (result 0))
      (if (null? cs)
          result
          (loop (cdr cs)
                (- power 1)
                `(+ ,(build-term (car cs) power) ,result))))))

;;; Quadratic: ax² + bx + c
(define (sym-quadratic a b c var)
  `(+ (* ,a (^ ,var 2)) (* ,b ,var) ,c))

;;; Linear: mx + b
(define (sym-linear m b var)
  `(+ (* ,m ,var) ,b))

;;; ============================================
;;; Calculus Helpers
;;; ============================================

;;; Find critical points (where derivative = 0)
;;; Returns the derivative (user must solve manually for now)
(define (sym-critical-points expr var)
  (sym-diff expr var))

;;; Tangent line at point x0: f(x0) + f'(x0)*(x - x0)
(define (sym-tangent-line expr var x0)
  (let ((df (sym-diff expr var)))
    `(+ (,expr (,var . ,x0))
        (* ,df (- ,var ,x0)))))

;;; ============================================
;;; Expression Transformations
;;; ============================================

;;; Compose two expressions: f(g(x))
(define (sym-compose f g var)
  ; This is tricky without proper substitution
  ; Placeholder for now
  `(,f (,g ,var)))

;;; ============================================
;;; Display Utilities
;;; ============================================

;;; Display derivative nicely
(define (show-derivative expr var)
  (display "f(")
  (display var)
  (display ") = ")
  (if (pair? expr)
      (display (sym expr))
      (display expr))
  (newline)
  (display "f'(")
  (display var)
  (display ") = ")
  (display (sym-diff expr var))
  (newline))

;;; Display first n derivatives  
;;; Note: In Phase 1, we can't chain symbolic operations
;;; because results are strings, not symbolic expressions
;;; This will be improved in Phase 2
(define (show-derivatives expr var n)
  (display "Note: show-derivatives requires Phase 2 (symbolic object types)")
  (newline)
  (display "For now, use sym-diff repeatedly:")
  (newline)
  (display "  (define f '")
  (display expr)
  (display ")")
  (newline)
  (display "  (define df (sym-diff f '")
  (display var)
  (display "))")
  (newline)
  (display "  (display df)")
  (newline))

;;; ============================================
;;; Common Mathematical Expressions
;;; ============================================

;;; Gaussian function
(define (sym-gaussian mean stddev var)
  `(* (/ 1 (* ,stddev (sqrt (* 2 pi))))
      (exp (* -0.5 (^ (/ (- ,var ,mean) ,stddev) 2)))))

;;; Sigmoid function
(define (sym-sigmoid var)
  `(/ 1 (+ 1 (exp (* -1 ,var)))))

;;; Logistic growth
(define (sym-logistic K r t)
  `(/ ,K (+ 1 (* (- (/ ,K P0) 1) (exp (* -1 ,r ,t))))))

