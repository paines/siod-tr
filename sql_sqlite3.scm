;;; sqlite3-utils.scm - Higher-level SQLite3 utilities for SIOD

(define (sql-query db sql-string)
  "Execute a query and return all rows as a list of lists"
  (let ((stmt (sqlite3-prepare db sql-string))
        (results '()))
    (let loop ()
      (let ((status (sqlite3-step stmt)))
        (cond
         ((eq? status 'row)
          (let ((ncols (sqlite3-column-count stmt))
                (row '()))
            (do ((i 0 (+ i 1)))
                ((>= i ncols))
              (set! row (cons (sqlite3-column stmt i) row)))
            (set! results (cons (reverse row) results))
            (loop)))
         ((eq? status 'done)
          (sqlite3-finalize stmt)
          (reverse results))
         (else
          (sqlite3-finalize stmt)
          (error "SQL error" (sqlite3-errmsg db))))))
    results))

(define (sql-query-one db sql-string)
  "Execute a query and return the first row only"
  (let ((stmt (sqlite3-prepare db sql-string)))
    (let ((status (sqlite3-step stmt)))
      (if (eq? status 'row)
          (let ((ncols (sqlite3-column-count stmt))
                (row '()))
            (do ((i 0 (+ i 1)))
                ((>= i ncols))
              (set! row (cons (sqlite3-column stmt i) row)))
            (sqlite3-finalize stmt)
            (reverse row))
          (begin
            (sqlite3-finalize stmt)
            '())))))

(define (sql-for-each db sql-string proc)
  "Execute a query and call proc for each row"
  (let ((stmt (sqlite3-prepare db sql-string)))
    (let loop ()
      (let ((status (sqlite3-step stmt)))
        (cond
         ((eq? status 'row)
          (let ((ncols (sqlite3-column-count stmt))
                (row '()))
            (do ((i 0 (+ i 1)))
                ((>= i ncols))
              (set! row (cons (sqlite3-column stmt i) row)))
            (proc (reverse row))
            (loop)))
         ((eq? status 'done)
          (sqlite3-finalize stmt))
         (else
          (sqlite3-finalize stmt)
          (error "SQL error" (sqlite3-errmsg db))))))))
