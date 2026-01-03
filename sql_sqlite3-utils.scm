;;; sql_sqlite3-utilities.scm - Simplified high-level utilities for SQLite3
;;;
;;; Provides convenient wrapper functions for common SQLite3 operations

(require-so (so-ext "sql_sqlite3"))

;;; ============================================================================
;;; Query Helpers - Return Results as Lists
;;; ============================================================================

(define (sql:query db sql)
  "Execute a query and return all rows as a list of lists.
   Example: (sql:query db \"SELECT * FROM users\")"
  (define stmt (sqlite3-prepare db sql))
  (define results '())
  (define status 'row)
  (while (eq? status 'row)
    (set! status (sqlite3-step stmt))
    (if (eq? status 'row)
        (let ((row '()))
          (define i 0)
          (while (< i (sqlite3-column-count stmt))
            (set! row (cons (sqlite3-column stmt i) row))
            (set! i (+ i 1)))
          (set! results (cons (reverse row) results)))))
  (sqlite3-finalize stmt)
  (reverse results))

(define (sql:query-one db sql)
  "Execute a query and return the first row or () if no results.
   Example: (sql:query-one db \"SELECT * FROM users LIMIT 1\")"
  (define stmt (sqlite3-prepare db sql))
  (define result-row '())
  (if (eq? 'row (sqlite3-step stmt))
      (begin
        (define i 0)
        (while (< i (sqlite3-column-count stmt))
          (set! result-row (cons (sqlite3-column stmt i) result-row))
          (set! i (+ i 1)))
        (set! result-row (reverse result-row))))
  (sqlite3-finalize stmt)
  result-row)

(define (sql:query-value db sql)
  "Execute a query and return a single value (first column of first row).
   Example: (sql:query-value db \"SELECT COUNT(*) FROM users\")"
  (define row (sql:query-one db sql))
  (if (null? row) () (car row)))

;;; ============================================================================
;;; Simple Execution
;;; ============================================================================

(define (sql:exec db sql)
  "Execute SQL without returning results.
   Example: (sql:exec db \"INSERT INTO users (name) VALUES ('Alice')\")"
  (sqlite3-exec db sql))

;;; ============================================================================
;;; Transaction Support
;;; ============================================================================

(define (sql:begin db)
  "Begin a transaction."
  (sqlite3-exec db "BEGIN TRANSACTION"))

(define (sql:commit db)
  "Commit the current transaction."
  (sqlite3-exec db "COMMIT"))

(define (sql:rollback db)
  "Roll back the current transaction."
  (sqlite3-exec db "ROLLBACK"))

;;; ============================================================================
;;; Table Introspection
;;; ============================================================================

(define (sql:table-exists? db table-name)
  "Check if a table exists.
   Example: (sql:table-exists? db \"users\")"
  (define stmt (sqlite3-prepare db 
    "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name=?"))
  (sqlite3-bind stmt 1 table-name)
  (sqlite3-step stmt)
  (define count (sqlite3-column stmt 0))
  (sqlite3-finalize stmt)
  (> count 0))

(define (sql:list-tables db)
  "Return a list of all table names."
  (define stmt (sqlite3-prepare db 
    "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"))
  (define tables '())
  (define status 'row)
  (while (eq? status 'row)
    (set! status (sqlite3-step stmt))
    (if (eq? status 'row)
        (set! tables (cons (sqlite3-column stmt 0) tables))))
  (sqlite3-finalize stmt)
  (reverse tables))

(define (sql:row-count db table-name)
  "Return the number of rows in a table.
   Example: (sql:row-count db \"users\")"
  (sql:query-value db (string-append "SELECT COUNT(*) FROM " table-name)))

;;; ============================================================================
;;; Connection Management
;;; ============================================================================

(define (sql:open filename)
  "Open a database connection.
   Example: (define db (sql:open \"test.db\"))"
  (sqlite3-open filename))

(define (sql:close db)
  "Close a database connection."
  (sqlite3-close db))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (sql:last-id db)
  "Get the rowid of the last INSERT."
  (sql:query-value db "SELECT last_insert_rowid()"))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(define (sql:print-table rows)
  "Pretty-print query results.
   Example: (sql:print-table (sql:query db \"SELECT * FROM users\"))"
  (if (null? rows)
      (puts "No results.\n")
      (begin
        (puts "Results:\n")
        (define row-num 0)
        (while (< row-num (length rows))
          (define row (nth row-num rows))
          (puts "  ")
          (define col-num 0)
          (while (< col-num (length row))
            (print (nth col-num row))
            (puts " ")
            (set! col-num (+ col-num 1)))
          (puts "\n")
          (set! row-num (+ row-num 1)))
        (puts (number->string (length rows)))
        (puts " rows.\n"))))

;;; ============================================================================
;;; Example Demonstration
;;; ============================================================================

(define (sql-utilities-demo)
  "Demonstration of SQLite3 utilities"
  (puts "=== SQLite3 Utilities Demo ===\n\n")
  
  (define db (sql:open ":memory:"))
  
  ;; Create table
  (puts "[1] Creating table...\n")
  (sql:exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)")
  
  ;; Insert data
  (puts "[2] Inserting data...\n")
  (sql:exec db "INSERT INTO users (name, age) VALUES ('Alice', 30)")
  (sql:exec db "INSERT INTO users (name, age) VALUES ('Bob', 25)")
  (sql:exec db "INSERT INTO users (name, age) VALUES ('Charlie', 35)")
  
  ;; Query all
  (puts "\n[3] Query all users:\n")
  (sql:print-table (sql:query db "SELECT * FROM users"))
  
  ;; Count
  (puts "\n[4] Count users:\n")
  (puts "Total: ")
  (print (sql:query-value db "SELECT COUNT(*) FROM users"))
  (puts "\n")
  
  ;; Query one
  (puts "\n[5] Get one user:\n")
  (define user (sql:query-one db "SELECT * FROM users WHERE name='Alice'"))
  (puts "User: ")
  (print user)
  (puts "\n")
  
  ;; List tables
  (puts "\n[6] List tables:\n")
  (puts "Tables: ")
  (print (sql:list-tables db))
  (puts "\n")
  
  ;; Table exists
  (puts "\n[7] Check if table exists:\n")
  (puts "users exists? ")
  (print (sql:table-exists? db "users"))
  (puts "\n")
  (puts "products exists? ")
  (print (sql:table-exists? db "products"))
  (puts "\n")
  
  ;; Row count
  (puts "\n[8] Row count:\n")
  (puts "users has ")
  (print (sql:row-count db "users"))
  (puts " rows\n")
  
  ;; Transaction
  (puts "\n[9] Transaction test...\n")
  (sql:begin db)
  (sql:exec db "INSERT INTO users (name, age) VALUES ('Diana', 28)")
  (sql:exec db "INSERT INTO users (name, age) VALUES ('Eve', 32)")
  (sql:commit db)
  (puts "After transaction: ")
  (print (sql:row-count db "users"))
  (puts " rows\n")
  
  ;; Last insert ID
  (puts "\n[10] Last insert ID:\n")
  (sql:exec db "INSERT INTO users (name, age) VALUES ('Frank', 40)")
  (puts "Last ID: ")
  (print (sql:last-id db))
  (puts "\n")
  
  (sql:close db)
  (puts "\n=== Demo complete! ===\n"))

(puts "SQLite3 utilities loaded. Try (sql-utilities-demo) for a demonstration.\n")
