;;; test-sqlite3.scm - Comprehensive SQLite3 binding tests

(require-so (so-ext "sql_sqlite3"))
(define (test-sqlite3)
  (puts "=== SQLite3 Binding Test Suite ===\n")
  
  ;; Test 1: Open/Close
  (puts "\n[TEST 1] Database open/close\n")
  (define db (sqlite3-open "test-comprehensive.db"))
  (puts "  Database opened successfully\n")
  
  ;; Test 2: Create table
  (puts "\n[TEST 2] CREATE TABLE\n")
  (sqlite3-exec db "DROP TABLE IF EXISTS people")
  (sqlite3-exec db "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT, age INTEGER, salary REAL)")
  (puts "  Table created successfully\n")
  
  ;; Test 3: INSERT with exec
  (puts "\n[TEST 3] INSERT using exec\n")
  (sqlite3-exec db "INSERT INTO people (name, age, salary) VALUES ('Alice', 30, 50000.50)")
  (sqlite3-exec db "INSERT INTO people (name, age, salary) VALUES ('Bob', 25, 45000.00)")
  (puts "  2 rows inserted\n")
  
  ;; Test 4: Prepared statement INSERT with bindings
  (puts "\n[TEST 4] INSERT using prepared statement with bindings\n")
  (define stmt-insert (sqlite3-prepare db "INSERT INTO people (name, age, salary) VALUES (?, ?, ?)"))
  (sqlite3-bind stmt-insert 1 "Charlie")
  (sqlite3-bind stmt-insert 2 35)
  (sqlite3-bind stmt-insert 3 60000.75)
  (sqlite3-step stmt-insert)
  (sqlite3-finalize stmt-insert)
  (puts "  1 row inserted via prepared statement\n")
  
  ;; Test 5: SELECT and iterate through results
  (puts "\n[TEST 5] SELECT and iterate through all rows\n")
  (define stmt-select (sqlite3-prepare db "SELECT id, name, age, salary FROM people"))
  (define status 'row)
  (while (eq? status 'row)
    (set! status (sqlite3-step stmt-select))
    (if (eq? status 'row)
        (begin
          (puts "  Row: ")
          (print (sqlite3-column stmt-select 0))
          (puts " | ")
          (print (sqlite3-column stmt-select 1))
          (puts " | ")
          (print (sqlite3-column stmt-select 2))
          (puts " | ")
          (print (sqlite3-column stmt-select 3))
          (puts "\n"))))
  (puts "  All rows retrieved\n")
  (sqlite3-finalize stmt-select)
  
  ;; Test 6: Column count
  (puts "\n[TEST 6] Column count\n")
  (define stmt-cols (sqlite3-prepare db "SELECT * FROM people"))
  (sqlite3-step stmt-cols)
  (puts "  Column count: ")
  (print (sqlite3-column-count stmt-cols))
  (puts "\n")
  (sqlite3-finalize stmt-cols)
  
  ;; Test 7: NULL values
  (puts "\n[TEST 7] NULL value handling\n")
  (define stmt-null (sqlite3-prepare db "INSERT INTO people (name, age, salary) VALUES (?, ?, ?)"))
  (sqlite3-bind stmt-null 1 "David")
  (sqlite3-bind stmt-null 2 ())
  (sqlite3-bind stmt-null 3 ())
  (sqlite3-step stmt-null)
  (sqlite3-finalize stmt-null)
  (puts "  Inserted row with NULL values\n")
  
  ;; Test 8: SELECT with WHERE clause using bindings
  (puts "\n[TEST 8] SELECT with WHERE clause and bindings\n")
  (define stmt-where (sqlite3-prepare db "SELECT name, age FROM people WHERE age > ?"))
  (sqlite3-bind stmt-where 1 28)
  (set! status 'row)
  (while (eq? status 'row)
    (set! status (sqlite3-step stmt-where))
    (if (eq? status 'row)
        (begin
          (puts "  ")
          (print (sqlite3-column stmt-where 0))
          (puts " is ")
          (print (sqlite3-column stmt-where 1))
          (puts " years old\n"))))
  (puts "  Query complete\n")
  (sqlite3-finalize stmt-where)
  
  ;; Test 9: Reset and reuse prepared statement
  (puts "\n[TEST 9] Reset and reuse prepared statement\n")
  (define stmt-reuse (sqlite3-prepare db "SELECT COUNT(*) FROM people WHERE age > ?"))
  (sqlite3-bind stmt-reuse 1 25)
  (sqlite3-step stmt-reuse)
  (puts "  Count (age > 25): ")
  (print (sqlite3-column stmt-reuse 0))
  (puts "\n")
  
  (sqlite3-reset stmt-reuse)
  (sqlite3-bind stmt-reuse 1 30)
  (sqlite3-step stmt-reuse)
  (puts "  Count (age > 30): ")
  (print (sqlite3-column stmt-reuse 0))
  (puts "\n")
  (sqlite3-finalize stmt-reuse)
  
  ;; Test 10: UPDATE
  (puts "\n[TEST 10] UPDATE\n")
  (sqlite3-exec db "UPDATE people SET salary = 55000 WHERE name = 'Bob'")
  (puts "  Updated Bob's salary\n")
  
  ;; Test 11: DELETE
  (puts "\n[TEST 11] DELETE\n")
  (sqlite3-exec db "DELETE FROM people WHERE age IS NULL")
  (puts "  Deleted rows with NULL age\n")
  
  ;; Test 13: Final data verification
  (puts "\n[TEST 13] Final data verification\n")
  (puts "  All remaining rows:\n")
  (define stmt-final (sqlite3-prepare db "SELECT * FROM people ORDER BY id"))
  (set! status 'row)
  (while (eq? status 'row)
    (set! status (sqlite3-step stmt-final))
    (if (eq? status 'row)
        (begin
          (puts "    ID=")
          (print (sqlite3-column stmt-final 0))
          (puts " Name=")
          (print (sqlite3-column stmt-final 1))
          (puts " Age=")
          (print (sqlite3-column stmt-final 2))
          (puts " Salary=")
          (print (sqlite3-column stmt-final 3))
          (puts "\n"))))
  (puts "  Verification complete\n")
  (sqlite3-finalize stmt-final)
  
  ;; Test 14: Close database
  (puts "\n[TEST 14] Close database\n")
  (sqlite3-close db)
  (puts "  Database closed successfully\n")
  
  ;; Test 15: Reopen and verify persistence
  (puts "\n[TEST 15] Verify data persistence\n")
  (define db2 (sqlite3-open "test-comprehensive.db"))
  (define stmt-verify (sqlite3-prepare db2 "SELECT COUNT(*) FROM people"))
  (sqlite3-step stmt-verify)
  (puts "  Row count after reopen: ")
  (print (sqlite3-column stmt-verify 0))
  (puts "\n")
  (sqlite3-finalize stmt-verify)
  (sqlite3-close db2)
  
  (puts "\n=== All tests complete ===\n"))

;; Run the tests
(test-sqlite3)
