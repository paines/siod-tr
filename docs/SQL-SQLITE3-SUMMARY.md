# SQLite3 Module Documentation and Utilities - Summary

## Overview

Created comprehensive documentation and high-level utilities for SIOD's SQLite3 bindings (`sql_sqlite3.so`).

## Files Created

### Documentation

**sql_sqlite3.md** - Complete module reference (12 sections)
- What SQLite3 is (and isn't)
- Loading the module
- Core API documentation
- Database operations (open, close)
- Simple SQL execution (sqlite3-exec)
- Prepared statements (recommended for SELECT)
- Error handling
- Complete examples (3 detailed examples)
- Common patterns
- SQLite data types
- Best practices & performance tips
- Limitations and future enhancements

### Utilities

**sql_sqlite3-utilities.scm** - High-level helper functions
- Query helpers: `sql:query`, `sql:query-one`, `sql:query-value`
- Simple execution: `sql:exec`
- Transaction support: `sql:begin`, `sql:commit`, `sql:rollback`
- Table introspection: `sql:table-exists?`, `sql:list-tables`, `sql:row-count`
- Connection management: `sql:open`, `sql:close`
- Utility functions: `sql:last-id`
- Pretty printing: `sql:print-table`
- Demo function: `sql-utilities-demo`

### Tests

**test-sql-utilities.scm** - Demonstration script
- Loads utilities
- Runs comprehensive demo

## Core SQLite3 API

### Database Operations

```scheme
(sqlite3-open filename)           ; Open/create database
(sqlite3-close db)                 ; Close database
```

### Simple Execution

```scheme
(sqlite3-exec db "CREATE TABLE ...") ; Execute non-SELECT SQL
```

### Prepared Statements (for SELECT)

```scheme
(sqlite3-prepare db "SELECT * FROM users WHERE id = ?")  ; Prepare
(sqlite3-bind stmt 1 value)                               ; Bind parameters (1-based!)
(sqlite3-step stmt)                                       ; Execute/advance ('row/'done/'error)
(sqlite3-column stmt index)                               ; Get column (0-based!)
(sqlite3-column-count stmt)                               ; Number of columns
(sqlite3-reset stmt)                                      ; Reset to reuse
(sqlite3-finalize stmt)                                   ; Destroy statement
```

### Error Handling

```scheme
(sqlite3-errcode db)    ; Get numeric error code
(sqlite3-errmsg db)     ; Get error message string
```

## Utility Functions

### Query Helpers (Return Lists)

```scheme
; Get all rows as list of lists
(sql:query db "SELECT * FROM users")
; => ((1 "Alice" 30) (2 "Bob" 25))

; Get first row
(sql:query-one db "SELECT * FROM users WHERE id=1")
; => (1 "Alice" 30)

; Get single value
(sql:query-value db "SELECT COUNT(*) FROM users")
; => 2
```

### Introspection

```scheme
(sql:table-exists? db "users")  ; => t or ()
(sql:list-tables db)             ; => ("users" "products")
(sql:row-count db "users")       ; => 42
```

### Transactions

```scheme
(sql:begin db)
(sql:exec db "INSERT ...")
(sql:exec db "UPDATE ...")
(sql:commit db)    ; or (sql:rollback db)
```

### Pretty Printing

```scheme
(sql:print-table (sql:query db "SELECT * FROM users"))
; Results:
;   1 "Alice" 30
;   2 "Bob" 25
; 2 rows.
```

## Demo Output

The demo successfully demonstrates:
1. ✅ Creating tables
2. ✅ Inserting data  
3. ✅ Querying all rows
4. ✅ Counting rows
5. ✅ Querying single row
6. ✅ Listing tables
7. ✅ Checking table existence
8. ✅ Getting row counts
9. ✅ Transactions
10. ✅ Last insert ID

```
=== SQLite3 Utilities Demo ===

[1] Creating table...
[2] Inserting data...

[3] Query all users:
Results:
  1 "Alice" 30
  2 "Bob" 25
  3 "Charlie" 35
3 rows.

[4] Count users:
Total: 3

[5] Get one user:
User: (1 "Alice" 30)

[6] List tables:
Tables: ("users")

[7] Check if table exists:
users exists? t
products exists? ()

[8] Row count:
users has 3 rows

[9] Transaction test...
After transaction: 5 rows

[10] Last insert ID:
Last ID: 6

=== Demo complete! ===
```

## Important Notes

### Index Mismatch!

⚠️ **CRITICAL**: Parameter binding and column retrieval use different indexing:

- **`sqlite3-bind`** uses **1-based** indexing (first parameter is 1)
- **`sqlite3-column`** uses **0-based** indexing (first column is 0)

This follows SQLite's C API directly.

```scheme
; Binding parameters (1-based)
(sqlite3-bind stmt 1 "Alice")   ; First parameter
(sqlite3-bind stmt 2 30)        ; Second parameter

; Reading columns (0-based)
(define name (sqlite3-column stmt 0))   ; First column
(define age (sqlite3-column stmt 1))    ; Second column
```

### Data Type Handling

- Numbers are stored as INTEGER or REAL
- Strings are stored as TEXT
- `nil` (empty list) represents NULL
- BLOB columns not yet supported

### In-Memory Databases

```scheme
(define db (sqlite3-open ":memory:"))
```

- Much faster (no disk I/O)
- Lost when connection closes
- Perfect for testing

## Usage Example

```scheme
(require-so (so-ext "sql_sqlite3"))
(load "sql_sqlite3-utilities.scm")

;; Open database
(define db (sql:open "mydata.db"))

;; Create table
(sql:exec db "CREATE TABLE users (
                id INTEGER PRIMARY KEY,
                name TEXT,
                age INTEGER)")

;; Insert data
(sql:exec db "INSERT INTO users (name, age) VALUES ('Alice', 30)")
(sql:exec db "INSERT INTO users (name, age) VALUES ('Bob', 25)")

;; Query and print
(sql:print-table (sql:query db "SELECT * FROM users"))

;; Get count
(puts "Total users: ")
(print (sql:query-value db "SELECT COUNT(*) FROM users"))
(puts "\n")

;; Close
(sql:close db)
```

## Best Practices

1. **Use prepared statements** for SELECT queries
2. **Use transactions** for multiple INSERTs/UPDATEs (100x faster!)
3. **Close connections** when done (or rely on GC)
4. **Use parameters** (?) to prevent SQL injection
5. **Check table existence** before operations
6. **Keep connections open** - opening is expensive

## Performance Tips

- Batch inserts in transactions
- Create indices on searched columns
- Use in-memory databases for temporary work
- Reuse prepared statements
- VACUUM periodically to reclaim space

## What's Available

### Low-Level (C bindings)
- sqlite3-open, sqlite3-close
- sqlite3-exec
- sqlite3-prepare, sqlite3-step, sqlite3-finalize
- sqlite3-bind, sqlite3-column
- sqlite3-reset, sqlite3-column-count
- sqlite3-errcode, sqlite3-errmsg

### High-Level (Scheme utilities)
- sql:query (returns all rows as lists)
- sql:query-one (returns first row)
- sql:query-value (returns single value)
- sql:exec (simple execution)
- sql:begin, sql:commit, sql:rollback (transactions)
- sql:table-exists?, sql:list-tables, sql:row-count (introspection)
- sql:open, sql:close (connection management)
- sql:last-id (get last insert ID)
- sql:print-table (pretty printing)

## Limitations

- BLOB columns not supported (use base64 TEXT instead)
- No callback support for sqlite3_exec results
- Utilities don't yet support parameterized queries (use raw API)
- Complex operations require using C API directly

## Future Enhancements

Potential additions:
- Parameter support in utility functions
- User-defined functions (sqlite3_create_function)
- Full-text search (FTS5)
- Backup API
- More introspection functions

## Success!

SQLite3 is now fully documented and wrapped with helpful utilities. You can use it for:
- Storing simulation results
- Configuration management
- Data analysis
- Quick prototyping
- Mathematical computing data storage

Perfect companion to your mathematical computing environment! 🗄️
