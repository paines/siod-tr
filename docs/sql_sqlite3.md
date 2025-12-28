# SQLite3 Database Bindings for SIOD

## Overview

The `sql_sqlite3` module provides SIOD bindings to SQLite3, a self-contained, serverless, zero-configuration SQL database engine. SQLite is ideal for embedded databases, local storage, testing, and applications that need a simple but powerful SQL database.

**SQLite Homepage:** https://www.sqlite.org/  
**Author:** DeeDeeCee (2025-12-23)  
**License:** Enjoy the pointlessness!

## What SQLite3 Is (and Isn't)

**SQLite3 is for:**
- Embedded databases in applications
- Local data storage and caching
- Configuration and state persistence
- Testing and prototyping
- Single-user databases
- Read-heavy workloads with occasional writes

**SQLite3 is NOT for:**
- High-concurrency multi-user systems (use PostgreSQL/MySQL)
- Network database servers (it's file-based)
- Very large datasets (>100GB, though it can handle terabytes)
- Applications requiring user authentication at the database level

## Loading the Module

```scheme
(require-so (so-ext "sql_sqlite3"))
```

## Core API

### Database Operations

#### `(sqlite3-open filename)`
Opens or creates a SQLite database file.

```scheme
(define db (sqlite3-open "test.db"))
```

**Parameters:**
- `filename` - Path to database file (created if doesn't exist)
  - Use `":memory:"` for in-memory database

**Returns:** Database handle object or error

**Example:**
```scheme
;; Open persistent database
(define db (sqlite3-open "data.db"))

;; Open in-memory database (fast, temporary)
(define memdb (sqlite3-open ":memory:"))
```

#### `(sqlite3-close db)`
Closes a database connection.

```scheme
(sqlite3-close db)
```

**Parameters:**
- `db` - Database handle

**Returns:** `nil`

**Note:** Database handles are automatically closed by garbage collector, but explicit closing is good practice.

### Simple SQL Execution

#### `(sqlite3-exec db sql-string)`
Executes SQL that doesn't return results (CREATE, INSERT, UPDATE, DELETE, etc.).

```scheme
(sqlite3-exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
(sqlite3-exec db "INSERT INTO users (name) VALUES ('Alice')")
```

**Parameters:**
- `db` - Database handle
- `sql-string` - SQL statement to execute

**Returns:** `nil` on success, or raises error

**Use for:**
- Table creation (CREATE TABLE)
- Schema modifications (ALTER TABLE, DROP TABLE)
- Data modifications (INSERT, UPDATE, DELETE)
- Index creation (CREATE INDEX)
- Transactions (BEGIN, COMMIT, ROLLBACK)

**Don't use for:**
- SELECT queries (use prepared statements instead)

### Prepared Statements (Recommended for SELECT)

Prepared statements are the proper way to execute SELECT queries and any SQL with parameters.

#### `(sqlite3-prepare db sql-string)`
Prepares an SQL statement for execution.

```scheme
(define stmt (sqlite3-prepare db "SELECT * FROM users WHERE id = ?"))
```

**Parameters:**
- `db` - Database handle
- `sql-string` - SQL with optional `?` placeholders

**Returns:** Statement handle object or error

**Note:** Use `?` for parameter placeholders (they're numbered starting from 1).

#### `(sqlite3-bind stmt index value)`
Binds a value to a parameter placeholder.

```scheme
(sqlite3-bind stmt 1 42)        ; Bind integer
(sqlite3-bind stmt 1 3.14)      ; Bind float
(sqlite3-bind stmt 1 "Alice")   ; Bind string
(sqlite3-bind stmt 1 nil)       ; Bind NULL
```

**Parameters:**
- `stmt` - Statement handle
- `index` - Parameter index (1-based)
- `value` - Value to bind (number, string, or nil for NULL)

**Returns:** `nil` on success, or error

#### `(sqlite3-step stmt)`
Executes the prepared statement and advances to next row.

```scheme
(define result (sqlite3-step stmt))
```

**Returns:**
- `'row` - A row is available, use `sqlite3-column` to retrieve data
- `'done` - No more rows
- `'error` - An error occurred

**Pattern:**
```scheme
(let loop ((result (sqlite3-step stmt)))
  (if (eq? result 'row)
      (begin
        ;; Process row with sqlite3-column
        (loop (sqlite3-step stmt)))))
```

#### `(sqlite3-column stmt index)`
Retrieves a column value from the current row.

```scheme
(define name (sqlite3-column stmt 0))  ; Get first column
(define age (sqlite3-column stmt 1))   ; Get second column
```

**Parameters:**
- `stmt` - Statement handle
- `index` - Column index (0-based, unlike bind!)

**Returns:**
- Number (for INTEGER or FLOAT columns)
- String (for TEXT columns)
- `nil` (for NULL columns)

**Note:** Column indices are **0-based** (unlike bind which is 1-based).

#### `(sqlite3-column-count stmt)`
Returns the number of columns in the result set.

```scheme
(define num-cols (sqlite3-column-count stmt))
```

**Returns:** Integer count of columns

#### `(sqlite3-reset stmt)`
Resets a prepared statement to be executed again.

```scheme
(sqlite3-reset stmt)
;; Can now bind new parameters and step again
```

**Use for:**
- Reusing the same prepared statement with different parameters
- More efficient than preparing a new statement each time

#### `(sqlite3-finalize stmt)`
Finalizes (destroys) a prepared statement.

```scheme
(sqlite3-finalize stmt)
```

**Note:** Statements are automatically finalized by garbage collector, but explicit finalization is good practice.

### Error Handling

#### `(sqlite3-errcode db)`
Returns the numeric error code from the last operation.

```scheme
(define code (sqlite3-errcode db))
```

**Returns:** Integer error code (0 = SQLITE_OK)

#### `(sqlite3-errmsg db)`
Returns the error message from the last operation.

```scheme
(define msg (sqlite3-errmsg db))
```

**Returns:** String error message

## Complete Examples

### Example 1: Basic Table Operations

```scheme
(require-so (so-ext "sql_sqlite3"))

;; Open database
(define db (sqlite3-open "example.db"))

;; Create table
(sqlite3-exec db "CREATE TABLE IF NOT EXISTS people (
                   id INTEGER PRIMARY KEY AUTOINCREMENT,
                   name TEXT NOT NULL,
                   age INTEGER,
                   email TEXT)")

;; Insert data
(sqlite3-exec db "INSERT INTO people (name, age, email) 
                   VALUES ('Alice', 30, 'alice@example.com')")
(sqlite3-exec db "INSERT INTO people (name, age) 
                   VALUES ('Bob', 25)")

;; Query with prepared statement
(define stmt (sqlite3-prepare db "SELECT * FROM people"))

(let loop ((result (sqlite3-step stmt)))
  (if (eq? result 'row)
      (begin
        (puts "ID: ") (print (sqlite3-column stmt 0))
        (puts " Name: ") (print (sqlite3-column stmt 1))
        (puts " Age: ") (print (sqlite3-column stmt 2))
        (puts "\n")
        (loop (sqlite3-step stmt)))))

(sqlite3-finalize stmt)
(sqlite3-close db)
```

### Example 2: Parameterized Queries

```scheme
(require-so (so-ext "sql_sqlite3"))

(define db (sqlite3-open ":memory:"))

;; Create table
(sqlite3-exec db "CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT, price REAL)")

;; Prepare INSERT statement
(define insert-stmt (sqlite3-prepare db "INSERT INTO products (name, price) VALUES (?, ?)"))

;; Insert multiple products
(define products '(("Widget" 9.99) ("Gadget" 19.99) ("Doohickey" 14.99)))

(define (insert-product product)
  (sqlite3-bind insert-stmt 1 (car product))
  (sqlite3-bind insert-stmt 2 (cadr product))
  (sqlite3-step insert-stmt)
  (sqlite3-reset insert-stmt))

(map insert-product products)
(sqlite3-finalize insert-stmt)

;; Query with parameter
(define select-stmt (sqlite3-prepare db "SELECT name, price FROM products WHERE price > ?"))
(sqlite3-bind select-stmt 1 15.00)

(let loop ((result (sqlite3-step select-stmt)))
  (if (eq? result 'row)
      (begin
        (puts "Product: ") (print (sqlite3-column select-stmt 0))
        (puts " Price: $") (print (sqlite3-column select-stmt 1))
        (puts "\n")
        (loop (sqlite3-step select-stmt)))))

(sqlite3-finalize select-stmt)
(sqlite3-close db)
```

### Example 3: Transactions

```scheme
(require-so (so-ext "sql_sqlite3"))

(define db (sqlite3-open "banking.db"))

(sqlite3-exec db "CREATE TABLE IF NOT EXISTS accounts (
                   id INTEGER PRIMARY KEY,
                   name TEXT,
                   balance REAL)")

;; Transfer money (must be atomic)
(define (transfer-money from-id to-id amount)
  (sqlite3-exec db "BEGIN TRANSACTION")
  
  ;; Deduct from sender
  (define deduct (sqlite3-prepare db "UPDATE accounts SET balance = balance - ? WHERE id = ?"))
  (sqlite3-bind deduct 1 amount)
  (sqlite3-bind deduct 2 from-id)
  (sqlite3-step deduct)
  (sqlite3-finalize deduct)
  
  ;; Add to receiver
  (define add (sqlite3-prepare db "UPDATE accounts SET balance = balance + ? WHERE id = ?"))
  (sqlite3-bind add 1 amount)
  (sqlite3-bind add 2 to-id)
  (sqlite3-step add)
  (sqlite3-finalize add)
  
  (sqlite3-exec db "COMMIT"))

;; Use it
(sqlite3-exec db "INSERT INTO accounts VALUES (1, 'Alice', 1000.0)")
(sqlite3-exec db "INSERT INTO accounts VALUES (2, 'Bob', 500.0)")
(transfer-money 1 2 100.0)

(sqlite3-close db)
```

## Common Patterns

### Pattern 1: Query All Rows

```scheme
(define (query-all-rows db sql)
  (define stmt (sqlite3-prepare db sql))
  (define results '())
  
  (let loop ((result (sqlite3-step stmt)))
    (if (eq? result 'row)
        (let ((row (let collect ((i 0) (cols '()))
                     (if (< i (sqlite3-column-count stmt))
                         (collect (+ i 1) (cons (sqlite3-column stmt i) cols))
                         (reverse cols)))))
          (set! results (cons row results))
          (loop (sqlite3-step stmt)))))
  
  (sqlite3-finalize stmt)
  (reverse results))
```

### Pattern 2: Check if Table Exists

```scheme
(define (table-exists? db table-name)
  (define stmt (sqlite3-prepare db 
    "SELECT name FROM sqlite_master WHERE type='table' AND name=?"))
  (sqlite3-bind stmt 1 table-name)
  (define result (eq? 'row (sqlite3-step stmt)))
  (sqlite3-finalize stmt)
  result)
```

### Pattern 3: Get Row Count

```scheme
(define (count-rows db table-name)
  (define stmt (sqlite3-prepare db (string-append "SELECT COUNT(*) FROM " table-name)))
  (sqlite3-step stmt)
  (define count (sqlite3-column stmt 0))
  (sqlite3-finalize stmt)
  count)
```

## SQLite3 Data Types

SQLite uses dynamic typing - columns can store any type of data:

| SQLite Type | SIOD Type | Example |
|-------------|-----------|---------|
| INTEGER | Number | `42` |
| REAL | Number | `3.14` |
| TEXT | String | `"Hello"` |
| NULL | nil | `nil` |
| BLOB | Not supported | - |

## Important Notes

### Parameter Binding Index Mismatch!
- `sqlite3-bind` uses **1-based** indexing (first parameter is 1)
- `sqlite3-column` uses **0-based** indexing (first column is 0)

This is how SQLite's C API works - we're just exposing it directly.

### NULL Handling
- Bind `nil` to insert NULL: `(sqlite3-bind stmt 1 nil)`
- NULL columns return `nil`: `(sqlite3-column stmt 0)` → `nil`

### In-Memory Databases
- Use `":memory:"` as filename for RAM-only database
- Much faster (no disk I/O)
- Lost when connection closes
- Great for testing

### Automatic Type Conversion
- Numbers that are whole values are stored as INTEGER
- Numbers with fractional parts are stored as REAL
- Everything else is TEXT

## Best Practices

1. **Use prepared statements** for SELECT queries
2. **Use transactions** for multiple INSERTs/UPDATEs
3. **Close statements** when done (or let GC handle it)
4. **Check error codes** after operations
5. **Use parameters** (?) instead of string concatenation (prevents SQL injection)
6. **Keep connections open** - opening is expensive

## Performance Tips

1. **Batch inserts** in transactions (100x faster)
2. **Create indices** on columns you search/join on
3. **Use prepared statements** and reuse them
4. **Use in-memory databases** for temporary work
5. **VACUUM** periodically to reclaim space

## See Also

- SQLite documentation: https://www.sqlite.org/docs.html
- `sql_sqlite3-utilities.scm` - High-level helper functions
- `test-sqlite3.scm` - Comprehensive test suite

## Limitations

- BLOB columns are not yet supported (use base64 TEXT instead)
- No callback support for sqlite3_exec results
- Error handling could be more detailed

## Future Enhancements

Potential additions:
- BLOB support
- User-defined functions (via sqlite3_create_function)
- Collation sequences
- Virtual tables
- Full-text search (FTS5)
- Backup API
