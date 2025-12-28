# SS Module Documentation - Socket Programming in SIOD

## Overview

The `ss` module provides TCP/IP socket programming capabilities for SIOD. It supports both client and server socket operations.

**Module Version:** `$Id: ss.c,v 1.7 1998/03/05 13:26:47 gjc Exp $`

## Loading the Module

```scheme
(require-so (so-ext "ss"))
```

## API Reference

### Socket Creation

#### `(s-open host port server-flag)`
Creates a socket connection.

**Parameters:**
- `host` (string): Hostname or IP address (e.g., "127.0.0.1", "localhost")
- `port` (number): Port number (1-65535)
- `server-flag` (number or list): 
  - `1` or non-() = Server mode (listen/accept)
  - `()` = Client mode (connect)

**Returns:** Socket object `#{SOCKET ...}`

**Examples:**
```scheme
;; Create server socket
(define server (s-open "127.0.0.1" 8080 1))

;; Create client socket
(define client (s-open "127.0.0.1" 8080 ()))
```

### Connection Management

#### `(s-accept server-socket)`
Accepts an incoming connection on a server socket.

**Parameters:**
- `server-socket`: Socket created with server-flag = 1

**Returns:** New socket object for the accepted connection

**Example:**
```scheme
(define server (s-open "127.0.0.1" 8080 1))
(define client-conn (s-accept server))
```

**Note:** This call blocks until a client connects.

#### `(s-close socket)`
Closes a socket connection.

**Parameters:**
- `socket`: Socket object to close

**Returns:** Unspecified

**Example:**
```scheme
(s-close client)
(s-close server)
```

### Data Transfer

#### `(s-write data socket)`
Writes data to a socket.

**CRITICAL:** Arguments are in this order: **data first, socket second**

**Parameters:**
- `data` (string): Data to send
- `socket`: Socket to write to

**Returns:** Number of bytes written

**Example:**
```scheme
(s-write "Hello, World!" client-socket)
```

#### `(s-read size socket)`
Reads data from a socket.

**CRITICAL:** Arguments are in this order: **size first, socket second**

**Parameters:**
- `size` (number): Maximum number of bytes to read
- `socket`: Socket to read from

**Returns:** String containing received data (may be less than size bytes)

**Example:**
```scheme
(define data (s-read 1024 client-socket))
```

#### `(s-force-output socket)`
Flushes output buffer, ensuring data is sent immediately.

**Parameters:**
- `socket`: Socket to flush

**Returns:** Unspecified

**Example:**
```scheme
(s-write "Message" socket)
(s-force-output socket)  ; Ensure it's sent now
```

### Information Functions

#### `(gethostname)`
Gets the local hostname.

**Returns:** String containing hostname

**Example:**
```scheme
(define hostname (gethostname))
; => "mycomputer.local"
```

#### `(gethostid)`
Gets the local host ID.

**Returns:** Number representing host ID

**Example:**
```scheme
(define hostid (gethostid))
; => 8323328 (0x7F0100)
```

#### `(getsockname socket)`
Gets local address and port of a socket.

**Parameters:**
- `socket`: Socket to query

**Returns:** Address information (format varies)

**Example:**
```scheme
(define sockinfo (getsockname server))
```

#### `(getpeername socket)`
Gets remote address and port of a connected socket.

**Parameters:**
- `socket`: Socket to query

**Returns:** Address information (format varies)

**Example:**
```scheme
(define peer (getpeername client))
```

### Lookup Functions

#### `(gethostbyname hostname)`
Resolves a hostname to IP address.

**Parameters:**
- `hostname` (string): Hostname to resolve

**Returns:** Host information

**Example:**
```scheme
(define host-info (gethostbyname "localhost"))
```

#### `(getservice port protocol)`
Looks up service information.

**Parameters:**
- `port` (number): Port number
- `protocol` (string): Protocol name (e.g., "tcp", "udp")

**Returns:** Service information or () if not found

**Example:**
```scheme
(define http (getservice 80 "tcp"))
```

#### `(getproto number)`
Looks up protocol information by number.

**Parameters:**
- `number`: Protocol number (e.g., 6 for TCP, 17 for UDP)

**Returns:** Protocol information or () if not found

**Example:**
```scheme
(define tcp-info (getproto 6))
```

## Common Patterns

### Simple Server

```scheme
(require-so (so-ext "ss"))

(define (echo-server port)
  ; Create server socket
  (define server (s-open "127.0.0.1" port 1))
  (puts "Server listening on port ")
  (print port)
  (puts "\n")
  
  ; Accept connection
  (define client (s-accept server))
  (puts "Client connected\n")
  
  ; Echo loop
  (define (echo-loop)
    (define data (s-read 1024 client))
    (if (> (string-length data) 0)
        (begin
          (s-write data client)
          (s-force-output client)
          (echo-loop))))
  
  (echo-loop)
  
  ; Cleanup
  (s-close client)
  (s-close server))

; Run server
(echo-server 8080)
```

### Simple Client

```scheme
(require-so (so-ext "ss"))

(define (simple-client host port message)
  ; Connect to server
  (define client (s-open host port ()))
  (puts "Connected to server\n")
  
  ; Send message
  (s-write message client)
  (s-force-output client)
  (puts "Sent: ")
  (print message)
  (puts "\n")
  
  ; Receive response
  (sleep 0.1)  ; Give server time to respond
  (define response (s-read 1024 client))
  (puts "Received: ")
  (print response)
  (puts "\n")
  
  ; Cleanup
  (s-close client))

; Use client
(simple-client "127.0.0.1" 8080 "Hello, Server!")
```

### HTTP GET Request

```scheme
(define (http-get host path)
  ; Connect to HTTP server
  (define socket (s-open host 80 ()))
  
  ; Build HTTP request
  (define request 
    (string-append "GET " path " HTTP/1.0\r\n"
                   "Host: " host "\r\n"
                   "\r\n"))
  
  ; Send request
  (s-write request socket)
  (s-force-output socket)
  
  ; Read response
  (define response "")
  (define chunk "x")
  (while (> (string-length chunk) 0)
    (set! chunk (s-read 1024 socket))
    (set! response (string-append response chunk)))
  
  ; Cleanup
  (s-close socket)
  
  response)

; Use it
(define page (http-get "example.com" "/"))
```

## Important Notes

### Argument Order

**CRITICAL:** The socket functions have non-intuitive argument orders:

```scheme
; CORRECT
(s-write "data" socket)    ; data first, socket second
(s-read 1024 socket)       ; size first, socket second

; WRONG - will cause errors
(s-write socket "data")    ; WRONG
(s-read socket 1024)       ; WRONG
```

### Server vs Client Flag

The third argument to `s-open` determines mode:

```scheme
(s-open host port 1)    ; Server: binds and listens
(s-open host port ())   ; Client: connects
```

**Any non-() value is treated as server mode**, so:
- `1` = server ✓
- `'t` = server ✓ 
- `()` = client ✓
- `0` = server (not client!) ⚠️

### Blocking Operations

These operations block until complete:
- `s-accept` - Blocks until client connects
- `s-read` - May block waiting for data

### Error Handling

Socket operations can fail. Errors will cause SIOD to exit unless wrapped in error handling.

### Timing Issues

When testing locally, add small delays to ensure data is transmitted:

```scheme
(s-write "data" socket)
(s-force-output socket)
(sleep 0.1)  ; Small delay
(define received (s-read 1024 other-socket))
```

## Testing

Run the test suite:

```bash
LD_LIBRARY_PATH=. ./siod test-ss.scm
```

## Platform Support

- ✅ Linux (tested)
- ✅ macOS (tested - with socklen_t fixes)
- ✅ Other Unix-like systems (should work)

## Socket Object Format

Socket objects print as:
```
#{SOCKET fd local-addr:local-port [remote-addr:remote-port]}
```

Examples:
```scheme
#{SOCKET 4 127.0.0.1:19753}                    ; Server socket
#{SOCKET 5 127.0.0.1:39315 127.0.0.1:19753}    ; Client socket
#{SOCKET 6 127.0.0.1:19753 127.0.0.1:39315}    ; Accepted connection
```

## Limitations

1. **No UDP support** - TCP only
2. **No non-blocking I/O** - All operations block
3. **No select/poll** - Cannot wait on multiple sockets
4. **No IPv6** - IPv4 only
5. **No SSL/TLS** - Plain text only
6. **No timeout** - Operations block indefinitely

## Future Enhancements

Possible improvements:
- UDP socket support
- Non-blocking mode
- Socket options (SO_REUSEADDR, etc.)
- IPv6 support
- Better error reporting

## See Also

- `test-ss.scm` - Comprehensive test suite
- Original SIOD documentation
- POSIX socket API documentation

---

**Part of SIOD-TR (The Reawakening)**  
**Cross-platform socket programming for Scheme**
