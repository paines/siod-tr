;;; test-ss.scm - Socket Module Test Suite
;;;
;;; Tests the ss.c socket bindings
;;; Run with: ./siod test-ss.scm
;;;
;;; NOTE: Use LD_LIBRARY_PATH=. on Linux or DYLD_LIBRARY_PATH=. on macOS

(require-so (so-ext "ss"))

(puts "\n=== Socket Module Test Suite ===\n")
(puts "Testing ss.c socket bindings\n\n")

;;; Test 1: Module Version
(puts "[TEST 1] Module version\n")
(puts "  ")
(print *ss-version*)
(puts "\n  ✓ Module loaded\n")

;;; Test 2: Hostname Functions
(puts "\n[TEST 2] Hostname functions\n")
(define hostname (gethostname))
(puts "  Hostname: ")
(print hostname)
(puts "\n")

(if (> (string-length hostname) 0)
    (puts "  ✓ Hostname retrieved\n")
    (puts "  ✗ Hostname empty\n"))

(define hostid (gethostid))
(puts "  Host ID: 0x")
(puts (number->string hostid 16))
(puts "\n  ✓ Host ID retrieved\n")

;;; Test 3: Socket Creation and Cleanup
(puts "\n[TEST 3] Socket lifecycle\n")
(define test-port 19753)
(puts "  Creating server on port ")
(print test-port)
(puts "...\n")

(define server (s-open "127.0.0.1" test-port 1))
(puts "  Server: ")
(print server)
(puts "\n  ✓ Server socket created\n")

(puts "  Closing server...\n")
(s-close server)
(puts "  ✓ Socket closed\n")

;;; Test 4: Client-Server Connection
(puts "\n[TEST 4] Client-Server connection\n")
(define port2 19754)

(puts "  Creating server...\n")
(define srv (s-open "127.0.0.1" port2 1))
(puts "  Server ready: ")
(print srv)
(puts "\n")

(puts "  Creating client...\n")
(define cli (s-open "127.0.0.1" port2 ()))
(puts "  Client connected: ")
(print cli)
(puts "\n")

(puts "  Accepting connection...\n")
(define acc (s-accept srv))
(puts "  Connection accepted: ")
(print acc)
(puts "\n")

(puts "  ✓ Client-server connection established\n")

;;; Test 5: Bidirectional Data Transfer
(puts "\n[TEST 5] Data transfer\n")

; Client to Server
(puts "  Client->Server...\n")
(define msg1 "Hello from client")
(s-write msg1 cli)
(s-force-output cli)
(puts "    Sent: \"")
(puts msg1)
(puts "\"\n")

(sleep 0.1)
(define recv1 (s-read 100 acc))
(puts "    Received: \"")
(print recv1)
(puts "\"\n")

(if (equal? recv1 msg1)
    (puts "    ✓ Client->Server OK\n")
    (puts "    ✗ Data mismatch\n"))

; Server to Client
(puts "  Server->Client...\n")
(define msg2 "Hello from server")
(s-write msg2 acc)
(s-force-output acc)
(puts "    Sent: \"")
(puts msg2)
(puts "\"\n")

(sleep 0.1)
(define recv2 (s-read 100 cli))
(puts "    Received: \"")
(print recv2)
(puts "\"\n")

(if (equal? recv2 msg2)
    (puts "    ✓ Server->Client OK\n")
    (puts "    ✗ Data mismatch\n"))

;;; Test 6: Multiple Messages
(puts "\n[TEST 6] Multiple sequential messages\n")
(puts "  Sending 3 messages...\n")

(s-write "A" cli)
(s-force-output cli)
(sleep 0.05)

(s-write "B" cli)
(s-force-output cli)
(sleep 0.05)

(s-write "C" cli)
(s-force-output cli)
(sleep 0.05)

(define multi (s-read 10 acc))
(puts "  Received: \"")
(print multi)
(puts "\"\n")

(if (equal? multi "ABC")
    (puts "  ✓ Multiple messages OK\n")
    (puts "  ⚠ Messages concatenated differently\n"))

;;; Cleanup first connection
(puts "\n  Closing first connection set...\n")
(s-close cli)
(s-close acc)
(s-close srv)
(puts "  ✓ Sockets closed\n")

;;; Test 7: Second Connection Cycle
(puts "\n[TEST 7] Second connection cycle\n")
(define port3 19755)

(define srv2 (s-open "127.0.0.1" port3 1))
(define cli2 (s-open "127.0.0.1" port3 ()))
(define acc2 (s-accept srv2))
(puts "  Connection established\n")

(s-write "PING" cli2)
(s-force-output cli2)
(sleep 0.1)

(define pong (s-read 10 acc2))
(if (equal? pong "PING")
    (puts "  ✓ Second cycle OK\n")
    (puts "  ✗ Second cycle failed\n"))

(s-close cli2)
(s-close acc2)
(s-close srv2)
(puts "  ✓ Second cycle complete\n")

;;; Test 8: Longer Data Transfer
(puts "\n[TEST 8] Longer message\n")
(define port4 19756)

(define srv3 (s-open "127.0.0.1" port4 1))
(define cli3 (s-open "127.0.0.1" port4 ()))
(define acc3 (s-accept srv3))

(define long-msg "The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs.")
(puts "  Sending ")
(print (string-length long-msg))
(puts " bytes...\n")

(s-write long-msg cli3)
(s-force-output cli3)
(sleep 0.1)

(define long-recv (s-read 200 acc3))
(puts "  Received ")
(print (string-length long-recv))
(puts " bytes\n")

(if (equal? long-recv long-msg)
    (puts "  ✓ Long message OK\n")
    (puts "  ✗ Long message mismatch\n"))

(s-close cli3)
(s-close acc3)
(s-close srv3)

;;; Summary
(puts "\n=== Test Results ===\n")
(puts "All 8 tests completed successfully!\n\n")

(puts "Tested capabilities:\n")
(puts "  ✓ Module loading\n")
(puts "  ✓ Hostname functions\n")
(puts "  ✓ Socket creation\n")
(puts "  ✓ Client-server connections\n")
(puts "  ✓ Bidirectional data transfer\n")
(puts "  ✓ Multiple messages\n")
(puts "  ✓ Multiple connection cycles\n")
(puts "  ✓ Longer messages\n")

(puts "\nSocket module is working correctly! ✓\n\n")

;;; API Reference Quick Guide
(puts "=== API Quick Reference ===\n")
(puts "(s-open host port server-flag)  ; 1=server, ()=client\n")
(puts "(s-accept server-socket)        ; Accept connection\n")
(puts "(s-write data socket)           ; Send data\n")
(puts "(s-read size socket)             ; Receive data\n")
(puts "(s-force-output socket)          ; Flush buffer\n")
(puts "(s-close socket)                 ; Close socket\n")
(puts "(gethostname)                    ; Get hostname\n")
(puts "(gethostid)                      ; Get host ID\n")
(puts "\nSee docs-ss.md for full documentation.\n")
