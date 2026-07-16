# cram

> Working name — rename freely.

**A from-scratch DEFLATE / zlib compressor in pure Common Lisp — with
`Z_SYNC_FLUSH`.** Fixed-Huffman blocks plus a greedy LZ77 hash-chain matcher over
a 32 KB window, producing standard zlib/DEFLATE (RFC 1950/1951) that any inflate
decodes.

It exists for one feature the existing pure-CL deflate ([salza2]) doesn't expose:
a **persistent stream you can sync-flush per message.** That's what the VNC/RFB
`ZRLE` and `Tight` encodings require — one zlib stream per connection, flushed to
a byte boundary after each rectangle so the client can decode it immediately,
while the compression window carries across rectangles. No FFI, no dependencies.

```lisp
(asdf:load-system "cram")

;; one-shot
(cram:zlib-compress  #(...bytes...))   ; => a complete zlib stream
(cram:deflate-compress #(...bytes...)) ; => raw DEFLATE

;; persistent, sync-flushed stream (the reason cram exists)
(let ((zs (cram:make-zstream)))
  (cram:compress zs message-1) (cram:sync-flush zs)   ; => bytes for message 1
  (cram:compress zs message-2) (cram:sync-flush zs)   ; may back-reference message 1
  (cram:finish zs))                                    ; => final bytes + adler-32
```

## Status & disclaimer

Small and young: fixed-Huffman only (no dynamic-Huffman blocks yet), greedy
matching (no lazy matching). Correct and standards-conformant, but not tuned for
maximum ratio. **Research / educational; not audited.** No warranty (see
[LICENSE](LICENSE)).

## Correctness

Verified by round-tripping through **chipz** (an independent pure-CL inflate): a
range of inputs (empty, tiny, text, run-length, patterned, random) compress and
decompress back byte-for-byte; compressible data actually shrinks; and — the
point — a **persistent, sync-flushed stream decodes correctly through one
persistent inflate state, one message at a time**, exactly as a VNC client would.

## Build & test

Pure Common Lisp, no dependencies (the test uses chipz as an oracle).

```lisp
(push #p"/path/to/cram/" asdf:*central-registry*)
(asdf:load-system "cram")
```

```sh
./run-tests.sh        # loads cram + chipz, round-trips, exits non-zero on failure
```

or from a REPL: `(asdf:load-system "cram/test")` then `(cram/test:run-tests)`.

## Not yet

Dynamic-Huffman blocks (better ratio on skewed data), lazy matching, a raw-gzip
wrapper, and a `full-flush` (window-reset) mode. Contributions welcome.

## License

MIT — see [LICENSE](LICENSE).

[salza2]: https://www.xach.com/lisp/salza2/
