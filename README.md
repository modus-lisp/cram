# cram

> Working name — rename freely.

**A from-scratch DEFLATE / zlib codec in pure Common Lisp — with `Z_SYNC_FLUSH`.**
Compress *and* decompress: fixed-Huffman + greedy LZ77 (hash-chain, 32 KB window)
on the way out; a puff-style canonical-Huffman inflater (all three block types) on
the way in. Standard zlib/DEFLATE (RFC 1950/1951) — its output is read by any
inflate, and it reads anyone's zlib.

It exists for one feature the existing pure-CL deflate ([salza2]) doesn't expose:
a **persistent stream you can sync-flush per message** — one zlib stream per
connection, flushed to a byte boundary after each message so the peer can decode
it immediately while the compression window carries over. That's what the VNC/RFB
`ZRLE` and `Tight` encodings need. Having both halves lets a project drop chipz
*and* salza2. No FFI, no dependencies.

```lisp
(asdf:load-system "cram")

;; one-shot, both ways
(cram:zlib-compress    #(...bytes...))  ; => a complete zlib stream
(cram:zlib-decompress  z)               ; => (values bytes consumed-input-bytes)
(cram:deflate-compress #(...))          ; / (cram:deflate-decompress ...) for raw DEFLATE

;; persistent, sync-flushed stream (the reason cram exists)
(let ((zs (cram:make-zstream)))
  (cram:compress zs message-1) (cram:sync-flush zs)   ; => bytes for message 1
  (cram:compress zs message-2) (cram:sync-flush zs)   ; may back-reference message 1
  (cram:finish zs))                                    ; => final bytes + adler-32
```

`zlib-decompress` / `deflate-decompress` return the byte count consumed as a
second value — so you can find where one stream ends inside a bigger buffer (git
objects packed back-to-back, say), and decode the next at that offset.

## Status & disclaimer

Small and young: fixed-Huffman only (no dynamic-Huffman blocks yet), greedy
matching (no lazy matching). Correct and standards-conformant, but not tuned for
maximum ratio. **Research / educational; not audited.** No warranty (see
[LICENSE](LICENSE)).

## Correctness

Cross-checked both directions against two independent libraries: cram's *output*
decodes under **chipz**, and cram *inflates* the output of **salza2** — plus
cram's own compress↔decompress round-trips. A range of inputs (empty, tiny, text,
run-length, patterned, random) survive byte-for-byte; compressible data actually
shrinks; the `consumed` count correctly locates a second stream packed right after
the first; and — the point — a **persistent, sync-flushed stream decodes through
one persistent inflate state, one message at a time**, exactly as a VNC peer
would.

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
