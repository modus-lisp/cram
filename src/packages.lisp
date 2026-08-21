;;;; packages.lisp — cram

(defpackage #:cram
  (:use #:cl)
  (:documentation
   "cram — a from-scratch DEFLATE / zlib compressor in pure Common Lisp, tailored
    to what the modus-lisp stack needs: a persistent zlib stream that can be
    SYNC-FLUSHED per message (Z_SYNC_FLUSH) — the thing RFB's ZRLE/Tight encodings
    require and that salza2 doesn't expose.  Output is standard zlib/DEFLATE (RFC
    1950/1951), decoded by any inflate (we verify against chipz).  No FFI; no
    dependencies.")
  (:export
   ;; compress: one-shot
   #:zlib-compress #:deflate-compress #:adler32 #:crc32
   ;; compress: persistent stream (the reason this exists) make -> compress* -> sync-flush* [-> finish]
   #:make-zstream #:zstream #:compress #:sync-flush #:sync-flush-stored #:finish #:reset
   ;; decompress (returns (values bytes consumed-input-bytes))
   #:zlib-decompress #:deflate-decompress #:gzip-decompress
   ;; LZW — the OTHER compression this stack meets (GIF; TIFF and PDF LZWDecode are
   ;; the same algorithm packed differently).  See src/lzw.lisp on why the variant is
   ;; an explicit argument and an unimplemented one signals.
   #:lzw-decode
   ;; Entropy-coding primitives (src/prefix.lisp): canonical prefix codes, LZ77
   ;; back-references and a hash-addressed recent-value cache.  Shared by DEFLATE
   ;; and by formats that reuse the same machinery with different framing —
   ;; WebP lossless (VP8L) is the first outside caller.
   #:make-bit-reader #:read-bits #:bit-reader-bytes-consumed
   #:bit-reader-overrun-p #:bit-reader-past-end-p
   #:make-prefix-code #:prefix-code #:prefix-code-p #:read-prefix-symbol
   #:read-prefix-coded-int #:lz77-copy
   #:make-recent-cache #:recent-cache #:recent-cache-p
   #:recent-cache-put #:recent-cache-ref #:recent-cache-index))
