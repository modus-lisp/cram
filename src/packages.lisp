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
   ;; one-shot
   #:zlib-compress #:deflate-compress #:adler32
   ;; persistent stream (the reason this exists): make -> compress* -> sync-flush* [-> finish]
   #:make-zstream #:zstream #:compress #:sync-flush #:finish #:reset))
