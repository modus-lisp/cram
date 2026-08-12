;;;; oracle.lisp — round-trip cram's output through chipz (an independent inflate).
;;;;
;;;; If chipz decompresses back to the original bytes, cram's DEFLATE/zlib output
;;;; is standards-correct — including the persistent, sync-flushed stream that RFB
;;;; ZRLE/Tight need (verified the way a VNC client would: one persistent inflate
;;;; state decoding each sync-flushed message in turn).

(defpackage #:cram/test (:use #:cl) (:export #:run-tests))
(in-package #:cram/test)

(defvar *checks* 0) (defvar *fails* 0)
(defun check (ok fmt &rest a) (incf *checks*) (unless ok (incf *fails*) (format t "  FAIL: ~?~%" fmt a)))
(defun u8 (seq) (coerce seq '(simple-array (unsigned-byte 8) (*))))
(defun cat (&rest vs) (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) vs))
(defun s->u8 (s) (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defvar *seed* 2463534242)
(defun rnd () (setf *seed* (logand (logxor *seed* (ash *seed* 13)) #xffffffff)
                    *seed* (logxor *seed* (ash *seed* -17))
                    *seed* (logand (logxor *seed* (ash *seed* 5)) #xffffffff)))
(defun random-bytes (n) (let ((a (make-array n :element-type '(unsigned-byte 8))))
                          (dotimes (i n a) (setf (aref a i) (logand (rnd) #xff)))))
(defun patterned (n) (let ((a (make-array n :element-type '(unsigned-byte 8))))
                       (dotimes (i n a) (setf (aref a i) (mod (+ (* i 3) (floor i 37)) 50)))))

(defun run-tests ()
  (setf *checks* 0 *fails* 0)
  (format t "~&[cram oracle — round-trip through chipz]~%")

  ;; 1. one-shot zlib round-trips over varied data
  (dolist (data (list (u8 #()) (s->u8 "a")
                      (s->u8 "the quick brown fox jumps over the lazy dog. ")
                      (u8 (make-array 3000 :initial-element 65))
                      (patterned 20000)
                      (random-bytes 8000)))
    (let* ((z (cram:zlib-compress data)) (back (chipz:decompress nil :zlib z)))
      (check (equalp back data) "one-shot roundtrip, len ~a -> ~a bytes" (length data) (length z))))

  ;; compression actually happens on compressible data
  (let* ((data (patterned 20000)) (z (cram:zlib-compress data)))
    (check (< (length z) (floor (length data) 4)) "patterned 20000 -> ~a bytes (compressed)" (length z)))
  (let* ((data (s->u8 (with-output-to-string (s) (dotimes (i 500) (write-string "hello world " s)))))
         (z (cram:zlib-compress data)))
    (check (< (length z) (floor (length data) 8)) "repeated text ~a -> ~a bytes" (length data) (length z)))

  ;; 2. streaming: sync-flush per chunk + finish, concatenated == a full zlib stream
  (let* ((c1 (s->u8 "the quick brown fox ")) (c2 (s->u8 "the quick brown fox jumps "))
         (c3 (s->u8 "over the lazy dog again and again"))
         (zs (cram:make-zstream)) (parts '()))
    (cram:compress zs c1) (push (cram:sync-flush zs) parts)
    (cram:compress zs c2) (push (cram:sync-flush zs) parts)   ; can back-reference c1
    (cram:compress zs c3) (push (cram:finish zs) parts)
    (let* ((full (apply #'cat (reverse parts))) (back (chipz:decompress nil :zlib full)))
      (check (equalp back (cat c1 c2 c3)) "streaming full-stream roundtrip")))

  ;; 2b. cram INFLATE round-trips (our own + an independent compressor's output)
  (dolist (data (list (u8 #()) (s->u8 "a")
                      (s->u8 "the quick brown fox jumps over the lazy dog. ")
                      (u8 (make-array 3000 :initial-element 65))
                      (patterned 20000) (random-bytes 8000)))
    (multiple-value-bind (back consumed) (cram:zlib-decompress (cram:zlib-compress data))
      (declare (ignore consumed))
      (check (equalp back data) "cram deflate<->inflate roundtrip, len ~a" (length data)))
    ;; independent source: salza2 compresses, cram inflates (salza2 mangles empty
    ;; input into a run of zeros — its quirk, not ours — so cross-check non-empty)
    (when (plusp (length data))
      (let ((z (salza2:compress-data data 'salza2:zlib-compressor)))
        (check (equalp (cram:zlib-decompress z) data) "salza2 -> cram inflate, len ~a" (length data))))
    ;; raw deflate round-trip
    (check (equalp (cram:deflate-decompress (cram:deflate-compress data)) data)
           "cram raw deflate<->inflate, len ~a" (length data)))

  ;; 2c. consumed-count: two zlib streams back-to-back, decode the first, find the second
  (let* ((d1 (s->u8 "FIRST object bytes here")) (d2 (s->u8 "SECOND object, different"))
         (buf (cat (cram:zlib-compress d1) (cram:zlib-compress d2))))
    (multiple-value-bind (out1 consumed) (cram:zlib-decompress buf)
      (check (equalp out1 d1) "consumed-count: first stream decodes")
      (check (equalp (cram:zlib-decompress buf :start consumed) d2)
             "consumed-count: second stream found at offset ~a" consumed)))

  ;; 2d. Incompressible input larger than a stored block's 16-bit length.  The
  ;; stored branch of EMIT-BLOCK wins exactly here, and it used to emit one
  ;; oversized block: everything past 65535 bytes was silently lost, and no
  ;; inflate (ours or chipz's) could read the result.
  (dolist (n '(65535 65536 70000 200000))
    (let* ((data (random-bytes n)) (z (cram:zlib-compress data)))
      (check (equalp (chipz:decompress nil :zlib z) data) "incompressible ~d B: chipz agrees" n)
      (check (equalp (cram:zlib-decompress z) data) "incompressible ~d B: cram round-trips" n)))

  ;; 3. RFB-style: one persistent inflate state, decode each sync-flushed message
  (let ((zs (cram:make-zstream)) (ds (chipz:make-dstate :zlib))
        (msgs (list (s->u8 "MESSAGE one, some pixels here ")
                    (s->u8 "MESSAGE one, some pixels here too ")   ; repeats -> matches prior
                    (s->u8 "MESSAGE three: end")))
        (all-ok t))
    (dolist (m msgs)
      (cram:compress zs m)
      (let* ((flush (cram:sync-flush zs))
             (out (make-array 65536 :element-type '(unsigned-byte 8))))
        (multiple-value-bind (consumed produced) (chipz:decompress out ds flush)
          (declare (ignore consumed))
          (unless (equalp (subseq out 0 produced) m) (setf all-ok nil)))))
    (check all-ok "RFB-style incremental: each sync-flush decodes to its message"))

  ;; 4. crc-32 (RFC 1952), against the standard check value
  (check (= (cram:crc32 (s->u8 "123456789")) #xcbf43926) "crc-32 check value")
  (check (= (cram:crc32 (u8 #())) 0) "crc-32 of empty input")

  ;; 5. gzip: salza2 produces the stream, cram must decode it.  An independent
  ;; producer is the point — round-tripping our own framing would prove nothing.
  ;; (Empty input is excluded for the same reason as the zlib case above: salza2
  ;; emits a stream that decodes to a run of zeros, which chipz reproduces too.)
  (dolist (data (list (s->u8 "a")
                      (s->u8 "the quick brown fox jumps over the lazy dog. ")
                      (patterned 20000)
                      (random-bytes 8000)))
    (let ((g (salza2:compress-data data 'salza2:gzip-compressor)))
      (check (equalp (cram:gzip-decompress g) data)
             "salza2 gzip -> cram, len ~a -> ~a bytes" (length data) (length g))))

  ;; 5b. the optional header fields real gzip files carry.  salza2 emits a bare
  ;; header, so splice FEXTRA/FNAME/FCOMMENT in and check they are skipped.
  (let* ((data (s->u8 "header-variant payload, long enough to compress a little"))
         (bare (salza2:compress-data data 'salza2:gzip-compressor))
         (body (subseq bare 10)))                        ; past the fixed 10-byte header
    (flet ((framed (flg &rest extra)
             (cat (u8 (list #x1f #x8b 8 flg 0 0 0 0 0 3))
                  (apply #'cat (mapcar #'u8 extra)) body)))
      (check (equalp (cram:gzip-decompress (framed #b00001000 (map 'list #'char-code "f.txt")
                                                   (list 0)))
                     data)
             "gzip FNAME skipped")
      (check (equalp (cram:gzip-decompress (framed #b00000100 (list 3 0) (list 1 2 3))) data)
             "gzip FEXTRA skipped")
      (check (equalp (cram:gzip-decompress (framed #b00010000 (map 'list #'char-code "hi")
                                                   (list 0)))
                     data)
             "gzip FCOMMENT skipped")
      (check (equalp (cram:gzip-decompress (framed #b00000010 (list #xaa #xbb))) data)
             "gzip FHCRC skipped")))

  ;; 5c. a corrupt trailer must be caught, not silently returned
  (let ((g (copy-seq (salza2:compress-data (s->u8 "integrity matters here")
                                           'salza2:gzip-compressor))))
    (setf (aref g (- (length g) 5)) (logxor 255 (aref g (- (length g) 5))))
    (check (nth-value 1 (ignore-errors (cram:gzip-decompress g))) "gzip crc-32 mismatch signals")
    (check (equalp (cram:gzip-decompress g :verify nil) (s->u8 "integrity matters here"))
           "gzip :verify nil skips the check"))
  (let ((z (copy-seq (cram:zlib-compress (s->u8 "adler check")))))
    (setf (aref z (1- (length z))) (logxor 255 (aref z (1- (length z)))))
    (check (nth-value 1 (ignore-errors (cram:zlib-decompress z))) "zlib adler-32 mismatch signals"))

  ;; 6. :END — a stream embedded in a bigger buffer, the WOFF1 case: one zlib
  ;; stream per table, with the next table's bytes immediately after it.
  (let* ((data (patterned 5000))
         (z (cram:zlib-compress data))
         (buf (cat (s->u8 "PREFIX") z (s->u8 "TRAILING GARBAGE AFTER THE STREAM"))))
    (check (equalp (cram:zlib-decompress buf :start 6 :end (+ 6 (length z))) data)
           ":end bounds a stream inside a larger buffer")
    (check (equalp (cram:zlib-decompress buf :start 6) data)
           ":end defaulted still decodes the embedded stream")
    ;; truncated: the bound must turn a run-off into an error, not a wrong answer
    (check (nth-value 1 (ignore-errors
                         (cram:zlib-decompress buf :start 6 :end (+ 6 (floor (length z) 2)))))
           ":end turns a truncated stream into an error"))

  (format t "----------------------------------~%")
  (format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
  (zerop *fails*))
