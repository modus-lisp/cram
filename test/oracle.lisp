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

  (format t "----------------------------------~%")
  (format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
  (zerop *fails*))
