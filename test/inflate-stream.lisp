;;;; inflate-stream.lisp — ZLIB-INFLATE-STREAM and ZLIB-INFLATE-TO against the one-shot inflate.
;;;;
;;;; Every stream is decoded three ways -- ZLIB-DECOMPRESS, ZLIB-INFLATE-TO in place, and
;;;; ZLIB-INFLATE-STREAM fed in reads of 1, 7 and 4096 octets -- and all must agree, report the
;;;; stream's own length with garbage after it, and refuse a corrupted adler-32.  Streams come
;;;; from chipz-independent sources: cram's compressor (fixed and stored blocks) and a dynamic-
;;;; Huffman stream from salza2, so all three block types are read across buffer boundaries.

(in-package #:cram/test)

(defun %stream-inflate (z chunk)
  "ZLIB-INFLATE-STREAM of Z read CHUNK octets at a time: (values output consumed)."
  (let ((pos 0) (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (multiple-value-bind (total used)
        (cram:zlib-inflate-stream
         (lambda (buf s e) (let ((n (min (- e s) (- (length z) pos) chunk)))
                             (replace buf z :start1 s :end1 (+ s n) :start2 pos) (incf pos n) n))
         (lambda (buf s e) (loop for i from s below e do (vector-push-extend (aref buf i) out))))
      (check (= total (length out)) "inflate-stream reports the octets it handed over")
      (values (u8 out) used))))

(defun run-inflate-stream-tests ()
  (format t "~&[inflate-stream — streamed against one-shot]~%")
  (let ((garbage (s->u8 "TRAILING GARBAGE AFTER THE STREAM")))
    (dolist (data (list (u8 #()) (s->u8 "a") (patterned 300000) (random-bytes 200000)
                        (u8 (make-array 250000 :initial-element 0))))
      (dolist (z (list (cram:zlib-compress data)
                       (salza2:compress-data data 'salza2:zlib-compressor)))
        ;; graded against the one-shot inflate of the same stream, which the oracle above has
        ;; already graded against chipz: salza2 compresses an EMPTY input to a stream of 32768
        ;; zeros (both inflates and chipz agree), so "the original data" is not the reference
        (let ((buf (cat z garbage)) (data (cram:zlib-decompress z)))
          (dolist (chunk '(1 7 4096))
            (multiple-value-bind (out used) (%stream-inflate buf chunk)
              (check (equalp out data) "streamed (reads of ~a), len ~a" chunk (length data))
              (check (= used (length z)) "streamed consumed ~a of a ~a-octet stream" used (length z))))
          (let ((n 0))
            (multiple-value-bind (total used)
                (cram:zlib-inflate-to buf (lambda (b s e) (declare (ignore b)) (incf n (- e s))))
              (check (and (= total n (length data)) (= used (length z)))
                     "in place, len ~a: ~a octets, ~a consumed" (length data) total used)))
          (when (> (length data) 10)
            (let ((bad (copy-seq z)))
              (setf (aref bad (1- (length bad))) (logxor (aref bad (1- (length bad))) 1))
              (check (nth-value 1 (ignore-errors (%stream-inflate bad 4096)))
                     "a corrupted adler-32 is refused, len ~a" (length data)))))))))
