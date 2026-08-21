;;;; inflate.lisp — DEFLATE / zlib DECOMPRESSION (RFC 1951/1950).
;;;;
;;;; The other half of cram: a from-scratch inflate so the ecosystem can drop
;;;; chipz too.  Puff-style canonical Huffman decoding (bit-at-a-time, no big
;;;; tables), all three block types (stored / fixed / dynamic).  The top-level
;;;; entries return a CONSUMED byte count as a second value, so a caller can find
;;;; where one stream ends inside a bigger buffer (e.g. objects packed back-to-
;;;; back in a git packfile).

(in-package #:cram)

;;; ---- bit reader (DEFLATE reads bits LSB-first) ------------------------------

;;; END bounds the readable input.  It matters when a stream sits inside a bigger
;;; buffer with unrelated bytes after it (WOFF1 packs one zlib stream per table):
;;; without it a corrupt length runs on into the neighbour instead of erroring.
(defstruct (bitr (:constructor %make-bitr))
  (data #() :type (simple-array (unsigned-byte 8) (*)))
  (pos 0) (acc 0) (n 0)
  ;; END bounds the readable region.  What happens PAST it depends on the format:
  ;; running off the end of a DEFLATE stream is a corrupt stream and must signal,
  ;; which is the default; VP8L pads its final symbol with implicit zeros and has
  ;; to read a few bits beyond the last byte, which is SOFT — shift in zero bytes
  ;; and set OVERRUN, so a caller that cares can ask afterwards.  Two formats, two
  ;; correct answers, and picking one would have broken the other.
  (end 0) (soft nil) (overrun nil))

(declaim (inline br-need br-bits br-bit))
(defun br-need (br count)
  (loop while (< (bitr-n br) count) do
    (let* ((p (bitr-pos br))
           (byte (cond ((< p (bitr-end br)) (aref (bitr-data br) p))
                       ((bitr-soft br) (setf (bitr-overrun br) t) 0)
                       (t (error "cram: out of input")))))
      (setf (bitr-acc br) (logior (bitr-acc br) (ash byte (bitr-n br))))
      (incf (bitr-pos br)) (incf (bitr-n br) 8))))

(defun br-bits (br count)
  (if (zerop count) 0
      (progn (br-need br count)
             (let ((v (logand (bitr-acc br) (1- (ash 1 count)))))
               (setf (bitr-acc br) (ash (bitr-acc br) (- count)) (bitr-n br) (- (bitr-n br) count))
               v))))

(defun br-bit (br) (br-bits br 1))

(defun br-align (br)
  "Discard bits up to the next byte boundary."
  (let ((drop (logand (bitr-n br) 7)))
    (setf (bitr-acc br) (ash (bitr-acc br) (- drop)) (bitr-n br) (- (bitr-n br) drop))))

(defun br-consumed (br)
  "Input bytes actually consumed (whole bytes still buffered in ACC don't count)."
  (- (bitr-pos br) (ash (bitr-n br) -3)))

;;; ---- canonical Huffman decoder (puff-style) --------------------------------

(defstruct (huff (:constructor %make-huff)) count symbol)

(defun build-huff (lengths n)
  (let ((count (make-array 17 :initial-element 0))
        (symbol (make-array n :initial-element 0))
        (offs (make-array 17 :initial-element 0)))
    (dotimes (i n) (incf (aref count (aref lengths i))))
    (setf (aref count 0) 0)
    (loop for len from 1 to 15 do (setf (aref offs (1+ len)) (+ (aref offs len) (aref count len))))
    (dotimes (i n)
      (when (plusp (aref lengths i))
        (setf (aref symbol (aref offs (aref lengths i))) i)
        (incf (aref offs (aref lengths i)))))
    (%make-huff :count count :symbol symbol)))

(defun decode-sym (br h)
  (let ((code 0) (first 0) (index 0) (count (huff-count h)) (symbol (huff-symbol h)))
    (loop for len from 1 to 15 do
      (setf code (logior code (br-bit br)))
      (let ((c (aref count len)))
        (when (< (- code c) first) (return-from decode-sym (aref symbol (+ index (- code first)))))
        (incf index c) (incf first c) (setf first (ash first 1) code (ash code 1))))
    (error "cram: invalid Huffman code")))

(defparameter *fixed-lit-huff* (build-huff *lit-len* 288))
(defparameter *fixed-dist-huff* (build-huff *dist-len* 30))
(defparameter *clen-order* #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;;; ---- block decoding ---------------------------------------------------------

(defun read-dynamic-tables (br)
  "Read a dynamic block's header; return (values lit-huff dist-huff)."
  (let* ((hlit (+ 257 (br-bits br 5))) (hdist (+ 1 (br-bits br 5)))
         (hclen (+ 4 (br-bits br 4))) (cl (make-array 19 :initial-element 0)))
    (dotimes (i hclen) (setf (aref cl (aref *clen-order* i)) (br-bits br 3)))
    (let ((clh (build-huff cl 19)) (lengths (make-array (+ hlit hdist) :initial-element 0)) (i 0))
      (loop while (< i (+ hlit hdist)) do
        (let ((sym (decode-sym br clh)))
          (cond
            ((< sym 16) (setf (aref lengths i) sym) (incf i))
            ((= sym 16) (let ((rep (+ 3 (br-bits br 2))) (prev (aref lengths (1- i))))
                          (dotimes (k rep) (setf (aref lengths i) prev) (incf i))))
            ((= sym 17) (let ((rep (+ 3 (br-bits br 3)))) (dotimes (k rep) (setf (aref lengths i) 0) (incf i))))
            (t          (let ((rep (+ 11 (br-bits br 7)))) (dotimes (k rep) (setf (aref lengths i) 0) (incf i)))))))
      (values (build-huff (subseq lengths 0 hlit) hlit)
              (build-huff (subseq lengths hlit) hdist)))))

(defun inflate-huffman (br out lit dist)
  (loop
    (let ((sym (decode-sym br lit)))
      (cond
        ((< sym 256) (vector-push-extend sym out))
        ((= sym 256) (return))
        (t (let* ((li (- sym 257))
                  (len (+ (aref *len-base* li) (br-bits br (aref *len-extra* li))))
                  (ds (decode-sym br dist))
                  (d (+ (aref *dist-base* ds) (br-bits br (aref *dist-extra* ds))))
                  (from (- (fill-pointer out) d)))
             (dotimes (k len) (vector-push-extend (aref out (+ from k)) out))))))))

(defun inflate-into (br out)
  "Inflate all blocks (until BFINAL) into the adjustable byte vector OUT."
  (loop
    (let ((bfinal (br-bit br)) (btype (br-bits br 2)))
      (ecase btype
        (0 (br-align br)
           (let ((len (br-bits br 16)))
             (br-bits br 16)                              ; NLEN (ignored)
             (dotimes (i len) (vector-push-extend (br-bits br 8) out))))
        (1 (inflate-huffman br out *fixed-lit-huff* *fixed-dist-huff*))
        (2 (multiple-value-bind (lh dh) (read-dynamic-tables br) (inflate-huffman br out lh dh))))
      (when (= bfinal 1) (return)))))

;;; ---- top-level --------------------------------------------------------------

(defun %new-out () (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
(defun %finalize (out) (let ((v (make-array (fill-pointer out) :element-type '(unsigned-byte 8))))
                         (replace v out) v))

(defun %ub8v (data) (coerce data '(simple-array (unsigned-byte 8) (*))))

(defun deflate-decompress (data &key (start 0) end)
  "Inflate a raw DEFLATE stream from DATA[START..END).  Returns (values bytes
   consumed-input-bytes), where CONSUMED is an index into DATA, not a length."
  (let* ((v (%ub8v data))
         (br (%make-bitr :data v :pos start :end (or end (length v))))
         (out (%new-out)))
    (inflate-into br out)
    (br-align br)
    (values (%finalize out) (br-consumed br))))

(defun zlib-decompress (data &key (start 0) end (verify t))
  "Inflate a zlib stream (RFC 1950) from DATA[START..END).  Returns (values bytes
   consumed-input-bytes), where CONSUMED includes the 2-byte header and the 4-byte
   adler-32.  VERIFY NIL skips the adler-32 check."
  (let* ((v (%ub8v data))
         (e (or end (length v))))
    (when (< (- e start) 2) (error "cram: zlib stream too short"))
    (let ((cmf (aref v start)) (flg (aref v (1+ start))))
      (unless (zerop (mod (+ (* cmf 256) flg) 31)) (error "cram: bad zlib header"))
      (when (logtest flg 32) (error "cram: preset dictionary not supported")))
    (let ((br (%make-bitr :data v :pos (+ start 2) :end e)) (out (%new-out)))
      (inflate-into br out)
      (br-align br)
      (let ((consumed (br-consumed br)))
        (when verify
          (unless (<= (+ consumed 4) e) (error "cram: truncated adler-32"))
          (let ((got (adler32 out))
                (want (logior (ash (aref v consumed) 24) (ash (aref v (+ consumed 1)) 16)
                              (ash (aref v (+ consumed 2)) 8) (aref v (+ consumed 3)))))
            (unless (= got want) (error "cram: adler-32 mismatch"))))
        (values (%finalize out) (+ consumed 4))))))

(defun gzip-decompress (data &key (start 0) end (verify t))
  "Inflate a gzip stream (RFC 1952) from DATA[START..END), including the optional
   FEXTRA / FNAME / FCOMMENT / FHCRC header fields.  Returns (values bytes
   consumed-input-bytes), where CONSUMED includes the 8-byte crc32+isize trailer.
   VERIFY NIL skips that trailer's checks."
  (let* ((v (%ub8v data))
         (e (or end (length v)))
         (p start))
    (when (< (- e start) 18) (error "cram: gzip stream too short"))
    (unless (and (= (aref v p) #x1f) (= (aref v (1+ p)) #x8b)) (error "cram: bad gzip magic"))
    (let ((cm (aref v (+ p 2))) (flg (aref v (+ p 3))))
      (unless (= cm 8) (error "cram: unexpected gzip method ~d" cm))
      (incf p 10)                       ; magic(2) cm(1) flg(1) mtime(4) xfl(1) os(1)
      (flet ((need (n) (unless (<= (+ p n) e) (error "cram: truncated gzip header")))
             (skip-cstring ()
               (loop until (and (< p e) (zerop (aref v p)))
                     do (when (>= p e) (error "cram: truncated gzip header")) (incf p))
               (incf p)))
        (when (logbitp 2 flg)                            ; FEXTRA
          (need 2)
          (incf p (+ 2 (logior (aref v p) (ash (aref v (1+ p)) 8)))))
        (when (logbitp 3 flg) (skip-cstring))            ; FNAME
        (when (logbitp 4 flg) (skip-cstring))            ; FCOMMENT
        (when (logbitp 1 flg) (need 2) (incf p 2))       ; FHCRC
        (need 0)))
    (let ((br (%make-bitr :data v :pos p :end e)) (out (%new-out)))
      (inflate-into br out)
      (br-align br)
      (let ((consumed (br-consumed br)))
        (when verify
          (unless (<= (+ consumed 8) e) (error "cram: truncated gzip trailer"))
          (let ((want-crc (logior (aref v consumed) (ash (aref v (+ consumed 1)) 8)
                                  (ash (aref v (+ consumed 2)) 16) (ash (aref v (+ consumed 3)) 24)))
                (want-len (logior (aref v (+ consumed 4)) (ash (aref v (+ consumed 5)) 8)
                                  (ash (aref v (+ consumed 6)) 16) (ash (aref v (+ consumed 7)) 24))))
            (unless (= (crc32 out) want-crc) (error "cram: gzip crc-32 mismatch"))
            ;; ISIZE is the length mod 2^32 (RFC 1952 §2.3.1), so compare it that way.
            (unless (= (mod (fill-pointer out) (expt 2 32)) want-len)
              (error "cram: gzip length mismatch"))))
        (values (%finalize out) (+ consumed 8))))))
