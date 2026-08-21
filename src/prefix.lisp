;;;; prefix.lisp — canonical prefix (Huffman) codes, LZ77 back-references and a
;;;; hash-addressed recent-value cache: the entropy-coding machinery that is
;;;; *not* specific to DEFLATE.
;;;;
;;;; cram grew these out of its inflate, but they are the same primitives every
;;;; LZ77+prefix-code container reaches for — DEFLATE, WebP lossless (VP8L),
;;;; Brotli's literal codes — so they are a documented public API rather than
;;;; inflate internals.  What differs between formats is the *framing* (how code
;;;; lengths are laid out in the header, what the alphabets mean, how a distance
;;;; symbol maps to a distance); that stays in the format decoder.
;;;;
;;;; Bit order: all of these read LSB-first within a byte (see BITR in
;;;; inflate.lisp), and a canonical code's *first transmitted bit is the most
;;;; significant bit of the code*.  DEFLATE and VP8L agree on both conventions,
;;;; which is why one reader serves both.

(in-package #:cram)

;;; ---- bit reader (public face of BITR) ---------------------------------------

(defun make-bit-reader (data &key (start 0) end)
  "A LSB-first bit reader over DATA from byte START.

   END, if given, is a soft limit: reads past it yield zero bits and set the
   reader's overrun flag instead of signalling, because several formats let the
   final symbol run off the end of the payload and expect implicit zero padding.
   With END omitted, reading past the end of DATA signals, as inflate wants."
  ;; The two modes the docstring promises, mapped onto the one reader: an explicit
  ;; END is SOFT (zero-fill past it, set OVERRUN), and no END means bounded by the
  ;; data and signalling past it, which is what inflate has always done.
  (let ((data (coerce data '(simple-array (unsigned-byte 8) (*)))))
    (%make-bitr :data data :pos start
                :end (or end (length data))
                :soft (and end t))))

(declaim (inline read-bits))
(defun read-bits (br count)
  "Read COUNT bits LSB-first; the first bit read is the least significant bit
   of the returned integer."
  (br-bits br count))

(defun bit-reader-overrun-p (br)
  "True once the reader has had to invent zero bits past its END.

   Note this fires on mere lookahead: refilling a byte at a time, or peeking a
   fixed window to index a decode table, routinely reaches past the last byte
   while decoding a perfectly well-formed final symbol.  For \"was this stream
   actually truncated?\" use BIT-READER-PAST-END-P."
  (and (bitr-overrun br) t))

(defun bit-reader-past-end-p (br)
  "True when the reader has genuinely CONSUMED bits beyond its END — i.e. the
   stream really was short, as opposed to having merely looked ahead past it.

   This is the check a format decoder wants before trusting its output: a
   truncated file otherwise decodes its last few symbols out of invented zero
   bits and returns plausible, wrong data."
  (let ((end (bitr-end br)))
    (and end (> (br-consumed br) end))))

(defun bit-reader-bytes-consumed (br)
  "Input bytes actually consumed; whole bytes still sitting in the bit
   accumulator do not count."
  (br-consumed br))

;;; ---- canonical prefix codes -------------------------------------------------
;;;
;;; A code is described purely by its per-symbol code *lengths*; the codes
;;; themselves follow canonically (shorter lengths first, ties in symbol order).
;;; Decoding is the puff-style walk — accumulate one bit at a time and compare
;;; against the running count of codes of each length — fronted by a small
;;; direct-lookup table for the common short codes.

(defconstant +max-prefix-length+ 15
  "Longest code length any supported format uses (DEFLATE and VP8L both cap at 15).")

(defstruct (prefix-code (:constructor %make-prefix-code) (:conc-name pfx-))
  ;; puff-style tables: COUNT[len] = number of codes of that length,
  ;; SYMBOL = symbols ordered by (length, symbol).
  (count (make-array (1+ +max-prefix-length+) :element-type 'fixnum) :type (simple-array fixnum (*)))
  (symbol #() :type (simple-array fixnum (*)))
  ;; Fast path: FAST[peek] -> (symbol << 4) | length, for codes no longer than
  ;; FAST-BITS.  A zero entry means "too long, take the slow walk".
  (fast nil :type (or null (simple-array fixnum (*))))
  (fast-bits 0 :type fixnum)
  ;; A code with exactly one symbol consumes NO bits when read.  This is not a
  ;; micro-optimisation: VP8L relies on it (a single-leaf tree is written with
  ;; length 1 but read with zero bits), and a decoder that eats a bit here
  ;; desynchronises the whole stream.
  (single nil :type (or null fixnum)))

(defun make-prefix-code (lengths &key (n (length lengths)) (fast-bits 9))
  "Build a canonical prefix code from the first N entries of LENGTHS (a sequence
   of per-symbol code lengths, 0 meaning \"symbol not present\").

   Returns the code, or NIL if the lengths do not describe a complete binary
   tree — over-subscribed, or under-subscribed with more than one symbol.  A
   code with a single symbol is accepted and reads as zero bits.  Callers should
   treat NIL as a corrupt bitstream rather than pressing on."
  (let ((count (make-array (1+ +max-prefix-length+) :element-type 'fixnum :initial-element 0))
        (nsym 0) (last-sym 0))
    (dotimes (i n)
      (let ((len (elt lengths i)))
        (when (plusp len)
          (when (> len +max-prefix-length+) (return-from make-prefix-code nil))
          (incf (aref count len)) (incf nsym) (setf last-sym i))))
    (when (zerop nsym) (return-from make-prefix-code nil))
    ;; Kraft check: sum of 2^-len must be exactly 1 (single-symbol codes excepted).
    (unless (= nsym 1)
      (let ((left 1))
        (loop for len from 1 to +max-prefix-length+ do
          (setf left (- (ash left 1) (aref count len)))
          (when (minusp left) (return-from make-prefix-code nil)))   ; over-subscribed
        (unless (zerop left) (return-from make-prefix-code nil))))   ; incomplete
    (when (= nsym 1)
      (return-from make-prefix-code
        (%make-prefix-code :count count
                           :symbol (make-array 1 :element-type 'fixnum
                                                 :initial-element last-sym)
                           :single last-sym)))
    ;; canonical ordering
    (let ((symbol (make-array nsym :element-type 'fixnum :initial-element 0))
          (offs (make-array (+ 2 +max-prefix-length+) :element-type 'fixnum :initial-element 0)))
      (loop for len from 1 to +max-prefix-length+
            do (setf (aref offs (1+ len)) (+ (aref offs len) (aref count len))))
      (dotimes (i n)
        (let ((len (elt lengths i)))
          (when (plusp len)
            (setf (aref symbol (aref offs len)) i)
            (incf (aref offs len)))))
      (let ((code (%make-prefix-code :count count :symbol symbol)))
        (when (plusp fast-bits) (%build-fast-table code fast-bits))
        code))))

(defun %build-fast-table (pc fast-bits)
  "Fill PC's direct-lookup table: every FAST-BITS-wide bit pattern whose prefix
   is a code of length <= FAST-BITS resolves in one array reference."
  (let* ((size (ash 1 fast-bits))
         (fast (make-array size :element-type 'fixnum :initial-element 0))
         (count (pfx-count pc)) (symbol (pfx-symbol pc))
         (code 0) (index 0))
    (loop for len from 1 to fast-bits do
      (dotimes (k (aref count len))
        ;; CODE is the canonical code of length LEN for symbol SYMBOL[index].
        ;; It arrives MSB-first, i.e. reversed relative to the LSB-first stream.
        (let ((rev (%reverse-bits code len))
              (entry (logior (ash (aref symbol index) 4) len))
              (step (ash 1 len)))
          (loop for i from rev below size by step do (setf (aref fast i) entry)))
        (incf index) (incf code))
      (setf code (ash code 1)))
    (setf (pfx-fast pc) fast (pfx-fast-bits pc) fast-bits)))

(defun %reverse-bits (v n)
  (let ((r 0)) (dotimes (i n r) (setf r (logior (ash r 1) (logand (ash v (- i)) 1))))))

(defun read-prefix-symbol (br pc)
  "Read one symbol of prefix code PC from bit reader BR."
  (declare (type prefix-code pc))
  (let ((single (pfx-single pc)))
    (when single (return-from read-prefix-symbol single)))   ; zero bits consumed
  (let ((fast (pfx-fast pc)) (fb (pfx-fast-bits pc)))
    (when fast
      (br-need br fb)
      (let ((entry (aref fast (logand (bitr-acc br) (1- (ash 1 fb))))))
        (unless (zerop entry)
          (let ((len (logand entry 15)))
            (setf (bitr-acc br) (ash (bitr-acc br) (- len))
                  (bitr-n br) (- (bitr-n br) len))
            (return-from read-prefix-symbol (ash entry -4)))))))
  ;; slow path: walk bit by bit (only for codes longer than FAST-BITS)
  (let ((code 0) (first 0) (index 0)
        (count (pfx-count pc)) (symbol (pfx-symbol pc)))
    (loop for len from 1 to +max-prefix-length+ do
      (setf code (logior code (br-bit br)))
      (let ((c (aref count len)))
        (when (< (- code c) first)
          (return-from read-prefix-symbol (aref symbol (+ index (- code first)))))
        (incf index c) (incf first c)
        (setf first (ash first 1) code (ash code 1))))
    (error "cram: invalid prefix code")))

;;; ---- prefix-coded integers --------------------------------------------------

(defun read-prefix-coded-int (br symbol)
  "Expand SYMBOL into the integer it stands for, reading its extra bits from BR.

   This is the length/distance integer coding WebP lossless uses: symbols 0-3
   are the literal values 1-4, and every higher symbol names a bucket
   (2 + (symbol & 1)) << ((symbol - 2) >> 1) wide, selected within by that many
   extra bits.  DEFLATE uses table-driven buckets instead; this closed form is
   the other common choice."
  (declare (type fixnum symbol))
  (if (< symbol 4)
      (1+ symbol)
      (let* ((extra (ash (- symbol 2) -1))
             (offset (ash (+ 2 (logand symbol 1)) extra)))
        (+ offset (br-bits br extra) 1))))

;;; ---- LZ77 back-references ---------------------------------------------------

(defun lz77-copy (out pos distance length)
  "Copy LENGTH elements into OUT at POS from POS-DISTANCE, one at a time so that
   overlapping references (the run-length case, distance smaller than length)
   read the elements this very call is writing.  Returns POS + LENGTH.

   Signals if the reference points before the start of OUT or would run past its
   end — a corrupt stream must not be allowed to fabricate pixels."
  (declare (type fixnum pos distance length))
  (when (or (< distance 1) (> distance pos) (> (+ pos length) (length out)))
    (error "cram: LZ77 reference out of range (pos ~d distance ~d length ~d limit ~d)"
           pos distance length (length out)))
  (let ((from (- pos distance)))
    (declare (type fixnum from))
    (dotimes (i length) (setf (aref out (+ pos i)) (aref out (+ from i)))))
  (+ pos length))

;;; ---- hash-addressed recent-value cache --------------------------------------
;;;
;;; A fixed-size array of recently emitted values addressed by a multiplicative
;;; hash of the value itself.  There is no conflict resolution and no eviction
;;; policy: a new value simply takes the slot its hash names.  That works because
;;; both sides run the same insert sequence, so the encoder always knows which
;;; slot a value currently occupies.  WebP lossless calls this the "colour
;;; cache"; nothing about it is colour-specific.

(defconstant +recent-cache-multiplier+ #x1e35a7bd
  "Golden-ratio-ish odd 32-bit multiplier; the constant WebP lossless specifies.")

(defstruct (recent-cache (:constructor %make-recent-cache) (:conc-name rc-))
  (table #() :type simple-vector)
  (shift 0 :type fixnum)
  (multiplier +recent-cache-multiplier+ :type (unsigned-byte 32)))

(defun make-recent-cache (bits &key (multiplier +recent-cache-multiplier+))
  "A cache of 2^BITS slots, addressed by the top BITS bits of the 32-bit
   product MULTIPLIER * value.  All slots start at 0."
  (check-type bits (integer 1 16))
  (%make-recent-cache :table (make-array (ash 1 bits) :initial-element 0)
                      :shift (- 32 bits) :multiplier multiplier))

(declaim (inline recent-cache-index))
(defun recent-cache-index (cache value)
  "The slot VALUE hashes to."
  (ash (logand (* (rc-multiplier cache) value) #xffffffff) (- (rc-shift cache))))

(declaim (inline recent-cache-put recent-cache-ref))
(defun recent-cache-put (cache value)
  "Store VALUE in the slot it hashes to.  Idempotent for a repeated value."
  (setf (svref (rc-table cache) (recent-cache-index cache value)) value))

(defun recent-cache-ref (cache index)
  "The value currently in slot INDEX."
  (svref (rc-table cache) index))
