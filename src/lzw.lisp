;;;; lzw.lisp — LZW decompression (the GIF variant).
;;;;
;;;; LZW is the other compression this stack meets.  DEFLATE covers PNG, zlib, HTTP
;;;; and RFB; LZW covers GIF, and TIFF, and PDF's LZWDecode filter.  It lives here
;;;; for the same reason inflate does: a decoder that lives inside the one format
;;;; that first needed it gets reimplemented by the next format that needs it.
;;;;
;;;; THE VARIANTS ARE NOT INTERCHANGEABLE, so this implements exactly one and says
;;;; which.  GIF packs codes LSB-FIRST and frames the stream in sub-blocks of at
;;;; most 255 bytes; TIFF and PDF pack MSB-first, have no sub-blocks, and bump the
;;;; code width one code EARLIER (the "early change" quirk).  A decoder that quietly
;;;; assumed the wrong one produces plausible garbage rather than an error, so the
;;;; variant is a required part of the contract and an unimplemented one signals.

(in-package #:cram)

(defun lzw-decode (bytes &key (start 0) (min-code-size 8) limit (variant :gif))
  "Decompress the LZW stream in BYTES beginning at START.

   MIN-CODE-SIZE is the initial code width less one, as GIF stores it before the
   data (8 for a full byte alphabet).  The clear code is 2^MIN-CODE-SIZE and the
   end code is one above it; the width grows by one each time the dictionary fills,
   to a ceiling of 12 bits, and resets on a clear code.

   LIMIT, when given, is the number of output bytes wanted — decoding stops there
   even if the stream continues, which is what a caller with a known pixel count
   wants and what keeps a corrupt stream from producing an unbounded vector.

   Returns (values OCTETS END-POSITION), END-POSITION being the index just past the
   stream's block terminator so the caller can carry on reading the container.

   VARIANT is :GIF and nothing else is implemented — see the header for why that is
   a signalled error rather than a best guess."
  (unless (eq variant :gif)
    (error "cram:lzw-decode: variant ~s is not implemented (only :GIF)." variant))
  (let* ((clear (ash 1 min-code-size))
         (end (1+ clear))
         (cap (or limit (* 16 (length bytes))))
         (out (make-array (min cap 16777216) :element-type '(unsigned-byte 8)
                                             :adjustable t :fill-pointer 0))
         ;; The dictionary as parallel PREFIX/SUFFIX arrays rather than a vector of
         ;; sequences: an entry is (prefix-code, one byte), and a string is recovered
         ;; by walking back through the prefixes.  4096 is the format's hard ceiling,
         ;; so none of this grows.
         (prefix (make-array 4096 :element-type 'fixnum :initial-element -1))
         (suffix (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
         (stack (make-array 4096 :element-type '(unsigned-byte 8)))
         (next (+ end 1))
         (width (1+ min-code-size))
         (prev -1)
         (acc 0) (bits 0)
         (pos start)
         (block-left 0))
    (dotimes (i clear) (setf (aref suffix i) i))
    (labels ((next-byte ()
               ;; Sub-block framing: a length byte, then that many data bytes, until
               ;; a zero-length block ends the stream.
               (loop
                 (when (plusp block-left)
                   (decf block-left)
                   (return (prog1 (aref bytes pos) (incf pos))))
                 (when (>= pos (length bytes)) (return nil))
                 (let ((n (aref bytes pos)))
                   (incf pos)
                   (when (zerop n) (return nil))
                   (setf block-left n))))
             (next-code ()
               (loop while (< bits width)
                     do (let ((b (next-byte)))
                          (when (null b) (return-from next-code nil))
                          ;; LSB-FIRST: the opposite of DEFLATE's Huffman packing in
                          ;; inflate.lisp, and the single most common way to get GIF
                          ;; wrong.
                          (setf acc (logior acc (ash b bits)))
                          (incf bits 8)))
               (prog1 (logand acc (1- (ash 1 width)))
                 (setf acc (ash acc (- width)))
                 (decf bits width))))
      (loop
        (let ((code (next-code)))
          (when (or (null code) (= code end)) (return))
          (cond
            ((= code clear)
             (setf next (+ end 1) width (1+ min-code-size) prev -1))
            (t
             (let ((sp 0) (cur code))
               (when (>= code next)
                 ;; The KwKwK case: a code for the entry currently being built.  Its
                 ;; expansion is the previous string followed by that string's FIRST
                 ;; byte — the one place where a decoder has to reason about an entry
                 ;; the encoder had and it does not yet.
                 (when (minusp prev) (return))
                 (setf (aref stack sp)
                       (let ((c prev))
                         (loop while (>= c clear) do (setf c (aref prefix c)))
                         (aref suffix c)))
                 (incf sp)
                 (setf cur prev))
               (loop while (>= cur clear)
                     do (setf (aref stack sp) (aref suffix cur))
                        (incf sp)
                        (setf cur (aref prefix cur)))
               (setf (aref stack sp) (aref suffix cur))
               (incf sp)
               ;; The walk produced the string backwards, so unwind it.
               (loop for i from (1- sp) downto 0
                     do (when (< (fill-pointer out) cap)
                          (vector-push-extend (aref stack i) out)))
               (when (and (>= prev 0) (< next 4096))
                 (setf (aref prefix next) prev
                       (aref suffix next) (aref stack (1- sp)))
                 (incf next)
                 ;; Width grows when the NEXT code to be assigned would not fit.
                 ;; TIFF/PDF grow one code sooner; see the header.
                 (when (and (= next (ash 1 width)) (< width 12))
                   (incf width)))
               (setf prev code))))
          (when (and limit (>= (fill-pointer out) limit)) (return))))
      ;; Drain to the terminator so END-POSITION really is past this stream.
      (loop while (next-byte))
      (values (coerce out '(simple-array (unsigned-byte 8) (*))) pos))))
