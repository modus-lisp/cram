;;;; deflate.lisp — DEFLATE (RFC 1951) + zlib (RFC 1950), with SYNC-FLUSH.
;;;;
;;;; Fixed-Huffman blocks + greedy LZ77 (hash-chain match finder over a 32 KB
;;;; window).  The reason cram exists is the persistent ZSTREAM: feed data, then
;;;; SYNC-FLUSH per message — the current block is closed and an empty stored
;;;; block is emitted (Z_SYNC_FLUSH) so the decoder can process everything so far,
;;;; while the LZ77 window carries over.  That's what RFB ZRLE/Tight need and what
;;;; salza2 doesn't expose.  Output is standard zlib, decodable by any inflate.

(in-package #:cram)

;;; ---- bit writer (DEFLATE packs bits LSB-first; Huffman codes MSB-first) -----

(defstruct (bitw (:constructor %make-bitw))
  (out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (acc 0) (n 0))

(defun bw-put (bw val nbits)
  "Write the low NBITS of VAL, least-significant bit first."
  (setf (bitw-acc bw) (logior (bitw-acc bw) (ash (logand val (1- (ash 1 nbits))) (bitw-n bw))))
  (incf (bitw-n bw) nbits)
  (loop while (>= (bitw-n bw) 8) do
    (vector-push-extend (logand (bitw-acc bw) #xff) (bitw-out bw))
    (setf (bitw-acc bw) (ash (bitw-acc bw) -8))
    (decf (bitw-n bw) 8)))

(defun rev-bits (v n)
  (let ((r 0)) (dotimes (i n r) (setf r (logior (ash r 1) (logand v 1)) v (ash v -1)))))

(defun bw-huff (bw code len) (bw-put bw (rev-bits code len) len))

(defun bw-align (bw)
  "Pad to a byte boundary with zero bits."
  (when (plusp (bitw-n bw))
    (vector-push-extend (logand (bitw-acc bw) #xff) (bitw-out bw))
    (setf (bitw-acc bw) 0 (bitw-n bw) 0)))

(defun bw-take (bw)
  "The bytes emitted so far (must be byte-aligned); clears the output buffer."
  (let ((v (make-array (fill-pointer (bitw-out bw)) :element-type '(unsigned-byte 8))))
    (replace v (bitw-out bw))
    (setf (fill-pointer (bitw-out bw)) 0)
    v))

;;; ---- fixed Huffman + length/distance tables (RFC 1951 §3.2) -----------------

(defun canonical-codes (lengths)
  "Assign canonical Huffman codes for the given per-symbol bit LENGTHS."
  (let* ((n (length lengths)) (maxlen (reduce #'max lengths))
         (bl (make-array (1+ maxlen) :initial-element 0))
         (next (make-array (1+ maxlen) :initial-element 0))
         (codes (make-array n :initial-element 0)) (code 0))
    (loop for l across lengths do (when (plusp l) (incf (aref bl l))))
    (loop for bits from 1 to maxlen do
      (setf code (ash (+ code (aref bl (1- bits))) 1) (aref next bits) code))
    (dotimes (i n codes)
      (let ((l (aref lengths i)))
        (when (plusp l) (setf (aref codes i) (aref next l)) (incf (aref next l)))))))

(defparameter *lit-len*
  (let ((a (make-array 288)))
    (loop for i from 0 to 143 do (setf (aref a i) 8))
    (loop for i from 144 to 255 do (setf (aref a i) 9))
    (loop for i from 256 to 279 do (setf (aref a i) 7))
    (loop for i from 280 to 287 do (setf (aref a i) 8))
    a))
(defparameter *lit-code* (canonical-codes *lit-len*))
(defparameter *dist-len* (make-array 30 :initial-element 5))
(defparameter *dist-code* (canonical-codes *dist-len*))

(defparameter *len-base* #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *len-extra* #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *dist-base* #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter *dist-extra* #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defparameter *len-sym*                 ; length (3..258) -> index into *len-base*
  (let ((a (make-array 259)))
    (loop for i from 0 to 28 do
      (loop for l from (aref *len-base* i) to (if (= i 28) 258 (1- (aref *len-base* (1+ i)))) do
        (setf (aref a l) i)))
    a))

(defun dist-index (d)
  (let ((i 0)) (loop while (and (< i 29) (>= d (aref *dist-base* (1+ i)))) do (incf i)) i))

(defun emit-literal (bw b) (bw-huff bw (aref *lit-code* b) (aref *lit-len* b)))

(defun emit-match (bw len dist)
  (let ((i (aref *len-sym* len)))
    (bw-huff bw (aref *lit-code* (+ 257 i)) (aref *lit-len* (+ 257 i)))
    (when (plusp (aref *len-extra* i)) (bw-put bw (- len (aref *len-base* i)) (aref *len-extra* i))))
  (let ((i (dist-index dist)))
    (bw-huff bw (aref *dist-code* i) (aref *dist-len* i))
    (when (plusp (aref *dist-extra* i)) (bw-put bw (- dist (aref *dist-base* i)) (aref *dist-extra* i)))))

;;; ---- LZ77 + block emission --------------------------------------------------

(defconstant +window+ 32768)
(defconstant +min-match+ 3)
(defconstant +max-match+ 258)
(declaim (inline hash3))
(defun hash3 (data i)
  (logand (logxor (ash (aref data i) 8) (ash (aref data (+ i 1)) 4) (aref data (+ i 2))) #xffff))

(defun deflate-block (bw data start end final head prev max-chain)
  "Emit a fixed-Huffman block for DATA[START:END].  HEAD/PREV are the persistent
   hash-chain tables (matches may reach back to earlier positions within the 32 KB
   window, so a streamed block can reference data from earlier flushes)."
  (bw-put bw (if final 1 0) 1) (bw-put bw 1 2)          ; BFINAL, BTYPE=01 (fixed)
  (let ((pos start) (wmask (1- +window+)))
    (labels ((mlen (a b)
               (let ((l 0) (lim (min +max-match+ (- end b))))
                 (loop while (and (< l lim) (= (aref data (+ a l)) (aref data (+ b l)))) do (incf l))
                 l))
             (insert (i)
               (let ((h (hash3 data i)))
                 (setf (aref prev (logand i wmask)) (aref head h) (aref head h) i))))
      (loop while (< pos end) do
        (if (<= (+ pos +min-match+) end)
            (let ((h (hash3 data pos)) (best 0) (bestpos -1) (cand -1) (chain 0))
              (setf cand (aref head h))
              (loop while (and (>= cand 0) (<= (- pos cand) +window+) (< chain max-chain)) do
                (let ((l (mlen cand pos)))
                  (when (> l best) (setf best l bestpos cand))
                  (when (>= best +max-match+) (return)))
                (setf cand (aref prev (logand cand wmask)) chain (1+ chain)))
              (insert pos)
              (if (>= best +min-match+)
                  (progn
                    (emit-match bw best (- pos bestpos))
                    (loop for k from 1 below best while (<= (+ pos k +min-match+) end)
                          do (insert (+ pos k)))
                    (incf pos best))
                  (progn (emit-literal bw (aref data pos)) (incf pos))))
            (progn (emit-literal bw (aref data pos)) (incf pos))))))
  (bw-huff bw (aref *lit-code* 256) (aref *lit-len* 256)))    ; end-of-block

(defun emit-sync-marker (bw)
  "Z_SYNC_FLUSH: an empty stored block (byte-aligns; decoder flushes so far)."
  (bw-put bw 0 1) (bw-put bw 0 2)                        ; BFINAL=0, BTYPE=00 (stored)
  (bw-align bw)
  (dolist (b '(0 0 #xff #xff)) (vector-push-extend b (bitw-out bw))))

;;; ---- adler-32 (RFC 1950) ----------------------------------------------------

(defun adler-step (data start end a b)
  (loop for i from start below end do
    (setf a (mod (+ a (aref data i)) 65521) b (mod (+ b a) 65521)))
  (values a b))

(defun adler32 (data &optional (start 0) (end (length data)))
  (multiple-value-bind (a b) (adler-step data start end 1 0) (logior (ash b 16) a)))

;;; ---- one-shot ---------------------------------------------------------------

(defun deflate-compress (data &key (max-chain 128))
  "Raw DEFLATE (RFC 1951) bytes for DATA."
  (let ((bw (%make-bitw))
        (head (make-array 65536 :element-type 'fixnum :initial-element -1))
        (prev (make-array +window+ :element-type 'fixnum :initial-element -1)))
    (deflate-block bw data 0 (length data) t head prev max-chain)
    (bw-align bw)
    (bw-take bw)))

(defun zlib-compress (data &key (max-chain 128))
  "A complete zlib stream (RFC 1950: header + DEFLATE + adler-32) for DATA."
  (let ((deflated (deflate-compress data :max-chain max-chain))
        (adler (adler32 data)))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 #(#x78 #x01) deflated
                 (vector (logand (ash adler -24) #xff) (logand (ash adler -16) #xff)
                         (logand (ash adler -8) #xff) (logand adler #xff)))))

;;; ---- persistent stream (make -> compress* -> sync-flush* [-> finish]) -------

(defstruct (zstream (:constructor %make-zstream))
  (data (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (head (make-array 65536 :element-type 'fixnum :initial-element -1))
  (prev (make-array +window+ :element-type 'fixnum :initial-element -1))
  (bw (%make-bitw)) (processed 0) (a 1) (b 0) (header nil) (max-chain 128))

(defun make-zstream (&key (max-chain 128)) (%make-zstream :max-chain max-chain))

(defun compress (zs bytes)
  "Feed BYTES into the stream (buffered until the next SYNC-FLUSH / FINISH)."
  (let ((d (zstream-data zs)) (s (fill-pointer (zstream-data zs))))
    (loop for x across bytes do (vector-push-extend x d))
    (multiple-value-bind (a b) (adler-step d s (fill-pointer d) (zstream-a zs) (zstream-b zs))
      (setf (zstream-a zs) a (zstream-b zs) b)))
  (values))

(defun %maybe-header (zs)
  (unless (zstream-header zs)
    (vector-push-extend #x78 (bitw-out (zstream-bw zs)))
    (vector-push-extend #x01 (bitw-out (zstream-bw zs)))
    (setf (zstream-header zs) t)))

(defun sync-flush (zs)
  "Compress everything fed since the last flush as a (non-final) block, then emit
   a Z_SYNC_FLUSH marker.  Returns the bytes produced (prefix them with a length
   for RFB).  The zlib header prefixes the first flush's output."
  (let ((zw (zstream-bw zs)) (d (zstream-data zs)))
    (%maybe-header zs)
    (deflate-block zw d (zstream-processed zs) (fill-pointer d) nil
                   (zstream-head zs) (zstream-prev zs) (zstream-max-chain zs))
    (setf (zstream-processed zs) (fill-pointer d))
    (emit-sync-marker zw)
    (bw-take zw)))

(defun finish (zs)
  "Close the stream: a final block over any un-flushed data + the adler-32.
   Returns the remaining bytes."
  (let ((zw (zstream-bw zs)) (d (zstream-data zs)))
    (%maybe-header zs)
    (deflate-block zw d (zstream-processed zs) (fill-pointer d) t
                   (zstream-head zs) (zstream-prev zs) (zstream-max-chain zs))
    (setf (zstream-processed zs) (fill-pointer d))
    (bw-align zw)
    (let ((adler (logior (ash (zstream-b zs) 16) (zstream-a zs))))
      (dolist (sh '(24 16 8 0)) (vector-push-extend (logand (ash adler (- sh)) #xff) (bitw-out zw))))
    (bw-take zw)))

(defun reset (zs)
  "Reset the stream to a fresh state (new zlib stream)."
  (setf (fill-pointer (zstream-data zs)) 0 (zstream-processed zs) 0
        (zstream-a zs) 1 (zstream-b zs) 0 (zstream-header zs) nil)
  (fill (zstream-head zs) -1) (fill (zstream-prev zs) -1)
  (setf (fill-pointer (bitw-out (zstream-bw zs))) 0
        (bitw-acc (zstream-bw zs)) 0 (bitw-n (zstream-bw zs)) 0)
  zs)
