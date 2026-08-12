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

;;; ---- LZ77 -> tokens ---------------------------------------------------------

(defconstant +window+ 32768)
(defconstant +min-match+ 3)
(defconstant +max-match+ 258)
(declaim (inline hash3))
(defun hash3 (data i)
  (logand (logxor (ash (aref data i) 8) (ash (aref data (+ i 1)) 4) (aref data (+ i 2))) #xffff))

(defun lz77 (data start end head prev max-chain)
  "Greedy LZ77 of DATA[START:END] -> a vector of tokens: a byte (0..255) for a
   literal, or (len . dist) for a match.  HEAD/PREV persist across calls, so a
   streamed block can reference data from an earlier flush (within 32 KB)."
  (let ((tokens (make-array 256 :adjustable t :fill-pointer 0)) (pos start) (wmask (1- +window+)))
    (labels ((mlen (a b)
               (let ((l 0) (lim (min +max-match+ (- end b))))
                 (loop while (and (< l lim) (= (aref data (+ a l)) (aref data (+ b l)))) do (incf l)) l))
             (insert (i)
               (let ((h (hash3 data i)))
                 (setf (aref prev (logand i wmask)) (aref head h) (aref head h) i))))
      (loop while (< pos end) do
        (if (<= (+ pos +min-match+) end)
            (let ((best 0) (bp -1) (cand (aref head (hash3 data pos))) (chain 0))
              (loop while (and (>= cand 0) (<= (- pos cand) +window+) (< chain max-chain)) do
                (let ((l (mlen cand pos)))
                  (when (> l best) (setf best l bp cand))
                  (when (>= best +max-match+) (return)))
                (setf cand (aref prev (logand cand wmask)) chain (1+ chain)))
              (insert pos)
              (if (>= best +min-match+)
                  (progn (vector-push-extend (cons best (- pos bp)) tokens)
                         (loop for k from 1 below best while (<= (+ pos k +min-match+) end) do (insert (+ pos k)))
                         (incf pos best))
                  (progn (vector-push-extend (aref data pos) tokens) (incf pos))))
            (progn (vector-push-extend (aref data pos) tokens) (incf pos)))))
    tokens))

;;; ---- length-limited Huffman (boundary package-merge) ------------------------

(defun huffman-lengths (freq n maxbits)
  "Optimal code lengths (each <= MAXBITS) for symbols 0..N-1 with counts FREQ.
   Unused symbols get length 0."
  (let ((lengths (make-array n :initial-element 0)) (used '()))
    (dotimes (i n) (when (plusp (aref freq i)) (push i used)))
    (setf used (sort used (lambda (a b) (< (aref freq a) (aref freq b)))))
    (let ((m (length used)))
      (cond
        ((zerop m) lengths)
        ((= m 1) (setf (aref lengths (first used)) 1) lengths)   ; a lone symbol: 1 bit
        (t (let* ((orig (mapcar (lambda (i) (cons (aref freq i) (list i))) used))
                  (packages orig))
             (dotimes (level (1- maxbits))                       ; build up to MAXBITS levels
               (let ((paired '()) (p packages))
                 (loop while (and p (cdr p)) do
                   (push (cons (+ (car (first p)) (car (second p)))
                               (append (cdr (first p)) (cdr (second p)))) paired)
                   (setf p (cddr p)))
                 (setf packages (merge 'list (copy-list orig) (nreverse paired) #'< :key #'car))))
             (let ((take (- (* 2 m) 2)) (i 0))                   ; first 2m-2 packages
               (dolist (pk packages)
                 (when (>= i take) (return))
                 (dolist (sym (cdr pk)) (incf (aref lengths sym)))
                 (incf i)))
             lengths))))))

;;; ---- token frequencies / sizing / emission ----------------------------------

(defun count-freqs (tokens)
  (let ((lit (make-array 286 :initial-element 0)) (dist (make-array 30 :initial-element 0)))
    (loop for tok across tokens do
      (if (integerp tok) (incf (aref lit tok))
          (progn (incf (aref lit (+ 257 (aref *len-sym* (car tok)))))
                 (incf (aref dist (dist-index (cdr tok)))))))
    (incf (aref lit 256))                                        ; end-of-block
    (values lit dist)))

(defun token-bits (tokens lit-len dist-len)
  (let ((bits (aref lit-len 256)))
    (loop for tok across tokens do
      (if (integerp tok) (incf bits (aref lit-len tok))
          (let ((li (aref *len-sym* (car tok))) (di (dist-index (cdr tok))))
            (incf bits (+ (aref lit-len (+ 257 li)) (aref *len-extra* li)
                          (aref dist-len di) (aref *dist-extra* di))))))
    bits))

(defun emit-tokens (bw tokens lit-code lit-len dist-code dist-len)
  (loop for tok across tokens do
    (if (integerp tok) (bw-huff bw (aref lit-code tok) (aref lit-len tok))
        (let ((li (aref *len-sym* (car tok))) (di (dist-index (cdr tok))))
          (bw-huff bw (aref lit-code (+ 257 li)) (aref lit-len (+ 257 li)))
          (when (plusp (aref *len-extra* li)) (bw-put bw (- (car tok) (aref *len-base* li)) (aref *len-extra* li)))
          (bw-huff bw (aref dist-code di) (aref dist-len di))
          (when (plusp (aref *dist-extra* di)) (bw-put bw (- (cdr tok) (aref *dist-base* di)) (aref *dist-extra* di))))))
  (bw-huff bw (aref lit-code 256) (aref lit-len 256)))          ; end-of-block

(defun rle-code-lengths (cl)
  "Run-length encode the code-length sequence CL into (symbol . extra) tokens
   (symbols 0-15 = a length; 16 = repeat prev 3-6; 17 = zero 3-10; 18 = zero 11-138)."
  (let ((out '()) (i 0) (n (length cl)))
    (loop while (< i n) do
      (let ((v (aref cl i)) (run 1))
        (loop while (and (< (+ i run) n) (= (aref cl (+ i run)) v)) do (incf run))
        (cond
          ((zerop v)
           (loop while (>= run 11) do (let ((r (min run 138))) (push (cons 18 (- r 11)) out) (incf i r) (decf run r)))
           (loop while (>= run 3)  do (let ((r (min run 10)))  (push (cons 17 (- r 3))  out) (incf i r) (decf run r)))
           (dotimes (k run) (push (cons 0 0) out) (incf i)))
          (t (push (cons v 0) out) (incf i) (decf run)          ; the length itself
             (loop while (>= run 3) do (let ((r (min run 6))) (push (cons 16 (- r 3)) out) (incf i r) (decf run r)))
             (dotimes (k run) (push (cons v 0) out) (incf i))))))
    (nreverse out)))

(defun bw-append (bw data start end)
  "Bulk-append the byte-aligned raw bytes DATA[start:end] to BW's output (one
   REPLACE, not a per-byte push) — so a stored block is a memcpy, not a loop."
  (let* ((o (bitw-out bw)) (p (fill-pointer o)) (need (+ p (- end start))))
    (when (> need (array-dimension o 0)) (adjust-array o (max need (* 2 (array-dimension o 0)))))
    (setf (fill-pointer o) need)
    (replace o data :start1 p :start2 start :end2 end)))

(defun emit-stored (bw data start end final)
  "One stored (BTYPE=00) DEFLATE block for DATA[start:end].  END-START must be
   <= 65535 (the stored length is 16-bit); use EMIT-STORED* for larger ranges."
  (bw-put bw (if final 1 0) 1) (bw-put bw 0 2)                  ; BFINAL, BTYPE=00
  (bw-align bw)
  (let ((len (- end start)))
    (vector-push-extend (logand len #xff) (bitw-out bw))
    (vector-push-extend (logand (ash len -8) #xff) (bitw-out bw))
    (vector-push-extend (logand (lognot len) #xff) (bitw-out bw))
    (vector-push-extend (logand (ash (lognot len) -8) #xff) (bitw-out bw))
    (bw-append bw data start end)))

(defun emit-stored* (bw data start end final)
  "DATA[start:end] as one or more stored blocks, each <= 65535 bytes; only the
   last carries FINAL.  No LZ77 — a big/incompressible range costs a copy."
  (if (= start end)
      (emit-stored bw data start end final)                    ; empty (final) block
      (loop for s from start below end by 65535
            for e = (min end (+ s 65535))
            do (emit-stored bw data s e (and final (= e end))))))

(defun emit-block (bw data start end tokens final)
  "Emit DATA[START:END] as the smallest of a stored / fixed-Huffman /
   dynamic-Huffman block for TOKENS."
  (multiple-value-bind (lit-freq dist-freq) (count-freqs tokens)
    (let ((lit-lengths (huffman-lengths lit-freq 286 15))
          (dist-lengths (huffman-lengths dist-freq 30 15)))
      (when (every #'zerop dist-lengths) (setf (aref dist-lengths 0) 1))  ; keep a valid dist tree
      (let* ((hlit (max 257 (1+ (or (position-if #'plusp lit-lengths :from-end t) 256))))
             (hdist (max 1 (1+ (or (position-if #'plusp dist-lengths :from-end t) 0))))
             (cl (concatenate 'vector (subseq lit-lengths 0 hlit) (subseq dist-lengths 0 hdist)))
             (rle (rle-code-lengths cl))
             (cl-freq (make-array 19 :initial-element 0)))
        (dolist (tk rle) (incf (aref cl-freq (car tk))))
        (let* ((cl-lengths (huffman-lengths cl-freq 19 7))
               (hclen 4))
          (loop for i from 18 downto 4 do
            (when (plusp (aref cl-lengths (aref *clen-order* i))) (setf hclen (1+ i)) (return)))
          (let* ((dyn-hdr (+ 3 5 5 4 (* 3 hclen)
                             (loop for tk in rle sum (+ (aref cl-lengths (car tk))
                                                        (case (car tk) (16 2) (17 3) (18 7) (t 0))))))
                 (dyn-bits   (+ dyn-hdr (token-bits tokens lit-lengths dist-lengths)))
                 (fixed-bits (+ 3 (token-bits tokens *lit-len* *dist-len*)))
                 (stored-bits (* 8 (+ 5 (- end start)))))
            (cond
              ((and (<= stored-bits dyn-bits) (<= stored-bits fixed-bits))
               ;; EMIT-STORED*, not EMIT-STORED: a whole one-shot block can exceed
               ;; the 16-bit stored length, and incompressible input is exactly
               ;; when this branch wins.  Splitting is the splitter's job.
               (emit-stored* bw data start end final))
              ((<= fixed-bits dyn-bits)
               (bw-put bw (if final 1 0) 1) (bw-put bw 1 2)     ; fixed
               (emit-tokens bw tokens *lit-code* *lit-len* *dist-code* *dist-len*))
              (t
               (bw-put bw (if final 1 0) 1) (bw-put bw 2 2)     ; dynamic
               (bw-put bw (- hlit 257) 5) (bw-put bw (- hdist 1) 5) (bw-put bw (- hclen 4) 4)
               (dotimes (i hclen) (bw-put bw (aref cl-lengths (aref *clen-order* i)) 3))
               (let ((cl-code (canonical-codes cl-lengths)))
                 (dolist (tk rle)
                   (bw-huff bw (aref cl-code (car tk)) (aref cl-lengths (car tk)))
                   (case (car tk) (16 (bw-put bw (cdr tk) 2)) (17 (bw-put bw (cdr tk) 3)) (18 (bw-put bw (cdr tk) 7)))))
               (emit-tokens bw tokens (canonical-codes lit-lengths) lit-lengths
                            (canonical-codes dist-lengths) dist-lengths)))))))))

(defun deflate-block (bw data start end final head prev max-chain)
  "Emit DATA[START:END] as one DEFLATE block (best of stored/fixed/dynamic).
   HEAD/PREV are the persistent hash tables (matches can reach earlier flushes)."
  (emit-block bw data start end (lz77 data start end head prev max-chain) final))

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

;;; ---- crc-32 (RFC 1952) ------------------------------------------------------
;;; gzip's integrity check, not zlib's.  Table built once on first use rather
;;; than written out as a literal.

(defvar *crc32-table*
  (let ((tbl (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 tbl)
      (let ((c n))
        (dotimes (k 8) (setf c (if (logtest c 1) (logxor #xedb88320 (ash c -1)) (ash c -1))))
        (setf (aref tbl n) c)))))

(defun crc32 (data &optional (start 0) (end (length data)))
  (let ((c #xffffffff) (tbl *crc32-table*))
    (loop for i from start below end
          do (setf c (logxor (aref tbl (logand (logxor c (aref data i)) #xff)) (ash c -8))))
    (logxor c #xffffffff)))

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

(defun compress (zs bytes &key (start 0) (end (length bytes)))
  "Feed BYTES[START,END) into the stream (buffered until the next SYNC-FLUSH /
   FINISH).  Bulk-copied (not pushed byte by byte) so feeding a big buffer is fast."
  (let* ((d (zstream-data zs)) (s (fill-pointer d)) (n (- end start)) (need (+ s n)))
    (when (> need (array-dimension d 0))
      (adjust-array d (max need (* 2 (array-dimension d 0)))))
    (setf (fill-pointer d) need)
    (replace d bytes :start1 s :start2 start :end2 end)
    (multiple-value-bind (a b) (adler-step d s need (zstream-a zs) (zstream-b zs))
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

(defun sync-flush-stored (zs)
  "Like SYNC-FLUSH but emits the pending data as STORED blocks — no LZ77 match
   search, so a big/incompressible flush costs a bulk copy + adler, not a deep
   deflate (~order-of-magnitude less CPU).  Still valid zlib: a client decodes it
   as ordinary ZRLE.  Trades compression ratio for encode speed — use when the
   link has bandwidth to spare (LAN) and the frame is large/incompressible."
  (let ((zw (zstream-bw zs)) (d (zstream-data zs)))
    (%maybe-header zs)
    (emit-stored* zw d (zstream-processed zs) (fill-pointer d) nil)
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
