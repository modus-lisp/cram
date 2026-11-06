;;;; inflate-stream.lisp — DEFLATE / zlib decompression that never holds the stream whole.
;;;;
;;;; ZLIB-DECOMPRESS takes its input as one vector and returns its output as another, so
;;;; inflating a 10 MB object costs the compressed bytes, the output, the output's growth
;;;; copies and a final copy, all at once.  ZLIB-INFLATE-STREAM pulls its input through a
;;;; READ function and hands its output to a SINK a piece at a time; between them it holds
;;;; one input buffer and an output buffer of 96 KB, the last 32 KB of which is the history
;;;; a back-reference can reach (DEFLATE's distances stop at 32768).  The Huffman decoding is
;;;; inflate.lisp's, unchanged; only where the bytes come from and go to differs.

(in-package #:cram)

(defconstant +history+ 32768 "How far back a DEFLATE match can reach.")
(defconstant +out-cap+ (+ 65536 +history+)
  "The output buffer: 64 KB written between flushes, plus the history kept across one.")

(defstruct (iout (:constructor %make-iout (sink)))
  (buf (make-array +out-cap+ :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (o 0 :type fixnum)                    ; next free index in BUF
  (flushed 0 :type fixnum)              ; BUF[0..FLUSHED) has gone to SINK already
  sink
  (total 0)                             ; octets handed to SINK
  (a 1) (b 0))                          ; adler-32 of everything handed to SINK

(defun iout-flush (out)
  "Hand BUF[FLUSHED..O) to the sink, adding it to the running adler-32."
  (let ((buf (iout-buf out)) (f (iout-flushed out)) (o (iout-o out)))
    (when (> o f)
      (multiple-value-bind (a b) (adler-step buf f o (iout-a out) (iout-b out))
        (setf (iout-a out) a (iout-b out) b))
      (funcall (iout-sink out) buf f o)
      (incf (iout-total out) (- o f))
      (setf (iout-flushed out) o))))

(defun iout-slide (out)
  "BUF is full: flush it, then keep only its last +HISTORY+ octets, at the front."
  (iout-flush out)
  (let ((buf (iout-buf out)) (o (iout-o out)))
    (replace buf buf :start1 0 :start2 (- o +history+) :end2 o)
    (setf (iout-o out) +history+ (iout-flushed out) +history+)))

(declaim (inline iout-put))
(defun iout-put (out byte)
  (when (= (iout-o out) +out-cap+) (iout-slide out))
  (setf (aref (iout-buf out) (iout-o out)) byte)
  (incf (iout-o out)))

(defun iout-copy (out dist len)
  "Append LEN octets copied from DIST back -- a match, which may overlap itself."
  (let ((buf (iout-buf out)))
    (when (> dist (min (iout-o out) +history+))
      (error "cram: back-reference ~d is past the start of the output" dist))
    (dotimes (k len)
      (when (= (iout-o out) +out-cap+) (iout-slide out))
      (let ((o (iout-o out)))
        (setf (aref buf o) (aref buf (- o dist)))
        (setf (iout-o out) (+ o 1))))))

(defun inflate-huffman-stream (br out lit dist)
  (loop
    (let ((sym (decode-sym br lit)))
      (cond
        ((< sym 256) (iout-put out sym))
        ((= sym 256) (return))
        (t (let* ((li (- sym 257))
                  (len (+ (aref *len-base* li) (br-bits br (aref *len-extra* li))))
                  (ds (decode-sym br dist))
                  (d (+ (aref *dist-base* ds) (br-bits br (aref *dist-extra* ds)))))
             (iout-copy out d len)))))))

(defun inflate-blocks-stream (br out)
  "Inflate all blocks (until BFINAL) into OUT."
  (loop
    (let ((bfinal (br-bit br)) (btype (br-bits br 2)))
      (ecase btype
        (0 (br-align br)
           (let ((len (br-bits br 16)))
             (br-bits br 16)                              ; NLEN (ignored)
             (dotimes (i len) (iout-put out (br-bits br 8)))))
        (1 (inflate-huffman-stream br out *fixed-lit-huff* *fixed-dist-huff*))
        (2 (multiple-value-bind (lh dh) (read-dynamic-tables br)
             (inflate-huffman-stream br out lh dh))))
      (when (= bfinal 1) (return)))))

(defun %zlib-inflate-br (br sink verify)
  "Inflate the zlib stream BR is at the start of into SINK.  Returns the output length."
  (let ((out (%make-iout sink)))
    (let ((cmf (br-bits br 8)) (flg (br-bits br 8)))
      (unless (zerop (mod (+ (* cmf 256) flg) 31)) (error "cram: bad zlib header"))
      (when (logtest flg 32) (error "cram: preset dictionary not supported")))
    (inflate-blocks-stream br out)
    (iout-flush out)
    (br-align br)
    (let ((want (logior (ash (br-bits br 8) 24) (ash (br-bits br 8) 16)
                        (ash (br-bits br 8) 8) (br-bits br 8))))
      (when (and verify (/= want (logior (ash (iout-b out) 16) (iout-a out))))
        (error "cram: adler-32 mismatch")))
    (iout-total out)))

(defun zlib-inflate-stream (read sink &key (verify t) (buffer-size 16384))
  "Inflate one zlib stream (RFC 1950) pulled through READ, handing the output to SINK.
   (READ BUF START END) puts the next input octets in BUF[START..END) and returns how many,
   0 at the end of the input; (SINK BUF START END) takes the next output octets, and must use
   them before it returns, since BUF is reused.  READ may be asked for more than the stream
   holds: what follows it is read but not used.  VERIFY NIL skips the adler-32 check.
   Returns (values OUTPUT-OCTETS INPUT-OCTETS), the second being the length of the zlib stream
   itself, header and adler-32 included -- where the next thing in the input starts."
  (let* ((inbuf (make-array buffer-size :element-type '(unsigned-byte 8)))
         (base 0)                       ; input octets in buffers before the current one
         (br (%make-bitr :data inbuf :pos 0 :end 0)))
    (setf (bitr-refill br)
          (lambda (br)
            (incf base (bitr-end br))
            (let ((n (funcall read inbuf 0 (length inbuf))))
              (setf (bitr-pos br) 0 (bitr-end br) n)
              (plusp n))))
    (let ((total (%zlib-inflate-br br sink verify)))
      (values total (+ base (br-consumed br))))))

(defun zlib-inflate-to (data sink &key (start 0) end (verify t))
  "ZLIB-INFLATE-STREAM of the zlib stream at DATA[START..END), DATA an octet vector already in
   memory, read in place: only the output goes through a buffer, to SINK a piece at a time.
   Returns (values OUTPUT-OCTETS INPUT-OCTETS)."
  (let* ((v (%ub8v data))
         (br (%make-bitr :data v :pos start :end (or end (length v)))))
    (let ((total (%zlib-inflate-br br sink verify)))
      (values total (- (br-consumed br) start)))))
