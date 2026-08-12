(require :asdf)
(asdf:load-system :cram)
(asdf:load-system :salza2)
(asdf:load-system :chipz)

(defun file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun salza2-zlib (data)
  (salza2:compress-data data 'salza2:zlib-compressor))

(format t "~&~40a ~8@a ~8@a ~8@a~%" "file" "raw" "cram" "salza2")
(let* ((ws (merge-pathnames "../../" (make-pathname :name nil :type nil
                                                    :defaults *load-truename*)))
       (tc 0) (ts 0) (tr 0))
  (dolist (path (append
                 (directory (merge-pathnames "cram/src/*.lisp" ws))
                 (directory (merge-pathnames "cairn/src/*.lisp" ws))
                 (list #p"/usr/share/dict/words")))
    (when (probe-file path)
      (let* ((data (file-bytes path))
             (c (cram:zlib-compress data))
             (s (salza2-zlib data))
             (back (chipz:decompress nil 'chipz:zlib (coerce c '(simple-array (unsigned-byte 8) (*))))))
        (incf tr (length data)) (incf tc (length c)) (incf ts (length s))
        (format t "~40a ~8d ~8d ~8d ~a~%"
                (file-namestring path) (length data) (length c) (length s)
                (if (equalp back data) "" "  !!MISMATCH")))))
  (format t "~%~40a ~8d ~8d ~8d~%" "TOTAL" tr tc ts)
  (format t "cram/salza2 = ~,1f%   cram/raw = ~,1f%~%"
          (* 100.0 (/ tc ts)) (* 100.0 (/ tc tr))))
