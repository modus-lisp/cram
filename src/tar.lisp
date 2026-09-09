;;;; tar.lisp — a POSIX ustar / GNU / PAX archive READER, in pure Common Lisp.
;;;;
;;;; ============================================================================================
;;;; WHY THIS IS IN CRAM
;;;; ============================================================================================
;;;;
;;;; cram already owns one half of `.tar.gz` and this is the other.  The alternative was to bury
;;;; a copy inside whichever consumer needed it first -- which is exactly how the stack ends up
;;;; with three archive readers that disagree about long file names.
;;;;
;;;; It is a READER only.  Nothing here writes an archive, because nothing has needed to; when
;;;; something does, the writer belongs beside it rather than inside the caller.
;;;;
;;;; ============================================================================================
;;;; PATHS OUT OF AN ARCHIVE ARE UNTRUSTED INPUT
;;;; ============================================================================================
;;;;
;;;; A tar entry's name is a string chosen by whoever built the archive.  `../../etc/cron.d/x` is
;;;; a perfectly well-formed ustar name, and an extractor that joins it to a destination directory
;;;; turns "unpack this file" into "write anywhere the process can".  For an archive fetched off
;;;; the network -- a package manager's tarball, say -- that is the difference between installing
;;;; a dependency and running one.
;;;;
;;;; So TAR-SAFE-NAME-P is not a helper a caller may remember to use: TAR-ENTRIES and TAR-EXTRACT
;;;; REFUSE an unsafe name by default and signal TAR-UNSAFE-NAME-ERROR naming it.  A caller that
;;;; genuinely wants the raw names (an archive inspector, a test) passes :ALLOW-UNSAFE-NAMES T and
;;;; is then visibly responsible for them.  Refusing by default costs a keyword argument; the
;;;; other way round costs whichever caller forgets.
;;;;
;;;; Rejected: an absolute path, any `..` component, a NUL, a `\` (a Windows separator that this
;;;; reader would otherwise treat as an ordinary name character, so `..\..\x` would sail through a
;;;; check that only looked at `/`), and the empty name.  A leading `./` is ordinary and kept.
;;;;
;;;; ============================================================================================
;;;; LONG NAMES ARE NOT OPTIONAL
;;;; ============================================================================================
;;;;
;;;; ustar stores a name in 100 bytes plus a 155-byte prefix.  Longer paths are carried out of
;;;; band, and BOTH schemes in the wild put the real name in a preceding pseudo-entry:
;;;;
;;;;   GNU  typeflag 'L' -- the next entry's name is this entry's CONTENT.
;;;;   PAX  typeflag 'x' -- the next entry's attributes are this entry's content, as a series of
;;;;                       "LEN key=value\n" records; `path` overrides the header name.
;;;;
;;;; A reader that skips those does not fail: it reports the PLACEHOLDER name (`././@LongLink`,
;;;; `PaxHeader/...`) and then the real entry with its name TRUNCATED to 100 bytes.  Silently
;;;; wrong names are the worst possible outcome for an installer, so both are implemented.

(in-package #:cram)

(define-condition tar-error (error)
  ((text :initarg :text :reader tar-error-text))
  (:report (lambda (c s) (write-string (tar-error-text c) s))))

(define-condition tar-unsafe-name-error (tar-error)
  ((name :initarg :name :reader tar-unsafe-name)))

(defconstant +tar-block+ 512)

(defun %tar-field (bytes off len)
  "The NUL-terminated (or LEN-bounded) ASCII field at OFF, as a fresh string."
  (let ((end off) (limit (min (length bytes) (+ off len))))
    (loop while (and (< end limit) (/= 0 (aref bytes end))) do (incf end))
    (let ((s (make-string (- end off))))
      (loop for i from off below end
            do (setf (char s (- i off)) (code-char (aref bytes i))))
      s)))

(defun %tar-octal (bytes off len)
  "The octal ASCII number at OFF.  Blanks and NULs terminate; an empty field is 0.

GNU base-256: a field whose first byte has the high bit set is a big-endian binary number, not
octal.  It is how sizes above 8 GB are carried, and reading one as octal yields nonsense rather
than an error, so it is handled rather than assumed absent."
  (if (and (< off (length bytes)) (logbitp 7 (aref bytes off)))
      (let ((n (logand (aref bytes off) #x7f)))
        (loop for i from (1+ off) below (min (length bytes) (+ off len))
              do (setf n (+ (ash n 8) (aref bytes i))))
        n)
      (let ((n 0))
        (loop for i from off below (min (length bytes) (+ off len))
              for b = (aref bytes i)
              while (<= 48 b 55)                     ; #\0 .. #\7
              do (setf n (+ (* n 8) (- b 48))))
        n)))

(defun %tar-zero-block-p (bytes off)
  (loop for i from off below (min (length bytes) (+ off +tar-block+))
        always (zerop (aref bytes i))))

(defun %tar-slice (bytes off len)
  (let* ((len (max 0 (min len (- (length bytes) off))))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (replace out bytes :start2 off :end2 (+ off len))
    out))

(defun %tar-checksum-ok-p (bytes off)
  "Verify the header checksum: the sum of all 512 header bytes with the checksum field itself
read as spaces.  Historic writers disagreed about signedness, so both readings are accepted."
  (let ((want (%tar-octal bytes (+ off 148) 8))
        (unsigned 0) (signed 0))
    (loop for i from 0 below +tar-block+
          for b = (if (<= 148 i 155) 32 (aref bytes (+ off i)))
          do (incf unsigned b)
             (incf signed (if (> b 127) (- b 256) b)))
    (or (= want unsigned) (= want signed))))

(defun tar-safe-name-p (name)
  "Can NAME be joined to a destination directory without escaping it?

See the header comment: this is the check that keeps `unpack a file` from becoming `write
anywhere`.  It is deliberately strict -- it answers about the NAME, not about any particular
filesystem, so it does not need to know where the caller intends to put it."
  (and (stringp name)
       (plusp (length name))
       (not (find #\Nul name))
       (not (find #\\ name))                          ; a Windows separator we would not split on
       (char/= (char name 0) #\/)                     ; absolute
       (let ((parts (loop with start = 0
                          for p = (position #\/ name :start start)
                          collect (subseq name start p)
                          while p do (setf start (1+ p)))))
         (notany (lambda (c) (string= c "..")) parts))
       t))

(defun %tar-pax-path (content)
  "The `path' record from a PAX extended header, or NIL.

Records are `LEN key=value\\n', where LEN counts its own digits.  The length prefix is what makes
a value containing a newline unambiguous, so it is parsed rather than split on newlines."
  (let ((i 0) (n (length content)) (result nil))
    (loop while (< i n)
          do (let ((sp (position 32 content :start i)))          ; the space after LEN
               (unless sp (return))
               (let ((len (let ((v 0))
                            (loop for k from i below sp
                                  for b = (aref content k)
                                  unless (<= 48 b 57) do (return-from %tar-pax-path result)
                                  do (setf v (+ (* v 10) (- b 48))))
                            v)))
                 (when (or (< len 1) (> (+ i len) n)) (return))
                 (let* ((body (%tar-field content (1+ sp) (- (+ i len) sp 2)))
                        (eq- (position #\= body)))
                   (when (and eq- (string= "path" (subseq body 0 eq-)))
                     (setf result (subseq body (1+ eq-)))))
                 (incf i len))))
    result))

(defun tar-do-entries (bytes fn &key allow-unsafe-names)
  "Walk the archive in BYTES, calling (FN NAME TYPEFLAG CONTENT) per entry.

CONTENT is a fresh byte vector for a regular file and NIL otherwise.  TYPEFLAG is the raw byte:
48 (`0') is a regular file, 53 (`5') a directory.  GNU long-name and PAX pseudo-entries are
consumed here and never reach FN -- they are framing, not members."
  (let ((off 0) (n (length bytes)) (zero-run 0)
        (pending-name nil))                            ; set by a preceding 'L' or 'x' entry
    (loop
      (when (> (+ off +tar-block+) n) (return))
      (cond
        ((%tar-zero-block-p bytes off)
         (incf zero-run) (incf off +tar-block+)
         (when (>= zero-run 2) (return)))              ; two zero blocks end the archive
        (t
         (setf zero-run 0)
         (unless (%tar-checksum-ok-p bytes off)
           (error 'tar-error :text (format nil "bad header checksum at offset ~d" off)))
         (let* ((name (%tar-field bytes off 100))
                (prefix (%tar-field bytes (+ off 345) 155))
                (size (%tar-octal bytes (+ off 124) 12))
                (tflag (aref bytes (+ off 156)))
                (data (+ off +tar-block+))
                (full (or pending-name
                          (if (plusp (length prefix))
                              (concatenate 'string prefix "/" name)
                              name))))
           (setf pending-name nil)
           (case tflag
             (76                                        ; 'L' -- GNU: the name is the content
              (setf pending-name (%tar-field (%tar-slice bytes data size) 0 size)))
             ((120 103)                                 ; 'x' / 'g' -- PAX extended header
              (let ((p (%tar-pax-path (%tar-slice bytes data size))))
                (when p (setf pending-name p))))
             (t
              (unless (or allow-unsafe-names (tar-safe-name-p full))
                (error 'tar-unsafe-name-error :name full
                       :text (format nil "refusing tar entry with an unsafe name: ~s~
~%  It would escape the destination directory.  Pass :ALLOW-UNSAFE-NAMES T only if you are ~
inspecting the archive rather than writing it out." full)))
              (if (or (= tflag 48) (= tflag 0))
                  (funcall fn full 48 (%tar-slice bytes data size))
                  (funcall fn full tflag nil))))
           (setf off (+ data (* +tar-block+ (ceiling size +tar-block+))))))))
    nil))

(defun tar-extract (bytes &key allow-unsafe-names)
  "Every REGULAR FILE in BYTES, as a list of (NAME . CONTENT).  Unsafe names are refused."
  (let ((acc '()))
    (tar-do-entries bytes
                    (lambda (name tflag content)
                      (when (and (= tflag 48) content) (push (cons name content) acc)))
                    :allow-unsafe-names allow-unsafe-names)
    (nreverse acc)))

(defun tar-list (bytes &key allow-unsafe-names)
  "Every entry name in BYTES, files and directories alike."
  (let ((acc '()))
    (tar-do-entries bytes (lambda (name tflag content) (declare (ignore tflag content))
                            (push name acc))
                    :allow-unsafe-names allow-unsafe-names)
    (nreverse acc)))
