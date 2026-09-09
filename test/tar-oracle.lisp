;;;; tar-oracle.lisp — grade cram's tar reader against GNU tar, on archives GNU tar built.
;;;;
;;;; A reader is only as good as the archives it has met, and the ones that break naive readers
;;;; are not exotic: a path over 100 characters, a path over 255, and a name that tries to leave
;;;; the destination directory.  So the fixtures are BUILT BY `tar` here rather than hand-rolled,
;;;; in both of the formats it can emit (--format=gnu and --format=pax), and graded against `tar`'s
;;;; own listing.  Hand-rolled fixtures would only ever test the reader against my own beliefs
;;;; about the format.

(in-package #:cram/test)

(defvar *tar-tmp* #p"/tmp/cram-tar-oracle/")

(defun sh (fmt &rest args)
  "Run a shell command, returning its stdout as a string."
  (with-output-to-string (out)
    (sb-ext:run-program "/bin/sh" (list "-c" (apply #'format nil fmt args))
                        :output out :error nil :search nil)))

(defun slurp-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun tar-names (bytes &rest args)
  (sort (mapcar #'car (apply #'cram:tar-extract bytes args)) #'string<))

(defun run-tar-tests ()
  (format t "~&[tar — graded against GNU tar]~%")
  (let* ((dir (namestring *tar-tmp*)))
    (sh "rm -rf ~a && mkdir -p ~a/src/deep/nested" dir dir)
    ;; A short name, a name that overflows ustar's 100-byte field, and one that overflows the
    ;; 100+155 prefix split as well -- the two thresholds where a reader starts truncating.
    (let ((long (format nil "src/deep/nested/~a.js" (make-string 120 :initial-element #\a)))
          (very-long (format nil "src/deep/nested/~a/~a.js"
                             (make-string 140 :initial-element #\b)
                             (make-string 140 :initial-element #\c))))
      (sh "cd ~a && echo short > src/a.js && printf 'LONG' > '~a'" dir long)
      (sh "cd ~a && mkdir -p '~a' && printf 'VERYLONG' > '~a'"
          dir (subseq very-long 0 (position #\/ very-long :from-end t)) very-long)
      (dolist (fmt '("gnu" "pax" "ustar"))
        (let ((archive (format nil "~a/t-~a.tar" dir fmt)))
          ;; ustar cannot represent the very long name at all, so it is only asked for the others.
          (sh "cd ~a && tar --format=~a -cf ~a src ~a 2>/dev/null" dir fmt archive
              (if (string= fmt "ustar") "--exclude='*bbb*'" ""))
          (when (probe-file archive)
            (let* ((bytes (slurp-bytes archive))
                   (ours (tar-names bytes))
                   (theirs (sort (remove "" (uiop:split-string (sh "tar -tf ~a" archive)
                                                               :separator '(#\Newline))
                                         :test #'string=)
                                 #'string<))
                   ;; `tar -tf` lists directories with a trailing slash; TAR-EXTRACT returns files
                   ;; only, so compare against their files.
                   (their-files (remove-if (lambda (s) (char= (char s (1- (length s))) #\/)) theirs)))
              (check (equal ours their-files)
                     "--format=~a: names match GNU tar (~d entries)~@[  ours=~s theirs=~s~]"
                     fmt (length ours)
                     (unless (equal ours their-files) (list ours their-files)))
              ;; CONTENTS, not just names, and for EVERY file rather than a couple I chose: a
              ;; long-name reader can get a name right and still pair it with the wrong data
              ;; block, which a name-only comparison cannot see.  GNU tar extracts the archive
              ;; and every byte we return is compared to the file it wrote.
              (let ((into (format nil "~a/x-~a" dir fmt)))
                (sh "rm -rf ~a && mkdir -p ~a && tar -xf ~a -C ~a 2>/dev/null" into into archive into)
                (let ((mismatch nil))
                  (dolist (e (cram:tar-extract bytes))
                    (let ((disk (probe-file (format nil "~a/~a" into (car e)))))
                      (unless (and disk (equalp (cdr e) (slurp-bytes disk)))
                        (setf mismatch (car e)))))
                  (check (null mismatch) "--format=~a: every file's bytes match GNU tar's~@[ (~a)~]"
                         fmt mismatch))))))))

    ;; ---- the security case ----------------------------------------------------------------
    ;; An archive whose entry names escape the destination.  `tar` itself refuses to store `..`
    ;; without -P, which is the point: these names only ever appear when someone means them.
    (sh "cd ~a && mkdir -p esc && echo pwned > esc/x && tar -P -cf evil.tar --transform='s|esc/x|../../etc/pwned|' esc/x 2>/dev/null" dir)
    (sh "cd ~a && tar -P -cf abs.tar --transform='s|esc/x|/etc/pwned|' esc/x 2>/dev/null" dir)
    (dolist (case '(("evil.tar" "a ../ path") ("abs.tar" "an absolute path")))
      (let ((path (format nil "~a/~a" dir (first case))))
        (when (probe-file path)
          (let ((bytes (slurp-bytes path)))
            (check (nth-value 1 (ignore-errors (cram:tar-extract bytes)))
                   "~a is REFUSED by default" (second case))
            (check (typep (nth-value 1 (ignore-errors (cram:tar-extract bytes)))
                          'cram:tar-unsafe-name-error)
                   "~a signals TAR-UNSAFE-NAME-ERROR, not a generic one" (second case))
            (check (not (nth-value 1 (ignore-errors
                                      (cram:tar-extract bytes :allow-unsafe-names t))))
                   "~a is readable when the caller asks for it explicitly" (second case))))))

    ;; The predicate itself, on the shapes that matter.
    (dolist (bad '("/etc/passwd" "../x" "a/../../x" "a/.." ".." "" "a\\..\\b"))
      (check (not (cram:tar-safe-name-p bad)) "unsafe name rejected: ~s" bad))
    (dolist (good '("package/index.js" "./package/a" "a/b/c.js" "a..b/c" "...x"))
      (check (cram:tar-safe-name-p good) "safe name accepted: ~s" good))))
