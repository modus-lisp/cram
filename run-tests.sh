#!/bin/sh
# run-tests.sh — load cram + round-trip its output through chipz. Exit 0 iff all pass.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
LISP=${LISP:-sbcl}
exec "$LISP" --non-interactive --disable-debugger --eval '(require :asdf)' \
  --eval "(push #p\"$ROOT/\" asdf:*central-registry*)" \
  --eval '(handler-bind ((warning (function muffle-warning))) (ql:quickload "chipz") (asdf:load-system "cram/test"))' \
  --eval '(uiop:quit (if (funcall (read-from-string "cram/test:run-tests")) 0 1))'
