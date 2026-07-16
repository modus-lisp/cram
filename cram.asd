;;;; cram.asd — a from-scratch DEFLATE/zlib compressor (with sync-flush) in CL.

(asdf:defsystem :cram
  :description "A pure-Common-Lisp DEFLATE/zlib compressor (RFC 1950/1951) with a
persistent, SYNC-FLUSHable stream — the Z_SYNC_FLUSH that RFB's ZRLE/Tight need
and salza2 doesn't expose.  Output is standard zlib, decodable by any inflate.
No FFI, no dependencies."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages") (:file "deflate")))))

(asdf:defsystem :cram/test
  :description "Round-trip cram's output through chipz (an independent inflate)."
  :depends-on ("cram" "chipz")
  :serial t
  :components ((:module "test" :serial t :components ((:file "oracle")))))
