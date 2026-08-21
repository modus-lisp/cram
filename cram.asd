;;;; cram.asd — the stack's compression library: DEFLATE/zlib (with sync-flush) and LZW.

(asdf:defsystem :cram
  :description "The stack's compression library, pure Common Lisp, no FFI and no
dependencies.  DEFLATE/zlib (RFC 1950/1951) both ways, including a persistent,
SYNC-FLUSHable stream — the Z_SYNC_FLUSH that RFB's ZRLE/Tight need and salza2
doesn't expose; output is standard zlib, decodable by any inflate.  Plus LZW
decoding for the formats that use it (GIF today; TIFF and PDF's LZWDecode are the
same algorithm with different packing).  Codecs live HERE and not inside whichever
image format first needed one."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages") (:file "deflate") (:file "inflate")
                             (:file "prefix") (:file "lzw")))))

(asdf:defsystem :cram/test
  :description "Round-trip cram both ways, cross-checked against chipz + salza2."
  :depends-on ("cram" "chipz" "salza2")
  :serial t
  :components ((:module "test" :serial t :components ((:file "oracle")))))
