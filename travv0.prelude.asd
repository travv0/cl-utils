(asdf:defsystem #:travv0.prelude
  :serial t
  :depends-on (:alexandria
               :metabang-bind
               :travv0.utils
               :cl-arrows)
  :pathname "./"
  :components ((:file "prelude")))
