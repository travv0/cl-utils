(asdf:defsystem #:travv0.utils
  :serial t
  :depends-on (:alexandria)
  :pathname "./"
  :components ((:file "utils")))

(asdf:defsystem #:travv0.utils/test
    :serial t
    :depends-on (:alexandria :fiveam)
    :pathname "./t/"
    :components ((:file "test-utils")))
