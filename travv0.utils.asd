(asdf:defsystem #:travv0.utils
  :serial t
  :depends-on (:alexandria :defpackage-plus)
  :pathname "./"
  :components ((:file "package")
               (:file "utils")))

(asdf:defsystem #:travv0.utils/tests
    :serial t
    :depends-on (:travv0.utils :alexandria :fiveam)
    :pathname "./t/"
    :components ((:file "test-utils")))
