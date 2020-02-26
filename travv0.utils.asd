(asdf:defsystem #:travv0.utils
  :serial t
  :depends-on (:alexandria :defpackage-plus :defstar)
  :pathname "./"
  :components ((:file "package")
               (:file "utils")))

(asdf:defsystem #:travv0.utils/test
    :serial t
    :depends-on (:travv0.utils :alexandria :rove)
    :pathname "./t/"
    :components ((:file "test-utils")))
