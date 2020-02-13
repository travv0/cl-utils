(defpackage+-1:defpackage+ #:travv0.utils
  (:use-only #:cl)
  (:local-nicknames (:a #:alexandria))
  (:import-from #:defstar
                #:defun*
                #:->)
  (:export #:desfun
           #:digits
           #:from-digits
           #:all-permutations
           #:make-combos
           #:get-command-line-args))
