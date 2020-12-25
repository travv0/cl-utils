(defpackage :travv0.utils/tests
  (:use #:cl #:travv0.utils #:fiveam)
  (:local-nicknames (#:a #:alexandria))
  (:export #:run-tests))

(in-package #:travv0.utils/tests)

(def-suite travv0.utils)
(in-suite travv0.utils)

(defun run-tests ()
  (run! 'travv0.utils))

(test desfun
  (is (equalp (macroexpand-1 '(desfun abcd (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                               "docstring"
                               (declare (ignore b c))
                               (list a d e2 f g h i j k)))
              '(progn (defun abcd (a b |(I &KEY J (K 8))| &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                        "docstring"
                        (destructuring-bind (a b (i . (&key j (k 8) &allow-other-keys)) &optional c (d 1) h &key ((:e e2) 2) (f 4) g)
                            (list a b |(I &KEY J (K 8))| c d h :e e2 :f f :g g)
                          (declare (ignore b c))
                          (list a d e2 f g h i j k)))
                (function abcd))))

  (desfun abcd (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
    (declare (ignore b c))
    (list a d e2 f g h i j k))

  (is (equal (abcd 1 2 '(3 . (:j 4)) 5 3 :e 6 :g 8)
             (list 1 3 6 4 8 '(:e 6 :g 8) 3 4 8)))

  (fmakunbound 'abcd))

(test sfun
  (is (equalp (macroexpand-1 '(sfun (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                               "docstring"
                               (declare (ignore b c))
                               (list a d e2 f g h i j k)))
              '(lambda (a b |(I &KEY J (K 8))| &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                "docstring"
                (destructuring-bind (a b (i . (&key j (k 8) &allow-other-keys)) &optional c (d 1) h &key ((:e e2) 2) (f 4) g)
                    (list a b |(I &KEY J (K 8))| c d h :e e2 :f f :g g)
                  (declare (ignore b c))
                  (list a d e2 f g h i j k)))))

  (is (equal (funcall (sfun (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                        (declare (ignore b c))
                        (list a d e2 f g h i j k))
                      1 2 '(3 . (:j 4)) 5 3 :e 6 :g 8)
             (list 1 3 6 4 8 '(:e 6 :g 8) 3 4 8))))

(test digits
  (is (equal (digits 12345) '(1 2 3 4 5)))
  (is (equal (digits "999999") '(9 9 9 9 9 9))))

(test from-digits
  (is (equal (from-digits '(6 5 4 3 2)) 65432)))

(test permutations
  (is (a:set-equal (permutations '(1 2 3))
                   '((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1))
                   :test #'equal))
  (is (a:set-equal (permutations '(1 2 3) 2)
                   '((1 2) (1 3) (2 3) (2 1) (3 1) (3 2))
                   :test #'equal))
  (is (a:set-equal (permutations '() 2) '()
                   :test #'equal))
  (is (a:set-equal (permutations '(1 2 3) 1) '((1) (2) (3))
                   :test #'equal)))

(test combinations
  (is (equal (combinations '(1 2 3) 3)
             '((1 2 3))))
  (is (equal (combinations '(1 2 3) 2)
             '((1 2) (1 3) (2 3))))
  (is (equal (combinations '() 2) '()))
  (is (equal (combinations '(1 2 3) 1) '((1) (2) (3)))))

(test canonicalize-path˜
  (is (string= (canonicalize-path "asdf")
               (namestring (merge-pathnames "asdf" (uiop:getcwd)))))
  (is (string= (canonicalize-path "/asdf")
               "/asdf"))
  (is (string= (canonicalize-path "~///.//test/fdsa/..//asdf")
               (namestring (merge-pathnames "test/asdf" (user-homedir-pathname)))))
  (is (eq nil nil)))
