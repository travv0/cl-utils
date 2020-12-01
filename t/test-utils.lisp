(defpackage :travv0.utils/test
  (:use #:cl #:travv0.utils #:rove))

(in-package #:travv0.utils/test)

(setup
  (desfun abcd (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
    (declare (ignore b c))
    (list a d e2 f g h i j k)))

(teardown
  (fmakunbound 'abcd))

(deftest desfun
  (testing "expansion"
    (ok (expands '(desfun abcd (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                   (declare (ignore b c))
                   (list a d e2 f g h i j k))
                 '(progn (defun abcd (a b |(I &KEY J (K 8))| &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                           (destructuring-bind (a b (i . (&key j (k 8) &allow-other-keys)) &optional c (d 1) h &key ((:e e2) 2) (f 4) g)
                               (list a b |(I &KEY J (K 8))| c d h :e e2 :f f :g g)
                             (declare (ignore b c))
                             (list a d e2 f g h i j k)))
                   (function abcd)))))

  (testing "function call"
    (ok (equal (abcd 1 2 '(3 . (:j 4)) 5 3 :e 6 :g 8)
               (list 1 3 6 4 8 '(:e 6 :g 8) 3 4 8)))))
(deftest sfun
  (testing "expansion"
    (ok (expands '(sfun (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                   (declare (ignore b c))
                   (list a d e2 f g h i j k))
                 '(lambda (a b |(I &KEY J (K 8))| &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                   (destructuring-bind (a b (i . (&key j (k 8) &allow-other-keys)) &optional c (d 1) h &key ((:e e2) 2) (f 4) g)
                       (list a b |(I &KEY J (K 8))| c d h :e e2 :f f :g g)
                     (declare (ignore b c))
                     (list a d e2 f g h i j k))))))

  (testing "function call"
    (ok (equal (funcall (sfun (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                          (declare (ignore b c))
                          (list a d e2 f g h i j k))
                        1 2 '(3 . (:j 4)) 5 3 :e 6 :g 8)
               (list 1 3 6 4 8 '(:e 6 :g 8) 3 4 8)))))

(run-suite *package*)
