(defpackage :travv0.utils/test
  (:use #:cl #:travv0.utils #:fiveam))

(in-package #:travv0.utils/test)

(test desfun
  (is (equalp
       (macroexpand-1 '(desfun abcd (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
                        (declare (ignore b c))
                        (list a d e2 f g h i j k)))
       '(defun abcd (a b |(I &KEY J (K 8))| &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
         (destructuring-bind (a b (i . (&key j (k 8) &allow-other-keys)) &optional c (d 1) h &key ((:e e2) 2) (f 4) g)
             (list a b |(I &KEY J (K 8))| c d h :e e2 :f f :g g)
           (declare (ignore b c))
           (list a d e2 f g h i j k)))))

  (desfun abcd (a b (i . (&key j (k 8))) &optional c (d 1) &rest h &key ((:e e2) 2) (f 4) g)
    (declare (ignore b c))
    (list a d e2 f g h i j k))

  (is (equal (abcd 1 2 '(3 . (:j 4)) 5 3 :e 6 :g 8)
             (list 1 3 6 4 8 '(:e 6 :g 8) 3 4 8))))
