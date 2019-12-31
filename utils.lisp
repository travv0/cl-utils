(in-package :cl-user)
(uiop:define-package #:travv0.utils
    (:nicknames :tu)
  (:use #:cl #:alexandria)
  (:shadow cl:defun)
  (:export #:defun
           #:digits
           #:+whitespace-chars+
           #:all-permutations
           #:make-combos))

(in-package :travv0.utils)

(defmacro defun (name lambda-list &body body)
  (declare (optimize speed))
  (labels ((add-allow-keys (list)
             (if (proper-list-p list)
                 (let ((new-list (mapcar #'add-allow-keys list)))
                   (if (and (position '&key new-list)
                            (not (position '&allow-other-keys new-list)))
                       (append new-list '(&allow-other-keys))
                       new-list))
                 list)))
    (let* ((expression (loop :for param :in lambda-list
                             :with into-keys = nil
                             :when (not (or (char= (char (write-to-string param) 0)
                                                   #\&)
                                            (char/= (char (write-to-string param) 0)
                                                    #\()))
                               :append (if into-keys
                                           (if (listp param)
                                               (if (listp (car param))
                                                   (car param)
                                                   (list (make-keyword
                                                          (write-to-string (car param)))
                                                         (intern (write-to-string (car param)))))
                                               (list (make-keyword param) param))
                                           (list (intern (string-upcase
                                                          (write-to-string param)))))
                             :when (eql param '&key)
                               :do (setf into-keys t)))
           (db-lambda-list (remove-if (lambda (param)
                                        (and (or (char= (char (write-to-string param)
                                                              0)
                                                        #\&)
                                                 (char/= (char (write-to-string param)
                                                               0)
                                                         #\())
                                             (not (eql param '&key))))
                                      (add-allow-keys lambda-list)))
           (defun-lambda-list (loop :for param :in lambda-list
                                    :with into-keys = nil
                                    :when (eql param '&key)
                                      :do (setf into-keys t)
                                    :append (if into-keys
                                                (if (listp param)
                                                    (list
                                                     (if (listp (car param))
                                                         param
                                                         (list (intern
                                                                (write-to-string (car param)))
                                                               (second param))))
                                                    (list param))
                                                (list (intern (string-upcase
                                                               (write-to-string param))))))))
      `(cl:defun ,name (,@defun-lambda-list)
         (destructuring-bind ,db-lambda-list (list ,@expression)
           ,@body)))))

(defun digits (number)
  (etypecase number
    (string (map 'list (compose #'parse-integer #'string) number))
    (number (map 'list (compose #'parse-integer #'string)
                 (write-to-string number)))))

(defconstant +whitespace-chars+
  (loop :for i :from 0 :to 255
        :for c = (code-char i)
        :when (sb-unicode:whitespace-p c)
          :collecting c))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))

(defun make-combos (n list)
  (labels ((r (n list i)
             (cond ((<= n 0) nil)
                   ((= n 1) (mapcar #'list list))
                   (t (if (= i 0)
                          '()
                          (append (mapcar (lambda (elem)
                                            (cons (car list) elem))
                                          (if (<= n 2)
                                              (mapcar #'list (cdr list))
                                              (make-combos (1- n) (cdr list))))
                                  (r n (append (cdr list) (list (car list))) (1- i))))))))
    (r n list (length list))))
