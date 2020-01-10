(in-package :cl-user)
(uiop:define-package #:travv0.utils
    (:nicknames :tu)
  (:use #:cl #:alexandria)
  (:export #:desfun
           #:digits
           #:*whitespace-chars*
           #:whitespace-p
           #:all-permutations
           #:make-combos))

(in-package :travv0.utils)

(defmacro desfun (name lambda-list &body body)
  (labels ((add-allow-keys (list)
             (if (proper-list-p list)
                 (let ((new-list (mapcar #'add-allow-keys list)))
                   (if (and (position '&key new-list)
                            (not (position '&allow-other-keys new-list)))
                       (append new-list '(&allow-other-keys))
                       new-list))
                 list)))
    (let* ((expression (loop :with into = nil
                             :for param :in lambda-list
                             :when (not (or (char= (char (write-to-string param) 0)
                                                   #\&)
                                            (char/= (char (write-to-string param) 0)
                                                    #\()))
                               :append (case into
                                         ('&key
                                          (if (listp param)
                                              (if (listp (car param))
                                                  (car param)
                                                  (list (make-keyword
                                                         (write-to-string (car param)))
                                                        (intern (write-to-string (car param)))))
                                              (list (make-keyword param) param)))
                                         ('&optional (if (listp param)
                                                         (list (car param))
                                                         (list param)))
                                         (otherwise
                                          (list (intern (string-upcase
                                                         (write-to-string param))))))
                             :when (char= #\& (char (write-to-string param) 0))
                               :do (setf into param)))
           (db-lambda-list (remove-if (lambda (param)
                                        (and (or (char= (char (write-to-string param)
                                                              0)
                                                        #\&)
                                                 (char/= (char (write-to-string param)
                                                               0)
                                                         #\())
                                             (not (position param
                                                            '(&optional &key)))))
                                      (add-allow-keys lambda-list)))
           (defun-lambda-list (loop :with into := nil
                                    :for param :in lambda-list
                                    :when (char= #\& (char (write-to-string param) 0))
                                      :do (setf into param)
                                    :collect (case into
                                               ('&key
                                                (if (listp param)
                                                    (if (listp (car param))
                                                        param
                                                        (list (intern
                                                               (write-to-string (car param)))
                                                              (second param)))
                                                    param))
                                               ('&optional param)
                                               (otherwise
                                                (intern (string-upcase
                                                         (write-to-string param)))))))
           (ignore-list (remove-if-not (lambda (sym)
                                         (char= #\_
                                                (char (write-to-string sym) 0)))
                                       defun-lambda-list)))
      `(defun ,name (,@defun-lambda-list)
         ,(when ignore-list
            `(declare (ignore ,@ignore-list)))
         (destructuring-bind ,db-lambda-list (list ,@expression)
           ,@body)))))

(defun digits (number)
  (etypecase number
    (string (map 'list (compose #'parse-integer #'string) number))
    (number (map 'list (compose #'parse-integer #'string)
                 (write-to-string number)))))

(defun whitespace-p (char)
  #+sbcl (not (not (sb-unicode:whitespace-p char)))
  #+lispworks (lw:whitespace-char-p char)
  #-(or sbcl lispworks)
  (not (not (position char
                      '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout)))))

(defparameter *whitespace-chars*
  (loop :for i :from 0
        :for c = (ignore-errors (code-char i))
        :while c
        :when (whitespace-p c)
          :collecting c))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (remove-duplicates
            (loop for element in list
                  append (mapcar (lambda (l) (cons element l))
                                 (all-permutations (remove element list
                                                           :count 1))))
            :test 'equal))))

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
