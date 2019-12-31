(in-package :cl-user)
(defpackage :travv0.utils
  (:use #:cl #:alexandria)
  (:export #:desfun))

(in-package :travv0.utils)

(defmacro desfun (name lambda-list &body body)
  (labels ((add-allow-keys (list)
             (if (proper-list-p list)
                 (let ((new-list (mapcar #'add-allow-keys list)))
                   (if (and (eql (car new-list) '&key)
                            (not (position '&allow-other-keys new-list)))
                       (append new-list '(&allow-other-keys))
                       new-list))
                 list)))
    (let* ((lambda-list (add-allow-keys lambda-list))
           (expression (loop for param in lambda-list
                             with into-keys = nil
                             when (not (char= (char (write-to-string param) 0)
                                              #\&))
                               append (if into-keys
                                          (if (listp param)
                                              (if (listp (car param))
                                                  (car param)
                                                  (list (make-keyword
                                                         (write-to-string (car param)))
                                                        (intern (write-to-string (car param)))))
                                              (list (make-keyword param) param))
                                          (list (intern (string-upcase
                                                         (write-to-string param)))))
                             when (eql param '&key)
                               do (setf into-keys t)))
           (db-lambda-list (remove-if (lambda (param)
                                        (and (char= (char (write-to-string param)
                                                          0)
                                                    #\&)
                                             (not (eql param '&key))))
                                      lambda-list))
           (defun-lambda-list (loop for param in lambda-list
                                    with into-keys = nil
                                    when (eql param '&key)
                                      do (setf into-keys t)
                                    append (if into-keys
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
      `(defun ,name (,@defun-lambda-list)
         (destructuring-bind ,db-lambda-list (list ,@expression)
           ,@body)))))
