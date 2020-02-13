(in-package :travv0.utils)

(setf defstar:*check-argument-types-explicitly?* t)

(defmacro desfun (name lambda-list &body body)
  (labels ((add-allow-keys (list)
             (if (a:proper-list-p list)
                 (let ((new-list (mapcar #'add-allow-keys list)))
                   (if (and (position '&key new-list)
                            (not (position '&allow-other-keys new-list)))
                       (append new-list '(&allow-other-keys))
                       new-list))
                 list)))
    (let ((expression (loop :with into = nil
                            :for param :in lambda-list
                            :unless (or (char= (char (write-to-string param) 0) #\&))
                              :append (case into
                                        (&key
                                         (if (listp param)
                                             (if (listp (car param))
                                                 (car param)
                                                 (list (a:make-keyword
                                                        (write-to-string (car param)))
                                                       (intern (write-to-string (car param)))))
                                             (list (a:make-keyword param) param)))
                                        (&optional (if (listp param)
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
                                                       #\&))
                                            (not (position param
                                                           '(&optional &key)))))
                                     (add-allow-keys lambda-list)))
          (defun-lambda-list (loop :with into := nil
                                   :for param :in lambda-list
                                   :when (char= #\& (char (write-to-string param) 0))
                                     :do (setf into param)
                                   :collect (case into
                                              (&key
                                               (if (listp param)
                                                   (if (listp (car param))
                                                       param
                                                       (list (intern
                                                              (write-to-string (car param)))
                                                             (second param)))
                                                   param))
                                              (&optional param)
                                              (otherwise
                                               (intern (string-upcase
                                                        (write-to-string param))))))))
      `(defun ,name (,@defun-lambda-list)
         (destructuring-bind ,db-lambda-list (list ,@expression)
           ,@body)))))

(defun* (from-digits -> integer) ((digits cons))
  (:pre (every (lambda (d) (and (integerp d) (<= 0 d 9))) digits))
  (read-from-string (map 'string
                         (a:compose (a:rcurry #'elt 0) #'write-to-string)
                         digits)))

(defun* (digits -> cons) ((number (or string integer)))
  (:pre (if (stringp number)
            (ignore-errors (parse-integer number))
            t))
  (etypecase number
    (string (map 'list (a:compose #'parse-integer #'string) number))
    (integer (map 'list (a:compose #'parse-integer #'string)
                  (write-to-string number)))))

(defun* (all-permutations -> list) ((list list))
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (remove-duplicates
            (loop for element in list
                  append (mapcar (lambda (l) (cons element l))
                                 (all-permutations (remove element list
                                                           :count 1))))
            :test 'equal))))

(defun* (make-combos -> list) ((n fixnum plusp) (list list))
  (:pre (>= (length list) n))
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

(defun* (get-command-line-args -> list) ()
  #+sbcl sb-ext:*posix-argv*
  #+lispworks system:*line-arguments-list*
  #+ecl ext:*command-args*
  )
