(in-package :travv0.utils)

(defmacro desfun (name lambda-list &body body)
  (multiple-value-bind (expression db-lambda-list defun-lambda-list)
      (parse-desfun-lambda-list lambda-list)
    `(progn (defun ,name ,@(desfun-body expression defun-lambda-list db-lambda-list body))
            (function ,name))))

(defun desfun-body (expression defun-lambda-list db-lambda-list body)
  (let ((has-docstring-p (typep (first body) 'string)))
    `((,@defun-lambda-list)
      ,(when has-docstring-p (first body))
      (destructuring-bind ,db-lambda-list (list ,@expression)
        ,@(if has-docstring-p
              (rest body)
              body)))))

(defmacro sfun (lambda-list &body body)
  (multiple-value-bind (expression db-lambda-list defun-lambda-list)
      (parse-desfun-lambda-list lambda-list)
    `(lambda ,@(desfun-body expression defun-lambda-list db-lambda-list body))))

(defmacro fn (name-or-lambda-list &body body)
  (etypecase name-or-lambda-list
    (symbol `(desfun ,name-or-lambda-list ,(first body) ,@(rest body)))
    (list `(sfun ,name-or-lambda-list ,@body))))

(defun parse-desfun-lambda-list (lambda-list)
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
      (values expression db-lambda-list defun-lambda-list))))

(defun from-digits (digits)
  "Converts a list of digits to an integer."
  (read-from-string (map 'string #'digit-char digits)))

(defun digits (number)
  "Converts an integer or a string representation of an integer to a
  list of its digits."
  (etypecase number
    (string (map 'list (a:compose #'parse-integer #'string) number))
    (integer (digits (write-to-string number)))))

(defun permutations (list &optional (length (length list)))
  "Returns all permutations of a list, optionally restricted to a
  given length."
  (if (plusp length)
      (let (result)
        (dotimes (i (length list) result)
          (destructuring-bind (front (n . back)) (split list i)
            (let ((next (append front back)))
              (if next
                  (dolist (p (permutations next (1- length)) result)
                    (push (cons n p) result))
                  (push (list n) result))))))
      (list nil)))

(defun split (seq n)
  "Split sequence at index n."
  (list (subseq seq 0 n)
        (subseq seq n)))

(defun combinations (list n)
  (let ((n (if (>= n (length list)) (length list) n)))
    (labels ((r (list n i)
               (cond ((<= n 0) nil)
                     ((= n 1) (mapcar #'list list))
                     (t (if (= i 0)
                            '()
                            (append (mapcar (lambda (elem)
                                              (cons (car list) elem))
                                            (if (<= n 2)
                                                (mapcar #'list (cdr list))
                                                (r (cdr list) (1- n) i)))
                                    (r (cdr list) n (1- i))))))))
      (r list n (length list)))))

(defun canonicalize-path (path &optional (dir-name (uiop:getcwd)))
  "Converts `path' to its canonical form, assuming `dir-name' as the
current working directory. Returns the canonical path, or nil if
`path' is nil."
  (when path
    (let ((path-string (namestring path)))
      (if (a:emptyp path-string)
          dir-name
          (let* ((from-home-p (char= (elt path-string 0) #\~))
                 (path-string (if from-home-p
                                  (loop for i from 1
                                        for c = (ignore-errors (elt path-string i))
                                        while (or (char= c #\/)
                                                  (char= c #\\))
                                        finally (return (subseq path-string i)))
                                  path-string)))
            (uiop:ensure-absolute-pathname
             (uiop:parse-native-namestring path-string)
             (if from-home-p (user-homedir-pathname) dir-name)))))))
