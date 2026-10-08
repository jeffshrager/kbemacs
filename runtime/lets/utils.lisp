(in-package :lets)

;; MacLISP compatible

(declaim (inline memq))
(defun memq (item lst)
  (member item lst :test #'eq))

(declaim (inline assq))
(defun assq (item alist)
  (assoc item alist :test #'eq))

(declaim (inline ncons))
(defun ncons (item)
  (cons item nil))

(defmacro putprop (sym val prop)
  `(setf (get ,sym ,prop) ,val))

(defmacro comment (&body body)
  (declare (ignore body))
  '(quote comment))

;; for macro definition
(defmacro defun-compile-time (function-name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:defun ,function-name ,lambda-list ,@body)
     (eval-when (:compile-toplevel) (compile ',function-name))))
