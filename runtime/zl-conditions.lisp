;;; -*- Mode: LISP; Package: ZL -*-
;;;
;;; The Lisp Machine flavor-based condition system, as much as KBE uses.
;;;
;;; A condition is a flavor instance built on SYS:CONDITION. SIGNAL and
;;; ERROR carry it to CL handlers inside a CL condition object, so CL
;;; handlers, the debugger and our CONDITION-CASE all see it.
;;;
;;; Proceeding: a CONDITION-BIND handler that returns a non-NIL first value
;;; has handled the condition, and SIGNAL (or ERROR) returns those values.
;;; KBE handlers return (SEND COND :PROCEED :HANDLED) and the like. A NIL
;;; return declines, and the next handler is tried.
;;;
;;; Native CL errors match the condition name SYS:ERROR (and
;;; SYS:CONDITION), so the KBE's error traps also catch, say, a CAR of a
;;; non-list.

(in-package "ZL")

;;;; Base flavors

(defflavor sys::condition ((sys::proceed-types '())) ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:method-combination (:case :base-flavor-last :proceed
                              :document-proceed-type)))

(defmethod (sys::condition :report) (stream)
  (cl:format stream "~A was signalled." (typep self)))

(defmethod (sys::condition :report-string) ()
  (with-output-to-string (s) (send self :report s)))

(defmethod (sys::condition :condition-names) ()
  (list (typep self)))

(defmethod (sys::condition :dangerous-condition-p) () nil)
(defmethod (sys::condition :debugging-condition-p) () nil)

(defflavor sys::error () (sys::condition))
(defflavor sys::abort () (sys::condition))

;;; FERROR and friends
(defflavor sys::ferror ((sys::format-string "") (sys::format-args '()))
           (sys::error)
  :initable-instance-variables
  :gettable-instance-variables)

(defmethod (sys::ferror :report) (stream)
  (apply #'format stream sys::format-string sys::format-args))

(defflavor dbg::proceedable-ferror () (sys::ferror))

(defmethod (dbg::proceedable-ferror :case :proceed :no-action) ()
  :no-action)

;;; Names KBE traps on but which this runtime never signals itself.
(defflavor sys::package-locked () (sys::error))
(defflavor sys::read-end-of-file () (sys::error))
(defflavor sys::pdl-overflow () (sys::error))

;;;; The CL side

(cl:define-condition %zl-condition (cl:condition)
  ((instance :initarg :instance :reader %zl-condition-instance)
   (tag :initarg :tag :initform nil :reader %zl-condition-tag))
  (:report (lambda (c stream)
             (write-string (send (%zl-condition-instance c) :report-string)
                           stream))))

(cl:define-condition %zl-error (%zl-condition cl:error) ())

(cl:defun %zl-condition-object (c)
  "The Lisp Machine condition object for a CL condition: the flavor
   instance for ours, the CL condition itself for native ones."
  (cl:if (typep c '%zl-condition) (%zl-condition-instance c) c))

(cl:defun %zl-condition-matches-p (object names)
  (cl:let ((names (cl:if (listp names) names (list names))))
    (cl:some (lambda (name)
               (cond ((instancep object) (typep object name))
                     ;; native CL condition
                     ((cl:member name '(sys::error sys::condition))
                      (cl:typep object 'cl:error))
                     ((eq name 'sys::abort) nil)
                     ((%zl-flavor-p name) nil)
                     (t (ignore-errors (cl:typep object name)))))
             names)))

(cl:defun %zl-cl-condition-send (c message &rest args)
  (case message
    (:report (princ c (car args)))
    (:report-string (princ-to-string c))
    (:proceed-types '())
    (:condition-names (list (type-of c)))
    (:operation-handled-p (cl:member (car args) '(:report :report-string)))
    (otherwise nil)))

;;;; Making and signalling conditions

(cl:defun make-condition (flavor &rest init-options)
  (apply #'make-instance flavor init-options))

(cl:defun %zl-condition-instance-from (thing init-options)
  (cl:if (instancep thing) thing (apply #'make-condition thing init-options)))

(cl:defun signal (thing &rest init-options)
  (cl:let ((instance (%zl-condition-instance-from thing init-options))
           (tag (list 'signal)))
    (cl:catch tag
      (cl:signal '%zl-condition :instance instance :tag tag)
      nil)))

(cl:defun error (thing &rest args)
  (cond ((stringp thing) (apply #'ferror thing args))
        (t
         (cl:let ((instance (%zl-condition-instance-from thing args))
                  (tag (list 'error)))
           (cl:catch tag
             (cl:error '%zl-error :instance instance :tag tag))))))

(cl:defun %zl-format-args (args)
  "FERROR's first argument may be a condition name (or NIL) before the
   format string."
  (cl:if (and args (not (stringp (car args))) (symbolp (car args)))
      (cdr args)
      args))

(cl:defun ferror (&rest args)
  (cl:let ((args (%zl-format-args args)))
    (error 'sys::ferror :format-string (car args) :format-args (copy-list (cdr args)))))

(cl:defun fsignal (&rest args)
  (cl:let ((args (%zl-format-args args)))
    (error 'dbg::proceedable-ferror :format-string (car args)
           :format-args (copy-list (cdr args))
           :proceed-types '(:no-action))))

(cl:defun cerror (proceedable restartable &rest args)
  (declare (ignore restartable))
  (apply (cl:if proceedable #'fsignal #'ferror) args))

(cl:defun warn (&rest args)
  (cl:let ((args (%zl-format-args args)))
    (apply #'cl:format *error-output* (concatenate 'string "~&Warning: "
                                                   (car args) "~%")
           (cdr args))))

;;;; Handlers

(cl:defmacro condition-bind (bindings &body body)
  `(handler-bind
       ((cl:condition
          (lambda (c)
            (%zl-run-bound-handlers
             c (list ,@(mapcar (lambda (b) `(cons ',(car b) ,(cadr b)))
                               bindings))))))
     ,@body))

(cl:defun %zl-run-bound-handlers (c handlers)
  (cl:let ((object (%zl-condition-object c))
           (tag (cl:if (typep c '%zl-condition) (%zl-condition-tag c))))
    (dolist (h handlers)
      (when (%zl-condition-matches-p object (car h))
        (cl:let ((values (multiple-value-list (funcall (cdr h) object))))
          (when (and (car values) tag)
            (cl:throw tag (values-list values))))))))

(cl:defmacro condition-bind-if (test bindings &body body)
  (cl:let ((f (cl:gensym "BODY")))
    `(flet ((,f () ,@body))
       (cl:if ,test (condition-bind ,bindings (,f)) (,f)))))

(cl:defmacro condition-case ((&optional var) form &body clauses)
  (cl:let* ((outer (cl:gensym "CONDITION-CASE"))
            (object (cl:gensym "OBJECT"))
            (var (or var (cl:gensym "IGNORE")))
            (no-error (find :no-error clauses :key #'car))
            (clauses (cl:remove no-error clauses))
            (labels (mapcar (lambda (c) (declare (ignore c)) (cl:gensym "CLAUSE"))
                            clauses)))
    `(cl:block ,outer
       (cl:let ((,object nil))
         (tagbody
            (cl:return-from ,outer
              ,(cl:let ((protected
                          `(handler-bind
                               ((cl:condition
                                  (lambda (c)
                                    (cl:let ((o (%zl-condition-object c)))
                                      (cond
                                        ,@(mapcar (lambda (clause label)
                                                    `((%zl-condition-matches-p o ',(car clause))
                                                      (setq ,object o)
                                                      (go ,label)))
                                                  clauses labels))))))
                             ,form)))
                 (cl:if no-error
                     `(multiple-value-call
                          (lambda ,(cadr no-error) ,@(cddr no-error))
                        ,protected)
                     protected)))
            ,@(loop for clause in clauses
                    for label in labels
                    append `(,label
                             (cl:return-from ,outer
                               (cl:let ((,var ,object))
                                 (declare (ignorable ,var))
                                 ,@(cdr clause))))))))))

(cl:defmacro condition-case-if (test (&optional var) form &body clauses)
  (cl:let ((f (cl:gensym "BODY")))
    `(flet ((,f () ,form))
       (cl:if ,test
           (condition-case (,@(cl:if var (list var))) (,f) ,@clauses)
           (,f)))))

;;; Restarts are a user-interface feature; headless, the body just runs.
(cl:defmacro error-restart ((&optional conditions format &rest args) &body body)
  (declare (ignore conditions format args))
  `(progn ,@body))
(cl:defmacro error-restart-loop ((&optional conditions format &rest args) &body body)
  (declare (ignore conditions format args))
  `(loop ,@body))
(cl:defmacro catch-error-restart ((&optional conditions format &rest args) &body body)
  (declare (ignore conditions format args))
  `(progn ,@body))
(cl:defmacro catch-error-restart-if (test (&optional conditions format &rest args) &body body)
  (declare (ignore test conditions format args))
  `(progn ,@body))
(cl:defmacro catch-error (form &optional (print? t))
  (declare (ignore print?))
  `(handler-case ,form (cl:error () nil)))
