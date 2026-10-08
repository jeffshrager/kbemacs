;;; -*- Mode: LISP; Package: ZL -*-
;;;
;;; Lisp Machine Flavors features KBE uses that Spice Flavors lacks,
;;; layered on the vendored Spice Flavors (runtime/flavors/):
;;;
;;;   - :CASE method combination, with :OTHERWISE methods:
;;;       (defflavor vocabulary () ()
;;;         (:method-combination (:case :base-flavor-last :interpret)))
;;;       (defmethod (vocabulary :case :interpret analyze) (x) ...)
;;;       (defmethod (vocabulary :otherwise :interpret) (verb &rest args) ...)
;;;     (send v :interpret 'analyze x) runs the most specific
;;;     (... :case :interpret analyze) method, or else the :OTHERWISE one.
;;;   - SEND to functions (as Zetalisp allows) and to CL streams;
;;;   - TYPEP that knows flavor names, and (TYPEP x) for the type name;
;;;   - Lisp Machine DEFFLAVOR options Spice rejects.
;;;
;;; Implementation of :CASE: each (flavor :case msg sub) method becomes an
;;; ordinary Spice method for the message :|msg sub|, and the flavor that
;;; declares the :CASE combination gets a primary method for msg that
;;; dispatches on its first argument. Whoppers and daemons on msg wrap the
;;; dispatch, as they wrap the combined method on the Lisp Machine.

(in-package "ZL")

(defparameter %zl-flavors-internals
  (find-package "http://pdp-10.trailing-edge.com/clisp#flavors-internals"))

(cl:defun %zl-fi (name) (cl:intern name %zl-flavors-internals))

(cl:defun %zl-case-message (message sub)
  (cl:intern (cl:format nil "~A ~A" (symbol-name message)
                        (cl:if (symbolp sub) (symbol-name sub) sub))
             "KEYWORD"))

(cl:defun %zl-otherwise-message (message)
  (cl:intern (cl:format nil "~A :OTHERWISE" (symbol-name message)) "KEYWORD"))

(cl:defun %zl-flavor-p (name)
  (and (symbolp name) name (cl:get name (%zl-fi "FLAVOR"))))

(cl:defun %zl-handles-p (instance message)
  (spice-flavors:get-handler-for instance message))

;;;; DEFFLAVOR

(cl:defmacro defflavor (name ivs components &rest options)
  (cl:let ((case-messages '()) (new-options '()))
    (dolist (opt options)
      (cl:let ((key (cl:if (consp opt) (car opt) opt)))
        (case key
          (:method-combination
           (cl:let ((keep '()))
             (dolist (ordering (cdr opt))
               (cl:if (eq (car ordering) :case)
                   (setq case-messages (append case-messages (cddr ordering)))
                   (push ordering keep)))
             (when keep
               (push (cons :method-combination (nreverse keep)) new-options))))
          ;; (:documentation :mixin "...") on the Lisp Machine
          (:documentation
           (cl:let ((doc (find-if #'stringp (cdr opt))))
             (when doc (push (list :documentation doc) new-options))))
          ((:special-instance-variables :select-method-order :default-handler
            :instantiation-flavor-function :run-time-alternatives
            :mixture)
           nil)
          (:inittable-instance-variables
           (push (cl:if (consp opt)
                        (cons :initable-instance-variables (cdr opt))
                        :initable-instance-variables)
                 new-options))
          (t (push opt new-options)))))
    `(progn
       (spice-flavors:defflavor ,name ,ivs ,components ,@(nreverse new-options))
       ,@(mapcar (lambda (message) `(%zl-define-case-dispatch ,name ,message))
                 case-messages)
       ',name)))

(cl:defmacro %zl-define-case-dispatch (flavor message)
  `(spice-flavors:defmethod (,flavor ,message) (%sub &rest %args)
     (%zl-case-dispatch self ',message %sub %args)))

(cl:defun %zl-case-dispatch (instance message sub args)
  (cl:let ((case-message (%zl-case-message message sub)))
    (cond ((%zl-handles-p instance case-message)
           (apply #'spice-flavors:send instance case-message args))
          ((%zl-handles-p instance (%zl-otherwise-message message))
           (apply #'spice-flavors:send instance (%zl-otherwise-message message)
                  sub args))
          (t (ferror "~S has no ~S method for ~S." instance message sub)))))

;;;; DEFMETHOD, DEFWHOPPER

(cl:defmacro defmethod (spec ll &body body)
  (multiple-value-bind (cll cbody) (%zl-lambda-parts ll body :doc-allowed t)
    (destructuring-bind (flavor &rest rest) spec
      (cl:let ((spice-spec
                 (cond ((and (eq (car rest) :case) (cddr rest))
                        `(,flavor ,(%zl-case-message (cadr rest) (caddr rest))))
                       ((eq (car rest) :otherwise)
                        `(,flavor ,(%zl-otherwise-message (cadr rest))))
                       (t spec))))
        `(spice-flavors:defmethod ,spice-spec ,cll ,@cbody)))))

(cl:defmacro defwhopper (spec ll &body body)
  (multiple-value-bind (cll cbody) (%zl-lambda-parts ll body)
    `(spice-flavors:defwhopper ,spec ,cll ,@cbody)))

;;;; SEND

(cl:defun send (object message &rest args)
  (cond ((instancep object) (apply #'spice-flavors:send object message args))
        ((functionp object) (apply object message args))
        ((and (symbolp object) object (fboundp object))
         (apply object message args))
        ((streamp object) (apply #'%zl-stream-send object message args))
        ((typep object 'cl:condition)
         (apply #'%zl-cl-condition-send object message args))
        (t (ferror "Can't send ~S to ~S." message object))))

(cl:defun lexpr-send (object message &rest args)
  (apply #'send object message (apply #'list* args)))

;;; The few stream messages the KBE sends.
(cl:defun %zl-stream-send (stream message &rest args)
  (case message
    (:tyo (write-char (%zl-char (car args)) stream))
    (:tyi (read-char stream nil (car args)))
    (:untyi (unread-char (car args) stream))
    (:string-out (write-string (string (car args)) stream))
    (:line-out (write-line (string (car args)) stream))
    (:fresh-line (fresh-line stream))
    (:read-cursorpos (values (or (sb-impl::charpos stream) 0) 0))
    (:operation-handled-p
     (cl:member (car args) '(:tyo :tyi :untyi :string-out :line-out
                             :fresh-line :read-cursorpos)))
    (:send-if-handles
     (cl:if (%zl-stream-send stream :operation-handled-p (car args))
         (apply #'%zl-stream-send stream args)))
    (:pathname (cl:if (typep stream 'file-stream) (pathname stream)))
    (:which-operations '(:tyo :tyi :untyi :string-out :line-out :fresh-line
                         :read-cursorpos))
    (otherwise nil)))

;;;; TYPEP

(cl:defun typep (object &optional (type nil type-p))
  (cond ((not type-p)                   ;(TYPEP x) returns the type name
         (cond ((instancep object) (send object :type-of))
               ((symbolp object) :symbol)
               ((integerp object) :fixnum)
               ((floatp object) :flonum)
               ((stringp object) :string)
               ((consp object) :list)
               ((characterp object) :fixnum)
               ((arrayp object) :array)
               ((functionp object) :compiled-function)
               (t (type-of object))))
        ((%zl-flavor-p type)
         (and (instancep object)
              (spice-flavors:send object (%zl-fi "TYPEP") type)))
        ((instancep object) nil)
        (t (case type
             (:symbol (symbolp object))
             (:fixnum (typep object 'fixnum))
             (:list (consp object))
             (:string (stringp object))
             (:flonum (floatp object))
             (:array (arrayp object))
             (t (cl:typep object type))))))
