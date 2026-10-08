;;; VANILLA.SLISP
;;; -*- Mode:clisp; package: Flavors -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC).
;;; **********************************************************************
;;;

(in-package "http://pdp-10.trailing-edge.com/clisp#flavors-internals")

(defcombination-ordering daemon-ordering-primary (order)
  (order-wrappers :base-flavor-first 'wrappers '(:wrapper :whopper))
  (order-methods :primary 'primary '(:primary))
  (order-methods :primary 'default '(:default))
  (case order
    (:base-flavor-last
     (order-methods :base-flavor-last 'befores '(:before))
     (order-methods :base-flavor-first 'afters '(:after)))
    (:base-flavor-first
     (order-methods :base-flavor-first 'befores '(:before))
     (order-methods :base-flavor-last 'afters '(:after)))
    (t (error "Unknown ordering ~S." order))))

(defcombination-ordering daemon-ordering-list (order)
  (order-wrappers :base-flavor-first 'wrappers '(:wrapper :whopper))
  (case order
    (:base-flavor-last
     (order-methods :base-flavor-last 'primary '(:primary))
     (order-methods :base-flavor-last 'default '(:default))
     (order-methods :base-flavor-last 'befores '(:before))
     (order-methods :base-flavor-first 'afters '(:after)))
    (:base-flavor-first
     (order-methods :base-flavor-first 'primary '(:primary))
     (order-methods :base-flavor-first 'default '(:default))
     (order-methods :base-flavor-first 'befores '(:before))
     (order-methods :base-flavor-last 'afters '(:after)))
    (t (error "Unknown ordering ~S." order))))

(defcombination :daemon daemon-ordering-primary (arg)
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
      (cond ((and (null (method-list 'befores)) (null (method-list 'afters)))
	     (car (call-methods 'primary)))
	    (t `(progn ,@(call-methods 'befores)
		       (multiple-value-prog1
			,.(or (call-methods primary) '(nil))
			,@(call-methods 'afters))))))))

;;; KBE: without daemons this used (car (call-methods primary)), which
;;; called only the first primary method instead of all of them.
(defcombination :progn daemon-ordering-list (arg)
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
		 (cond ((and (null (method-list 'befores))
			     (null (method-list 'afters)))
			`(progn ,@(call-methods primary)))
		       (t `(progn ,@(call-methods 'befores)
				  (multiple-value-prog1
				   (progn ,@(call-methods primary))
				   ,@(call-methods 'afters))))))))

(Defflavor vanilla-flavor () ())

;;; KBE: pass PRIN1? as the Lisp Machine :PRINT-SELF protocol does
;;; (stream level prin1?).
(defmethod (vanilla-flavor print) (stream depth)
  (send self :print-self stream depth *print-escape*))

(Defmethod (vanilla-flavor describe) ()
  (send self :describe))

(DEfmethod (vanilla-flavor typep) (type)
  (let ((flavor (get type 'flavor)))
    (and flavor
	 (not (null (member flavor
			    (flavor-all-components
			     (get-flavor
			      (instance-descriptor-type
			       (instance-descriptor self))))))))))

(defmethod (vanilla-flavor :print-self) (stream depth &optional prin1?)
  (declare (ignore depth prin1?))
  (format stream "#<~A ~A>"
	  (instance-descriptor-type (instance-descriptor  self))
	  (pointer-to-fixnum self)))

(defmethod (vanilla-flavor :describe) (&optional (stream *standard-output*))
  (let* ((name (instance-descriptor-type (instance-descriptor self)))
	 (vec (iv-env-vector (flavor-instance-env (get-flavor name)))))
    (format stream "~&An instance of flavor ~S." name)
    (cond ((zerop (length vec))
	   (format stream "~%No instance variables."))
	  (t (format stream "~%Instance variables:")))
    (dotimes (i (length vec))
      (format stream "~%~3,8@T~S ~S" (aref vec i)
	      (symeval-in-instance self (aref vec i) t :unbound)))))

(defmethod (vanilla-flavor :which-operations) ()
  (let ((list nil))
    (do-handlers ((name function)
		  (flavor-descriptor
		   (get-flavor
		    (instance-descriptor-type
		     (instance-descriptor self)))))
      (declare (ignore function))
      (push name list))
    list))

(defmethod (vanilla-flavor :operation-handled-p) (operation)
  (not (null (get-handler operation (instance-descriptor self)))))

(defmethod (vanilla-flavor :send-if-handles) (operation &rest arguments)
  (when (get-handler operation (instance-descriptor self))
    (apply #'send self operation arguments)))

;;; From here are additions by Victor

(defmethod (vanilla-flavor :get-handler-for) (operation)
  (get-handler operation (instance-descriptor self)))

(defmethod (vanilla-flavor :eval-inside-yourself) (form)
  (eval form))

(defmethod (vanilla-flavor :funcall-inside-yourself) (function &rest args)
  (funcall function args))

(defmethod (vanilla-flavor :break) ()
  (break))

(defmethod (vanilla-flavor :type-of) ()
  (instance-descriptor-type (instance-descriptor self)))

(cl:defmethod describe-object ((inst %instance) stream)
  (send inst :describe))
