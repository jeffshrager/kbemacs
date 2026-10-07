;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 105-110; listing conditions.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; CONDITION: Basic condition primitives for the KBE

;;;; Condition Mixin

;;; CONDITION-MIXIN
;;;
;;;  A base mixin for conditions which is tolerant of unsupplied data.

(defflavor condition-mixin (description data) ()
  :initable-instance-variables
  :gettable-instance-variables
  (:default-init-plist
    :data nil
    :description nil
    :proceed-types '()))

(defmethod (condition-mixin :report) (stream)
  (cond (description (format stream "~A" description))
        (t (format stream "~A was signalled, but had no description."
                   (string-capitalize
                     (a-or-an
                       (string-downcase (de-hyphenate (typep self)))))))))

(defwhopper (condition-mixin :proceed) (proceed-type &rest args)
  (if (not (or (memq proceed-type (send self ':proceed-types))
               (eq proceed-type ':which-operations)))
      (ferror "Attempted illegal (SEND '~S ':PROCEED '~S ...)"
              self proceed-type))
  (lexpr-continue-whopper proceed-type args))

(defmethod (condition-mixin :bug-report-recipient-system) () ':kbe)

(defmethod (condition-mixin :before :bug-report-description) (stream arg)
  arg ;ignored
  (let ((options (odd-KBE-options)))
    (when options
      (format stream "~2&KBE option~P in effect:~{~% * ~A~}~%"
              (length options) options))))

;;;; Mixable condition types

;;; INTERNAL-CONDITION-MIXIN
;;;     USER-CONDITION-MIXIN
;;;
;;;  Mixing in these types allow one to distinguish conditions which
;;;  should be visible to the interface (USER conditions) from
;;;  conditions which should be visible only internally (INTERNAL
;;;  conditions)

(defflavor internal-condition-mixin () ())
(defflavor     user-condition-mixin () ())

;;;  SIMPLE-CONDITION-MIXIN
;;; WARNING-CONDITION-MIXIN
;;;   ERROR-CONDITION-MIXIN
;;;
;;;  Mixing in these types allows one to distinguish conditions which
;;;  are either used just to implement some control structure (SIMPLE
;;;  conditions); mild, ignorable errors (WARNING conditions); or
;;;  potentially fatal errors (ERROR conditions).

(defflavor  simple-condition-mixin () (condition-mixin sys:condition))
(defflavor warning-condition-mixin () (condition-mixin sys:condition))
(defflavor   error-condition-mixin () (condition-mixin sys:error))

;;; CONTINUABLE-ERROR-CONDITION-MIXIN
;;;
;;;  Like ERROR-CONDITION-MIXIN, but provides for the ability
;;;  to proceed the error with no (user) action.

(defflavor continuable-error-condition-mixin (continue-description)
           (error-condition-mixin)
  :initable-instance-variables
  :gettable-instance-variables)

(defmethod (continuable-error-condition-mixin :case :proceed :continue) ()
  ':continue)

(defmethod (continuable-error-condition-mixin
             :case :document-proceed-type :continue)
           (stream)
  (lexpr-funcall #'format stream (send self ':continue-description)
                                 (send self ':data)))

;;; Warning/Error Mixin

;;; Internal Error/Warning
;;;  We hope this kind doesn't happen in normal user interactions.

(defflavor internal-continuable-error ()
           (internal-condition-mixin continuable-error-condition-mixin))

(defflavor internal-error ()
           (internal-condition-mixin error-condition-mixin))

(defflavor internal-warning ()
           (internal-condition-mixin warning-condition-mixin))

(defflavor internal-condition ()
           (internal-condition-mixin simple-condition-mixin))

(defmethod (internal-condition :case :proceed :handled) ()
  "Declare this condition handled; stop signalling it."
  ':handled)

;;; User Error
;;;  We expect this kind might occur in normal user interactions.

(defflavor user-error ()
           (user-condition-mixin error-condition-mixin))

(defflavor user-warning ()
           (user-condition-mixin warning-condition-mixin))

(compile-flavor-methods
  internal-error internal-warning internal-condition
  user-error user-warning)

;;;; Abstractions for Errors and Warnings

;;; (WARN format-string . format-args)
;;;
;;;  This warns about an interesting but non-fatal condition
;;;  probably the fault of the user.

(defun warn (format-string &rest format-args)
  (signal 'user-warning
          ':description
             (lexpr-funcall #'format nil format-string format-args)
          ':data
             (copylist format-args)
          ':proceed-types '(:handled)))

;;; (COMPLAIN format-string . format-args)
;;;
;;;  This signals a fatal error which is likely to have been brought
;;;  on by some fault of the user's rather than a system bug.

(defun complain (format-string &rest format-args)
  (error 'user-error
         ':description
            (lexpr-funcall #'format nil format-string format-args)
         ':data (copylist format-args)))

(defprop complain t :error-reporter)

;;; (BARF format-string . format-args)
;;;
;;;  This signals a fatal error due to a system bug of some sort.

(defvar *bug* nil "Bound to a condition object during KBE error breaks.")
(defvar *bug-history* '() "List of data about previous bugs.")

(defun barf (format-string &rest format-args)
  (let ((*bug* (make-condition 'internal-error
                 ':description
                    (lexpr-funcall #'format nil format-string format-args)
                 ':data (copylist format-args)
                 ;; This :PROCEED-TYPES belongs in the call to ERROR
                 ;; later in this definition, but due to a design
                 ;; misfeature in the way :settable-instance-variables
                 ;; works, it gets ignored, so we fake the effect here.
                 ':proceed-types '())))
    (push *bug* *bug-history*)
    (error *bug*)))

(defprop barf t :error-reporter)

(defun display-bug-message (&optional (n 0) (stream standard-output))
  (let ((n (or n 0))) ;in case user used NIL explicitly
    (let ((bug-report (nth n *bug-history*)))
      (cond ((not bug-report) (format stream "(No ~:R message)" n))
            (t (format stream "~%~A~%"
                       (send bug-report ':report-string)))))))

(defun cbarf (continue-format-string error-format-string &rest format-args)
  (declare (special *debug*))
  (unless (not *debug*)
    (let ((*bug* (make-condition 'internal-continuable-error
                   ':continue-description
                   (lexpr-funcall #'format nil continue-format-string
                                  format-args)
                   ':description
                   (lexpr-funcall #'format nil error-format-string
                                  format-args)
                   ':data (copylist format-args)
                   ;; This :PROCEED-TYPES belongs in the call to SIGNAL
                   ;; later in this definition, but due to a design
                   ;; misfeature in the way :settable-instance-variables
                   ;; works, it gets ignored, so we fake the effect here.
                   ':proceed-types '(:continue))))
      (push *bug* *bug-history*)
      (let ((vals (multiple-value-list (signal *bug*))))
        (selectq (car vals)
          (:continue (values))
          (otherwise
           (ferror "Tried to proceed with ~{~S~^, ~}." vals)))))))

(defprop cbarf t :error-reporter)
