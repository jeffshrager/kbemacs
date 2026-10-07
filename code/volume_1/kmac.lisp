;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 142-148; listing kmac.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; KMAC: This module contains KBE-specific utility macros for use
;;;       by any other module in the KBE system except BASIC and MACRO.
;;;       It may also contain related variable definitions.
;;; PDF page 143
;;; Interface Control
;;; These primitives are mainly for use in KBE bootstrapping and
;;; debugging. Except for special reasons (debugging, self-testing),
;;; they should not be called dynamically by the running KBE.
;;; Unless the interface is not broken, it shouldn't ever be disabled
;;; anyway.
;;; (ENABLE-INTERFACE)
;;; (DISABLE-INTERFACE)
(defvar *interface-enabled* nil ;Set to T after bootstrapping system.
  "If false, the KBE interface is disabled.")
(defun enable-interface () (setq *interface-enabled* t))
(defun disable-interface () (setq *interface-enabled* nil))
;;; (WITH-INTERFACE-DISABLED ...forms...)
;;;   Runs FORMS in a context where the interface is bound off,
;;;   returning the value of the last form.
(defmacro with-interface-disabled (&body forms)
  `(let ((*interface-enabled* nil))
     ,@forms))
;;; (WHEN-INTERFACE-ENABLED ...forms...)
;;;   If executed in a context where the interface is enabled,
;;;   its effect and value is the same as for PROGN.
;;;   If executed in a context where the interface is disabled,
;;;   it has no effect and returns an undefined value.
(defmacro when-interface-enabled (&body forms)
  `(cond (*interface-enabled*
          ,@forms)
         (t ;We should never be being used for value in the
            ;interface, but in order to ease debugging, we'll
            ;return a distinctive marker just in case.
          '<interface-disabled>)))
;;; PDF page 144
;;; Initializations
(defvar *system-initializations* '() "KBE initializations list.")
(defmacro define-system-initialization (name &body forms)
  (cond ((not forms)
         `(delete-initialization ',name '() '*system-initializations*))
        (t
         `(add-initialization ',name
                             ',(cond ((cdr forms) `(progn ,@forms))
                                     (t (car forms)))
                             '()
                             '*system-initializations*))))
(defun initialize-system (&optional redo?)
  (initializations '*system-initializations* redo?))
(define-system-initialization enable-interface
  (enable-interface))
;;; PDF page 145
;;; KBE Option Set
;;; *KBE-switches*
;;; An alist of switch names and variables.
(defvar *KBE-switches* '())
(defun set-switch (switch value)
  (declare (values success? comment-string))
  (let ((var-info (assq switch *KBE-switches*))
        (switch-name (string-downcase switch)))
    (cond ((not var-info)
           (values nil
                   (text "I don't know about any switch named /"~A/"."
                         switch-name)))
          (t
           (let ((old-value (symeval (cadr var-info))))
             (set (cadr var-info) value)
             (values t
                     (text (cond ((eq old-value value)
                                  "~:[Not~;Already~] in ~A mode.")
                                 (t
                                  "~:[Disabled~;Enabled~] ~A mode."))
                           value switch-name)))))))
;;; *KBE-Options*
;;; *KBE-Internal-Options*
;;; *KBE-Trace-Options*
;;; Options defined in this set control the various parameterized
;;; actions of the KBE which are intended for the user to be able
;;; to control.
(defmacro defdef-option (name doc options-list)
  `(local-declare ()
     (define-user-option-alist ,options-list)
     (defprop ,name ,doc si:definition-type-name)
     (defmacro ,name (var doc default-val type &rest data)
       `(local-declare ()
          (define-user-option (,var ,',options-list)
                              ,default-val ,type ,doc ,@data)
          ,(when (eq type ':boolean)
             (let ((tag (intern-keyword (string-trim "*" var))))
               `(if (not (memq ',tag *kbe-switches*))
                    (push (list ',tag ',var) *KBE-switches*))))
          ',var))))
(defdef-option define-KBE-option "KBE option" *KBE-Options*)
(defdef-option define-internal-option "KBE internal option" *KBE-Internal-Options*)
(defdef-option define-trace-option "KBE trace option" *KBE-Trace-Options*)
;;; For implementors debugging the system, this variable should be set T.
;;; For "production" (and, of course, for demos) it should be left NIL to
;;; save the embarrassment and frustration of error breaks in at least
;;; some of the places they might happen.
(define-internal-option *debug* "KBE debug mode (local)" t :boolean)
(define-system-initialization disable-debug-mode
  (setq *debug* nil))
(define-internal-option *bug-mail-address*
;;; PDF page 146
  "BUG-KBE@MIT-OZ" :sexp)
;;; For tracing modules during debugging
(defmacro format-trace (tracevar format-string &rest format-args)
  `(when ,tracevar
     (format trace-output "~&")
     (format trace-output ,format-string ,@format-args)))
(defmacro pp-trace (tracevar object)
  `(when ,tracevar (pp:pp ,object trace-output)))
(defmacro with-trace (tracevar &body forms)
  `(when ,tracevar
     (let ((standard-output trace-output))
       ,@forms)))
(defun edit-KBE-options (&optional (full? *debug*))
  (cond (full?
         (choose-user-options
           (append *KBE-Options* *KBE-Internal-Options* *KBE-Trace-Options*)))
        (t
         (choose-user-options *KBE-Options*))))
(defun odd-KBE-options ()
  (append (odd-options *KBE-Options*)
          (odd-options *KBE-Internal-Options*)))
;;; PDF page 147
;;; (WITH-AUTOMATIC-ERROR-RECOVERY . body)
;;; Try to continue errors that can be easily proceeded.
(defmacro with-automatic-error-recovery-if (cond &body forms)
  `(condition-bind-if ,cond ((continuable-error-condition-mixin
                              #'(lambda (cond)
                                  (send cond ':proceed ':continue)))
                            (dbg:proceedable-ferror
                              #'(lambda (cond)
                                  (send cond ':proceed ':no-action))))
     ,@forms))
(defmacro with-automatic-error-recovery (&body forms)
  `(with-automatic-error-recovery-if t ,@forms))
;;; PDF page 148
;;; (DEFINE-CONDITION condition (arg1 arg2 ...))
;;; This defines a way of signalling certain non-fatal conditions
;;; internally in the system in a functional fashion. For example,
;;;   (DEFINE-CONDITION FOO ((X 3)))
;;;   (FOO)   ;signals a FOO condition, X defaults to 3
;;;   (FOO 4) ;signals a FOO condition, X defaults to 4
;;; If body is supplied, it should take care of doing the
;;; signalling. It may assume that the args are bound and
;;; appropriately defaulted.
(defdefmacro define-condition "KBE condition" (name bvl . body)
  `(local-declare ()
     (defflavor ,name ,bvl (internal-condition)
       :initable-instance-variables
       :gettable-instance-variables)
     (compile-flavor-methods ,name)
     (defun ,name (&optional ,@bvl)
       ,@(or body
             `((signal
                 ',name
                 ':proceed-types '(:handled)
                 ,@(mapcan #'(lambda (x)
                               (cond ((atom x)
                                      (list `',(intern-keyword x) x))
                                     (t
                                      (list `',(intern-keyword (car x))
                                            (car x)))))
                           bvl)))))))
