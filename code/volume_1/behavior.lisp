;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 162-164; listing behavior.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; EVENT: A dictionary of events in the KBE actor world.
;;; PDF page 162
(defun not-a-delegate (message &rest args)
  (cbarf "Proceed quietly, ignoring the ~S message."
         "Attempt to send a message when there is no active delegate."
         message (copylist args)))
(defvar *delegate* #'not-a-delegate
  "An actor with all the default behaviors.")
;;; These are easily abused. Very sparing use is recommended.
;;; If you're not sure why you're using it, you're probably cheating.
(defun send-delegate (kwd &rest args)
  (lexpr-send *delegate* kwd args))
(defun lexpr-send-delegate (kwd &rest args)
  (lexpr-funcall #'lexpr-send *delegate* kwd args))
(defun window-delegate (&optional (window terminal-io))
  (or (send-any-superior-if-handles window ':delegate)
      *delegate*)) ;There must be something better -- might want to
                   ;create a *default-delegate* some day but this will do
                   ;for now. --kmp
;;; PDF page 163
;;; Event Handler Mixin
;;; ACTOR-DEFAULT-BEHAVIOR-MIXIN
;;; Adding this mixin allows a flavor to receive :EVENT messages
;;; and handle events.
(defflavor actor-default-behavior-mixin () ()
  (:method-combination (:case :base-flavor-last :behavior)))
;;; :EVENT
;;; This message is sent to a flavor instance when an event is
;;; signalled. It just re-dispatches via a :BEHAVIOR message.
;;; Other actors may want to redefine this method to conditionally
;;; either forward the event notification to another actor, handle
;;; it themselves (by sending self a :BEHAVIOR message), or
;;; whatever.
(defmethod (actor-default-behavior-mixin :event) (&rest args)
  (lexpr-send self ':behavior args))
;;; OTHERWISE :BEHAVIOR
;;; The fall-through case for events that are not handled
;;; by a :CASE :BEHAVIOR method.
(defmethod (actor-default-behavior-mixin :otherwise :behavior)
           (event &rest args)
  (ferror "~A has no defined behavior for the event ~S.~% Event Data: ~S"
          self event (copylist args)))
;;; (DEFINE-BEHAVIOR (flavor name) bvl . body)
;;; This defines a :CASE method for event dispatching of FLAVOR on NAME
;;; with the given bvl and body.
(defdefmacro define-behavior "KBE actor behavior" ((type event) bvl . body)
  `(defmethod (,type :case :behavior ,event) ,bvl
     (prog ()
           (progn ,@body)
           (return))))
(defprop define-behavior "KBE actor behavior" si:definition-type-name)
;;; PDF page 164
;;; Event Definition
;;; (DEFINE-EVENT name bvl . body)
;;; This declares an event type and creates a default behavior. If no
;;; body is given, the default is to make a body which will type out
;;; information about the event and prompt for any declared return
;;; values.
(defdefmacro define-event "KBE event" (name bvl . body)
  (let ((declarations nil)
        (documentation nil)
        (body-forms nil))
    ;; Process body
    (do ((b body (cdr b)))
        ((null b) (setq body-forms (nreverse body-forms)))
      (let ((form (car b)))
        (cond ((and (cdr b) (stringp form))
               (setq documentation form))
              ((and (not (atom form)) (eq (car form) 'declare))
               (setq declarations (append declarations (cdr form))))
              (t
               (push form body-forms)))))
    ;; Hack defaulting
    (when (not body-forms)
      (let ((values (assq 'values declarations))
            (arglist (parse-argument-list bvl)))
        (setq body-forms
              `((let (prinlevel prinlength)
                  (format t
                          ,(text "~~&~A.~{~~% ~A: ~~S~}"
                                 (string-capitalize-words name)
                                 (mapcar #'string-capitalize-words
                                         arglist))
                          ,@arglist))
                (return ,@(mapcar #'(lambda (rv)
                                      `(prompt-and-read
                                         ':eval-form
                                         ,(text "Supply ~A return value: " rv)))
                                  (cdr values)))))))
    ;; Process body, returning result
    `(local-declare ()
       ;; default method handling for things which handle events
       (define-behavior (actor-default-behavior-mixin ,name) ,bvl
         ,@body-forms)
       ;; main function for signalling events
       (defun ,name (&rest args)
         (declare (arglist ,@bvl) ,@declarations)
         ,documentation
         (when-interface-enabled
           (lexpr-send-delegate ':event ',name args))))))
