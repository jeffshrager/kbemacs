;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 170-181; listing actor.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; ACTOR: Primitive actor specification.
;;; For debugging.
(define-trace-option *actor-debug* "Show trace of ACTOR internals"
  nil :boolean)
;;; PDF page 171
(defflavor actor-message-passing-mixin ((inbox nil)
                                      (current-msg-id 0)
                                      (debug-mode-default nil)) ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)
(defmethod (actor-message-passing-mixin :after :init) (&rest ignore)
  (when (not inbox)
    (setq inbox (make-instance 'inbox ':owner self)))
  (add-to-mailing-list (send self ':short-name) inbox))
(defmethod (actor-message-passing-mixin :new-msg-id) ()
  (incf current-msg-id))
;;; PDF page 172
;;; Tell
(defun *tell (actor key &rest args)
  (lexpr-send-delegate ':tell actor '(:notify-upon-termination t)
                       key args))
(defmethod (actor-message-passing-mixin :tell) (actor options key &rest args)
  (when actor
    (deliver actor
             (lexpr-funcall #'make-instance 'message
               ':to actor
               ':from (send self ':name)
               ':type key
               ':id (send self ':new-msg-id)
               ':data (copylist args)
               options))))
;;; PDF page 173
;;; Ask
(defun *ask (actor key &rest args)
  (cond ((send-delegate ':name? actor)
         (lexpr-send-delegate ':event key args))
        (t
         (lexpr-send-delegate ':ask actor key args))))
(defmethod (actor-message-passing-mixin :ask) (actor key &rest args)
  (if (not actor)
      (ferror "No actor specified in (ASK -??- ~A~{ '~S~})"
              key (copylist args)))
  (lexpr-send self ':send-locked self ':ask-internal actor key args))
(defmethod (actor-message-passing-mixin :ask-internal) (actor key &rest args)
  (send (send self ':inbox)
        ':await-message
        (lexpr-send self ':tell actor '(:notify-upon-termination t)
                    key args)
        actor))
;;; PDF page 174
;;; Delegate
(defun *delegate (actor key &rest args)
  (cond ((send-delegate ':name? actor)
         (lexpr-send-delegate ':event key args))
        (t
         (lexpr-send-delegate ':delegate actor key args))))
(defmethod (actor-message-passing-mixin :delegate) (actor key &rest args)
  (if (not actor)
      (ferror "No actor specified in (DELEGATE -??- ~A~{ '~S~})"
              key (copylist args)))
  (lexpr-send self ':send-locked self ':delegate-internal actor key args))
(defmethod (actor-message-passing-mixin :delegate-internal) (actor key &rest args)
  (let ((msg-id (lexpr-send self ':tell actor
                            '(:notify-upon-termination t)
                            key args))
        (mbox (send self ':inbox)))
    (let ((result (multiple-value-list
                    (send mbox ':await-message msg-id actor))))
      (do ((m (send mbox ':next-reply msg-id)
              (send mbox ':next-reply msg-id)))
          ((null m))
        (send self ':process-message m))
      (answer (car result)))))
;;; PDF page 175
;;; Message Support
(defmethod (actor-message-passing-mixin :send-locked) (&rest args)
  (lexpr-send (send self ':inbox) ':send-locked args))
;;; PDF page 176
;;; Enqueueing Messages
(defmethod (actor-message-passing-mixin :queue-message) (message)
  (comment ;needed? -kmp 2/22/84
    (cond ((not (typep message 'message)) ;needed?
           (setq message (make-message message)))))
  (send (send self ':inbox) ':insert-message message))
;;; PDF page 177
;;; Dequeueing Messages
(comment ;need something more complex now. --kmp 4/9/84
  (defun simple-request? (msg)
    (memq (send msg ':type) '(status-report)))
) ;tnemmoc
(defun simple-request? (ignore) nil)
(defmethod (actor-message-passing-mixin :read-messages) (&optional (wait? t))
  (let ((mbox (send self ':inbox)))
    (do ((m (send mbox ':next-message wait? #'simple-request?)
            (send mbox ':next-message wait? #'simple-request?)))
        ((not m))
      (send self ':process-message m))))
(define-condition end-of-command ())
(defvar *message-being-processed* nil)
(defvar *message-being-processed-terminated* nil)
(defmethod (actor-message-passing-mixin :process-message) (message)
  (with-defaults ((*debug* debug-mode-default))
    (with-trace *actor-debug*
      (format t "~&Processing...")
      (send message ':display))
    (let* ((*message-being-processed* message)
           (*message-being-processed-terminated* nil))
      (condition-bind ((end-of-command
                         ;; This should check for ending twice, but can't
                         ;; because that can happen in a benign way with
                         ;; when a proxy both forwards termination through
                         ;; and finishes that forwarding. I should think
                         ;; more about this sometime. --kmp
                         #'(lambda (cond)
                             (unless *message-being-processed-terminated*
                               (when (send *message-being-processed*
                                           ':notify-upon-termination)
                                 (reply answer nil))
                               (setq *message-being-processed-terminated* t))
                             (send cond ':proceed ':handled))))
        (when-abnormal-exit
          (condition-case-if (not *debug*) (cond)
              (let ((result (lexpr-send self ':behavior
                                        (send message ':type)
                                        (send message ':data))))
                (if (send message ':notify-upon-termination)
                    (reply answer result)))
            (sys:error (reply complaint (text "~A" cond)))
            (sys:abort (reply complaint "Request aborted.")))
          (if (send message ':notify-upon-termination)
              (reply complaint "Request aborted.")))))))
(defmethod (actor-message-passing-mixin :current-message) ()
  *message-being-processed*)
(defmethod (actor-message-passing-mixin :current-client) ()
  (let ((message (send self ':current-message)))
    (if message (send message ':from))))
(defun *reply (key &rest args)
  (lexpr-send-delegate ':reply key args))
(defmethod (actor-message-passing-mixin :reply-to) (message key &rest args)
  (if (not message)
;;; PDF page 178
      (lexpr-send self ':event key args)
      (lexpr-send self ':tell (send message ':from)
                  (list ':in-reply-to (send message ':id))
                  key args)))
(defmethod (actor-message-passing-mixin :reply) (key &rest args)
  (lexpr-send self ':reply-to (send self ':current-message) key args))
(defwhopper (actor-message-passing-mixin :event) (key &rest args)
  (cond (*reply-to*
         (lexpr-send self ':reply-to *reply-to* key args))
        (t
         (with-reply-to nil
           (lexpr-continue-whopper key args)))))
;;; PDF page 179
;;; Actor Name Mixin
;;; This mixin allows actors to have both fully qualified names
;;; and short names. The semantics of these is as follows:
;;; :NAME: A fully qualified name, necessary for use in
;;;        communications between actors.
;;; :SHORT-NAME: A possibly-abbreviated name, not suitable for
;;; inter-actor communications, but acceptable for use in text
;;; intended to immediately reach a human user (such as console output).
(defflavor actor-name-mixin ((short-name nil)) (name-mixin)
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)
(defmethod (actor-name-mixin :after :init) (&rest ignore)
  (let ((my-name (send self ':name)))
    (when (not (full-actor-name? my-name))
      (setq short-name my-name)
      (send self ':set-name (make-actor-full-name my-name)))
    (when (not short-name)
      (setq short-name my-name))))
(defmethod (actor-name-mixin :print-self) (stream level prin1? &rest ignore)
  level ;ignored
  (cond (prin1?
         (format stream "#<~A>" (send self ':name)))
        (t
         (format stream "~A" (send self ':short-name)))))
;;; PDF page 180
;;; Actor
;;; Anything which wants to be treated like an actor should be built
;;; out of this.
(defflavor actor () (actor-message-passing-mixin
                    actor-default-behavior-mixin
                    actor-name-mixin
                    property-list-mixin))
(compile-flavor-methods actor)
;;; PDF page 181
(defun parse-actor (actor)
  (cond ((not actor) nil)
        ((typep actor 'actor) actor)
        ((not (stringp actor))
         (barf "Cannot coerce ~S to an actor or actor designator." actor))
        ((send-any-that-handles ':name? actor))
        ((parse-remote-actor actor))
        (t (ferror "~A is not a known actor." actor))))
(defun full-actor-name? (name)
  (string-search "@" name))
(defun make-actor-full-name (name &optional (host si:local-host))
  (text "~A@~A" name host))
(defun parse-actor-full-name (name)
  (declare (values actor-name host-name remote?))
  (let ((pos (string-reverse-search "@" name)))
    (cond ((not pos)
           (values name (send si:local-host ':name) nil))
          (t
           (let ((name (substring name 0 pos))
                 (host (si:parse-host (substring name (1+ pos)))))
             (cond ((eq host si:local-host)
                    (parse-actor-full-name name))
                   (t
                    (values name (send host ':name) t))))))))
