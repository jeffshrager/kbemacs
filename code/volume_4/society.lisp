;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>SOCIETY.LISP.86, 3 January 1984 00:25:26
;;; PDF pages 16-19 (printed source pages 1-4).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

(defflavor society (members lock-cell) ()
           :initable-instance-variables
           :settable-instance-variables
           :gettable-instance-variables
           (:default-init-plist
               :members nil
             :lock-cell nil))

(defmethod (society :print-self) (stream level prin1? &rest ignore)
  level ;ignored
  (cond ((not prin1?)
         (format stream "Society-~O" (%pointer self)))
        (t
         (format stream "#<Society (~{~A~^, ~}) ~O>"
                 members (%pointer self)))))

(defmethod (society :insert) (actor)
  (with-lock (lock-cell :whostate "Await Society")
    (cond ((memq actor members) nil)
          (t
           (push actor members)
           actor))))

(defmethod (society :delete) (actor)
  (with-lock (lock-cell :whostate "Await Society")
    (cond ((memq actor members)
           (setq members (remove actor members))
           actor)
          (t nil))))

(defmethod (society :map) (fn)
  (mapc fn members)
  t)

(compile-flavor-methods society)

;; This flavor should be mixed into a frame which wants to allow a given
;; actor to be responsible for negotiating replies to that frame.
;; Typically, the actor will have to send the frame a :SET-DELEGATE
;; message identifying itself as the owner of the frame.

(defflavor delegate-mixin ((delegate nil) (delegate-flavor nil)) ()
           :initable-instance-variables
           :settable-instance-variables
           :gettable-instance-variables)

(defmethod (delegate-mixin :after :init) (&rest ignore)
  (when (not delegate)
    (setq delegate (make-instance delegate-flavor ':window self))))

(defvar *society* (make-instance 'society) "Holds data about our society")

(defmacro do-society ((var) &body forms)
  `(dolist (,var (send *society* ':members))
    ,@forms))

;;; (SEND-ALL message arg1 arg2 ... argN)
;;;
;;; Sends a given message to all actors in the KBE society.
;;; Return value is not guaranteed.

(defun send-all (message &rest args)
  (do-society (actor)
    (lexpr-send actor message args)))

(defun lexpr-send-all (message &rest args)
  (lexpr-funcall #'lexpr-funcall #'send-all message args))

;;; (SEND-ALL-THAT-HANDLE message arg1 arg2 ... argN)
;;;
;;; Sends a given message to all actors in the KBE society who claim
;;; to handle that message.

(defun send-all-that-handle (message &rest args)
  (do-society (actor)
    (lexpr-send-if-handles actor message args)))

(defun lexpr-send-all-that-handle (message &rest args)
  (lexpr-funcall #'lexpr-funcall #'send-all-that-handle message args))

;;; (SEND-ANY message arg1 arg2 ... argN)
;;;
;;; Sends a given message to the first actor which claims to handle
;;; the given message, returning that actor.

(defun send-any (message &rest args)
  (do-society (actor)
    (when (handles? actor message)
      (lexpr-send actor message args)
      (return actor))))

(defun lexpr-send-any (message &rest args)
  (lexpr-funcall #'lexpr-funcall #'send-any message args))

;;; (SEND-ANY-THAT-HANDLES message arg1 arg2 ... argN)
;;;
;;; Sends a given message to the first actor which claims to handle
;;; the given message and which does so returning a non-NIL result.
;;; Returns the actor.

(defun send-any-that-handles (message &rest args)
  (do-society (actor)
    (when (lexpr-send-if-handles actor message args)
      (return actor))))

(defun lexpr-send-any-that-handles (message &rest args)
  (lexpr-funcall #'lexpr-funcall #'send-any-that-handles message args))

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp WITH-LOCK Indent:1;
;;; Lisp DO-SOCIETY Indent:1;
;;; Lisp WITH-DELEGATE Indent:1;
;;; End:;
