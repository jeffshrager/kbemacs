;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>REMOTE.LISP.79, 12 March 1984 03:28:56
;;; PDF pages 33-38 (printed source pages 1-6).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; REMOTE: Module for hacking apprentices, etc. from afar.
;;; This isn't the cleanest possible implementation, but it has come
;;; a long way...

;;; Remote Actor Support
;;; REMOTE-ACTOR-MIXIN
;;; All remote actors want this mixin. It defines the essential
;;; ingredients that will allow the actor to talk via the Chaos
;;; net to a remote counterpart.
(defflavor remote-actor-mixin (host local-name) ()
           :initable-instance-variables
           :gettable-instance-variables
           :settable-instance-variables)

;;; Remote Actors
;;; REMOTE-ACTOR
;;; This is a full scale actor acting as a go-between to an actor
;;; on a remote host. It is part of the local society structure,
;;; so receives all blast messages sent via the society network.
(defflavor remote-actor () (actor-background-process-mixin
                            remote-actor-mixin
                            essential-actor))
;;; These are messages that will want to be handled remotely.
(comment ;revised to use different strategy. -kmp 2/22/84
 (defmethod (remote-actor :process-message) (message)
   (condition-case (cond)
       (kaos-send (send self ':host) ':local-KBE-dispatch
                  (send self ':local-name) ':queue-message message)
     (sys:error (complain "~A" cond))))
 )
(defmethod (remote-actor :process-message) (message)
  (condition-case (cond)
      (kaos-send (send self ':host) ':remote-service
                 (send si:local-host ':name)
                 (send self ':local-name)
                 message)
    (sys:error (complain "~A" cond))))

;;; Network Handlers
;;; :DELIVER-MESSAGE
;;; This handler puts a given message in a mailbox.
(define-KBE-option *allow-KBE-remote*
    "KBE  remote communication enabled"
  t :boolean)
(defvar *notification-data* '() "Data about who knows about servers.")
(defun remote-service-notify (host)
  (when (or (not user-id) (equal user-id ""))
    (setq user-id "KBE"))
  (when (not *allow-KBE-remote*)
    (complain "Sorry, ~A has disabled remote KBE communication."
              user-id))
  (let ((data (ass #'string-equal user-id *notification-data*)))
    (when (not data)
      (setq data (ncons user-id) )
      (push data *notification-data*) )
    (when (not (mem #'string-equal host (cdr data)))
      (tv:notify nil "The KBE is being used remotely from ~A." host)
      (push host (cdr data)))))
(define-kaos-handler :remote-service (host destination data)
  (remote-service-notify host)
  (deliver destination (make-message data)))
;;; :LOCAL-KBE-DISPATCH local-name msg &rest args
;;; This handler sends the given message with given args to the local
;;; actor with the given LOCAL-NAME, returning the result(s).
(comment ;no longer needed? -kmp 2/22/84
 (define-kaos-handler :local-KBE-dispatch (to-name msg &rest args)
   (let ((actor (parse-actor to-name)))
     (if actor
         (with-delegate actor ;Still needed with message passing?~kmp 2/8/84
           (lexpr-send actor msg args)))))
 )

;;; Making Remote Actors
;;; (PARSE-REMOTE-ACTOR host name)
;;; No remote actors are  initially set up, but they can be created
;;; with this function. eg,
;;;   (PARSE-REMOTE-ACTOR  "Magic-1@MIT-LISPM-47")
;;; finds or creates  a remote actor locally which will try to talk
;;; to an actor named Magic-1  at MIT-LISPM-47.
;;; It will not  create the actor on that machine. That happens upon
;;; demand when  contacting the machine.
;;; If the host component of the name designates the local machine,
;;; a normal local actor (not a remote-actor) will be returned.
;;; This will  try to handle local actors, too, if it thinks it is
;;; not infinitely recursing.
(defun parse-remote-actor (name)
  (multiple-value-bind (actor-name host-name remote?)
      (parse-actor-full-name name)
    (cond (remote?
           (let ((full-name (make-actor-full-name actor-name host-name) ))
             (or (send-any-that-handles ':name? full-name)
                 (make-instance 'remote-actor
                                ':name full-name
                                ':host host-name
                                ':local-name actor-name))))
          ;;; Iff name seems to be shrinking in size, recurse...
          ((< (string-length actor-name) (string-length name) )
           (parse-actor actor-name)))))

;;; Request compile-time compilation of flavors. By default, the
;;; methods would  not get compiled until fasload-time otherwise.
(compile-flavor-methods remote-actor)
;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; Ends;
