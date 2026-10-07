;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>MAGIC-ACTOR.LISP.115, 29 May 1984 19:53:36
;;; PDF pages 67-71 (printed source pages 1-5).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; APPRENTICE: A description of the Apprentice actor's behavior.

;;; The Apprentice Actor
;;; APPRENTICE
+>; An apprentice can work for several clients.
;;; In addition to normal actor-world requests, apprentices
;;; are especially good at TASK requests, which are commands
;;; written in the KBE's pseudo-English dialect which cause the
;;; apprentice to analyze, manipulate, or synthesize code
;;; or documentation.
(defflavor apprentice ((focus-info '()))
  (programmer-vocabulary standard-actor)
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:default-init-plist
      :priority -1        ; Low priority so as not to interfere with user.
    ))
(defmethod (apprentice :after :init) (&rest ignore)
  (add-to-mailing-list "Programmers" (send self ':short-name) nil))
(define-system-initialization apprentice
  (make-instance 'apprentice))
(defwhopper (apprentice :event) (&rest args)
  (with-kbe-package ;sigh
      (lexpr-continue-whopper args)))
;;; :PREPARE-REMARK-WINDOW  remark-window
;;; :PREPARE-TYPEOUT-WINDOW remark-window
;;; We just want a new line before remarks and typeout.
;;; No point in clearing any more of screen than we have to,
;;; since just about anything might be good for debugging.
(defmethod (apprentice :prepare-remark-window) (w)
  (send w ':fresh-line))
(defmethod (apprentice :prepare-typeout-window) (w)
  (send w ':fresh-line))

;;; Apprentice: Idiosyncratic Behaviors
;;; TASK request
;;; This handles a text request.
(defun apprentice-eval-command (command myself)
  (bind-focus (send myself  ':current-client-focus)
              (with-user-package
                  (with-reply-to (send myself  ':current-message)
                    (eval-command command) ))))
(define-behavior (apprentice task) (text)
  (return (apprentice-eval-command (parse-command text) self)))

;;;; Per-Client Focus Maintenance
;;; An apprentice may be working for more than one client.
;;; In order to keep things straight with regard to their
;;; focus info, it uses the following abstraction internally:
;;; :CURRENT-CLIENT-FOCUS
;;; Reads  the focus info for the current client.
;;; :SET-CURRENT-CLIENT-FOCUS data
;;; Sets the focus for the current client to some given DATA.
;;; Implementation Notes: -
;;; It is not necessary to specify the amount and format
;;; of the data. The only we guarantee here is that a bundle of
;;; per-client data that is saved at one time can be retrieved
;;; Jater by someone understanding the saver's format.
;;; Normally a plist would be easier to manipulate than an alist
;;; for this sort of application, but we need an alist here because
;;; we want the EQUAL lookup of ASSOC, rather than the EQ lookup
;;; we would get with GET/PUTPROP.
(defmethod (apprentice :current-client-focus) ()
  (cadr (assoc (send self ':current-client) focus-info)))
(defmethod (apprentice :set-current-client-focus) (data)
  (let ((client (send self ':current-client)))
    (let ((entry (assoc client focus-info)))
      (cond ((not entry)
             (push (list client data) focus-info))
            (t
             (setf (cadr entry) data))))))

;;; Initialization
;;; Request compile-time compilation of flavors. By default, the      :      5     :        =  :    ,
;;; methods would not get compiled until fasload-time otherwise.   ee
(compile-flavor-methods apprentice)
