;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>STANDARD-ACTOR.LISP.1145, 3 June 1984 23:52:21
;;; PDF pages 20-32 (printed source pages 1-13).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; STANDARD-ACTOR: Basic actor definitions.

;;;; Vocabulary
(define-action report (status) "Requests a status report."
               "I don't know how to compute a status report.")

;;;; Actor I/O Mixin
;;; %%% Message Passing %%%
;;; I think most of this mixin can go away when the real message passing
;; goes in. That should eliminate conflicts of multiple processes
;;; typing asynchronously on same window. The only thing that wants to
;;; stay is the WINDOW, REMARK-WINDOW, and TYPEOUT-WINDOW stuff.
;;; KMP -- 12/31/83
(defflavor actor-io-mixin ((io-lock nil)
                           (window nil)
                           (window-flavor nil)
                           (remark-window nil)
                           (typeout-window nil))
  ()
  :initable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables)
(defmethod (actor-io-mixin :after :init) (&rest ignore)
  (setq window (or window
                   (if window-flavor (tv:make-window window-flavor
                                                     ':delegate self))
                   (make-syn-stream 'terminal-io)))
  (setq typeout-window (or typeout-window
                           (send-if-handles window ':typeout-window)
                           window) )
  (setq remark-window  (or remark-window
                           (send-if-handles window ':remark-window)
                           typeout-window
                           window) ))
;;;   : WITH-I0-LOCK message ...message-args...
;;;  General hook into the IO-LOCK facility.
;;;  Does a given message with given args while holding onto an I/O lock.
(defmethod (actor-io-mixin :with-io-lock) (message &rest args)
  (with-lock (io-lock :whostate "Await IO")
    (lexpr-send self message args)))
;;; :REMARK format-string ...format-args...
(defwhopper (actor-io-mixin :remark) (&rest args)
  (with-lock (io-lock :whostate "Await IO")
    (lexpr-continue-whopper args)))
(defmethod (actor-io-mixin :remark) (format-string &rest format-args)
  (send self ':prepare-remark-window remark-window)
  (lexpr-funcall #'format remark-window format-string format-args) )
(defmethod (actor-io-mixin :prepare-remark-window) (w)
  (send w ':clear-screen) )
(define-interpretation (actor-io-mixin remark) (string)
  (send self ':remark "~A" string))
(comment ;not needed? -kmp 4/4/84
 (define-behavior (actor-io-mixin remark) (string)
   (send self ':remark "~A" string))
 ); tnemmoc
;;; :TYPEOUT format-string ...format-args...
(defwhopper (actor-io-mixin :typeout) (&rest args)
  (with-lock (io-lock :whostate "Await IO")

    (lexpr-continue-whopper args)))
(defmethod (actor-io-mixin :typeout) (format-string &rest format-args)
  (send self ':prepare-typeout-window typeout-window)
  (lexpr-funcall #'format typeout-window format-string format-args))
(defmethod (actor-io-mixin :prepare-typeout-window) (w)
  (send w ':clear-screen))
(define-interpretation (actor-io-mixin typeout) (text)
  (send self ':typeout "~A" text))
(comment ;not needed? ~kmp 4/8/84
 (define-behavior (actor-io-mixin typeout) (string)
   (send self  ':typeout "~A" string))
 );tnemmoc

;;;; Actor Process Window Mixin
(defflavor actor-background-process-mixin ((process-window nil)
                                           (process? t)
                                           (priority 0))
  ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)
;;; :INIT
;;; Assures that a window with a process gets created if one
;;; was not given as an init option.
(defmethod (actor-background-process-mixin :after :init) (&rest ignore)
  (when (and process?  (not process-window) )
    (setq process-window
          (tv:make-window 'actor-window
                          ':name (text "Background KBE: ~A"
                                       (send self ':short-name) )
                          ':delegate self
                          ':priority priority) )
    (send process-window ':run-reason self)))
;;; :PROCESS
;;;  Returns the associtaed process,
;;;     or NIL if there is no associated process.
(defmethod (actor-background-process-mixin :process) ()
  (send (send self ':process-window) ':process))
;;; :WHOSTATE
;;;  Returns the whostate of the associated process,
;;;    or NIL if there is no associated process.
(defmethod (actor-background-process-mixin :whostate) ()
  (let ((p (send self ':process)))
    (if p (send p ':whostate))))
;;; :PROCESS-STATE
;;; Returns two values describing this actor's internal state:
;;;
;;;  (1) A string describing this actor's state.
;;;    (2) A flag saying whether this string is likely to be interesting
;;;         to the user if he hasn't specifically requested it. This
;;;        flag is NIL if the actor is 'happy' (active or bored) and T
;;;        otherwise (meaning the actor is blocked in some state that may
;;;       require manual intervention to clear).
 ;;; This is actually subject to a timing screw since the stack group might
 ;;; change between the time we poll for a whostate and the time we check
 ;;; the stack group, but I don't know the right primitives to get around
 ;;; this problem and since the information is never used by a program,
 ;;; (ie, is only intended to be heuristically interesting to human users)
 ;;; I'm going to not worry about that for now. ~-KMP
(defvar *unknown-process-states* nil "For debugging only.")
(defmethod (actor-background-process-mixin :process-state) ()
  (let ((process (send self ':process)))
    (let ((state (send process ':whostate)))
      (cond ((equal state "Bored")
             (values "bored" nil))

            ((equal state "RUN")
             (values "active" nil))
            ((equal state "Lock")
             (values "blocked awaiting some resource" nil))
            ((or (%string-equal state 0 "Await" 0 5)
                 (%string-equal state 0 "Avoid" 0 5))
             (values (text "blocked to ~A" (string-downcase state))
                     nil))
            ((or  (%string-equal state 0 "CONNECT" 0 7)
                  (%string-equal state 0 "NET" 0 3)
                  (%string-equal state 0 "FILE" 0 4))
             (values "negotiating with the Chaosnet" nil))
            ((equal state "TYI")
             (values "blocked awaiting keyboard input" t))
            ((sys:sg-previous-stack-group (send process ':stack-group))
             ;;; We assume here that our actor runs in a toplevel stack
             ;; group and if he's in any recursive stack group, it must
             ;; be a debugger stack group. This turns out to be a poor
             ;; assumption, but it's better than nothing...
             (cond ((equal state "Selected")
                    (values "waiting to be debugged" t))
                   (t
                    (values "being interactively debugged" t))))
            (t
             (push state *unknown-process-states*)
             (values (format nil "blocked in state ~S" state) t))))))
;;; :RESET
;;;  Resets this actor's process.
(defmethod (actor-background-process-mixin :reset) ()
  (send process-window ':reset)
  t)
;;; :SELECT
;;; Selects some window associated with this actor.
(defmethod (actor-background-process-mixin :select) ()
  (send process-window ':select))

;;;; Members of Society
;;; SOCIETY-MEMBER-MIXIN
(defflavor society-member-mixin (society) ()
           :initable-instance-variables
           :settable-instance-variables
           :gettable-instance-variables
           (:default-init-plist
               :society *society*))
;;; :INIT-SOCIETY
;;; This message allows a hook so that an actor can decide to let
;;; its associated society know that it is a member or not. Most
;;; kinds of actors will just want to insert themselves into the
;;; society. Others which like to remain anonymous, might redefine
;;; this method to be a no-op.
(defmethod (society-member-mixin :after :init) (&rest ignore)
  (send society ':insert self))
;;; :NAME? name
;;; Each society member must answer this message so that the whole
;;; society can be polled for such a member.
(defmethod (society-member-mixin :name?) (test-name)
  (or (string-equal test-name (send self ':name))
      (string-equal test-name (send self ':short-name))))

;;;; Actors
;;; ESSENTIAL-ACTOR
;;; This flavor has a bunch of common things that most actors want.
(defflavor essential-actor () (society-member-mixin actor))
(defflavor basic-actor ()
  (actor-io-mixin
   believer-mixin
   default-vocabulary
   essential-actor))
(define-system-initialization default-delegate
  (setq *delegate* (make-instance 'basic-actor)))
;;; ANSWER
(define-behavior (basic-actor answer) (data)
  (let ((text (cond ((stringp data) data)
                    ((not (atom data))
                     (get data ':text)))))
    (if text (typeout text))))
;;; COMPLAINT
(define-behavior (basic-actor complaint) (data)
  (remark (text "~@[~A: ~]Error! ~A"
                (send self ':current-client)
                data)))
;;; >Report status.
;;;  Returns a string suitable for display to the user saying what
;;;  this actor's current state is.
(define-interpretation (basic-actor report) ()
  (let ((box (send self ':inbox)))
    (let ((base 10.) (*nopoint t))
      (text "I am ~A; my inbox contains ~A."
            (send self ':process-state)
            (send box ':describe-storage) ))))
;;; These used to be events but now are simple functions.
(define-behavior (basic-actor suggest-code-home) (code-name where)
  (if (not (belief ':suggested-code-home code-name) )
      (believe ':suggested-code-home code-name where) ))
(define-behavior (basic-actor suggested-code-home) (code-name)
  (return (belief ':suggested-code-home code-name) ))

;;; Standard Actor
;;; Like Basic Actor, but with a process.
(defflavor standard-actor ()
  (actor-background-process-mixin basic-actor))

;;;; For Employers
(define-vocabulary employer-vocabulary (&rest deep-structure)
  (send self ':defer-task (unparse-command deep-structure)))
(defflavor employer-mixin ((associated-apprentice nil))
  (employer-vocabulary)
  :initable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables)
(define-behavior (employer-mixin associated-apprentice) ()
  (return (send self ':associated-apprentice)))
;;; TASK
;;; Handles requests by submitting them to  its apprentice.
(define-behavior (employer-mixin task) (request)
  (return (eval-command (parse-command request) )))
(defmethod (employer-mixin :submit-by-proxy) (text &optional await?)
  await?  ; ignored
  (send (send self ':inbox) ':send-locked self
        ':submit-by-proxy-internal text await?))
(defmethod (employer-mixin :submit-by-proxy-internal) (text await?)
  (let ((id-list (mapcar #'(lambda (command)
                             (send self ':defer-task
                                   (unparse-command command) nil))
                         (read-commands-from-string text))))
    (when await?
      (let ((my-inbox (send self ':inbox)))
        (dolist (id id-list)
          (answer (send my-inbox ':await-message id)))))))
(defmethod (employer-mixin :defer-task) (request &optional (await? t))
  (let ((apprentice (send self ':find-apprentice nil)))
    (cond ((not apprentice)
           (complain "Can't find an apprentice!"))
          ((not await?)
           (tell apprentice task request) )
          (t
           (ask apprentice task request)))))
(defun reset-apprentice () ;for debugging
  (let ((obj (parse-actor (associated-apprentice))))
    (cond ((not obj) "No associated apprentice to reset.")
          ((not *debug*)
           (text "Not going to reset ~A because not in debug mode." obj))
          ((send-if-handles obj ':reset) (text "~A has been reset." obj))
          (t (text "~A could not be reset." obj)))))
(defun select-apprentice () ;for debugging
  (let ((obj (parse-actor (associated-apprentice))))
    (cond ((not obj) "No associated apprentice to select.")
          ((not *debug*)
           (text "Not going to select ~A because not in debug mode." obj))
          ((send-if-handles obj ':select) (text "Selecting ~A..." obj))
          (t (text "~A could not be selected." obj)))))
;;; :REMARK
;;; :TYPEOUT
;;; %%% Message Passing %%%.
;;;  These whoppers have to do with the fact that these methods sometimes
;;;  get called from the wrong process. That's something dirty which
;;; should be cleaned up one day. --kmp

(defwhopper (employer-mixin :remark) (&rest args)
  (with-user-package (lexpr-continue-whopper args)))
(defwhopper (employer-mixin :typeout) (&rest args)
  (with-user-package (lexpr-continue-whopper args)))
;;; (EMPLOYER-MIXIN :FIND-APPRENTICE) &rest specs
;;; Should find an appropriate apprentice, or NIL if none available.
;;; For now, let's cheat and just know a reasonable name.
(define-KBE-option *default-apprentice*
    "Name of default apprentice"
  "Apprentice 1"  :sexp)
(defmethod (employer-mixin :find-apprentice) (&optional specs)
  specs ;ignored, for now at least
  (or associated-apprentice
      (setq associated-apprentice *default-apprentice*) ))
;;; :WARN format-string format-args...
;;; This message is sent to warn about an unusual but  non-fatal
;;; condition.
(defmethod (employer-mixin :warn) (from string)
  (send self  ':remark "~A: ~A" from string))

;;;; Alternate Window Type
(defflavor actor-window ((priority 0))
  (delegate-mixin
   associated-process-mixin
   tv:window)
  :initable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables
  (:default-init-plist
      :save-bits t
    :activate-p t
    :deexposed-typeout-action ':permit
    :more-p nil))
(defmethod (actor-window :run-reason) (reason)
  (send (send self ':process) ':run-reason reason))
(defmethod (actor-window :reset) ()
  (send (send self ':process) ':reset))
(defmethod (actor-window :process-toplevel) ()
  (send current-process ':set-priority priority)
  (with-delegate (send self ':delegate)
    (send-delegate ':read-messages)))
(defmethod (actor-window :selectable-windows) ()
  (if *debug* (list (list (send self ':name-for-selection) self))))
(defmethod (actor-window :name-for-selection) ()
  (send self ':name))
(compile-flavor-methods essential-actor
                        basic-actor
                        standard-actor
                        actor-window)

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; Lisp WITH-LOCK Indent:1;
;;; End:;
