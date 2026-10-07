;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>DEBUG.LISP.159, 13 December 1984 14:15:21
;;; PDF pages 246-253 (printed source pages 1-8).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; This package sets up things which are probably worthwhile to have
;;; around for debugging but which are totally independent of anything
;;; the KBE depends on to run, Further, it should not rely on any macros
;;; in the KBE so that it doesn't get recompiled a lot...
;;; This takes in a form and macroexpands all of the macros inside
;;; it, until there are no macros left.  Note that it forces all of the
;;; macros to displace whether they do or not.  It then returns the
;;; result to you.  The hard part is that it has to know about fexprs
;;; in order to know what not to macroexpand.
;;; Note: This function should not be called from code.
;;;       It isn't reliable enough.

;;; Utilities
(defun macroexpandall (thing)
  (cond ((eq (typep thing) 'list)
         (let ((new (macroexpand thing)))
           (rplaca thing (car new)) (rplacd thing (cdr new)))
         (cond ((not (symbolp (car thing)))
                (mapc #'macroexpandall thing))
               (t
                (selectq (car thing)
                         ((QUOTE COMMENT DEFPROP)
                          nil)
                         ((LAMBDA PROG SI:DISPLACED)
                          (mapc #'macroexpandall (cddr thing)))
                         ((DEFUN)
                          (mapc #'macroexpandall (cdddr thing)))
                         ( (COND)
                           (dolist (clause (cdr thing))
                             (mapc #'macroexpandall clause)))
                         ((DO)
                          (dolist (var (cadr thing))
                            (if (not (atom var)) (mapc #'macroexpandall var)))
                          (mapc #'macroexpandall (caddr thing) )
                          (mapc #'macroexpandall (cdddr thing)))
                         (otherwise
                          (mapc #'macroexpandall (cdr thing))))))))
  thing)
(defun ops (x &optional (stream? standard-output) &rest substrings)
  (let ((ops (copylist (if (listp x) x (send x ':which-operations)))))
    (dolist (s substrings)
      (setq ops (mapcan #'(lambda (x)
                            (if (string-search s x) (ncons x)))
                        ops)))
    (setq ops (sort ops #'string-lessp))
    (when stream?
      (let ((fields-for-display
             (let ((package (pkg-find-package "GLOBAL")))
               (let ((sym (elist ops)))
                 (rlist (cond ((eq (symbol-package sym) package)
                               (text ":~S" sym))
                              (t (text "~S" sym))))))))
        (let ((field-width (+ 2 (rmax (string-length
                                       (elist fields-for-display)))))
              (n-fields    (// (or (send-if-handles stream?
                                                    ':size-in-characters)
                                   1)
                               field-width))
              (field-height (// (+ (length fields-for-display)
                                   (1- n-fields))
                                n-fields)))
          (let ((buckets (do ((i 0 (1+ i))
                              (f fields-for-display (cdr f))
                              (b nil)
                              (bs nil))
                             ((null f)
                              (dotimes (i (- (length (car bs))
                                             (length b)))
                                (push "" b))
                              (lexpr-funcall
                               #'mapcar
                               #' list
                               (nreconc bs (ncons (nreverse b)))))
                           (when (= i field-height)
                             (push (nreverse b) bs)
                             (setq b nil)
                             (setq i 0))
                           (push (car f) b))))

            (let ((format-string (text "~&~~{~~{~~~D,' A~~}~%~~}"
                                       field-width)))
              (format stream? format-string buckets))))))
    (cond (stream? (values))
          (t ops))))

;;; Vocabulary
(defun parse-request ()
  (if (eq (peek-token) 'to) (read-token))
  (cond ((atom (peek-token) )
         (unparse-command (parse-sentence)))
        (t
         (parse-name ':expression))))
(define-action tell ((actor (parse-name ':expression))
                     (request (parse-request)))
  "Asks an actor to tell another actor to do something."
  (tell actor task request))
(define-action ask ((actor (parse-name ':expression))
                    (request (parse-request)))
  "Asks an actor to ask another actor to do something."
  (ask actor task request))

;;; Debugging
(defun init (&optional (query? (not *debug*)))
  (cond-every ((or (not query?) (fquery '() "Init database? "))
               (db-init)
               (format query-io "~&Database initialized.~%"))
              ((or (not query?) (fquery '() "Init belief system? "))
               (send *beliefs* ':reset)
               (format query-io "~&Beliefs reset.~%"))
              ((or (not query?) (fquery '() "Init processes? "))
               (do-society (actor)
                 (if (send-if-handles actor ':reset)
                     (format query-io "~&~A's process has been reset."
                             actor))))))
(defun debug (&optional (on? t) (query? t))
  (setq *debug* on?)
  (let ((request (text "~:[Disable~;Enable~] debug mode" on?))
        (query-io terminal-io))
    (format query-io "~&~A for...~%" request)
    (do-society (actor)
      (let* ((name (send actor ':short-name) ))
        (when (and (send-if-handles actor ':process) ;ignore dummy actors
                   (or (not query?) (fquery '() " ~A? " name)))
          (if (not query?) (format query-io "~&~A: " name) )
          (condition-case (cond)
              (ask name task request)
            (error (format query-io " ~A" cond))
            (:no-error (format query-io "  Done.")))))))
  on?)
(defun kbe-1  ()
  (let ((request (prompt-and-read-command)))
    (condition-restart-case (condition-case-if (not *debug*) (cond)
                                               (eval-command request)
                                               (sys:error
                                                (format t "~&<<Request Failed>>~%~A~&"
                                                        cond)
                                                (values) ))
                            (((sys:abort sys:error) "Abort this KBE request.") (values)))))
(defun kbe (&optional (delegate "Apprentice 1") (verbose? t))
  (with-delegate (parse-actor delegate)
    (when verbose?
      (format t "~&Entering the KBE command loop.~
                 ~% Type text commands to ~A or ~C to exit.~:[~
                  ~% Note: You are not in debug mode.~;~]~%"
              delegate #\abort *debug*))
    (condition-restart-case-if verbose?
                               (do () (nil) ;indefinitely
                                 (let ((l (multiple-value-list (kbe-1))))
                                   (unless (not l)
                                     (psetq *   (car l)
                                            **  *
                                            *** **))
                                   (format t "~&~@[^Y ~{~S~^,~%  ~}~%~]"
                                           l)))
                               (((sys:error null-sentence) "Exit the KBE command loop.")
                                (format t "~&Conversation with ~A terminated.~
                   ~%Leaving the KBE command loop.~%"
                                        delegate)
                                (values)))))

;;; Debugging window
(comment ;This doesn't work yet. ~-KMP 3/13/84
 (define-system-initialization debug-frame-system-key
   (add-system-key #/D t   "Debug Frame (KBE)"  'debug-frame) )
 (defflavor debug-pane () (simple-pane-mixin
                           associated-process-mixin
                           actor-window)
            (:default-init-plist
                :delegate-flavor 'standard-actor
              :selectable-p t))
 (defmethod (debug-pane :after :activate) (&rest ignore)
   (send self ':set-label
         (text "~A: ~A"
               (send self ':name)
               (send (send self ':delegate) ':short-name))))
 (defmethod (debug-pane :process-toplevel) ()
   (kbe (send (send self ':delegate) ':name) nil))
 (defflavor debug-frame () (tv:bordered-constraint-frame)
            :initable-instance-variables
            :gettable-instance-variables
            :settable-instance-variables
            (:default-init-plist
                :save-bits t
              :panes '((pane-1 debug-pane)
                       (pane-2 debug-pane)
                       (lisp-pane listener-pane))
              :constraints
              '((main  . ((pane-1 pane-2 lisp-pane)
                          ((lisp-pane 0.25))
                          ((pane-1 0.5))
                          ((pane-2 :even)))))
              :selected-pane 'pane-1))
 )  ;tnemmoc

;;; ADA Read-Eval-Print Loop
;;; (ADA-REPL-PROMPT stream char)
;;; Called  internally by ADA-REPL in order to do prompting.
(define-internal-option *ada-input-marker* "Ada input prompt"
  "" :string)
(defun ada-repl-prompt (stream char)
  char ; ignored
  (format stream "~&~A" *ada-input-marker*))
;;; (ADA-REPL-READ [stream])
;;; Reads an ADA expression (from a given stream, if one is provided).
;;; If possible, does so with rubout-handling enabled.
(define-condition ada-resume ())
(defun ada-repl-read (&optional (stream terminal-io) (break? nil))
  (cond ((send stream ':operation-handled-p ':rubout-handler)
         (ada-repl-prompt stream nil)
         (do () (nil) ;forever
           (cond ((eq (tyipeek) #\c-Z)
                  (tyi) ;Read and echo the char
                  (signal 'sys:abort))
                 ((and break? (= (tyipeek) #\Resume) )
                  (send stream ':tyi)
                  (format stream "~&[Resume]~%")
                  (ada-resume)))
           (condition-restart-case
            (return-if
             (send stream ':rubout-handler  '((:prompt ada-repl-prompt) )
                   #'ada-parse-to-end stream  '(#\End #/;)))
            ((sys:abort "Start over with typing  this Ada statement.")
             (ada-repl-prompt stream nil)))))
        (t
         (ada-parse-to-end stream  '(#\End #/;)))))
;;; (ADA-REPL [show-result? T] [show-result-in-ada?  T])
;;; Runs an ADA Read-Eval-Print Loop.
 ;;; The optional argument SHOW-RESULT?  says whether to show return values.
 ;;; The optional argument SHOW-RESULT-IN-ADA?  says if ADA display is used.
(define-internal-option  *ada-result-marker* "Ada result marker"
  "" :string)
                                        ; " ^Y " :string)
(defun ada-repl (&optional (break? nil)
                   (show-result? t)
                   (show-result-in-ada? t))
  (with-user-package
      (condition-case ()
          (let ((ada-supress (not show-result-in-ada?)))
            (do ((- '-)
                 (* '*)
                 (** '**)
                 (*** '***)
                 (+ '+)
                 (++ '++)
                 (+++ '+++))
                (nil) ;forever
              (condition-restart-case
               (progn (setq - (ada-repl-read standard-input break?))
                      (let ((temp (prog1 (multiple-value-list (eval -))
                                    (setq +++ ++
                                          ++  +
                                          +   -))))
                        (when temp
                          (setq *** **
                                **  *
                                *   (car temp))
                          (when show-result?
                            (let ((base 10.) (*nopoint t))
                              (format t "~&~A~:=" *ada-result-marker* *))))))
               ((sys:abort "Return to Ada command level")
                (format t "~&Back to Ada command level")))))
        ((sys:abort ada-resume) ':resume))))

(defun ada-break-repl (&optional (string nil))
  (format t ">ADA Breakpoint~@[ ~A~]; ~
              Resume to continue, Abort to quit.~2%"
          string)
  (ada-repl t))
(defun (com-mode-break  :ada) () (ada-break-repl))
;;; Define an Ada Listener window
(defflavor ada-listener  () (associated-process-mixin tv:window)
           (:default-init-plist
               :save-bits t))
(defmethod  (ada-listener :process-toplevel) ()
  (with-user-package  (ada-repl)))
 ;;; Define relevant editor commands
(defflavor ada-listener () (associated-process-mixin tv:window)
           (:default-init-plist
               :font-map  '(fonts:bigfnt)
             :save-bits t))
(defmethod (ada-listener :process-toplevel) ()
  (ada-repl))
