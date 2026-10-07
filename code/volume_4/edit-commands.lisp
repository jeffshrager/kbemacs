;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>EDIT-COMMANDS.LISP.414, 21 December 1984 18:28:23
;;; Knowledge-Based_Editor_Jan-1985_volume_4.pdf, PDF pages 164-176.

;;; (c) Copyright 1984, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; EDIT-COMMANDS: Editor commands for KBE interaction.

;;;; Support routines

(defvar *saved-requests* '() "Used by yank stuff later in this file.")

(defun read-command-for-editor (stream)
  (condition-case (cond)
      (read-command stream)
    (error (editor-barf "~A" cond))))

(defun read-commands-from-string (string)
  (with-input-from-string (stream string)
    (read-commands-from-stream-contents stream)))

(defun read-commands-from-stream-contents (stream)
  (do ((l nil (cons r l))
       (r (read-command-for-editor stream)
          (read-command-for-editor stream)))
      ((null r) (nreverse l))))


;;; This PRESCAN-FUNCTION idea will do for now, though in fact it
;;; is probably more a property of the actor involved than of
;;; the function call which invokes us.

(defun assure-buffered-definitions-consistent (requests)
  (let ((referenced-definitions '())
        (requests-to-queue '()))
    (when requests
      (multiple-value-bind (refs refs?)
          (imbedded-references (car requests))
        (cond (refs
               (setq referenced-definitions refs))
              (refs?
               (let ((local-ref (current-editor-definition-name)))
                 (if (symbolp local-ref)
                     (setq referenced-definitions (ncons local-ref))))))))
    (dolist (request (cdr requests))
      (setq referenced-definitions (union
                                    (imbedded-references request)
                                    referenced-definitions)))
    (dolist (ref referenced-definitions)
      (let ((def (get-definition ref)))
        (cond (def
               (send-delegate ':believe ':buffered-code ref def)
                  (push (text "Analyze ~A .~%"
                              (unparse-command `(code ',def)))
                        requests-to-queue))
              (t
               (send-delegate ':believe ':suggested-code-home ref
                              (let ((name
                                     (current-editor-definition-name)))
                                (append
                                 (if name `(:near ,name))
                                 `(:buffer
                                   ,(buffer-name
                                     (current-editor-buffer))))))))))
    requests-to-queue))

(defun buffer-command-prescan (requests)
  (let ((focus (presentational-focus)))
    (suggest-focus nil) ;Void old focus info
    (let ((special-requests
           (assure-buffered-definitions-consistent requests)))
      (append special-requests
              (when special-requests
                (cond ((not focus) '())
                      (t
                       (list (text "Locate $~S ." focus)))))))))

;;; (SUBMIT-STREAM-CONTENTS-TO-KBE stream [await? NIL] [focus NIL])
;;;
;;;  A basic utility function. This does repeated sentence reads from the
;;;  given stream until it is empty, submitting each sentence as it goes.
;;;  Returns as soon as the requests are queued.
;;;
;;;  The return value is the number of commands queued.

(defun submit-stream-contents-to-KBE (stream
                                      &optional
                                        (await? nil)
                                        (prescan-function nil))
  (let ((requests (read-commands-from-stream-contents stream)))
    (let ((p (if prescan-function (funcall prescan-function requests))))
      (let ((text (apply #'string-append
                         (append p
                                 (list #\return)
                                 (mapcar #'unparse-command requests)))))
        (send-delegate ':submit-by-proxy text await?))
      (length requests))))

;; Things to do (KMP, 4/17/84)
;;
;; This should probably catch "a" references such as "Define a program..."
;; since we are conservative about what we do with non-existent definitions
;; anyway. Then we could use the info about what was referred to in order
;; to figure out what we were needing to update.
;;
;; Also, we could use that
;; info in order to suggest code homes for definitions being created if
;; they don't yet exist. Suggesting such homes for things even if they
;; were definite references to things supposed to already exist wouldn't
;; be such a bad thing, I suppose.
;;
;; --- Ideas above this line have been more or less done. ---
;;
;; Editing actor should handle the entire spectrum of things it gets
;; back by just processing them locally and maintaining a queue of things
;; that the interface actor can ask about. The interface should just
;; batch requests to it, prescanning them and appending any relevant
;; LOCATE and ANALYZE requests. Upon return (success or failure) it should
;; get the most accurate data about definitions from the editing actor
;; and insert them in the buffer. It should also check for and perform
;; typeout. It should also maybe offer (query) to update other definitions.
;;
;; Think more about the PROGN nature of the current TASK setup. Perhaps
;; there needs to be a verb to accomplish paragraphing. People do this
;; differently. What do replies to paragraphs look like? Lists? Multiple
;; values? ...
;;
;; Super-Hyper-Control-Meta-Resume should be fixed to query the editing
;; actor about known definitions and use that data.
;;
;; Reinitialize should locally clear the editor buffers of definitions.
;;  Editing actor should clear its database.
;;  Finally any apprentice(s) should reinitialize (if not shared?).

(defun imbedded-references (request)
  (declare (values references references?))
  (selectq (car request)
           ((DEFINE)
            (values (list (caddr (caddr request))) nil))
           (otherwise
            (let ((*any-reference?* nil))
              (declare (special *any-reference?*))
              (values (mapcan #'imbedded-references-1 (cdr request))
                      *any-reference?*)))))

(defun imbedded-references-1 (phrase)
  (declare (special *any-reference?*))
  (if (atom phrase) '()
      (selectq (car phrase)
               ((it this that here top)
                (setq *any-reference?* t)
                '())
               ((root-reference)
                (setq *any-reference?* t)
                (let ((name (caddr (caddr phrase))))
                  (if name (ncons name))))
               ((relative-reference)
                (setq *any-reference?* t)
                (imbedded-references-1 (nth 5 phrase)))
               (otherwise
                '()))))

;;; (SUBMIT-STREAM-CONTENTS stream)
;;;
;;;  Submits contents of stream to KBE.

(defun submit-stream-contents (stream)
  (assure-real-editor-buffer)
  (acknowledge-having-queued-stream
   (submit-stream-contents-to-KBE stream nil #'buffer-command-prescan)))

;;; (EXECUTE-STREAM-CONTENTS stream [update-buffer? nil])
;;;
;;;  A basic utility function. This does repeated sentence reads from the
;;;  given stream until it is empty, then submits the bundle of requests
;;;  as a priority request for immediate execution by the KBE. Doesn't
;;;  return until the requests are done.

(defun execute-stream-contents (stream &optional (update-buffer? t))
  (assure-real-editor-buffer)
  (let ((*pending-typeout-notification* nil)
        (*pending-code-notification* nil))
    (submit-stream-contents-to-KBE stream t #'buffer-command-prescan)
    (when update-buffer?
      (resume-KBE-edits nil)
      (editor-echo-output "Done."))))

;;; Common utility

(defun acknowledge-having-queued-stream (n)
  (cond ((zerop n)
         (editor-echo-output "No requests accepted."))
        (t
         (editor-echo-output
          "~D request~:P accepted. Type Hyper-S to see queue status.~
          ~%Type Hyper-Resume to obtain results when ~A."
          n (if (equal *pending-typeout-notification*
                       *pending-code-notification*)
                *pending-code-notification*
                (text "~A or ~A"
                      *pending-code-notification*
                      *pending-typeout-notification*))))))


(defun cursor-location ()
  (when (real-editor-buffer? (current-editor-buffer))
    (multiple-value-bind (name type)
        (current-editor-definition-name)
      (multiple-value-bind (path toplevel?)
          (current-editor-path)
        (let ((code (get-definition name)))
          (or (visible-cursor-location code (if (not toplevel?) path nil))
              (if (and name toplevel?)
                  (text "the ~A ~A"
                        (definition-type-name code)
                        name))))))))

(defun visible-cursor-location (code &optional (path '()))
  (funcall (or (get 'visible-cursor-location (buffer-editing-type))
               (get 'visible-cursor-location ':lisp))
           code path))


(defun (visible-cursor-location :ada) (&rest ignore)
  (multiple-value-bind (start-pt end-pt)
      (bounds-of-definition)
    (when start-pt
      (setq start-pt (editor-point)) ;Don't really care where start is
      (when (zwei:bp-< start-pt end-pt)
        (let ((found-pt (search-buffer-between start-pt end-pt "{the")))
          (ignore-errors
            (trim-cursor-location
             (cadr ;pseudo-call to kbe-comment-name
                                        ;new-style format for comment at this point. -kmp,dick
              (read-from-string
               (string-buffer-between (backward-char found-pt 4)
                                      (search-buffer-forward
                                       "}" found-pt)))))))))))

(defun (visible-cursor-location :lisp) (code path)
  (lisp-cursor-location code path))

(defun lisp-cursor-location (code &optional (path '()))
  (trim-cursor-location (*catch 'found-visible-cursor
                                (visible-cursor-location-1 code path))))

(define-KBE-option *abstract-code-references*
    "Specificity of mini-buffer references"
  t :assoc '(("Minimal" . T) ("Maximal" . NIL)))

(defun trim-cursor-location (description)
  (when description
    (let ((high-node (cond ((or (not *abstract-code-references*)
                                (not (memq 'of description)))
                            description)
                           (t
                            (do ((d description)) (nil)
                              (setq d (or (cdr (memq 'of d))
                                          (return d))))))))
      (if (string-equal (car high-node) 'the)    (pop high-node))
      (if (string-equal (car high-node) 'input)  (pop high-node))
      (if (string-equal (car high-node) 'output) (pop high-node))
      (text "the ~{~A~^ ~}" (mapcar #'string-downcase high-node)))))

(defun visible-cursor-location-1 (code path)
  (cond ((atom code) nil)
        ((and (kbe-comment? code)
              (kbe-comment-needs-implementation? code))
         (*throw 'found-visible-cursor (caddr code)))
        (t
         (let ((rest-of-path (cdr path)))
           (dolist (c (nthcdr (or (car path) 0) code))
             (visible-cursor-location-1 c rest-of-path)
             (setq rest-of-path nil))))))

(defun string-for-command (prefix suffix)
  (let ((string (cursor-location)))
    (cond (string (text "~A~A~A" prefix string suffix))
          (t prefix))))


(defvar *in-command-mini-buffer* nil)

(defvar *going-to-submit* ':undefined ;Global binding can't be relied upon.
  "Used internally by KBE Super- and Hyper- commands.")

(defmacro when-command-setup-needed-for (command &body forms)
  `(cond ((not (real-editor-buffer? (current-editor-buffer)))
          (simple-buffer-insert ,command)
          t)
         (t
          ,@forms)))

(defun simple-buffer-insert (thing)
  (when (not (equal thing ""))
    (with-buffer-pointer (start-pt (editor-point))
      (if (sentence-terminator? (char-before (editor-point)))
          (insert-into-buffer-before-point " "))
      (do ((pt (editor-point) (backward-char pt)))
          ((not pt))
        (let ((c (char-before pt)))
          (when (not (whitespace-char? c))
            (if (not (sentence-terminator? c))
                (insert-into-buffer-before-point
                 (if (OK-to-append-delimiter-directly?
                      (string-buffer-between (beginning-of-line)
                                             (end-of-line)))
                     ". " " . ")))
            (return))))
      (insert-into-buffer-before-point (text "~A" thing))
      (fill-buffer-between start-pt (editor-point)))))

(defun going-to-submit (val)
  (when *in-command-mini-buffer*
    (cond ((and *going-to-submit* (not val))
           (set-mode-line "Execute KBE commands:"))
          ((and (not *going-to-submit*) val)
           (set-mode-line "Submit KBE commands:"))))
  (setq *going-to-submit* val))

(defun send-stream-contents (stream)
  (remark "") ;Make sure we are in a fresh remark context
  (cond (*going-to-submit*
         (submit-stream-contents stream))
        (t
         (execute-stream-contents stream))))

(defun immediate-command? (string)
  (let ((len (string-length string)))
    (and (> len 0) (memq (aref string (1- len)) '(#/. #/! #/?)))))

(defun maybe-send-command (string)
  (cond ((immediate-command? string)
         (let ((text (or (if (> (array-active-length string) 60.)
                             (text "~A ..." (substring string 0 56.)))
                         string)))
           (let ((pos (string-search-char #\return text)))
             (when pos
               (setq text (text "~A ..." (substring text 0 pos)))))
           (editor-echo-output "~&~A ^^ ~A"
                               (bucky-char-name (editor-last-command-char))
                               text))
         (with-input-from-string (str string)
           (send-stream-contents str))
         t)
        (t nil)))

(defun editor-prompt-and-execute (string arg)
  (going-to-submit nil) ;maybe side-effect an outer binding.
  (when-command-setup-needed-for string
                                 (let ((*going-to-submit* nil)
                                       (*in-command-mini-buffer* t))
                                   (when (or arg (not (maybe-send-command string)))
                                     (send-stream-contents
                                      (mini-buffer-stream "Execute KBE commands:" t string))))
                                 nil))

(defun editor-prompt-and-submit (string arg)
  (going-to-submit t) ;maybe side-effect an outer binding.
  (when-command-setup-needed-for string
                                 (let ((*going-to-submit* t)
                                       (*in-command-mini-buffer* t))
                                   (when (or arg (not (maybe-send-command string)))
                                     (send-stream-contents
                                      (mini-buffer-stream "Submit KBE commands:" t string))))
                                 nil))

(defun request-command-key (name await?)
  (if await?
      (symbolconc 'com-KBE-execute- name)
      (symbolconc 'com-KBE-submit- name)))

(defvar *simple-mini-buffer-command* nil)

(defdefmacro define-editor-request "KBE editor request" (name keys . body)
             (check-arg keys (or (fixp keys) (and (listp keys)
                                                  (every keys #'fixp)))
                        "a character or list of characters")
             (if (not (stringp (car body)))
                 (ferror "First form in body must be a string:~% ~S" (car body)))
             (let ((name-for-execute (symbolconc 'com-KBE-execute- name))
                   (name-for-submit  (symbolconc 'com-KBE-submit- name)))
               (if (fixp keys) (setq keys (list keys)))
               `(local-declare ()
                               (defprop ,name-for-execute ,(car body) mini-buffer-text)
                               (defprop ,name-for-submit  ,(car body) mini-buffer-text)
                               (defprop ,name-for-execute nil await?)
                               (defprop ,name-for-execute t   await?)
                               (defprop ,name-for-submit  ,name-for-execute execute-command)
                               (defprop ,name-for-execute ,name-for-submit  submit-command)
                               ,(if (equal (car body) "")
                                    `(setq *simple-mini-buffer-command*
                                           (cons ',name-for-submit ',name-for-execute)))
                               (define-editor-command ,name-for-execute (arg)
                                   ,(mapcar #'(lambda (c) (add-bucky-bits c ':super)) keys)
                                 document-mini-buffer-execute-command
                                 (check-sectionization)
                                 (editor-prompt-and-execute (progn ,@body) arg))
                               (define-editor-command ,name-for-submit (arg)
                                   ,(mapcar #'(lambda (c) (add-bucky-bits c ':hyper)) keys)
                                 document-mini-buffer-submit-command
                                 (check-sectionization)
                                 (editor-prompt-and-submit (progn ,@body) arg))
                               ',name)))

(defun document-mini-buffer-execute-command (name char mode)
  char ;ignored
  (cond ((eq mode ':name) (editor-command-name name))
        ((neq mode ':full)
         (format t "KBE execute: ~A"
                 (get name 'mini-buffer-text)))
        (t
         (document-mini-buffer (get name 'mini-buffer-text) t))))

(defun document-mini-buffer-submit-command (name char mode)
  char ;ignored
  (cond ((eq mode ':name) (editor-command-name name))
        (t
         (let* ((execute-name (get name 'execute-command))
                (execute-key
                 (if execute-name (command-key-name execute-name))))
           (cond (execute-key
                  (format t
                          "Like ~A, but to submit rather than to execute."
                          execute-key))
                 ((neq mode ':full)
                  (format t "KBE submit: ~A"
                          (get name 'mini-buffer-text)))
                 (t
                  (document-mini-buffer (get name 'mini-buffer-text)
                                        nil)))))))

(defun simple-command-key (await?)
  (when *simple-mini-buffer-command*
    (command-key-name
     (cond (await? (cdr *simple-mini-buffer-command*))
           (t (car *simple-mini-buffer-command*))))))

(defun document-mini-buffer (text await?)
  (let ((immediate? (immediate-command? text)))
    (cond ((let ((b (current-editor-buffer)))
             (and b (not (real-editor-buffer? b))))
           (format t "Set up to ~:[submit~;execute~] text~:[:  ~A~;.~]~%"
                   await? (equal text "") text))
          ((equal text "")
           (format t "Enters a mini-buffer reading text for the KBE.~
                    ~%When mini-buffer is exited, ~:[submits~;executes~] ~
                      the KBE commands it contains.~%"
                   await?))
          ((not immediate?)
           (let ((key (simple-command-key await?)))
             (if key
                 (format t "Like ~A, but the mini-buffer initially contains:~
                           ~%  ~A~%"
                         key text)
                 (format t "Enters a mini-buffer initially containing:~
                           ~2%  ~A~
                           ~2%When mini-buffer is exited, ~
                              its text is ~:[submitted to~;executed by~] ~
                              the KBE."
                         text await?))))
          (t
           (format t "Immediately ~:[submits~;executes~] KBE command:  ~A~
                    ~%With an explicit arg, allows confirmation//editing ~
                      in mini-buffer."
                   await? text)))))


(define-editor-request Anything (#/X #\meta-X)
  "")

(define-editor-request Make-Cliche #/C
  "Define a cliche ")

(define-editor-request Make-Defun #/D
  "Define a program ")

(define-editor-request Append-Parameter #/P
  "Append a parameter ")

(define-editor-request What-Needs-To-Be-Done #/W
  "What needs to be done?")

(define-editor-request Finish #\end
  "Finish editing {this definition}"
  (text "Finish editing ~@[~A~]"
        (append-delimiter-if (this-definition-name))))

(define-editor-request Reap-Database #\rubout
  "Reap database.")

(define-editor-request Reinitialize-Database #\clear-input
  "Reinitialize database.")

(define-editor-request Clear-Display #\clear-screen
  "Clear display.")

(define-editor-request Implement #/I
  "Implement {next unfilled role} as "
  (string-for-command "Implement " " as "))

(define-editor-request Move-To (#/. #\meta-/.)
  "Move to ")

(define-editor-request Comment #/;
  "Comment {this definition}."
  (text "Comment ~@[~A~]"
        (append-delimiter-if (this-definition-name))))

(define-editor-request Describe (#/= #\control-=)
  "Describe this.")

(define-editor-request Explain #/E
  "Explain this.")

(define-editor-request Undo #/U
  "Undo.")

(define-editor-request Use-Language #/L
  "Use language ")

(define-editor-request Share #/S
  "Share ")

(define-editor-request Remove #/R
  "Remove {next unfilled role}"
  (string-for-command "Remove " ""))

(define-editor-request Test-Yourself #/T
  "Test yourself.")

(define-editor-request View #/V
  "View {this definition} in {another language}."
  (text "View ~@[~A in ~A.~]"
        (this-definition-name)
        (if (neq *display-language* 'lisp)
            "Lisp" "PLI")))

(define-editor-request Add #/A
  "Add ")

(defun definition-type-name (code)
  (selectq (car code)
           (define-cliche "cliche")
           (def-ada-top
               (ada-definition-type-name code))
           (otherwise "function")))

(define-editor-request Analyze #/Z
  "Analyze ${text of current definition}."
  (let ((name (current-editor-definition-name)))
    (let ((code (get-definition name)))
      (ignore-errors ;clumsy, but effective for now -kmp
        (send-delegate ':believe ':buffered-code (cadr code) code))
      (if (not code) (editor-barf "Can't grok context."))
      (append-delimiter
       (text "Analyze ;the ~A ~A~% ~A"
             (definition-type-name code) name
             (unparse-command `(code ',code)))))))

;;;; Misc foreground-related commands

;;; These should be doing Execute-style requests rather than doing the
;;; stuff in foreground ...

(defun this-definition-name ()
  (multiple-value-bind (name type)
      (current-editor-definition-name)
    (if name (text "the ~A ~S"
                   (definition-type-name (get-definition))
                   name))))

;;;; Commands for manipulating KBE submit requests

;;; Hyper-Resume
;;;
;;;  Completes the edits which have come as replies to any requests
;;;  which were sent to the Apprentice by a 'submit' style request
;;;  (or an aborted 'execute' style request).

(define-editor-command com-resume-KBE-edits (confirm?)
    #\Hyper-Resume
  "Complete any requests which were done in background."
  (resume-KBE-edits confirm?))

(defun resume-KBE-edits (confirm?)
  (assure-real-editor-buffer)
  (send-delegate ':finish-requests confirm?))

;;; Hyper-Status
;;; Hyper-S
;;;
;;;  Gives information about the status of requests that have
;;;  been sent to the KBE.
;;;
;;;  With an argument, also gives information about the
;;;  apprentice's status.

(define-editor-command com-KBE-status (ignore)
    (#-3600 #\Hyper-Status #\Hyper-S)
  "Inquires about the status of the KBE requests."
  (editor-echo-output "~A: ~A"
                      (send-delegate ':short-name) ;say who we are
                      (task "Report status."))
  nil) ;no changes to buffer
