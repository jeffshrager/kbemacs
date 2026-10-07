;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>VERBS.LISP
;;; PDF pages 62-84 (printed source pages 1-23).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

(define-internal-option *automatic-reap*
                         "Do a DB-REAP after every command"
  #+3600 nil  ; 3600 is fast enough that this doesn't matter much.
  #-3600 t    ; LM-2 needs all the help it can get, even at the expense
              ; of having Undo not work.
   :boolean)
(defvar *last-db-context* nil "holds old context point for backup")
(def-db-var *undo-stack* nil "...")
(defun top-segcase (segcase)
   (declare (values segcase))
   (repeat ((ts (~seg segcase)) nts)
     (setq nts (?supseg ts))
     (cond ((null nts) (return (list ts)))
           (t (setq ts nts)))))
;;; PDF page 63

(define-db-interpretation how (toplevel-reference)
  (let ((results (copytree (parsed->code toplevel-reference))))
    (when (and *debug* (cdr results))
      (cbarf "Ignore extra code."
             "More than one piece of code for ~A."
             (unparse-command toplevel-reference)))
    (let ((result (car results)))
      `(:code
        :text ,(cond (result (let (prinlevel prinlength)
                               (text "~S" result)))
                     (t (text "I don't know the definition of ~A"
                              (unparse-command toplevel-reference))))
        :definition ,result))))
;;; This is used to create a literal piece of plan.
;;; g,   the arg to ANALYZE is currently a (CODE ...) form.
(defun (code action) (code)
  (if (stringp code)
      (setq code (string-parser code))
      (setq code (cadr code)))
  (list (create-plan-for-code code)))
;;; This is the only entry into the analyzer,etc. which should be used.
;;; Mote the way it takes care of the {a ...} stuff.
(defun create-plan-for-code (lisp-code)
  (declare (special *pending-dflows*))
  (if (eq *pending-dflows* '*top-level*)
      (let (*pending-dflows*)
        (prog1 (create-plan-for-code1 lisp-code)
               (if *pending-dflows* (complain "pending dflows remain"))))
      (create-plan-for-code1 lisp-code)))
;note when we are redefining a whole program,
;we must be carefull not to evaluate the fillers untill
;after we do the surface-analyze and make the result be IT!
;otherwise we must evaluate them first in order to
;get hold of the right referents.
(defun create-plan-for-code1 (lisp-code)
  (let* ((pending-implements (check-for-implements lisp-code))
         (segcase (analyze (surface-analyze lisp-code)))
         (total-redefinition
          (and (listp lisp-code)
               (memq (car lisp-code) '(defun define-cliche def-ada-top)))))
    (if total-redefinition (make-be-it (list segcase)))
    (setq pending-implements
          (mapcar #'(lambda (x)
                      (list (car x) (car (eval-command (cadr x)))))
                  pending-implements) )
    (make-be-it (list segcase))
    (dolist (imp pending-implements)
      (let* ((slot (car imp))
             (slotsegcase (~slotseg (??& segcase slot * slot)))
             (filler (cadr imp)))
        (implement-internal slotsegcase filler)
        (--& * slot * slot)))
    segcase))
(declare (special check-for-implements-list) )
(defun check-for-implements (code)
  (let (check-for-implements-list)
    (check-for-implements1 code)
    check-for-implements-list) )
;this finds instances of {a ...} and queues up implements to do for them,
;;; PDF page 64

;note we are completely punting the problem of not being able to
;fix the slot in the right place in general.  It happens not to
;be a problem in the scenario chosen.
;also this doesn't handle fexprs well.        .
(defun check-for-implements1 (code)
  (cond ((not (listp code)))
        ((and (kbe-comment? code)
              (eq (kbe-comment-type code) ':indefinite))
         (let* ((fake-slot (symbolconc 'slot- (gensym)))
                (filler (parse-command
                          (kbe-comment-name->string
                            (kbe-comment-name code))
                          #'parse-indefinite-reference)))
           (push (list fake-slot filler) check-for-implements-list)
           (rplaca (cddr code) (list 'the fake-slot))))
        (T (do ((list code (cdr list)))
               ((not (listp list)))
             (check-for-implements1 (car list))))))
;;; PDF page 65

;;; >Analyze <code>.
 ;;;
;;; Constructs the code for a plan designated by <reference>.
 ; Issues: Focus probably isn't set right after this.
  ;;; The multiple calls to EVAL-COMMAND need to have the
 ;         command termination signal get deferred.
(define-db-action Analyze ((reference (parse-reference) ))
                  "Makes database consistent with the given <reference>."
  (if (not (eq (car reference) 'code))
      (complain "Non literal reference for Analyze: ~A"
                (unparse-command reference)))
  (let* ((code (cadadr reference))
         (expression (cond ((stringp code) (string-parser code) )
                           (t (cadr code)))))
    (when (or (atom expression)
              (not (memq (car expression)
                         '(defun define-cliche def-ada-top))))
      (complain "Only able to analyze definitions."))
    (assure-db-consistency expression)
    (suggest-focus `(:name  ,(cadr expression)))
    (values)))
 ;This definition might should be elsewhere. -kmp
(defmacro without-end-of-command (&body forms)
  `(condition-bind ((end-of-command #'(lambda (cond)
                                        (send cond ':proceed ':handled))))
     ,@forms))
(defun eval-commands (commands
                      &optional (end-mode ':normal) (fn #'eval-command)
                      &rest args)
  ; END-MODE should be one of :IGNORE, :DEFER, or :NORMAL
  (cond ((null commands) nil)
        ((null (cdr commands))
         (if (eq end-mode ':ignore)
             (without-end-of-command
               (lexpr-funcall fn (car commands) args))
             (lexpr-funcall fn (car commands) args)))
        (T
         (if (neq end-mode ':normal)
             (without-end-of-command
               (lexpr-funcall fn (car commands) args))
             (lexpr-funcall fn (car commands) args))
         (lexpr-funcall #'eval-commands (cdr commands) end-mode fn args))))
(define-internal-option *trap-db-inconsistency*
                        "Trap when CODE-EQUAL fails in DB"
  nil :boolean)
(defun assure-db-consistency (test-code)
  (let* ((name (cadr test-code))
         (good-code (path->code name)))
    (cond ((not good-code) (force-db-consistency test-code))
          ((code-equal test-code good-code)) ;nothing to do
          (t
           (when *trap-db-inconsistency*
             (fsignal "CODE-EQUAL failed.~% Test code: ~:=~% Good code: ~:="
                      test-code good-code))
           (let ((result (compare-code name good-code test-code)))
             (cond (result
                    (eval-commands (with-input-from-string (str result)
                                     (read-commands-from-stream-contents
                                       str))
                                   ':ignore))
                   (t
                    (force-db-consistency test-code))))))
;;; PDF page 66

    (make-be-it (list (name->segcase name)))))
(defun force-db-consistency (expression)
  (code (create-plan-for-code expression) ))
;;; PDF page 67

;;; Utilities
(*define-completion 'here)
(define-db-action add ((reference (parse-reference))
                       (where (progn
                                (if (eq (peek-token) 'here) (read-token))
                                '(here))))
                  "Adds <reference> at the cursor position.
                   The word /"here/" may optionaily follow for euphony."
  where ; ignored.
  (let ((sexp (make-kbe-comment nil (ncons (unparse-command reference))))
        (name (get (locf *here*) ':name))
        (path (get (locf *here*) ':path)))
    (eval-command `(analyze (code '',(splice-code-at-path
                                         (path->code name nil)
                                         path
                                         sexp))))))
(defun splice-code-at-path (old-code path code-to-add)
  (let ((the-copy (copytree old-code)))
    (splice-code-at-path-1 the-copy
                           (or path (complain "Cannot insert code there."))
                           code-to-add
                           nil 0) ;parent info
    the-copy))
(defun splice-code-at-path-1 (old-code path code-to-add parent n)
  (cond ((not path)
         (cond ((not parent) (barf "Parent node not given?"))
               ((zerop n)
                (let ((a (car parent))
                      (d (cdr parent)))
                  (setf (car parent) code-to-add)
                  (setf (cdr parent) (cons a d))))
               (t
                (push code-to-add (nthcdr n parent)))))
        (t
         (splice-code-at-path-1 (nth (car path) old-code)
                                (cdr path)
                                code-to-add
                                old-code
                                (car path)))))
;;; PDF page 68

 ;;; >Compute <reference>.
 ;;; Returns the referent of the given <reference>.
 ;;; Intended mainly for debugging.
(define-db-action compute ((reference  (parse-reference) ))
                   "Computes referent of the given <reference>."
   (copytree (eval-command reference) ))
 ;evals an expr and returns the result.
 ;;; i assume this is mostly for debugging? -kmp
(define-db-action eval ((code (let ((exp (parse-name ':expression)))
    (when (or (atom exp)
    (neq (car exp) 'quote))
                                   (parse-error
                                     "EVAL expects a quoted name."))
                                 exp)))
                   "Calls EVAL on <code>."
   (copytree (eval (eval-command code))))
;;; PDF page 69

(define-db-action explain ((reference (parse-definite-reference)))
                  "Explains the segment designated by <reference>."
  (explain-reference reference) )
(define-KBE-option *explainer* "Explanation format"
  'document-fn :assoc '(("Paragraph" . explain-fn)
                        ("Outline"   . document-fn)))
(defun explanation (fn) (funcall *explainer* fn))
(defun explain-reference (reference)
  (let ((things (eval-command reference)))
    (if (null things)
        (complain "I don't know anything about ~A."
                  (unparse-command reference)))
    (make-be-it things)
    (with-output-to-string (standard-output)
      (dolist (thing things)
        (let ((fn-name (get-toplevel-name thing)))
          (values (format t "~S ...~2%~A~2&"
                          fn-name
                          (format-explanation
                            (explanation fn-name)))))))))
(define-db-action document ((reference (parse-definite-reference)))
                  "Documents the segment designated by <reference>."
  (document-reference reference) )
(define-db-action comment ((reference (parse-definite-reference) ))
                  "Comments the program designated by <reference>."
  (document-reference reference)) ;a more modest name for this.
(defun document-reference (reference)
  (let ((things (eval-command reference) ))
    (if (null things)
        (complain "I don't know anything about ~A,"
                  (unparse-command reference) ))
    (request-display reference)
    (let ((fn-name (get-toplevel-name (car things))))
      (document-code fn-name (explanation fn-name)))
    (values)))
;;; PDF page 70

(define-db-action move (to (reference (parse-definite-reference)))
                  "Moves to the segment designated by <reference>."
  (request-display reference)
  (values))
;Highlight the specified thing(s) on the tv display...
(define-db-action display ((reference (parse-definite-reference) ))
                  "Displays the segment designated by <reference>."
  (request-display reference)
  (values) )
(defun request-display (reference)
  (let ((things (eval-command reference) ))
    (if (null things)
        (complain "I don't know anything about ~A."
                  (unparse-command reference) ))
    (make-be-it things)
    (let ((parsed-forms (Rlist (make-ref (Elist things)))))
      (if (and *debug* (cdr parsed-forms))
          (fsignal "Multiple focus seen by REQUEST-DISPLAY" parsed-forms))
      (change-focus-to (car parsed-forms) T))))
(defun change-focus-to (parsed-form &optional (force-highlight? nil))
  (let* ((things (eval-command parsed-form) )
         (topfn  (if things (get-toplevel-name (car things))))
         (paths  (if parsed-form
                     (let ((code (condition-case (cond)
                                     (parsed->code parsed-form)
                                   (sys:error nil))))
                       (if code
                           (rlist (code->path topfn (elist code))))))))
    (suggest-focus
      `(:reference ,(unparse-command parsed-form)
        :name  ,topfn
        :paths ,paths
        :path  ,(car paths)
        :force-highlight ,force-highlight?))))
;;; PDF page 71

(define-db-action reinitialize (database)
                  "Reinitializes the KBE database (for debugging)."
  (db-init)
  (list ':text  ':text "Database has been reinitialized."))
(define-db-action use (language (language (parse-name ':keyword)))
                  "Sets the language of reference to <language>."
  (use-language-internal language)
  (suggest-focus  '())
  (values))
(defun resolve-language (name)
  (cond ((or (symbolp name) (stringp name))
          (setq name (intern-keyword name) ))
        (t
         (complain "Odd language spec: ~S" name)))
  (cond ((memq name '(:pl1 :pli)) 'PLI)
        ((memq name '(:lisp)) 'LISP)
        ((memq name  '(:ada))     'ADA)
        (t
         (complain "Sorry, I never heard of the ~A language." name))))
(defvar *display-language*  'lisp "For possible use by VIEW")
(defun use-language-internal (name)
  (declare (special *input-language*             ; From PARSE
                    *display-language*) )        ; From VIEW
  (*catch 'no-redisplay-needed
    (let ((language (resolve-language name)))
      (setq *input-language* language)
      (if (eq *display-language* language)
          (*throw 'no-redisplay-needed t))
      (setq *display-language* language) )
    (let ((things (that)))
      (when things
        (clear-display)
        (request-display (make-ref (car things))))))
  (values))

;;;0nly works for lisp, but is obsolete anyway.
(defun grind-code (form) (text "~:=-&" form) )
(define-db-action view ((toplevel-reference (parse-toplevel-reference) )
                        in (language (parse-name ':keyword)))
                  "Requests the code named by <toplevel-reference> be
                   displayed in the given <language>."
  (let ((*display-language* (resolve-language language) ))
    (declare (special *display-language*) )
    (let ((code (parsed->code toplevel-reference)))
      (cond ((not code)
             (complain "I don't know anything about ~A."
                       (unparse-command toplevel-reference)))
            (T
             `(:text :text ,(grind-code (car code))))))))
;;; PDF page 72

;this takes in a set of things, makes up an english description (based
;on make-ref) and prints it on the tty.

(define-db-action describe ((reference (parse-definite-reference)))
                  "Describes the segment designated by <reference>."
  (describe-reference reference))
(defun describe-reference (reference)
  (let ((things (eval-command reference)))
    (list ':text
          ':text (cond ((null things)
                        (text "I don't know about ~A."
    (unparse-command reference) ))
                       (t
                        (text "~{~A.~*~%~}"
                              (determine-ref-sharing
                                (ordinal-sort things))))))))
;this deletes the common tails from a set of reference strings and
;returns only the unique initial parts.
(defun determine-ref-sharing (things &aux strings)
  (setq strings (Rlist (string-reverse
                         (unparse-command
                           (make-ref
                             (Elist things))))))
  (prog (pos)
      L (setq pos (min (or (string-search " fo " (car strings)) 123456)
                       (or (string-search " ni " (car strings)) 123456)))
        (if (eq pos 123456) (return nil))
        (setq pos (+ pos 4))
        (cond ((Rand-fast (string-equal (car strings)
                                         (Elist (cdr strings))
                                        0 0 pos pos))
               (setq strings (Rlist (substring (Elist strings) pos)))
               (go L))))      ;
  (mapS #'string-nreverse (Elist strings))
  strings)
;;; PDF page 73

;;; This function allows you to ask about what still needs to be done
;;; (as recorded in TO-DO assertions).
(define-db-action what (needs to be done (where (parse-position) "in") /?)
                  "Requests a list of things that need to be done.
                   An optional context may be supplied as in /"What needs
                   to be done in the program FOO./""
  (let ((ref (if where
                 (let ((things (car (eval-command where))))
                   (cond (things (make-ref things))
                         (t
                          (barf "I don't know anything about ~A."
                                (unparse-command where))))))))
    (letS (((fn . strings)
            (Elist (alist-of-strings-to-do-in ref))))
      (list ':possibilities
            ':type ':unfinished-edits
            ':context (if ref (unparse-command ref))
            ':data (Rlist (cons fn strings))))))
(defun alist-of-strings-to-do-in-function (name)
  (alist-of-strings-to-do-in (function-name->parsed name)))
(defun function-name->parsed (function-name)
  (parse-command (text "the program ~S" function-name)
                 #'parse-definite-reference "definite reference") )
(defun alist-of-strings-to-do-in (ref)
  (let ((things (eval-command ref)))
    (if (or (not ref) things) (alist-of-strings-to-do things))))
;this is the main function which gets the key info.
(defun alist-of-strings-to-do (things &aux)
  (if (null things)
      (setq things (~set ~segcase (??& * @seg 'lambda-exp))))
  (let* ((segs
          (makeqset
            (Rnconc (all-subsegs (~seg (get-segcase (Elist things)))))))
         (stuff-by-fn
          (letS* ((segcase (& (Elist segs)))
                  (to-do (not-implemented? (~seg segcase))))
            (Reqalist (get-toplevel-name segcase)
                      (cons (Fselect to-do to-do) segcase)))))
    (letS (((fn . stuff) (Elist stuff-by-fn))
           (refs (determine-ref-sharing
                   (letS () (Rlist (cdr (Elist stuff))))))
           (strings (letS ((code (cdr (assq (car (Elist stuff))
                                            '((feed   . "Supply data to")
                                              (use . "Use")
                                              (expand . "Implement")))))
                           (ref (Elist refs)))
                      (Rlist (text "~A ~A." code ref)))))
    (Rbag (cons fn strings)))))

(defun not-implemented? (seg)
  (cond ((?empty-slot seg))
        ((and (or (??& (&* seg) @seg 'lambda-exp)
                  (??& (&* seg) cliche-def))
              (null (??& (& seg) subseg)))
         'expand)
        ((and (??& (&* seg) cliche 'pseudo-subcliche)
              (Rand-fast
                (not-implemented?
                  (Elist (~set ~seg ~slotseg (??& (&* seg) slot))))))
         'expand)))
;;; PDF page 74

 ;;; (FINISH-EDIT marker)  -
 ;;; Historically, this  function was responsible for getting a lot
 ;;; of the busy work of editing done. It would recode the modified
 ;;; program and redisplay it with the cursor positioned on the item       :
 ;;; to which the pronoun "this" would refer. It made the assumption
 ;;; that after the edit operation, "this" would designate
 ;;; something in the modified program unless the whole program has
 ;;; been destroyed.
 ;;; In the current model of the world, FINISH-EDIT sees that
 ;;; many of these same things get done, but mostly by hooks rather than
  ;;; wired  code. For a simple demo, we might still want display to happen
 ;;; automatically, but for the projected mode of operation (with the
  ;;; apprentice working in.background), there is no reason for the coder
 ;;; to code something up unless it is going to be viewed. Hence we want
 ;;; a modularity which will allow us to be able to say that if there is
 ;;;. more real work to be done, do it first. Call the coder only if the
 ;;; apprentice is not otherwise busy. This saves useless coding.
 ;;; Note also that FINISH-EDIT is now a special form, not
  ;;; a function. Its argument should be an unquoted symbol which names
 ;;; a variable which will be set to *last-db-context* if any such exists.
(defmacro finish-edit (old-context-name &body list-of-one-it-form)
   `(progn (make-be-it ,@list-of-one-it-form)
           (when (and *last-db-context* (not *automatic-reap*))
             (remember-db-context *last-db-context* ,old-context-name)
             (push `(,*last-db-context* . ,(it))
                   *undo-stack*) )
           (code-and-display-it)
           (db-context-was *last-db-context*)
           ;: From an outsider's point of view, we're all done...
           (end-of-command)
           ;;; Now just clean up locally...
           (db-reap-and-gc)
           (values)))
 ;;; >Reap database.
(define-db-action Reap (database)
                   "Causes the database to be reaped and GC'd."
   (db-reap)
   (db-gc)
   (list ':text ':text "Database has been reaped and GC'd."))
 ;;; (DB-REAP-AND-GC)
 ;;; Currently, this will compact the db if *AUTOMATIC-REAP* is non-(),
 ;;; and it will unconditionally call DB-GC.
  ;;; At some point, this may want to change so that it doesn't always
 ;;; call DB-GC, which is currently a very expensive operation.
 ;;; An important assumption of the recent DB changes is that the database
 ;;; is compacted every time you do something. There are places which
 ;;; probably depend on this being true. To find them, look for places
 ;;; where conditionals hinge on the value of *AUTOMATIC-REAP*.
(defun db-reap-and-gc ()
   (cond (*automatic-reap* (db-reap)))
   (db-gc))
 ;;; (CODE-AND-DISPLAY-1T)
 ;;; If something is being referred to by "it" (the value of *that*),
 ;;; codes and displays that thing.
;;; PDF page 75

 ;;; If nothing is being referred to (presumably due to the whole
;;; program having been destroyed), a new context is pushed and the
;;; display is cleared in case it might have been invalidated.
(defun code-and-display-it ()
  (cond (*that*
         (code (top-segcase (get-segcase (car (eval-command *that*)))))
         (change-focus-to *that*))
        (T (push-db-context) (clear-display))))
;;; PDF page 76

;;; This only works for segments at the moment.
(define-db-action Remove ((definite-reference (parse-definite-reference) ))
                  "Remove the thing referred to by <definite-reference>."
  (remove-reference definite-reference) )
(defun remove-reference (reference)
  (let ((things (eval-command reference)))
    (cond ((null things)
           (complain "I don't know anything about ~A."
                     (unparse-command reference) ))
          ((or (cdr things)
               (neq (thing-type (car things)) 'seg))
           (complain "I don't know how to kill ~A."
                     (unparse-command reference))))
    (finish-edit <IMP
      (prog1 [(& (plan-top (~seg (car things))))]
             (2remove-seg (car things))))))
;;; (UNDO-LAST-EDIT)
;;; Running this program will undo an edit. This only makes sense if
;;; we are not compacting the DB, which destroys the information about
;;; other contexts besides the initial context to which we could back up.
(define-db-action undo () "Undoes the last request."
  (undo-last-edit))
(defun undo-last-edit ()
  (let (((context . references) (pop *undo-stack*)))
    (cond ((and (not *automatic-reap*)
                context
                (not (> (car context) (car *db-context*))))
           (restore-db-context context)
           (db-context-was *last-db-context*)
           (setq *this* nil)
           (setq *that* (car references) )
           (if (cdr references)
               (cbarf "Ignore extra focus info."
                      "Multiple focus seen in UNDO-LAST-EDIT."
                      references) )
           (change-focus-to (car references)))
          (t
           (complain "No saved context to undo to!")))))
;;; PDF page 77

(defun unparse-parameters-or-roles (data)
  (when (cdr data)
    (text "~A ~A"
          (string-downcase
            (cond ((cddr data) (car data))
                  (t (a-or-an (or (singular-form (car data))
                                  (car data))))))
          (phrase-append "and" (mapcar #' unparse-command
                                       (cdr data))))))
(defun (parameter  unparse) (x) (unparse-parameters-or-roles x))
(defun (parameters unparse) (x) (unparse-parameters-or-roles x))
(defun (role       unparse) (x) (unparse-parameters-or-roles x))
(defun (roles      unparse) (x) (unparse-parameters-or-roles x))
(define-db-action define
                  (a<n> (optional-cliche-name (parse-optional-cliche-name) )
                     (spec
                       (let ((name (parse-definition-spec)))
                         (if (name-symbol name) name
                             (parse-error "Missing definition name."))))
                     (optional-roles-or-parameters
                       (when (eq (peek-token) 'with)
                         (read-token)
                         (parse-roles-or-parameters spec))
                       "with"))
                  "Defines a given program//cliche. Parameters//roles
                   may optionally also be specified as /"... with a
                   parameter//role X./" or /"... with parameters//roles
                   X and Y./""
  (make-definition-internal
    optional-cliche-name
    spec
    (cdr optional-roles-or-parameters) ))
(defun make-home-suggestion (name)
  (let ((this-thing (car (this)))) ;°%%% use *almost-this*? -kmp §/26/84
    (if this-thing
        (let ((near-name (get-toplevel-name this-thing)))
          (suggest-code-home (name-symbol name)
                             `(:near ,near-name))))))
(defun make-definition-internal (cliche-name name args)
  (if cliche-name (check-name cliche-name) )
  (check-name name)
  (if (not (name-symbol name) )
      (complain "No name was given for the ~A to be defined."
                (unparse-command name) ))
  (make-home-suggestion name)
  (caseq (name-type name)
    (program-definition
      (lexpr-funcall #'eval-commands (list name) ':ignore
                     #'make-program-definition args))
    (cliche-definition
      (lexpr-funcall #'eval-commands (list name) *: ignore
                     #'make-cliche-definition  args))
    (T (complain "Cannot define a ~A" (unparse-command name))))
  (if cliche-name
      (eval-command
        (parse-command (text "Implement the ~A as a ~A"
                             (unparse-command name)
                             (unparse-command cliche-name)))))
   (values) )
(defun make-cliche-definition (name &rest roles)
   (if (or (get (name-symbol name) 'cliche-plan)
           (??& * cliche-def (name-symbol name)))
;;; PDF page 78

      (complain "The ~A is already defined." (unparse-command name)))
  (let* ((segcase (& (genseg (name-symbol name)))))
    (+& segcase seg1ype 'simple)
    (+& segcase plantype 'expr)
    (+& segcase @seg 'opencode nil nil)
    (+& segcase cliche-def (name-symbol name) nil)
    (when roles
      (complain "Can't specify roles in /"Define a cliche.../" any more.")
      (comment (Rignore (add-role (name-symbol (Elist roles)) segcase))))
    (finish-edit <DEFINE
      (list segcase))))
(define-db-action Append (a parameter
                            (var (parse-variable-name) )
                            (where (when (eq (peek-token) 'to)
                                     (read-token)
                                     (parse-toplevel-reference) )
                                   "to" (top)))
                  "Appends the specified parameter to the parameter
                   list of the current program (or to some other
                   program, whose name may be introduced by /"to/")."
  (let ((things (eval-command where)))
    (check-name var)
    (if (not (and (null (cdr things))
                  things
                  (eq (thing-type (car things)) 'seg)
                  (??& (car things) @seg  'lambda-exp)))
        (complain "Can only add a parameter to a defun."))
    (add-parameter (name-symbol var) (car things))
    (finish-edit <ADD things)))
;;; PDF page 79

;;; Note: Better error checking here would be desirable. Right now we
;;; don't worry about the case where the function being redefined
;;; had a different I0-spec from the one replacing it. If that happens,
;;; and if previously analyzed programs have made assumptions about -
;;; that I0-spec, the world will be confused. --kmp
(defun make-program-definition (name &rest variables)
 (if  (or (let ((segcase
                  (~01 ~segcase
                      (??& * @seg  'lambda-exp (name-symbol name)))))
            (cond ((not segcase) nil)
                  (T (kill-seg (~seg segcase)) T)))
          (fboundp (name-symbol name) ))
      (grumble "Redefining the ~A" (unparse-command name)))
  (let* ((segcase (& (genseg (name-symbol name)))))
    (+& segcase seg1ype 'simple)
    (+& segcase plantype 'expr)
    (+& segcase @seg 'lambda-exp (name-symbol name) nil)
    (Rignore (add-parameter (name-symbol (Elist variables)) segcase))
    (finish-edit <DEFINE
      (list segcase))))
(defun add-parameter (symbol segcase)
  (if (??& (&* segcase input) @io 'lambda-arg symbol)
      (barf "the program already has an arg: ~A" symbol))
  (let* ((port (+& segcase input nil (genobj (~seg segcase) symbol) nil))
         (argno (1+ (length (~nset ~argno
                              (??& (&* segcase input) @io 'lambda-arg))))))
    (+& port @io 'lambda-arg symbol argno)
    port))
;;; PDF page 80

;This focuses on the ability to actually do the sharing.
;ie get the new dflow right, and delete any segments that
;are redundant.  It does this at the cost of only weakly checking
;whether sharing is in fact a correct thing to do.
;Note that this assumes that seg1 is not forced to be after seg2.
(define-db-action share ((definite-reference-1 (parse-definite-reference))
                         and
                         (definite-reference-2 (parse-definite-reference)))
                  "Shares <definite-reference-1>
                    and <definite-reference-2>.
                   eg, /"Share the x and the y./""
  (share-references definite-reference-1 definite-reference-2))
(defun share-references (ref1 ref2)
  (let ((thing1 (eval-command ref1))
        (thing2 (eval-command ref2)))
    (let ((failures '()))
      (cond ((not thing1) (push "~0G~A is unknown to me" failures))
            ((or (cdr thing1)
                 (neq (thing-type (car thing1)) 'seg))
             (push "~0G~A is not a seg" failures)))
      (cond ((not thing2) (push "~1G~A is unknown to me" failures))
            ((or (cdr thing2)
                 (neq (thing-type (car thing2)) 'seg))
             (push "~1G~A is not a seg" failures)))
      (if failures
          (let ((unref1 (unparse-command ref1))
                (unref2 (unparse-command ref2)))
            (complain
              (auto-fill-string
                (text "Unable to share ~A and ~A because ~A."
                      unref1 unref2
                      (phrase-append "and"
                                     (mapcar #'(lambda (x)
                                                 (text x unref1 unref2))
                                             failures))))))))
    (finish-edit <SHARE
      (share0 (~seg (car thing1)) (~seg (car thing2))))))
(defun share0 (seg1 seg2)
  (when (not (eq seg1 seg2))
    (let ((seg1ins (sort (??& (&* (&* seg1) input) @io) #'arg-order))
          (seg2ins (sort (??& (&* (&* seg2) input) @io) #'arg-order)))
      (when (not (and (simple-funcall-or-constant seg1)
                      (simple-funcall-or-constant seg2)
                      (equal (?@seg-name seg1) (?@seg-name seg2))
                      (eq (length seg1ins) (length seg2ins))))
        (complain "Unable to share ~A with ~A."
                  (unparse-command (make-ref (& seg1) t))
                  (unparse-command (make-ref (& seg2) t))))
      (letS ((from1 (dflow-source (~deep (Elist seg1ins))))
             (from2 (dflow-source (~deep (Elist seg2ins)))))
        (when (not (eq from1 from2))
          (when (not (and (??& from1 @io  'returnval)
                          (??& from2 @io  'returnval)))
            (barf "Ports not sharable: ~:= and ~:=" from1 from2))
          (share0 (~seg~segcase from1) (~seg~segcase from2))))
      (let* ((((first-seg) (second-seg))
              (ordinal-sort (list (& seg1) (& seg2)))))
        (Rignore (implement-dflow (??& (&* first-seg) output)
                   (Elist (share-dests (?& (&* second-seg) output)))))
        (kill-shared-seg second-seg)
        (list (& first-seg))))))
; swe used to do this but it choked in demo.
; (let* ((expr-seg (shared-expr seg1 seg2))
; (shared-seg (~seg (copyseg seg nil)))
; PDF page 81
; (sharedins (sort (??& (&* (&* shared-seg) input) @io)
; 
; (>& (& expr~seg) subseg (& shared-seg))
; (>& (& expr-seg) role (& shared-seg) 'action)
; (Rignore (implement-dflow (?& (&* shared-seg) output)
; (Elist (append (share-dests (??& (&* seg1) output))
; 
; (Rignore (implement-dflow (dflow-source (~deep (Elist seg1ins)))
; (~deep (Elist sharedins))))
; (kill-shared-seg seg1)
; (kill-shared-seg seg2)
; (list (& shared-seg)))
;;; PDF page 82

(defun shared-expr (seg1 seg2)
  (let (((lca) (2intermediate-segs seg1 seg2)))
    (prog ()
      L (if (eq (?plantype lca) 'expr) (return lca))
        (setq lca (?supseg lca))
        (if (null lca)
            (barf "No containing expr for ~A and ~A" seg1 seg2))
        (go L))))
(defun kill-shared-seg (seg)
  (let ((supseg (?supseg seg)))
    (2remove-seg (& seg))
    (prog (supsup)
       L (when (and (setq supsup (?supseg supseg))
                    (null (??subseg supseg)))
           (2remove-seg (& supseg))
           (setq supseg supsup)
           (go L)))))
(defun share-dests (ret)
  (prog (to-do found out dflows)
     (setq to-do (list ret))
   L (if (null to-do) (return found))
     (setq out (pop to-do))
     (cond ((and (output-fact? out) (setq dflows (??& * dflow out)))
            (letS ((df (Elist dflows)))
              (push (~to df) to-do)
              (make-fact-be-out df))
            (if (not (eq out ret)) (2remove-output out)))
           (T (push out found)))
     (go L)))
(defun dflow-source (in)
  (let ((source (or (~01 ~from (??& * dflow * in))
                    (~01 (??& * tinput * * in))
                    (and (eq (~keyword in)  'toutput) (~lower-io in)))))
    (cond (source (dflow-source source))
          ((and (??& (~segcase in) @seg  'funcall 'Snoop)
                (eq (~keyword in)  'output))
           (dflow-source (?& (~segcase in)  input)))
          ((and (??& * role (&* (~seg~segcase  in)) 'dfjoin)
                (eq (~keyword  in) 'output))
           (let* ((join-in (?& (& (~seg~segcase  in) 'body)
                                input 'id * (~obj in)))
                  (before-join-in  (~01 ~from (??& * dflow * join-in))))
             (dflow-source
               (if (??&  (~segcase before-join-in) plantype 'ssr-body)
                   (let  ((rec (~subseg (?& (~segcase before-join-in)
                                            recursive))))
                      (?& rec input * (~obj before-join-in)))
                   before-join-in))))
           (T in))))
(defun arg-order  (@io1 @io2)
  (and (~argno @io1) (~argno @io2) (< (~argno @io1) (~argno @io2))))
(defun simple-funcall-or-constant  (seg)
   (and (memq (?@seg-kind seg) '(funcall constant))
        (??& (&* (&* seg) output) @io 'returnval)
        (null (cdr (??& (&* seg) output)))
        (null (??& (&* (&* seg) input) @io 'free-var))))
;;; PDF page 83

;;; >Finish {definition-name}.
 ;;;
;;; Cleans up after having edited {definition-name}.

(defvar *at-end-noticers* nil "list of fns to call by s-end")
(defun add-at-end-noticer (fn)
  (if (not (memq fn *at-end-noticers*))
      (push fn *at-end-noticers*) )
  fn)
(define-db-action Finish (editing
                           (definition-name (parse-toplevel-reference)))
                  "Cleans up after editing <definition-name>."
  (let ((code-list (parsed->code definition-name) ) fixed-code)
    (when (not (string-equal (definition-type-name (car code-list))
                             "cliche"))
      (let ((ref (lisp-cursor-location code-list)))
        (if ref (complain "You haven't implemented ~A yet." ref)))
      (setq fixed-code (remove-markers (car code-list)))
      (when (and code-list (not (equal (car code-list) fixed-code)))
        (make-be-it (eval-command definition-name))
        (coder-confirmation (cadr fixed-code) fixed-code)))
    (dolist (fn *at-end-noticers*)
      (funcall fn (or fixed-code (car code-list))))))
(defun remove-markers (code)
  (cond ((atom code) code)
        ((eq (car code) 'kbe-/#-comment-macro)
         (when (not (cadr code))
           (barf "Bad code format: ~S" code))
         (remove-markers (car (cadr code))))
        (t
         (cons (remove-markers (car code))
               (remove-markers (cdr code))))))
;;; PDF page 84
;;; Local Modes:;
;;; Mode: LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp REPEAT Indent:1;
;;; Lisp REPEAT* Indent:1;
;;; Lisp WITHOUT-END-OF-COMMAND Indent:0;
;;; End:;
