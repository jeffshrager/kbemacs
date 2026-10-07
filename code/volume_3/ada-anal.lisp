; -*- Mode:LISP; Package: KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; PDF pages 155-178 (printed source pages 1-24).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;NOTE WE MUST BE VERY CAREFULL IN THIS FILE NOT TO LET CODE
;CONSTANTS GET INTO THE DATA BASE!

;In the demo we have the notion of a logical file.  it is defined
;here because the coder takes advantage of it.

(defstruct (ada-file :conc-name (:type :named-list))
  name
  package-name
  record-type
  key-type
  name-variable
  record-def
  def)

(defvar *predefined-files* nil "alist of ada files defined by name")

;we call this here for convenience of testing.  it
;sets up the info about the standard files in MAINTENANCE_FILES.

(defun init-ada-files ()
  (setq *predefined-files*
        (copytree
        `((models .,
            (make-ada-file
              name 'models package-name 'model_io
              record-type 'model_type key-type 'model_key_type
              name-variable 'models_name
              record-def '(ada-rec (NAME (ada-string 1 16))
                                   (MAKER (ada-string 1 16)))
              def '(ada-file-def
                     (models model_io model_type
                             model_key_type models_name)
                     (ada-constant-dcl models_name (ada-string)
                                   (str-lit "models.data"))
                     (ada-type-dcl model_key_type (ada-string 1 3))
                     (ada-type-dcl model_type
                                   (ada-rec (name (ada-string 1 16))
                                            (maker (ada-string 1 16))))
                     (ada-new-package model_io
                       (keyed_io) (model_type) (model_key_type))
                     (ada-dcl models (file_type model_io)))))
          (defects .,
            (make-ada-file
              name 'defects package-name 'defect_io
              record-type 'defect_type key-type 'defect_key_type
              name-variable 'defects_name
              record-def '(ada-rec (NAME (ada-string 1 32))
                                   (model (model_key_type)))
              def '(ada-file-def
                     (defects defect_io defect_type
                       defect_key_type defects_name)
                     (ada-constant-dcl defects_name (ada-string)
                                   (str-lit "defects.data"))
                     (ada-type-dcl defect_key_type (ada-string 1 5))
                     (ada-type-dcl defect_type
                       (ada-rec (name (ada-string 1 32))
                                (model (model_key_type))))
                     (ada-new-package defect_io
                       (keyed_io) (defect_type) (defect_key_type))
                     (ada-dcl defects (file_type defect_io)))))
          (repairs .,
            (make-ada-file
              name 'repairs package-name 'repair_io
              record-type 'repair_type key-type 'repair_index_type
              name-variable 'repairs_name
              record-def '(ada-rec (date (time))
                                   (defect (defect_key_type))
                                   (next (repair_index_type))
                                   (comment (ada-string 1 32)))
              def '(ada-file-def
                     (repairs repair_io repair_type
                       repair_index_type repairs_name)
               (ada-constant-dcl repairs_name (ada-string)
                                 (str-lit "repairs.data"))
               (ada-subtype-dcl repair_index_type (index_type))
               (ada-type-dcl repair_type
                             (ada-rec (date (time))
                                      (defect (defect_key_type))
                                      (next (repair_index_type))
                                      (comment (ada-string 1 32))))
               (ada-new-package repair_io
                 (chained_io) (repair_type) (repair_index_type))
               (ada-dcl repairs (file_type repair_io)))))
          (units .,
            (make-ada-file
              name 'units package-name 'unit_io
              record-type 'unit_type key-type 'unit_key_type
              name-variable 'units_name
              record-def '(ada-rec (model (model_key_type))
                                   (repair (repair_index_type)))
              def '(ada-file-def
                     (units unit_io unit_type unit_key_type units_name)
               (ada-constant-dcl units_name (ada-string)
                                 (str-lit "units.data"))
               (ada-type-dcl unit_key_type (ada-string 1 7))
               (ada-type-dcl unit_type
                             (ada-rec (model (model_key_type))
                                      (repair (repair_index_type))))
               (ada-new-package unit_io
                 (keyed_io) (unit_type) (unit_key_type))
               (ada-dcl units (file_type unit_io))))))))

    (init-ada-files)

    (defun get-ada-file-info (name)
  (when (or (symbolp name) (stringp name))
    (if (stringp name)
        (setq name (intern name)))
    (if (not (singular-form name))
        (setq name (intern (string-pluralize name))))
    (cdr (assq name *predefined-files*))))

    (add-at-end-noticer 'check-for-new-files)

    (declare (special *file-package-name*))

    (defun check-for-new-files (form)
  (when (and (listp form) (eq (car form) 'def-ada-top)
             (eq (car (nth 4 form)) 'def-ada-package))
    (let ((*file-package-name* (cadr form)))
      (determine-ada-file-info (nth 2 (nth 4 form))))))

;given a declaration list, this looks to see if you
;are defining any new file objects, and stores the appropriate
;information this is also the place where we will create special
;cliches for the file.

    (defun determine-ada-file-info (dcls)
  (setq dcls (clean-aggregate-file-defs (copytree dcls)))
  (dolist (dcl dcls)
    (when (and (eq (car dcl) 'ada-file-def)
               (not (get-ada-file-info (caadr dcl)))
               (assq 'ada-new-package (cddr dcl)))
      (handle-new-file dcl))))

    (defun handle-new-file (dcl)
  (let* ((name (caadr dcl))
         (package-dcl (assq 'ada-new-package (cddr dcl)))
         (package-name (cadr package-dcl))
         (record-type (car (nth 3 package-dcl)))
         (record-def (dolist (d (cddr dcl))
                       (if (and (eq (car d) 'ada-type-dcl)
                                (eq (cadr d) record-type))
                           (return (caddr d)))))
         (key-type (car (nth 4 package-dcl)))
         (name-variable-dcl (assq 'ada-constant-dcl (cddr dcl)))
         (name-variable (cadr name-variable-dcl)))
    (push (cons name (make-ada-file name name
                                    package-name package-name
                                    record-type record-type
                                    key-type key-type
                                    name-variable name-variable
                                    record-def record-def
                                    def dcl))
          *predefined-files*)
    (make-new-cliches name)))

;this create the right PRINT_ and _HEADINGS cliches for a new file.

    (defun make-new-cliches (name)
  (let* ((root (singular-form name))
         (print-name (symbolconc "PRINT_" root))
         (headings-name (symbolconc print-name "_HEADINGS"))
         (file-info (get-ada-file-info name))
         (record-def (ada-file-record-def file-info))
         ((print-stuff heading-string)
          (make-print-stuff root record-def)))
    (suggest-code-home print-name
                       '(:buffer "CLICHE-LIBRARY.ADA PS:<KBE.DEMO> OZ:"))
    (suggest-code-home headings-name
                       '(:buffer "CLICHE-LIBRARY.ADA PS:<KBE.DEMO> OZ:"))
    (code (create-plan-for-code
            (copytree
            `(def-ada-top ,print-name nil nil
               (def-ada-cliche ,print-name
                 (primary-roles (,root)
                  described-roles (,root)
                  comment
                   (str-lit ,(string-append
                               "prints out the fields of {the "
                               (string-downcase root) "}")))
                 ((ada-dcl ,root (,(ada-file-record-type file-info))))
                 ((ada-setq ,root (kbe-/#-comment-macro ()
                                    (the input ,root)))
                  .,print-stuff))))))
    (code (create-plan-for-code
            (copytree
            `(def-ada-top ,headings-name ((text_IO)) ((text_IO))
               (def-ada-cliche ,headings-name
                 (comment
                   (str-lit ,(string-append "prints headings for printing "
                                            (string-downcase root)
                                            " records")))
                 ()
                 ((ada-fn (PUT) (str-lit ,heading-string))
                  (ada-fn (NEW_LINE) (int-lit 2))))))))
    (typeout (text "The cliches ~A and ~A defined."
                   headings-name print-name))))

;this only works for things that can be printed on one line at the moment.

    (defun make-print-stuff (var record-def)
  (let (print-stuff heading-stuff (current-column 1))
    (dolist (def (cdr record-def))
      (let* ((field-name (car def))
             (field-type (cadr def))
             ((field-printer field-length)
              (make-field-printer var field-name field-type)))
        (when field-printer
          (when (> current-column 1)
            (incf current-column)
            (push " " heading-stuff))
          (cond ((not (> (string-length field-name) field-length))
                 (setq field-name
                       (format nil "~V:@<~A~>" field-length
                               (string-capitalize
                                 (string-downcase (string field-name)))))
                 (when (> current-column 1)
                   (push `(ada-fn (set_col) (int-lit ,current-column))
                         print-stuff))
                 (push field-printer print-stuff)
                 (incf current-column field-length)
                 (push field-name
                       heading-stuff)
                 (push " " heading-stuff)
                 (incf current-column))
                (T (let* ((excess (- (string-length field-name)
                                     field-length))
                          (before (// excess 2))
                          (after (- excess before)))
                     (incf current-column before)
                     (when (> current-column 1)
                       (push `(ada-fn (set_col) (int-lit ,current-column))
                             print-stuff))
                     (push field-printer print-stuff)
                     (incf current-column field-length)
                     (push (string-capitalize
                             (string-downcase (string field-name)))
                           heading-stuff)
                     (incf current-column after)))))))
    (push (copytree '(ada-fn (NEW_LINE))) print-stuff)
    (setq heading-stuff (apply #'string-append (nreverse heading-stuff)))
    (list (nreverse print-stuff)
          (substring heading-stuff 0
                     (1+ (string-reverse-search-not-char
                           #\space heading-stuff))))))

    (defun make-field-printer (var field-name field-type)
  (let (len)
    (cond ((eq (car field-type) 'ada-string)
           `((ada-fn (PUT) (ada-select ,var ,field-name))
             ,(caddr field-type)))
          ((eq (car field-type) 'time)
           `((ada-fn (PUT) (ada-fn (FORMAT_DATE)
                                   (ada-select ,var ,field-name)))
             10))
          ((eq field-name 'next) nil)
          ((equal field-type '(repair_index_type)) nil)
          ((setq len (get-predefined-type-len (car field-type)))
           `((ada-fn (PUT) (ada-select ,var ,field-name))
             ,len))
          (T (barf "unexpected type in file def ~A" field-type)))))

    (defun get-predefined-type-len (type)
  (let* ((spot (string-search-char #/_ (string type)))
         (root (maybe-intern
                 (if spot (substring (string type) 0 spot) (string type))
                 (symbol-package type)))
         (file-def (ada-file-def (get-ada-file-info root))))
    (when file-def
      (dolist (dcl (cddr file-def))
        (if (and (eq type (cadr dcl))
                 (eq (caaddr dcl) 'ada-string))
            (return (caddr (caddr dcl))))))))

;the following makes it possible for the analyzer to
;understand ada programs.  Everything is either expanded
;into lisp stuff, or just turned into a pseudo-function
;that is handled like an ordinary expr/lexpr.

;we have to have a definition here for everything that is
;a fexpr or macro above.

    (comment a testing function
 (defun tt ()
  (setq segcase (analyze (surface-analyze (copytree test))))
  (code segcase)
  (setq code (?@seg-code (~seg segcase)))
  (if (not (equal (car code) test))
      (print "not equal"))
  (let (ada-supress) (pl code))) )

;note encoding of real fn name in symbol here.
;also we must add the file arg to the text_io fns in order for
;the analyzer to understand it easily.

    (defvar *special-text_io-fns*
  '(new_line new_page set_col page line get put)
  "text_io fns that have an implicit file arg")

    (defun (ada-fn k*expander) (form)
  (let* ((args (cddr form)) new-form)
    (if (memq (caadr form) *special-text_io-fns*)
        (push 'current_output args))
    (setq new-form `(,(symbolconc "ADA-FN~" (caadr form)) ., args))
    (when (eq (caadr form) 'read)
      (setq new-form
            `(setq ,(caddr new-form)
                   (ada-fn~read ,(cadr new-form) ., (cdddr new-form)))))
    (if (eq (caadr form) 'set_output)
        (setq new-form `(setq current_output ,new-form)))
    new-form))

;this should really be different if a var is being passed.
    (defun (ada-attribute k*expander) (form)
  `(,(symbolconc "ADA-ATTRIBUTE~" (cadr form))))

    (defun (ada-convert k*expander) (form)
  `(,(symbolconc "ADA-CONVERT~" (caadr form)) ,(caddr form)))

    (defun (ada-construct k*expander) (form)
  `(,(symbolconc "ADA-CONSTRUCT~" (caadr form)) ., (cddr form)))

    (defun (ada-select k*expander) (form)
  `(,(symbolconc "ADA-SELECT~" (caddr form))  ,(cadr form)))

    (defun (str-lit k*expander) (form)
  `',form)
    (defun (enum-lit k*expander) (form)
  `',form)
    (defun (int-lit k*expander) (form)
  `',form)

;many declarations turn into things that just stay around.

    (defun (ada-use k*expander) (form)
  `(ada-use~pseudo ',(cdr form)))

    (defun (ada-file-def k*expander) (form)
  `(ada-file-def~pseudo ',(cdr form)))

    (defun (ada-type-dcl k*expander) (form)
  `(ada-type-dcl~pseudo ',(cadr form) ',(caddr form)))

    (defun (ada-subtype-dcl k*expander) (form)
  `(ada-subtype-dcl~pseudo ',(cadr form) ',(caddr form)))

;these also have to be processed by the forms that they bind in.

    (defun (ada-init-dcl k*expander) (form)
  `(setq ,(cadr form)
         (ada-init-dcl~pseudo
           ',(cadr form) ',(caddr form) ,(cadddr form))))

    (defun (ada-constant-dcl k*expander) (form)
  `(setq ,(cadr form)
         (ada-constant-dcl~pseudo
           ',(cadr form) ',(caddr form) ,(cadddr form))))

    (defun (ada-dcl k*expander) (form)
 `(setq ,(cadr form)
        (ada-dcl~pseudo ',(cadr form) ',(caddr form))))

    (defun (ada-new-fn k*expander) (form)
  (let* (((name params)
          (specs-for-new-ada-fn
            (append (caddr form) '(functions) nil) ;kludge
            (caadr form)))
         (internal-name (symbolconc '"ADA-FN~" name))
         (param-names (mapcar #'car params))
         (side-outs (mapcar #'(lambda (n)
                                `(,(symbolconc n "-OUT") side-effect ,n))
                            param-names)))
    (eval `(defun (,internal-name 6proto-seg) ()
             (6fast-defspecs
               '(,internal-name (z .,side-outs) .,param-names)))))
  `(ada-new-fn~pseudo ',(cdr form)))

    (defun (ada-fn~create 6proto-seg) ()
  (6fast-defspecs
    '(ada-fn~create (z (file side-effect file-in)) file-in mode . name)))

    (defun (ada-new-package k*expander) (form)
  `(ada-new-package~pseudo ',(cdr form)))

;these are for nested definitions only.  they don't
;understand hardly a thing.  they just remember what they
;are given so that they can produce it again.

    (defun (def-ada-fn k*expander) (form)
  `(def-ada-fn~pseudo ',(cdr form)))

    (defun (def-ada-generic k*expander) (form)
  `(def-ada-generic~pseudo ',(cdr form)))

    (defun (def-ada-package k*expander) (form)
  `(def-ada-package~pseudo ',(cdr form)))

    (defun (def-ada-package-body k*expander) (form)
  `(def-ada-package-body~pseudo ',(cdr form)))


;def-ada-top is handled specially in surface-analyze
;to generate these guys.

;these handle top level forms
;note that we must get rid of file closes because the loop analyzer
;would choke on them.  (This is just like the way with-open-file is
;handled in lisp.)  Note also that any exceptions and clean_up
;fn are similarly discarded.

    (defun (ada-top-fn k*expander) (form)
  (let* (((nil (name params return) dcls stmnts except) form)
         (param-vars (mapcar #'car params))
         ((bound-vars . new-dcls) (discard-some-dcls dcls))
         (new-stmnts (discard-close-info stmnts)))
    (if return
        `(lambda ,param-vars
           (prog ,bound-vars ,@ new-dcls ,@ new-stmnts))
        `(lambda ,param-vars
           (let ,bound-vars ,@ new-dcls ,@ new-stmnts)))))

;note hack to let us no we have a package when we are done.

    (defun (ada-top-package k*expander) (form)
  (let* (((nil name dcls) form))
    `(lambda nil ., (append dcls (ncons ''*package*)))))

;note that it would be a real problem to deal with return values
;from cliches without it forcing the program you put it in to
;become a function.  As it is since we only create procedures,
;we have no problem.

    (defun (def-ada-cliche k*expander) (form)
  (let* (((nil name nil . body) form))
    (if (cdr body)
        (let* (((dcls stmnts except) body)
               ((bound-vars . new-dcls) (discard-some-dcls dcls))
               (new-stmnts (discard-close-info stmnts))
               (return? (assq 'ada-return new-stmnts)))
          (if return?
              `(prog ,bound-vars ,@ new-dcls ,@ new-stmnts)
              `(let ,bound-vars ,@ new-dcls ,@ new-stmnts)))
        (barf "not supported yet" form))))

    (defun discard-some-dcls (dcls)
  (let (new-dcls bound-vars)
    (do ((d dcls (cdr d))) ((null d) (cons bound-vars (nreverse new-dcls)))
      (caseq (caar d)
        (def-ada-fn
          (if (not (eq (caadr (car d)) 'clean_up))
              (push (car d) new-dcls)))
        ((ada-dcl ada-init-dcl ada-constant-dcl)
         (push (cadar d) bound-vars)
         (push (car d) new-dcls))
        ((ada-use ada-type-dcl ada-subtype-dcl ada-new-package
          ada-new-fn def-ada-package def-ada-package-body
          def-ada-generic ada-file-def) (push (car d) new-dcls))
        (T (cbarf "unexpeted ada dcl ~a" (car d)))))))

    (defun discard-close-info (stmnts)
  (let (new-stmnts)
    (do ((s stmnts (cdr s))) ((null s) (nreverse new-stmnts))
      (if (not (and (list (car s)) (eq (caar s) 'ada-fn)
                    (member (cadar s) '((close) (clean_up)))))
          (push (car s) new-stmnts)))))

    (defun (ada-top-package-body k*expander) (form)
  (barf "not supported" form))

    (defun (ada-top-generic k*expander) (form)
  (barf "not supported" form))

    (defun (ada-begin k*expander) (form)
  (if (caddr form) (barf "ada exceptions not supported for begins"))
  `(progn ., (cadr form)))

;note that the only kind of exceptions we support are the following
;one kind in a begin in a loop.

    (defun (ada-loop k*expander) (form)
  (let (((nil bod exceptions) form))
    (if exceptions (barf "ada exceptions not supported for loops"))
    (if (and (null (cdr bod)) (listp (car bod)) (eq (caar bod) 'ada-begin)
             (caddr (car bod))          ;has exceptions
             (equal (car (last (cadr (car bod))))
                    '(ada-exit))) ;exits if possible
        (let* ((begin (car bod))
               (nested-exceptions (caddr begin))
               (stmnts (nreverse (cdr (reverse (cadr begin))))))
          `(prog () L (if (no-ada-exceptions ',nested-exceptions)
                          (return nil))
                 ,@stmnts ,(append '(go L) nil)))
        `(prog () L ,@(cadr form) ,(append '(go L) nil)))))

    (defun (ada-while k*expander) (form)
  (if (cadddr form) (barf "ada exceptions not supported for while"))
  `(prog () L (if (not ,(cadr form)) ,(append '(return) nil))
          ,@(caddr form) ,(append '(go L) nil)))

    (defun (ada-cond k*expander) (form)
  `(cond ., (mapcar #'(lambda (clause)
                        (if (equal (car clause) '(enum-lit true))
                            (cons 't (cdr clause))
                            clause))
                    (cdr form))))

    (defun (ada-return k*expander) (form)
  `(return ,(cadr form)))

    (defun (ada-exit k*expander) (form)
 `(if ,(or (cadr form) (ncons 'ada-exit-zero-arg))
      ,(append '(return) nil)))

    (defun (ada-raise k*expander) (form)
  (barf "ada exceptions not supported yet") form)

    (defun (ada-setq k*expander) (form)
  `(setq ., (cdr form)))

;the following define which standard ada fns have side-effects.

    (comment
   ;for the time being, we are pretending that these io fns
   ;do not side-effect their file input.

  (defun (ada-fn~create 6proto-seg) ()
    (6fast-defspecs
      '(ada-fn~create (z (file side-effect file-in)) file-in mode . name)))

  (defun (ada-fn~open 6proto-seg) ()
    (6fast-defspecs
      '(ada-fn~open (z (file side-effect file-in)) file-in mode . name)))

  (defun (ada-fn~reset 6proto-seg) ()
    (6fast-defspecs
      '(ada-fn~reset (z (file side-effect file-in)) file-in)))

  (defun (ada-fn~read 6proto-seg) ()
    (6fast-defspecs
      '(ada-fn~read (z (file side-effect file-in)) file-in . index)))

  (defun (ada-fn~set_index 6proto-seg) ()
    (6fast-defspecs
      '(ada-fn~set_index (z (file side-effect file-in)) file-in index)))  )

    (defun (ada-fn~write 6proto-seg) ()
  (6fast-defspecs
    '(ada-fn~write (z (file side-effect file-in)) file-in item . index)))

;the following are for text_io fns with a file input.

    (defun (ada-fn~new_line 6proto-seg) ()
  (6fast-defspecs
    '(ada-fn~new_line (z (file side-effect file-in)) file-in . num)))

    (defun (ada-fn~set_col 6proto-seg) ()
  (6fast-defspecs
    '(ada-fn~set_col (z (file side-effect file-in)) file-in . num)))

    (defun (ada-fn~new_page 6proto-seg) ()
  (6fast-defspecs
    '(ada-fn~new_page (z (file side-effect file-in)) file-in)))

    (defun (ada-fn~get 6proto-seg) ()
  (6fast-defspecs
    '(ada-fn~get (z (file side-effect file-in)
                    (var-out side-effect var)) file-in var)))

    (defun (ada-fn~put 6proto-seg) ()
  (6fast-defspecs
    '(ada-fn~put (z (file side-effect file-in)) file-in var . width)))

;the following undoes most of the above in a post pass after the coder
;is finished its normal work

    (declare (special open-info type-info var-type-info var-list
                  use-info with-info fn-type did-set_output *saved-code*))

    (comment
 (defun t2 ()
   (setq code (copytree *saved-code*))
   (cleanup-ada-code code)
  (let (all-code 5noop-tran)
    (5clean-noops1 code nil nil))
   (let (ada-supress) (pl code)))
) ;end comment

    (defun cleanup-ada-code (code)
  (declare (special 5noop-tran all-code))
  (setq *saved-code* (copytree code)) ;for debugging ease
  (let (all-code 5noop-tran) ;we don't care if we loose pointing ability
    (5clean-noops1 code nil nil))
  (if (not (memq (car code) '(defun define-cliche)))
      (barf "can only code ada fns and cliches now, not expresions"))
  (cleanup-ada-fn-call code))

    (defun cleanup-ada-fn-call (call)
  (when (and (listp call) (symbolp (car call)))
    (caseq (car call)
      (quote (clean-ada-quote call))
      (setq (clean-ada-setq call))
      (cond (clean-ada-cond call))
      (loop (clean-ada-loop call))
      (return (clean-ada-return call))
      (defun (clean-ada-defun call))
      (define-cliche (clean-ada-cliche call))
      (T (clean-ada-~fn call))))
  call)

;note that since we don't support ada-return in a loop, and
;the coder will never produce a return except in a loop, any
;return we run into must be an ada-exit.  But every such return
;will be nested in a cond from the way we do things, so
;if we run into a return, it is an error.

    (defun clean-ada-return (call)
  (cbarf "continue and ignore" "erronious RETURN in ada code ~A" call)
  (clean-kill-call call))

    (defun clean-ada-quote (call)
  (when (equal call '(quote *package*))
    (setq fn-type (cadr call))
    (clean-kill-call call))
  (when (and (listp (cadr call))
             (memq (caadr call) '(str-lit enum-lit int-lit)))
    (rplaca call (caadr call))
    (rplacd call (cdadr call))))

    (defun clean-ada-setq (call)
  (caseq (if (listp (caddr call)) (caaddr call) nil)
    (ada-fn~read
      (let ((var (cadr call))) ;note work to preserve @seg pointers
        (rplaca call '5noop)
        (rplacd call (cddr call))
        (setq call (cadr call))
        (rplaca call 'ada-fn)
        (rplacd call (list* (ncons 'read) (cadr call) var (cddr call)))
        (mapc #'cleanup-ada-fn-call (cddr call))
        (clean-dcl-info call)
        (clean-open-info call)))
    (ada-fn~set_output
      (setq did-set_output t)
      (rplaca call 'ada-fn)
      (rplacd call (cons (ncons 'set_output) (cdaddr call)))
      (cleanup-ada-fn-call (caddr call)))
    (ada-init-dcl~pseudo
      (rplaca call 'ada-init-dcl)
      (rplacd call (list (cadr call)
                         (cadr (caddr (caddr call)))
                         (cadddr (caddr call))))
      (cleanup-ada-fn-call (cadddr call))
      (clean-dcl-info call)
      (clean-kill-call call))
    (ada-constant-dcl~pseudo
      (rplaca call 'ada-constant-dcl)
      (rplacd call (list (cadr call)
                         (cadr (caddr (caddr call)))
                         (cadddr (caddr call))))
      (cleanup-ada-fn-call (cadddr call))
      (clean-dcl-info call)
      (clean-kill-call call))
    (ada-dcl~pseudo
      (rplaca call 'ada-dcl)
      (rplacd call (list (cadr call)
                         (cadr (caddr (caddr call)))))
      (clean-dcl-info call)
      (clean-kill-call call))
    (T (cond ((eq (caddr call) 'nil)
              (clean-kill-call call))
             (T (rplaca call 'ada-setq)
                (cleanup-ada-fn-call (caddr call))
                (clean-dcl-info call))))))

    (defun clean-kill-call (call)
  (rplaca call '5noop)
  (rplacd call nil))

;note that at the moment, there is nothing that can determine that
;a defun is supposed to be an ada function so we only create procedures.
;we would have to work something out to extend this.

    (defun clean-ada-defun (call)
  (let* ((name (cadr call))
         (params (caddr call))
         (body (cdddr call))
         (aux-list (when (and (listp (car body))
                              (eq (caar body) 'let)
                              (null (cdr body)))
                     (prog1 (cadar body) (setq body (cddar body)))))
         (var-list (cons '*return* (append params aux-list)))
         open-info type-info var-type-info use-info with-info
         (final-thing (car (last call)))
         (fn-type (if (member final-thing '('*procedure* '*package*))
                      (cadr final-thing)
                      '*procedure*))
         did-set_output)
    (mapc #'cleanup-ada-fn-call body)
    (let* ((top-with (mapcar #'ncons
                             (sort (makeqset with-info) #'alphalessp)))
           (top-use (delete '(direct_io)
                            (delete '(sequential_io)
                                    (append top-with nil))))
           (opens (clean-get-opens open-info))
           (closes (clean-get-closes opens))
           (stmnts (append opens body closes))
           ;can only hack input args now
           (param-list (clean-get-params params))
           (dcls (clean-get-dcls aux-list)) ;must be after all other dcls
           (exceptions (clean-get-exceptions closes)))
      (rplaca call 'def-ada-top)
      (rplacd call
        (caseq fn-type
          (*package*
            `(,name ,top-with ,top-use
              (def-ada-package ,name ,dcls)))
          (T (let ((innards (if (or dcls stmnts exceptions)
                                (list dcls stmnts exceptions))))
               `(,name ,top-with ,top-use
                 (def-ada-fn (,name ,param-list) ., innards))))))))))

(defun clean-ada-cliche (call)
  (let* ((name (cadr call))
         (comments (caddr call))
         (stmnts (cdddr call))
         open-info type-info var-type-info use-info with-info
         (fn-type '*procedure*) did-set_output aux-list var-list)
    (when (and (null (cdr stmnts))
               (listp (car stmnts))
               (eq (caar stmnts) 'let))
      (setq aux-list (cadar stmnts))
      (setq stmnts (cddar stmnts)))
    (setq var-list aux-list)
    (mapc #'cleanup-ada-fn-call stmnts)
    (let* ((top-with (mapcar #'ncons
                             (sort (makeqset with-info) #'alphalessp)))
           (top-use (delete '(direct_io)
                            (delete '(sequential_io)
                                    (append top-with nil))))
           (opens (clean-get-opens open-info))
           (closes (clean-get-closes opens))
           (new-stmnts (append opens stmnts closes))
           (dcls (clean-get-dcls aux-list)) ;must be after get-closes
           (exceptions (clean-get-exceptions closes)))
      (rplaca call 'def-ada-top)
      (rplacd call
       (caseq fn-type
         (*procedure*
           (let ((innards (if (or dcls new-stmnts exceptions)
                              (list dcls new-stmnts exceptions))))
             `(,name ,top-with ,top-use
               (def-ada-cliche ,name ,comments ., innards)))))))))

(defun dummy-ada-role ()
  (copytree '(kbe-/#-comment-macro () ())))

(defun clean-get-params (vars)
  (mapc #'(lambda (v)
            (if (not (or (assq v var-type-info) (assq v type-info)))
                (push `(,v ada-dcl ,v ,(dummy-ada-role))
                      var-type-info)))
        vars)
  (mapcar #'(lambda (p)
              (or (let* ((entry (assq p var-type-info)))
                    (setq var-type-info (delq entry var-type-info))
                    (cddr entry))
                  (let* ((entry (assq p type-info)))
                    (setq var-type-info (delq entry type-info))
                    (cddr entry))))
          vars))

(defun clean-get-dcls (aux)
  (mapc #'(lambda (v)
            (if (not (or (assq v var-type-info) (assq v type-info)))
                (push `(,v ada-dcl ,v ,(dummy-ada-role))
                      var-type-info)))
        aux)
  (let* ((dcls (append (mapcar #'cdr var-type-info)
                       (mapcar #'cdr type-info))))
    (setq dcls (clean-sort-dcls dcls))
    (when (eq (caar dcls) 'ada-use)
      (setq use-info (append (mapcar #'car (cdr (pop dcls))) use-info)))
    (setq use-info (sort (makeset use-info) #'alphalessp))
    (when (memq 'int_io use-info)
      (setq use-info (delq 'int_io use-info))
      (setq use-info (append use-info (ncons 'int_io))))
    (if use-info
        (push `(ada-use .,(mapcar #'ncons use-info)) dcls))
    dcls))

(defun clean-type-info (info)
  (if (clean-refers-to info 'time) (push 'calendar with-info))
  (if (clean-refers-to info 'text_io) (push 'text_io with-info))
  (if (memq (car info) '(ada-new-package ada-new-fn))
      (caseq (caaddr info)
        ((chained_io keyed_io sort summarize) (push 'functions with-info))
        ((sequential_io direct_io) (push (caaddr info) with-info))))
  (push (cons (cadr info) (copytree info)) type-info))

(defun clean-refers-to (list item)
  (or (eq list item)
      (and (listp list)
           (dolist (l list)
             (if (clean-refers-to l item) (return T))))))

(defun clean-sort-dcls (dcls)
  (setq dcls (clean-aggregate-file-defs dcls))
  (setq dcls (sort dcls #'clean-dcl-sort))
  (prog (packages)
      L (when (not (eq (caar dcls) 'ada-use))
          (if packages (push `(ada-use ., (makeset packages)) dcls))
          (return nil))
        (setq packages (append packages (cdr (pop dcls))))
        (go L))
  (map #'disaggregate-file-def dcls)
  dcls)

(defun clean-aggregate-file-defs (dcls)
  (let* (file-defs removed-dcls file-dcls)
    (dolist (dcl dcls)
      (when (and (eq (car dcl) 'ada-dcl) (eq (caaddr dcl) 'file_type))
        (push dcl file-defs)))
    (when file-defs
      (dolist (def file-defs)
        (let* ((root (singular-form (first-symbol (cadr def))))
               (package-dcl (dcl-for (symbolconc root "_IO") dcls))
               (name-dcl (dcl-for (symbolconc (cadr def) "_NAME") dcls))
               (type-dcls
                 (delq nil
                       (Rlist (dcl-for (car (Elist (reverse
                                                     (cdddr package-dcl))))
                                       dcls))))
               (use (car (member `(ada-use (,(cadr package-dcl))) dcls)))
               file-dcl-list)
          (push def file-dcl-list)
          (if use (push use file-dcl-list))
          (when package-dcl (push package-dcl file-dcl-list))
          (setq file-dcl-list (append type-dcls file-dcl-list))
          (when name-dcl (push name-dcl file-dcl-list))
          (setq removed-dcls (append file-dcl-list removed-dcls))
          (push `(ada-file-def ,(nreverse ;important is file var first
                                  (mapcar #'cadr file-dcl-list))
                               ., file-dcl-list)
                file-dcls)))
      (setq dcls (append file-dcls (qset- dcls removed-dcls))))
    dcls))

(defun dcl-for (symbol dcls)
  (car (mem #'(lambda (x y) (eq x (cadr y))) symbol dcls)))

(defun disaggregate-file-def (dcls)
  (when (eq (caar dcls) 'ada-file-def)
    (rplacd dcls (append (cdddar dcls) (cdr dcls)))
    (rplaca dcls (caddar dcls))))

(defun clean-dcl-sort (dcl1 dcl2)
  (let* ((class1 (clean-dcl-class dcl1))
         (class2 (clean-dcl-class dcl2)))
    (or (ordered-by-references dcl1 dcl2)
        (when (not (ordered-by-references dcl2 dcl1))
          (or (< class1 class2)
              (when (= class1 class2)
                (caseq class1
                  ((0 6) nil)
                  (1 (if (= (length (cadr dcl1)) (length (cadr dcl2)))
                         (alphalessp-dcl dcl1 dcl2)
                         (< (length (cadr dcl1)) (length (cadr dcl2)))))
                  ((2 3) (alphalessp-dcl dcl1 dcl2))
                  ((4 5) (alphalessp-dcl dcl1 dcl2)))))))))

(defun clean-dcl-class (dcl)
  (caseq (car dcl)
    ((ada-use) 0)
    ((ada-file-def) 1)
    ((ada-type-dcl ada-subtype-dcl ada-new-package) 2)
    ((ada-dcl ada-constant-dcl ada-init-dcl) 3)
    (def-ada-fn (if (equal (cadr dcl) '(clean_up nil)) 6 4))
    (ada-new-fn 5)))

(defun alphalessp-dcl (dcl1 dcl2)
  (alphalessp (first-symbol (cadr dcl1)) (first-symbol (cadr dcl2))))

(defun first-symbol (thing)
  (if (listp thing) (first-symbol (car thing)) thing))

(defun ordered-by-references (dcl1 dcl2)
  (caseq (car dcl1)
    (ada-use nil)
    (ada-file-def (Ror-fast (5po1 dcl2 (Elist (cadr dcl1)))))
    ((def-ada-fn ada-new-fn) (5po1 dcl2 (caadr dcl1)))
    (T (5po1 dcl2 (cadr dcl1)))))

(defun clean-dcl-info (info)
  (caseq (car info)
    ((ada-constant-dcl ada-init-dcl)
     (if (or (eq fn-type '*package*) (memq (cadr info) var-list))
         (let ((old-spot (assq (cadr info) var-type-info)))
           (if old-spot (rplacd old-spot (copytree info))
               (push (cons (cadr info) (copytree info)) var-type-info)))))
    (ada-dcl
      (if (clean-refers-to info 'text_io) (push 'text_io with-info))
      ;used to have (when (not (not-fully-specified info))
      (if (or (eq fn-type '*package*) (memq (cadr info) var-list))
          (let ((old-spot (assq (cadr info) var-type-info)))
            (if old-spot
                (unless (not-fully-specified info)
                  (rplacd old-spot (copytree info)))
                (push (cons (cadr info) (copytree info))
                      var-type-info)))))
    (ada-setq (clean-dcl-info-setq info))
    (ada-fn (clean-dcl-info-fn info))))

;Note fn names that are ambiguous type-wise or use/with-wise are ignored.
;This fn should be interacting with a real parser and overload resolution
;and everything.  It is just a gross hack for now.

(defun clean-dcl-info-fn (info)
  (caseq (caadr info) ;figure with
    ((time_of clock month day year) (push 'calendar with-info))
    ((set_output standard_output new_line new_page set_col page line)
     (push 'text_io with-info))
    ((get put)
     (cond ((cdddr info) ;is an integer PUT (GROSS KLUDGE here).
            (push 'functions with-info) (push 'int_io use-info))
           (T (push 'text_io with-info))))
    ((format_date query_user_for_date) (push 'functions with-info))
    ((read write close open create reset size index set_index end_of_file)
     (let ((package
             (ada-file-package-name (get-ada-file-info (caddr info)))))
       (when package
         (push 'maintenance_files with-info)
         (push package use-info)))))
;this part is very incomplete it works in the demo only understanding
;READ since almost every variable in the demo touches a read.
  (caseq (caadr info) ;figure types
    (read
     (let* ((file-info (get-ada-file-info (caddr info)))
            (record-type (or (ada-file-record-type file-info)
                             (intern (string-append
                                       (file-name-root (caddr info))
                                       "_TYPE"))))
            (key-type (or (ada-file-key-type file-info)
                          (intern (string-append
                                    (file-name-root (caddr info))
                                    "_KEY_TYPE")))))
       (clean-dcl-var-type (cadddr info) record-type)
       (if (car (cddddr info))
           (clean-dcl-var-type (car (cddddr info)) key-type))))))

;this is so gross it makes the fn above look great.  It should (and could)
;be really looking for the types of things in the type list and all,
;rather for ease in debugging, it just has appropriate canned knowledge
;about strings which is all it needs to know for the demo.

(defun clean-dcl-info-setq (info)
  (when (listp (caddr info))
    (cond ((or (eq (car (caddr info)) 'str-lit)
               (and (eq (car (caddr info)) 'ada-fn)
                    (eq (caadr (caddr info)) '&)))
           (let ((size (clean-string-size (caddr info))))
             (clean-dcl-var-type
               (cadr info)
               `(ada-string 1 ,(if (plusp size) size (dummy-ada-role))))))
           ((and (eq (car (caddr info))  'ada-fn)
                 (eq (caadr (caddr info)) 'query_user_for_date))
            (clean-dcl-var-type (cadr info) 'time)))))

;this is awfully cludgy, it could be looking things up much better.

(defun clean-string-size (exp)
  (cond ((symbolp exp)
         (caseq exp
           ((model_key ) 3)
           ((defect_key) 5)
           ((unit_key) 7)
           (T -1000)))
        ((listp exp)
         (caseq (car exp)
           (str-lit (string-length (cadr exp)))
           (ada-select
             (cond ((equal (cdr exp) '(model name)) 16)
                   (T -1000)))
           (ada-fn
             (caseq (caadr exp)
               (format_date 10)
               (& (+ (clean-string-size (caddr exp))
                     (clean-string-size (cadddr exp))))))
           (T -1000)))
        (T -1000)))

(defun clean-dcl-var-type (var type)
  (when (symbolp var)
    (let ((spot (assq var var-type-info))
          (new-info
            `(ada-dcl ,var ,(if (symbolp type) (ncons type) type))))
    (cond ((not spot)
           (push (cons var new-info) var-type-info))
          ((not-fully-specified spot)
           (rplacd spot new-info))))))

(defun not-fully-specified (list)
  (cond ((not (listp list)) nil)
        ((eq (car list) 'kbe-/#-comment-macro) T)
        (T (dolist (item list)
             (if (not-fully-specified item) (return T))))))

(defun clean-open-info (info)
  (let* ((file (caddr info))
         (entry
           (copytree
             (caseq (caadr info)
               ((open create) info)
               (read `(ada-fn (open) ,file
                              (enum-lit in_file)
                              ,(symbolconc file  "_NAME")))
               (write `(ada-fn (open) ,file
                               (enum-lit out_file)
                               ,(symbolconc file "_NAME"))))))
         (old-entry (cdr (assq (caddr info) open-info))))
    (cond ((null old-entry) (push (cons file entry) open-info))
          (t (if (not (equal (cadr entry) (cadr old-entry)))
                 (rplaca (cdr old-entry) (ncons 'create)))
             (if (not (equal (cadddr entry) (cadddr old-entry)))
                 (rplaca (cdddr old-entry)
                         (copytree '(enum-lit inout_file))))))))

(defun clean-get-opens (open-info)
  (sort (mapcar #'cdr open-info)
        #'(lambda (a b)
            (if (equal (cadr a) (cadr b))
                (alphalessp (caddr a) (caddr b))
                (equal (cadr a) '(open))))))

(defun clean-get-closes (opens)
  (let* ((closes (mapcar #'(lambda (open) `(ada-fn ,(ncons 'close)
                                                    ,(caddr open)))
                         opens)))
    (if did-set_output
        (push (copytree '(ada-fn (set_output) (ada-fn (standard_output))))
              closes))
    (when (cdr closes)
      (clean-type-info `(def-ada-fn ,(copytree '(CLEAN_UP nil)) nil
                         ,closes
                         ,(copytree '((((STATUS_ERROR)) (ada-return))))))
      (setq closes (copytree '((ada-fn (clean_up))))))
    closes))

(defun clean-get-exceptions (closes)
  (if closes
      (copytree
        `((((DEVICE_ERROR) (END_ERROR) (NAME_ERROR) (STATUS_ERROR))
           ,@ closes
           (ada-fn (PUT) (str-lit "Data Base Inconsistent")))
          (((others)) ,@ closes (ada-raise))))))

(defun clean-ada-~fn (call)
  (let* ((~spot (string-search-char #/~ (string (car call)))))
    (if (null ~spot) (mapc #'cleanup-ada-fn-call (cdr call))
        (let* ((name-string (string (car call)))
               (prefix (intern (substring name-string 0 ~spot) 'kbe))
               (root (intern (substring name-string (1+ ~spot))
                             (symbol-package (car call)))))

          (rplaca call prefix)
          (caseq prefix
            ((ada-use ada-file-def)
             (clean-type-info (rplacd call (cadr (cadr call))))
             (clean-kill-call call))
            (ada-dcl
             (rplacd call (list (cadr (cadr call)) (cadr (caddr call))))
             (clean-dcl-info call)
             (clean-kill-call call))
            ((ada-constant-dcl ada-init-dcl)
             (rplacd call (list (cadr (cadr call))
                                (cadr (caddr call))
                                (cadddr call)))
             (cleanup-ada-fn-call (cadddr call))
             (clean-type-info call)
             (clean-kill-call call))
            ((ada-type-dcl ada-subtype-dcl)
             (clean-type-info
               (list prefix (cadr (cadr call)) (cadr (caddr call))))
             (clean-kill-call call))
            ((ada-new-fn ada-new-package)
             (rplacd call (cadr (cadr call)))
             (clean-type-info call)
             (clean-kill-call call))
            ((def-ada-fn def-ada-generic
              def-ada-package def-ada-package-body)
             (rplacd call (cadr (cadr call)))
             (clean-type-info call)
             (clean-kill-call call))
            (ada-select
              (rplacd call (list (cadr call) root))
;             (if (not (symbolp (cadr call)))
;                 (barf "cannot select field of non variable ~A" call))
              )
            ((ada-construct ada-convert)
             (push (ncons root) (cdr call))
             (mapc #'cleanup-ada-fn-call (cddr call)))
            (ada-attribute
              (push root (cdr call)))
            (ada-fn
              (if (eq root 'read) ;here only if value of read not needed
                  ;we must think up a var name (GROSS!)
                  (push (singular-form (cadr call)) (cddr call)))
              (if (memq root *special-text_io-fns*)
                  (rplaca (cdr call) (ncons root))
                  (push (ncons root) (cdr call)))
              (mapc #'cleanup-ada-fn-call (cddr call))
              (clean-dcl-info call)
              (if (memq root '(open create read write))
                  (clean-open-info call))
              (if (memq root '(open create))
                  (clean-kill-call call))))))))

(defun clean-ada-loop (call)
  (let* ((stmnts (cddr call)) while-test exceptions moved-stmnt new-loop)
    (dolist (stmnt stmnts)
      (when (and (listp stmnt)
                 (eq (car stmnt) 'cond)
                 (null (cddr stmnt))
                 (listp (car (cadr stmnt)))
                 (eq (caar (cadr stmnt)) 'no-ada-exceptions))
        (setq exceptions (cadr (cadr (car (cadr stmnt)))))
        (setq moved-stmnt stmnt)
        (return nil))
      (when (and (listp stmnt)
                 (eq (car stmnt) 'cond)
                 (null (cddr stmnt))
                 (listp (car (cadr stmnt)))
                 (eq (caar (cadr stmnt)) 'not))
        (setq while-test (cadr (car (cadr stmnt))))
        (setq moved-stmnt stmnt)
        (return nil)))
    (if moved-stmnt (setq stmnts (delq moved-stmnt stmnts)))
    (setq new-loop
          (cond (while-test
                 (cleanup-ada-fn-call while-test)
                 (list 'ada-while while-test stmnts nil))
                (exceptions
                 (list 'ada-loop
                       `((ada-begin (,@ stmnts (ada-exit))
                                    ,exceptions))
                       nil))
                (T (list 'ada-loop stmnts nil))))
    (rplaca call (car new-loop))
    (rplacd call (cdr new-loop))
    (mapc #'cleanup-ada-fn-call stmnts)))

(defun clean-ada-cond (call)
  (cond ((and (null (cddr call))
              (null (cddr (cadr call)))
              (member (cadr (cadr call)) '((return nil) (return))))
         (rplaca call 'ada-exit)
         (if (equal (car (cadr call)) '(ada-exit-zero-arg))
             (rplacd call nil)
             (rplacd call (ncons (car (cadr call)))))
         (cleanup-ada-fn-call (cadr call)))
        (T (rplaca call 'ada-cond)
           (dolist (clause (cdr call))
             (if (eq (car clause) 't)
                 (rplaca clause (copytree ''(enum-lit true))))
             (mapc #'cleanup-ada-fn-call clause)))))
