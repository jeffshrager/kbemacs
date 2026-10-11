;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts: MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: IMPLEMENT.LISP; PDF pages 85-123.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.
(comment
;;; the following stuff just exists as part of the Xplan test.
;;; it is not part of the running kbe.

    (defun xplan-test (comment &optional (print-plan nil) (reps 6) &aux pas)
  (with-open-file (file "<pa.xplan>test-results.oplan" 'out)
    (print comment file)
    (print comment)
    (letS (((test-name times average-time seg)
	    (xplan-test0 (Elist '(:copy :average-imp :enumerator-imp))
			 print-plan reps))
	   (text (format nil
			 "~2%times for ~A in oplan in microseconds:~
			  ~{~%    ~8@{~:12D~}~}~
			  ~%Average of middle 50% of times: ~:12D~
			  ~2%Skeleton of the plan produced:~%"
			 test-name times average-time)))
      (multiple-value (nil pas) (construct-plan-def seg))
      (princ text)
      (pl pas)
      (princ text file)
      (pl pas file)))
  (values))

    (defun xplan-test0 (test-name print-plan reps)
  (let* ((a-quarter (// reps 4))
	 times seg middle-times average-time)
    (do ((i reps (1- i))) ((zerop i))
      (multiple-value-bind (s time) (xplan-test1 test-name)
	(push time times)
	(setq seg s)))
    (setq middle-times (sort (copylist times) #'lessp))
    (setq middle-times (nthcdr a-quarter middle-times))
    (setq middle-times (nthcdr a-quarter (reverse middle-times)))
    (setq average-time (// (apply #'+ middle-times) (length middle-times)))
    (if print-plan
	(with-open-file
	  (file (format nil "<pa.xplan>~A-test.oplan" test-name) 'out)
	  (format file
		  ";;; -*- Mode:LISP; Package:KBE; Fonts: MEDFNB; -*-~
		   ~2% ;plan resulting from ~A test~2%"
		  test-name)
	  (multiple-value-bind (plan pas) (construct-plan-def seg)
	    (pl pas file)
	    (pl plan file))))
    (list test-name times average-time seg)))

    (defun xplan-test1 (name)
  (xplan-test-setup)
  (let* ((start-time (time:fixnum-microsecond-time))
	 (seg (caseq name
		(:copy (xplan-test-copy 'mydelete))
		(:average-imp
		  (xplan-test-implement
		    (~slotseg (?& '(mydelete) slot * 'test))
		    'average)
		  'mydelete)
		(:enumerator-imp
		  (xplan-test-implement
		    (~slotseg (?& '(mydelete) slot * 'enumerator))
		    'list-enumeration)
		  'mydelete)
		(T (barf "invalid test name"))))
	 (end-time (time:fixnum-microsecond-time)))
    (values seg (time-difference end-time start-time))))

    (defvar mydelete-plan-file nil "saves reading in the file multiple times")

    (defun xplan-test-setup ()
  (when (null mydelete-plan-file)
    (with-open-file (file "<pa.xplan>mydelete.oplan" 'in)
      (setq mydelete-plan-file (read file))))
  (db-init)
  (eval mydelete-plan-file))

    (defun xplan-test-copy (seg)
  (~seg (copyseg seg nil)))

    (defun xplan-test-implement (seg cliche-name)
  (~seg (implement-internal seg (a*seg `(name cliche ,cliche-name)))))
) ;end of comment

;This stuff allows you to implement something as a reference to a
;port that you cannot make dflow to at first reference.

;it is absolutely essential that the top level value of
;this variable never be changed!
(defconst *pending-dflows* '*top-level*
  "holds dflows that cannot be handled till later")

(defun add-pending-dflow (entry)
  (if (eq *pending-dflows* '*top-level*)
      (barf "*pending-dflows* is not bound"))
  (if (null (eval entry))
      (push entry *pending-dflows*)))

;take care of the ones you can, and leave the rest.

(defun 2check-pending-dflows ()
  (if (eq *pending-dflows* '*top-level*)
      (barf "*pending-dflows* is not bound"))
  (let ((stuff *pending-dflows*))
    (setq *pending-dflows* nil)
    (mapc #'add-pending-dflow stuff)))

;;; This file contains the functions that support the
;;; verbs implement and reimplement.  as well as indefinite references
;;; which create an instance of a cliche.

(defvar 2ROLE-FILLERS nil "The things that fill in the roles in a cliche")

(defun (cliche-instance action) (name &rest 2ROLE-FILLERS)
  (if (eq *pending-dflows* '*top-level*)
      (let (*pending-dflows*)
        (prog1 (cliche-instance-action name)
               (if *pending-dflows* (complain "pending dflows remain"))))
      (cliche-instance-action name)))

(defun cliche-instance-action (name)
  (let* ((segcase (a*seg name))
         (parts (get (get (~clichename (?& segcase cliche))
                          'cliche-comments)
                     'roles)))
    (if (> (length 2role-fillers) (length parts))
        (barf "cliche-instance -- too many role-fillers"))
    (letS* ((filler (car (Elist 2ROLE-FILLERS)))
            (role-name (Elist parts)))
      (implement-internal0
        (~slotseg (?& segcase slot * role-name)) filler))
    (2check-constraints (~seg segcase))
    (ncons segcase)))

(defun a*seg (name)
  (check-name name)
  (caseq (name-type name)
    (cliche (let ((seg (2get-cliche (name-symbol name))))
              (if (not seg)
                  (& (create-cliche-for-function (name-symbol name)))
                  (& seg))))
    (program (& (create-cliche-for-function (name-symbol name))))
    (T (complain "~A not cliche or function name"
                 (unparse-command name)))))

;this gets hold of an instance of a cliche.  see defprototype
;to see how they get set up in the first place.
;Note that it does some variable renaming stuff based on suggestions.

;Note each kind of constraint either runs at creation time on the
;plan list (eg rename) or later on the plan db (eg default)

(defvar 2creation-time-plan nil)

(declare (special 2cliche-name))

(defun 2get-cliche (2cliche-name)
  (let ((2creation-time-plan (get 2cliche-name 'cliche-plan)))
    (when 2creation-time-plan
      (mapcar #'2check-constraint
              (get (get 2cliche-name 'cliche-comments)
                   'constraints))
      (~seg (eval 2creation-time-plan)))))

;note that this is pretty dangerous because it renames so much stuff.
;you must make sure that the thing you are renaming isn't some
;special keyword or anything.  This could be fixed better in principle
;however.  It assmes that the old-morphene is singular as is
;the new-morphene.  It finds every place the old-morphene occurs, and
;changes it to the new-morphene.  The old-morphene is said to occur
;anywhere it (or its plural form) exists in a name preceded by -, _,
;space, or the start of the name, and followed by -, _, space, or
;the end of the name.

(defun rename-all (old-morphene new-morphene)
  (when (and 2creation-time-plan
             (not (member new-morphene '(nil "nil" "nils"))))
    (let ((alist (2get-rename-alist
                   old-morphene new-morphene 2creation-time-plan)))
      (if alist (setq 2creation-time-plan
                      (ada-sublis alist 2creation-time-plan))))))

;this looks for every symbol with the old prefix and sets up
;an appropriate alist for renaming.  it assumes that everything
;is upper case at the moment

(declare (special 2rename-alist 2rename-old-singular 2rename-old-plural
                  2rename-new-singular 2rename-new-plural))

(defun 2get-rename-alist (old new stuff)
  (let* (2rename-alist
         (2rename-old-singular (string-upcase old))
         (2rename-old-plural (pluralize 2rename-old-singular))
         (2rename-new-singular (string-upcase new))
         (2rename-new-plural (pluralize 2rename-new-singular)))
    (2gra stuff)
    2rename-alist))

(defun pluralize (string)
  (or (cdr (assoc string '(("datum" . "data"))))
      (string-pluralize string)))

(defun 2gra (stuff)
  (cond ((and (or (symbolp stuff) (stringp stuff))
              (not (assq stuff 2rename-alist))) ;saves time and duplication
         (2gra0 stuff))
        ((listp stuff) (mapc #'2gra stuff))))

;this assumes that everything is upper case for now.

(defun 2gra0 (item)
  (let* ((string (string item))
         (parts)
         (parts-end 0)
         (start 0)
         end
         (slen (string-length 2rename-old-singular))
         (plen (string-length 2rename-old-plural))
         (len (string-length string)))
    (prog ()
     L (cond ((and (%string-equal
                     2rename-old-singular 0 string start slen)
                   (or (>= (+ start slen) len)
                       (memq (aref string (+ start slen))
                             '(#/- #/_ #\space))))
              (when (not (= start parts-end))
                (push (substring string parts-end start) parts))
              (push 2rename-new-singular parts)
              (setq start (+ start slen))
              (setq parts-end start))
             ((and (%string-equal
                     2rename-old-plural 0 string start plen)
                   (or (>= (+ start plen) len)
                       (memq (aref string (+ start plen))
                             '(#/- #/_ #\space))))
              (when (not (= start parts-end))
                (push (substring string parts-end start) parts))
              (push 2rename-new-plural parts)
              (setq start (+ start plen))
              (setq parts-end start)))
       (setq end (string-search-set "-_ " string start))
       (when (null end)
         (when parts
           (when (not (= parts-end len))
             (push (substring string parts-end) parts))
           (let ((new-item (apply #'string-append (reverse parts))))
             (if (symbolp item)
                 (setq new-item
                       (maybe-intern new-item (symbol-package item))))
             (push (cons item new-item) 2rename-alist)))
         (return))
       (setq start (1+ end))
       (go L))))

;note this works on LISP or ADA.

(defun file-name-root (thing)
  (or (cond ((null thing) nil)
            ((stringp thing)
             (let* ((spot (string-search-set  "._-" thing))
                    (root
                      (intern (if spot
                                  (string-upcase (substring thing 0 spot))
                                  thing)
                              'kbe)))
               (or (singular-form root) root)))
            ((and (listp thing) (memq (car thing) '(quote str-lit)))
             (file-name-root (cadr thing)))
            ((and (listp thing) (equal (car thing) '(ada-string)))
             (file-name-root (string (cadr thing))))
            ((and (listp thing) (null (cdr thing))) ;maybe a segcase
             (file-name-root (car thing)))
            ;; These next two things are a gross cludge, but
            ;; there are lots of ways you could do it right by
            ;; either storing info with the cliche, or searching
            ;; over the plan.  I am just cheating for now.
            ((and (symbolp thing) (memq thing *db-segs*)
                  (??& (&* thing) cliche 'file_summarization))
             'summary)
            ((and (symbolp thing) (memq thing *db-segs*)
                  (??& (&* thing) cliche 'file_selection))
             'selection)
            ((and (symbolp thing) (memq thing *db-segs*))
             (file-name-root
               (or (~01 ~name (??& (&* thing) @seg 'constant))
                   (and (??& (&* thing) @seg 'funcall '5noop)
                        (~01 string ~name
                             (??& (&* (&* thing) input) @io
                                  'actual-arg * 1))))))
            ((symbolp thing) (file-name-root (string thing))))
      (*throw '2constraint-abort nil)))

;this creates a cliche for a function.

(declare (special 2NAME))

(defun create-cliche-for-function (symbol)
  (let* ((type (cond ((fboundp symbol) 'lisp)
                     ((eq (ada-symbol-type symbol) 'fn) 'ada)
                     (T (barf "~A not a function" symbol))))
         (arglist (caseq type
                    (lisp (arglist symbol))
                    (ada (mapcar #'(lambda (x ignore) x)
                                 '(x y z w)
                                 (do ((info ada-info (cdr info)))
                                     ((null info))
                                   (declare (special ada-info))
                                   (when (eq symbol (cadaar info))
                                     (return (caadar info))))))))
         (length (length arglist)))
    (cond ((or (> length 2)
               (memq '&rest (cdr arglist)) ; kludge -- ok if is first
               (memq '&quote arglist)
               (memq '&optional arglist))
           (barf "Cannot create cliche for arglist ~A" arglist)))
    (let* ((template (get (cond ((= length 0) 'zeroary-function)
                                ((= length 1) 'unary-function)
                                ((= length 2) 'binary-function))
                          'cliche-plan)))
      (~seg (eval (subst (caseq type
                           (lisp symbol)
                           (ada (intern (string-append "ADA-FN-" symbol))))
                         'fn template))))))

;note that some constraints do not run at creation time.
;We must test to see that they don't

(defvar *2focus-seg* nil)
(defvar *supress-constraints* nil "block application of constraints")

(defun 2check-constraints (*2focus-seg*)
  (when (and (not *supress-constraints*) *2focus-seg*)
    (let ((cliche-name
            (~01 ~clichename (??& (&* *2focus-seg*) cliche))))
      (cond ((null cliche-name))
            ((eq cliche-name 'pseudo-subcliche)
             (2check-constraints
               (~01 ~seg ~segcase (??& * slot (&* *2focus-seg*)))))
            (T (mapcar #'2check-constraint
                       (get (get cliche-name 'cliche-comments)
                            'constraints)))))))

(defun 2check-constraint (constraint)
  (*catch '2constraint-abort
    (if (2check-should-eval constraint) (eval constraint))))

;this tries to see if you shouldn't even attempt the constraint.
;it is important because it can save a great deal of time.

(defun 2check-should-eval (constraint)
  (let* ((fn-name (car constraint))
         (first-arg (cadr constraint)))
    (when (eq fn-name 'ada-fn)
      (setq fn-name (caadr constraint))
      (setq first-arg (caddr constraint)))
    (cond (2creation-time-plan (not (memq fn-name '(default derived))))
          ((memq fn-name '(rename-all rename rename_all)) nil)
          ((eq fn-name 'default)
           (let ((segcase (eval first-arg)))
             (and segcase (not-implemented? (~seg segcase)))))
          (T T))))

(defun default (segcase source)
  (when (not 2creation-time-plan)
    (when (and segcase (not-implemented? (~seg segcase)))
      (2check-constraints
        (~seg (implement-seg (~seg segcase)
                             (~seg (plan-for-source source))))))))

(defun derived (segcase source)
  (when (not 2creation-time-plan)
    (let* ((current-code (car (?@seg-code (~seg segcase)))))
      (if (and (listp current-code) (eq (car current-code) '*code-is-car*))
          (setq current-code (cadr current-code)))
      (when (or (not-implemented? (~seg segcase))
                (not (equal current-code source)))
        (implement-seg (~seg segcase) (~seg (plan-for-source source)))))))

(defun plan-for-source (source)
  (if (and (listp source) (memq (car source) *db-segs*)) source
      (create-plan-for-code source)))

;note that all the fns you write to work in constraint things basically
;have to take in segments and produce either code or a chunk of plan.

;these are terrible cludges but work in simple cases.
;they just assume that every string with ~s in it in a hunk
;of code is a format string.

(defvar total-size 0)

(defun size-in-lines (segcase)
  (if (not segcase) (*throw '2constraint-abort nil))
  (let ((total-size 0))
    (cond ((not-implemented? (~seg segcase)) 1)
          (T (mapcar #'line-size-0 (all-subsegs (~seg segcase)))
             total-size))))

(defun line-size-0 (seg)
  (when (eq (?@seg-kind seg) 'constant)
    (let ((stuff (cadr (?@seg-name seg))))
      (if (stringp stuff) (line-size-1 stuff)))))

(defun line-size-1 (string)
  (do ((chars (nreverse (listarray string)) (cdr chars)))
      ((null chars))
    (when (member (car chars) '(#/% #/&))
      (setq total-size (+ total-size (ls-numeric-arg chars))))))

;this works pretty well (note digits are reversed already in the list)
(defun ls-numeric-arg (list)
  (let (digits)
    (dolist (char list)
      (if (= char #/~) (return nil))
      (if (member char '#.(listarray "0123456789"))
          (push char digits)))
    (if (null digits) (push #/1 digits))
    (readlist digits)))

(defun size-in-characters (segcase)
  (let ((total-size 0))
    (cond ((not-implemented? (~seg segcase)) 1)
          (T (mapcar #'char-size-0 (all-subsegs (~seg segcase)))
             total-size))))

(defun char-size-0 (seg)
  (when (eq (?@seg-kind seg) 'constant)
    (let ((stuff (cadr (?@seg-name seg))))
      (if (stringp stuff) (char-size-1 stuff)))))

;this is real weak - only works if length args specified in format.
(defun char-size-1 (string)
  (do ((chars (nreverse (listarray string)) (cdr chars)))
      ((null chars))
    (when (member (car chars) '#.(listarray "AaSsDdOoFfEeXx"))
      (setq total-size (+ total-size (ls-numeric-arg chars))))))

;these are some handy primitives for renaming things.

(defun 2rename-vars-avoiding (seg avoid-list)
  (let* ((names (2var-names-used seg))
         (name-clashes (qset^ names avoid-list)))
    (if name-clashes
        (2rename-var-names seg
           (letS ((clash (Elist name-clashes)))
             (Rlist (cons clash (2find-safe-name clash avoid-list))))))))

(defun 2var-names-used (seg)
  (makeqset (mapcan #'2var-names-used-seg (all-subsegs seg))))

(defun 2var-names-used-seg (seg)
  (let (names)
    (lets ((name (5goodname (~name (Elist (??& (&* (&* seg) port) @io))))))
      (if name (push name names)))
    (lets ((name
             (5goodname
               (~varname (Elist (??& (&* (&* seg) port) @createvar))))))
      (if name (push name names)))
    names))

(defun 2rename-var-names (seg alist)
  (Rignore (2rename-var-names-seg (Elist (all-subsegs seg)) alist)))

(defun 2rename-var-names-seg (seg alist)
  (lets* ((@ioF (Elist (??& (&* (&* seg) port) @io)))
          (name (5goodname (~name @ioF)))
          (new-name (cdr (assq name alist))))
    (when new-name
      (make-fact-be-out @ioF)
      (assert-fact (&~name @ioF new-name))))
  (lets* ((@createvarF (Elist (??& (&* (&* seg) port) @createvar)))
          (name (5goodname (~varname @createvarF)))
          (new-name (cdr (assq name alist))))
    (when new-name
      (make-fact-be-out @createvarF)
      (assert-fact (&~varname @createvarF new-name)))))

(defun 2find-safe-name (name avoid-list)
  (let ((root (substring name
                         0
                         (1+ (string-reverse-search-not-set
                               "-_0123456789" (string name))))))
    (do ((i 1 (1+ i)))
        (nil)
      (let ((new-name (let ((base 10.) (*nopoint t))
                        (maybe-intern (text "~A~A" root i)
                                      (symbol-package name)))))
        (if (not (memq new-name avoid-list))
            (return new-name))))))

;IMPLEMENT and REIMPLEMENT.

(define-db-action copy (to (seg-reference (parse-seg-reference))
                           (reference (parse-reference)))
                  "Copies the cliche <reference> to
                   the cliche <seg-reference>"
  (setq reference ;kludge converts the phrase to a phrase.
        `(cliche-instance (name cliche ,(caddr (caddr reference)))))
  (if (eq *pending-dflows* '*top-level*)
      (let (*pending-dflows*)
        (prog1
          (*implement (add-descriptor seg-reference 'unimplemented)
                      'like reference)
          (if *pending-dflows* (complain "pending dflows remain"))))
      (*implement (add-descriptor seg-reference 'unimplemented)
                  'like reference)))

(*define-completion 'as)
(*define-completion 'like)

(define-db-action implement ((seg-reference (parse-seg-reference))
                             (as-or-like (require-one-of '(as like)))
                             (reference (parse-reference)))
                  "Implements the segment given by <seg-reference>
                   <as//like> the code fragment given by <reference>."
  (if (eq *pending-dflows* '*top-level*)
      (let (*pending-dflows*)
        (prog1
          (*implement (add-descriptor seg-reference 'unimplemented)
                      as-or-like
                      reference)
          (if *pending-dflows* (complain "pending dflows remain"))))
      (*implement (add-descriptor seg-reference 'unimplemented)
                  as-or-like
                  reference)))

(defun *implement (old-ref relation new-ref)
  (let ((old (eval-command old-ref))
        (new (let ((*supress-constraints* (eq relation 'like)))
               (eval-command new-ref))))
    (let ((failures '()))
      (cond ((not old) (push "~0G~A is unknown to me" failures))
            ((or (cdr old)
                 (neq (thing-type (car old)) 'seg))
             (push "~0G~A is not a seg" failures)))
      (cond ((not new) (push "~1G~A is unknown to me" failures))
            ((cdr new)
             (push "~1G~A is not a single seg or port" failures)))
      (if failures
          (let ((unref1 (unparse-command old-ref))
                (unref2 (unparse-command new-ref)))
            (complain
              (auto-fill-string
                (text "Unable to implement ~A as ~A because ~A."
                      unref1 unref2
                      (phrase-append "and"
                                     (mapcar #'(lambda (x)
                                                 (text x unref1 unref2))
                                             failures))))))))
    (if (not (not-implemented? (~seg (car old))))
        (complain "~A was already implemented."
                  ;; Create a fully-qualified reference to the seg...
                  (string-capitalize
                    (unparse-command (make-ref (car old) t)))))
    (finish-edit <IMP
      (list (caseq relation
              (as (implement-internal (car old) (car new)))
              (like (implement-like (car old) (car new))))))))

(define-db-action reimplement ((seg-reference (parse-seg-reference))
                               (as-or-like (require-one-of '(as like)))
                               (reference (parse-reference)))
                  "Re-implements the segment given by <seg-reference> as
                   the code fragment given by <reference>."
  (if (eq *pending-dflows* '*top-level*)
      (let (*pending-dflows*)
        (prog1
          (*reimplement (add-descriptor seg-reference 'implemented)
                        as-or-like
                        reference)
          (if *pending-dflows* (complain "pending dflows remain"))))
      (*reimplement (add-descriptor seg-reference 'implemented)
                    as-or-like
                    reference)))

(defun *reimplement (old-ref relation new-ref)
  (let ((old (eval-command old-ref))
        (new (let ((*supress-constraints* (eq relation 'like)))
               (eval-command new-ref))))
    (let ((failures '()))
      (cond ((not old) (push "~0G~A is unknown to me" failures))
            ((or (cdr old)
                 (neq (thing-type (car old)) 'seg))
             (push "~0G~A is not a seg" failures)))
      (cond ((not new) (push "~1G~A is unknown to me" failures))
            ((cdr new)
             (push "~1G~A is not a single seg or port" failures)))
      (if failures
          (let ((unref1 (unparse-command old-ref))
                (unref2 (unparse-command new-ref)))
            (complain
              (auto-fill-string
                (text "Unable to reimplement ~A as ~A because ~A."
                      unref1 unref2
                      (phrase-append "and"
                                     (mapcar #'(lambda (x)
                                                 (text x unref1 unref2))
                                             failures))))))))
    (if (not-implemented? (~seg (car old)))
        (complain "~A is not yet implemented."
                  ;; Create a fully-qualified reference to the seg...
                  (string-capitalize
                    (unparse-command (make-ref (car old) t)))))
    (finish-edit <REIMP
      (list (caseq relation
              (as (implement-internal (car old) (car new)))
              (like (implement-like (car old) (car new))))))))

;this defines a top level thing by copying all of the
;plan for another thing over to it.  Only implementing
;like of cliches are support at the moment.

;currently must say "implement the cliche FOO like a bar."
;later maybe support "Implement the program FOO like the program BAR"

(defun implement-like (oldthing newthing)
  (let* ((cliche-def (~01 (--& oldthing cliche-def)))
         (new-cliche-name (~01 ~clichename (??& newthing cliche)))
         (new-comments (cdr (get new-cliche-name 'cliche-comments))))
    (if (not (and cliche-def new-cliche-name))
        (barf "can only IMPLEMENT a top level cliche LIKE another cliche"))
    (>& newthing cliche-def (~cliche-def-name cliche-def) new-comments)
    (kill-seg-and-its-subsegs (~seg oldthing))
    newthing))

;this fn allows you to define what the insides of a preexisting
;segment are.  It takes in two segments. it throws away everything
;inside the old original one, and puts everything from the second into
;it.  (note this destroys both of the inputs, producing the result.)
;The hard part is trying to reconcile the way the old seg is
;interfaced to the world, with the way the new one is.  The primary
;vehical for this is looking at the ports.

(defun implement-internal (oldthing newthing)
  (let* ((slot-superior (~01 ~seg ~segcase (??& * slot oldthing)))
         (result (implement-internal0 oldthing newthing)))
    (2check-constraints slot-superior)
    (2check-pending-dflows)
    result))

(defun implement-internal0 (oldthing newthing)
  (cond ((memq (thing-type newthing) '(input tinput output toutput))
         (implement-seg-as-port (~seg oldthing) newthing))
        ((not (eq (thing-type newthing) 'seg))
         (barf "can only implement something as a seg or port"))
        ((?supseg (~seg newthing))
         ;assume is a preexisting seg in same deffn and use dflow.
         (implement-seg-as-port (~seg oldthing)
                                (~deep (?& (&* newthing output)
                                           @io 'returnval))))
        (T (implement-seg (~seg oldthing) (~seg newthing)))))

(defun implement-seg-as-port (oldseg port)
  (let ((newseg (~seg (eval '(plan-def 5noop nil
                               (DEFFACTS 5NOOP
                                 (SEGTYPE (SIMPLE))
                                 (PLANTYPE (NEW-VIEW))
                                 (INPUT (NIL IN NIL))
                                 (OUTPUT (PART OUT IN))
                                 (@SEG (FUNCALL 5NOOP NIL))
                                 (@IO (IN ACTUAL-ARG NIL 1)
                                      (OUT RETURNVAL NIL NIL))))))))
    ;make sure no dflow hooked up to oldseg input
    (2remove*assertions (??& * dflow * (&* (&* oldseg) input)))
    (--& (&* (&* oldseg) input) @io)
    (--& (&* oldseg) input)
    (2imp oldseg newseg)
    (let* ((in (?& (&* newseg) input)))
      (add-pending-dflow `(implement-seg-as-port-dflow ',in ',port)))
    (& newseg)))

(defun implement-seg-as-port-dflow (in port)
  (when (equal (top-segcase (~segcase in)) (top-segcase (~segcase port)))
    (implement-dflow port in)
    T))

;This implements one segment as another.  The work done by this
;individual fn is to determine how to make a proper interface between
;the two segments.  Note that compatability of segments is implemented
;by using library cliches.  (something should be done so we don't
;loose the info from the ports on the oldseg when one of the
;intermediate cliches is used.)

;we make the following assumptions
; oldseg must be simple and cannot be a recursion, ssr-body, pred, or join.
; newseg must be simple and cannot be a recursion, ssr-body, pred, or join.
;  In addition it cannot be a lambda-exp, or ssr.

(defun implement-seg (oldseg newseg)
  (let* ((old-type (2imp-type oldseg))
         (new-type (2imp-type newseg))
         (seg))
    (if (or (eq old-type 'cannot-implement)
            (memq new-type '(cannot-implement ssr top)))
        (barf "IMPLEMENT-SEG: cannot use these segments in implement~
               ~% oldseg: ~S~% newseg: ~S"
               oldseg newseg))
    (caseq old-type
      (ssr ;this will only be if the coersion below happend already
        (cond ((eq (?plantype newseg) 'tempcomp)
               (setq seg (2imp oldseg newseg)))
              (t (setq seg (2imp oldseg (2get-cliche 'map)))
                 (2imp (?1role seg 'op) newseg))))
      (simple
        (if (??& (&* newseg) plantype 'tempcomp)
            (cond ((and (??& * role (&* oldseg) 'op))
                   (let ((ssr (~seg~segcase (?& * role (&* oldseg) 'op)))
                          (slotF (?& * slot (&* oldseg))))
                     (assert-fact (&~slotseg slotF (& ssr)))
                     (setq oldseg ssr))
                   (setq seg (2imp oldseg newseg)))
                  ((check-isnt-init oldseg (containing-tc oldseg))
                   (setq seg (implement-in-tc oldseg
                                              (containing-tc oldseg)
                                              newseg)))
                  ((check-isnt-init oldseg (refered-to-tc oldseg newseg))
                   (setq seg (implement-in-tc oldseg
                                              (refered-to-tc oldseg newseg)
                                              newseg)))
                  ;this is the only time a nested loop is formed.
                  (T (setq seg (2imp oldseg newseg))))
            (setq seg (2imp oldseg newseg))))
      (top ;this happens a lot.
        (cond ((and (??& (&* oldseg) cliche-def)
                    (??& (&* newseg) cliche))
               (setq seg (2imp-top oldseg (2get-cliche 'simple-subseg)))
               (--& (&* seg) cliche)
               (--& (&* seg) slot)
               (2imp (car (??subseg seg)) newseg))
              ((null (??subseg newseg))
               (setq seg (2imp-top oldseg (2get-cliche 'simple-subseg)))
               (2imp (car (??subseg seg)) newseg))
              (t (setq seg (2imp-top oldseg newseg))))))
    (& seg)))

;note that you cannot implement a cliche-def directly as a cliche because
;you couldn't tell which slots went with what.

(defun check-isnt-init (seg tc)
  (when tc
    (let ((supseg (?supseg seg)))
      (cond ((or (null supseg) (eq tc supseg)) T)
            ((and (eq (?plantype supseg) 'ssr)
                  (??& (&* supseg) role (&* seg) 'init)) nil)
            (T (check-isnt-init supseg tc))))))


;this deterimes type information needed by implement-seg

(defun 2imp-type (seg)
  (let* ((stype (?segtype seg))
         (ptype (?plantype seg))
         (@type (?@seg-kind seg))
         (role-name (~01 ~rolename (??& * role (&* seg)))))
    (cond ((or (not (eq stype 'simple))
               (memq ptype '(ssr-body join))
               (memq @type '(join recursion))
               (memq role-name '(join rec body dfjoin)))
           'cannot-implement)
          ((or (eq @type 'lambda-exp)
               (??& (&* seg) cliche-def))
           'top)
          ((or (eq ptype 'ssr)
               (memq role-name '(bssr gen acc trn term filter)))
           'ssr)
          (t 'simple))))

;this implements a topmost seg with another one.  The old ports are
;preserved in addition to any new ones.  Unless the new seg is also a top
;seg things are logically as if the new seg was made a subseg of the old
;one.  Note it is vital that the top seg itself maintains integrity.
;(this really should be hacked so that if one of the outputs of the newseg
;is a part of one of its inputs this fact carries over.)

(defun 2imp-top (oldseg newseg)
  (dolist (fact (seg-facts oldseg))
    (make-fact-be-out fact)
    (fact-key-dispatch fact
      ((input output tinput toutput @io @seg @createvar cliche-def)
       (make-fact-be-in fact))
      ((subseg (kill-seg-and-its-subsegs (~seg~subseg fact))))))
  (over (??& (&* newseg) input) -> in
        (~01 ~name (??& in @io)) -> name
    (2replace-port in
     (or (and name (~1st ~deep (??& (&* (&* oldseg) input) @io * name)))
         (let ((newport (>& (& oldseg) input nil
                            (genobj oldseg (~obj in)) nil)))
           (over (--& in @io) (assert-fact (&~deep _ newport)))
           newport)))
    (make-fact-be-out in))
  (over (??& (&* newseg) output) -> out
        (~01 ~name (??& out @io)) -> name
    (2replace-port out
     (or (and (??& out @io 'returnval)
              (~1st ~deep (??& (&* (&* oldseg) output) @io 'returnval)))
         (and name (~1st ~deep (??& (&* (&* oldseg) output) @io * name)))
         (let ((newport (>& (& oldseg) output nil
                            (genobj oldseg (~obj out)) nil)))
           (over (--& out @io) (assert-fact (&~deep _ newport)))
           newport)))
    (make-fact-be-out out))
  (dolist (fact (seg-facts newseg))
    (make-fact-be-out fact)
    (fact-key-dispatch fact
      ((segtype subseg dflow plantype role slot cliche)
       (assert-fact (&~segcase fact (& oldseg))))
      (@seg (when (eq (~kind fact) 'lambda-exp)
              (--& (&* oldseg) @seg)
              (assert-fact (&~segcase fact (& oldseg)))))
      (cflow (>& (& oldseg) cflow
                 (cond ((not (eq (~seg (~from fact)) newseg))
                        (~from fact))
                       (t (& oldseg (~case (~from fact)))))
                 (cond ((not (eq (~seg (~to fact)) newseg))
                        (~to fact))
                       (t (& oldseg (~case (~to fact)))))))))
  oldseg)

(defun containing-tc (seg)
  (let ((supseg (?supseg seg)))
    (cond ((null supseg) nil)
          ((eq (?plantype supseg) 'tempcomp) supseg)
          (T (containing-tc supseg)))))

;this just finds first one which is certainly buggy.

(defun refered-to-tc (oldseg newseg)
  (or (letS ((free-in
               (Elist (??& (&* (&* newseg) input) @io))) ;actual-arg too
             (in (~deep free-in))
             (name (~name free-in))
             (source (2find-var-source name oldseg 'before))
             (tc-seg (if source (containing-tc (~seg~segcase source)))))
        (if tc-seg (done tc-seg)))
      (letS ((pending (Elist *pending-dflows*)))
        (caseq (car pending)
          (implement-seg-as-port-dflow
            (let* ((in (eval (cadr pending)))
                   (port (eval (caddr pending)))
                   (tc (containing-tc (~seg~segcase port))))
              (if (and (2seg-inside-seg (~seg~segcase in) newseg) tc)
                  (done tc))))
          (2imp-input-dflow
            (let* ((in (eval (cadr pending)))
                   (name (eval (caddr pending)))
                   (source (2find-var-source name oldseg 'before))
                   (tc (if source
                           (containing-tc (~seg~segcase source)))))
              (if (and (2seg-inside-seg (~seg~segcase in) newseg) tc)
                  (done tc))))))))

(defun 2seg-inside-seg (low-seg high-seg)
  (and low-seg
       (or (eq low-seg high-seg)
           (2seg-inside-seg (?supseg low-seg) high-seg))))

(defun implement-in-tc (oldseg tc-seg newseg)
  (let* ((ssr-seg (genseg 'ssr-seg))
         (ret (~01 ~deep (??& (&* (&* newseg) output) @io 'returnval)))
         (out-slot (~01 ~slotseg (??& (&* newseg) slot * * 'output)))
         (out-ret
           (if out-slot
               (~01 ~deep (??& (&* out-slot output) @io 'returnval)))))
    (>& (& tc-seg) subseg (& ssr-seg))
    (>& (& tc-seg) role (& ssr-seg) 'acc)
    (2imp ssr-seg newseg)
    (~seg (implement-seg-as-port oldseg (or ret out-ret)))))

;this has a GROSS kludge in it a the moment for the demo, because I
;haven't figured out how to make it work in general.  It is the
;coder that should really be figuring this out.

(defun 2rename-vars-for-imp (newseg topseg)
  (let* ((newseg-names (2var-names-used-seg newseg))
         avoid-names)
    (dolist (pending *pending-dflows*)
      (if (and (eq (car pending) '2imp-input-dflow)
               (2seg-inside-seg
                 (~seg~segcase (eval (cadr pending))) newseg))
          (push (eval (caddr pending)) newseg-names)))
    (setq avoid-names (qset- (2var-names-used topseg) newseg-names))
    ;;here come da kludge.
    (setq avoid-names (qset^ '(sum) avoid-names))
    (if avoid-names
        (2rename-vars-avoiding newseg avoid-names))))

;this is the heart of the matter it replaces an inner seg with
;another.  the ports on the old one, and dflow touching them serve
;only to specify how to hook the new seg up, the old seg info is then
;just thrown away.

(defun 2imp (oldseg newseg)
 (let ((topseg (~seg (top-segcase (& oldseg))))
       (subsegs (--& * subseg (&* oldseg))))
;change variable names in newseg so that we won't confuse coder.
;note that variable names on the top of newseg are ok if they
;clash. we must do this after oldseg is out but before newseg is in.
  (2rename-vars-for-imp newseg topseg)
;connect newseg into parent.
  (over subsegs (assert-fact (&~subseg _ (& newseg))))
  (over (--& * slot (&* oldseg)) (assert-fact (&~slotseg _ (& newseg))))
  (over (--& * role (&* oldseg)) (assert-fact (&~roleseg _ (& newseg))))
  (over (--& * cflow * (&* oldseg)) (assert-fact (&~to _ (& newseg))))
  (over (--& * cflow (&* oldseg)) ;maybe bug if strange case names!
    (assert-fact (&~from _ (& newseg (~case (~from _))))))
;hook up the ports and slots of new.
  (letS* ((old-dummy (filterS #'dummy-port?
                              (Elist (??& (&* oldseg) toutput))))
          (new-dummy (2make-dummy-toutput newseg)))
    (2replace-port old-dummy new-dummy))
  (letS ((free-out (Elist (??& (&* (&* newseg) output) @io 'free-var)))
         (out (~deep free-out))
         (name (~name free-out))
         (old-out (and name (~1st ~deep (??& (&* (&* oldseg) output)
                                             @io 'free-var name)))))
    (cond (old-out
           (2replace-port old-out out)
           (make-fact-be-out old-out))))
  (letS ((se (Elist (??& (&* (&* newseg) output) @io 'side-effect)))
         (out (~deep se))
         (name (~name se))
         (old-out (2find-side-effect-out oldseg name)))
    (cond (old-out
           (2replace-port old-out out)
           (make-fact-be-out old-out))
          (T (2splice-into-parallel-dflows out name))))
  (let ((ret (~01 ~deep (??& (&* (&* newseg) output) @io 'returnval)))
        (old-ret (~01 ~deep (??& (&* (&* oldseg) output) @io 'returnval))))
    (cond ((and ret old-ret)
           (2replace-port old-ret ret)
           (make-fact-be-out old-ret))))
  ;;note we can hook up an output slot again even if it is used.
  (let* ((output-slotFs (??& (&* newseg) slot * * 'output))
         (out (sole-used-output? (?supseg newseg) oldseg)))
    (cond ((and out (null (??& out @io 'returnval))
                output-slotFs (null (cdr output-slotFs)))
           (letS ((to (Elist (~set ~to (??& * dflow out)))))
             (implement-dflow (?& (~slotseg (car output-slotFs)) output)
                              to))
           (make-fact-be-out out))))
;the next three things are needed so we can find ports for variables
  (assert-fact (&~code (-& (&* newseg) @seg)
                       (~01 ~code (??& (&* oldseg) @seg))))
  (--& (&* oldseg) cflow)
  ;we had to keep the dflow till found side-effect and free-var outputs.
  (--& (&* oldseg) dflow)
;we did the outputs first to help fix the position of newseg
  (letS* ((old-dummy (filterS #'dummy-port?
                              (Elist (??& (&* oldseg) tinput))))
          (new-dummy (2make-dummy-tinput newseg)))
    (2replace-port old-dummy new-dummy))
  (letS ((free-in (Elist (??& (&* (&* newseg) input) @io))) ;actual-arg too
         (in (~deep free-in))
         (name (~name free-in))
         (old-in (and name (or (~1st ~deep (??& (&* (&* oldseg) input)
                                                 @io 'free-var name))
                               (2input-from-var oldseg name)))))
    (cond (old-in
           (2replace-port old-in in)
           (make-fact-be-out old-in))
          (name (add-pending-dflow `(2imp-input-dflow ',in ',name)))))
  (let* ((input-slotFs
           (Rlist (filterS #'(lambda (x)
                               (?empty-slot (~seg (~slotseg x))))
                           (Elist (??& (&* newseg) slot * * 'input)))))
         (in (sole-fed-input? (?supseg newseg) oldseg)))
    (cond ((and input-slotFs (null (cdr input-slotFs)) in)
           (let* ((slotseg (~slotseg (car input-slotFs)))
                  (new-in (>& slotseg input nil
                              (genobj (~seg slotseg) 'in) nil)))
             (>& new-in @io 'actual-arg '_ 1)
             (--& slotseg @seg)
             (>& slotseg @seg 'funcall '5noop nil)
             (implement-dflow (~from (-& * dflow * in)) new-in)
             (make-fact-be-out in)))))
  (2remove*assertions (??& * dflow * (&* (&* oldseg) port)))
  (2remove*assertions (??& * dflow (&* (&* oldseg) port)))
  (2remove*assertions (??& * tinput * * (&* (&* oldseg) port)))
  (2remove*assertions (??& * toutput * * (&* (&* oldseg) port)))
  (kill-seg-and-its-subsegs oldseg)
  newseg))

(defun 2imp-input-dflow (in name)
  (let* ((newseg (~seg~segcase in))
         (source (2find-var-source name newseg 'before))
         (top (top-segcase (& newseg))))
    (when (and (null source)
               (or (??& top @seg 'lambda-exp) (??& top cliche-def)))
      (setq source
            (~deep (>& (>& top input
                           nil (genobj (~seg top) name) nil)
                       @io 'free-var name nil))))
    (when source
      (implement-dflow source in)
      T)))

(defun 2find-side-effect-out (oldseg name)
  (if name
      (dolist (port (??output oldseg))
        (when (2side-effect-out port name)
          (return port)))))

(defun 2side-effect-out (port name)
  (or (??& port @io 'side-effect name)
      (let ((in (~01 (??& (&* (~seg~segcase port)) input * (~id port)))))
        (and (eq (~keyword port) 'output) (eq (~kind port) 'id) in
             (2ifv name in)))
      (let ((source (~01 ~from (??& * dflow * port))))
        (and source (2side-effect-out source name)))
      (and (eq (?segtype (~seg~segcase port)) 'join)
           (dolist (in (??& (&* (~seg~segcase port)) input
                            'id * (~obj port)))
             (when (2side-effect-out in name) (return port))))))

(defun 2splice-into-parallel-dflows (out name)
  out name nil) ;not implemented yet

(defun dummy-port? (port)
  (and (memq (~keyword port) '(tinput toutput))
       (or (null (~lower-io port))
           (dummy-port? (~lower-io port)))))

(defun 2make-dummy-toutput (seg)
  (if (not (eq (?plantype seg) 'ssr))
      (let* ((last-subseg (car (2brothers-before seg seg)))
             (sub-dummy (2make-dummy-toutput last-subseg))
             (port (>& (& seg) toutput (& last-subseg)
                       (genobj seg 'dummy) sub-dummy)))
        (>& port @io 'dummy nil nil)
        port)
      (let* ((rec (?1role seg 'rec))
             (lower-env (~from (?& * cflow * (&* rec))))
             (port (>& (& seg) toutput lower-env (genobj seg 'dummy) nil)))
        (>& port @io 'dummy nil nil)
        port)))

(defun 2make-dummy-tinput (seg)
  (if (not (eq (?plantype seg) 'ssr))
      (let* ((last-subseg
               (letS ((sub (Elist (reverse (2brothers-before seg seg)))))
                 (if (memq (?plantype sub) '(ssr tempcomp)) (done sub))))
             (sub-dummy (2make-dummy-tinput last-subseg))
             (port (>& (& seg) tinput (& last-subseg)
                       (genobj seg 'dummy) sub-dummy)))
        (>& port @io 'dummy nil nil)
        port)
      (let* ((body (?1role seg 'body))
             (lower-env (~to (?& (& body) cflow (&* body))))
             (port (>& (& seg) tinput lower-env (genobj seg 'dummy) nil)))
        (>& port @io 'dummy nil nil)
        port)))

(defun sole-used-output? (supseg seg)
  (let ((ports (Reqset (~from (Elist (??& (&* supseg) dflow
                                          (&* (&* seg) port)))))))
    (cond ((and ports (null (cdr ports))) (car ports)))))

(defun sole-fed-input? (supseg seg)
  (let ((ports (Reqset (~to (Elist (??& (&* supseg) dflow
                                        * (&* (&* seg) port)))))))
    (cond ((and ports (null (cdr ports))) (car ports)))))

(defun 2input-from-var (seg name)
  (letS ((in (Elist (??input seg))))
    (if (2ifv name in) (done in))))

(defun 2ifv (name port)
  (if port
      (let ((var (2var-for-port port)))
        (cond ((eq var name) port)
              ((not (null var)) nil)
              ((2ifv name
                     (or (~01 ~from (??& * dflow * port))
                         (~01 (??& * tinput * * port))
                         (if (eq (~keyword port) 'toutput)
                             (~lower-io port)))))))))

(defun 2var-for-port (port)
  (or (~01 ~name (??& port @io))
      (~01 ~varname (??& port @createvar))
      (and (??& port @io 'returnval)
           (??& (~segcase port) @seg 'funcall '5noop)
           (2fed-input (car (??& (~segcase port) input)))
           (~01 ~name (??& (&* (~segcase port) input)
                           @io 'actual-arg * 1)))
      (~01 ~name (??& (&* * tinput * * port) @io 'free-var))
      (~01 ~name (??& (&* * toutput * * port) @io 'free-var))))

(defun 2fed-input (port)
  (and port
       (or (??& * dflow * port)
           (2fed-input (car (??& * tinput * * port))))))

;this looks over a plan and tries to find a near port before the
;specified seg which carries dflow in the indicated variable,
;The key problem is one of determining the appropriate ordering on nearby
;segments.  The order as last coded is used as a heuristic.  The argument
;START controls where the fn starts looking from.
;note it makes a top level port if it cannot find one.
;(Note this no longer allows you to use a slot name as if it were a
;variable.  However, it preferentially tries to find an output slot
;as the source for the specified variable.

(declare (special 2nearest-hit 2slot-hit))

(defun 2find-var-source (var seg start)
  (let (2nearest-hit 2slot-hit)
    (*catch '2fvs (2find-var-source1 var seg start))
    (or 2slot-hit 2nearest-hit)))

(defun 2fvs-test (var port)
  (when (eq (2var-for-port port) var)
    (if (null 2nearest-hit) (setq 2nearest-hit port))
    (when (2comes-from-output-slot port)
      (setq 2slot-hit port)
      (*throw '2fvs nil))))

(defun 2comes-from-output-slot (port)
  (when port
    (or (??& * slot (~segcase port) * 'output)
        (2comes-from-output-slot
          (or (~01 ~from (??& * dflow * port))
              (~01 (??& * tinput * * port))
              (if (eq (~keyword port) 'toutput)
                  (~lower-io port)))))))

(defun 2find-var-source1 (var seg start)
  (caseq start
    (at-front
      (dolist (in [!(??& (&* seg) input) !(??& (&* seg) tinput)])
        (2fvs-test var in))
      (2find-var-source1 var seg 'before))
    (at-end
     (dolist (out [!(??& (&* seg) output) !(??& (&* seg) toutput)])
        (2fvs-test var out))
     (2find-var-source1 var seg 'inside))
    (before
     (let ((supseg (?supseg seg)))
       (when supseg
         (over (2brothers-before supseg seg)
           (2find-var-source1 var _ 'at-end))
         (2find-var-source1 var supseg 'at-front))))
    (inside ;this is potentially buggy if several guys produce the var
     (over (2brothers-before seg seg)
       (2find-var-source1 var _ 'at-end)))))

;;; PDF page 111

;this gives every brother of the given seg which is not specificially
;constrained to be after the given seg.
;;; returns a list in reverse order with the close ones first,
;and with all of the unordered ones at the tail.
(defun 2brothers-before (supseg seg)
 (let ((subsegs (??subseg supseg)) forced-after forced-before)
   (repeat ((to-do [seg]))
     (if (null to-do) (return nil))
     (over [!(~nset ~seg ~segcase ~to
                    (??& (&* supseg) dflow (&* (&* (top to-do)) port)))
            !(~nset ~seg ~to
                    (??& (&* supseg) cflow (&* (top to-do))))
            !(~nset ~seg~segcase ~lower-io
                    (??& (&* (pop to-do)) tinput))] ;if seg=supseg
           (cond ((and (memq _ subsegs) (not (memq _ forced-after)))
                  (push _ forced-after)
                  (push _ to-do)))))
   (repeat ((to-do [seg]))
      (if (null to-do) (return nil))
      (over [!(~nset ~seg ~segcase ~from
                     (??& (&* supseg) dflow * (&* (&* (top to-do)) port)))
             !(~nset ~seg ~from
                     (??& (&* supseg) cflow * (&* (top to-do))))
             !(~nset ~seg~segcase ~lower-io
                     (??& (&* (pop to-do)) toutput))] ;if seg=supseg
            (cond ((and (memq _ subsegs) (not (memq _ forced-before)))
                   (push _ forced-before)
                   (push _ to-do)))))
    (if (eq supseg seg)
        (if (> (length forced-before) (length forced-after))
            (nreconc forced-before
                     (reverse-order-by-dflow
                       (qset- subsegs forced-before)))
            (nconc forced-after
                   (reverse-order-by-dflow (qset- subsegs forced-after))))
        (nreconc forced-before
                 (reverse-order-by-dflow
                   (Rlist (filterS #'(lambda (x)
                                       (and (not (eq x seg))
                                            (not (memq x forced-before))
                                            (not (memq x forced-after))))
                                   (Elist subsegs))))))))
;assumes segs are all in one supseg, and no loops in flow
;returns a list of them where no seg follows a seg it gets dflow from
;;; works by repeatedly finding segs that don't get flow from anybody
(defun reverse-order-by-dflow (segs)
  (when segs
    (let (result (supseg (?supseg (car segs))))
      (repeat ((to-do (copy segs)))
        (if (null to-do) (return) )
        (repeat ((seg (car to-do)))
          (when (not
                 (over [!(~nset ~seg ~segcase ~from
                           (??& (&* supseg) dflow * (&* (&* seg) port)))
                        !(~nset ~seg ~from
                           (??& (&* supseg) cflow * (&* seg)))]
                   (when (memq _ to-do) (setq seg _) (return T))))
            (push seg result)
            (setq to-do (delq seg to-do))
            (return))))
      result)))
;this fixes things so that everything about the old port
;now applies to the new one.
;;; PDF page 112

(defun 2replace-port (oldport newport)
  (over (--& * dflow * oldport) (assert-fact (&~to _ newport)))
  (over (--& * dflow oldport) (assert-fact (&~from _ newport)))
  ;this still isn't right if lower-env not simple.
  (over [!(--& * tinput * * oldport) !(--& * toutput * * oldport)]
        (2replace-port _ (>& (~segcase _) (~keyword _) (~segcase newport)
                             (~obj _) newport))))
;this just makes a lambda-exp into an open code thing.
(defun 2top->simple (seg)
  (--& (&* seg) @seg)
  (>& (& seg) @seg 'opencode nil nil)
  (over (--& (&* (&* seg) port) @io 'lambda-arg)
    (>& (~deep _) @io 'free-var (~name _) nil))
  seg)
;;; PDF page 113

;This is expected to get called a lot, so I try real hard here to
;be able to make a dflow between any two points. A key problem is
;deciding where we are going to have temporal dflow.
;2intermediate-steps does most of the hard work.
(defun implement-dflow (*from *to)
  (if (not (and *from *to))
      (barf "IMPLEMENT-DFLOW: must have from and to5"))
  ;kill old dflow to destination
  (let* ((other-df (??& * dflow * *to)))
    (>& (~segcase *to) dflow *from *to) ;CLUDGE prevents deletion of to
    (2remove*assertions other-df)
    (--& * dflow *from *to))
  (let (interval)
    (letS (((fn arg) (Elist (2intermediate-steps *from *to))))
      (setq interval (funcall fn arg interval)))))
;This figures out what kinds of dflow has to be created in order
;to bridge the gap between these ports.  A big complexity comes
;in from needing to decide when temporal flow has to be used.
;Another problem is trying to find a good source for the dflow
;goven that we cannot in general convert a toutput into an output.
;The output is a list of steps to be performed in order.  each
;step is a function call to be evaluated.
(defun 2intermediate-steps (*from *to)
  (let ((steps (2intermediate-steps1 *from *to)))
    (cond ((listp steps) steps)
          ((eq steps 'failed-bad-non-ssr-from)
           (setq *from (2get-higher-from *from))
           (if (null *from) (barf "cannot make dflow ~:= to ~:=" *from *to))
           (2intermediate-steps *from *to))
          ((eq steps 'failed-bad-temporal-from)
           (2intermediate-steps (2get-better-from *from) *to))
          ((eq steps 'failed-bad-temporal-to)
           (2intermediate-steps *from (2get-better-to *to))))))
(defun 2get-higher-from (from)
  (let ((source (dflow-source from)))
    (if (not (equal source from)) source)))
;if *from is a port that we cannot make into a final value
;(and we need to) then this program tries to find some equivalent
;port that we can make into a final value,
(defun 2get-better-from (old-from)
  (let ((*from old-from))
    (if (repeat (did-something)
          (caseq (~keyword *from)
            (toutput (setq *from (~lower-io *from)))
            (tinput
              (let ((prior (~01 ~from (??& * dflow * *from))))
                (if (null prior) (return nil))
                (setq *from prior)))
            (input
              (if (eq (?plantype (~seg~segcase *from)) 'ssr-body)
                  (return did-something) )
              (let ((upper (or (~01 (??& * tinput * * *from))
                               (~01 ~from (??& * dflow * *from)))))
                (if (null upper) (return nil))
                (setq *from upper)))
            (output
              (if (memq (?@seg-name (~seg~segcase *from))
                        '(Svariable Snoop))
                  (setq *from (??& (&* (~seg~segcase *from)) input))
                  (return nil))))
          (setq did-something T))
        *from
;;; PDF page 114

       (force-to-final-value old-from))))
;this tries real hard to just bash something to a final value
;willy nilly. it produces a plan that isn't really right,
;but the coder makes ok code anyway.
(defun force-to-final-value (*from)
 (let ((body (?supseg (~seg~segcase *from))))
   (if (not (and (eq (~keyword *from)  'output)
                 (eq (?plantype body)  'ssr-body)))
       (barf  "2cannot coerce to final value"))
   (let* ((seg (?supseg body))
           (dfjoin (?1role seg 'dfjoin))
           (rec (?1role seg 'rec))
           (body-out (>& (& body) output nil (genobj body 'last) nil))
           (rec-out (>& (& rec) output nil
                        (genobj rec (~obj body-out)) nil))
           (dfjoin-out (>& (& dfjoin) output nil
                           (genobj dfjoin 'last) nil))
           (dfjoin-in-rec (>& (& dfjoin 'rec) input 'id
                              (genobj dfjoin 'last) (~obj dfjoin-out)))
           (dfjoin-in-body (>& (& dfjoin 'body) input 'id
                               (genobj dfjoin 'last) (~obj dfjoin-out))))
      (>& body-out @io 'free-var nil nil)
      (>& rec-out @io 'free-var nil nil)
      (>& dfjoin-out @io 'free-var nil nil)
      (>& dfjoin-in-rec @io 'free-var nil nil)
      (>& dfjoin-in-body @io 'free-var nil nil)
      (>& (& body) dflow *from dfjoin-in-body)
      (>& (& body) dflow rec-out dfjoin-in-rec)
      (>& (& body) dflow dfjoin-out body-out)
      body-out)))
;if *to is a port that we cannot make into an initial value
;(and we need to) then this program tries finds an equivalent
;port that we can make into an initial value.
(defun 2get-better-to (old-to)
  (let ((*to old-to))
    (if (repeat (did-something)
          (if (eq (~keyword *to) 'tinput)
              (setq *to (~lower-io *to))
              (return did-something) )
          (setq did-something T))
        *to
        (barf "Cannot coerce input to initial-value ~:=" old-to))))
;;; PDF page 115

;note the cludgy way we just fail when we cannot make dflow
;from a temporal port.
(defun 2intermediate-steps1 (*from *to)
  (prog outer ()
    (let* ((from-seg (~seg~segcase *from))
           (to-seg (~seg~segcase *to))
           ((lca from-ancesters to-ancesters)
            (2intermediate-segs from-seg to-seg))
           (temporal-middle
            (and (eq (?plantype lca) 'tempcomp)
                 from-ancesters
                 (or (eq (?plantype (car from-ancesters)) 'ssr)
                     (and (eq (?plantype (car from-ancesters)) 'tempcomp)
                          (not (??& * role (&* (car from-ancesters))
                                    'init))))
                 to-ancesters
                 (or (eq (?plantype (car to-ancesters)) 'ssr)
                     (and (eq (?plantype (car to-ancesters)) 'tempcomp)
                          (not (??& * role (&  (car to-ancesters))
                                    'init))))))
           (steps `((2id-horizontal ,lca))))
      (if (or (eq (~keyword *from) 'tinput)
              (if from-ancesters
                  (and (eq (~keyword *from) 'input)
                       (not (eq (?plantype from-seg) 'ssr-body)))
                  (not (eq (~keyword *from) 'input)))
              (eq (~keyword *to) 'toutput)
              (if to-ancesters
                  (eq (~keyword *to) 'output)
                  (not (eq (~keyword *to) 'output))))
          (barf "impossible dflow ~:* -> ~:=" *from *to))
      (cond (temporal-middle ;plan outgoing temporal steps
             (repeat (seg)
               (cond ((null (cdr from-ancesters)) (return nil)))
               (setq seg (pop from-ancesters))
               (caseq (?plantype seg)
                 (tempcomp (push `(2id-temporal-outgoing ,seg) steps))
                 (ssr (push `(2id-temporal-outgoing ,seg) steps)
                      (if (cdr from-ancesters)  ;if port not on ssr-body
                          (pop from-ancesters)) ;skip ssr-body
                      (return nil))  ;done with temporal stuff
                 (T (return-from outer 'failed-bad-non-ssr-from))))
             (if (and (null (cdr from-ancesters))
                      (eq (?plantype from-seg) 'ssr)
                      (not (eq (~keyword *from) 'toutput)))
                 (barf "cannot make OUTPUT of ssr be TOUTPUT"))))
      (cond ((cdr from-ancesters) ;plan outgoing non temporal steps
             (if (eq (~keyword *from) 'toutput)
                 (return-from outer 'failed-bad-temporal-from) )
             (repeat (seg)
               (cond ((null (cdr from-ancesters)) (return nil)))
               (setq seg (pop from-ancesters))
               (caseq (?plantype seg)
                 (ssr-body (return-from outer 'failed-bad-temporal-from))
                 (T (push `(2id-outgoing ,seg) steps))))
             (if (eq (?plantype from-seg) 'ssr-body)
                 (if (eq (~keyword *from) 'input)
                     (push `(2id-make-final-value ,from-seg) steps)))))
      (cond (temporal-middle ;plan ingoing temporal steps
             (repeat (seg)
               (cond ((null (cdr to-ancesters)) (return nil)))
               (setq seg (pop to-ancesters))
               (caseq (?plantype seg)
                 (tempcomp (push `(2id-temporal-ingoing ,seg) steps))
                 (ssr (push `(2id-temporal-ingoing ,seg) steps)
                      (if (cdr to-ancesters)  ;if port not on ssr-body         :
                          (pop to-ancesters)) ;skip ssr-body
;;; PDF page 116

                     (return nil))  ;done with temporal stuff
                (T (barf "cannot make input of non-ssr be toutput"))))
            (if (and (null (cdr to-ancesters))
                     (eq (?plantype to-seg) 'ssr)
                     (not (eq (~keyword *to)  'tinput)))
                 (barf "cannot make INPUT of ssr be TINPUT"))))
      (cond ((cdr to-ancesters) ;plan ingoing non temporal steps
             (if (eq (~keyword *to) 'tinput)
                 (return-from outer 'failed-bad-temporal-to))
             (repeat (seg)
               (cond ((null (cdr to-ancesters)) (return nil)))
               (setq seg (pop to-ancesters))
               (caseq (?plantype seg)
                 (ssr-body (push `(2id-make-initial-value ,seg) steps))
                 (T (push `(2id-ingoing ,seg) steps))))))
      (push `(2id-initial-interval (,*from ,*to)) steps)
      (return-from outer steps))))
;;; PDF page 117

;Finds all of the intervening segment boundaries encountered when
;going from the first seg to the second. It works by first finding
;the lowest common ancestor (it assumes that there is one).
;Note the seg itself is listed as the closest ancester of itself.
(defun 2intermediate-segs (fromseg toseg)
  (let* ((from-ancestors (2ancestors fromseg))
         (to-ancestors (2ancestors toseg))
         (lca))
    (repeat ()
      (cond ((not (eq (car from-ancestors) (car to-ancestors)))
             (return nil)))
      (setq lca (pop from-ancestors))
      (pop to-ancestors))
    (if (null lca)
        (barf "2intermediate-segs: ~S and ~S not in same plan"
              fromseg toseg))
    [lca from-ancestors to-ancestors]))
(defun 2ancestors (seg)
  (repeat (result)
    (push seg result)
    (setq seg (?supseg seg))
    (if (null seg) (return result))))
;;; PDF page 118

;this is a litte dummy step to get things started right.
(defun 2id-initial-interval (interval ignore)
  (list (car interval) (cadr interval)))
(defun 2id-outgoing (to-seg interval)
  (let (((*from *to) interval))
    (if (not (and (eq (~keyword *from) 'output)
                  (eq (?supseg (~seg~segcase *from)) to-seg)
                  (not (eq (?plantype to-seg) 'ssr-body))))
        (barf "2id-outgoing"))
    (let ((port (car (~set ~to (??& (&* to-seg) dflow *from
                                    (&* (&* to-seg) output))))))
      (when (null port)
        (setq port (>& (& to-seg) output nil
                       (genobj to-seg 'obj) nil))
        (>& port @io 'free-var '_ nil)
        (>& (& to-seg) dflow *from port))
      (list port *to))))
(defun 2id-temporal-outgoing (to-seg interval)
  (let (((*from *to) interval))
    (if (not (and (memq (?plantype to-seg) '(tempcomp ssr))
                  (or (and (memq (~keyword *from) '(input toutput))
                           (eq (?supseg (~seg~segcase *from)) to-seg))
                      (and (eq (~keyword *from) 'output)
                           (eq (?supseg (?supseg (~seg~segcase *from)))
                               to-seg)))))
        (barf "2id-temporal-outgoing"))
    (let ((port (~01 (??& (&* to-seg) toutput (~segcase *from) * *from))))
      (when (null port)
        (setq port (>& (& to-seg) toutput (~segcase *from)
                       (genobj to-seg 'obj) *from))
        (>& port @io 'free-var '_ nil))
      (list port *to))))
;[Note that the OUTPUTs on segs inside an SSR-BODY cannot be routed outside
;  as final values because we cannot guarrantee that they will exist.]
(defun 2id-make-final-value (body interval)
  (let (((*from *to) interval))
    (if (not (and (eq (~keyword *from) 'input)
                  (eq (~seg~segcase *from) body)
                  (eq (?plantype body) 'ssr-body)))
        (barf "2id-make-final-value"))
    (let* ((seg (?supseg body))
           (dfjoin (?1role seg 'dfjoin))
           (rec (?1role seg 'rec))
           (body-out (>& (& body) output nil (genobj body 'last) nil))
           (rec-out (>& (& rec) output nil
                        (genobj rec (~obj body-out)) nil))
           (dfjoin-out (>& (& dfjoin) output nil
                           (genobj dfjoin 'last) nil))
           (dfjoin-in-rec (>& (& dfjoin 'rec) input 'id
                              (genobj dfjoin 'last) (~obj dfjoin-out)))
           (dfjoin-in-body (>& (& dfjoin 'body) input 'id
                               (genobj dfjoin 'last) (~obj dfjoin-out))))
      (>& body-out @io  'free-var nil nil)
      (>& rec-out @io  'free-var nil nil)
    (>& dfjoin-out @io  'free-var nil nil)
      (>& dfjoin-in-rec @io  'free-var nil nil)
       (>& dfjoin-in-body @io 'free-var nil nil)
       (>& (& body) dflow *from dfjoin-in-body)
       (>& (& body) dflow rec-out dfjoin-in-rec)
       (>& (& body) dflow dfjoin-out body-out)
       (list body-out *to))))
;;; PDF page 119

(defun 2id-ingoing (from-seg interval)
 (let (((*from *to) interval))
   (if (not (and (eq (~keyword *to) 'input)
    (eq (?supseg (~seg~segcase *to)) from-seg)
                 (not (eq (?plantype from-seg) 'ssr-body))))
       (barf "2id-ingoing"))
   (let ((port (car (~set ~from (??& (&* from-seg) dflow
                                      (&* (&* from-seg) input) *to)))))
      (when (null port)
        (setq port (>& (& from-seg) input nil
                       (genobj from-seg 'obj) nil))
        (>& port @io 'free-var '_ nil)
        (>& (& from-seg) dflow port *to))
      (list *from port))))
(defun 2id-temporal-ingoing (from-seg interval)
  (let (((*from *to) interval))
    (if (not (and (memq (?plantype from-seg) '(tempcomp ssr))
                  (or (and (eq (~keyword *to) 'tinput)
    (eq (?supseg (~seg~segcase *to)) from-seg))
                      (and (eq (~keyword *to) 'input)
                           (eq (?supseg (?supseg (~Seg~segcase *to)))
                               from-seg)))))
        (barf "2id-temporal-ingoing"))
    (let ((port (~01 (??& (&* from-seg) tinput (~segcase *to) * *to))))
      (when (null port)
        (setq port (>& (& from-seg) tinput (~segcase *to)
                       (genobj from-seg 'obj) *to))
        (>& port @io *free-var '_ nil))
      (list *from port))))
;[Note we do not have the kind of problem we had with final-value.]
(defun 2id-make-initial-value (body interval)
  (let (((*from *to) interval))
    (if (not (and (eq (~keyword *to) 'input)
                  (eq (?supseg (~seg~segcase *to)) body)
                  (eq (?plantype body) 'ssr-body)))
        (barf "2id-make-initial-value"))
    (let* ((seg (?supseg body))
           (rec (?1role seg 'rec))
           (body-in (>& (& body) input nil (genobj body 'init) nil))
           (rec-in (>& (& rec) input nil
                       (genobj rec (~obj body-in)) nil)))
      (>& body-in @io 'free-var nil nil)
      (>& rec-in @io 'free-var nil nil)
      (>& (& body) dflow body-in *to)
      (>& (& body) dflow body-in rec-in)
      (list *from body-in))))
(defun 2id-horizontal (lca interval)
  (let (((*from *to) interval))
    (>& (& lca) dflow *from *to)
    nil))
;;; PDF page 120

;something should be added here so that if you remove a seg that is
 ;a slot (or contains a slot or has a brother that is a slot) then the
;cliche instance with these slots gets erased out of the plan since it
;isn't accurate any more.
(defun 2remove-seg (thing)
 (when (not (and (??& thing segtype  'simple)
                  (memq (~type (??& thing plantype))
                        '(EXPR AND COMP SSR TEMPCOMP CONSTANT NEW-VIEW))
                  (memq (~kind (?& thing @seg))
                        '(OPENCODE FUNCALL CONSTANT))))
    (barf "2REMOVE-SEG: Cannot remove seg.~% Thing: ~:=" thing))
  (letS ((se (Elist (??& (&* thing output) @io 'side-effect)))
      (out    (~deep se))
         (in (?& thing input * (~id out)))
         (in-dflow (-& * dflow * in))
         (out-dflow (-& * dflow out)))
    (>& (~segcase in-dflow) dflow (~from in-dflow) (~to out-dflow)))
  (2remove*assertions (??& * dflow * (&* (&* (~seg thing)) port)))
  (2remove*assertions (??& * dflow (&* (&* (~seg thing)) port)))
  (let ((cflow (~01 (--& * cflow * thing))))
    (if cflow (assert-fact (&~to cflow (~to (-& * cflow thing))))))
  (kill-seg-and-its-subsegs (~seg thing)))
;;; This actually removes assertions from the database.
(defun 2remove*assertions (things)
  (dolist (thing things)
    (cond ((not (fact? thing))
           (2remove-seg     thing))
          (T (caseq (~keyword thing)
               (dflow (2remove-dflow thing))
               (input   (2remove-input   thing))
               (output  (2remove-output  thing))
               (tinput  (2remove-tinput  thing))
               (toutput (2remove-toutput thing))
               (T (barf "2REMOVE*ASSERTIONS: cannot remove~% Thing: ~:="
                  thing)))))))
(defun 2remove-dflow (thing)
  (erase-facts thing)
  (if (and (find-facts (~to thing))
           (null (??& * dflow * (~to thing)))
           (null (??& * tinput * * (~to thing))))
      (cond ((and (input-fact? (~to thing))
                  (memq (?plantype (~seg~segcase (~to thing)))
                        '(EXPR AND COMP TEMPCOMP PRED XOR)))
             (2remove*assertions [(~to thing)]))))
  (if (and (find-facts (~from thing))
           (null (??& * dflow (~from thing)))
           (null (??& * toutput * * (~from thing))))
      (cond ((and (input-fact? (~from thing))
                  (memq (?plantype (~seg~segcase (~from thing)))
                         '(EXPR AND COMP TEMPCOMP PRED XOR))
                  (null (??& (&* (~seg~segcase (~from thing)))
                             @seg 'lambda-exp)))
             (2remove*assertions [(~from thing)])))))
(defun 2remove-toutput (thing)
  (--& thing @io)
  (erase-facts thing)
  (2remove*assertions (??& * toutput * * thing))
  (2remove*assertions (??& * dflow thing)))
(defun 2remove-tinput  (thing)
   (--& thing @io)
   (erase-facts thing)
   (2remove*assertions (??& * dflow * thing))
;;; PDF page 121

  (2remove*assertions (??& * toutput * * thing))
  (2remove*assertions (??& * tinput * * thing))
  (if (eq (?@seg-kind (~seg~segcase (~lower-io thing))) 'opencode)
      (2remove*assertions [(~lower-io thing)])))
;;; PDF page 122

(defun 2remove-input (thing)
  (let* ((segcase (~segcase thing))
         (other-ports (cond ((~id thing)
    (??& segcase output * (~id thing)))
                            (T
                             (??& segcase output 'id * (~obj thing))))))
    (let ((@io (~01 (??& thing @io))))
      (if (and @io (memq (~kind @io) '(lambda-arg actual-arg)))
          (dolist (other (??& (&* segcase input) @io 'lambda-arg))
            (when (> (~argno other) (~argno @io))
              (assert-fact (&~argno (erase-fact other)
                                    (1- (~argno other))))))))
    (2remove*assertions (??& * dflow * thing))
    (2remove*assertions (??& * dflow thing))
    (2remove*assertions (??& * toutput * * thing))
    (2remove*assertions (??& * tinput * * thing))
    (--& thing @io)
    (--& thing @createvar)
    (erase-facts thing) ;note this must be first to avoid looping!
    (let ((rec (~01 ~subseg (??& segcase recursive) ))
          (sup (~01 ~segcase (??& * recursive segcase))))
      (if rec (setf other-ports
                    (append (??& rec input * (~obj thing)) other-ports)))
      (if sup (setf other-ports
                    (append (??& sup input * (~obj thing)) other-ports))))
    (2remove*assertions other-ports)))
(defun 2remove-output (thing)
  (let* ((segcase (~segcase thing))
         (other-ports (cond ((~id thing) (??& segcase input * (~id thing)))
                            (T (??& segcase input  'id * (~obj thing))))))
    (2remove*assertions (??& * dflow * thing))
    (2remove*assertions (??& * dflow thing))
    (2remove*assertions (??& * toutput * * thing) )
    (2remove*assertions (??& * tinput * * thing))
    (--& thing @io)
    (--& thing @createvar)
    (erase-facts thing) ;note this must be first to avoid looping
    (let ((rec (~01 ~subseg (??& segcase recursive) ))
          (sup (~01 ~segcase (??& * recursive segcase))))
      (if rec (setq other-ports
                    (append (??& rec output *  (~obj thing)) other-ports)))
      (if sup (setq other-ports
                    (append (??& sup output *  (~obj thing)) other-ports))))
    (2remove*assertions other-ports)))
;;; PDF page 123
;;; Local Modes:;
;;; Mode: LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp REPEAT Indent:1;
;;; Lisp REPEAT* Indent:1;
;;; End:;
