;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 121-140; listing macro.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; MACRO: This module contains general lisp utility macros for use
;;;        by any other module in the KBE system except BASIC.
;;;
;;;        This module also contains some support functions needed at
;;;        runtime by the macros it defines.

;;;; Control Structure

;;; (REPEAT let-bvl body)
;;;
;;;  REPEAT is an infinite loop. It takes optional variable bindings
;;;  a la LET. It does not allow assignment specs for the second pass
;;;  through; for that, use DO.
;;;

(defmacro repeat (bvl &body body)
  ;; Interim error check. Can go away after next full system recompilation
  (dolist (statement body)
    (if (atom statement)
	(ferror "GO tags not allowed in REPEAT: ~S" statement)))
  `(let ,bvl (do () (nil) (progn ,@body))))

;;; (REPEAT* let-bvl body)
;;;
;;;  This is the same as REPEAT, but uses destructuring LET*, not LET.
;;;

(defmacro repeat* (bvl &body body)
  ;; Interim error check. Can go away after next full system recompilation
  (dolist (statement body)
    (if (atom statement)
	(ferror "GO tags not allowed in REPEAT*: ~S" statement)))
  `(let* ,bvl (do () (nil) (progn ,@body))))


;;; (RETURN-IF x)      Returns x iff x is non-null.
;;; (RETURN-IF-NOT x)  Returns x iff x is null.

(defmacro return-if (x)
  (once-only (x) `(if ,x (return ,x))))

(defmacro return-if-not (x)
  `(return-if (not ,x)))

;;;; Binding

(defmacro with-defaults (vars-and-locs &body forms)
  `(let ,vars-and-locs
     (unwind-protect (progn ,@forms)
		     ,@(mapcar #'(lambda (x) `(setf ,(cadr x) ,(car x)))
			       vars-and-locs))))

;;;; Lisp data accessors

;;; These are intended only for very low-level use in specifying data
;;; storage layouts. Not for normal user code.
;;;
;;; Note: These should rightly be funcallable as well, but there's little
;;;       point. SUBSTs currently don't compile right, so better we should
;;;       get an error if they are funcalled since currently we don't
;;;       intentionally funcall them anyway...

(defmacro cr (x) x)
(defmacro cdddddr (x) `(cdr (cddddr ,x)))

;;;; Control Structure

;;; These macros are not being phased out and are acceptable for
;;; writing into new code.

;;; (WHEN-ABNORMAL-EXIT exp form1 form2 form3 ...)
;;;
;;;  Executes exp, returning its value.
;;;  If an abnormal exit is done from exp, form1, form2, ... are executed.

(defmacro when-abnormal-exit (exp &body abnormal-exit-forms)
  (let ((var (gensym)))
    `(let ((,var t))
       (unwind-protect (values-list
			 (prog1 (multiple-value-list ,exp)
				(setq ,var nil)))
		       (cond (,var ,@abnormal-exit-forms))))))

;;; (WHEN-ERROR exp form1 form2 ...)
;;;
;;; If exp doesn't err, returns the value it returns.
;;;
;;; If exp errs, traps the error and executes forms.
;;;    In this case, the value of the last form is returned.

(defvar *when-error-debug* nil)

(defmacro when-error (exp &body forms)
  `(condition-case-if (not *when-error-debug*) ()
       ,exp
     (sys:error ,@forms)))

;;; (CONDITION-RESTART-CASE-IF condition form
;;;   ((flavor1 format-string1 . format-args1) . action1)
;;;   ((flavor2 format-string2 . format-args2) . action2)
;;;   ...)
;;;
;;; Evaluates CONDITION, then executes FORM.
;;; If the condition evaluated to something true, the FORM is
;;; evaluated in a context where restart handlers are in effect
;;; according to the clauses (first clause is innermost restart
;;; handler). The action for each clause is taken IFF that restart
;;; handler is selected.

(defmacro condition-restart-case-if (cond form &body clauses)
  (cond ((null clauses) form)
	(t
	 (let ((tag (gensym)))
	   `(condition-restart-case-if ,cond
		(prog ,tag ()
		      (declare (si:invisible-block t))
		      (catch-error-restart-if ,cond
					      ,(caar clauses)
			(return-from ,tag ,form))
		      (return-from ,tag (progn ,@(cdar clauses))))
	      ,@(cdr clauses))))))

(defmacro condition-restart-case (form &body clauses)
  `(condition-restart-case-if t ,form ,@clauses))

;;;; File-I/O

;;; (FILE-DEFAULT-FN2 filename type version)
;;;
;;;  Does filename defaulting for the fn2 and/or version number.
;;;  This works in a reasonably file-system independent way.
;;;
;;;  Eg, something like this should happen:
;;;
;;;   (FILE-DEFAULT-FN2 "foo;bar" 'PLAN ':newest)
;;;       => #<ITS-PATHNAME "FOO;BAR PLAN">
;;;
;;;   (FILE-DEFAULT-FN2 "ee:<kmp>foo" 'PLAN ':oldest)
;;;       => #<TOPS20-PATHNAME "EE:PS:<KMP>FOO.PLAN.-2">

(defmacro file-default-fn2 (filename &optional (type    '':unspecific)
						 (version '':unspecific))
  `(fs:merge-pathname-defaults (fs:parse-pathname ,filename)
			       NIL ;no full pathname default
			       ,type
			       ,version))

;;;; List/String Handling

;;; (COLLECT (var list) predicate-expression)
;;;
;;;  var should be a symbol. list is a list to iterate across.
;;;  on each pass of the iteration, predicate-expression is evaluated
;;;  in a context where var is bound to a new element of the list.
;;;  collects and returns a list of all values of var for which
;;;  predicate-expression returns true.

(defmacro collect ((var list) predicate)
  (let ((lvar (gensym)) (rvar (gensym)))
    `(do-named t ;makes us invisible to any RETURN or GO in the predicate
	       ((,lvar ,list (cdr ,lvar))
		(,rvar nil)
		(,var))
	       ((null ,lvar) ,rvar)
       (setq ,var (car ,lvar))
       (if ,predicate (push ,var ,rvar)))))

;;;; Message Passing

;;; (MY symbol)
;;;
;;;  Same as (SEND SELF ':symbol).

(defmacro my (keyword)
  `(send self ',(intern keyword "")))

;;;; Stacks

;;; These are useful functions to use along with PUSH and POP in
;;; dealing with lists as stacks.

;;; (WHEN-POP-FAILS stack . actions-if-empty)
;;;
;;;  If stack is a list, returns the top of the stack (popping that
;;;  element at the same time).
;;;
;;;  If stack is an atom or is empty, runs actions-if-empty instead.

(defmacro when-pop-fails (stack &body actions-if-empty)
  (cond (actions-if-empty
	 `(cond ((atom ,stack) ,@actions-if-empty)
		(t (pop ,stack))))
	(t `(pop ,stack))))

;this is just another mnemonic for CAR except that it will also optionally
;check whether the stack is a valid stack.

(defmacro top (stack . actions-if-empty)
  (cond (actions-if-empty
	 `(cond ((atom ,stack) ,@actions-if-empty)
		(t (car ,stack))))
	(t `(car ,stack))))

;;;; SETF-related extensions

;;; (<- loc expression loc expression ...)
;;;
;;;  Like a multi-arg version of SETF.

(defmacro <- (&rest body)
  (do ((code nil) (l body (cddr l)))
      ((null l)
       (cond ((null (cdr code)) (car code))
             (t `(progn ,@(nreverse code)))))
    (if (null (cdr l))
        (ferror "Odd number of arguments to <-~% ~S" `(<- ,@body)))
    (push `(setf ,(car l) ,(cadr l)) code)))

;;; (DEFDEFMACRO defining-name (name bvl . body) . forms)
;;;
;;; This is like DEFMACRO but is specially designed for macros that
;;; define other macros. It sets up appropriate declarations so that
;;; ZWEI can later find definitions of things which were the result
;;; of an expansion of that macro.

(defmacro defdefmacro (defining-name
                       defining-type
                       (name-var bvl-var . body-var)
                       &body forms)
  (let ((namespec-var (gensym)))
    (make-defdefmacro-source-file-wrapper
      'defdefmacro
      defining-name
      `(local-declare ()
         ;(defprop ,defining-name defun zwei:definition-function-spec-type)
         (defprop ,defining-name ,defining-type si:definition-type-name)
         (defmacro ,defining-name (,namespec-var ,bvl-var &body ,body-var)
           (let ((,name-var ,namespec-var))
             (make-defdefmacro-source-file-wrapper ',defining-name
                                                   ,namespec-var
                                                   (progn ,@forms))))))))

(defprop defdefmacro defun zwei:definition-function-spec-type)

;;; (DEFINE-SETF-HANDLER name (form-pattern value) . body)
;;;
;;;  This is an extended version of DEFSETF which also works when
;;;  you want to get your hands on the whole input form, or some tail
;;;  of it. It was mostly necessary in Maclisp where DEFSETF is a pain.
;;;  On the LispM, it's pretty straightforward...

(defmacro define-setf-handler (name (form value) &body body)
  (let ((temp (gensym)))
    `(eval-when (eval load compile) ;This EVAL-WHEN is losing because
                                    ; it side-effects the environment
                                    ; global to the compiler, but we
                                    ; have to have it because otherwise
                                    ; the SETF property won't be defined
                                    ; in time for our compiled macros to
                                    ; use it...
                ;; These convolutions get around warnings about redefining
                ;; existing SETF handlers if there are any...
                (defun (,name :nsetf) (,temp ,value)
                  (let ((,form ,temp)) ,@body))
                (cond ((and (get ',name ':setf) ;warn only first time...
                            (not (get ',name ':nsetf)))
                       (grumble
                         "~%;Replacing system SETF property for ~S ~
                              with an upward compatible definition.~
                         ~%; The old version is saved on the ~
                              :OSETF property."
                         ',name)
                       (putprop ',name (get ',name ':setf) ':osetf)))
                (putprop ',name
                         (get ',name ':nsetf)
                         ':setf))))

;;; This makes it possible to assign to a [] expression
;;; The basic idea is to define SETFs for
;;; the basic list constructors CONS NCONS LIST LIST*.
;;; This will then work to destructure something if you say
;;; (SETF (LIST* A B) X) or (SETF `(,A ,@B) X) or (SETF [A | B] X)
;;; note that there is no particularly good thing to do with
;;; append, since it is ambiguous.

;;; (IGNORABLE-BY-SETF? obj)
;;;
;;;  Returns T if obj is something our destructuring SETF extensions
;;;  should not bother trying to construct an assignment for. This
;;;  spans things like (FUNCTION ...), (QUOTE ...), T, NIL, numbers, etc.

(defun ignorable-by-setf? (obj)
  (cond ((listp obj)   (memq (car obj) '(quote function)))
        ((symbolp obj) (memq obj '(t nil)))
        (t t)))

;;; (SETF (CONS a b) cons)
;;;
;;;  Assigns a the (CAR cons) and b the (CDR cons).

(define-setf-handler cons ((nil a b) v)
  (let* ((var (gensym)))
    `(let ((,var ,v))
       ,@(cond ((not (ignorable-by-setf? a)) `((setf ,a (car ,var)))))
       ,@(cond ((not (ignorable-by-setf? b)) `((setf ,b (cdr ,var))))))))

;;; (SETF (NCONS a) cons)
;;;
;;;   Assigns a the (CAR cons).
;;;
;;;   Presumably the car is NIL, but no check is actually made.

(define-setf-handler ncons ((nil a) v)
  (cond ((not (ignorable-by-setf? a)) `(setf ,a (car ,v)))))

;;; (SETF (LIST x y z ..) list)
;;;
;;;  Assigns x, y, z, ... to successive cars of list.

(define-setf-handler list ((nil . args) v)
  (let* ((var (gensym)))
    (do ((tos args (cdr tos)) (step `(setq ,var (cdr ,var))) (result))
        ((null tos) `(let ((,var ,v)) ,@(nreverse result)))
        (cond ((not (eq tos args)) (push step result)))
        (cond ((not (ignorable-by-setf? (car tos)))
               (push `(setf ,(car tos) (car ,var)) result))))))

;;; (SETF (LIST* x y z ... w) list)
;;;
;;;  Like (SETF (LIST x y z ... w) list) except w receives the remaining
;;;  tail, not the car of the tail.

(define-setf-handler list* ((nil . args) v)
  (let* ((var (gensym)))
    (do ((tos args (cdr tos)) (step `(setq ,var (cdr ,var))) (result))
        ((null tos) `(let ((,var ,v)) ,@(nreverse result)))
      (cond ((not (eq tos args)) (push step result)))
      (cond ((ignorable-by-setf? (car tos)))
            ((cdr tos) (push `(setf ,(car tos) (car ,var)) result))
            (t  (push `(setf ,(car tos) ,var) result))))))

;;;; Array abstractions

;;; (ARRAYDEFS (name type) (name type) ...)
;;; or
;;; (ARRAYDEFS name name name ...)
;;;
;;;  This takes in a list of atom type pairs and sets things up to access
;;;  the atoms as arrays. The type info was for Maclisp and is now
;;;  obsolete.
;;;
;;;  It makes macros, so that the atom name can be used to refer to the
;;;  array, even though the array is the value of the atom and not the
;;;  array property of the atom.  The fact that it is the value makes
;;;  parameter passing easier.
;;;
;;;  Note: ARRAYDEFS does NOT allocate any arrays or initialize the atoms
;;;        in any way.  You must do that elsewhere.

(defmacro arraydefs (&rest pairs)
  `(progn 'compile
          ,@(mapcar #'(lambda (form)
                        (let ((name (cond ((symbolp form) form)
                                          (t (car form)))))
                          `(defmacro ,name (&rest args)
                             `(aref ,',name ,@args))))
                    pairs)))

;;;; ALIST utility macros

;;; The following macros implement simple actions on a new data type the
;;; array with non-numeric indices.  This is actually implemented as an
;;; alist.  The intention is that it should be possible to write an
;;; expression just as if you were using an array i.e. (foo 'bar) and get
;;; the element in foo associated with 'bar.  The main macros are
;;; DEF-ALIST and DEF-ALISTQ which are like ARRAYDEFS above.  They generate
;;; a set of macros which can be used to fetch and store in an alist.
;;; you must also bind the name of the alist as some kind of variable and
;;; initialize it.  NIL is the empty alist and is an appropriate
;;; initialization in general.
;;;
;;; It is important to note that these macros work by side-effecting
;;; the alist (sometimes by a setq to the atom holding the alist).
;;; This is why they must be macros, and forces some care in their use.
;;; Particularly, if an alist is passed to a subroutine, then
;;; some side effects will reflect back to the caller and some won't,
;;; unless the alist name is a special variable which is used as the name
;;; of the alist in both the caller and called.
;;;
;;; More specifically two basic operations are supported by the
;;; macros produced by a call (DEF-ALIST[Q] FOO).  First an individual
;;; element of the alist (which is stored as the value of the atom FOO)
;;; can be referrenced as (FOO BAR).  this always returns a value no
;;; matter whether or not the alist actually has an explicit entry for
;;; BAR.  NIL is returned as the implicit value for all elements not in
;;; the alist.  there is thus no way to tell if an element is explicitly
;;; present unless you know that NIL is not a possible explicit value.
;;; (you can use ASSOC/ASSQ if you really want to know this.  the
;;; intention of this package is that an alist is an array-like object
;;; that implicitly has an infinite number of entries.)  Second, the
;;; form (setf (FOO BAR) BAZ) can be used to assign a value to an element
;;; of the alist.  this creates an explicit element of the alist if one
;;; does not already exist (to do this it does a setq to foo so beware).

;;; This is the defining form.  It must be used at the top level since it
;;; produces defprops that act like defuns.

(defmacro def-def-alist (name accessor)
  `(defmacro ,name (atom)
     (let ((temp (gensym 'g)))
       `(progn 'compile
               (defmacro ,atom (key)
                 `(cdr (,',',accessor ,key ,',atom)))
               (define-setf-handler ,atom ((nil key) v)
                 `(let ((,',temp ,key))
                    (rplacd (or (,',',accessor ,',temp ,',atom)
                                (car (setq ,',atom (cons (ncons ,',temp)
                                                         ,',atom))))
                            ,v)))
               ',atom))))

;;; This defines our def-alist guys...

(def-def-alist def-alist  assoc)
(def-def-alist def-alistq assq )

;;; (MEMOIZE var key . body)
;;;
;;;  This macro is usually wrapped around a function and used to memoize
;;;  its results in order to save recomputation.  It could also be used
;;;  when recomputation was undesirable for any reason.  VAR must be a
;;;  variable bound outside the scope of a set of calls on memoize.
;;;  memoize first looks (with assoc) on VAR to see if a result is
;;;  recorded under KEY.  If there is it is returned.  If not, BODY is
;;;  computed and the result stored under KEY on VAR.  This is obviously
;;;  related to the alist fns above

(defmacro memoize (var key &body original-body)
  (let ((body (cond ((cdr original-body)
                     `(progn ,@original-body))
                    (t (car original-body)))))
    `(cdr (or (assoc ,key ,var)
              (top (push (cons ,key ,body) ,var))))))

;;; (MEMOIZEQ var key . body)
;;;
;;;  The same as MEMOIZE except that it uses ASSQ

(defmacro memoizeq (var key &body original-body)
  (let ((body (cond ((cdr original-body) `(progn ,@original-body))
                    (t (car original-body)))))
    `(cdr (or (assq ,key ,var)
              (top (push (cons ,key ,body) ,var))))))

;;;; Process locking utility macros

;;; (WITH-LOCK (loc . options) . body)
;;;
;;;  Waits for the location given by lock-id to become nil, then siezes
;;;  the lock itself and runs body.

(defmacro with-lock ((loc . options) &body body)
  (let ((var  (gensym))
        (opts (locf options)))
    (let ((whostate (or (get opts ':whostate) "Lock")))
      `(let ((,var (locf ,loc)))
         (unwind-protect (progn (process-lock ,var nil ,whostate)
                                ,@body)
                         (process-unlock ,var nil nil))))))

;;; (DEFUN-LOCKED fn bvl . body)
;;;
;;;  This is like (DEFUN fn bvl . body) except that the function fn
;;;  is not re-entrant. If you try to invoke fn recursively or
;;;  simultaneously from some other process, it will block waiting
;;;  for the first invocation to terminate before starting a second.

(defmacro defun-locked (namespec bvl &body body)
  (let ((var (intern (format nil "*FUNCTION-~A-LOCK*" namespec)
                     (cond ((symbolp namespec) (symbol-package namespec))
                           (t package)))))
    `(local-declare ()
       (defvar ,var nil
         ,(format nil "Synchronization lock for ~S" namespec))
       (defun ,namespec ,bvl
         (with-lock (,var :whostate
                     ,(format nil "~A"
                              (cond ((symbolp namespec)
                                     (string-capitalize-words namespec))
                                    (t namespec))))
           ,@body)))))

;;;; Noises

(defmacro define-noise (name &rest args-info)
  `(init-noise-for ',name ,@(mapcar #'(lambda (x) `',x) args-info)))
