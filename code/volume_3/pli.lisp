;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: PLI.LISP; PDF pages 215-226.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; PLI: Support for letting the apprentice hack PL/1.

;this lets you print out lisp as PLI.  It works by temporarily
;changing a lot of gprint formats.

;;; This fixes everything up to print out lisp in PLI.
;;; This page shows some samples of how things should print.

(comment (setq test '
 (defun sqrt (num &aux result)
   (prog ()
         (setq result 1)
      LP   (cond ((< (abs (- (* result result) num)) 0.0001)
                  (return result)))
         (setq result (// (+ (// num result) result) 2))
         (go LP))) ))

;SQRT: PROCEDURE(NUM) RETURNS (FLOAT);
;        DCL NUM FLOAT,
;            RESULT FLOAT;
;        RESULT = 1;
;    LP: IF ABS(RESULT * RESULT - NUM) < 0.0001 THEN RETURN (RESULT);
;        RESULT = (NUM / RESULT + RESULT) / 2;
;        GOTO LP;
;      END SQRT;

(comment (setq test '
 (defun delete (symbol &aux current prev)
   (prog ()
         (setq prev (hash symbol))
         (setq current (cdr prev))
      LP   (cond ((not current) (return nil)))
         (cond ((eq (car current) symbol)
                (rplacd prev (cddr prev))
                (return nil)))
         (setq prev current)
         (setq current (cdr current))
         (go LP))) ))

;DELETE: PROCEDURE(SYMBOL);
;          DCL SYMBOL ...,
;              CURRENT POINTER,
;              PREV POINTER,
;              HASH EXTERNAL ENTRY (...) RETURNS (...),
;              NODE_PTR POINTER,
;              1 NODE BASED (NODE_PTR),
;                2 VALUE ...,
;                2 NEXT POINTER;
;          PREV = HASH(SYMBOL);
;          CURRENT = PREV->NEXT;
;      LP: IF CURRENT=NULL() THEN RETURN;
;          IF CURRENT->VALUE = SYMBOL
;             THEN DO; PREV->NEXT = PREV->NEXT->NEXT; RETURN; END;
;          PREV = CURRENT;
;          CURRENT = CURRENT->NEXT;
;          GOTO LP;
;        END DELETE;

(defvar *printing-as-pli* nil "controls printing format")
(defvar *saved-fn-gformat* ':Gfn-format)

(defun print-as-pli (&rest gprint-args)
  (setq *saved-fn-gformat* gfn-format)
  (let ((base 10.) (*nopoint t)
        (gfn-format 'pli-fn-format)
        (gsymbol-car-format 'pli-fn-format)
        (gspecial-formatters
          (list* 'pli-test-nil 'pli-test-T gspecial-formatters))
        (*printing-as-pli* T))
    (unwind-protect
      (progn (pli-setup) (lexpr-funcall #'gprint1 gprint-args))
      (pli-unsetup))))

(defmacro def-pli-gformat (name args &body rest)
  `(progn 'compile
     (pli-prepare-for-setup ',name)
     (defun (,name pli-gformat) ,args
       (cond (*printing-as-pli* ., rest)
             (T (funcall (or (get ',name 'saved-gformat)
                             *saved-fn-gformat*)
                         ,(car args)))))))

(defvar *pli-gformat-atoms* nil
  "list of all atoms with special pli formats")

(defun pli-prepare-for-setup (name)
  (when (not (memq name *pli-gformat-atoms*))
    (push name *pli-gformat-atoms*)))

(defun pli-setup ()
  (dolist (symbol *pli-gformat-atoms*)
    (let ((old-gformat (get symbol ':gformat))
          (pli-gformat (get symbol 'pli-gformat)))
      (when (not (eq old-gformat pli-gformat))
        (putprop symbol old-gformat 'saved-gformat)
        (putprop symbol pli-gformat ':gformat)))))

(defun pli-unsetup ()
  (setq gfn-format *saved-fn-gformat*)
  (setq gspecial-formatters (delq 'pli-test-nil gspecial-formatters))
  (setq gspecial-formatters (delq 'pli-test-T gspecial-formatters))
  (setq  *printing-as-pli* nil)
  (dolist (symbol *pli-gformat-atoms*)
    (let ((saved-gformat (getl symbol '(saved-gformat))))
      (when saved-gformat
        (if (cadr saved-gformat)
            (putprop symbol (cadr saved-gformat) ':gformat)
            (remprop symbol ':gformat))))))

;This file has formats which cause gprint to print lisp out as
;PLI.  It only maps from a subset of lisp into a subset of PLI.
;It is only intended to support the simple demo, but there is no reason
;it couldn't be extended to cover everything.

; There is a fundamental problem with data types.  I will just punt
;that for now.  This leads to problems with fns that have different
;names for different data types in PLI but not in Lisp (eg NOT).
; Another problem is that everything can be nested in everything in
;Lisp, but not in PLI.  The coder has to fix this problem because
;additional variables have to be introduced.  I will assume here that
;this doesn't happen (eg no CONDs or PROGs embedded in expressions).
; also there are some Lips fns that just don't have very good PLI
;counterparts eg PSETQ and MULTIPLE-VALUE.  I will just do kludges
;for these things.
; note that there is no garbage collector in PLI and there is no way
;I can think of to get around the fact that the plans we have just leave
;out the computation needed to decide what to free.  (note delete above.)
; note that PLI identifiers cannot contain "-" signs.  I will translate
;them to "_".
; You cannot have any literal list data in a PLI program.  In fact any
;necessary use of QUOTE is liable to be big trouble.
; Note that some of the problem with the fact that there are lots of lisp
;programs that this won't work on is ameliorated by the fact that the coder
;produces a very restricted class of programs, and that this class is
;particularly amenable to translation to PLI.

(defvar pli-bind 0  "the binding strength of the parent operation")
(defvar pli-statement T "T iff at statement level")
(defvar pli-subroutine nil "controls printing of returns")

;;; This looks at a defun and sets things up to PLI print it right.
;;;
;;; Note: This doesn't do a very good job of getting the cursor positions
;;;       set up.

(def-pli-gformat defun (list)
  (let* (((_ name arglist . body) list)
         args
         (vars (do ((l arglist (cdr l)))
                   ((or (eq (car l) '&aux) (null l))
                    (setq args (reverse args))
                    (cdr l))
                 (push (car l) args)))
         (bod body)
         (pli-subroutine (pli-is-subroutine bod))
         ((ret-type list-type . decls)
          (pli-guess-decls (append args vars) bod))
         (pli-bind 0)
         (pli-statement T))
    (GF "*': '{0{2'PROCEDURE'(<*','_>)" name args)
    (cond ((not pli-subroutine) (GF "_'RETURNS ('C')'" ret-type)))
    (GF "';'")
    (cond (decls
           (GF "A{4'DCL '[<[*<_C>]','A>]" decls)
           (cond (list-type
                  (GF "','A'NODE_PTR POINTER,'
                         A{2'1 NODE BASED (NODE_PTR),'
                               A'2 VALUE 'C','
                               A'2 NEXT POINTER'}" list-type)))
           (GF "';'}")))
    (cond ((and (null (cdr body))
                (eq (caar body) 'prog))
           (pli-top-prog (car body)))
          ((and (null (cdr body))
                (eq (caar body) 'loop))
           (pli-top-loop (car body)))
          (T (pli-defun-body body)))
    (GF "}A'END '*';'}" name)))

;;; This looks at a fn body and guesses that it is a subroutine if:
;;;  (1) it is a PROG
;;;  (2) every return is a (RETURN NIL).

(defun pli-is-subroutine (list)
  (and (eq (caar list) 'PROG)
       (null (cdr list))
       (pli-is-s1 list)))

(defun pli-is-s1 (list)
  (cond ((atom list) T)
        ((eq (car list) 'RETURN) (eq (cadr list) nil))
        (T (do ((l list (cdr l)) (val nil))
               ((atom l) val)
             (setq val (pli-is-s1 (car l)))
             (if (not val) (return nil))))))

;;; This looks over a program body and tries to estimate what the data
;;; types of things are based on what functions things are used in.
;;; It assumes that all guesses will be consistant.  It also determines
;;; the right info for any free variables.

;;; THIS IS VERY BUGGY AT THE MOMENT, IT WORKS FOR THE EXAMPLES BUT
;;; PROBABLY NOT MUCH ELSE. --Dick

(declare (special pli-decls pli-ret-type pli-list-type))

(defun pli-guess-decls (vars list)
  (let ((pli-decls (mapcar #'(lambda (x) (list x '|...|)) vars))
        (pli-ret-type '|...|)
        pli-list-type)
    (mapc #'pli-gd list)
    (list* pli-ret-type pli-list-type pli-decls)))

(defun pli-gd (list)
  (cond ((atom list))
        ((memq (car list) '(quote go kbe-#-comment-macro)))
        ((or (not (atom (car list)))
             (memq (car list) '(prog loop)))
         (mapc #'pli-gd list))
        ((eq (car list) 'cond)
         (mapc #'(lambda (l) (mapc #'pli-gd (cdr l))) (cdr list)))
        ((eq (car list) 'return) ;assumes return not only ref to var.
         (cond ((eq pli-ret-type '|...|)
                (setq pli-ret-type
                      (or (cadr (assq (cadr list) pli-decls)) '|...|)))))
        ((eq (car list) 'multiple-value) (pli-gd (caddr list)))
        ((memq (car list) '(psetq setq))
         (do ((l (cdr list) (cddr l)))
             ((null l))
           (pli-gd2 (car l) (cadr l))
           (pli-gd (cadr l))))
        ((memq (car list) '(arrayfetch arraystore))
         (pli-gd2 (cadr list) list)
         (pli-gd (caddr list)))
        (T (cond ((and (null (assq (car list) pli-decls)) ;all crockish!
                       (null (get (car list) 'pli-gformat))
                       (not (memq (car list) '(abs remainder)))) ;etc.
                  (setq pli-decls
                        (append
                          pli-decls
                          (ncons (list (car list)
                                       '|EXTERNAL ENTRY (...) RETURNS (...)|))))))
           (do ((l (cdr list) (cdr l)))
               ((null l))
             (pli-gd2 (car l) list)
             (pli-gd  (car l))))))

(defun pli-gd2 (symbol expr)
  (cond ((and (symbolp symbol) (not (memq symbol '(T nil))))
         (let ((type
                 (cond ((cadr (assq expr pli-decls)))
                       ((numberp expr) 'FLOAT)
                       ((not (listp expr)) '|...|)
                       ((memq (car expr)
                              '(< lessp > greaterp = + - * // expt ^
                                  abs remainder))
                        'FLOAT)
                       ((memq (car expr)
                              '(car cdr caar cadr cdar cddr rplaca rplacd))
                        (setq pli-list-type '|...|) 'POINTER)
                       ((memq (car expr) '(arrayfetch arraystore))
                        '|(...) ...|)
                       (T '|...|)))
               (entry (assq symbol pli-decls)))
           (cond ((null entry)
                  (setq entry (list symbol '|...| 'EXTERNAL))
                  (setq pli-decls (append pli-decls (list entry)))))
           (cond ((not (eq type '|...|)) (setf (cadr entry) type)))))))

(defun pli-defun-body (list)
  (cond ((null list))
        ((eq (car (car list)) 'loop) ;allow tail-recursive loops
         (if (cdr list) (barf "Ill-formed DEFUN body"))
         (pli-top-loop (car list)))
        ((null (cdr list))
         (let ((pli-statement nil))
           (GF "A'RETURN ('*')'" (car list))))
        (T (GF "A*" (car list)) (pli-defun-body (cdr list)))))

;This does PROG when it is wrapped around the entire body of a defun.
;in this case the returns are actually returns from the fn and everything
;is hunky dory.  Other PROGs aren't handled at all now.  Assumes no
;multiple labels.

(defun pli-top-prog (list) (pli-prog-sexpr (cddr list)))

(defun pli-top-loop (list)
  (if (eq (cadr list) 'do) (pop list))
  (if (not (Rand-fast (not (symbolp (Elist (cdr list))))))
      (barf "Ill-formed LOOP expression."))
; (GF "A{-4A'LP:'{0[<_*A>]A_'GO LP;'}}" (cdr list))
  (GF "A{0'DO WHILE (''1''B);'A___{0[<*A>]}A'END;'}" (cdr list)))

(defun pli-prog-sexpr (list)
  (cond ((null list))
        ((not (atom (car list)))
         (GF "A*" (car list)) (pli-prog-sexpr (cdr list)))
        (T (GF "A{#A*': '*}" (- (+ 2 (flatsize (car list))))
                             (car list) (cadr list))
           (pli-prog-sexpr (cddr list)))))

;this does SETQs.

(def-pli-gformat setq (list)
  (let ((pli-statement nil))
    (GF "{0[<{3*_N'='_*';'}A>]}}" (cdr list)))
  (pli-statement? T))

;you need a lot more than this in general, but this will work for the demo
;so I am punting the rest.  The coder is a better place to put assignment
;sorting anyway.

;This prints CONDs as IF/THEN/ELSEs.

(def-pli-gformat if (list)
  (GF "*" `(cond (,(cadr list) ,(caddr list))
                 ,@(if (cdddr list) `((t ,@(cdddr list)))))))

(def-pli-gformat when (list)
  (GF "*" `(cond (,(cadr list) ,@(cddr list)))))

(def-pli-gformat cond (list)
  (GF "{3[$pli-cond-clause . %pli-cond-rest ]}" (cdr list)))

(defun pli-cond-rest (list)
  (cond (list
         (GF "A['ELSE'_+3$pli-cond-clause . %pli-cond-rest ]" list))))

(defun pli-cond-clause (list)
  (cond ((not (eq (car list) 'T))
         (let ((pli-statement nil))
           (GF "'IF'_*_N'THEN'_" (car list)))))
  (GF "%pli-progn " (cdr list)))

(def-pli-gformat progn (list) (pli-progn (cdr list)))

(defun pli-progn (list)
  (cond ((null list) (pli-statement? T))
        ((null (cdr list)) (GF "*" (car list)))
        (T (GF "{4'DO;'[<_*N>]_'END'}" list) (pli-statement? T))))

;these are function like things that can only be used as statements
;Note we print the ";"s as part of the statements so that the boxes will
;work out right.

(defmacro pli-expression-format-bind (level statement? body)
  `(progn
     (let ((need-parens (and (not (zerop ,level))
                             (not (> ,level pli-bind))))
           (pli-bind ,level)
           (pli-statement nil))
       (cond (need-parens (GF "{0'('")))
       ,body
       (cond (need-parens (GF "')'}"))))
     (pli-statement? ,statement?)))

(defun pli-statement? (required?)
  (cond (pli-statement (GF "';'"))
        (required? (warn "Nested statement in PLI mode"))))

(def-pli-gformat return (list)
 (pli-expression-format-bind 0 T
   (cond ((and pli-subroutine (eq (cadr list) nil))
          (GF "'RETURN'"))
         (T (GF "{0'RETURN'_'('*')'}" (cadr list))))))

(def-pli-gformat go (list)
  (pli-expression-format-bind 0 T
    (GF "{0'GOTO'_*}" (cadr list))))

;this does all other functions

(defun pli-fn-format (list)
  (cond (*printing-as-pli*
         (pli-expression-format-bind 0 nil
           (GF "{0*(1<*','_N>)}" (car list) (cdr list))))
        (T (funcall *saved-fn-gformat* list))))

;  T   is assumed to always be true (ie, '1'B) -- not a bad assumption.

(defun pli-test-t (thing)
  (when (and *printing-as-pli* (eq thing T)) (pli-t-format thing) T))

(defun pli-t-format (ignore)
  (pli-expression-format-bind 0 nil
    (GF "'''1''B'")))

;  NIL is assumed to always be the null pointer (ie, NULL()) -- quite a
;      bad assumption; it is very often false (ie, '0'B).

(defun pli-test-nil (thing)
  (when (and *printing-as-pli* (null thing)) (pli-nil-format thing) T))

(defun pli-nil-format (ignore)
  (pli-expression-format-bind 0 nil
    (GF "'NULL()'")))

(def-pli-gformat kbe-#-comment-macro (x)
  (pli-expression-format-bind 20 nil
    (cond ((equal *kbe-role-marker* "{")
           (new-style-kbe-/#-comment-macro-print x))
          (T (old-style-kbe-/#-comment-macro-print x)))))

(defun new-style-kbe-/#-comment-macro-print (x)
  (let* (((code name) (cdr x))
         (comment (kbe-comment-name->string name)))
    (cond ((null code) (GF "{1'{'S'}'}" comment))
          ((and (listp (car code)) (eq (caar code) '...))
           (GF "{1'('{1'{'S'}'}-{0%role-args }')'}"
               comment (cdar code)))
          (T (GF "{2'{'[*', 'B.]S'}'}" code comment)))))

(defun old-style-kbe-/#-comment-macro-print (x)
  (let* (((code name) (cdr x))
         (comment (kbe-comment-name->string name)))
    (GF "{2'#{'[*', 'B.]S'}'}" code comment)))

(defun (|...| :gformat) (x)
  (gf "{5'(... '%role-args ')'}" (cdr x)))

(defun role-args (args)
  (let ((same-line T) (first-line T))
    (dolist (arg args)
      (if first-line (setq first-line nil)
          (cond ((not same-line) (gf "-"))
                (T (gf "_"))))
      (cond ((and (symbolp arg) (equal (string arg) "&IN-OUT"))
             (gf "*" arg)
             (setq same-line T))
            (T (gf "*" arg) (setq same-line nil))))))

;this converts expressions to infix.  there are several key problems:
;  priority and parentheses are kept track of by the variable
;PLI-BIND.  this has the finding strength of the containing expression
;(0 at top [statement] level).  If the operator to be printed has
;stronger binding than the parent then it is just printed, otherwise
;parentheses are used.  [Note that it doesn't take advantage of left to
;right precedence at the moment.]  The binding strengths are:
; 0 top level (weakest)
; 1 between commas (not in a statement, but super weak binding)
; 2 OR
; 3 AND
; 4 < lessp > greaterp = eq
; 5 + -
; 6 * /
; 7 expt ^
; 8 unary- NOT NULL (unary operators - highest)
;note some fns like CAR and NULL turn into expressions like things.
;  The fn NOT is used two ways in lisp: as NULL and as NOT.  Here I
;assume that no variable holds a boolean value, and that certain functions
;(like < and OR) return boolean values while the rest don't.
;THIS IS QUITE BUGGY!!!

(defmacro def-pli-expression-format (lisp pli level)
  `(def-pli-gformat ,lisp (list)
     (pli-expression-format-bind ,level nil
       ,(list 'GF (string-append "{0[<*_'" pli "'_N>]}") '(cdr list)))))

(def-pli-expression-format or /| 2)
(def-pli-expression-format and & 3)
(def-pli-expression-format < < 4)
(def-pli-expression-format lessp < 4)
(def-pli-expression-format > > 4)
(def-pli-expression-format greaterp > 4)
(def-pli-expression-format = = 4)
(def-pli-expression-format eq = 4)
(def-pli-expression-format + + 5)
(def-pli-expression-format * * 6)
(def-pli-expression-format // // 6)
(def-pli-expression-format expt ** 7)
(def-pli-expression-format ^ ** 8)

(def-pli-gformat - (list)
  (cond ((cddr list)
         (pli-expression-format-bind 5 nil
           (GF "{0[<*_'-'_N>]}" (cdr list))))
        (T (pli-expression-format-bind 8 nil
             (GF "{0['-'N*]}" (cdr list))))))

(def-pli-gformat not (list)
  (cond ((and (not (atom (cadr list)))
              (memq (car (cadr list))
                    '(< lessp > greaterp = atom or and)))
         (pli-expression-format-bind 8 nil
           (GF "{0'^E'*}" (cadr list))))
        (T (pli-expression-format-bind 4 nil
             (GF "{0*'=NULL()'}" (cadr list))))))

(putprop 'null (get 'not 'pli-gformat) 'pli-gformat)

(def-pli-gformat arrayfetch (list)
  (pli-expression-format-bind 0 nil
    (GF "{0[*'('*]')'}" (cdr list))))

(def-pli-gformat arraystore (list)
  (pli-expression-format-bind 0 T
    (GF "{0[*'('*') = '*]}}" (cdr list))))

;The list functions are recoded using pointers.  This works fine
;for CAR CDR RPLACA and the like, but not for CONS.  To get CONS to work,
;the coder would have to be told that CONS returns its value in a free
;variable.  Putting this a bit stronger, you cannot have any function
;that returns a pointer, you cannot nest it in a pointer qualification
;expression (that is to say you cannot take the CAR of it) unless you
;put it in a variable first.  this could easily be fixed by the coder
;easy enough.

(def-pli-gformat car (list)
  (pli-expression-format-bind 0 nil
    (GF "{*'->VALUE'}" (cadr list))))
(def-pli-gformat cdr (list)
  (pli-expression-format-bind 0 nil
    (GF "{*'->NEXT'}" (cadr list))))
(def-pli-gformat caar (list)
  (pli-expression-format-bind 0 nil
    (GF "{*'->VALUE->VALUE'}" (cadr list))))
(def-pli-gformat cadr (list)
  (pli-expression-format-bind 0 nil
    (GF "{*'->NEXT->VALUE'}" (cadr list))))
(def-pli-gformat cdar (list)
  (pli-expression-format-bind 0 nil
    (GF "{*'->VALUE->NEXT'}" (cadr list))))
(def-pli-gformat cddr (list)
  (pli-expression-format-bind 0 nil
    (GF "{*'->NEXT->NEXT'}" (cadr list))))

(def-pli-gformat rplaca (list)
  (pli-expression-format-bind 0 T
    (GF "{3*'->VALUE'_N'='_*}" (cadr list) (caddr list))))
(def-pli-gformat rplacd (list)
  (pli-expression-format-bind 0 T
    (GF "{3*'->NEXT'_N'='_*}" (cadr list) (caddr list))))
