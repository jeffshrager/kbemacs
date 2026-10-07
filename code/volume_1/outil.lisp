;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 194-223; listing outil.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; This file contains a bunch of obsolete macros that many of DICK's files
;;; use, but which should not be used in new code. Functions defined in
;;; this file will be phased out as soon as possible.

;;; one of the gross things about this file is that you have to load it
;;; before you can compile it in maclisp.

(declare (special deffn pat assigned macros))

;;; This makes it possible to assign to an arbitrary pattern when this
;;; file is loaded.
;;;
;;; Note: I am claiming that the right thing is returned from the
;;;       pattern is important to make things work, even though it is
;;;       a lie --Dick

;;; Notes:
;;;
;;; BEGIN is not used externally to this file. It can go away any time.
;;;   -kmp
;;;
;;;

(define-setf-handler /[ (pat v) `(match ,v ,pat nocheck))

;THIS WAS MAINLY JUST USED FOR DESTRUCTURING AND SHOULD NOT BE NEEDED
;ANY MORE. IT CANNOT GO AWAY, HOWEVER, BECAUSE A FEW PLACES STILL USE IT.

;the following set of functions and macros implement a simple pattern-
;matching facility. the main ideas behind this facility are:
;
; 1. the main use is intended to be picking out pieces of a list structure
; 2. the facility is intended to be efficient, to give you as much power
;    as can be gotten cheaply.
; 3. to this end all the pattern matching expands into inline code,
;    instead of being a call on a subroutine.
; 4. the basic matching is the inverse of the list construction described
;    above.
;               for example if v1 = 4 and v2 = 5 then:
;               ['foo v1 v2] evaluates to '(foo 4 5)
;               as an inverse of this:
;               (match '(foo a b) ['foo v1 v2]) returns T
;                  and sets v1 to 'a and v2 to 'b
;               with this simple type of pattern,
;               (equal list (progn (match 'list pattern) (eval 'pattern)))
; 5. a basic pattern consists of a series of "pattern-elements" in []s
;    each pattern-element is either:
;       a. an atom, this is assigned the value of the corresponding item
;             in the matched list.
;       b. a quoted atom, this matches an occurence of the atom.
;             otherwise the pattern fails returning NIL.
;       c. a pattern in []s, this matches recursively against a sublist
;             in the matched list.
;     the matching proceeds by matching the first element of the pattern
;     against the first element of the list, the second against the second
;     etc. if a pattern element is a pattern list then matching recurses
;     matching this pattern against the sublist at this point.
; 6. the basic pattern is extended in several ways:
;       a. a pattern element may be the special atom "_" which
;          matches anything.
;       b. a pattern element may be preceded by the special atom "!" which
;          indicates that the pattern applies to the CDR of the matched
;          list at this point rather than the CADR.  in addition when
;          an ! match succeeds it does not move the matching position
;          pointer.  for example [! x ! y] will assign exactly the same
;          thing to x and y.  it should be noted that !
;          is the inverse of the list constructor ! only if it is on the
;          last pattern element.  ! can not be used to match against a
;          non-terminal portion of a list.  the pattern never has to back
;          up, and anything which is assigned to a variable is an actually
;          part of the matched list rather than a copied out central
;          portion.  this is a clear example of getting all that can be
;          gotten from ! cheaply without going on to do the hard stuff.
;       c. a pattern element can be followed by arbitraryly many of the
;          following three things in any order
;       d. an unquoted sexpr, this is evaluated and the match fails if it
;          returns NIL
;       e. a quoted sexpr, this is evaluated and the value ignored
;       f. in both types of sexpr, an occurence of _ referres to the
;          current item being matched against unless it is quoted.
;       g. the special atom "=" followed by another atom, the atom is
;          assigned the value of the corresponding item in the matched
;          list.  (this is particularly usefull when the basic
;          pattern-element is a sub pattern)
;       h. several patern-elements can be set up as alternatives by
;          seperating them by "~"s. if the first one of these fails to
;          match, then instead of causing the entire pattern to fail,
;          the matcher attempts to match the next alternative against the
;          same place.  as soon as one of the alternatives succeedes,
;          control passes to after the last alternative.
;       7. if the match fails, it returns NIL but no attempt is made to
;          undo any of the side effects which may have happend before
;          the point of failure.
; 8. the parts of the pattern are evaluated in the exact order in which
;    they appear.
; 9. the matcher has a mode where it assumes that the pattern will match
;    and does not check that it does match (except in explicit
;    alternatives) this saves a lot of time when the mtching is being
;    used solely for the purpose of selecting parts of a list.

;consider the following examples of the use of the matcher:
;1. simple selection of parts of a list
;       (match foo [p1 p2])
;       if foo = '(a b) then
;       the match succeeds and p1 <- 'a, p1 <- p2
;       if foo = '(a b c) then
;       the match fails becasue foo is too long and p1 <- 'a, p2 <- 'b
;       if foo = 4 then
;       the match fails because foo is not a list and the values
;       of p1 and p2 are not changed
;2. more complex selection of parts of a list
;       (match foo [![p1 p2 ! _ = p3] = p0])
;       if foo = '(a b) then
;       the match succeeds and p0 <- '(a b), p1 <- 'a, p2 <- 'b, p3 <- nil
;       if foo = '(a b c) then
;       the match succeeds and
;          p0 <- '(a b c), p1 <- 'a, p2 <- 'b, p3 <- '(c)
;       if foo = 4 then
;       the match fails and p0, p1, p2, p3 are unchanged.
;       (note that (match foo [! p0 p1 p2 ! p3]) does the same thing
;       whenever it succeeds but that p0 will always be assigned to
;       no matter how it fails)
;3. an optional trailing argument
;       (match foo [p1 p2 ~ ! _ '(setq p2 'default)])
;       if foo = '(a b) then
;       the match succeeds and p1 <- 'a, p2 <- 'b
;       if foo = '(a) then
;       the match succeeds and p1 <- 'a, p2 <- 'default
;       if foo = '(a b c) then
;       the match succeeds and p1 <- 'a, p2 <- 'default
;       if foo = 4 then
;       the match fails and p1, p2 are unchanged
;4. an optional internal item. It is important to note that the match
;       does not look ahead and will not back up.  suppose we expect a
;       list to contain (p1 (optional p2) p3 p4)
;       there are several ways this type of match can be done:
;       a. it is important to note that this way will not work!
;          (match foo [p1 p2 ~ ! _ p3 p4])
;          if foo = '(a b c d) then
;          the match will succeed and
;              p1 <- 'a, p2 <- 'b, p3 <- 'c, p4 <- 'd
;          if foo = '(a c d) then
;          the match will fail and
;              p1 <- 'a, p2 <- 'c, p3 <- 'd, p4 unchanged
;          after p2 successfully matches 'c, the matcher goes on and
;           will not backup when there is nothing to match p4 against.
;       b. we can explicitly encode the backup.
;              (match foo [p1 ![p2 p3 p4] ~ ![p3 p4]])
;       c. we can explicitly look ahead.
;              (match foo [p1 ! _ (eq (length _) 2) ~ p2 p3 p4])
;       d. if there is some feature which p2 has and p3 can not have then
;          we can test that for example if p3 must be a list and p2
;          must be an atom then we can use
;              (match foo [p1 _ (atom _) = p2 ~ ! _ p3 p4])
;5. for a complex example of a pattern see the example in the macro deffn

;the following grammer gives the exact syntax of a pattern

;          pattern = p-list
;           p-list = [ {p-clause}*n ]
;         p-clause = {p-element ~}*n p-element
;        p-element = {!}
;                    atom v _ v ' atom v p-list
;                    {sexpr v = atom v ' sexpr}*n

;the pattern checking functions below check that a given pattern is
;syntactically correct, and return an internal form of the pattern which
;is described by the grammer below.  In the internal form a pattern
;element consists of three parts:
;       1. a flag specifying whether or not the element matches the
;               CADR of CDR of the current point.
;       2. a subpattern or nil if there is none.
;       3. a list of sexprs which will be evaluated in the order given.
;               if a sexpr is quoted then its value will be ignored,
;               otherwise a NIL value will cause the match to fail.
;in addition u1-p-check returns a list of all of the variables explicitly
;assigned too (ie cases 6.a and 6.g above) the if a is assigned to twice,
;it is in the list twice.

;  checked-pattern = checked-p-list
;   checked-p-list = ( {checked-p-clause}*n )
; checked-p-clause = ( {checked-p-element}*n>0 )
;checked-p-element = ( T v NIL
;                      checked-p-list v NIL
;                      ( { {'} sexpr }*n ) )
;
;------------------------------------------------------------------------
;
;as an example of the checking operation:
;
;
;       (u1-p-check '[! [CODE (MEMQ CODE '(A B C))  ~ ! _  '(SETQ CODE 'C)
;                        OPERATION
;                        ['OPANDS
;                          [_ ! _] = OPAND1
;                          OPAND2 (NOT (ATOM OPAND2))]
;                        ENV  ~ ! _  '(SETQ ENV NIL)]        = BODY])
;
;==>
;          ((((T (((NIL NIL ('(SETQ CODE _) (MEMQ CODE '(A B C))))
;                 (T NIL ('(SETQ CODE 'C))))
;                ((NIL NIL ('(SETQ OPERATION _))))
;                ((NIL
;                  (((NIL NIL ((EQ _ 'OPANDS))))
;                   ((NIL (((NIL NIL NIL)) ((T NIL NIL)))
;                         ('(SETQ OPAND1 _))))
;                   ((NIL NIL ('(SETQ OPAND2 _) (NOT (ATOM OPAND2))))))
;                  NIL))
;                ((NIL NIL ('(SETQ ENV _))) (T NIL ('(SETQ ENV NIL)))))
;               ('(SETQ BODY _)))))
;          BODY ENV OPAND2 OPAND1 OPERATION CODE)

;;; U1-P-CHECK
;
;                             BEHAVIOR
;INPUT  PATTERN      - a pattern
;OUTPUT RETURN VALUE - a checked pattern in internal form and a list
;                      of assigned variables
;
;
;                             PLAN
;VARS   ASSIGNED - a special variable used to collect the
;                  assigned variables
;
;METHOD the fns below exactly follow the structure of the grammers above
;       in order to check and parse the pattern

(defun u1-p-check (pattern)
  (let ((assigned nil))
    (declare (special assigned))
    (cons (u1-p-l-check pattern) (reverse assigned))))

;------------------------------------------------------------------------

;                             BEHAVIOR
;INPUT  PAT - pattern (this variable is special, and is used by the
;             following two functions as they step through the pattern.
;             it is rebound at each call of u1-p-l-check
;             as parsing recurses into a sub pattern)
;OUTPUT RETURN VALUE - a checked pattern

(defun u1-p-l-check (pat)
  (cond ((or (atom pat)
             (not (eq (car pat) '/[)))          ;a pattern must be in []s
         (ferror "u1-p-l-check1 pattern fails to be a list starting with '[;
 look up stack at special variable pat to see whole pattern:~%~S" pat))
        (t (setq pat (cdr pat))))
  (do ((p-l nil [(u1-p-c-check) ! p-l])) ;a pattern is a list of clauses
      ((null pat) (reverse p-l))))

;-------------------------------------------------------------------------

(defun u1-p-c-check ()
  (cond ((atom pat)
         (ferror "u1-p-c-check1 pattern clause fails to be a list of one
 or more elements; look up stack at special variable pat to see more:~%~S" pat)))
  (do ((p-c [(u1-p-e-check)]                                      ;clause is one or more elements
            [(u1-p-e-check) ! p-c]))
      ((or (atom pat)
           (cond ((eq (car pat) '/:)
                  (ferror "/":/" in pattern")))
           (not (eq (car pat) '~)))                               ;seperated by ~s
       (reverse p-c))
      (setq pat (cdr pat))))

(defun u1-p-e-check ()

;                             PLAN
;VARS   BANG - flag set if ! is found
;       SUB-PAT - holds the sub pattern if any
;       SEXPRS - collects the sexprs derived from the pattern
;PURPOSE this is the main routine. it matches and parses a pattern element.
;       it just follows the grammer, lengthy but easy.

  (prog (bang sub-pat sexprs)
    (cond ((atom pat)
           (ferror "u1-p-e-check1 pattern element is fails to be list of items;
 look up the stack at special variable pat to see more:~%~S" pat))
          ((eq (car pat) '!)                                     ;OPTIONAL initial !
           (setq bang t)
           (setq pat (cdr pat))))
    (cond ((atom pat)
           (ferror "u1-p-e-check2 pattern element fails to have obligatory
basic matching element; look up te stack at special variable pat to see more:~%~S" pat))
          ((eq (car pat) '_))                                    ;OBLIGATORY _
          ((atom (car pat))                                      ;or atom
           (setq sexprs
                 [['quote ['setq (car pat) '_]]                    ;which gets assigned to
                  ! sexprs])
           (setq assigned [(car pat) ! assigned]))                 ;remember was assigned to
          ((and (eq (caar pat) 'quote)                           ;or quoted atom
                (atom (cadar pat)))
           (setq sexprs
                 [['eq '_ (car pat)]                               ;which gets compared
                  ! sexprs]))
          ((eq (caar pat) '/[)                                   ;or subpattern
           (setq sub-pat (u1-p-l-check (car pat))))
          (t (ferror "u1-p-e-check3 subpattern fails to begin with '[;
 look up the stack at the special varible pat to see more:~%~S" pat)))
    (setq pat (cdr pat))
    (do ()                                                       ;0-n extra expressions
        ((or (atom pat)                                          ;if pat points to the start of
             (and (atom (car pat))                               ;a pattern element then we
                  (not (eq (car pat) '=)))                       ;are done
             (and (not (atom (car pat)))
                  (eq (caar pat) 'quote)
                  (not (atom (cdar pat)))
                  (atom (cadar pat)))
             (and (not (atom (car pat)))
                  (eq (caar pat) '/[))))
      (cond ((eq (car pat) '=)                                   ;might be a = assignment
             (setq pat (cdr pat))
             (cond ((or (atom pat)
                        (not (atom (car pat))))
                    (ferror "u1-p-e-check = in pattern fails to be followed by on atom;
 look up the stack at special variable pat to see more:~%~S" pat)))
             (setq sexprs
                   [['quote [ 'setq (car pat) '_]]               ;in which case do assignment
                    ! sexprs])
             (setq assigned [(car pat) ! assigned]))             ;remember was assigned
            (t (setq sexprs [(car pat) ! sexprs])))              ;otherwise take sexpr unchanged
      (setq pat (cdr pat)))
    (return [bang sub-pat (reverse sexprs)])))                   ;return the checked pattern

;the macro MATCH produces code to do the mathcing. the code produced to match a pattern
;against a list has the form of a lambda binding up the list as U1-ITEM, whose body is an
;AND.  the sexprs whose value does not effect the success or failure of the match are
;hidden inside PROGNs.  a set of alternatives shows up as an OR.  MATCH tries to be smart
;enough so that simple cases produce simple code.  The following is an example of the code
;produced.

;        (MATCH FOO [! [CODE (MEMQ CODE '(A B C))  ~ ! _  '(SETQ CODE 'C)
;                       OPERATION
;                       ['OPANDS
;                         [_ ! _] = OPAND1
;                         OPAND2 (NOT (ATOM OPAND2))]
;                       ENV  ~ ! _  '(SETQ ENV NIL)]        = BODY])
; expands into ==>
;
;       ((LAMBDA (U1-ITEM)
;          (AND ((LAMBDA (U1-ITEM)
;                  (AND (OR (AND (NOT (ATOM U1-ITEM))
;                                (PROGN (SETQ CODE (CAR U1-ITEM))
;                                (MEMQ CODE '(A B C)))
;                                (PROGN (SETQ U1-ITEM (CDR U1-ITEM)) T))
;                           (PROGN (SETQ CODE 'C) T))
;                       (NOT (ATOM U1-ITEM))
;                       (PROGN (SETQ OPERATION (CAR U1-ITEM))
;                              (SETQ U1-ITEM (CDR U1-ITEM))
;                       (NOT (ATOM U1-ITEM)))
;                       ((LAMBDA (U1-ITEM)
;                           (AND (NOT (ATOM U1-ITEM))
;                                (EQ (CAR U1-ITEM) 'OPANDS)
;                                (PROGN (SETQ U1-ITEM (CDR U1-ITEM))
;                                (NOT (ATOM U1-ITEM)))
;                                ((LAMBDA (U1-ITEM) (NOT (ATOM U1-ITEM))) (CAR U1-ITEM))
;                                (PROGN (SETQ OPAND1 (CAR U1-ITEM))
;                                       (SETQ U1-ITEM (CDR U1-ITEM))
;                                (NOT (ATOM U1-ITEM)))
;                                (PROGN (SETQ OPAND2 (CAR U1-ITEM))
;                                (NOT (ATOM OPAND2)))
;                                (NULL (CDR U1-ITEM))))
;                        (CAR U1-ITEM))
;                        (PROGN (SETQ U1-ITEM (CDR U1-ITEM))
;                        (OR (AND (NOT (ATOM U1-ITEM))
;                                 (PROGN (SETQ ENV (CAR U1-ITEM))
;                                 (NULL (CDR U1-ITEM))))
;                            (PROGN (SETQ ENV NIL) T)))))
;               U1-ITEM)
;              (PROGN (SETQ BODY U1-ITEM) T)))
;        FOO)

;MATCH takes an optional second argument which if eq to NOMATCH causes the code produced
;to assume that the pattern will match.  in this case, the lambda bodies do not need to
;be ANDs and all of the predicates are ignored. this leads to a significant reduction
;in the code produced as the following example shows.

;       (MATCH FOO [! [CODE (MEMQ CODE '(A B C))  ~ ! _  '(SETQ CODE 'C)
;                      OPERATION
;                      ['OPANDS
;                        [_ ! _] = OPAND1
;                        OPAND2 (NOT (ATOM OPAND2))]
;                      ENV  ~ ! _  '(SETQ ENV NIL)]        = BODY]  NOCHECK)
;
;expands into ==>
;
;       ((LAMBDA (U1-ITEM)
;          ((LAMBDA (U1-ITEM)
;              (OR (AND (NOT (ATOM U1-ITEM))
;                       (PROGN (SETQ CODE (CAR U1-ITEM))
;                       (MEMQ CODE '(A B C)))
;                       (PROGN (SETQ U1-ITEM (CDR U1-ITEM)) T))
;                  (PROGN (SETQ CODE 'C) T))
;              (SETQ OPERATION (CAR U1-ITEM))
;              (SETQ U1-ITEM (CDR U1-ITEM))
;              ((LAMBDA (U1-ITEM)
;                   (SETQ U1-ITEM (CDR U1-ITEM))
;                  (SETQ OPAND1 (CAR U1-ITEM))
;                  (SETQ U1-ITEM (CDR U1-ITEM))
;                  (SETQ OPAND2 (CAR U1-ITEM)))
;               (CAR U1-ITEM))
;              (SETQ U1-ITEM (CDR U1-ITEM))
;              (OR (AND (NOT (ATOM U1-ITEM))
;                       (PROGN (SETQ ENV (CAR U1-ITEM))
;                        (NULL (CDR U1-ITEM))))
;                  (PROGN (SETQ ENV NIL) T)))
;            U1-ITEM)
;          (SETQ BODY U1-ITEM))
;        FOO)

(defun (match macro) (body)

;                            BEHAVIOR
;INPUT  BODY = ( 'match obj pattern {check})
;OUTPUT RETURN VAL - the code to do the matching (this replaces the macro call itself)
;                            PLAN
;VARS   OBJ - the object to match the pattern against (this will be evaluated before the match)
;       PATTERN - the pattern
;       CHECK - this inhibits code to check matching if (third body) = NOCHECK
;       CODE - this holds the code to return
;METHOD u1-p-cgen does the real work

  (prog (obj pattern check code)
    (cond ((lessp (length body) 3)
           (ferror "Not enough arguments to function MATCH:~%~S" body)))
    (setq obj (cadr body)
          pattern (caddr body))
    (setq check (cond ((atom (cdddr body)))
                      ((not (eq (cadddr body) 'nocheck)))))
    (setq code (u1-p-cgen check obj
                          (car (u1-p-check pattern))))
    (cond ((atom code) (setq code ['progn code])))
    (rplaca body (car code))
    (rplacd body (cdr code))
    (return body)))

(defun u1-p-cgen (check item pattern)

;                           BEHAVIOR
;INPUT  CHECK - T/NIL to check or not
;       ITEM - the item to match the pattern against
;       PATTERN - the checked pattern

    (cond ((null pattern) 't)                                     ;if no pattern no matching
          ((and (null (cdr pattern))                              ;[![ ... ]] = [ ... ]
                (null (cdar pattern))                             ;a minor optimization to save
                (caaar pattern)                                   ;code
                (null (caddr (caar pattern))))
           (u1-p-cgen check item (cadaar pattern)))
          (t (u1-make-lambda item                                 ;make a lambda of
               [(u1-make-body check
                  (do ((code (u1-p-c-cgen check t nil
                                (car (last pattern)))
                             [!(u1-p-c-cgen check nil code        ;all the clause codes
                                 (car pat))
                              ! code])
                       (pat (cdr (reverse pattern))
                            (cdr pat)))
                      ((null pat) code)))]))))

;---------------------------------------------------------------

(defun u1-p-c-cgen (check last following p-c)

;                            BEHAVIOR
;INPUT  CHECK - T/NIL
;       LAST - T/NIL whether clause is last in the pattern (therefore will need to test
;               whether item matched against is too long)
;       FOLLOWING - ARB/NIL this says whether or not any of the clauses following this one
;               have produced any code at all, if not then this clause does not have to
;               produce any code to get ready for the next clause to exicute
;               (namely cdring down the list)
;OUTPUT RETURN VAL - the code for the clause

  (cond ((null (cdr p-c))
         (u1-p-e-cgen check last following (car p-c)))            ;if only one element no OR
        (t[(u1-make-or                                            ;an OR of
             (mapcar #'(lambda (x)
                         (u1-make-body t
                           (u1-p-e-cgen t last following x)))     ;the codes for the elements
                     p-c))])))

(defun u1-p-e-cgen (check last following p-e)

;                           BEHAVIOR
;INPUT  same as for u1-p-c-cgen
;OUTPUT RETURN VAL - the code for the element
;                           PLAN
;VARS   BANG - SUBP - SEXPRS the parts of the pattern
;       CURRENT - the thing being matched against. either U1-ITEM or (CAR U1-ITEM)

  (prog (bang subp sexprs current)
    (setq bang (car p-e)
          subp (cadr p-e)
          sexprs (caddr p-e))
    (setq current (cond (bang 'u1-item)
                        (t '(car u1-item))))
    (cond ((not check)                                            ;if not checking throw away
           (setq sexprs                                           ;the predicates
                 (mapcar #'(lambda (x)
                             (cond ((and (not (atom x))
                                         (eq (car x) 'quote))
                                    x)
                                   (t t)))
                         sexprs))))
    (setq sexprs                                                  ;substitute CURRENT for _ in
          (mapcar #'(lambda (x)                                   ;the sexprs
                      (cond ((atom x) x)
                            ((eq (car x) 'quote)                  ;outermost ' is different
                             ['quote ! (u1subst current '_ (cdr x))]) ;does not block subst
                            (t(u1subst current '_ x))))
                  sexprs))
    (setq sexprs [(cond ((and check (not bang))                   ;if checking and no !
                         '(not (atom u1-item)))                   ;must see that u1-item is list
                        (t t))
                  (cond (subp (u1-p-cgen check current subp))     ;recurse for subpattern if any
                        (t t))
                 ! sexprs                                         ;put sexprs here
                  (cond ((and following (not bang))               ;do cdr if needed
                         '(quote (setq u1-item (cdr u1-item))))
                        (t t))
                  (cond ((and check last (not bang))              ;check for end if needed
                         '(null (cdr u1-item)))
                        (t t))])
    (return (delq 't sexprs))))                                   ;throw away the Ts

;---------------------------------------------------------------

(defun u1subst (new old list)

;                           BEHAVIOR
;INPUT  NEW - object to replace old
;       OLD - an object usually an atom to be substituted for
;       LIST - the list to do the substitution in
;OUTPUT RETURN VAL - the list after substitution
;                           PLAN
;PURPOSE this does substitution just like SUBST except that it does not go done
;       past qoutes

  (cond ((eq list old) new)
        ((or (atom list)
             (eq (car list) 'quote))                              ;stop if hit quote
         list)
        (t (do ((rest list (cdr rest))
                (newl nil (cons (u1subst new old (car rest)) newl)))
               ((null rest) (reverse newl))
               (cond ((atom rest) (return (nreconc newl (u1subst new old rest)))))))))

;these little fns try to be reasonably intelligent about producing basic code structures

(defun u1-make-and (preds)

;make an AND of a list of preds

  (setq preds (u1-flatten-like-ops 'and preds))                   ;expand out inner ands
  (setq preds (delq 't preds))                                    ;discard Ts
  (cond ((null preds) t)                                          ;no preds then T
        ((null (cdr preds)) (car preds))                          ;one pred the pred
        (t ['AND ! preds])))                                      ;else an and
;---------------------------------------------------------------

(defun u1-make-or (preds)

;this is just like u1-make-and but for ORs

  (setq preds (u1-flatten-like-ops 'or preds))
  (setq preds (delq 'nil preds))
  (cond ((null preds) nil)
        ((null (cdr preds)) (car preds))
        (t ['OR ! preds])))
;---------------------------------------------------------------

(defun u1-make-lambda (arg body)

;this makes a lambda out of a body which is a list of actions

  (setq body (u1-flatten-like-ops 'progn body))                   ;expand out inner progns
  (cond ((equal body '(t)) t)                                     ;if only a (T) no lambda
        (t [['lambda '(u1-item) ! body] arg])))                   ;lambda var is 'u1-item
;---------------------------------------------------------------

(defun u1-make-progn (actions)

;just like u1-make-and but makes a PROGN

  (setq actions (u1-flatten-like-ops 'progn actions))
  (cond ((null actions) t)
        ((null (cdr actions)) (car actions))
        (t ['progn ! actions])))

;---------------------------------------------------------------

(defun u1-flatten-like-ops (op actions)

;nconc in any action whose car is op this reduces the height of the list

  (mapcan #'(lambda (action)
              (cond ((or (atom action)
                         (not (eq (car action) op)))
                     [action])
                    (t (cdr action))))
          actions))

(defun u1-make-body (check code)

;                           BEHAVIOR
;INPUT  CHECK - T/NIL
;       CODE - a list of sexprs
;OUTPUT RETURN VAL - if check then an AND of the sexprs with the qouted sexprs
;               hidden in progns so that there values will be ignored.  As few
;               progns as possible are used.  if not check then just a progn of the
;               sexprs with the quotes removed
;                           PLAN
;METHOD the only difficulty comes in trying to get the progns just right

  (setq code (delq 't code))                                      ;get rid of Ts
  (cond (check
         (u1-make-and
           (do ((ocode (reverse code) (cdr ocode))                ;the original code in reverse
                (ncode nil)                                       ;the list of codes to be anded
                (pcode nil))                                      ;a set of actions to be progned
               (())
               (cond ((or (null ocode)                            ;if not a pred
                          (atom (car ocode))
                          (not (eq (caar ocode) 'quote)))
                      (cond (pcode                                ;if some pcode
                             (setq ncode
                                   [(u1-make-progn pcode) ! ncode]))) ;package them up
                                   ;note if only one then no progn is used the one is a pred
                      (cond (ocode (setq pcode [(car ocode)])))) ;set to use pred to end progn
                     (t (setq pcode [(cadar ocode)                ;put action on pcode
                                     !(cond (pcode)               ;if last thing is action
                                            (t [t]))])))          ;must use T in progn
               (cond ((null ocode) (return ncode))))))
        (t (u1-make-progn                                         ;if not checking
             (mapcar #'(lambda (x)
                         (cond ((and (not (atom x))
                                     (eq (car x) 'quote))         ;just strip of quotes
                                (cadr x))
                               (t x)))
                     code)))))

;NOW THAT DEFUN& AND DEFMACRO EXIST, THIS IS OBSOLETE
;DO NOT USE IT ANY MORE.

;the following function is intended to replace defun as the basic function for defining
;functions.  its major improvements are:
;       1. it uses a pattern to specify what the inputs to the function being defined are.
;          they way the pattern is interpreted depends on the type of the function being
;          defined:
;               a. (FEXPR MACRO) the function is known to have only 1 arg. the pattern is
;                  matched against this arg.  if the function is a MACRO and the first
;                  element of the pattern is a literal match against the name of the
;                  macro, that part of the match is optimized away.
;               b. (LEXPR) here the pattern is matched against listify of the args.
;                  if the corespondence between parts of the pattern and the args can
;                  be determined at macro time (ie no top level ~s or !s in an initial portion
;                  of the pattern) then initial parts of teh listify wont actually be done.
;               c. (EXPR) here the pattern is decomposed and the first pattern element is
;                  matched against the first arg etc.  (there must not be any top level
;                  ~s or !s because it must be possible to tell how many args are intended.)
;       2. one of the major uses of the pattern is to pick out parts of an input argument
;          and put them in to variables.  to facilitate this, deffn binds every variable name
;          which is assigned to (by = or as first item in a pattern element).  If you do not
;          want a variable bound, do a  '(setq var _). if you do want it bound but it is not
;          logical to assign a part of the input to it assign some part to it and then override
;       NOTE that all of the variable binding set up in the defined function by deffn
;          is done with lambdas and not with progs. as a result the value of the function
;          is the value of its last form, there should not be a return unless the prog is
;          explicit in the body.
;       3. the pattern has two roles.  first it allows for selecting out parts of the input.
;          second it checks the syntactic correctness of the input.  if the function
;          being  defined is called with an input which does not match the pattern, the
;          function issues an error 'BAD-ARGS-TO-name and a list of  arguments.  For
;          efficiency, the checking feature is turned off in compiled code.  The CHECK
;          parameter to deffn will override this default causing the match to be checked
;          all the time, or (NOCHECK) never.
;       4. in general deffn makes up variable names (begining in U1) for the actual
;          arguments to the function. However it optimizes a bit, and uses names in the
;          pattern if it can.  This results in a simplyfication of the pattern.  as a
;          result a simple deffn like:
;               (deffn foo [a b c] body)
;          produces no more code than the corresponding defun:
;               (DEFUN foo (a b c) body)
;       5. deffn has features aimed at making macro definition easier:
;               a. if the MACROS switch is T then a macro is both known in the
;                  compiler and COMPILED (this is an extension of the basic compiler
;                  action). the keyword KEEP/NOKEEP will override the value of MACROS
;                  for a particular macro definition KEEP is the same as MACROS=T.
;               b. the macros are by default displacing
;                  the DISPLACE parameter will make them always displace
;                  or NODISPLACE never displace.
;       6. deffn takes an optional parameter after the name to specify the property
;          of the atom which the function should go on (this is similar to defuns
;          (name prop)).
;       7. deffn assigns its body to the global variable DEFFN to aid in debugging
;          at initial read in time.

(defun (deffn macro) (mbody)
;                           BEHAVIOR
;INPUT  MBODY - ('deffn
;               name            ;the name of the function
;               {pname}         ;the prop name (the default is based on the type)
;               {type}          ;expr,fexpr,macro or lexpr (expr is default)
;               {keep/nokeep}   ;says whether to compile a macro default nokeep
;               {displace/nodisplace} ;displacing is the default
;               {check/nocheck} ;default is nocheck
;               pattern         ;the pattern describing the inputs
;               ! body)         ;the body of the function
;OUTPUT RETURN-VALUE - a defun with the right body to do all the stuff discussed above.
;               basically binding the right variables and doing the matching in addition to
;               the body.  deffn always displaces itself.

  (setq deffn mbody)                                              ;to aid debugging
  (prog (name pname namecode type displace? keep? check? pat body fbody)
    (cond ((not (match mbody                                      ;parse the macro call
                  ['deffn
                    name (atom name)
                    _ (memq _ '(expr fexpr macro)) = type = pname
                     ~ 'lexpr = type '(setq pname 'expr)
                     ~ _ (atom _)
                         (not (memq _ '(keep nokeep displace nodisplace check nocheck)))
                         = pname '(setq type 'expr)
                     ~ ! _ '(setq type 'expr pname 'expr)
                   _ (memq _ '(keep nokeep)) '(setq keep? (eq _ 'keep))
                     ~ ! _ '(setq keep? nil)
                   _ (memq _ '(displace nodisplace))
                      '(setq displace? (eq _ 'displace))
                     ~ ! _ '(setq displace? t)
                   _ (memq _ '(check nocheck)) '(setq check? (eq _ 'check))
                     ~ ! _ '(setq check? nil)
                   pat (not (atom pat))
                  ! body (not (atom body))]))
           (ferror "Bad args to DEFFN:~%~S" mbody)))
    (setq namecode                                                ;construct name code for deffn
          (cond ((eq pname 'expr) [name])                         ;if EXPR omit keyword EXPR
                ((memq pname '(fexpr macro))                      ;if FEXPR or MACRO
                 [name pname])                                    ;one after the other
                (t [[name pname]])))                              ;otherwise sublist format
    (setq fbody (u1-make-fn name type displace? check? pat body)) ;call this to do the work
    (setq fbody
          (cond ((and (eq pname 'macro) keep?) ;if a kept macro
                 `(progn 'compile
                    (declare (defun ,name macro ,.fbody))         ;declare a defun
                    (defun (,name macro) ,.fbody)))               ;and compile the macro
                (t ['defun !! namecode !! fbody])))               ;otherwise just normal defun
    (return (displace mbody fbody))))                             ;displace yourself

(defun u1-make-fn (name type displace? check pat body)

;                           BEHAVIOR
;INPUT  name - the name of the fn being defined
;       type - either (macro fexpr expr or lexpr) says how to parse the parameters
;       displace? - either T or nil says whether a macro
;               being defined will displace itself or what
;       check - (t/nil) says whether argument matching should be checked
;               at runtime or just assumed
;       pat - the pattern for the parameters to the fn
;       body - the body of the function
;OUTPUT return value - the cdr of a lambda definition for the fn being defined
;                           PLAN
;VARS   c-pat - the checked form of the pattern
;       vars - the variables assigned too in the pattern (variables which become args
;               are deleted from this list).
;       variable-part - the tail of cpat begining with the first top level pattern
;               element which is either optional or !ed.  Ie the first place where
;               it is not clear how many arguments there are.
;       args - the list of arguments to the top level lambda expression.
;       pat-code - the code which does the pattern matching connected with the args.
;       i - used to count the args to a lexpr.
;       exprl - expression list used with u1findelligablesetq below.
;       assign - a var which can be used directly as an argument name.
;METHOD most of the work here involves deciding how to pattern match to get the vars
;       assigned too there are three seperate segments one for macros and fexprs, one
;       for exprs and one for lexprs.  then the results are consed together and returned

  (prog (c-pat vars variable-part args pat-code i exprl assign)
    (match (u1-p-check pat) [c-pat ! vars] nocheck)               ;check pattern
    (setq variable-part                                           ;get variable part
          (do ((pat c-pat (cdr pat)))
              ((null pat) nil)
              (cond ((or (cdar pat)                               ;there is if an alternative
                         (caaar pat))                             ;or a !
                     (return pat)))))
;for macros and fexprs
    (cond ((memq type '(macro fexpr))
           (cond (nil)
;this stuff is patched out untill JONL fixes the bug where
;displace messes up when compiled, if you also use the var name
;it is displacing for other things. !!!!!
;                ((and (match c-pat [[['t _ ! exprl]] ! _])     ;if there is a variable to use
;                      (setq assign (u1findelligablesetq (car exprl) vars))) ;for the arg
;                 (setq args [(cadadr assign)])                          ;use it
;                 (rplaca exprl (delq assign (car exprl)))      ;take it out of pattern
;                 (setq vars (delq (car args) vars)))           ;take it out of vars
                 (t(setq args ['u1-body])))                       ;otherwise use U1-BODY
           (setq pat-code
                 [(cond ((match c-pat                             ;if first part of pattern is
                                [[['nil 'nil
                                   [['eq '_ [ 'quote _ (eq _ name)]]]]] ;fn name don't bother
                                 ! _ (not (atom _))])
                         (u1-p-cgen check ['cdr (car args)] (cdr c-pat))) ;to match against it
                        (t(u1-p-cgen check (car args) c-pat)))])  ;otherwise match whole thing
           (cond ((and (eq type 'macro) displace?)
                  (cond ((cdr body) (setq body [['progn ! body]]))) ;make body progn if list
                  (setq body [['displace (car args) ! body]]))))
;for exprs
          ((eq type 'expr)
           (cond (variable-part (ferror "u1-make-fn1 fn is an expr but the pattern does not
 specify an unconditional number of arguments:~%~S" deffn)))
           (mapc #'(lambda (p-c)                                  ;for each arg position
                     (cond ((and (match p-c [['nil _ ! exprl]])   ;do same as for macros
                                 (setq assign (u1findelligablesetq (car exprl) vars)))
                            (setq args [(cadadr assign) ! args])
                            (rplaca exprl (delq assign (car exprl)))
                            (setq vars (delq (car args) vars)))
                           (t(setq args [(gensym) ! args])))
                     (rplaca (car p-c) 'T)                        ;set up match for this arg
                     (setq pat-code [(u1-p-cgen check (car args) [p-c]) ! pat-code]))
                 c-pat))
;for lexprs
          ((eq type 'lexpr)
           (setq args 'u1-nargs)
           (setq i 0)
           (prog ()                                               ;so return will work in map
            (map #'(lambda (p-cl)
                     (cond ((eq variable-part p-cl)               ;do listify for variable part
                            (setq pat-code
                                  [(u1-p-cgen check
                                              ['listify ['- i 'u1-nargs]]
                                              p-cl)
                                   ! pat-code])
                            (return t))
                           (t(rplaca (caar p-cl) 't)              ;match others seperately
                             (setq i (1+ i))
                             (setq pat-code
                                   [(u1-p-cgen check
                                               ['arg i]
                                               [(car p-cl)])
                                    ! pat-code]))))
                 c-pat))
           (cond ((and check (not (= i 0)))                       ;check for right number of args
                  (setq pat-code
                        [!! pat-code ['not ['lessp 'u1-nargs i]]])))))
;now for all types
    (cond ((not (atom args))(setq args (reverse args))))          ;args were in wrong order
    (setq pat-code (reverse pat-code))                            ;also pat code
    (cond (check                                                  ;if checking set up for
           (setq pat-code (u1-make-and pat-code))                 ;error message if fail
           (setq pat-code
                 (cond ((eq pat-code t) nil)
                       (t[['cond [['not pat-code]
                                  ['ferror nil
                                           (format nil "Bad args to ~S: ~~S"
                                                   name)
                                           (cond ((atom args) ['listify args])
                                                 (['list ! args]))]]]]))))
          (t(setq pat-code                                        ;otherwise just do neccesary
                  (delq 't                                        ;matching
                        (u1-flatten-like-ops 'progn
                                             pat-code)))))
    (setq body [!! pat-code ! body])                              ;add patcode to body
    (do ((v vars (cdr v)))                                        ;remove duplicat vars
        ((null v))
        (rplacd v (delq (car v) (cdr v))))
    (cond (vars                                                   ;if vars set up a lambda for
           (setq body [[['lambda vars ! body]                     ;them with appropriate initial
                        ! (mapcar #'(lambda (ignore) nil)
                                  vars)]])))
    (setq body [args ! body])                                     ;put the args in front
    (return body)))                                               ;and return

;                           BEHAVIOR
;INPUT  actions - the action list of a pattern element
;       vars - the list of all assignments in the whole pattern
;OUTPUT return value - if there is an assignment in the action list to a var which
;               ocurs exactly once in the var list, the assignment is returned.
;               it is assumed that if this is the case then it is alright to assign
;               the correct value to this variable before the match starts. I think
;               that it may be possible to think up an example where this isn't quite true.

(defun u1findelligablesetq (actions vars)
  (do ((action actions (cdr action))
       (var)(mem))
      ((null action) nil)
      (cond ((and (match (car action) ['quote ['setq var ! _]])
                  (setq mem (memq var vars))
                  (not (memq var (cdr mem))))
             (return (car action))))))

;OBSOLETE USE LET* INSTEAD

;this is a much more fancy substitute for an inline lambda.  the main
;differences are:
;1. the arguments are not evaluated in parallel.
;2. arbitrary patterns can be used instead of a variable as the
;destination of a value.  begin figures what variables have to be lambda
;bound in the same way as deffn.
;3. the correct lambda binding value {nil/0/0.0} is used so that things
;will complie right.
;4. begin does not allow the archaic form of setting values.
;
;the input is of the form:
;(begin (
;   var                     OR
;   var <- arg
;each as many times as desired
;   ) body)
;a var can be either: a variable, or a pattern (recognized by the []s)
;the resulting lambda is:
;((lambda (vars ..) (<- var arg ...) body) {nil/0/0.0} ...)
;
;for example a valid begin is:
;(begin (x  y <- foo  z <- '(1)  [a b c] (caddr y)) body)
;which becomes:
;((lambda (x y z a b c) (<- y foo  z '(1)  [a b c] (caddr y)) body)
; nil nil nil nil nil nil)
;
;this function iterates matching pieces off the front of the
;initializations and then conses up the result.

(deffn begin macro keep displace check ['begin inits ! body]
  (prog (vars assigns)
    (do ()
        ((null inits))
        (cond ((atom inits)
               (ferror "BEGIN inits not a list~% ~S" `(,inits . ,body)))
              ((atom (car inits))                                 ;atom var is ok
               (setq vars [(car inits) ! vars]))                  ;remember var
              ((eq (caar inits) '/[)                              ;pattern var is ok
               (setq vars [!!(cdr (u1-p-check (car inits))) ! vars]))   ;remember vars in pat
              (t (ferror "Unrecognizable init in BEGIN; ~
                          probably a missing /"<-/"~%Source: ~S"
                         `(BEGIN ,inits ,@body))))
        (cond ((or (atom (cdr inits))
                   (not (eq (cadr inits) '<-)))                   ;if <-
               (setq inits (cdr inits)))
              ((atom (cddr inits))
               (ferror "A trailing /"<-/" in a BEGIN "
                       `(BEGIN ,inits ,@body)))
              (t (setq assigns [(caddr inits) (car inits) ! assigns])    ;remember to assign
                 (setq inits (cdddr inits)))))                    ;(note assigns is reversed)
    (cond (assigns
           (setq body [['<- !! (reverse assigns)] ! body])))      ;put assigns in body
    (setq vars (reverse vars))                                    ;put vars in order
    (do ((v vars (cdr v)))
        ((null v))
        (rplacd v (delq (car v) (cdr v))))                        ;kill duplicates
    (return [['lambda vars ! body]                                ;cons up result
             !!(mapcar
                 #'(lambda (ignore) nil)
                vars)])))

;THIS IS GOING TO BE OBSOLETE AS SOON AS I GET TC TO WORK

;the next pages define a looping construct OVER which is particularly convenient in
;a fairly large subset of cases where looping is required.  It is
;inspired by the map functions in lisp.  however it differes from them in the fact
;that instead of a functional argument, it has a literal body which is
;expanded inline into the code and repetatively executed for example:
;       (mapc #'(lambda (x) (print x)) foo)       is the same as
;       (over foo (print _))
;note that the basic order of arguments is reversed (on the theory that
;the functional specification is usually much longer than the data
;specification).  also, "_" is the default name for the variable which
;holds the successive sublists.
;    in addition, over is a bit like do in that it allows
;the user to explicitly specify how to step over the initial value, and
;how to accumulate a result, this leads to a great deal more flexability
;than the six forms of map give you. a use of the macro over (or for) has
;three basic parts:
;       (enumeration-specifications
;        accumulation-specification
;        body)
;there must be at least one enumeration-specification there can be
;several.  each one has six parts:
;       "over"
;       overvar "<-"    ;which defaults (see below) if omitted
;       start-expr      ;which must be present
;       "by" byexp      ;which defaults to (cdr _) or (1+ _) (see below) if omitted
;       "until" untilexp ;which defaults to (null _) or nil (see below) if omitted
;       source-expr     ;which defaults to (car _) if omitted
;       "->" source-var ;which defaults to "_" if there is no overvar
;       there can be any number of additional 'source-expr -> source-var' specifications
;for example the following are all valid enumeration specifications:
;       over [a x] <- foo by (cadr x) until (hunkp x)  -> w  (nreverse (cadar x)) -> [y z]
;note that any fns can be used in the source expression and the variables to assign to
;can actually be patterns.  any variable assigned in the overvar can be referrenced in
;the by, until, and source expresions; the variables assigned to by a source expression
;cannot be referred to in any start by or until expression, but can be refered to by
;any source expression to the right of it.
;    the power of the macro is increased by the fact that there are a lot of defaults available
;for example in the by, until, and source expressions "_" can be used to refer to the
;overvar even if the user has not specified the overvar (! this is not usually compatable with
;using a pattern for the overvar because the entire overvar expression is substituted for
;each instance of "_"). as an example of the use of "_" consider:
;       over x <- foo by (cdr x) until (null x) (car x) -> y          is the same as
;       over x <- foo by (cdr _) until (null _) (car _) -> y          is the same as
;       over foo by (cdr _) until (null _) (car _) -> y
;in the last case an internal name is generated for the overvar and used, it cannot
;be refferenced by the user except as _ in the by until and source expressions.
;     in addition, the by, untill, and source expresions have default values
;specified in terms of "_".  what they default to depends on the start-expression.
;if the start-expr is a number then the by defaults to "(1+ _)" the until defaults
;to "nil" which is to say the enumeration will not stop and the source defaults to
;"_". the following two specifications are the same:
;       over 1 by (1+ _) until nil _ -> i
;       over 1 -> i
;if the start-expression is not a number then the by defaults to "(cdr _)", the until
;defaults to "(null _)" and the source defaults to "(car _)" as a result the example
;prior to the last could be specified as:
;       over foo -> y

;this makes it very easy to specify the enumeration used by mapc, mapcar, and mapcan
;     the enumeration used by map, maplist, and mapcon can be abtained by specifying a
;overvar and no source var e.g.:
;       over x <- foo
;if neither an overvar or a source var is specified e.g.:
;       over foo
;then _ is used as the source var and an internally generated name is used as the
;overvar.  (note that the "_" used in the by, until, and
;source expresions has a local scope and therefor has nothing to do with the "_" used as
;a default for the source var).  clearly if there is more than
;one enumeration-specification then at most one of them can be allowed to generate "_"
;as a default source var or there will be trouble.
;
;consider now an accumulation-specification, it specifies how a result is accumulated and
;returned.  it has four parts
;       "select" select    ;which defaults to T (ie no selection)
;       "return" return    ;which defaults to nil or over-acc (if there is a step)
;       "step" step        ;which defaults to nil (ie no step)
;       "init" init        ;which defaults to a value of nil
;in writing the select, and step, all of the variables defined in the enumeration-
;specifications can be used (the init and return cannot refer to any source vars).  in addition
;there are several other special variables which can be referred to (they are defined only
;if they are reffered to somewhere).  these variables are
;  1. over-acc which is assigned the value init to start out and is assigned the result
;       of the step at each iteration.
;  2. over-body this can appear only in the step and only once!  the entire body of the over
;       (see below) is substituted for this in the step.
;  3. first-start this is assigned the initial value of the start expression of the first
;       enumeration-specification (it is usefull for sideffecting this start expression)
;before discussing how the parts of the accumulation specification interact it should be said
;that the body of a call on over is simply the remainder of the call after the above parts
;are matched at the front.  if the body consists of more than one sexpr it is wrapped in
;a progn.  they body can contain an explicit return in order to stop the iteration.
;if it does, it must also specify what is to be returned, the return expression only
;applies to the normal termination of the over.
;the best way to see how the accumulation specification works is through a generalized
;schema (the schema does not show how simple cases are optimized to promote efficiency)
;       (over ov <- start by (A _) until (B _) (C _) -> sv
;             select (D ov sv over-acc first-start)
;             return (E ov over-acc first-start)
;             step (F ov sv over-acc first-start over-body)
;             init (G ov first-start)
;             (H ov sv over-acc first-start))
;expands into:
;       (KBE-obsolete-loop-macro (first-start <- start
;              ov <- first-start
;              sv
;              over-acc <- (G ov first-start))
;         (if (B ov) (return (E ov over-acc first-start)))
;         (<- sv (C ov))
;         (if (D ov sv over-acc first-start)
;             (<- over-acc (F ov sv over-acc first-start
;                                 (H ov sv over-acc first-start))))
;         (<- ov (A ov)))

;the body says what to do on each iteration, the select says which iterations to do something
;on, the step says how to combine the results of a series of iterations, the init says what
;value to start with, and the return says what to return.  for example the following sums
;the numbers in a list foo:
;       (over foo -> x
;             select (numberp x)
;             return over-acc
;             step (+ over-acc over-body)
;             init 0
;         x)
;the return does not actually have to be specified because the return defaults to over-acc
;if there is a step and nil otherwise.
;
;in order to increase the convenience there is a way of quickly specifying what the return,
;step, and init are for a bunch of common cases.  this is by the use of a keyword.  the keyword
;is placed between the select and the return and specifies defaults for the return step and
;init.  a keyword is any atom which has a u1overstep property and optionaly a u1overreturn and
;u1overinit property.  this use of plists allows for the easy extension of the number
;of keywords.  it is expected that keywords will be used 95% of the time.  individual
;keywords already defined are discussed below where the defprops are done.  for example
;one of them "sum" allows you to accumulate a sum and enables you to write the above as:
;       (over foo -> x select (numberp x) sum x)
;
;finally to give a better feel for this construct consider a couple of comparison examples:
;       (mapcar #'+ l1 l2 l3)
;would be expressed as
;       (over l1 -> c1 over l2 -> c2 over l3 -> c3
;            list (+ c1 c2 c3))
;
;
;       (do ((l list (cdr l))
;            (k nil)
;            (i 1 (1+ i)))
;           ((null l) (nreverse k))
;           (setq k (cons (symbolconc (car l) i) k)))
;would be expressed as
;       (over list
;        for i <- 1
;        list (symbolconc _ i))
;
;
;       (memq x list)
;could be expressed as
;       (over l <- list -> item
;         (if (eq item x) (return l)))

(deffn over macro keep displace [! macrocall]
  (begin (e initpart bypart exitpart assignpart
          step return init body keyword select)
    (do ((overvar)
         (startexpr)
         (byexpr)
         (untilexpr)
         (sourceexpr)
         (assignableexpr))
        (nil)
      (if (not (match macrocall
                      ['over ! [overvar '<- ! e] '(<- _ e) ~ ! _ '(<- overvar nil)
                             startexpr
                             ! ['by byexpr ! e] '(<- _ e)
                               ~ ! _ '(<- byexpr (cond ((numberp startexpr)
                                                        '(1+ _))
                                                       (t '(cdr _))))
                             ! [_ (memq _ '(until untill)) untilexpr ! e]
                               '(<- _ e)
                               ~ ! _ '(<- untilexpr (cond ((numberp startexpr)
                                                           nil)
                                                          (t '(null _))))
                             ! [sourceexpr ! e '-> ! _] '(<- _ e)
                               ~ ! _ '(<- sourceexpr (cond ((numberp startexpr)
                                                            '_)
                                                           (t '(car _))))
                             ! ['-> assignableexpr ! e] '(<- _ e)
                               ~ ! _ '(<- assignableexpr (if (null overvar) '_))
                             ! macrocall]))
          (return nil))
      (if (null overvar)
          (setq overvar (symbolconc 'overvar assignableexpr)))
      (push overvar   initpart)
      (push '<-       initpart)
      (push startexpr initpart)
      (cond (untilexpr
             (push (u1oversubst overvar '_ untilexpr) exitpart)
             (when (and (not (atom overvar))
                        (not (equal (car exitpart) untilexpr)))
               (ferror "OVER: Non-atomic overvar in UNTIL expression:~
                        ~% data: ~S"
                       (car exitpart)))))
      (cond (byexpr (push overvar                              bypart)
                    (push (u1oversubst overvar '_ byexpr) bypart)))
      (do () (nil)
        (if (null assignableexpr) (return nil))
        (push assignableexpr initpart)
        (push assignableexpr assignpart)
        (push (u1oversubst overvar '_ sourceexpr) assignpart)
        (match macrocall
               [! [sourceexpr ! e '-> ! _] '(<- _ e)
                  ~ ! _ '(<- sourceexpr (cond ((numberp startexpr) '_)
                                              (t '(car _))))
                ! ['-> assignableexpr ! e] '(<- _ e)
                  ~ ! _ '(<- assignableexpr nil)
                ! macrocall])))
    (match macrocall
      [ ! ['select select ! e] '(<- _ e) ~ ! _ (<- select t)
       keyword (atom keyword)
               (<- step (get keyword 'u1overstep))
               '(<- return (get keyword 'u1overreturn))
               '(<- init (get keyword 'u1overinit))
        ~ ! _
       ! ['return return ! e] '(<- _ e) ~ ! _
       ! ['step step ! e] '(<- _ e)
                          '(if (not return) (<- return 'over-acc)) ~ ! _
       ! ['init init ! e] '(<- _ e) ~ ! _
       ! body])
    (setq body (cond ((cdr body) `(progn ,@body))
                     (t (car body))))
    (if step (setq body (u1oversubst body 'over-body step)))
    (cond ((u1overusedin 'over-acc [step return (cdr macrocall)])
           (setq body `(<- over-acc ,body))
           (push 'over-acc initpart)
           (cond (init (push '<-  initpart)
                       (push init initpart)))))
    (if (not (eq select t)) (<- body `(if ,select ,body)))
    (setq exitpart (cond ((cdr exitpart) `(or ,@(reverse exitpart)))
                         (t (car exitpart))))
    (setq initpart (reverse initpart))
    (if (u1overusedin 'first-start [step return (cdr macrocall) init])
        (setq initpart `(first-start       <- ,(third initpart)
                         ,(first initpart) <- first-start
                         ,@(cdddr initpart))))
    `(KBE-obsolete-loop-macro
          ,initpart
          ,@(if exitpart   `((if ,exitpart (return ,return))))
          ,@(if assignpart `((<- ,@(reverse assignpart))))
          ,@(if body       `(,body))
          ,@(if bypart     `((<- ,@(reverse bypart)))))))

(defun u1oversubst (new old list)
  (cond ((eq list old) new)
        ((or (atom list)
             (memq (car list) '(quote over for)))                 ;stop if hit quote or over
         list)
        (t (mapcar #'(lambda (x) (u1oversubst new old x))
                   list))))

(defun u1overusedin (item list)
  (cond ((eq list item) t)
        ((or (atom list)
             (memq (car list) '(quote over for)))
         nil)
        (t (do ((l list (cdr l)))
               ((atom l) nil)
               (cond ((u1overusedin item (car l)) (return t)))))))

;-----------------------------------------------------------------

(defmacro def-over-step (name code)
  `(defprop ,name ,code u1overstep))

(defmacro def-over-return (name code)
  `(defprop ,name ,code u1overreturn))

(defmacro def-over-init (name code)
  `(defprop ,name ,code u1overinit))

;;; LIST
;;;  This conses up a list of the results of the steps in the order they
;;;  are produced. (this is like MAPCAR, or MAPLIST)

(def-over-step   LIST (cons over-body over-acc))
(def-over-return LIST (nreverse over-acc))

;;; NLIST
;;;  This produces a list of the results of the steps but reuses the list
;;;  cells in the first argument.  the number of results must be the same
;;;  as the length of the first argument for this to work right.

(def-over-step   NLIST (cdr (rplaca over-acc over-body)))
(def-over-return NLIST first-start)
(def-over-init   NLIST first-start)

;;; REVLIST
;;;  This CONS's up a list like "list" above but it is faster because
;;;  it does not reverse the result in order to get the right order.

(def-over-step   REVLIST (cons over-body over-acc))
(def-over-return REVLIST over-acc)

;;; BAG
;;;  This is the same thing with a better name for it. It is used when
;;;  the order of the object return doesn't matter.

(def-over-step   BAG     (cons over-body over-acc))
(def-over-return BAG     over-acc)

;;; NCONC
;;;  This NCONC's the results of the steps together like MAPCAN or MAPCON.

(def-over-step   NCONC   (nreconc over-body over-acc))
(def-over-return NCONC   (nreverse over-acc))

;;; APPEND
;;;  This is like the above but does an APPEND.

(def-over-step   APPEND  (nconc (reverse over-body) over-acc))

;;; PDF pages 221-223
(def-over-return APPEND (nreverse over-acc))
;;; AND combines the results; FASTAND aborts on the first null step.
(def-over-step AND (and over-body over-acc))
(def-over-return AND over-acc)
(def-over-init AND t)
(def-over-step FASTAND (or over-body (return nil)))
(def-over-return FASTAND over-acc)
(def-over-init FASTAND t)
;;; OR combines the results; FASTOR aborts on the first non-null step.
(def-over-step OR (or over-body over-acc))
(def-over-return OR over-acc)
(def-over-step FASTOR (let ((u1u1**temp over-body))
                       (if u1u1**temp (return u1u1**temp))))
(def-over-return FASTOR nil)
(def-over-step SUM (+ over-body over-acc))
(def-over-return SUM over-acc)
(def-over-init SUM 0)
;;; CLASSIFYQ sorts (key . value) results into an ASSQ structure.
;;; Values in each bucket occur in reverse temporal order.
(def-over-step CLASSIFYQ
  (let ((pair over-body))
    (begin ([key ! obj] <- pair
            buck <- (assq key over-acc))
      (cond (buck (rplacd buck [obj ! (cdr buck)]) over-acc)
            (t [[key obj] ! over-acc])))))
(def-over-return CLASSIFYQ over-acc)
;;; CLASSIFY uses ASSOC instead of ASSQ.
(def-over-step CLASSIFY
  (let ((pair over-body))
    (begin ([key ! obj] <- pair
            buck <- (assoc key over-acc))
      (cond (buck (rplacd buck [obj ! (cdr buck)]) over-acc)
            (t [[key obj] ! over-acc])))))
(def-over-return CLASSIFY over-acc)
(def-over-step MAKEQSET (let ((item over-body))
                         (cond ((memq item over-acc) over-acc)
                               (t (cons item over-acc)))))
(def-over-return MAKEQSET over-acc)
(def-over-step MAKESET (let ((item over-body))
                        (cond ((member item over-acc) over-acc)
                              (t (cons item over-acc)))))
(def-over-return MAKESET over-acc)
(def-over-step MAKEFSET (cons over-body over-acc))
(def-over-return MAKEFSET (makefset over-acc))
;;; This alternative looping construct is a PROG, with no termination slot.
;;; Inits use LET* rather than parallel evaluation; <- selects BEGIN.
(defmacro KBE-obsolete-loop-macro (inits . body)
  (setq body `(do () (nil) ,@body))
  (cond (inits `(,(cond ((memq '<- inits) 'begin) (t 'let*))
                 ,inits ,body))
        (t body)))
