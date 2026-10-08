;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>REFER.LISP
;;; PDF pages 48-61 (printed source pages 1-14).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

(defmacro with-consistent-db (&body forms)
  (let ((startcontext  (gensym))
        (old-*this*    (gensym))
        (old-*that*    (gensym)))
    `(let ((,startcontext (cdr *db-context*) )
           (,old-*that* *that*)
           (,old-*this* *this*))
       (when-abnormal-exit (progn ,@forms)
         (cond (,startcontext (rdb ,startcontext))
               (t (db-init)))
         (setq *that* ,old-*that*)
         (setq *this* ,old-*this*)))))
;;; Actions that access the database need. to explicit take a lock
;;; on the database.
(define-vocabulary programmer-vocabulary)
(defmacro define-db-action (name bvl doc &body forms)
  `(define-action (programmer-vocabulary ,name) ,bvl ,doc
     (with-db-locked
       (error-restart (sys:error ',(text "Retry /"~A,../" command." name))
         (with-consistent-db
           ,@forms)))))
(defmacro define-db-interpretation (name bvl &body forms)
  `(define-interpretation (programmer-vocabulary ,name) ,bvl
     (with-db-locked
       (error-restart (sys:error ',(text "Retry /"~A,../" command." name) )
         (with-consistent-db
           ,@forms)))))
;;; PDF page 49

;;; Focus info
(defvar *that*       nil "Keeps track of the referent for 'that'.")
(defvar *this*       nil "Keeps track of the referent for 'this'.")
(defvar *here*       nil "A plist descriptor of here.")
(defvar *almost-this* nil "Like *this*, but not as exact.")
(defmacro bind-focus (loc &body forms)
 (let ((var (gensym)))
   `(let ((,var ,loc))
      (let ((*this*        (car    ,var))
            (*that*        (cadr    ,var))
            (*almost-this* (caddr  ,var))
            (*here*        (cadddr ,var)))
        (unwind-protect (progn ,@forms)
           (setf ,loc
                 (list *this* *that* *almost-this* *here*)))))))
(defun it ()
 (let ((that-ref (eval-command *that*))
       (this-ref (eval-command (or *this* *almost-this*))))
   (unless (equal this-ref that-ref)
     (complain "Ambiguous reference for 'it'. Use 'this' or 'that'."))
   this-ref))
(defun this () (eval-command *this*))
(defun that () (eval-command *that*))
(defun (it action) () (it))
(defun (this action) () (this))
(defun (that action) () (that))
(defun make-be-it (things)
 (setq *that* (make-ref (get-segcase (car things))))
 (setq *this* *that*)
 (setq *almost-this* *that*)
 (setq *here*
        (ignore-errors
         (let ((fn-name  (~01 ~name (??& (top-segcase
                                           (car (eval-command *that*)))
                                        @SEG  'lambda-exp))))
            (when fn-name
              `(:name ,fn-name
                :path ,(code->path fn-name (parsed->code *that*))))))))
(defun make-be-this (things &optional path position)
  (setq *this* nil *almost-this* nil)
  (when things
    (let ((fn (get-toplevel-name (car things))))
      ;;; Probably PATH->CODE and CODE->PARSED should not err here, but
      ;;; if they do, we want it to be a soft error because this
      ;;; focus-setting is generally not important since we are relatively
      ;;; error-tolerant about absence of focus anyway. ~kmp 5/12/84
      (setq *this* (ignore-errors (code->parsed (path->code fn path))))
      (setq *almost-this* *this*)
      (setq *here* position)
      (when (not *almost-this*)
        (do ((l (reverse path) (cdr l))) (nil)
          (let ((parsed (ignore-errors
                          (code->parsed (path->code fn (reverse l))))))
            (when parsed
              (setq *almost-this* parsed)
              (return)))
          (if (null l) (return)))))))
;;; Default references
;;; PDF page 50

(defun default-segcase ()
 (let ((result (eval-command (or *this* *almost-this*))))
   (cond (result (get-segcase (car result)))
         (t      nil))))
;;; Should this really be able to err or are all the functions that
;;; call it content with getting NIL (or something else) back if it
;;; fails. It was getting an ill-formed fact error in FIND-FACTS when
;;; doing (TOP-SEGCASE NIL), so I changed it to say something epsilon
;;; more intelligent. We should decide on a general strategy...
(defun (top action) ()
  (let ((sc (default-segcase)))
    (cond (sc (list (top-segcase sc)))
          (t (complain "No default context has been established.")))))
;;; PDF page 51

(define-db-action locate ((position (parse-name ':expression)))
                 "Verbally marks a shift of presentational focus to
                  <position>. Does not shift the presentational focus.
                  The <position> is an arbitrary plist containing data
                  about the position."
 (let ((data (eval-command position)))
   (let ((type (get (locf data) ':type))
         (name (get (locf data) ':name))
         (path (get (locf data)  ':path)))
     (let ((ref (or (get (locf data) ':reference)
                    (selectq type
                      ((define-cliche) (text "the cliche ~S" name) )
                      ((defun)         (text "the program ~S" name))
                      (otherwise nil)))))
        (when ref
          (let ((things
                  (ignore-errors
                    (eval-command
                       (parse-command ref #'parse-definite-reference)))))
            (when things (make-be-this things path data))))))
    (values)))
;;; PDF page 52

;this is here so that the parser will always be able to parse
;this even though it isn't actually in any stored cliche.
(*define-role 'mark)
;this takes in a segcase and makes it be a marked part of its
;top level plan segment.
(defun make-be-marked (segcase)
 (>& (top-segcase segcase) slot segcase  'mark *segment))
;this gets rid of all of the marks in a program.
(defun clear-marks (segcase)
  (--& (top-segcase segcase) slot * 'mark))
;;; PDF page 53

;;; (EVAL-COMMAND command)
;;; Special-purpose evaluator for parsed commands or command phrases.
(defun eval-command (command &aux fn)
 (cond ((or (memq command '(t nil)) (numberp command)) command)
       ((not (listp command) )
        (barf "A KBE command may not be ~A:~% ~S"
              (a-or-an (string-downcase (typep command) ))
              command) )
       ((eq (car command) 'quote) (cadr command) )
       ((eq (car command) 'name) command)
       ((setq fn (get (car command) 'verb-action))
         ;;; Actions are special forms now.
         ;;; They must eval their own args. --kmp 2/23/84
        (apply fn (cdr command)))
       ((setq fn (get (car command) 'action))
        (apply fn (mapcar #'eval-command (cdr command) )))
        (T
        (barf "The identifier ~S was unrecognized in a KBE command:~% ~S"
              (car command) command) )))
;;; PDF page 54

;;; The first part of this file deals with REFERENCE. It provides a
;;; number of mechanisms (roughly equivalent to noun phrases in English)
;;; the user can use to refer to parts of a plan. The intent is that the
;;; lisp expressions will read something like English if you don't
;;; scrutinize them too closely.
;;; Each of these returns a THING which is one of:
 ;;; a seg      -- a segcase with the case NIL
;;; a port     -- input, output, tinput, toutput
;;; There is a problem with representing the fact that sometimes you
;;; have a list of things, and sometimes you have just one. In order
;;; to keep this straight, almost every function takes and returns lists
;;; of things even if it demands or produces just a single item. This
;;; makes the representation uniform.
;;; A real problem here is that a THING is the actual thing from
;;; the data base. As a result it only stays accurate as long as the
;;; db doesn't change too much.
;;; In order to deal with this, we do the following: Whenever something
;;; is stored, instead of storing it, we store a path name, which is
;;; designed to be able to still refer to it even if every atom is
;;; renamed, and even if a fair amount of modification in the form
;;; of new segments is added.
;;; (pl? thing)
;;; Converts NIL or thing to NIL or (thing). It is used to make a
;;; list of things when you may or may not have a thing.
(defmacro pl? (thing)
  (once-only (thing)
    `(if ,thing (list ,thing))))
;utilities for working with names
(defmacro name-type (name)
  `(cadr ,name))
(defmacro name-symbol (name)
  `(caddr ,name))
(defun check-name (name)
  (if (not (and (listp name)
                (eq (car name) 'name)
                (listp (cdr name))
                (memq (name-type name)
                      '(cliche cliche-definition program
                        program-definition role variable segment) )
                (listp (cddr name) )
                (symbolp (name-symbol name))))
      (barf "~A bad name" name)))
;;; PDF page 55

;THE is used to refer to item(s).
;Note that it is important that THE never blows up because it is sometimes
;called on an expression that no longer refers to anything in order to
;find out if this is the case. Root-reference is used to specify
;root segments. Note tha the list of adjectives is only for euphony and
;;; ignored here.
(defun (root-reference action) (ignore name)
  (check-name name)
  (caseq (name-type name)
    (segment
     (if (memq (name-symbol name) *db-segs*)
         (ncons (& (name-symbol name)))))
    (program-definition
      (if (name-symbol name)
          (pl? (~01 ~segcase (??& * @seg 'lambda-exp (name-symbol name))))
          (let ((sc (default-segcase)))
            (if sc (list (top-segcase sc))))))
    (cliche-definition
      (if (name-symbol name)
          (pl? (~01 ~segcase (??& * cliche-def (name-symbol name))))
          (let ((sc (default-segcase)))
            (if sc (list (top-segcase sc))))))
    (T (complain "'The ~A' -- not a valid reference"
                 (unparse-command name)))))
(defun (relative-reference action)
       (ordinal adjectives plural? of-in superiors)
  (let* ((things (Rappend (relative-reference1 adjectives of-in
                                               (Elist superiors)))))
    (cond (plural? things)
          (ordinal (pl? (ordinal-select ordinal things)))
          ((null (cdr things)) things))))
(defun relative-reference1 (adjectives of-in superior)
  (let* ((( implemented? pgm cliche role) (the-adj-parse adjectives))
         (candidates
           (caseq of-in
             (of (if role
                     (~set ~slotseg (??& superior slot))
                     (~set ~subseg (??& superior subseg))))
             (in (Rlist (& (Elist (all-subsegs (~seg superior)))))))))
    (letS ((thing (Elist candidates)))
      (Rlist (Fselect thing
               (and (if (eq implemented? 'implemented)
                        (not (not-implemented? (~seg thing))) T)
                    (if (eq implemented? 'unimplemented)
                        (not-implemented? (~seg thing)) T)
                    (if pgm
                        (and (not (fact? thing))
                             (or (??& thing @seg 'funcall pgm)
                                 (??& thing @seg 'funcall
                                      (symbolconc "ADA-FN~" pgm))))
                        T)
                    (if cliche
                        (and (not (fact? thing))
                             (??& thing cliche cliche) )
                        T)
                    (if role
                        (if (eq role  '*)
                            (??& * slot thing *)
                            (??& * slot thing role))
                        T)))))))
(defun the-adj-parse (adjectives)
   (let ((( implemented? pgm cliche role) nil))
     (letS ((name (Elist adjectives) ))
;;; PDF page 56

     (cond ((memq name '( implemented unimplemented) )
            (setq implemented? name))
           ((eq name 'role) (setq role '*))
           (T (check-name name)
              (caseq (name-type name)
                (program (setq pgm (name-symbol name) ))
                (cliche (setq cliche (name-symbol name)))
                (role (setq role (name-symbol name) ))
                (T (complain "Bad adjective ~A in RELATIVE-REFERENCE"
                             (unparse-command name)))))))
   (list implemented? pgm cliche role)))
;;; PDF page 57

(defun all-subroles (segcase)
 (declare (values things))
 (let ((*result* '()))
   (declare (special *result*))
   (all-subroles-aux segcase)
   *result*))
(defun all-subroles-aux (thing)
 (declare (special *result*))
 (push thing *result*)
 (if (not (fact? thing))
     (Rignore (all-subroles-aux (~slotseg (Elist (??& thing slot)))))))
(defun (port-reference action) (ordinal adjectives plural? superiors)
 (let* ((things (Rappend (port-reference1 ordinal adjectives
                                    (Elist superiors)))))
   (cond (plural? things)
         ((null  (cdr things)) things))))
(defun port-reference1 (argno adjectives superior)
  (let* (((name key kind) (port-reference-adj-parse adjectives))
         (candidates
           (caseq key
             (input (??& superior input))
             (output (??& superior output))
             (tinput (??& superior tinput))
             (toutput (??& superior toutput))
             (temporal (append (??& superior tinput)
                               (??& superior toutput)))
             (non-temporal (append (??& superior input)
                                   (??& superior output)))
             (nil (append (??& superior input)
                          (??& superior output)
                          (??& superior tinput)
                          (??& superior toutput))))))
    (letS ((thing (Elist candidates))
           (@ioF (?& thing @io)))
      (Rlist (Fselect thing
               (and (if argno (eq argno (~argno @ioF)) T)
                    (if name (eq name (~name @ioF)) T)
                    (if kind (eq kind (~kind @ioF)) T)))))))
(defun port-reference-adj-parse (adjectives)
  (let* (name key kind temporal)
    (letS ((adj (Elist adjectives)))
      (caseq adj
        ((input output) (setq key adj))
        ((temporal non-temporal) (setq temporal adj))
        (port )
        (parameter (setq key 'input) (setq kind 'lambda-arg))
        (argument (setq key 'input) (setq kind 'actual-arg))
        (free-variable (setq kind 'free-var))
        (value (setq key 'output) (setq kind 'returnval))
        (side-effect (setq kind 'side-effect))
        (side-effected (setq kind 'side-effected) )
        (T (check-name adj)
           (if (not (eq (name-type adj) 'var))
               (complain "Bad adjective ~A in PORT-REFERENCE"
                         (unparse-command adj)))
           (setq name (name-symbol adj)))))
    (if (eq temporal 'temporal)
        (caseq key
          (input (setq key 'tinput))
          (output (setq key 'toutput))))
    (setq key (or key temporal))
    (list name key kind)))
;;; PDF page 58

;this implements reference-based on ordinal numbers (first, second,
;last, ...). This operates in a somewhat ad hoc way for each kind of
;thing that may have been found.
;there are only two basic situations where this works.
;1. if you have a set of subsegs, then this assumes that they are all
;  subsegs in the same program, and uses dflow. and cflow in that program
;;; to order them, before selecting one.
;2. if they are inputs with ARGNOs then these are used to order them.
;there is a hack in that what is actually used to order the things is
;the order of appearance in the code (taking account of the fact that
;arguments are evaluated before fns).  (Note that if two things have
;the same code, or are atoms then things won't work too well.) This a
;reasonable hack since it looks reasonable given what the user sees, and it
;disambiguates things a lot.  (It assumes that all of the things are in the
;same piece of code.)
;this is the key fn.  It takes a list of things.  It assumes that all of
;the things are in the same function.
;;; returns a cons of two things: the first is a list of those things which
;have code corresponding to them' in the function, sorted in the order they
;appear
; the second is a list of the other things in the order they originally
;were in.
(declare (special 2codes 2result))
(defun order-things (things)
  (cond ((null (cdr things)) [things])
        ((let* ((2codes (over things revlist [(corr-code _) ! _]))
                (start (car (corr-code (top-segcase
                                          (get-segcase (car things))))))
                (2result))
           (order-things1 start nil nil)
           [(nreverse 2result) ! (over 2codes select (not (eq (car _) T))
                                  revlist (cdr _))]))))
;this recursive program actually traverses a code tree looking for things.
;note that the decendants of a cell are ordered before it.
(defun order-things1 (cell car-parent cdr-parent)
  (cond ((not (atom cell))
         (order-things1 (car cell) cell nil)
         (order-things1 (cdr cell) nil cell)))
  (over 2codes select (not (memq (car _) '(T nil)))
     (cond ((cond ((eq (caar _) cell))
                  ((atom (caar _)) nil)
                  ((and (eq (caaar _) ' *code-is-car*)
                        (eq (cdaar _) car-parent)))
                  ((and (eq (caaar _) ' *code-is-cdr*)
                        (eq (cdaar _) cdr-parent))))
            (push (cdr _) 2result)
            (setf (car _) T)))))
;this finds the piece of code corresponding to a thing if there is one.
(defun corr-code (thing)
  (caseq (thing-type thing)
    ((seg segcase) (?@seg-code (~seg thing)))
    ((dflow cflow) (corr-code (~to thing)))
    ((input tinput output toutput)
     (or (?@io-code thing) (corr-code (~segcase thing))))))
;;; PDF page 59

;expects a list of things and gives you the numth one
(defun ordinal-select (num things)
  (cond ((null things) nil)
        ((null (cdr things)) (car things))
        (T (let* (((sort . others) (order-things things)))
             (cond ((> num (length things)) nil)
                   ((minusp num) (setq num (length things))))
             (cond ((not (> num (length sort))) (nth (1- num) sort))
                   ((null (cdr others)) (car others)))))))
;this is the inverse of the above it tells you the numeric position of
;a thing in a list of things
(defun ordinal-pos (thing things)
  (cond ((not (member thing things)) nil)
        ((null (cdr things)) 1)
        (T (let* (((sort . others) (order-things things))
                  (spot (member thing sort)))
             (cond ((and spot
                         (null (cdr spot))
                         (null others)
                         (> (length things) 2))
                    -1)
                   (spot (- (length things)
                            (length (cdr spot))))
                   ((null (cdr others)) -1))))))
;this takes in a list of things and returns it sorted in terms of
;there code order,  (This actually tries to deal with things that
;correspond not at all or ambiguously to the code.)
(defun ordinal-sort (things)
  (let (((sort . others) (order-things things)))
    (nconc sort others)))
;;; PDF page 60

;this is the inverse fn of the reference expression THE.
;the goal here is to get an unambiguous reference expression that will
;point to the thing in question.  there are two difficulties here
;which work against each other.  You want the result to be reasonably
;aesthetic, because the user sees these expressions printed out to him,
;and you want it to be unambigous.  If you specify that you want a
;short result, then it tries to be less verbous.
(defun make-ref (thing &optional (short nil))
  (or (make-ref-slot thing short)
      (caseq (thing-type thing)
        (SEG     (make-ref-seg thing short))
        (INPUT    (make-ref-input thing))
        (OUTPUT  (make-ref-output thing))
        (TINPUT  (make-ref-tinput thing))
        (TOUTPUT (make-ref-toutput thing)))))
;this computes the basic type of a thing (the keyword or SEG)
(defun thing-type (thing)
  (cond ((and (fact? thing)
              (memq (~keyword thing) '(input output tinput toutput)))
         (~keyword thing))
        ((and (listp thing)
              (memq (~seg thing) *db-segs*)
              (null (~case thing)))
         'seg)
        (T (barf "THING-TYPE: not a valid thing.~% Thing: ~:="  thing))))
(defun make-ref-slot (thing &optional  (short T))
  (let ((slotF (~01 (??& * slot thing))))
    (if slotF
        (let* ((others (~set ~slotseg  (??& (~segcase slotF) slot
                                         * (~slotname slotF))))
               (ordinal (if (cdr others)  (ordinal-pos thing others))))
           `(relative-reference ,ordinal
                                '(,@(make-ref-extra
                                      thing (or short ordinal))
                                  (name role ,(~slotname slotF)))
                               nil  'of ,(make-ref (~segcase slotF) T))))))

(defvar *internal-names*  '(Svariable Snoop zeroary-function unary-function
                            binary-function simple-subseg)
  "List of  'function' and 'cliche' names that we don't want user to see.")
(defun make-ref-extra  (thing short)
  (if (and  (not short) (not (fact? thing)))
      (let*  ((@seg (~01 (??& thing @seg)))
              (fn-name (if (and @seg (eq (~kind @seg) 'funcall))
                           (~name @seg)))
              (cliche-name (~01 ~clichename (??& thing cliche))))
        (cond  ((and cliche-name
                     (not (memq cliche-name *internal-names*) ))
                `((name cliche ,cliche-name)))
               ((and fn-name
                     (not (memq fn-name *internal-names*) ))
                `((name program ,fn-name)))))))
;;; PDF page 61

(defun make-ref-seg (thing &optional (short T))
  (let* ((cliche-def-name (~01 ~cliche-def-name (??& thing cliche-def)))
         (@seg (~01 (??& thing @seg))))
    (cond (cliche-def-name
           `(root-reference ',(make-ref-extra thing short)
                            (name cliche-definition  ,cliche-def-name) ))
          ((and @seg (eq (~kind @seg) 'lambda-exp))
           `(root-reference ',(make-ref-extra thing short)
                            (name program-definition  ,(~name @seg))))
          (T `(root-reference nil (name segment  ,(~seg thing)))))))
(defun make-ref-input (thing)
  (let* ((@io (?& thing @io))
         (segcase (~segcase thing))
         (seg-ref (make-ref segcase T)))
    (caseq (~kind @io)
      (free-var `(port-reference nil  '((name variable ,(~name @io))
                                 free-variable  input)
                           nil  ,seg-ref))
      (lambda-arg `(port-reference nil  '((name variable ,(~name @io))
                                          parameter)
                             nil  ,seg-ref))
      (actual-arg `(port-reference  ,(if (cdr (??& (&* segcase input) @io
                                             'actual-arg))
                                   (~argno @io))
                              '(argument) nil ,seg-ref)))))
(defun make-ref-output  (thing)
  (let* ((@io (?& thing @io))
         (seg-ref (make-ref  (~segcase thing) T)))
    (caseq (~kind @io)
      (free-var  `(port-reference nil '((name variable ,(~name @io))
                                  free-variable output)
                            nil ,seg-ref))
      (side-effect  `(port-reference nil '((name variable ,(~name @io))
                                     side-effect output)
                            nil ,seg-ref))
      (returnval  `(port-reference nil '(value) nil ,seg-ref)))))
(defun make-ref-tinput  (thing)
   (let* ((@io (?& thing @io))
          (seg-ref (make-ref (~segcase thing) T)))
     `(port-reference nil '(,@(if (~name @io)
                                  `((name variable ,(~name @io))))
                      temporal input)
                nil ,seg-ref)))
(defun make-ref-toutput (thing)
   (let* ((@io (?& thing @io))
          (seg-ref (make-ref (~segcase thing) T)))
     `(port-reference nil '(,@(if (~name @io)
                                  `((name variable ,(~name @io))))
                      temporal output)
                nil ,seg-ref)))
