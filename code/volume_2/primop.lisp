;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_2.pdf
;;; PDF pages 28-42; listing primop.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;this operates like an evaluator, making all of the surface plan
;except for data flow.  In leu of data flow, DF numbers are
;associated with each port.  These are later used
;as unambiguous variable names governing the data flow.

;[note it assumes that the piece of code is not a split, this
;fits with the fact that in lisp there is no way to write a naked split.]

;there are several special variables which it uses to keep track of things
;while it works.
;DEFINING-NAME -- the name of the fn currently being defined this is used
;  to detect recursive calls.
;RE-FLOW-EVAL -- this is used as a hack when you are flow-evaluating a
;   recursive fn for the second time, in order to keep your hands
;   on the definition from the prior time.
;INLINE-LIST -- this is used by define-consolidated in order to tell
;   6instantiate-funcall what fns to include inline.
;UNGROUP-LIST -- used when consolidating things to keep track of what to
;   ungroup before returning the result.
;DEFINING-CODE -- this holds the actual code
;  which is the original definition.  it is used to decide what is good to
;  put in a code field, and what is not.
;LASTSEGCASE -- this holds the segcase which immediately precedes (in clfow)
;  the next seg to be evaluated.
;SUPERSEGCASE -- the segcase of the top seg being defined
;ALIST -- this is a normal alist that links each var name with a df-num
;  It grows and shrinks as variables are bound and unbound.
;DF-NUM -- this counts up continuously recording what the largest df-num
;  used so far is.  (note that these numbers are actually bit strings with
;  only one 1 in them that shifts as it counts up).
;DF-NUMS -- a list of all the df-nums created (this is actually bound
;  above in 6flow-analyze, because 6data-flow uses it).
;DF-NAME -- this is an alist, which shows what variable is carrying the
;  dflow indicated by the dfnum.  it is often just a guess, but better
;  than nothing.  it is also used by 6data-flow.
;TAGS -- this is an alist linking each tag with the name of the join
;  corresponding to it. (note that it grows and shrinks like a stack
;  as you enter and leave calls on K*PROG where tags are defined and redefined.)
;  Also note that any join that is only branched to once, and all inaccessable
;  code will be removed from the program in this pass.
;RETVAR -- this is used purely in order to get more mnemonic
;  names for output objects.  it is set by k*psetq 6fexpr in order to
;  get information to 6instantiate-funcall and k*quote 6fexpr.

;must be nil when 6flow-analyze called by itself.
(setq DEFINING-CODE nil DEFINING-NAME nil)

(def-alistq ALIST)
(def-alistq TAGS)
(def-alistq DF-NAME)

;these fns implement operations with dfnums
;the first generates them

(defun 6df-num ()
  (let* ((new-bits (lsh (car DF-NUM) 1)))
    (push (reverse DF-NUM) DF-NUMS)
    (rplaca DF-NUM new-bits)
    (if (zerop new-bits) (push 1 DF-NUM))
    (top DF-NUMS)))

;this links a port and a dfnum, returning the dfnum

(defun 6+df (port dfn)
  (if (null dfn) (barf "6+DF: Null dfn~% Port: ~:=" port))
  (putprop (~seg~segcase port)
           [[(~obj port) ! dfn] ! (get (~seg~segcase port) 'df)]
           'df)
  (push (cons dfn port) 6DFN-PORT-LIST)
  dfn)

;this gets a port from a dfn

(defun 6dfn->port (dfn)
  (cdr (assoc dfn 6DFN-PORT-LIST)))

;this retrieves a dfnum for a port

(defun 6?df (port)
  (cdr (assq (~obj port) (get (~seg~segcase port) 'df))))

;this records a guess at a variable name.  It returns the df-num

(defun 6df-name (dfn name)
  (setf (DF-NAME dfn) name)
  dfn)

;this is a very typical combination

(defun 6df (port name)
  (6+df port (6df-name (6df-num) name)))


;these are just to help debugging.  the first takes in a bit string,
;and returns a list of the bit positions on in it.

(defun 6d (list)
  (over DF-NUMS -> dfn
   over i <- (1- (length df-nums)) by (1- i) until nil
   select (6set^? dfn list) bag i))

(defun 6dfns ()
  (sortcar
    (over *db-segs* -> s
      nconc (over (get s 'df) -> [obj ! dfn]
              list [['/[ s obj] ! (or (6d dfn) ['none])]))
    #'(lambda (x y)
        (cond ((eq (car x) (car y)) (alphalessp (cadr x) (cadr y)))
              (t (alphalessp (car x) (car y)))))))

(defun 6flow-eval (code)
  (let* ((superseg (genseg (or MNEMONIC-NAME 'seg)))
         (SUPERSEGCASE (& superseg))
         (LASTSEGCASE SUPERSEGCASE)
         (DF-NUM [1])
         (ALIST [nil])
         TAGS RETVAR returnval)
    (+& SUPERSEGCASE segtype 'simple)
    (+& SUPERSEGCASE @seg 'opencode nil nil)
    (+& (+& SUPERSEGCASE output nil (genobj superseg 'ret) nil)
        @io 'returnval nil nil)
    (setq returnval (6eval code))
    (+& SUPERSEGCASE cflow LASTSEGCASE SUPERSEGCASE)
    (let ((ret (car (??& (&* SUPERSEGCASE output) @io 'returnval))))
      (if (and returnval ret) (6+df (~deep ret) returnval)))
    (over (??subseg (~seg SUPERSEGCASE)) -> subseg
          (??& SUPERSEGCASE cflow * (&* subseg)) -> cflow-in
      (cond ((null cflow-in)
             (barf "6FLOW-EVAL found inaccessible code.~% Seg: ~S"
                   subseg))
            ((eq (?segtype subseg) 'join)
         (cond ((null (cdr cflow-in))
                (make-fact-be-out (car cflow-in))
                (assert-fact (&~from (-& SUPERSEGCASE cflow (&* subseg))
                                     (~from (car cflow-in))))
                (kill-seg subseg))
               (t
                (let ((next
                        (~seg (~to (?& SUPERSEGCASE cflow (&* subseg))))))
                  (if (eq (?segtype next) 'join)
                      (fuse-joins subseg next))))))))
    SUPERSEGCASE))

;before we go on, a word about the way free variables are handled.
;there is no easy way to look at the code, and tell what free
;varaibles are used.  (A particular problem is that some fn it calls
;may use a free variable that is never mentioned in the program
;itself).  Therefore, 6eval starts out assuming there aren't any, and
;then whenever it encouters one, it adds a corresponding input to the
;superseg, and makes an entry on the alist for it.  NOTE that no
;distinction is made based on whether the free var is an input or an
;output or both at this time, this is all straightend out in
;6create-super-outputs.  (the NIL that the alist starts out as is there
;so that there will always be some alist to add the new var to.)  the
;fact that a var is free is detected simply by the fact that it is not
;on the alist.  (all this work happens in the fn 6symeval.)

(defun 6symeval (v)
  (or (ALIST v)
      (let ((in (+& SUPERSEGCASE input nil
                    (genobj (~seg SUPERSEGCASE) v) nil)))
        (+& in @io 'free-var v nil)
        (nconc ALIST [[v ! (6df in v)]])
        (ALIST v))))

;this is the part that actually looks like an evaluator.  the
;real guts of it are in the 6fexpr defuns which tell you exactly
;what it really does with each 6fexpr.

(defun 6eval (form)
  (let (fexpr)
    (cond ((self-evalling-form form)
           (funcall (get 'k*quote '6fexpr) `',form))
          ((symbolp form) (6symeval form))
          ((setq fexpr (get (car form) '6fexpr))
           (funcall fexpr form))
          (t (6apply (let (RETVAR) (mapcar #'6eval (cdr form)))
                     (6instantiate-funcall form))))))

;the funny order of args to 6apply makes it simpler to get the cflow right.
;note that nothing at all is done about side-effect outputs here.

(defun 6apply (argvals seg)
  (cond (LASTSEGCASE
         (+& SUPERSEGCASE subseg (& seg))
         (+& SUPERSEGCASE cflow LASTSEGCASE (& seg))
         (setq LASTSEGCASE (& seg))
         (dolist (@io (??& (&* (&* seg) port) @io 'actual-arg))
           (6+df (~deep @io) (nth (1- (~argno @io)) argvals)))
         (dolist (@io (??& (&* (&* seg) port) @io 'free-var))
           (6+df (~deep @io) (6symeval (~name @io))))
         (dolist (@io (??& (&* (&* seg) port) @io 'side-effect))
           (6+df (~deep @io)
                 (6?df
                   (?& (~segcase (~deep @io)) input * (~id (~deep @io))))))
         (let* ((ret (car (??& (&* (&* seg) output) @io 'returnval))))
           (cond (ret
                  (if RETVAR (+& (~deep ret) @createvar RETVAR))
                  (6df (~deep ret) RETVAR)))))
        (t (kill-seg-and-its-subsegs seg)))) ;kill inexcutable code immediately!

;this creates a segment for a constant datum.  it takes the whole
;quoted expression as an argument.

(defun (k*quote 6fexpr) (item)
  (let* ((segcase (& (genseg
                       (cond ((or (eq (car item) 'cobol-string)
                                  (eq (car item) 'string) ;to be deleted
                                  (symbolp (cadr item))
                                  (numberp (cadr item)))
                              (intern-KBE
                                (format nil "<~a>" (cadr item))))
                             (t 'c)))))
         (obj (genobj (~seg segcase) (or RETVAR 'val))))
    ;; kludge to keep K*QUOTE from getting out of this module.
    ;; There should be a more general way to do this. eg, STRING
    ;; should be K*STRING and we should have to turn it back to STRING.
    ;; Should be data-driven somehow ... --kmp
    (let* ((item-copy (copytree item)))
      (if (eq (car item-copy) 'K*QUOTE) (rplaca item-copy 'QUOTE))
      (+& segcase @seg 'constant item-copy (6good-code item)))
    (+& segcase segtype 'simple)
    (+& segcase plantype 'constant)
    (+& (+& segcase output nil obj nil)
        @io 'returnval nil nil)
    (6apply nil (~seg segcase))))

;this is used by COBOL
(putprop 'cobol-string (get 'k*quote '6fexpr) '6fexpr)
(putprop 'string (get 'k*quote '6fexpr) '6fexpr) ;to be deleted

;note that segments are created for k*psetqs here, and then removed later.
;note that the expansion of (cond (foo (go bar))) leads to a meaningless
;k*psetq that is ignored here.

(defun (k*psetq 6fexpr) (code)
  (6build-k*psetq
    (over [var val ! rest] <- (cdr code) by rest until (null var)
          (let ((RETVAR var)) (6eval val)) -> dfn
      select dfn
      bag [var ! dfn])))

(defun 6build-k*psetq (pairs)
  (if pairs
      (let* ((seg (genseg 'k*psetq))
             (segcase (& seg)))
        (+& segcase @seg 'funcall 'k*psetq nil)
        (+& segcase segtype 'simple)
        (over pairs -> [var ! dfn]
         over i <- 1 by (1+ 1) until nil
              (+& segcase input nil (genobj seg var) nil) -> in
              (+& segcase output 'id (genobj seg var) (~obj in)) -> out
          (6+df in dfn)
          (6+df out (6symeval var)))
        (6apply (mapcar #'cdr pairs) (~seg segcase))
        (cdar (last pairs))))) ;the returnval

;this is for slot stuff.  Note we take advantage of the
;fact that we always have a segment in order to just
;assert the slot info

(defvar *SUGGESTED-NAME-FROM-SLOT* nil
  "used to pass a suggested name to 6instantiate-funcall")

(defun (:property k*slot 6fexpr) (call)
  (let* (((form slotname slottype) (cdr call))
         (*SUGGESTED-NAME-FROM-SLOT* slotname)
         (args (Rlist (6eval (filterS #'(lambda (x) (not (is-&in-out x)))
                                     (Elist (cdr form))))))
         (seg (6instantiate-funcall form)))
    (+& SUPERSEGCASE slot (& seg) slotname slottype)
    (6apply args seg)))
(defun is-&in-out (x) (and (symbolp x) (string-equal x "&in-out")))

;;; PDF page 34
;;; Instantiate an expr/lexpr prototype, annotating free variable ports.
(defun 6instantiate-funcall (form)
  (let* (((fn . args) form) tmp
         (proto-seg
           (cond ((eq fn nil)
                  (prog1 (6fast-proto *SUGGESTED-NAME-FROM-SLOT* args)
                    (setq args (Rlist (filterS #'(lambda (x) (not (is-&in-out x)))
                                               (Elist args))))))
                 ((and (boundp 'INLINE-LIST) (memq fn INLINE-LIST))
                  (copyseg (?lambda-exp fn) fn))
                 ((and (eq fn DEFINING-NAME) RE-FLOW-EVAL)
                  (6copy-top-seg RE-FLOW-EVAL fn))
                 ((setq tmp (?lambda-exp fn)) (6copy-top-seg tmp fn))
                 ((setq tmp (get fn '6proto-seg)) (funcall tmp))
                 (t (when (not (or (fboundp fn) (memq fn SIMPLE-EXPRS)))
                      (warn "~S isn't a known function." fn))
                    (6fast-proto fn args)))))
    (--& (&* proto-seg) @seg)
    (--& (&* proto-seg) plantype)
    (cond ((and DEFINING-NAME (eq fn DEFINING-NAME))
           (+& SUPERSEGCASE recursive (& proto-seg))
           (+& (& proto-seg) @seg 'recursion nil nil))
          ((not (and (boundp 'INLINE-LIST) (memq fn INLINE-LIST)))
           (+& (& proto-seg) plantype 'new-view)
           (+& (& proto-seg) @seg 'funcall fn (6good-code form))))
    (over (??& (&* (&* proto-seg) input) @io 'lambda-arg) -> lexpr@io
      select (minusp (~argno lexpr@io))
      (make-fact-be-out lexpr@io)
      (make-fact-be-out (~deep lexpr@io))
      (over i <- (- (~argno lexpr@io)) by (1+ i) until nil
        over (nthcdr (1- (- (~argno lexpr@io))) args)
        (+& (+& (& proto-seg) input nil
                (genobj proto-seg (~obj (~deep lexpr@io))) nil)
            @io 'lambda-arg nil i)))
    (over (??& (&* (&* proto-seg) output) @io 'lambda-arg) -> lexpr@io
      select (minusp (~argno lexpr@io))
      (make-fact-be-out lexpr@io)
      (make-fact-be-out (~deep lexpr@io))
      (over i <- (- (~argno lexpr@io)) by (1+ i) until nil
        over (nthcdr (1- (- (~argno lexpr@io))) args)
        (+& (+& (& proto-seg) output nil
                (genobj proto-seg (~obj (~deep lexpr@io))) nil)
            @io 'lambda-arg nil i)))
    (over (--& (&* (&* proto-seg) port) @io 'lambda-arg) -> @io
          (nth (1- (~argno @io)) args) -> arg
          (and (symbolp arg) (not (eq arg T)) arg) -> var-arg
      (+& (~deep @io) @io 'actual-arg var-arg (~argno @io)))
    proto-seg))

;;; PDF page 35
;;; Copy top-level specs for a funcall new-view; not joins or TCs.
(defun 6copy-top-seg (seg root)
  (let* ((newseg (genseg root)) changes)
    (over (??& (&* seg) input) -> in
          (+& (& newseg (~case (~segcase in))) input nil
              (genobj newseg (~obj in)) nil) -> new
      (push (cons (~obj in) (~obj new)) changes)
      (over (??& in @io) (+& new @io (~kind _) (~name _) (~argno _))))
    (over (??& (&* seg) output) -> out
          (+& (& newseg (~case (~segcase out))) output (~kind out)
              (genobj newseg (~obj out)) (cdr (assq changes (~id out)))) -> new
      (push (cons (~obj out) (~obj new)) changes)
      (over (??& out @io) (+& new @io (~kind _) (~name _) (~argno _))))
    (over (??& (&* seg) segtype)
      (+& (& newseg (~case (~segcase _))) segtype (~type _)))
    (over (??& (&* seg) assert)
      (+& (& newseg (~case (~segcase _))) assert (sublis changes (~wff _))))
    (over (??& (&* seg) when)
      (+& (& newseg (~case (~segcase _))) when (sublis changes (~wff _))))
    newseg))
(defvar 6fast-proto-names)
(defun 6fast-proto (fn arg-list)
  (let* ((args arg-list) (ins) (outs (ncons (or RETVAR 'z)))
         (6fast-proto-names outs))
    (prog (arg var is-in-out)
     L (cond ((null args) (return nil)))
       (setq arg (pop args) is-in-out nil)
       (cond ((is-&in-out arg) (setq is-in-out T arg (pop args))))
       (setq var (6fast-proto-name arg arg-list))
       (push var ins)
       (if is-in-out
           (push [(6fast-proto-name nil arg-list) 'side-effect var] outs))
       (go L))
    (6fast-defspecs `(,fn ,(nreverse outs) . ,(nreverse ins)))))
(defun 6fast-proto-name (arg args)
  (cond ((and arg (symbolp arg) (not (eq arg t))
              (not (memq arg 6fast-proto-names)))
         (push arg 6fast-proto-names) arg)
        (T (lets ((let (Elist '(a b c d e f g h i j k l m n o p q r s t))))
             (cond ((and (not (memq let 6fast-proto-names)) (not (memq let args)))
                    (push let 6fast-proto-names) (done let)))))))

;;; PDF page 36
;;; Fast prototype specs: (name outputs . inputs). Outputs may be
;;; (name PART input) or (name SIDE-EFFECT input). Atomic tails specify lexprs.
(defun 6fast-defspecs (specs)
  (let* (((name outs . ins) specs) (seg (genseg name)) (num 0) intrn)
    (+& (& seg) segtype 'simple)
    (+& (& seg) plantype 'builtin)
    (+& (& seg) @seg 'lambda-exp name nil)
    (over list <- ins until nil
      (setq num (1+ num))
      (cond ((null list) (return))
            ((atom list)
             (let ((in (genobj seg list)))
               (push (cons list in) intrn)
               (+& (+& (& seg) input nil in nil) @io 'lambda-arg list (- num)))
             (return)))
      (let ((in (genobj seg (car list))))
        (push (cons (car list) in) intrn)
        (+& (+& (& seg) input nil in nil) @io 'lambda-arg (car list) num)))
    (over list <- outs
      (cond ((null list) (return))
            ((atom list)
             (+& (+& (& seg) output nil (genobj seg list) nil)
                 @io 'lambda-arg list (- num))
             (return)))
      (cond ((atom (car list))
             (+& (+& (& seg) output nil (genobj seg (car list)) nil)
                 @io 'returnval nil nil))
            ((eq (cadr (car list)) 'part)
             (+& (+& (& seg) output 'part (genobj seg (caar list))
                     (cdr (assq (caddar list) intrn))) @io 'returnval nil nil))
            ((eq (cadr (car list)) 'side-effect)
             (+& (+& (& seg) output 'id (genobj seg (caar list))
                     (cdr (assq (caddar list) intrn))) @io 'side-effect nil nil))))
    seg))

;;; PDF pages 37-38
(setq SIMPLE-EXPRS
  '(make-array maknum *dummy-expr*
    ;for cobol you would need these too.
    ;eofp invalid-keyp alphanumericp numericp alphabeticp concatenate
    ;current-date time-of-day day date
    ;for fortran you would need these too.
    ;_+i _-i _*i _//i _**i _u-i _+r _-r _*r _//r _**r _u-r _**ri
    ;_+d _-d _*d _//d _**d _u-d _**di _itor _itod _rtod _dtor _rtoi
    ;_dtoi _exp _dexp _alog _dlog _sin _dsin _cos _dcos _sqrt _dsqrt
    ;_iabs _abs _dabs _float _dfloat _sngl _dble _make-array
    ))
(defun (k*setf 6proto-seg) ()
  (6fast-defspecs '(k*setf ((out side-effect in)) access-list in value)))
(defun (5noop 6proto-seg) () (6fast-defspecs '(5noop ((out part in)) in)))
(defun (car 6proto-seg) () (6fast-defspecs '(car ((obj part list)) list)))
(defun (cdr 6proto-seg) () (6fast-defspecs '(cdr ((obj part list)) list)))
(defun (cddr 6proto-seg) () (6fast-defspecs '(cddr ((obj part list)) list)))
(defun (caar 6proto-seg) () (6fast-defspecs '(caar ((obj part list)) list)))
(defun (cadr 6proto-seg) () (6fast-defspecs '(cadr ((obj part list)) list)))
(defun (cdar 6proto-seg) () (6fast-defspecs '(cdar ((obj part list)) list)))
(defun (assoc 6proto-seg) () (6fast-defspecs '(assoc ((res part list)) obj list)))
(defun (rplacd 6proto-seg) ()
  (6fast-defspecs '(rplacd (z (cell side-effect cons)) cons obj)))
(defun (rplaca 6proto-seg) ()
  (6fast-defspecs '(rplaca (z (cell side-effect cons)) cons obj)))
(defun (arraystore 6proto-seg) ()
  (6fast-defspecs '(arraystore (z (arrayout side-effect array)) array index item)))
(defun (format 6proto-seg) ()
  (6fast-defspecs '(format (z (file-out side-effect file)) file string . arg)))
(defun (arrayfetch 6proto-seg) ()
  (6fast-defspecs '(arrayfetch ((item part array)) array index)))
(defun (aset 6proto-seg) ()
  (6fast-defspecs '(aset (z (arrayout side-effect array)) obj array . idx)))
(defun (aref 6proto-seg) ()
  (6fast-defspecs '(aref ((item part array)) array . idx)))
(defun (non-null 6proto-seg) ()
  (~seg (plan-def non-null nil
          (deffacts non-null
            (plantype (builtin)) (segtype (split)) (@seg (lambda-exp non-null))
            (input (nil obj nil)) (@io (obj lambda-arg obj 1))
            non-null (when (obj)) ;note interaction with 5get-lisp-pred
            null (when ((not obj)))))))
(defun (k*infinite-loop-pseudo-exit 6proto-seg) ()
  (~seg (plan-def k*infinite-loop-pseudo-exit nil
          (deffacts k*infinite-loop-pseudo-exit
            (plantype (builtin)) (segtype (split))
            (@seg (lambda-exp k*infinite-loop-pseudo-exit))
            never (when (T)) always (when ((not T)))))))

;;; PDF page 39
;Similar definitions for FORTRAN.
(comment
  (defun (_lt 6proto-seg) ()
    (~seg (plan-def lt nil
            (deffacts _lt (plantype (builtin)) (segtype (split))
              (@seg (lambda-exp _lt)) (input (nil obj nil))
              (@io (obj lambda-arg obj 1))
              yes (when ((minusp obj))) no (when ((not (minusp obj))))))))
  (defun (_eq 6proto-seg) ()
    (~seg (plan-def eq nil
            (deffacts _eq (plantype (builtin)) (segtype (split))
              (@seg (lambda-exp _eq)) (input (nil obj nil))
              (@io (obj lambda-arg obj 1))
              yes (when ((zerop obj))) no (when ((not (zerop obj))))))))
  (defun (_gt 6proto-seg) ()
    (~seg (plan-def gt nil
            (deffacts _gt (plantype (builtin)) (segtype (split))
              (@seg (lambda-exp _gt)) (input (nil obj nil))
              (@io (obj lambda-arg obj 1))
              yes (when ((plusp obj))) no (when ((not (plusp obj)))))))))
;COBOL parser prototypes also require the Lisp prototypes above.
(defun (substring 6proto-seg) () (6fast-defspecs '(substring ((z part x)) x i j)))
(defun (Cread 6proto-seg) ()
  (6fast-defspecs '(Cread ((side side-effect file) . item) file)))
(defun (Cwrite 6proto-seg) ()
  (6fast-defspecs '(Cwrite ((outfile side-effect infile)) infile . obj)))
(defun (Crewrite 6proto-seg) ()
  (6fast-defspecs '(Crewrite ((outfile side-effect infile)) infile . obj)))
(defun (Copeni 6proto-seg) ()
  (6fast-defspecs '(Copeni ((file side-effect infile)) infile . flag)))
(defun (Copeno 6proto-seg) ()
  (6fast-defspecs '(Copeno ((file side-effect infile)) infile . flag)))
(defun (Copenio 6proto-seg) ()
  (6fast-defspecs '(Copenio ((file side-effect infile)) infile)))
(defun (Cclose 6proto-seg) ()
  (6fast-defspecs '(Cclose ((outfile side-effect infile)) infile . flag)))
(defun (skip 6proto-seg) ()
  (6fast-defspecs '(skip to-channel ((outfile side-effect infile)) num infile)))
(defun (nterpri 6proto-seg) ()
  (6fast-defspecs '(nterpri ((outfile side-effect infile)) infile num)))

;;; PDF page 40
;;; Override flow-eval assertions to make the outermost segment a lambda-exp.
(defun (k*lambda 6fexpr) (form)
  (let* (((return? args body) (cdr form)))
    (-& SUPERSEGCASE @seg)
    (+& SUPERSEGCASE @seg 'lambda-exp DEFINING-NAME (6good-code DEFINING-CODE))
    (if (null return?) ;eg for fortran and cobol
        (let ((ret-out (~deep (-& (&* SUPERSEGCASE output) @io 'returnval))))
          (make-fact-be-out ret-out)))
    (over args -> arg over i <- 1 by (1+ i)
          (+& SUPERSEGCASE input nil (genobj (~seg SUPERSEGCASE) arg) nil) -> in
      (+& in @io 'lambda-arg arg i)
      (push [arg ! (6df in arg)] ALIST))
    (6eval body)))
;;; Binding and statement sequencing; new bindings create k*psetq segments.
(defun (k*prog 6fexpr) (form)
  (let (((inits . body) (cdr form)))
    (let ((ALIST ALIST) (TAGS TAGS) ;bound so can pop out again
          (pairs (over inits -> [var expr]
                   bag [var ! (let ((RETVAR var)) (6eval expr))])))
      (over pairs -> [var ! _] (push [var ! (6df-name (6df-num) var)] ALIST))
      (6build-k*psetq pairs)
      (over body -> line select (and (not (atom line)) (eq (car line) 'k*tag))
        (let* ((joincase (& (genseg 'join))))
          (+& joincase segtype 'join)
          (+& joincase plantype 'join)
          (+& joincase @seg 'join (cadr line) nil)
          (+& SUPERSEGCASE subseg joincase)
          (push [(cadr line) ! joincase] TAGS)))
      (over b <- body -> sexpr
        (cond ((not (null (cdr b))) (let (RETVAR) (6eval sexpr)))
              ((cdr sexpr) (barf "Multiple returns not supported yet."))
              (sexpr (return (6eval (car sexpr)))) ;one return val
              (t (return nil)))))))

;;; PDF page 41
(defun (k*go 6fexpr) (form)
  (if LASTSEGCASE
      (+& SUPERSEGCASE cflow LASTSEGCASE
          (& (~seg (TAGS (cadr form))) (gencase (~seg (TAGS (cadr form)))))))
  (setq LASTSEGCASE nil) ;signal default inaccesability of following code
  nil)
(defun (k*tag 6fexpr) (form)
  (let* ((lab (cadr form)))
    (if LASTSEGCASE
        (+& SUPERSEGCASE cflow LASTSEGCASE
            (& (~seg (TAGS lab)) (gencase (~seg (TAGS lab))))))
    (setq LASTSEGCASE (tags lab)) nil))
(defun (k*ifnull 6fexpr) (form)
  (predicate-6fexpr 'non-null [(cadr form)] 'null 'non-null (caddr form)))
(defun (k*if 6fexpr) (form)
  (predicate-6fexpr 'non-null [(cadr form)] 'non-null 'null (caddr form)))
(defun (k*infinite-loop-pseudo-exit 6fexpr) (form)
  (predicate-6fexpr (car form) nil 'never 'always (cadr form)))
(defun predicate-6fexpr (pred exprs branch-case no-branch-case lab)
  (let* ((RETVAR nil) (vals (mapcar #'6eval exprs))
         (seg (6instantiate-funcall [pred ! exprs])))
    (6apply vals seg)
    (+& SUPERSEGCASE cflow (& seg branch-case)
        (& (~seg (TAGS lab)) (gencase (~seg (TAGS lab)))))
    (setq LASTSEGCASE (& seg no-branch-case)))
  nil)
(defun (_lt 6fexpr) (form) ;used by fortran
  (predicate-6fexpr (car form) [(cadr form)] 'yes 'no (caddr form)))
(defun (_eq 6fexpr) (form) ;used by fortran
  (predicate-6fexpr (car form) [(cadr form)] 'yes 'no (caddr form)))
(defun (_gt 6fexpr) (form) ;used by fortran
  (predicate-6fexpr (car form) [(cadr form)] 'yes 'no (caddr form)))

;;; PDF page 42
;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; End:;
