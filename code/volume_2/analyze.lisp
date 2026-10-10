;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_2.pdf
;;; PDF pages 86-131; listing analyze.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; All internal fns in this file have the prefix "3" to
;;; avoid name conflicts.

(declare (special segfacts psegs nsegs 3affected
                  segtypes FLOWS-HELPS-ANALYZE))

(define-trace-option *ANALYZE-Debug*
                     "Show trace of ANALYZE internals"
  nil :boolean)

;;; There was a comment here that said:
;;;
;;;   All the @ data base statments are ignored totally.
;;;
;;; What does that mean? What was the scope of the statement? Is it
;;; really still true? --kmp  yes its true --DICK

(defmacro 3n1 (seg) `(first  (3nsegs ,seg)))
(defmacro 3n2 (seg) `(second (3nsegs ,seg)))
(defmacro 3p1 (seg) `(first  (3psegs ,seg)))
(defmacro 3p2 (seg) `(second (3psegs ,seg)))

;______________________________________________________________________

(defmacro 3joinp (seg)
  `(eq (3segtype ,seg) 'join))

(defun 3pred2p (seg)
  (and (eq (3segtype seg) 'split) (null (cddr (3nsegs seg)))))

(defmacro 3simplep (seg)
  `(eq (3segtype ,seg) 'simple))

;______________________________________________________________________

(defun 3segtype (seg)
  (if seg (memoizeq segtypes seg
            (cond ((??& * recursive (&* seg)) 'rec)
                  (t (?segtype seg))))))

;these find and memoize the basic cflows, they find only out-in cflow!!
;and they refuse to find recs (this is how recs don't get recognized as
;part of anything but an ssr.)

(defun 3psegs (seg)
  (memoizeq psegs seg
    (letS* ((cfl (Elist (??& * cflow * (&* seg))))
            (supseg (~seg (~segcase cfl)))
            (fseg (~seg (~from cfl))))
       (Rlist (Fselect fseg (and (not (eq supseg seg))
                                 (not (eq supseg fseg))
                                 (not (eq (3segtype fseg) 'rec))))))))

(defun 3nsegs (seg)
  (memoizeq nsegs seg
    (letS* ((cfl (Elist (??& * cflow (&* seg))))
            (supseg (~seg (~segcase cfl)))
            (tseg (~seg (~to cfl))))
       (Rlist (fselect tseg (and (not (eq supseg seg))
                                 (not (eq supseg tseg))
                                 (not (eq (3segtype tseg) 'rec))))))))

; (we will discuss 3optnext note that 3optprev is symetrical) the
;expressions FIRST and NEXT must both evaluate to a segment. they are
;each evaluated at most once, but their order of evaluation is not
;guarenteed.  if NEXT is not simple, it is returned and VAR is set to
;INIT. if NEXT is simple then it successor is returned, and VAR is set
;to NEXT.  note two things.  VAR is always the predecessor of the
;segment which is returned (no matter what); and VAR holds the
;optionally appearing simple segment if it appears.  these are
;included in order to make it easier to deal with optional segments.

(defmacro 3optnext (var first next)
  `(progn
     (setq ,var ,next)
     (cond ((3simplep ,var)
            (3n1 ,var))
           (t (prog2 nil ,var (setq ,var ,first))))))

(defmacro 3optprev (var first prev)
  `(progn
     (setq ,var ,prev)
     (cond ((3simplep ,var)
            (3p1 ,var))
           (t (prog2 nil ,var (setq ,var ,first))))))

;______________________________________________________________________


(defun 3get-top-j (join psegs)
  (let* (top-cases bot-cases)
    (letS* ((cfl (Elist (??& * cflow * (&* join)))))
      (cond ((memq (~seg (~from cfl)) psegs)
             (push (~case (~to cfl)) top-cases))
            (t (push (~case (~to cfl)) bot-cases))))
    (car (break-up-join top-cases bot-cases join))))

(defun 3get-bot-j (join psegs)
  (let* (top-cases bot-cases)
    (letS* ((cfl (Elist (??& * cflow * (&* join)))))
      (cond ((memq (~seg (~from cfl)) psegs)
             (push (~case (~to cfl)) bot-cases))
            (t (push (~case (~to cfl)) top-cases))))
    (cdr (break-up-join top-cases bot-cases join))))

;this gives you a list of all the ssrs in a plan, most of the actions below
;happen on an ssr by ssr basis, for speed, and so they can be called on
;only one bit if needed.

(declare (special 3SSRS))

(defun 3ssrs (seg)
  (let (3SSRS) (3ssrs1 seg) 3SSRS))

(defun 3ssrs1 (seg)
  (if (eq (?plantype seg) 'ssr) (push seg 3SSRS))
  (mapc #'3ssrs1 (??subseg seg)))

;this is the main entry point, note the special variable
;FLOWS-HELPS-ANALYZE.

;Special variables are used to control specific aspects of the
;analysis process.

(defvar NO-GROUP-IN-TC    nil
  "prevents the intermediate grouping of ssrs in a TC")
(defvar NO-DFLOW-MOVEMENT nil
  "prevents movement of segments based on data flow")

(defun analyze-for-recognition (segcase)
  (let ((NO-GROUP-IN-TC T) (NO-DFLOW-MOVEMENT T))
    (analyze segcase)))

(defun analyze (supsegcase)
  (cond ((??subseg (~seg supsegcase))
         (db-context-was <ANAL)
         (let ((supseg (~seg supsegcase))
               segtypes psegs nsegs segfacts)
           ;;
           (letS* ((seg (Elist (??subseg supseg))))
             (cond ((and (null (?plantype seg)) (??subseg seg))
                    (format-trace *ANALYZE-Debug*
                                  "~%Recurring to analyze ~A"
                                  seg)
                    (analyze (& seg)))))
           ;;
           (format-trace *ANALYZE-Debug* "~2%Initial bottom-up grouping")
           ;;
           (if (and (boundp 'FLOWS-HELPS-ANALYZE)
                    (eq (car FLOWS-HELPS-ANALYZE) supseg))
               (letS* ((segcase (Elist (cdr FLOWS-HELPS-ANALYZE)))
                       (cflow (if ;if this is really there
                                  (memq (~seg segcase) *db-segs*)
                                  (car (??& supsegcase cflow segcase))))
                       (plan-temp (if cflow (3exprp (~seg (~to cflow))))))
                 (cond (plan-temp (funcall (car plan-temp)
                                           (cdr plan-temp))))))
           (3group-up supseg)
           ;;
           (format-trace *ANALYZE-Debug*
             "~%Simplification of structure based on dflow")
           (if (not NO-DFLOW-MOVEMENT) (3dflow-simp supseg))
           ;;
           (format-trace *ANALYZE-Debug* "~%Temporal analysis of SSRs")
           (db-context-is <SSRANAL) (push-db-context) ;for debugging
           (3ssr-simp supseg)
           ;;
           (format-trace *ANALYZE-Debug*
                         "~%Top down simplification of EXPRs")
           (3expr-simp supseg)
           ;;
           (3fix-slot-assertions supseg)
           ;;
           (3fix-infinite-loops supseg)
           ;;
           (db-context-is >ANAL)
           (push-db-context))))
 supsegcase)

;this fixes up infinite loops by removing the dummy exits that
;are inserted early on in order for things to work (a kludge)
;we are assuming here that if an ssr has an instance of a
;special k*infinte loop-speudo-exit in it then
;it can be discarded for sure.

(defun 3fix-infinite-loops (supseg)
  (dolist (ssr (3ssrs supseg))
    (let ((body (~01 ~seg ~roleseg (??& (&* ssr) role * 'body))))
      (when (lets ((seg (Elist (??subseg body))))
              (Ror-fast
                (??& (&* seg) @seg 'funcall 'k*infinite-loop-pseudo-exit)))
        (kill-seg-and-its-subsegs ssr)))))

;there is a hack here to deal with ssr things.  if one or more
;slot names have the form <name2-of-name1>, then the ssr(s) containing
;them are grouped together as a tc and given the slot name <name1>.
;The individual slots themselves are given the names <name2>
;in the subthing.

(defun 3fix-slot-assertions (supseg)
  (let* ((slot-groups
          (letS* ((slot-fact (Elist (??& (&* supseg) slot)))
                  ((name1 name2) (3slotname-parse (~slotname slot-fact))))
            (cond (name1 (make-fact-be-out slot-fact)))
            (Reqalist (fselect name1 name1)
                      (list (~slotseg slot-fact)
                            name2
                            (~slottype slot-fact))))))
    (letS* (((name1 . prs) (Elist slot-groups))
            (ssrs (letS* () (Reqset (3up-to-ssr (car (Elist prs))))))
            (mainseg (car ssrs)))
      (when (cdr ssrs)
        (if (cdr (letS* () (Reqset (?supseg (Elist ssrs)))))
            (barf "Compound slot names not all in same TC. ~% SSRs: ~S"
                ssrs))
        (let* ((upper (?supseg (car ssrs)))
               (roles (??& (&* upper) role))
               rolenames)
          (setq mainseg (group2 ssrs name1 nil))
          (+& (& mainseg) plantype 'tempcomp)
          (dolist (role roles)
            (cond ((memq (~seg (~roleseg role)) ssrs)
                   (push (~rolename role) rolenames)
                   (+& (& mainseg) role
                       (~roleseg role) (~rolename role)))))
          (+& (& upper) role (& mainseg)
              (cond ((memq 'term rolenames) 'term)
                    ((memq 'acc rolenames) 'acc)
                    ((memq 'gen rolenames) 'gen)
                    (t 'trn)))))
      (+& (& mainseg) cliche 'pseudo-subcliche)
      (+& (& supseg) slot (& mainseg) name1 'segment)
      (dolist (pr prs)
        (let (((sc name2 type) pr))
          (+& (& mainseg) slot sc name2 type))))))

(defun 3up-to-ssr (obj)
  (prog (seg)
        (setq seg (cond ((fact? obj) (~seg (~segcase obj)))
                        (t (~seg obj))))
      L (setq seg (?supseg seg))
        (cond ((null seg) (barf "Complex slot name not in SSR."))
              ((eq (?plantype seg) 'ssr) (return seg)))
        (go L)))

;;; (3slotname-parse sym)
;;;
;;;  If sym has the string "-OF-" as a part of its pname,
;;;     returns a list of two things:
;;;      1. the part of the symbol following the -OF-
;;;      2. the part of the symbol preceding the -OF-
;;;     where both symbols are interned on the KBE package.
;;;
;;;  Otherwise it returns NIL

(defun 3slotname-parse (symbol)
  (let ((pname (get-pname symbol)))
    (let ((idx (string-search "-OF-" pname)))
      (cond ((not idx) nil)
            (t
             (list (intern-KBE (substring pname (+ idx 4)    ))
                   (intern-KBE (substring pname 0        idx))))))))

;this does basic bottom up grouping of a segment.
;it is modified so that it can be called with a seg that already has a
;plantype, in order to do a little extra grouping.  temporal-compose
;calls it that way.

(defun 3group-up (supseg)
  (repeat (segs plantype done (priortype (?plantype supseg)))
    (setq segs     (??subseg supseg)
          segtypes nil
          nsegs    nil
          psegs    nil)
    (setq plantype (or (if (and (null priortype) (null (cdr segs)))
                           ['3exprize [(car segs) 'action]])
                       (Ror-fast (3exprp (Elist segs)))
                       (Ror-fast (3simplepredp (Elist segs)))
                       (Ror-fast (3complexpredp (Elist segs)))
                       (Ror-fast (3bigloopp (Elist segs)))
                       (3ssrp supseg segs))
          done (= (length segs) (length (cdr plantype))))
    (if (and (null plantype) (null priortype))
        (barf "3GROUP-UP: Did not completely analyze.~% supseg: ~S"
              supseg))
    (if (or (null plantype) (and done priortype)) (return))
    (funcall (car plantype) (cdr plantype))
    (format-trace *ANALYZE-Debug* "~%")
    (if done (return))))

;this takes a segment and checks to see whether a compound predicate,
;simple xor or loop can be constructed starting with the given seg.
;It returns either nil if no match is possible or a cons of the
;constructor fn and an alist of the segments which will form the body
;of the new segment and thier roles.  what constructor fn is
;appropriate depends on what plan type is applicable.

(defun 3simplepredp (p)
  (if (3pred2p p)
      (let (a b a1 a2 j p2)
        (cond ((and (3simplep (setq a (3p1 p)))
                    (let ((dflow (??& * dflow (&* (&* a) output))))
                      (and dflow (Rand-fast (eq (~seg~segcase
                                                (~to (Elist dflow))) p)))))
               ['3predize-i ! [[a 'init][p 'pred]]])
              ((and (3pred2p (setq p2 (3p1 p)))
                    (setq j (cond ((eq (3n1 p2) p) (3n2 p2))
                                  (t (3n1 p2))))
                    (3joinp j) ;is a join
                    (or (eq j (3n1 p)) (eq j (3n2 p)))
                    (set= (letS* ((dflowF
                                   (Elist
                                    (??& * dflow *
                                         (&* (~to (?& * cflow (&* p) (&* j)))
                                             input)))))
                            (Rlist (cons (~from dflowF) (~id (~to dflowF)))))
                          (letS* ((dflowF
                                   (Elist
                                     (??& * dflow *
                                          (&* (~to (?& * cflow (&* p2) (&* j)))
                                              input)))))
                            (Rlist (cons (~from dflowF) (~id (~to dflowF)))))))
               ['3predize ! [[p 'pred] [p2 'pred]
                                        [(3get-top-j j [p p2]) 'join]]])
              ((and (3joinp (setq j (3optnext a1 p (3n1 p))))
                    (eq j (3optnext a2 p (3n2 p))))
               ['3xorize ! [[p 'pred]
                          !(if (not (eq a1 p)) [[a1 'action]])
                          !(if (not (eq a2 p)) [[a2 'action]])
                          [(3get-top-j j [a1 a2]) 'join]]])
              ((and (3joinp (setq j (3optprev b p (3p1 p))))
                    (or (eq j (3optnext a p (3n1 p)))
                        (eq j (3optnext a p (3n2 p)))))
               ['3loop-ssrize ! [[p 'l-pred]
                                !(if (not (eq a p)) [[a 'l-body]])
                                !(if (not (eq b p)) [[b 'l-body]])
                                [(3get-bot-j j [a]) 'l-join]]])))))
;______________________________________________________________________

;this checks whether a segment can be the start of an expr

(defun 3exprp (seg)
  (if (and (3simplep seg)
           (or (null (3psegs seg))
               (not (3simplep (3p1 seg))))
           (3n1 seg)
           (3simplep (3n1 seg)))
     ['3exprize [seg 'action]
      ! (Rlist (list (enumerateS #'(lambda (ns)
                                     (or (null ns) (not (3simplep ns))))
                                  #'3n1
                                  (3n1 seg))
                     'action))]))

;this works on commutable preds and two exit loops.

(defun 3complexpredp (p)
  (if (3pred2p p)
      (let (a b c j p2)
        (cond ((and (3pred2p (setq p2 (3p1 p)))
                    (3joinp (cond ((eq (3n1 p2) p)
                                  (setq j (3optnext b p2 (3n2 p2))))
                                  (t
                                  (setq j (3optnext b p2 (3n1 p2))))))
                    (or (eq j (3optnext a p (3n1 p)))
                        (eq j (3optnext a p (3n2 p))))
                    (3commutable-preds p p2 (cond ((eq a p) j)
                                                  (t a))))
               ['3commute-preds ! [[p 'pred]
                                   [p2 'pred]
                                   !(if (not (eq a p)) [[a 'action]])
                                   [(3get-top-j j [a b]) 'join]
                                   !(if (not (eq b p2)) [[b 'action]])]])
              ((and (3pred2p (setq p2 (3optprev b p (3p1 p))))
                    (3joinp (setq j (3optprev c p2 (3p1 p2))))
                    (or (eq j (3optnext a p (3n1 p)))
                        (eq j (3optnext a p (3n2 p)))))
               ['3loop-ssrize ! [[p 'l-pred]
                                 [p2 'l-pred]
                                 !(if (not (eq a p)) [[a 'l-body]])
                                 !(if (not (eq b p)) [[b 'l-body]])
                                 !(if (not (eq c p2)) [[c 'l-body]])
                                 [(3get-bot-j j [a]) 'l-join]]])))))

(defun 3bigloopp (j)
  (let (a b p frontq endq)
    (cond ((and (3joinp j)
                (3pred2p (setq p (3optnext a j (3n1 j)))))
           (setq frontq [p]
                 endq   frontq)
           (if (repeat ()
                 (if (null frontq) (return nil))
                 (setq p (3optnext a (car frontq) (3n1 (car frontq))))
                 (if (eq p j) (return t))
                 (if (3pred2p p) (setq endq (cdr (rplacd endq [p]))))
                 (setq p (3optnext a (car frontq) (3n2 (car frontq))))
                 (if (eq p j) (return t))
                 (if (3pred2p p) (setq endq (cdr (rplacd endq [p]))))
                 (setq frontq (cdr frontq)))
               ['3loop-ssrize ! [[(3get-bot-j j [a]) 'l-join]
                                 !(if (not (eq a (top frontq))) [[a 'l-body]])
                                 [(top frontq) 'l-pred]
                                 !(letS* ((p2 (enumerateS
                                              #'(lambda (p) (eq p j))
                                              #'(lambda (p)
                                                  (3optprev b nil (3p1 p)))
                                              (3optprev b nil
                                                        (3p1 (top frontq))))))
                                    (nconc (Rnconc [!(if b [[b 'l-body]])
                                                    [p2 'l-pred]])
                                           (if b [[b 'l-body]])))]])))))

;this recognizes that a seg is an ssr because it already has a rec
;[eg it wasn't originally coded as a loop, but was really recursive.]
;the only effort which then has to be done is to make sure the right roles
;get set up.  [note that there is a bit of effort in the other parts of
;this file in order to prevent recs from getting recognized as part of
;other things.]

(defun 3ssrp (supseg segs)
  (let* ((recs (~nset ~seg~subseg (??& (&* supseg) recursive))))
    (if (and recs (qset< recs segs))
        ['3recursive-ssrize
           ![!(Rbag (list (Elist recs) 'rec))
             !(Rbag (list (filterS #'(lambda (s) (not (memq s recs)))
                                  (Elist segs))
                          'other))]])))

(defun 3recursive-ssrize (segs)
  (barf "analysis of recursive programs not currently supported")
;this does not set up the correspondance between the inputs of the
;body and the rec right!
  (when (eq (cadadr segs) 'rec)
    (barf "3RECURSIVE-SSRIZE: Cannot handle multiple recursion.~
         ~% Segs: ~S"
          segs))
  (let* ((rec (caar segs))
         (body (group2 (mapcar #'car segs) 'tcbody 'always))
         (ssr (?supseg body))
         (dj (genseg 'dj)))
;make the rec be linked to the body instead of the ssr.
    (assert-fact (&~segcase (-& * recursive (&* rec)) (& body)))
;set up the dfjoin
    (+& (& body) subseg (& dj))
    (+& (& dj) segtype 'join)
    (+& (& dj) @seg 'join nil nil)
    (dolist (df (--& (&* body) dflow * (&* (&* body) output)))
      (let* ((from (~from df))
             (sourcedf (--& (&* body) dflow *
                            (&* (&* (~seg~segcase from)) ;the original join
                                input 'id * (~obj from))))
             (obj (~obj from))
             (nout (+& (& dj) output nil (genobj dj obj) nil)))
        (--& from @io) (--& from @createvar)
        (make-fact-be-out from)
        (+& (& body) dflow nout (~to df))
        (dolist (sdf sourcedf)
          (--& (~to sdf) @io) (--& (~to sdf) @createvar)
          (make-fact-be-out (~to sdf))
          (+& (& body) dflow (~from sdf)
              (+& (& dj (cond ((eq (~seg~segcase (~from sdf)) rec) 'rec)
                              (t 'body)))
                  input 'id (genobj dj obj) (~obj nout))))))
;set up plantype and roles
    (+& (& ssr) plantype 'ssr)
    (+& (& ssr) role (& body) 'body)
    (+& (& ssr) role (& dj) 'dfjoin)
    (+& (& ssr) role (& rec) 'rec)
    (+& (& body) plantype 'ssr-body)))

;there might be something funny here with multiple exits,
;and the kind of joins they are supposed to have.

(defun 3predize (rsegs)
  (let ((seg (group (Rlist (car (Elist rsegs))) 'pred)))
    (>& (& seg) plantype 'pred)
    (3make-roles rsegs seg)
    seg))

(defun 3predize-i (rsegs)
  (let* ((seg (3predize rsegs))
         (init (caar rsegs))
         (cflowin (car (--& (&* seg) cflow * (&* init)))))
    (if cflowin
        (assert-fact (&~to cflowin (~to (-& (&* seg) cflow (&* init))))))
;this is a cludge to get mnemonic output cases in this situation.
;note cludge to avoid renaming clobbering
    (letS* (((cf outside-cf)
             (Elist (letS* ((cflowF (Elist (--& (&* seg) cflow * (&* seg)))))
                     (Rbag (list cflowF (-& * cflow (~to cflowF))))))))
      (>& (& seg) cflow (~from cf) (& seg (~case (~from cf))))
      (>& (~segcase outside-cf) cflow
          (& seg (~case (~from cf))) (~to outside-cf)))
    seg))

(defun 3exprize (rsegs)
  (let ((seg (group (Rlist (car (Elist rsegs))) 'expr)))
    (>& (& seg) plantype 'expr)
    (--& (&* seg) cflow)
    (3make-roles rsegs seg)
    seg))


(defun 3xorize (rsegs)
  (let ((seg (group (Rlist (car (Elist rsegs))) 'xor)))
    (>& (& seg) plantype 'xor)
    (3make-roles rsegs seg)
    (3xor-simp seg)
    seg))

;______________________________________________________________________

(defun 3make-roles (rsegs seg)
  (dolist (rseg rsegs)
    (let (((dseg name . nil) rseg))
      (>& (& seg) role (& dseg) name))))
;______________________________________________________________________

;the xor simplification must happen before the dflow movement

(defun 3xor-simp (xor)
  (let* ((pred (?role xor 'pred))
         (join (?role xor 'join))
         (innards (??role xor 'action)))
    (if (null ;leave alone if dflow from pred
          (??& (&* xor) dflow (&* (&* pred) output)))
        (dolist (iseg innards)
          (when (and (eq (?plantype iseg) 'xor) (null (?role iseg 'init)))
            (let* ((ipred (?role iseg 'pred))
                   (ijoin (?role iseg 'join))
                   (iinnards (??role iseg 'action)))
              (when (null ;no good if ipred dflow
                      (??& (&* iseg) dflow (&* (&* ipred) output)))
                (ungroup iseg)
                (dolist (iinnard iinnards)
                  (>& (& xor) role (& iinnard) 'action))
                (setq pred (3predize [[pred 'pred] [ipred 'pred]]))
                (>& (& xor) role (& pred) 'pred)
                (setq join (fuse-joins ijoin join))
                (format-trace *ANALYZE-Debug* "~%"))))))))

;This program takes a loop which has been located, and converts it
;into a SSR.  it also constructs the plantype and role statements
;which are appropriate.  (the transformation is detailed in an
;illustration in the thesis.)

(def-alistq upport)

(defun 3loop-ssrize (rsegs)
  (let* ((body (group2 (Rlist (car (Elist rsegs))) 'tcbody 'always))
         (bodycase (& body))
         (bodypat (&* body))
         (oldjoin (dolist (rseg rsegs)
                    (let (((seg role) rseg))
                      (if (eq role 'l-join) (return seg)))))
         (oldjoinpat (&* oldjoin))
         (ijcase (~to (-& bodypat cflow bodypat oldjoinpat)))
         (ssr (group [body] 'tc))
         (ssrpat (&* ssr))
         (ssrcase (& ssr))
         (rec (genseg 'rec))
         (reccase (& rec))
         (dj (genseg 'dj))
         (djcase (& dj)))
;get rid of the cflow connections to the old join in the loop.
    (+& bodycase cflow bodycase (~to (-& bodypat cflow oldjoinpat)))
    (+& bodycase cflow (~from (-& bodypat cflow * oldjoinpat)) reccase)
;set up relationship between recurrence and body
    (+& bodycase recursive reccase)
    (+& bodycase subseg reccase)
    (+& reccase segtype (?segtype body))
    (+& reccase @seg 'recursion nil nil)
;set up dfjoin
    (+& bodycase subseg djcase)
    (+& djcase segtype 'join)
    (+& djcase @seg 'join nil nil)
    (dolist (df (--& bodypat dflow * (&* bodypat output)))
      (let* ((obj (~obj (~to df)))
             (nout (+& djcase output nil (genobj dj obj) nil)))
        (+& bodycase dflow (~from df)
            (+& (& dj 'body) input 'id (genobj dj obj) (~obj nout)))
        (+& bodycase dflow
            (+& reccase output nil (genobj rec obj) nil)
            (+& (& dj 'rec) input 'id (genobj dj obj) (~obj nout)))
        (+& bodycase dflow nout (~to df))))
;set up plantype and roles
    (+& ssrcase plantype 'ssr)
    (+& ssrcase role bodycase 'body)
    (+& ssrcase role reccase 'rec)
    (+& ssrcase role djcase 'dfjoin)
    (+& bodycase plantype 'ssr-body)
;setup appropriate dflow to and from body and rec
(let* ((joined-idfs
        (--& bodypat dflow (&* bodypat input) (&* ijcase input)))
       rec-inputs-used)
;make all of the inputs which go to other than the input case of the
;join available on each cycle of the ssr, and remember which
;ones these are.
      (dolist (df (??& bodypat dflow (&* bodypat input)))
        (let* ((input (~from df)))
          (cond ((not (member input rec-inputs-used))
                 (push input rec-inputs-used)
                 (>& bodycase dflow input
                     (>& reccase input nil (genobj rec (~obj input)) nil))))))
;now take care of the dflows going to the input part of the old join
      (dolist (df joined-idfs)
        (let* ((dest (~to df))
               (input (~from df)))
          (if ;must duplicate the input to stop trouble.
              (memq input rec-inputs-used)
              (+& ssrcase dflow (~from (?& ssrpat dflow * input))
                  (setq input
                        (+& bodycase input nil
                            (genobj body (~obj input)) nil))))
          (push input rec-inputs-used)
          (dolist (df (--& bodypat dflow (&* oldjoinpat output * (~id dest))))
            (>& bodycase dflow input (~to df)))
          (>& bodycase dflow
              (~from
                (-& bodypat dflow * (&* oldjoinpat input * * (~id dest))))
              (>& reccase input nil (genobj rec (~obj input)) nil)))))
;get rid of everything about the old join
    (kill-seg-and-its-subsegs oldjoin)

;now for each pred in the loop, the cflow has to be rerouted
;through new joins to cases of the body.
    (dolist (rseg rsegs)
      (let (((pred role) rseg))
        (when (eq role 'l-pred)
          (let* ((pbcf (-& bodypat cflow (&* pred) bodypat))
                 (join (genseg 'join)))
            (+& bodycase subseg (& join))
            (+& (& join) segtype 'join)
            (+& (& join) @seg 'join nil nil)
            (+& bodycase cflow (& rec (~case (~to pbcf))) (& join 'rec))
            (+& bodycase cflow (~from pbcf) (& join 'pred))
            (+& bodycase cflow (& join) (~to pbcf))))))))

;this performs the transformation depicted on the next page.  the key
;question is whether of not the two preds can just be commuted or not.
;they can if and only if the condition under which p2 goes to p is
;IMPLIED by the condition under which p takes the branch outside.  And
;the input data to the two preds is compatable, here we assume they
;must be the same.  of the 36 ways the predicates _lt _eq and _gt can
;interact, 12 of them satisfy this criterion.  However, 6 of these are
;uninteresting because they correspond to situations where one of the
;three choices can never be taken.  this leaves 6 cases of interrest.
;(the first (_lt no _eq no) is sometimes created by _AIF in FREAD)
;(another situation (_lt no _eq yes) which is not commutable can also
;be produced by _AIF.)

(defun 3commutable-preds (p p2 a)
  (let* ((ppat (&* p))
         (p2pat (&* p2))
         (apat (&* a))
         (supseg (?supseg p))
         (supsegpat (&* supseg))
         (p2pcflow (?& supsegpat cflow p2pat ppat))
         (pacflow (?& supsegpat cflow ppat apat))
         (-pdflows (??& supsegpat dflow * (&* ppat input)))
         (-p2dflows (??& supsegpat dflow * (&* p2pat input))))
    (and (= (length -p2dflows) 1)
         (= (length -pdflows) 1)
         (equal (~from (car -p2dflows)) (~from (car -pdflows)))
         (member [(~01 ~name (??& p2pat @seg))
                  (~case (~from p2pcflow))
                  (~01 ~name (??& ppat @seg))
                  (~case (~from pacflow))]
                  '((_lt no _eq no)
                   (_lt no _gt no)
                   (_gt no _eq no)
                   (_eq no _lt no)
                   (_gt no _lt no)
                   (_eq no _gt no))))))

;______________________________________________________________________

;this routine actually commutes the order of two predicates.  It takes
;an argument which is a list of segments and role names.  there can be
;from 3 to 5 segments in this list, the order is very important. the
;lower pred is first, the higher pred is second, the segment which
;follows the lower pred must be third, and the seg which follows the
;higher pred must be last.

(defun 3commute-preds (segs)
  (let* ((((p) (p2) (a)) segs)
         (pcase (& p))
         (ppat (&* p))
         (p2case (& p2))
         (p2pat (&* p2))
         (apat (&* a))
         (supseg (?supseg p))
         (supsegpat (&* supseg))
         (p2pcflow (-& supsegpat cflow p2pat ppat))
         (pacflow (-& supsegpat cflow ppat apat))
         (-p2cflow (-& supsegpat cflow * p2pat)))
    (pp-trace *ANALYZE-Debug* [['3commute-preds ! segs]])
    (assert-fact (&~to p2pcflow (~to pacflow)))
    (assert-fact (&~to -p2cflow pcase))
    (assert-fact (&~to pacflow p2case))
    (3xorize (cdr segs))))

;this has the same argument order as the fn above.

(defun 3unfactor-preds (segs)
  (let* ((((p) (p2) (a)) segs)
         (pcase (& p))
         (ppat (&* p))
         (p2case (& p2))
         (p2pat (&* p2))
         (apat (&* a))
         (b (caar (last segs)))
         (bpat (&* b))
         (supseg (?supseg p))
         (supsegcase (& supseg))
         (supsegpat (&* supseg))
         (p2pcflow (-& supsegpat cflow p2pat ppat))
         (pacflow (-& supsegpat cflow ppat apat))
         (p2bcflow (?& supsegpat cflow p2pat bpat))
         (-p2cflow (-& supsegpat cflow * p2pat))
         (p2c (copyseg p2 p2))
         (p2cacase (& p2c (~case (~from p2pcflow))))
         (p2cbcase (& p2c (~case (~from p2bcflow))))
         (p2ccase (& p2c))
         (j (genseg 'join))
         (jcase (& j)))
    (pp-trace *ANALYZE-Debug* [['3unfactor-preds ! segs]])
    (+& supsegcase subseg p2ccase)
    (+& supsegcase subseg jcase)
    (+& jcase segtype 'join)
    (+& jcase @seg 'join nil nil)
    (+& jcase plantype 'join)
    (assert-fact (&~to p2pcflow (~to pacflow)))
    (assert-fact (&~to -p2cflow p2ccase))
    (+& supsegcase cflow p2cacase pcase)
    (+& supsegcase cflow p2cbcase (& j p2c))
    (assert-fact (&~to pacflow (& j p)))
    (+& supsegcase cflow jcase p2case)
    (3predize [[p2c 'pred][p 'pred][j 'join]])
    (3xorize (cdr segs))))

;   this little fn looks at a segment and all its decendents trying to
;factor the dflow forward.  it works from the topmost subsegs downward.
;it returns nil if no change instigated by it propogated up to it.
;otherwise it returns the seg which is the highest level that must be
;backed up to in order to continue the simplification.
;   in the first part (the computation of high) it looks at each subseg in
;turn and tests whether it can be dflow moved.  If it finds one which can
;be, it moves it then stops looking at the subsegs.  Instead, it returns
;the name of the highest seg which could
;have been modified by the move.
;   If no subsegs are moved, then in the second part, it calls itself
;recursively on each subseg.  if a subseg is modified, then if that seg is
;the highest modified point, it just calls itself again to try for further
;moves. (note the memory variable segfacts is reset).  if the modification
;reached to a higher level, then the problem is kicked upstares.  this
;results in the stack unwinding up to the appropriate point to begin
;another pass.
;   The complexity of this fn stems mostly from the fact that the control
;stack is used to encode a rather complex order of simplyfication of segs
;which is dynamically modified as the situation warents.  this complexity
;was introduced because this simplification runs around the program an
;awful lot.  It takes advantage of the fact that nothing can ever be moved
;between the inside and the outside of an ssr-body in order to break up
;its work.

(defun 3dflow-simp (seg)
  (let* (SEGFACTS)
    (3dflow-simp1 nil seg)
    (dolist (ssr (3ssrs seg))
      (3dflow-simp1 nil (?role ssr 'body)))))

(defun 3dflow-simp1 (supseg seg)
  (repeat ()
    (if (*catch seg (3dflow-simp2 seg)) (return))
    (when (null (?segtype seg)) ;ie got ungrouped by the move
      (if (null supseg)
          (barf "3DFLOW-SIMP1: Top seg got ungrouped.~% Seg: ~S" seg))
      (*throw supseg nil)))) ;go up to supseg - else just look again.

(defun 3dflow-simp2 (seg)
  (let ((subsegs (??subseg seg)))
    (dolist (dseg subsegs)
      (let* ((cpath (3check-path dseg (3suggest-dmove-path dseg))))
        (when cpath
          (3do-dflow-move dseg cpath)
          (setq segfacts nil)
          (*throw (3high cpath seg) nil))))
    (dolist (dseg subsegs)
      (when (and (??subseg dseg) (not (eq (?plantype dseg) 'ssr-body)))
        (3dflow-simp1 seg dseg)))
    t))

(defun 3high (cpath init-high)
  (repeat ((i 0) (high init-high) (rpath cpath) dir seg)
    (if (null rpath) (return high))
    (desetq (dir seg . rpath) rpath)
    (cond ((eq dir 'out)
           (cond ((zerop i) (setq high seg))
                 (t (incf i))))
          (t (decf i)))))

(defun 3check-path (seg path)
  (cond ((not (eq (third (3segfacts seg)) 'simple)) nil)
        (t (3check-step seg nil (car path) (cdr path)))))

;this takes as arguments, the seg that is moving, the reverse of the path
;up to this point, the seg which marks this point in the path, and the
;rest of the path after this point.  first the rest of the path is
;checked, and then the current portion is checked.

(defun 3check-step (seg pre-path oseg path-info)
  (let* ((path path-info)
         ((dir nseg . rest-path) path))
    (if path
        (let* ((npath (3check-step seg
                                  [dir oseg ! pre-path] nseg rest-path))
               ((oterm otype ostype) (3segfacts oseg))
               ((nterm ntype nstype) (3segfacts nseg)))
          (prog () (go start)
           error (barf "3CHECK-STEP: Error.~% Data: ~S" [oseg ! path])
           not-ok (return nil)
           ok-if-npath (if (null npath) (return nil))
           ok    (return [dir nseg ! npath])
           start (cond ((eq dir 'OUT)
                        (cond ((memq otype '(PRED XOR SSR AUG FILTER
                                             TERM TEMPCOMP SSR-BODY))
                               (go not-ok))
                              ((memq otype '(EXPR COMP AND))
                               (go ok-if-npath))))
                       ((not nterm) ;and (eq dir 'IN)
                        (cond ((memq ntype '(SSR AUG FILTER TERM TEMPCOMP))
                               (go ok))
                              ((memq ntype '(PRED XOR))
                               (cond ((3side-effects seg) (go not-ok))
                                     (T (go ok-if-npath)))) ;no xor init
                              ((eq ntype 'SSR-BODY)
                               (go not-ok))
                              ((memq ntype '(EXPR COMP AND))
                               (cond ((memq otype '(EXPR COMP AND))
                                      (go ok-if-npath))
                                     (t
                                      (go ok))))))
                       ((eq ntype 'REC) ;and (not (null nterm))
                        (go not-ok))
                       ((eq ntype 'JOIN)
                        (cond ((and (eq otype 'XOR)
                                    (not (??& (&* oseg) role
                                              (&* (cond (pre-path
                                                         (cadr pre-path))
                                                        (t seg)))
                                              'ACTION)))
                               (go ok))
                              (t (go not-ok))))
                       ((eq ntype 'PRED)
                        (cond ((not (eq otype 'PRED)) (go ok))
                              ((cdr (??role oseg 'PRED)) (go ok))
                              (t (go not-ok))))
                       ((eq ntype 'EXPR)
                        (cond ((memq otype '(EXPR COMP AND))
                               (go not-ok))
                              (t (go ok)))))
            (go error))))))

(defun 3side-effects (seg)
  (Ror-fast (??& (&* (& (Elist (all-subsegs seg))) output)
                 @io 'side-effect)))

(defun 3segfacts (seg)
  (memoizeq segfacts seg
    (let* ((terminal (null (??subseg seg)))
           (type (?plantype seg))
           (stype (cond ((??& * recursive (&* seg)) 'rec)
                        (t (?segtype seg)))))
      (if terminal (setq type (cdr (assq stype '((split . pred)
                                                 (simple . expr)
                                                 (join . join)
                                                 (rec . rec))))))
      (if (or (null type) (null stype))
          (barf "3SEGFACTS: Error.~% Data: ~S" seg))
      [terminal type stype])))


;this routine generates a listing of the common portion of the dflow
;paths out of a segment each step on the list is a seg and a
;direction.  for example a result of ((s1 output) (s2 output) (s3
;input) (s4 input)) means that all of the output from the segment goes
;out of s1 , then out of s2 then into s3 and finally into s4 at which
;point the dflow paths either diverge, or all terminate.

(defun 3common-dflow-path (segcase)
  (if (3is-mobile (~seg segcase))
      (letS* ((ports (enumerateS
                       #'(lambda (ps)
                           (or (null ps)
                               (progn (setq segcase (~segcase (car ps)))
                                      (Ror-fast
                                        (not (equal (~segcase
                                                       (Elist (cdr ps)))
                                                    segcase))))))
                       #'(lambda (ps)
                           (Rnconc (~nset ~to (??& * dflow (Elist ps)))))
                       (~nset ~to (??& * dflow (&* (&* (~seg segcase))
                                                   output))))))
        (Rlist (list (~seg segcase) (~keyword (car ports)))))))


;certain kinds of segs cannot be moved.  In particular roles that don't
;have any dflow from them to say were they should really be.
;Also probably some kind of side-effect guys cannot move either,
;but we aren't dealing with that now.

(defun 3is-mobile (seg)
  (cond ((and (??& * slot (&* seg))
              (null (??& * dflow (&* (&* seg) output))))
         nil)
        (T (Rand-fast (3is-mobile (Elist (??subseg seg)))))))

;this routine transforms the common dflow path above into a segment
;movement path this path would be (s1 out s2 out s' in s3 in s4) in
;the above example.  s' is the superseg of s2. this path is a list of
;the series of supersegs of seg as it is moved and indications of the
;directions to be taken.  if the dflow path is nil then the movement
;path will be (sup) where sup is the superior of seg.

(defun 3suggest-dmove-path (seg)
  (cdr (over [[tseg code] ! cdp [nseg ncode]]
                <- [[seg 'output] ! (3common-dflow-path (& seg))]
             by cdp until (null tseg)
         nconc
         (cond ((eq code 'output)
                (cond ((eq ncode 'output) ['out nseg])
                      (t (let ((sup (?supseg tseg)))
                           (cond (sup ['out sup])
                                 (t (return (nreverse over-acc))))))))
               (t ['in tseg])))))

(defun 3do-dflow-move (seg path)
  (when (not (null path))
    (letS* (((dir nseg . rpath) (generateS #'cddr path)))
      (cond (rpath
             (cond ((eq dir 'OUT) (move-seg-up seg))
                   (t (move-seg-down nseg seg))))
            (t
             (cond ((eq dir 'OUT)
                    (move-seg-up seg)
                    (let ((sseg (?supseg seg)))
                      (>& (& sseg) role (& seg)
                          (cond ((memq (?plantype sseg) '(EXPR COMP AND))
                                 'ACTION)
                                (t 'INIT)))))
                   (t
                    (let (((nterm ntype nstype) (3segfacts nseg)))
                      (cond (nterm
                             (cond ((eq ntype 'JOIN)
                                    (let* ((nsegcase
                                            (~segcase
                                             (~to (a?& * dflow
                                                       (&* (&* seg) output)
                                                       (&* (&* nseg) input)))))
                                           (nsegcase-cf
                                            (?& * cflow * nsegcase))
                                           (action (~seg (~from nsegcase-cf)))
                                           ((aterm atype) (3segfacts action)))
                                      (if (not
                                            (??& * role (&* action) 'action))
                                          (setq action nil))
                                      (cond (action
                                             (cond ((and
                                                      (not aterm)
                                                      (memq atype
                                                            '(EXPR COMP AND)))
                                                    (move-seg-down action seg)
                                                    (>& (& (?supseg seg)) role
                                                        (& seg)
                                                        'ACTION))
                                                   (t
                                                    (>&
                                                     (& (~seg~segcase
                                                         nsegcase-cf))
                                                     role
                                                     (& (3exprize
                                                         [[seg 'action]
                                                          [action 'action]]))
                                                     'action))))
                                            (t
                                             (--& * role (&* seg))
                                             (>& (& (~seg~segcase nsegcase-cf))
                                                 role (& seg) 'action)
                                             (make-fact-be-out nsegcase-cf)
                                             (assert-fact (&~from nsegcase-cf
                                                                  (& seg)))
                                             (assert-fact (&~to nsegcase-cf
                                                                (& seg)))))))
                                   (t (let ((role (car (--& * role (&* nseg))))
                                            (newseg
                                             (cond ((eq ntype 'pred)
                                                    (3predize-i
                                                     [[seg 'init]
                                                      [nseg 'pred]]))
                                                   ((eq ntype 'expr)
                                                    (3exprize
                                                     [[seg 'action]
                                                      [nseg 'action]]))
                                                   (t
                                                    (barf "3DO-DFLOW-MOVE: ~
                                                             Error 1.~
                                                          ~% Seg: ~S~
                                                          ~% Path: ~S"
                                                           seg path)))))
                                       (if role
                                           (assert-fact
                                             (&~roleseg role (& newseg))))))))
                           (t ;(not nterm)
                            (cond ((memq ntype '(EXPR COMP AND))
                                   (move-seg-down nseg seg)
                                   (>& (& (?supseg seg))
                                       role (& seg) 'action))
                                  ((memq ntype '(PRED XOR SSR AUG TERM
                                                 FILTER TEMPCOMP))
                                   (let* ((init (car (--role nseg 'init))))
                                     (move-seg-down nseg seg)
                                     (if init
                                         (setq seg (3exprize
                                                     [[seg 'action]
                                                      [init 'action]])))
                                     (>& (& (?supseg seg))
                                         role (& seg) 'init)))
                                  (t (barf "3DO-DFLOW-MOVE: Error 2.~
                                            ~% Seg: ~S~% Path: ~S"
                                             seg path))))))))
            (done))))
    (format-trace *ANALYZE-Debug* "~%"))
  nil)

;this works bottom up simplifying and analyzing exprs.  if an expr has
;an expr as supseg, then the lower expr is ungrouped.  if an expr has
;a non expr superseg then the expr is analyzed to see if it can have
;an AND at the top level.  for the convenience of modular calling, it works
;on an ssr by ssr basis.

;NOTE THAT THE PROCESSING OF EXPRS TO SELECT ANDs HAS BEEN TEMPORARILY
;COMMENTED OUT.  IT DOESN'T SEEM TO MAKE ANYTHING WORK BETTER AT THE
;CURRENT TIME, AND MAKES THINGS A BIT MORE CONFUSING IN THE PLAN EDITOR.
;SEE THE NEXT PAGE.

(defun 3expr-simp (seg)
  (mapc #'3expr-simp1 (qsetU [seg] (3ssrs seg))))

(defun 3expr-simp1 (seg)
  (cond ((eq (?plantype seg) 'expr) (3expr-simp-top seg))
        (t (mapc #'3expr-simp2 (??subseg seg)))))

(defun 3expr-simp2 (seg)
  (cond ((eq (?plantype seg) 'ssr) nil)
        ((eq (?plantype seg) 'expr) (3expr-simp-top seg))
        (t (mapc #'3expr-simp2 (??subseg seg)))))

(defun 3expr-simp-top (seg) ;seg must be an expr
  (dolist (s (??subseg seg)) (3expr-simp-ungroup seg s)))

(defun 3expr-simp-ungroup (supseg seg)  ;supseg must be an expr
  (let* ((subsegs (??subseg seg)))
    (cond ((not (eq (?plantype seg) 'expr)) (mapc #'3expr-simp2 subsegs))
          (subsegs
           (dolist (s subsegs) (3expr-simp-ungroup seg s))
           (setq subsegs (??subseg seg)) ;may have changed if ungrouping
           (ungroup seg)
           (dolist (s subsegs) (>& (& supseg) role (& s) 'action))
           (format-trace *ANALYZE-Debug* "~%")))))

;(defun 3expr-simp1 (seg)
;  (cond ((eq (?plantype seg) 'expr) (3expr-simp-and seg))
;        (t (mapc #'3expr-simp2 (??subseg seg)))))
;
;(defun 3expr-simp2 (seg)
;  (cond ((eq (?plantype seg) 'ssr) nil)
;        ((eq (?plantype seg) 'expr) (3expr-simp-and seg))
;        (t (mapc #'3expr-simp2 (??subseg seg)))))
;
;(defun 3expr-simp-and (seg) ;seg must be an expr
;  (dolist (s (??subseg seg)) (3expr-simp-ungroup seg s))
;  (let ((split (3dflow-split (??subseg seg))))
;    (when (cdr split)
;      (dolist (set split)
;        (cond ((cdr set)
;               (>& (& seg) role (& (3exprize (Rlist (list (Elist set)
;                                                          'action))))
;
;                   'action))
;              (t (assert-fact (&~rolename (-& (&* seg) role (&* (car set)))
;                                          'action)))))
;      (--& (&* seg) plantype)
;      (>& (& seg) plantype 'and)
;      (format-trace *ANALYZE-Debug* "~%"))))
;
;;this takes a set of segs which form a directed graph connected be
;;dflow arcs, and partitions them into unconnected subgraphs.  this is
;;done by coloring each seg and taking a transitive closure of the
;;dflow arcs in order to propagate the colors to regions.  at the end
;;each region of one color is a disjoint subgraph.  a list of the
;;subgraphs is returned.
;
;(defun 3dflow-split (segs)
;  (let* ((segcolors (letS* ((s (Elist segs)))
;                      (nreverse (Rlist (cons s s))))))
;    (over segs -> seg
;         (over (??& * dflow (&* (&* seg) output) (&* * input))
;               (~seg (~segcase (~to (car _)))) -> nseg
;               (cdr (assq nseg segcolors)) -> color
;           select color
;           makeqset init [(cdr (assq seg segcolors))]
;           color) -> colors
;      (when (cdr colors)
;        (dolist (item segcolors)
;          (when (memq (cdr item) (cdr colors))
;            (setf (cdr item) (car colors))))))
;    (over (over segcolors -> [seg ! key] classifyq [key ! seg])
;      nlist (cdr _))))

;this function calls 3ssr-analyze on all of the SSRs in a plan

(defun 3ssr-simp (seg)
  (dolist (ssr (3ssrs seg))
    (3ssr-analyze ssr)))


;this part of the file has functions which make it possible to analyze
;an SSR as a TEMP COMP.

  ;; a clasification of the roles of an xor being worked on
(def-alistq xrole-fillers)

  ;; an alist of fsets showing what segs each seg affects
(def-alistq 3AFFECTED)

  ;; shows what tjoin goes with each term pred (each term is
  ;; assumed to be binary for now)
(def-alistq term-join-cor)

  ;; shows what join goes with each filter pred
(def-alistq filter-join-cor)

  ;; shows the actions that are dominated by
  ;; each filter pred
(def-alistq pred-action-cor)

  ;; a mapping of ports to sets of ports to show the
  ;; effect of the relationship between a seg and
  ;; its recursive instances.
(def-alist porttrans)

;this are just special because I had to break up the fn in order for
;the lispm to compile it.

(declare (special terms tjoins body bodypat rec dj tc porttrans
                  3AFFECTED todo non-join-segs segtypes ssr term-join-cor
                  filter-join-cor seg-order pred-action-cor))

;note as of 1984 we are doing this a new way where we don't separate
;terms from generators.  As soon as we get to the point where we would
;have taken out a terminator we just leave everything as a bssr.
;the following variable can switch this feature on and off.

(defvar *separate-gens-and-terms* nil)

(defun 3ssr-analyze (ssr)
  (let* (terms                  ;a set of the termination preds
         tjoins         ;the termination joins
         (body (?role ssr 'body))
         (bodypat (&* body))
         (rec (?role ssr 'rec))
         (dj (?role ssr 'dfjoin))
         tc porttrans 3AFFECTED todo non-join-segs segtypes
         term-join-cor filter-join-cor seg-order pred-action-cor)
    (format-trace *ANALYZE-Debug* "~%")
    (pp-trace *ANALYZE-Debug* ['3ssr-analyze ssr])
    (3ssra-build-tc)
    (3ssra-build-porttrans)
    (3ssra-classify-subsegs)
    (3ssra-init-affects)
    (3ssra-ungroup)
    ;force it to copy
    (setq non-join-segs (makefset [!! todo !! terms !! nil]))
    (3ssra-compute-seg-order)
    (3ssra-build-affected)
    (pp-trace *ANALYZE-Debug* ['3AFFECTED-relation ! 3AFFECTED])
    (repeat ()
      (when (null 3AFFECTED)
        (cond ((not *separate-gens-and-terms*))
              ((and (null (??input ssr))
                    (null (??output ssr))
                    (null (??& (&* ssr) toutput)))
               (kill-seg-and-its-subsegs ssr))
              (t
               (barf "SSR-ANALYZE: Couldn't completely analyze.~
                      ~% TC: ~S"
                     tc)))
        (return))
      (3ssra-remove-fragment))
;distribute the init of the tc into the pieces as much as possible.
    (let ((dummy (genseg 'dummy)))
      (>& (& tc) subseg (& dummy)) ;to prevent collapse of tc
      (>& (& dummy) segtype 'simple) ;needed by 3segfacts
      (3dflow-simp1 nil tc)
      (kill-seg-and-its-subsegs dummy))
    tc))

;build up the tempcomp seg.  Make a new seg so that we can have
;a temp comp.  This is done inside out in case the ssr is an outermost seg.

(defun 3ssra-build-tc ()
  (let* ((role-facts (??& (&* ssr) role)))
    (setq tc  ssr
          ssr (group2 [body] 'bssr 'always))
    (dolist (role-fact role-facts)
      (cond ((not (eq (~rolename role-fact) 'init))
             (assert-fact (&~segcase role-fact (& ssr))))))
    (>& (& ssr) plantype 'ssr)
    (-& (&* tc) plantype)
    (>& (& tc) plantype 'tempcomp)
    (>& (& tc) role (& ssr) 'bssr)
    (-& (&* tc) cflow (&* tc) (&* ssr))))

;construct the mapping between the ports of the rec and the ports of
;the body

(defun 3ssra-build-porttrans ()
  (dolist (out (??output body))
    (setf (porttrans out)
          (??& (&~seg (~segcase out) rec) output * (~obj out))))
  (dolist (in (??input rec))
    (setf (porttrans in) (??& bodypat input * (~obj in)))))

;classify the subsegs of the body

(defun 3ssra-classify-subsegs ()
  (dolist (seg (??subseg body))
    (when (and (not (eq seg rec)) (not (eq seg dj)))
      (cond ((eq (3segtype seg) 'split) (push seg terms))
            ((eq (3segtype seg) 'join) (push seg tjoins))
            (t (push seg todo)))))
  (setq todo (makefset todo)))

;set up the affects relation and term-join-cor for the termination preds
;note affects just holds the next pred in line for a term.

(defun 3ssra-init-affects ()
  (dolist (term terms)
    (setf (3AFFECTED term) nil)
    (repeat ((waiting [term]) seg)
      (setq seg (when-pop-fails waiting (return)))
      (cond ((and (not (eq seg term)) (memq seg terms))
             (setf (3AFFECTED term) [seg]))
            ((memq seg tjoins)
             (setf (term-join-cor term) seg)
             (setf (term-join-cor seg) term))
            ((and (not (eq seg body)) (not (eq seg rec)))
             (setq waiting
                   (append (Rlist (~seg (~to (Elist (??& bodypat
                                                         cflow (&* seg))))))
                           waiting)))))))

;compute how much inner ungrouping to do

(defun 3ssra-ungroup ()
  (repeat ((prescan todo) ptype seg subs)
    (setq seg   (when-pop-fails prescan (return))
          ptype (?plantype seg))
    (cond ((memq ptype '(expr and comp))          ;ungroup these for sure
           (setq subs (makefset (??subseg seg))
                 todo (fsetU (delq seg todo) subs))
           (setq prescan (append subs prescan))
           (letS* (((aseg . asegs) (Elist 3AFFECTED)))
             (cond ((memq seg asegs)
                    (setf (3AFFECTED aseg) [!! subs !!(delq seg asegs)]))))
           (letS* (((pred . actions) (Elist pred-action-cor)))
             (cond ((memq seg actions)
                    (setf (pred-action-cor pred)
                          [!! subs !! (delq seg actions)]))))
           (ungroup seg))
          ((eq ptype 'xor)                          ;check if can ungroup
           (let* ((newptran (letS* ((o (Elist (??output seg))))
                              (Rlist [o !(3dfaffect body [o] porttrans)])))
                  xrole-fillers subs)
             (when (and (letS* ((affects (cdr (Elist newptran))))
                          (Rand-fast
                            (letS* ()
                              (Rand-fast
                                (eq (~seg~segcase (Elist affects)) seg)))))
                        ;xor affects only itself
                        (letS* ((sub (Elist (??role seg 'action))))
                          (Rand-fast
                            (letS* ()
                              (Rand-fast
                                (eq (~seg~segcase
                                      (Elist (3dfaffect seg (??output sub)
                                                        newptran)))
                                    sub))))))
               ;each action affects only itself
               (setq xrole-fillers (??roles seg))
               (setf (3AFFECTED (first (xrole-fillers 'pred)))
                     (xrole-fillers 'action))
               (setq subs (makefset [!!(xrole-fillers 'init)
                                     !!(xrole-fillers 'action)
                                     ! nil]) ;must protect actions!
                     todo (fsetU (fsetU (delq seg todo) subs)
                                 (xrole-fillers 'pred))
                     prescan [!! subs ! prescan])
               (setf (filter-join-cor (first (xrole-fillers 'pred)))
                     (first (xrole-fillers 'join)))
               (setf (filter-join-cor (first (xrole-fillers 'join)))
                     (first (xrole-fillers 'pred)))
               (setf (pred-action-cor (car (xrole-fillers 'pred)))
                     (xrole-fillers 'action))
               (letS* (((aseg . asegs) (Elist 3AFFECTED)))
                 (cond ((memq seg asegs)
                        (setf (3AFFECTED aseg) [(car (xrole-fillers 'pred))
                                                !! subs
                                                !! (delq seg asegs)]))))
               (ungroup seg)))))))

;construct a list of the non-join segs in cflow order
;(note trouble with filter joins).

(defun 3ssra-compute-seg-order ()
  (repeat ((segs [(~seg (~from (?& bodypat cflow * (&* rec))))]) seg)
    (setq seg (pop segs))
    (if (eq seg body) (return))
    (push seg seg-order)
    (let ((prior-segs (~set ~seg ~from (??& bodypat cflow * (&* seg)))))
      (cond ((null (cdr prior-segs))
             (if ;if filter pred it is there
              (not (memq (car prior-segs) segs))
              (push (car prior-segs) segs)))
            (t
             (push (or (filter-join-cor seg)
                       (barf "3SSR-ANALYZE: Bad join in SSR.~% Seg: ~S"
                             seg))
                   segs)
             (setq segs (append (delq (top segs) prior-segs) segs)))))))

;now construct the data flow part of the "3AFFECTED" relation

(defun 3ssra-build-affected ()
  (dolist (seg non-join-segs)
    (repeat ((bodypat (&* body))
             (tdo (??output seg))
             (affects (3AFFECTED seg))
             done port)
      (when (null tdo)
        (setf (3AFFECTED seg) (makefset affects))
        (return))
      (setq port (pop tdo))
      (when (not (member port done))
        (push port done)
        (dolist (df (??& bodypat dflow port))
          (let* ((to (~to df))
                 (toseg (~seg~segcase to))
                 (trans (porttrans to)))
            (if (not (memq toseg affects))
                (cond (trans (setq tdo (append trans tdo)))
                      ((3joinp toseg)
                       (setq tdo
                             (append (??& (&* toseg) output * (~id to)) tdo)))
                      (t
                       (push toseg affects)
                       (letS* (((pred . actions) (Elist pred-action-cor)))
                         (if (and (memq toseg actions)
                                  (not (memq seg actions))
                                  (not (memq pred affects)))
                             (push pred affects))))))))))))

;this returns a list of the ports reachable from the ports given.
;porttrans is used to account for the identity of the rec and the body

(defun 3dfaffect (supseg ports porttrans)
  (repeat ((supsegpat (&* supseg))
           (todo ports)
           done affected port)
    (setq port (when-pop-fails todo (return affected)))
    (when (not (member port done))
      (push port done)
      (dolist (df (??& supsegpat dflow port))
        (let* ((to (~to df))
               (toseg (~seg~segcase to))
               (trans (porttrans to)))
          (cond (trans (setq todo (append trans todo)))
                ((3joinp toseg)
                 (setq todo
                       (append (??& (&* toseg) output * (~id to))
                               todo)))
                (t (push to affected))))))))

; Look for an augmentation, filter or term and pull it out.
(defun 3ssra-remove-fragment ()
  (prog ()
    (let* (headseg othersegs terminal? filter? junk)
      (dolist (bucket 3AFFECTED)
        (let* (((seg . affects) bucket))
          (when (and (not (term-join-cor seg))
                     (fset< (fset^ affects non-join-segs) [seg]))
            (setq headseg seg)
            (push bucket junk)
            (return))))
      (if (null headseg)
          (let* ((buckets (3ssr-find-removable-group
                           3AFFECTED term-join-cor non-join-segs))
                 (segs (Rbag (car (Elist buckets)))))
            (when buckets
              (setq headseg (car segs))
              (dolist (seg (cdr segs))
                (if (memq seg (memq headseg seg-order))
                    (psetq headseg seg seg headseg))
                (push seg othersegs)
                (assert-fact (&~to (-& (&* body) cflow * (&* seg))
                                  (~to (-& (&* body) cflow (&* seg))))))
              (setq junk (append buckets junk)))))
      (when (null headseg)
        (when (and (not *separate-gens-and-terms*)
                   (< (length (letS ((seg (car (Elist 3affected))))
                                (Rlist (Fselect seg (term-join-cor seg)))))
                      2))
          (setq 3affected nil) (return))
        (dolist (bucket 3AFFECTED)
          (let* (((seg . affects) bucket))
            (when (and (term-join-cor seg)
                       (fset< (fset^ affects non-join-segs) [seg])
                       (let (affected-before now-after)
                         (letS* ((s (Elist seg-order)))
                           (cond (now-after
                                  (if (memq seg (3AFFECTED s)) (done nil)))
                                 (t (cond ((eq seg s)
                                           (if (not affected-before) (done t))
                                           (setq now-after t))
                                          ((and (not (term-join-cor s))
                                                (memq seg (3AFFECTED s)))
                                           (setq affected-before t)))))
                           T)))
              (setq headseg seg)
              (push bucket junk)
              (return)))))
      (cond ((null headseg)
             (grumble "SSR-ANALYZE: Couldn't completely analyze.~% SSR: ~S"
                      ssr)
             (setq 3AFFECTED nil))
            (t
             (setq filter? (filter-join-cor headseg)
                   terminal? (term-join-cor headseg))
             (if (not (or filter? terminal? NO-GROUP-IN-TC
                          (??& * slot (&* headseg))))
                 (repeat ()
                   (if (not
                        (letS* ((bucket (Elist 3AFFECTED))
                                ((seg . affects) bucket))
                          (Ror (cond ((and (not (memq bucket junk))
                                           (not (term-join-cor seg))
                                           (not (filter-join-cor seg))
                                           (not (??& * slot (&* seg)))
                                           (qset< affects [headseg ! othersegs])
                                           (null (??& (&* ssr) toutput (&* seg))))
                                      (push bucket junk)
                                      (if (memq seg (memq headseg seg-order))
                                          (psetq headseg seg seg headseg))
                                      (push seg othersegs)
                                      (assert-fact
                                        (&~to (-& (&* body) cflow * (&* seg))
                                              (~to (-& (&* body) cflow
                                                       (&* seg)))))
                                      t)))))
                       (return))))
             (setq 3AFFECTED (qset- 3AFFECTED junk)
                   non-join-segs (qset- non-join-segs [headseg ! othersegs]))
             (if othersegs
                 (let ((newheadseg
                         (3exprize [[headseg 'action]
                                    ! (Rlist (list (Elist othersegs)
                                                   'action))])))
                   (dolist (seg (cons headseg othersegs))
                     (dolist (toutputF (--& * toutput (&* seg)))
                       (let* ((newtoutputF
                                (assert-fact
                                  (&~lower-env toutputF (& newheadseg)))))
                         (dolist (dflowF (--& * dflow toutputF))
                           (assert-fact (&~from dflowF newtoutputF)))))
                     (dolist (toutputF
                               (--& * toutput * * (&* (&* seg) output)))
                       (let* ((newport (>& (& newheadseg) output nil
                                          (genobj newheadseg (~obj toutputF)) nil))
                              (newtoutputF
                                (assert-fact (&~lower-io toutputF newport))))
                         (>& (& newheadseg) dflow (~lower-io toutputF) newport)
                         (dolist (dflowF (--& * dflow toutputF))
                           (assert-fact (&~from dflowF newtoutputF))))))
                   (setq headseg newheadseg)))
             (temporal-decompose [headseg ! (or filter? terminal?)]
                                 (cond (filter? 'filter)
                                       (terminal? 'term)
                                       (t 'aug))))))))

; Find the shortest group closed under effects, excluding terms.
(defun 3ssr-find-removable-group (3AFFECTED term-join-cor non-join-segs)
  (let* ((segs (letS* ((x (car (Elist 3affected))))
                 (Rbag (Fselect x (not (term-join-cor x))))))
         (subsets (Rlist (3ssr-subsets1 (Elist segs)
                                      3AFFECTED term-join-cor non-join-segs)))
         (shortest
           (car (sort (Rbag (filterS #'cdr (Elist subsets)))
                      #'(lambda (x y) (< (length x) (length y)))))))
    (Rlist (assq (Elist shortest) 3affected))))

(defun 3ssr-subsets1 (start-seg 3AFFECTED term-join-cor non-join-segs)
  (prog (to-do result seg)
    (setq result (ncons start-seg))
    (setq to-do (3AFFECTED start-seg))
 L  (if (null to-do) (return result))
    (setq seg (pop to-do))
    (if (or (memq seg result) (not (memq seg non-join-segs))) (go L))
    (if (term-join-cor seg) (return nil))
    (push seg result)
    (setq to-do (append to-do (3affected seg)))
    (go L)))

(def-alistq newmap)
(def-alistq newportmap)
(declare (special copy-cflow term-join-cor))
(declare (special newmap datatouts newportmap dominant-segcase
                  keyseg join type))

(defun temporal-decompose (stuff type) ;type is aug/term/filter
  (let* (((keyseg . join) stuff)
         (newmap [!(3copy-ssr-skeleton ssr tc) [keyseg ! keyseg]
                  !(if join [[join ! join]])])
         datatouts newportmap dominant-segcase copy-cflow
         (*DB-CHECK-FACT-PORTS* NIL))
    (pp-trace *ANALYZE-Debug*
              [[ 'temporal-decompose [keyseg ! join] type]
               (newmap ssr)])
    (td-find-dominant)
    (td-copy-output-dflow)
    (td-copy-input-dflow)
    (3copy-cflow (&* body) body (& body) newmap [body rec])
    (td-move-toutputs)
    (if (and dominant-segcase (null (??& (&* (newmap ssr)) tinput)))
        (>& (& tc) dflow
            (>& (& ssr) toutput dominant-segcase (genobj ssr 'dummy) nil)
            (>& (& (newmap ssr)) tinput (& keyseg)
                (genobj (newmap ssr) 'dummy) nil)))
    (td-move-segs)
    (td-maybe-has-become-simple-seg)
    (newmap ssr)))

(defun td-maybe-has-become-simple-seg ()
  (when (and (??& (&* ssr) segtype 'split)
             (= (length (??& (&* (?supseg ssr)) cflow (&* ssr))) 1))
    (assert-fact (&~from (-& (&* body) cflow (&* rec)) (& rec)))
    (assert-fact (&~to (-& (&* body) cflow * (&* body)) (& body)))
    (assert-fact (&~from (-& (&* ssr) cflow (&* body)) (& body)))
    (assert-fact (&~to (-& (&* ssr) cflow * (&* ssr)) (& ssr)))
    (assert-fact (&~from (-& (&* (?supseg ssr)) cflow (&* ssr)) (& ssr)))
    (assert-fact (&~type (-& (&* rec) segtype) 'simple))
    (assert-fact (&~type (-& (&* body) segtype) 'simple))
    (assert-fact (&~type (-& (&* ssr) segtype) 'simple))))

(def-alistq ssr-roles)
(defun 3copy-ssr-skeleton (ssr tc)
  (let* ((ssr-roles (??roles ssr))
         (newssr (genseg 'ssr))
         (newssrcase (& newssr))
         (newbody (genseg 'body))
         (newbodycase (& newbody))
         (newdj (genseg 'dfj))
         (newdjcase (& newdj))
         (newmap [[ssr ! newssr]
                  [(car (ssr-roles 'body)) ! newbody]
                  [(car (ssr-roles 'dfjoin)) ! newdj]
                  ! (letS* ((seg (Elist (ssr-roles 'rec)))
                            (nrec (genseg seg)))
                      (+& newbodycase recursive (& nrec))
                      (+& (& nrec) segtype 'simple)
                      (+& newssrcase role (& nrec) 'rec)
                      (+& newbodycase subseg (& nrec))
                      (Rlist (cons seg nrec)))]))
    (+& (& tc) subseg newssrcase)
    (+& newssrcase segtype 'simple)
    (+& newbodycase segtype 'simple)
    (+& newssrcase plantype 'ssr)
    (+& newbodycase plantype 'ssr-body)
    (+& newssrcase subseg newbodycase)
    (+& newssrcase role newbodycase 'body)
    (+& newbodycase subseg newdjcase)
    (+& newssrcase role newdjcase 'dfjoin)
    (+& newdjcase segtype 'join)
    (+& newssrcase cflow newssrcase newbodycase)
    (+& newssrcase cflow newbodycase newssrcase)
    newmap))

(defun td-find-dominant ()
  (repeat ((seg keyseg) segcase)
    (setq segcase (~from (a?& (&* body) cflow * (&* seg)))
          seg (~seg segcase))
    (if (eq seg body) (return))
    (cond ((term-join-cor seg)
           (setq dominant-segcase segcase)
           (return))
          ((filter-join-cor seg)
           (cond ((eq 'join (?segtype seg))
                  (setq seg (filter-join-cor seg)))
                 (t (setq dominant-segcase segcase)
                    (return)))))))

(defun td-copy-output-dflow ()
  (do ((todo (let ((outs (??output keyseg)))
               (setq newportmap (letS ((out (Elist outs)))
                                 (Rlist (cons out out))))
               outs))
       (done nil)
       (tokill nil))
      (nil)
   retry
    (when (null todo)
      (dolist (f tokill) (make-fact-be-out f))
      (return))
    (when (memq (top todo) done)
      (pop todo)
      (go retry))
    (push (top todo) done)
    (setq datatouts
          (nconc (--& (&* ssr) toutput * * (top todo)) datatouts))
    (dolist (df (--& (&* body) dflow (pop todo)))
      (let* ((to (~to df))
             (toseg (~seg~segcase to))
             (totype (cond ((eq toseg rec) 'rec)
                           (t (?segtype toseg))))
             (newfrom (newportmap (~from df))))
        (cond ((or (eq toseg keyseg) (eq toseg join))
               (>& (& (newmap body)) dflow newfrom to))
              ((and (eq totype 'join) (filter-join-cor toseg))
               (--& to @io)
               (make-fact-be-out to)
               (dolist (out (??& (&* toseg) output * (~id to)))
                 (--& out @io)
                 (make-fact-be-out out)
                 (push out todo)
                 (setf (newportmap out) newfrom)))
              ((eq toseg dj)
               (let ((id-port (~id-port to)))
                 (when (null (newportmap id-port))
                   (setf (newportmap id-port) (=port-seg id-port (newmap dj)))
                   (push id-port todo)
                   (push id-port tokill))
                 (make-fact-be-out to)
                 (setf (newportmap to)
                       (>& (& (newmap toseg) (~case (~segcase to))) input 'id
                           (genobj (newmap toseg) (~obj to))
                           (~obj (newportmap id-port)))))
               (>& (& (newmap body)) dflow newfrom (newportmap to)))
              ((eq toseg rec)
               (setf (newportmap to) (=port-seg to (newmap toseg)))
               (make-fact-be-out to)
               (>& (& (newmap body)) dflow newfrom (newportmap to))
               (dolist (p (porttrans to))
                 (push p todo)
                 (setf (newportmap p) (=port-seg p (newmap (~seg~segcase p))))
                 (make-fact-be-out p)
                 (dolist (idf (--& (&* ssr) dflow * p))
                   (let* ((from (~from idf))
                          (tcdf (?& (&* tc) dflow * from)))
                     (when (null (??& (&* ssr) dflow from))
                       (make-fact-be-out from)
                       (make-fact-be-out tcdf))
                     (when (not (newportmap from))
                       (setf (newportmap from) (=port-seg from (newmap ssr)))
                       (assert-fact (&~to tcdf (newportmap from))))
                     (>& (& (newmap ssr)) dflow
                         (newportmap from) (newportmap p))))))
              ((eq toseg body)
               (setf (newportmap to) (=port-seg to (newmap toseg)))
               (make-fact-be-out to)
               (>& (& (newmap body)) dflow newfrom (newportmap to))
               (dolist (p (porttrans to))
                 (push p todo)
                 (setf (newportmap p) (=port-seg p (newmap (~seg~segcase p))))
                 (make-fact-be-out p)
                 (dolist (odf (--& (&* ssr) dflow to))
                   (let* ((oto (~to odf)))
                     (setf (newportmap oto) (=port-seg oto (newmap ssr)))
                     (make-fact-be-out oto)
                     (>& (& (newmap ssr)) dflow
                         (newportmap to) (newportmap oto))
                     (dolist (dflowF (--& (&* tc) dflow oto))
                       (assert-fact (&~from dflowF (newportmap oto))))))))
              (t (barf "TEMPORAL-DECOMPOSE: Trouble with dflow.~% DF: ~S"
                       df)))))))

(defun td-copy-input-dflow ()
  (dolist (df (--& (&* body) dflow * (&* (&* keyseg) input)))
    (let* ((from (~from df)))
      (cond ((and (eq (~seg~segcase from) body)
                  (letS* (((rec mapto) (Elist porttrans)))
                    (Rand-fast (or (not (equal mapto from))
                                   (??& (&* body) dflow from rec)))))
             (setf (newportmap from) (=port-seg from (newmap body)))
             (let* ((idf (?& (&* ssr) dflow * from))
                    (idfrom (~from idf))
                    (tcdf (?& (&* tc) dflow * idfrom))
                    (otherdfs (??& (&* body) dflow from))
                    (move-all?
                      (Rand-fast (memq from
                                       (porttrans (~to (Elist otherdfs)))))))
               (when move-all? (make-fact-be-out from))
               (setf (newportmap idfrom) (=port-seg idfrom (newmap ssr)))
               (assert-fact (&~to tcdf (newportmap idfrom)))
               (>& (& (newmap ssr)) dflow (newportmap idfrom) (newportmap from))
               (>& (& (newmap body)) dflow (newportmap from) (~to df))
               (when move-all?
                 (make-fact-be-out idf)
                 (when (null (??& (&* ssr) dflow idfrom))
                   (make-fact-be-out idfrom)
                   (make-fact-be-out tcdf))
                 (setq datatouts
                       (nconc (--& (&* ssr) toutput * * from) datatouts)))
               (dolist (df otherdfs)
                 (let* ((to (~to df)))
                   (cond ((memq from (porttrans (~to df)))
                          (when move-all?
                            (make-fact-be-out df)
                            (make-fact-be-out to))
                          (setf (newportmap to)
                                (=port-seg to (newmap (~seg~segcase to))))
                          (>& (& (newmap body)) dflow
                              (newportmap from) (newportmap to))))))))
            (t
             (>& (& tc) dflow
                 (let ((le (repeat ((segcase (~segcase (~to df)))
                                    seg (from-seg (~seg~segcase from)))
                             (setq seg (~seg segcase))
                             (cond ((eq seg from-seg) (return segcase))
                                   ((eq seg body)
                                    (barf "TEMPORAL-DECOMPOSE: Can't find l-env~
                                           ~% From: ~:=" from))
                                   ((eq seg (~seg dominant-segcase))
                                    (3update-affected from seg porttrans body)
                                    (return segcase)))
                             (setq segcase
                                   (~from (a?& (&* body) cflow
                                               * (&* (~seg segcase))))))))
                   (or (~01 (??& (&* ssr) toutput le * from))
                       (>& (& ssr) toutput le (genobj ssr (~obj from)) from)))
                 (>& (& (newmap ssr)) tinput (& keyseg)
                     (genobj (newmap ssr) (~obj (~to df))) (~to df))))))))

(defun 3copy-cflow (usegpat seg source newmap br)
  (cond ((memq seg copy-cflow) nil)
        (t (push seg copy-cflow)
           (dolist (cf (??& usegpat cflow (&* seg)))
             (let* ((to (~to cf))
                    (the-source (or source (~from cf))))
               (cond ((newmap (~seg to))
                      (>& (& (newmap (~seg usegpat))) cflow
                          (& (newmap (~seg the-source))
                             (if (not (memq (~seg the-source) br))
                                 (~case the-source)))
                          (& (newmap (~seg to))
                             (if (not (memq (~seg to) br)) (~case to))))
                      (3copy-cflow usegpat (~seg to) nil newmap br))
                     ((not (and (term-join-cor (~seg to))
                                (eq (?segtype (~seg to)) 'join)))
                      (3copy-cflow usegpat (~seg to)
                                   the-source newmap br))))))))

(defun td-move-toutputs ()
  (dolist (dt datatouts)
    (let* ((newsegcase (& (newmap ssr) (~case (~segcase dt))))
           (le (~lower-env dt))
           (newle (cond ((eq (~seg le) keyseg) le)
                        ((null (cdr le)) (& (newmap (~seg le))))
                        (t (barf "TEMPORAL-DECOMPOSE: Cannot move lower io~
                                  ~% DT: ~S" dt))))
           (newlio (newportmap (~lower-io dt)))
           (newdt (>& newsegcase toutput newle
                     (genobj (newmap ssr) (~obj dt)) newlio)))
      (if (not (and newle newlio))
          (barf "TEMPORAL-DECOMPOSE: Can't move toutput.~% DT: ~S" dt))
      (dolist (dflowF (--& (&* tc) dflow dt))
        (assert-fact (&~from dflowF newdt)))))
  (dolist (ct (??& (&* ssr) toutput (&* keyseg)))
    (cond ((null (~lower-io ct))
           (let* ((nct (>& (& (newmap ssr)) toutput (~lower-env ct)
                          (genobj (newmap ssr) (~obj ct)) nil)))
             (dolist (dflowF (--& (&* tc) dflow ct))
               (assert-fact (&~from dflowF nct)))))
          (t
           (let* (new-le nct2 ncti nct)
             (repeat ((seg keyseg) segcase
                      (lio (~lower-io ct))
                      (lioseg (~seg~segcase (~lower-io ct))))
               (setq segcase (~from (a?& (&* body) cflow * (&* seg)))
                     seg (~seg segcase))
               (cond ((eq seg lioseg)
                      (setq new-le segcase)
                      (return))
                     ((eq seg body)
                      (barf "TEMPORAL-DECOMPOSE: Can't find lower-env~% CT: ~S"
                            ct))
                     ((eq seg (~seg dominant-segcase))
                      (3update-affected lio seg porttrans body)
                      (setq new-le segcase)
                      (return))))
             (setq nct2
                   (or (car (??& (&* ssr) toutput new-le * (~lower-io ct)))
                       (assert-fact (&~lower-env ct new-le)))
                   ncti
                   (cond ((let ((df (car (??& (&* tc) dflow nct2
                                              (&* (& (newmap ssr)) tinput
                                                  (& keyseg))))))
                            (if df (~to df))))
                         (t (>& (& (newmap ssr)) tinput (& keyseg)
                                (genobj (newmap ssr) (~obj ct)) nil)))
                   nct
                   (>& (& (newmap ssr)) toutput (~lower-env ct)
                       (genobj (newmap ssr) (~obj ct)) ncti))
             (>& (& tc) dflow nct2 ncti)
             (dolist (dflowF (--& (&* tc) dflow ct))
               (assert-fact (&~from dflowF nct))))))
    (erase-fact ct)))

(defun 3update-affected (from dominant-seg porttrans body)
  (repeat (from-seg)
    (setq from-seg (~seg~segcase from))
    (when (not (eq from-seg body))
      (setf (3AFFECTED from-seg)
            (fsetU (3AFFECTED from-seg) [dominant-seg]))
      (return))
    (setq from (letS* (((fport . tports) (Elist porttrans)))
                (cond ((member from tports) (done fport)))))
    (setq from (~from (?& (&* body) dflow * from)))))

(defun td-move-segs ()
  (cond ((eq type 'aug)
         (-subseg body keyseg)
         (+subseg (newmap body) keyseg)
         (>& (& body) cflow
             (~from (-& (&* body) cflow * (&* keyseg)))
             (~to (-& (&* body) cflow (&* keyseg)))))
        ((eq type 'filter)
         (-subseg body keyseg)
         (+subseg (newmap body) keyseg)
         (-subseg body join)
         (+subseg (newmap body) join)
         (--& (&* body) cflow (&* keyseg) (&* join))
         (>& (& body) cflow
             (~from (-& (&* body) cflow * (&* keyseg)))
             (~to (-& (&* body) cflow (&* join)))))
        (t
         (-subseg body keyseg)
         (+subseg (newmap body) keyseg)
         (--& (&* body) cflow (&* keyseg) (&* join))
         (>& (& body) cflow
             (~from (-& (&* body) cflow * (&* keyseg)))
             (~to (-& (&* body) cflow (&* keyseg))))
         (-subseg body join)
         (+subseg (newmap body) join)
         (let* ((cf1 (car (??& (&* body) cflow (&* rec) (&* join))))
                (cf2 (car (??& (&* body) cflow (&* join) (&* body))))
                (segcase
                  (cond ((and cf1 cf2)
                         (make-fact-be-out cf1)
                         (make-fact-be-out cf2)
                         (prog1
                           (~to (-& (&* tc) cflow
                                    (~to (-& (&* ssr) cflow (~to cf2)))))
                           (if (null (??& (&* body) cflow (&* rec)))
                               (>& (& body) cflow (& rec) (& body)))
                           (if (null (??& (&* ssr) cflow (&* body)))
                               (>& (& ssr) cflow (& body) (& ssr)))))
                        (t (repeat ((segcase (& join)))
                             (if (eq (~seg segcase) tc) (return segcase))
                             (setq segcase (~to (?& * cflow segcase))))))))
           (>& (& tc) cflow (& (newmap ssr)) segcase))))
  (>& (& (newmap ssr)) role (& keyseg) 'op)
  (cond ((memq type '(term filter))
         (>& (& tc) role (& (newmap ssr)) type))
        ((null (??& (& (newmap ssr)) tinput))
         (>& (& tc) role (& (newmap ssr)) 'gen))
        ((null (??& (& (newmap ssr)) output))
         (>& (& tc) role (& (newmap ssr)) 'trn))
        (t (>& (& tc) role (& (newmap ssr)) 'acc))))

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp REPEAT Indent:1;
;;; Lisp REPEAT* Indent:1;
;;; End:;
