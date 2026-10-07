;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 251-290; listing db.

;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; This is a concise listing of each kind of fact that can exist
;;; in the database, annotated with the fields it has and notes about
;;; what those fields can contain.

;;; (segcase INPUT kind obj id)  kind={NIL,ID}
;;; (segcase OUTPUT kind obj id)  kind={NIL,ID,PART}
;;;
;;; (segcase TINPUT lower-env obj lower-io)
;;; (segcase TOUTPUT lower-env obj lower-io)
;;;
;;; (segcase EXPECT wff)
;;; (segcase ASSERT wff)
;;; (segcase WHEN wff)
;;;
;;; (segcase SEGTYPE type)  type={SPLIT, JOIN, SIMPLE}
;;;
;;; (segcase SUBSEG subseg)
;;;
;;; (segcase DFLOW from to)
;;; (segcase CFLOW from to)
;;;
;;; (segcase PLANTYPE type)   type={EXPR,AND,COMP,XOR,PRED,SSR,SSR-BODY,
;;;                                 TEMPCOMP,JOIN,CONSTANT,BUILTIN,NEW-VIEW}
;;;
;;; (segcase ROLE roleseg rolename)  rolename={ACTION,INIT,PRED,JOIN,REC,
;;;                                            OP,BODY,DFJOIN,BSSR,GEN,
;;;                                            ACC,TRN,TERM,FILTER}
;;;
;;; (segcase SLOT slotseg slotname slottype)  slottype={INPUT,OUTPUT,SEGMENT}
;;;
;;; (segcase CLICHE clichename)
;;; (segcase CLICHE-DEF cliche-def-name cliche-def-comments)
;;;
;;; (segcase RECURSIVE subseg)
;;;
;;; (segcase @SEG kind name code)  kind={OPENCODE,FUNCALL,CONSTANT,
;;;                                      LAMBDA-EXP,JOIN,RECURSION}
;;;
;;; (deep @IO kind name argno)  kind={LAMBDA-ARG,ACTUAL-ARG,FREE-VAR,
;;;                                   RETURNVAL,SIDE-EFFECT,DUMMY}
;;;
;;; (deep @CREATEVAR varname)

;;; DB: Database for the Knowledge-Based Editor

;;; Implementation Notes:
;;;
;;;  This file is now free of @names!
;;;
;;;  This file contains no explicit (<- ...) calls.
;;;
;;;  All (LOOP ...)'s have been updated to (REPEAT ...) or (REPEAT* ...)
;;;
;;;  This file contains no (BEGIN ...)
;;;
;;;  This file seems to no longer depend on (MATCH ...).
;;;  This file looks clear of all but 2-arg IF.
;;;  This file DOES still depend on (OVER ..).

;;;; ...

;;; (Re)Initializes the DB

(defun db-init ()
  (mapc #'db-init-sym *db-segs*)
  (db-init-vars))

;;; (MAKE-INTERNED-CMARKER in? in out)
;;;
;;;  Returns a cmarker whose IN-LAYERS, OUT-LAYERS, and CMARKER-IS-IN?
;;;  fields are the values given by the arguments. If possible, returns
;;;  an existing cmarker, otherwise conses a fresh one.
;;;
;;;  Note: Although we attempt here to eliminate duplicate cmarkers,
;;;        this notion is not rigidly enforced elsewhere.

(defsubst maybe-find-existing-cmarker (in out)
  (dolist (cmarker *cmarkers*)
    (if (and (equal in  (in-layers cmarker))
             (equal out (out-layers cmarker)))
        (return cmarker))))

(defun make-interned-cmarker (in? in out)
  (cond ((null in) *null-cmarker*)
        ((maybe-find-existing-cmarker in out))
        (t (top (db-push (build-cmarker in? in out nil) *cmarkers*)))))

;;; These functions manipulate contexts.
;;;
;;; Notes:
;;;
;;;  The functions have to work a bit because pre-computation is done
;;;  on the cmarkers.
;;;
;;;  The NEW-CMARKER field is just set to nil in order to save time
;;;  here, and in order to save creating cmarkers that may never be used.
;;;
;;;  PUSH-DB-CONTEXT cannot change whether something is IN or OUT and is
;;;  therefore particularly fast.

(defsubst invalidate-new-cmarkers ()
  (setf (new-cmarker *null-cmarker*) nil)     ;invalidate *null-cmarker*
  (dolist (cmarker *cmarkers*)                ; and all the real cmarkers'
    (setf (new-cmarker cmarker) nil)))

(defun push-db-context ()
  (incf *db-context-count*)
  (setq *db-context* (db-cons *db-context-count* *db-context*))
  (invalidate-new-cmarkers)
  *db-context*)

(defun pop-db-context ()
  (set-db-context (cdr *db-context*)))

(defun set-db-context (new-context)
  (setq *db-context* (sort (db-copylist new-context) #'>))
  (setf (new-cmarker *null-cmarker*) nil) ;Note this is essential
  (let ((in-layers) (out-layers))
    (dolist (cmarker *cmarkers*)
      (setq in-layers  (in-layers  cmarker))
      (setq out-layers (out-layers cmarker))
      (setf (cmarker-is-in? cmarker)
            (dolist (layer *db-context*)
              (cond ((member layer  in-layers) (return t  ))
                    ((member layer out-layers) (return nil)))))
      (setf (new-cmarker cmarker) nil))
    *db-context*))

;;;; Making facts be IN

;;; Overview:
;;;
;;;  These are the fns for making something IN in the db. Each of these
;;;  functions returns the fact which was made IN. In the case of
;;;  ASSERT-FACT, this may not necessarily be EQ to the fact given as
;;;  an argument.
;;;
;;;  There are also abstraction macros, +& and >& which compose the
;;;  behavior of ADD-FACT or ASSERT-FACT (respectively) and the & macro.


;;;  (MAKE-FACT-BE-IN fact)
;;;
;;;   Makes a fact that is already indexed in the database be IN.

(defun make-fact-be-in (fact)
  (let ((c (cmarker fact)))
    (setf (cmarker fact)
          (cond ((cmarker-is-in? c) c)
                ((new-cmarker c))
                (t (setf (new-cmarker c)
                         (make-interned-cmarker T
                           (add-layer (car *db-context*) (in-layers  c))
                           (del-layer (car *db-context*) (out-layers c))))
                   (new-cmarker c)))))
  (when-db-traced (db-display-trace-info ADD fact))
  fact)

;;;  (ADD-FACT fact)            ;see also +&
;;;
;;;   Indexes a fact into the data base, and then makes it be IN.
;;;   Note that it is imperative that an equal fact not already be
;;;   indexed in the db whether it is IN or not!

(defun add-fact (fact)                                          ;+&
  (if (and *db-check-facts* (null (audit-fact fact nil)))
      (barf "ADD-FACT: Syntactically incorrect fact.~
            ~% Notes: ~:=~
            ~% Offending datum: ~:="
             *db-audit* fact))
  (dolist (hunk-and-num (index-fact fact))
    (db-push fact (dbx (cdr hunk-and-num) (car hunk-and-num))))
  (make-fact-be-in fact))

;;;  (ASSERT-FACT fact)         ;see also >&
;;;
;;;   Takes a fact, checks to see if it is already indexed, indexes it
;;;   if it isn't, and then makes it be IN.
;;;
;;;   Note:  ASSERT-FACT is the safe function to call if you have any
;;;          doubts at all about whether the fact is already indexed in
;;;          the database or not.

(defun assert-fact (fact)                                       ;>&
  (if (and *db-check-facts* (null (audit-fact fact nil)))
      (barf "ASSERT-FACT: Syntactically incorrect fact.~
            ~% Notes: ~:=~
            ~% Offending datum: ~:="
             *db-audit* fact))
  (let* ((idxs (index-fact fact))
         (db-fact (dolist (f (dbx (cdar idxs) (caar idxs)))
                    (if (matches-fact? fact f) (return f)))))
    (cond (db-fact (setq fact db-fact))
          (t (dolist (hunk-and-num idxs)
               (db-push fact (dbx (cdr hunk-and-num)
                                  (car hunk-and-num)))))))
  (make-fact-be-in fact))

;;;; Finding DB facts

;;; (FIND-FACTS pattern)
;;;
;;;  This function finds facts in the database.  It takes in a pattern,
;;;  and finds all of the facts that are IN, and which match the pattern.
;;;  A key complexity is dealing with inadequately indexed patterns.
;;;  A list of all the facts found is returned.
;;;
;;;  The macros ??&, ?&, and A?& are available for composing the actions
;;;  of FIND-FACTS and &* with varying kinds of error checking.

(defun find-facts (pattern)
  (setq *db-last-pattern-found* pattern)
  (if (and *db-check-facts* (null (audit-fact pattern t)))
      (barf "FIND-FACTS: Syntactically incorrect pattern.
            ~% Notes: ~:=~
            ~% Pattern: ~:="
             *db-audit* pattern))
  (let* (((hunk . num) (car (index-fact pattern))) facts)
    (setq facts
          (cond (num
                 (cond (hunk (collect (fact (dbx num hunk))
                                      (and (fact-is-in? fact)
                                           (matches-fact? pattern fact))))
                       (t
                        (mapcan #'(lambda (seg)
                                    (collect
                                      (fact (dbx num (db-index seg)))
                                      (and (fact-is-in? fact)
                                           (matches-fact? pattern fact))))
                                *db-segs*))))
                (t (grumble "FIND-FACTS: Very slow retrieval pattern:
                            ~% Pattern: ~:="
                             pattern)
                   (dolist (seg *db-segs*)
                     (let ((hunk (db-index seg)))
                       (over num <- 10. by (1- num) until (minusp num)
                        nconc (collect
                                (fact (dbx num hunk))
                                (and (fact-is-in? fact)
                                     (matches-fact? pattern fact)))))))))
    (when-db-traced (dolist (fact facts)
                      (db-display-trace-info FIND fact)))
    facts))

;this fn gives you all of the facts that have the given seg in their deep.
;it is called by facts? below.  it is needed so that you can get all these
;facts fast without doing a bunch of finds with all the different keywords.

(defun seg-facts (seg)
  (let* ((hunk (db-index seg)) facts)
    (setq facts
          (if hunk
              (over num <- 10. by (1- num) until (minusp num)
               nconc (collect (fact (dbx num hunk)) (fact-is-in? fact)))))
    (when-db-traced (dolist (fact facts)
                      (db-display-trace-info FIND fact)))
    facts))

;;;; ...
;this gives you a list of all the facts that refer to the given port.
;ie @io @createvar dflow tinput toutput input output.
;note that with inputs, outputs, the ref could be indirect through
;the id field, and with a dflow it is remotely possible (though an
;error) that it could refer to the same port twice, in this case
;the dflow will be in the list twice.

(defun port-facts (port)
  (let* ((seg (~seg~segcase port))
         (obj (~obj port))
         (hunk (db-index seg)) facts)
    (cond (hunk
           (over-in-facts (f (dbx 4 hunk))
             (if (eq (~deep f) port) (push f facts)))
           (over-in-facts (f (dbx 5 hunk))
             (if (eq (~deep f) port) (push f facts)))
           (over-in-facts (f (dbx 8 hunk))
             (if (eq (~id f) obj) (push f facts)))
           (over-in-facts (f (dbx 9 hunk))
             (if (eq (~id f) obj) (push f facts)))
           (over-in-facts (f (dbx 10. hunk))
             (if (eq (~deep f) port) (push f facts)))
           (over-in-facts (f (dbx 12. hunk))
             (if (eq (~from f) port) (push f facts)))
           (over-in-facts (f (dbx 13. hunk))
             (if (eq (~to f) port) (push f facts)))
           (over-in-facts (f (dbx 14. hunk))
             (if (and (toutput-fact? f) (eq (~lower-io f) port))
                 (push f facts)))
           (over-in-facts (f (dbx 15. hunk))
             (if (and (tinput-fact? f) (eq (~lower-io f) port))
                 (push f facts)))))
    (when-db-traced (dolist (fact facts)
                      (db-display-trace-info FIND fact)))
    facts))

;these are the fns that make facts out in the data base.  MAKE-FACT-BE-OUT
;takes an actual fact indexed in the db and makes it be out.  ERASE-FACTS
;takes a pattern as input, finds all of the facts that match that pattern,
;and then makes each of those facts be out.  Each fn returns the facts it
;made out.  as a collateral effeciency, MAKE-FACT-BE-OUT keeps count of
;all of the
;facts it makes out which are then not IN in any layer, and from time
;to time it flushes all of them out of the db all together in order to
;save space.  There are macros available: --&, -&, and a-& combining
;the actions of &* and ERASE-FACTS.

(defun make-fact-be-out (fact)
  (cond ((cmarker fact)
         (if (and *db-check-facts* *db-check-fact-ports*)
             (if (port-fact? fact)
                 (let ((facts (port-facts fact)))
                   (if facts
                       (barf "MAKE-FACT-BE-OUT: Deleting a port ~
                              that is referred to.~
                             ~% Fact deleted: ~:=~
                             ~% Relevant Data: ~:="
                             fact facts)))))
         (let ((c (cmarker fact)))
           (setf (cmarker fact)
                 (cond ((not (cmarker-is-in? c)) c)
                       ((new-cmarker c))
                       (t
                        (setf
                          (new-cmarker c)
                          (make-interned-cmarker nil
                            (del-layer (car *db-context*) (in-layers  c))
                            (add-layer (car *db-context*) (out-layers c))))
                        (new-cmarker c)))))
         (when-db-traced (db-display-trace-info ERASE fact))))
  fact)

(defun erase-facts (pattern)
  (let ((facts (find-facts pattern)))
    (dolist (fact facts) (make-fact-be-out fact))
    facts))

(defun erase-fact (pattern)
  (sel1 (erase-facts pattern)))

;;; (KILL-SEG seg)
;;;
;;;  This function gets rid of every fact refering to the indicated
;;;  segment in any way. It is needed so that you can wipe out all
;;;  those facts conveniently without doing dozens of seperate fetches.
;;;  It does not return the facts killed.
;;;
;;;  Note: This might have some far reaching effects if somebody
;;;        refers indirectly to the ports through a tinput or something.

(defun kill-seg (seg)
  (let* ((hunk (db-index seg)))
    (if hunk
        (dolist (num '( 0.  1.  2.  3.  4.  5.  6.  7.     ;up to ports
                       10. 11. 12. 13. 14. 15.            ;after ports
                        8.  9.))                          ;ports last!
          (over-in-facts (f (dbx num hunk))
            (cond ((port-fact? f)
                   (kill-port f))
                  (t
                   (make-fact-be-out f))))))))

;this kills every fact refering to a given port.
;ie @io @createvar dflow tinput toutput input output.
;note that this can have some far reaching transitive effects if
;some tinput or something refers to the port. (note that if there
;was a loop in this refering (a gross bug), this will loop)

(defun kill-port (port)
  (let* ((seg (~seg~segcase port))
         (obj (~obj port))
         (hunk (db-index seg)))
    (when hunk
      (over-in-facts (f (dbx  4 hunk))
        (if (eq (~deep f) port) (make-fact-be-out f)))
      (over-in-facts (f (dbx  5 hunk))
        (if (eq (~deep f) port) (make-fact-be-out f)))
      (over-in-facts (f (dbx  8 hunk))
        (if (eq (~id   f)  obj) (kill-port f)))
      (over-in-facts (f (dbx  9 hunk))
        (if (eq (~id   f)  obj) (kill-port f)))
      (over-in-facts (f (dbx 10. hunk))
        (if (eq (~deep f) port) (make-fact-be-out f)))
      (over-in-facts (f (dbx 12. hunk))
        (if (eq (~from f) port) (make-fact-be-out f)))
      (over-in-facts (f (dbx 13. hunk))
        (if (eq (~to   f) port) (make-fact-be-out f)))
      (over-in-facts (f (dbx 14. hunk))
        (if (and (toutput-fact? f)
                 (eq (~lower-io f) port))
            (kill-port f)))
      (over-in-facts (f (dbx 15. hunk))
        (if (and (tinput-fact? f)
                 (eq (~lower-io f) port))
            (kill-port f)))
      (make-fact-be-out port))))

;delete all assertions in the data base which are not present in any
;subset of the specified context.  This is the function you must use
;in order to restore the db to a prior state.  It makes the assumption
;that only the layers in the specified context are being saved
;anyplace.  all references to any other layers are removed,
;and *db-context-count* is reduced to reflect this fact.

(defun restore-db-context (context)
  (set-db-context context)
  (setq *db-context-count* (car *db-context*))
  (setq *db-context-names*
        (over *db-context-names* -> name
              (symeval name) -> con
         select (cond ((over c <- context fastor (equal con c)))
                      (t (set name nil)))
         bag name))
  (dolist (cmarker *cmarkers*)
    (setf (in-layers cmarker)
      (over (in-layers cmarker)  select (member _ *db-context*) list _))
    (setf (out-layers cmarker)
      (over (out-layers cmarker) select (member _ *db-context*) list _)))
  (push-db-context)
  (prog1 (db-expunge)
         (dolist (seg *db-segs*) (set-up-db-names seg))))

;;;; Unique name generation

;;; This cluster of functions supports the generation of unique names.
;;;
;;; There are three key requirments of the database in this regard:
;;;
;;;   (1) There must not be two segs with the same ~SEG name in the
;;;       db at once.
;;;
;;;   (2) There must not be two ports with the same ~OBJ on the same seg.
;;;
;;;   (3) There must not by two cases with the same ~case name on the
;;;       same seg.
;;;
;;; An additional desideratum is that it should be possible to
;;; back up the naming process, when the db is backed up, so that things
;;; will go along again just as they did before. This helps debugging
;;; an amazing amount.
;;;
;;; Also, symbols take up lots of storage, so we don't want to have any
;;; more names then we need. [This was true in Maclisp. Do we care in
;;; LispM lisp? --kmp]
;;;
;;; The following functions meet all of these criteria.
;;;
;;; The key idea is that every seg in the db has a DB-NAMES property that
;;; holds a list of object names used on it, and the number of the largest
;;; case number used so far.  To get a new
;;; name, you must specify what seg you want it on, and it makes a
;;; name from the root you gave that doesn't clash with any of the names
;;; already in existence. (The criterion for non-clashing of seg names
;;; is that the symbol not already appear in the list held by *DB-SEGS*.)
;;; To get restoration to an earlier time, the DB-NAMES properties are
;;; simply recreated as of that time.
;;;
;;; The way things are set up, they assume that you use
;;; GENSEG, GENOBJ, and GENCASE -always-.

(defun genseg (atom)
  (let* ((seg (genatom-not-in *db-segs* atom)))
    (db-push seg *db-segs*)
    (setf (db-names seg) (db-cons nil 0))
    (setf (db-index seg) (make-dbhunk))
    seg))

;This makes the actual name.

(defun genatom-not-in (list atom)
  (prog (prefix-string new i)
      (if (not (memq atom list)) (return atom))
      (setq prefix-string (string-right-trim ".0123456789" (string atom)))
      (when (not (string-equal prefix-string (string atom)))
        (setq new (intern prefix-string *kbe-package*))
        (if (not (memq new list)) (return new)))
      (setq i 1)
    L (setq new (intern (string-append prefix-string
                          (let ((*nopoint T)) (format nil "~A" i)))
                        *kbe-package*))
      (if (not (memq new list)) (return new))
      (setq i (1+ i))
      (go L)))

;this works for objects.  (note it assumes that the seg is created by
;genseg)

(defun genobj (seg atom)
  (let* ((entry (db-names seg))
         (name (genatom-not-in (car entry) atom)))
    (push name (car entry))
    name))

;this works for cases, however, it runs on different assumptions.  there
;are two ways cases are used.  either they are mnemonic things like
;yes-no or rec-body, in which case they never clash, and are not checked.
;or they are just generated numbers.  All this does for you is give
;you a new unique integer. you can also copy the cases from a join to
;another that has
;none, which is always ok, and you don't call gencase in that situation.

(defun gencase (seg)   ;always uses the root CE
  (let* ((entry (db-names seg))
         (num (1+ (cdr entry))))
    (setf (cdr entry) num)
    num))

;this sets up the db-names lists non-incrementally.
;(see restore-db-context)
;it just looks everywhere an assertion could be using a name, and lists
;them.

(defun set-up-db-names (seg)
  (let* ((entry (db-names seg)))
    (when entry
      (setf (car entry)
            (let ((hunk (db-index seg)) obj objs)
              (dolist (num '(8 9))
                (dolist (port (dbx num hunk))
                  (setq obj (~obj port))
                  (if (not (memq obj objs)) (push obj objs))))
              (db-copylist objs)))
      (setf (cdr entry)
            (let* ((hunk (db-index seg)) (num 0) case)
              (dolist (num '(8 9 10.))
                (dolist (fact (dbx num hunk))
                  (setq case (if (not (fact? (~deep fact)))
                                 (~case (~deep fact))))
                  (if (and (numberp case) (> case num))
                      (setq num case))))
              (dolist (fact (dbx 14. hunk))
                (setq case
                  (cond ((cflow-fact?  fact) (~case (~from      fact)))
                        ((tinput-fact? fact) (~case (~lower-env fact)))))
                (if (and (numberp case) (> case num))
                    (setq num case)))
              (dolist (fact (dbx 15. hunk))
                (setq case
                  (cond ((cflow-fact?   fact) (~case (~to        fact)))
                        ((toutput-fact? fact) (~case (~lower-env fact)))))
                (if (and (numberp case) (> case num))
                    (setq num case)))
              num)))))

;;; (index-fact fact)
;;;
;;;  This indexing function is the heart of the database.
;;;
;;;  Notes: It is important that we know that no fact can ever index
;;;         into the same bucket twice, because it never has the same
;;;         number twice.
;;;
;;;         This function is overloaded, which is good because it means
;;;         both FIND-FACTS and ASSERT-FACT can call it, but it is bad
;;;         because it always does the work necessary for both functions,
;;;         which is likely to be inefficient.

(defun index-fact (fact)
  (let* ((idxs (get (~keyword fact) 'db-keyword-idxs))
         (indecies))
    (over idxs -> idx
     over (cdr fact) until nil -> field
     select idx
      (setq indecies (nconc (cond ((fact? field) (idx-port field idx))
                                  (t (idx-seg field idx)))
                            indecies)))
    (or indecies (over idxs select _ (return [[nil ! _]])))))

;this takes in a segcase, and a bucket idx.
;it returns [[hunk ! idx]] or nil if the segcase is ~*

(defun idx-seg (segcase idx)
  (cond ((or (eq segcase '~*)
             (eq (~seg segcase) '~*)) nil)
        (T (let ((hunk (db-index (~seg segcase))))
             (if hunk (ncons (cons hunk idx)))))))

;this indexes a port fact

(defun idx-port (field idx)
  (cond ((eq field '~*) nil)
        (t (idx-seg (~deep field) idx))))

;this fn compares a pattern and a fact (both hunks) to see if they match.

(defsubst mempat (x y)
  (cond ((and (listp x)
              (or (eq (car x) '*code-is-car*)
                  (eq (car x) '*code-is-cdr*)))
         (dolist (item y)
           (if (and (eq (car x) (car item))
                    (eq (cdr x) (cdr item)))
               (return t))))
        (t (memq x y))))

(defun matches-fact? (pattern fact)
  (cond ((not (and (fact? pattern) (fact? fact))) nil)
        ((not (eq (cmarker pattern) *null-cmarker*))
         (eq pattern fact))
        (T (over (get (~keyword fact) 'db-keyword-types) -> type
             over (cdr fact) until nil -> field
             over (cdr pattern) until nil -> pat
              fastand
               (or (eq pat '~*)
                   (caseq type
                     (S  (match-segcase pat field))
                     (P  (matches-fact? pat field))
                     (SP (cond ((fact? field) (matches-fact? pat field))
                               (t (match-segcase pat field))))
                     (C (or (eq pat field)
                            (and (= (length pat) (length field))
                                 (over pat fastand (mempat _ field)))))
                     ((K NP) (eq pat field))
                     (L  (equal pat field))))))))


;this matches segcases

(defun match-segcase (pat sc)
  (and (not (fact? pat))
       (not (fact? sc))
       (or (eq (~seg  pat) '~*) (eq (~seg  pat) (~seg  sc)))
       (or (eq (~case pat) '~*) (eq (~case pat) (~case sc)))))

;the following routine audits facts.  this simple audit just checks for
;syntactic correctness, and does as much as it can fast without doing
;any searches in the data base.  In a sense these routines are the real
;definition of what is correct in a fact.  the routines are used by
;ASSERT-FACT in order to check it isn't putting garbage in the db.

;Audit one fact/pattern.  the tests are basically the same for the
;two, except that only patterns can have ~*s in them.  If the check is
;supposed to allow this, then the second argument must be T. (note we
;check that each pattern has the right number of fields.)  The special
;variable *db-audit* is set to a list of all the errors
;that were found.

(defun audit-fact (fact pattern?)
  (let (types)
    (setq *db-audit* nil)
    (cond ((not (fact? fact))
           (push "Not a fact//pattern at all" *db-audit*))
          ((eq (~keyword fact) '~*)
           (push 'fact-has-* *db-audit*))
          ((null (setq types (get (~keyword fact) 'db-keyword-types)))
           (push "Bad keyword" *db-audit*))
          ((not (= (length types) (length (cdr fact))))
           (push "wrong number of fields" *db-audit*))
          ((over types -> type
            over (get (~keyword fact) 'db-keyword-rests) until nil -> rest
            over (cdr fact) until nil -> field
             (cond
               ((eq field '~*)
                (push 'fact-has-* *db-audit*))
               ((caseq type
                  (S  (audit-segcase field))
                  (P  (if field (audit-fact field nil)))
                  (SP (cond ((fact? field) (audit-fact field nil))
                            (field (audit-segcase field))))
                  (K  (cond ((not (memq field rest))
                             (push "bad flag field value" *db-audit*))))
                  (NP (cond ((fact? field)
                             (push "NP field is fact" *db-audit*))
                            ((not (symbolp field))
                             (push "NP field not symbol" *db-audit*))
                            ((null field))
                            ((not (or (eq (~deep fact) '~*)
                                      (eq (~seg~segcase fact) '~*)
                                      (memq field
                                            (car (db-names
                                                   (~seg~segcase fact))))))
                             (push "NP not made with GENOBJ" *db-audit*))))
                  ((L C) (cond ((fact? field)
                                (push "field is fact" *db-audit*))))))))))
    (when *db-audit*
      (if pattern? (setq *db-audit* (delq 'FACT-HAS-* *db-audit*)))
      (setq *db-audit* (nreverse *db-audit*)))
    (null *db-audit*)))

(defun audit-segcase (sc)
  (let* (seg case)
    (cond ((fact? sc)
           (push "segcase is fact" *db-audit*))
          ((not (and (listp sc)
                     (setq case (~case sc) seg  (~seg sc))
                     (symbolp seg)
                     (or (symbolp case) (numberp case))))
           (push "Segcase is ill-formed" *db-audit*))
          ((or (eq seg '~*) (eq case '~*))
           (push 'fact-has-* *db-audit*))
          ((not (db-index seg))
           (push "Seg not created with genseg" *db-audit*)))))

;Audit every fact in the db (usefull if you haven't been auditing
;for a while.)

(defun db-audit ()
  (let ((count 0))
    (over *db-segs* -> seg
          (db-index seg) -> hunk
      (over num <- 10 by (1- num) until (minusp num)
        (over (dbx num hunk)
          select (and (fact-is-in? _) (null (audit-fact _ nil)))
          (incf count)
          (grumble "DB-AUDIT found a bad fact.~
                    ~% Notes: ~:=~
                    ~% Offending fact: ~:=" *db-audit* _))))
    count))

;the following fns compress and expand facts.  the goal if the compressed
;form is to get something that is at the same time more compact, and
;easier to read.  The key properties of the compression
;are as follows:
;1. a fact is compressed, by converting it to a list, and compressing
;   each of its fields.
;2. a segcase (SEG . CASE) becomes the list (SEG CASE) and
;   a seg (SEG) is converted to simply the atom SEG.
;3. the keyword field is left unchanged.
;4. the other fields are printed in order as a list.
;5. if a field is a port (eg input output tinput toutput) then it is
;   abbreviated to just [(abbreviated (~segcase field)) (~object field)]
;   note that this is printed out by a grind macro, from an internal form
;   (/[ segcase obj), and that in order to read it back in, you
;   have to have an appropriate read macro defined
;6. if the initial field is a port, then the abbreviation of 5 is done,
;   but in addition, it is spliced into the top level result, and the
;   keyword is inserted before the object.  IE
;      if the deep is a segcase then each field is just compresed:
;    (C-segcase KEY C-field1 C-field2 ...)
;      if the deep is a port then this is broken apart into two pieces:
;    (C-segcase-dep KEY obj-deep C-field1 C-field2 ...)
;   [NOTE that one of the goals of the compaction is that the segcase
;   of the fact will always be the first field of the fact, and the keyword
;   the second]
;7. the most complex compression happens to the code field of an @seg,
;   the problem is that the EQ structure relating various assertions is
;   vital in this situation, and is hard to recover across printing
;   and reading.  when several facts are being compressed, the variable
;   *DB-TOP-FN* is bound to information describing the code of the top
;   level fn.
;   (at other times, this variable is NIL).  Each code element is
;    represented as either itself (if it is not in the top-fn), or a
;    number.  This number
;   is the mouse scan position order of the the piece of code.
;   (counting from the root as 0.)  What *DB-TOP-FN* actually contains is
;    an assq list where the ith entry is a cons of the code with mouse
;    scan position i and i
;8. fields which are lists are copied so that there will be no sharing
;   between the compressed form and the data base.  It is VITAL
;   that there never be any sharing.

;note that compress is designed so that it doesn't crash if you give
;it a pattern.

;here are some examples of the compression:
;{CM (join1) output nil obj1 nil} => (join1 output nil obj1 nil)
;{CM (join1 . 1) segtype simple} => ((join1 1) segtype simple)
;{CM (ssr1) tinput (expr6) foo {CM (expr6) input nil foo  nil}}
;   => (ssr1 tinput expr6 foo [expr6 foo])
;{CM (ssr1) toutput (pred1 . yes) bar
;    {CM (ssr1) tinput (pred1) tuz {CM (pred1) input nil x nil}}}
;   => (ssr1 toutput (pred1 yes) bar [ssr1 tuz])

(defun compress-fact (fact)
  (let ((cfact
          (over (get (~keyword fact) 'db-keyword-types) -> type
           over (cdr fact) -> field
           list (caseq type
                  (S  (compress-segcase field))
                  (P  (compress-port field))
                  (SP (cond ((fact? field) (compress-port field))
                            (t (compress-segcase field))))
                  (C  (compress-code-list field))
                  ((K NP)  field)
                  (T  (copytree field))))))
    (if (and (listp (car cfact)) (eq (caar cfact) '/[))
        (setq cfact `(,(cadar cfact) ,(cadr cfact)
                      ,(caddar cfact) .,(cddr cfact))))
    cfact))

(defun compress-segcase (segcase)
  (cond ((eq segcase '~*) segcase)
        ((~case segcase) [(~seg segcase) (~case segcase)])
        (t (~seg segcase))))

(defun compress-port (port)
  (cond ((eq port '~*) port)
        ((null port) nil)
        ((not (and (fact? port) (fact-is-in? port)))
         (grumble "COMPRESS-FACT: Compressing non-existent subport.~
                  ~% Port: ~:="
                  port)
         nil)
        (t `(/[ ,(compress-segcase (~segcase port)) ,(~obj port)))))

;this expands a compressed fact back out into an actual fact and puts
;it in the DB.  Note that unique seg names are generated so that
;there will be no collision between these new facts and any old ones.
;(NOTE that it is critical that any port fact referred to by the compressed
;fact, must be IN the data base before expansion is attempted.)

;It is imperative that the facts created share no list structure
;with the arguments passed in.


;;; This interacts with plan-def in order to rename segs.
;;; Note: *DB-NEW-NAMES* is bound by PLAN-DEF.
(declare (special *db-new-names*))

(defun expand-fact (cfact)
  (let*  ((types (get (cadr cfact) 'db-keyword-types))
          (cseg (cond ((atom (car cfact)) (car cfact))
                      (T (caar cfact))))
          (fact (db-make-list (1+ (length types)))))
    (if (> (length cfact) (length types))
        (setq cfact `((/[ ,(car cfact) ,(caddr cfact))
                      ,(cadr cfact) ., (cdddr cfact))))
    (rplaca fact *null-cmarker*)
    (over types -> type
     over cfact -> field
     over f <- (cdr fact)
      (rplaca f
        (caseq type
          (S  (expand-segcase field))
          (P  (expand-port field))
          (SP (cond ((and (listp field) (eq (car field) '/[))
                     (expand-port field))
                    (t (expand-segcase field))))
          (C  (expand-code-list field))
          (NP (if field
                  (let ((seg (~seg~segcase fact))
                        (key (cons cseg field)))
                    (cond ((cdr (assoc key *db-new-names*)))
                          (T (let ((o (genobj seg field)))
                               (push (cons key o) *db-new-names*)
                               o))))))
          (K  field)
          (L  (copytree field)))))
    (add-fact fact)))

(defun expand-segcase (csc)
  (let* ((seg  (cond ((atom csc) csc) (t (car csc))))
         (case (cond ((atom csc) nil) (t (cadr csc))))
         (seg-to-use
           (cond ((cdr (assq seg *db-new-names*)))
                 (T (let ((s (genseg seg)))
                      (push (cons seg s) *db-new-names*)
                      s))))
         (case-to-use
           (let ((key (cons seg case)))
             (cond ((symbolp case) case)
                   ((cdr (assoc key *db-new-names*)))
                   (T (let ((c (gencase seg-to-use)))
                        (push (cons key c) *db-new-names*)
                        c))))))
    (& seg-to-use case-to-use)))

(defun expand-port (port)
  (cond ((null port) nil) ;special case for null lower-ios
        (T (let* (((csc old-obj) (cdr port))
                  (old-seg (cond ((atom csc) csc) (T (car csc))))
                  (segcase (expand-segcase csc))
                  (new-obj (cdr (assoc (cons old-seg old-obj)
                                       *db-new-names*))))
             (car (or (??& segcase   input * new-obj)
                      (??& segcase  output * new-obj)
                      (??& segcase  tinput * new-obj)
                      (??& segcase toutput * new-obj)
                      (barf "EXPAND-PORT: Can't find deep io~
                          ~% Port: ~:=" port)))))))

;this takes in a set of facts, and compresses them together in a block.
;it assumes that all of the facts are about the same seg, and it is only
;really usefull if the facts have been sorted first.
; initially, each individual fact is compressed as above.  after this each
;one is of the form [segcase keyword ! rest].  these are then factored
;together
;according to their cases, and keywords.  each group produced is a list of
;a keyword, and a sequence of rests.  a new group is started every time
;the keyword, or the case changes.  whenever the case changes, a group
;consisting of just the case is produced.  (if the first case is nil, then
;no initial nil is produced.)

(defun compress-facts (facts)
  (let* (case result group key
         (cases (and facts (all-fact-cases (car facts)))))
    (over facts -> fact
          (compress-fact fact) -> [sc k ! rest]
          (if (not (atom sc)) (cadr sc)) -> c
      (when (or (not (eq k key)) (not (eq c case)))
        (if group (push (nreverse group) result))
        (setq group [k]
              key   k))
      (push rest group)
      (when (not (eq c case))
        (push c result)
        (setq cases (delq c cases))
        (setq case c)))
    (push (nreverse group) result)
    (setq result (nconc cases result))
    (nreverse result)))

;takes a fact and gives all the cases on the seg of the fact.
;kludge just called above.

(defun all-fact-cases (fact)
  (let* ((seg (~deep fact)))
    (cond ((fact? seg) (setq seg (~deep seg))))
    (setq seg (~seg seg))
    (over [!!(~nset ~case ~to (??& * cflow * (&* seg)))
           !!(~nset ~case ~segcase ~to (??& * dflow * (&* (&* seg) port)))
           !!(~nset ~case ~segcase ~from (??& * dflow (&* (&* seg) port)))
           !!(~nset ~case ~from (??& * cflow (&* seg)))]
          select _ makeqset _)))

;this is the inverse of compress-facts.  it takes in the shared compressed
;form, and expands them all out.

(defun expand-facts (info)
  (let* (((seg . lines) info)
         (csc seg))
    (dolist (line lines)
      (cond ((atom line) (setq csc [seg line]))
            (T (dolist (rest (cdr line))
                 (expand-fact [csc (car line) ! rest])))))))

;;; (BUILD-*DB-TOP-FN* list)
;;;
;;;  This is used both on compression and reading in in order to construct
;;;  a *DB-TOP-FN*. It will globally assign *DB-TOP-FN*; if that's
;;;  not desirable, you should bind that variable before calling this
;;;  function.
;;;
;;;  See PLAN-DEF which calls this.

(defvar *db-top-fn-idx* nil
  "Communication var for build-*DB-TOP-FN* and build-*DB-TOP-FN*-aux")

(defun build-*DB-TOP-FN* (list)
  (let* ((*db-top-fn-idx* 0.))
    (setq *db-top-fn* nil)                          ;global assignment
    (build-*DB-TOP-FN*-aux list)
    (setq *db-top-fn* (reverse *db-top-fn*))))  ;global assignment

(defun build-*DB-TOP-FN*-aux (obj)
  (push (cons obj *db-top-fn-idx*) *db-top-fn*) ;global assignment
  (incf *db-top-fn-idx*)
  (cond ((not (atom obj))
         (build-*DB-TOP-FN*-aux (car obj))
         (build-*DB-TOP-FN*-aux (cdr obj)))))

(defun compress-code-list (list)
  (if (listp list) (mapcar #'compress-code list)
      list))

(defun compress-code (code)
  (cond ((and (listp code)
              (memq (car code) '(*code-is-car* *code-is-cdr*)))
         [(car code) (compress-code (cdr code))])
        ((cdr (assq code *db-top-fn*)))
        (t (copy code)))) ;prevents pointers into DB.

(defun expand-code-list (list)
  (mapcar #'expand-code list))

(defun expand-code (code)
  (cond ((null *db-top-fn*) nil)
        ((numberp code) (car (nth code *db-top-fn*)))
        ((and (listp code)
              (memq (car code) '(*code-is-car* *code-is-cdr*)))
         [(car code) ! (expand-code (cadr code))])))

(defun construct-DEFFACTS (seg)
  `(deffacts ,seg ,@(compress-facts (sort-facts (seg-facts seg)))))

(defun construct-DEFSPECS (seg)
  `(defspecs ,seg
             ,@(compress-facts
                 (sort-facts
                   (collect (fact (seg-facts seg))
                            (not (plan-fact? fact)))))))

(defun construct-DEFPLAN (seg)
  `(defplan ,seg
            ,@(compress-facts
                (sort-facts
                  (collect (fact (seg-facts seg)) (plan-fact? fact))))))

;note that the division into DEFFACTS, DEFSPECS, and DEFPLAN is only for
;the convenience of the user and means nothing in actuality.

;these are not ever evaled.  You MUST nest then inside a plan-def.

(defmacro deffacts (name &body stuff)
  (ferror "Not defined except inside PLAN-DEF: ~S"
          `(DEFFACTS ,name ,@stuff)))

(defmacro defplan (name &body stuff)
  (ferror "Not defined except inside PLAN-DEF: ~S"
          `(DEFPLAN ,name ,@stuff)))

(defmacro defspecs (name &body stuff)
  (ferror "Not defined except inside PLAN-DEF: ~S"
          `(DEFSPECS ,name ,@stuff)))

;this prints out a deffacts prettily.

(declare (special deffacts-latch))

(pp:define-list-print-function deffacts (list s)
  (let ((deffacts-latch T))
    (format s "~1!~=~^ ~@_~=~^ ~@{~\kbe:deffacts-form\~^~}~." list)))

(pp:define-printing-directive deffacts-form (item s)
  (cond ((or (atom item) deffacts-latch) (format s "~%")))
  (setq deffacts-latch T)
  (cond ((atom item) (prin1 item s) (setq deffacts-latch nil))
        ((memq (car item) '(dflow @io))
         (format s " ~4,0T(~!~S~^ ~:I~@_~@{~=~^ ~0,40T~:_~}~.)" item))
        ((memq (car item) '(cflow))
         (format s " ~4,0T(~!~S~^ ~:I~@_~@{~=~^ ~0,20T~:_~}~.)" item))
        (T (format s " ~4,0T(~!~S~^ ~:I~@_~@{~=~^ ~:_~}~.)" item))))

(pp:define-list-print-function defspecs (list s)
  (let ((deffacts-latch T))
    (format s "~1!~=~^ ~@_~=~^ ~@{~\kbe:deffacts-form\~^~}~." list)))

(pp:define-list-print-function defplan (list s)
  (let ((deffacts-latch T))
    (format s "~1!~=~^ ~@_~=~^ ~@{~\kbe:deffacts-form\~^~}~." list)))

(pp:define-list-print-function plan-def (list s)
  (format s "~1!~1I~=~^ ~@_~=~^~%  ~=~^~@{~%~=~^~}~."
          list))

;this reconstructs a dumped piece of data base.  Note the
;way it works to prevent there from being any name clashes with
;what ever else is in the db, and the way it does everything
;in reverse order so that the db finds will work out ok
;during expansion.  It returns the seg defined.

(defvar *plan-def-segcases* nil "the segcases plan-defs eval to")

(defun plan-def (&quote &rest body)
  (let* (((result-seg code . defns) body)
         *db-new-names* *db-top-fn*)
    (build-*DB-TOP-FN* (copytree code)) ;copying is vital!!
    (dolist (def (reverse defns))
      (when (eq (car def) 'deffacts)
        (expand-facts (cdr def))))
    (setq result-seg (cdr (assq result-seg *db-new-names*)))
    (push (& result-seg) *plan-def-segcases*)
    (& result-seg)))

;this dumps out a set of plans into a file that can be loaded back
;in again by doing a LOAD.

(defun dump-plans (segcases filename)
  (with-open-file (file (file-default-fn2 filename 'PLAN ':newest) ':out)
    (let ((package *KBE-package*))
      (format file ";;; -*- Mode:LISP; Package:KBE; Base:10; -*-~%")
      (dolist (segcase segcases)
        (multiple-value-bind (plan pas)
            (construct-plan-def (~seg segcase))
          (pl plan file)
          (format file "~/|~%/"")
          (pl pas file)
          (format file "/"~%~/|"))))))

;this reads in a file that contains 1 or more plan-defs, and
;returns a list of the top level segs asserted.
;THIS IS OBSOLETE. JUST LOAD THE FILE INSTEAD SEE ABOVE.

(defun load-dump (filename)
  (with-open-file (file (file-default-fn2 filename 'PLAN ':newest))
    (let ((package *KBE-package*))
      (repeat (result item)
        (setq item (read file t))
        (when (eq item t) ;returns t on eof
          (push-db-context)
          (db-context-was >LOAD)
          (return (nreverse result)))
        (cond ((and (not (atom item))
                    (eq (car item) 'plan-def))
               (push (eval item) result)))))))

;this conses up a list representation of all of the facts
;about a seg and its subsegs.  As a side-effect, it also creates the
;PAS print structure.  It returns them as two values.  if trim?
;is T then terminal segments are not included in the output.

(defun construct-plan-def (seg &optional trim?)
 (prog ()
  (let* ((*db-top-fn* nil)
         (external-rep)
         (code (~01 ~code (??& (&* seg) @seg)))
         (waiting [[seg]])
         (pasindex ['*ROLE-FORMAT* nil ! (car waiting)]))
    (build-*DB-TOP-FN* code)
    (repeat* ((todo [seg])
              done info subs roles)
      (cond ((null todo) (return nil)))
      (cond ((memq (top todo) done) (pop todo))
            (t (setq info (construct-DEFFACTS (pop todo)))
               (setq subs
                     (mapcar #'car
                             (cdr (assq-carefully 'subseg (cddr info))))
                     roles
                      (or (over (cdr (assq-carefully 'role (cddr info)))
                                -> [! e s n]
                           select
                            (cond ((eq n 'body)
                                   (not (over (cdr (assq-carefully 'role
                                                            (cddr info)))
                                         fastor (eq (cadr _) 'op))))
                                  (t (not (memq n '(join rec dfjoin)))))
                            list e)
                          (over (cdr (assq-carefully 'subseg (cddr info)))
                           list [(car _) nil])))
               (cond ((or subs (not trim?)) (push info external-rep)))
               (setq todo (append subs todo))
               (let ((spot (assq (cadr info) waiting)))
                 (if (and spot subs)
                     (rplacd spot
                             [(caadr (assq-carefully 'plantype(cddr info)))
                              !(over roles -> [s n]
                                list ['*ROLE-FORMAT* n
                                      !(top (push [s] waiting))])]))))))
    (return `(plan-def ,seg ,(copytree code) ,@(nreverse external-rep))
            pasindex))))

;the purpose of this function is to produce a new segment which is an
;exact copy of another segment.   It just uses compress and expand in
;order to get all the right unique names generated and everything.
;the second arg used to be a prefix to ues for the new seg but is
;now obsolete.  This would be faster if it specially integrated everything
;together without doing any consing of compressed facts and leaving out
;most of the sorting.  But, this is simpler and more obviously bug free.

(defun copyseg (oldseg ignore)
  (eval (construct-plan-def oldseg)))

;these things just make the pas print out prettier.

(pp:define-list-print-function *role-format* (list s)
  (let* (((role seg ptype . subsegs) (cdr list))
         (>1level (over subsegs -> sub
                    fastor (and (not (atom sub))
                                (over sub fastor (not (atom _)))))))
    (setq role  (nametran role)
          ptype (nametran ptype))
    (if >1level
        (format s "~@!~2I~@[~A ~]~S~@[{~A}~]~{~%~=~^~}~."
                role seg ptype subsegs)
        (format s "~@!~@[~A ~]~S~@[{~A}~] ~:I~{~:_~=~^~}~."
              role seg ptype subsegs))))

(defun nametran (name)
  (or (cdr (assq name
             '((action   . "act" ) (init     . "init") (pred     . "pred"  )
               (join     . "join") (op       . "op"  ) (body     . "body"  )
               (rec      . "rec" ) (dfjoin   . "dfj" ) (bssr     . "bssr"  )
               (term     . "term") (filter   . "fltr") (trn      . "trn"   )
               (acc      . "acc" ) (gen      . "gen" ) (expr     . "expr"  )
               (and      . "and" ) (comp     . "comp") (xor      . "xor"   )
               (pred     . "pred") (ssr      . "ssr" ) (ssr-body . "ssrb"  )
               (aug      . "aug" ) (term     . "term") (filter   . "filter")
               (tempcomp . "tc"  ) (join     . "join") (constant . "const" )
               (builtin  . "bi"  ) (new-view . "nv"  ) )))
      name))

;Note that these funny fexprs are ment to be called by the user, they
;should not be called from code!!

(defun basic-qfn (seg var body)
  (when (or (setq seg (car seg)) (null (symeval var)))
    (eval (subst (get-seg seg 1) 'seg body)))
  (pp:pp (symeval var))
  (values))

(defun get-seg (seg cnt)
  (cond ((minusp cnt) (grumble "Bad seg designator.~% seg: ~S" seg) nil)
        ((atom seg)
         (cond ((db-index seg) seg)
               (t (get-seg (eval seg) (1- cnt)))))
        ((atom (car seg))
         (cond ((db-index (car seg)) (car seg))
               (t (get-seg (eval seg) (1- cnt)))))
        ((atom (caar seg))
         (if (db-index (car seg)) (caar seg)))))

(defun f fexpr (seg)
  (basic-qfn seg 'qf '(setq qf (construct-DEFFACTS 'seg))))

(defun s fexpr (seg)
  (basic-qfn seg 'qs '(setq qs (construct-DEFSPECS 'seg))))

(defun p fexpr (seg)
  (basic-qfn seg 'qp '(setq qp (construct-DEFPLAN  'seg))))

(defun pas fexpr (seg)
  (basic-qfn seg 'qpas
    '(multiple-value (qpp qpas) (construct-plan-def 'seg t))))

(defun pp fexpr (seg)
  (basic-qfn seg 'qpp
    '(multiple-value (qpp qpas) (construct-plan-def 'seg t))))

(defun pwp fexpr (seg)
  (basic-qfn seg 'qpwp
    '(multiple-value (qpwp qpas) (construct-plan-def 'seg nil))))

;;; PDF pages 279-280
(def-alistq f-by-k)
(defvar *SO* nil "...???...")
(defvar *PO* nil "...???...")
(declare (special fo role-order))
(defvar *key-order*
  '(segtype plantype recursive when input output tinput toutput
    expect assert @seg @io @createvar role subseg cflow dflow))
;Sort facts about the same segment in a useful order.
(defun sort-facts (facts)
  (if facts
      (let* ((seg (cond ((fact? (~deep (car facts)))
                        (~seg (~deep (~deep (car facts)))))
                       (t (~seg (~deep (car facts)))))))
        (over (sortcar
                (over facts -> fact
                  classifyq [(cond ((fact? (~deep fact))
                                    (~case (~deep (~deep fact))))
                                   (t (~case (~deep fact)))) ! fact])
                #'(lambda (x y)
                    (cond ((null x) t) ((null y) nil) ((alphalessp x y)))))
              -> case-fs
              (sortcar (over (cdr case-fs) -> f classifyq [(~keyword f) ! f])
                #'(lambda (x y)
                    (let ((xtail (memq x *key-order*))
                          (ytail (memq y *key-order*)))
                      (cond (xtail (or (null ytail) (memq y (cdr xtail))))
                            (ytail nil) (t (alphalessp x y))))))
              -> f-by-k
              (if (f-by-k '@io)
                  (~set ~deep
                    (sort (append (f-by-k '@io) nil) ;don't kill entry
                      #'(lambda (x y)
                          (cond ((not (same-fact-key? (~deep x) (~deep y)))
                                 (input-fact? (~deep x)))
                                ((memq (~kind x) '(lambda-arg actual-arg))
                                 (or (not (memq (~kind y) '(lambda-arg actual-arg)))
                                     (< (~argno x) (~argno y))))
                                ((memq (~kind y) '(lambda-arg actual-arg)) nil)
                                ((eq (~kind x) 'returnval) t)
                                ((eq (~kind y) 'returnval) nil)
                                (t (alphalessp (~name x) (~name y))))))))
              -> *po*
              (enumerate-flows seg (~set ~seg~subseg (f-by-k 'subseg))
                (over (f-by-k 'cflow)
                  bag [(~seg (~to _)) ! (~seg (~from _))])
                (over (f-by-k 'dflow)
                  bag [(~seg~segcase (~to _)) ! (~seg~segcase (~from _))]))
              -> fo
              (if (f-by-k 'plantype)
                  (get (~type (car (f-by-k 'plantype))) 'role-order))
              -> role-order
              (over (f-by-k 'role)
                bag [(~seg~roleseg _) ! (~rolename _)]) -> role-info
              (mapcar #'car
                (sort [!(over (f-by-k 'subseg)
                          select (null (assq (~seg~subseg _) role-info))
                          bag [(~seg~subseg _)])
                       ! role-info]
                  #'(lambda (x y)
                      (car (or (in-order role-order (cdr x) (cdr y))
                               (in-order fo (car x) (car y))
                               [(alphalessp (car x) (car y))]))))) -> *so*
          nconc (setq *so* [seg ! *so*])
                (over f-by-k nconc (sort-key-facts (cdr _)))))))

;;; PDF page 281
(defun sort-key-facts (facts)
  (fact-key-dispatch (car facts)
    ((input output tinput toutput)
     (sort facts #'(lambda (x y)
                     (car (or (in-order *po* x y)
                              [(alphalessp (~obj x) (~obj y))])))))
    ((@io @createvar)
     (sort facts #'(lambda (x y)
                     (car (or (in-order *po* (~deep x) (~deep y))
                              [(alphalessp (~obj (~deep x)) (~obj (~deep y)))])))))
    (subseg (sort facts #'(lambda (x y)
                           (in-order? *so* (~seg~subseg x) (~seg~subseg y)))))
    (role (sort facts #'(lambda (x y)
                         (in-order? *so* (~seg~roleseg x) (~seg~roleseg y)))))
    (cflow
     (sort facts
       #'(lambda (x y)
           (let* ((xfrom (~seg (~from x))) (xto (~seg (~to x)))
                  (yfrom (~seg (~from y))) (yto (~seg (~to y))))
             (cond ((in-order? *so* xto xfrom) ;x is back pointer
                    (cond ((in-order? *so* yfrom yto) ;y is forward pointer
                           (in-order? *so* xfrom yto))
                          (t (in-order? *so* xfrom yfrom))))
                   (t ;x is forward pointer
                    (cond ((in-order? *so* yfrom yto) ;both forward
                           (cond ((eq xto yto) (in-order? *so* yfrom xfrom))
                                 (t (in-order? *so* xto yto))))
                          (t (not (in-order? *so* yfrom xto))))))))))
    (dflow
     (sort facts
       #'(lambda (x y)
           (car (or (in-order *so* (~seg~segcase (~from x))
                                  (~seg~segcase (~from y)))
                    (cond ((not (equal (~from x) (~from y)))
                           [(alphalessp (~obj (~from x)) (~obj (~from y)))])
                          (t (in-order *so* (~seg~segcase (~to x))
                                            (~seg~segcase (~to y))))))))))
    (t facts)))
(defprop xor (init pred action join) role-order)
(defprop pred (init pred join) role-order)
(defprop ssr (init op body rec) role-order)
(defprop tempcomp (init) role-order)

;;; PDF page 282
;;; IN-ORDER? returns truth; IN-ORDER returns NIL if unsure or (answer).
(defun in-order? (order x y)
  (if (not (eq x y))
      (dolist (w order)
        (cond ((eq x w) (return t)) ((eq y w) (return nil))))))
(defun in-order (order x y)
  (if (not (eq x y))
      (dolist (w order)
        (cond ((eq x w) (return '(t))) ((eq y w) (return '(nil)))))))

;;; PDF page 283
(def-alistq graph)
(def-alistq cgraph)
(declare (special SEGS))
(defun enumerate-flows (head SEGS cpairs dpairs)
  (setq cpairs (over cpairs makeset _)
        dpairs (over (append dpairs cpairs) makeset _)
        SEGS (append SEGS nil))
  (repeat* ((graph (over dpairs classifyq _))
            (cgraph (cond (cpairs (over cpairs classifyq _)) (t graph)))
            result)
    (setq result (nconc (enumerate-flows-aux head result graph) result))
    (cond ((null SEGS) (return (nreverse result)))
          (t ;must be a cycle pick a seg to tag on.
           (setq head
                 (over SEGS -> seg
                       (cgraph seg) -> need
                       (length need) -> need-num
                       (over need select (memq _ result) sum 1) -> got-num
                   revlist [(- (* 100. got-num) need-num) ! seg]))
           (setq head
                 (cdar (sort head
                         #'(lambda (x y)
                             (or (> (car x) (car y))
                                 (and (= (car x) (car y))
                                      (alphalessp (cdr x) (cdr y))))))))
           (setq SEGS (delq head SEGS))))))
(defun enumerate-flows-aux (head old graph)
  (repeat (cut (result [head]))
    (setq cut (over SEGS -> s
                select (over (graph s)
                         fastand (or (memq _ old) (memq _ result)))
                bag (setq SEGS (delq s SEGS)) s))
    (cond ((null cut) (return result))
          ((null (cdr cut)) (setq result (cons (car cut) result)))
          (t (let ((new-old [!! result !! old]))
               (over (sort (over cut bag (enumerate-flows-aux _ new-old graph))
                       #'(lambda (x y)
                           (let ((lx (length x)) (ly (length y)))
                             (or (> lx ly)
                                 (and (= lx ly) (alphalessp (car x) (car y)))))))
                 (setq result (append _ result))))))))

;;; PDF pages 284-285
;Access functions return parts of facts directly, rather than assertions.
;Prefixes: ? zero/one, ?1 exactly one, ?? any number, a? at least one;
;- delete zero/one, -1 delete exactly one, -- delete all, a- delete one;
;+ add, > assert replacing similar facts. Ports remain represented as facts.
(defun ??input (seg) (??& (&* seg) input))
(defun --input (seg) (--& (&* seg) input))
(defun ??output (seg) (??& (&* seg) output))
(defun --output (seg) (--& (&* seg) output))
(defun ~id-port (port)
  (if (~id port)
      (sel1 (find-facts (& (& (~seg~segcase port))
                          (cond ((input-fact? port) 'output) (t 'input))
                          nil (~id port) nil)))))
;Remove unimportant commentary and then the port; handle dflows first.
(defun kill-port-and-commentary (port)
  (--& port @io)
  (--& port @createvar)
  (make-fact-be-out port))

;;; PDF page 286
(defun ?segtype (seg) (~01 ~type (??& (& seg) segtype)))
(defun >segtype (seg type)
  (--& (& seg) segtype) (>& (& seg) segtype type) type)
(defun -segtype (seg) (~01 ~type (--& (& seg) segtype)))
(defun ??subseg (seg) (~nset ~seg ~subseg (??& (& seg) subseg)))
(defun all-subsegs (seg)
  [seg ! (over (??subseg seg) nconc (all-subsegs _))])
(defun +subseg (seg subseg)
  (>& (& seg) subseg (& subseg)) subseg)
(defun -subseg (seg subseg)
  (-& (& seg) subseg (& subseg)) subseg)
(defun --subseg (seg) (~nset ~seg ~subseg (--& (& seg) subseg)))
(defun ?supseg (seg) (~01 ~seg ~segcase (??& * subseg (& seg))))
(defun -supseg (seg) (~01 ~seg ~segcase (--& * subseg (& seg))))
(defun >supseg (seg subseg)
  (--& * subseg (&* subseg))
  (>& (& seg) subseg (& subseg)))

;;; PDF page 287
(defun ?dflow-source (port) (~01 ~from (??& * dflow * port)))
(defun ??dflow-use (port) (~nset ~to (??& * dflow port)))
;Compute the redundant superseg field.
(defun +dflow (from to)
  (let* ((segf (~seg~segcase from)) (segt (~seg~segcase to))
         (supf (?supseg segf)) (supt (?supseg segt))
         (sup (cond ((eq segf segt)
                     (cond ((input-port-fact? from) segf) (t supf)))
                    ((eq supt supf) supt)
                    ((eq segf supt) supt)
                    ((eq segt supf) supf)
                    (t (barf "+DFLOW - no compatible supseg.~
                              ~% segf: ~S~
                              ~% segt: ~S" segf segt)))))
    (>& (& sup) dflow from to)))
;The following two are commented out because side cases are ambiguous.
;(defun ?cflow-source (segcase)
;  (~01 ~from (??& * cflow * segcase)))
;(defun ?cflow-use (segcase)
;  (~01 ~to (??& * cflow segcase)))
;Cannot create a cflow inside a seg from its input to its output side.
(defun +cflow (from to)
  (let* ((segf (~seg from)) (segt (~seg to))
         (supf (?supseg segf)) (supt (?supseg segt))
         (sup (cond ((eq supt supf) supf)
                    ((eq segf supt) supt)
                    ((eq segt supf) supf)
                    (t (barf "+CFLOW - no compatible supseg~
                              ~% From: ~S~
                              ~% To:   ~S" from to)))))
    (>& (& sup) cflow from to)))

;;; PDF page 288
(defun ?plantype (seg) (~01 ~type (??& (& seg) plantype)))
(defun >plantype (seg type)
  (--& (& seg) plantype) (>& (& seg) plantype type) type)
(defun -plantype (seg) (~01 ~type (--& (& seg) plantype)))
(defun ??roles (seg)
  (over (??& (& seg) role) -> role
    classifyq [(~rolename role) ! (~seg~roleseg role)]))
(defun --roles (seg)
  (over (--& (& seg) role) -> role
    classifyq [(~rolename role) ! (~seg~roleseg role)]))
(defun ??role (seg name) (~nset ~seg~roleseg (??& (& seg) role * name)))
(defun ?role (seg name) (~01 ~seg~roleseg (??& (& seg) role * name)))
(defun ?1role (seg name) (~seg~roleseg (?& (& seg) role * name)))
(defun --role (seg name) (~nset ~seg~roleseg (--& (& seg) role * name)))
(defun -role (seg name) (~01 ~seg~roleseg (--& (& seg) role * name)))
(defun +role (seg roleseg name) (>& (& seg) role (& roleseg) name))
(defun ?recurrence (supseg) (~01 ~seg ~subseg (??& (&* supseg) recursive)))
(defun ?recursive-supseg (subseg)
  (~01 ~seg ~segcase (??& * recursive (&* subseg))))
(defun +recursive (supseg subseg) (>& (& supseg) recursive (& subseg)))

;;; PDF page 289
(defun code-for-defun (name)
  (let ((segcase (~01 ~segcase (or (??& * @seg 'lambda-exp name)
                                 (??& * cliche-def name)))))
    (cond (segcase (car (~01 ~code (??& segcase @seg)))))))
(defun ?lambda-exp (name) (~01 ~seg ~segcase (??& * @seg 'lambda-exp name)))
(defun ?@seg-kind (seg) (~01 ~kind (??& (&* seg) @seg)))
(defun ?@seg-name (seg) (~01 ~name (??& (&* seg) @seg)))
(defun ?@seg-code (seg) (~01 ~code (??& (&* seg) @seg)))
;The copying of the code list is VITAL to avoid bugs.
(defun >@seg-code (seg code)
  (let ((@seg (-& (&* seg) @seg)))
    (>& (& seg) @seg (~kind @seg) (~name @seg) (append code nil))))
(defun ?@io-kind (port) (~01 ~kind (??& port @io)))
(defun ?@io-name (port) (~01 ~name (??& port @io)))
;Guess code associated with a port. Return code pieces or NIL.
(defun ?@io-code (port)
  (cond ((tio-fact? port)
         (cond ((~lower-io port) (?@io-code (~lower-io port)))))
        (T (let ((@seg (~01 (??& (& (~seg~segcase port)) @seg)))
                 (@io (~01 (??& port @io))))
             (cond ((not (and @io @seg (~code @seg)
                              (listp (car (~code @seg))))) nil)
                   ((and (eq (~kind @io) 'actual-arg)
                         (eq (~kind @seg) 'funcall))
                    `((*CODE-IS-CAR* . ,(nthcdr (~argno @io)
                                               (car (~code @seg))))))
                   ((and (eq (~kind @io) 'lambda-arg)
                         (eq (~kind @seg) 'lambda-exp)
                         (eq (caar (~code @seg)) 'define))
                    `((*CODE-IS-CAR* . ,(nthcdr (1- (~argno @io))
                                               (caddar (~code @seg))))))
                   ((eq (~kind @io) 'returnval) (~code @seg)))))))

;;; PDF page 290
(defun ?empty-slot (seg)
  (let ((slotF (~01 (??& * slot (&* seg))))
        (unfilled (??& (&* seg) @seg 'funcall nil)))
    (cond ((null slotF) nil)
          (T (caseq (~slottype slotF)
               (input (if unfilled 'feed))
               (output
                (if (no-dflow-from
                      (~deep (?& (&* (&* seg) output) @io 'returnval))) 'use))
               (segment (if unfilled 'expand)))))))
(defun no-dflow-from (port)
  (and (null (??& * dflow port))
       (Rand-fast (no-dflow-from (Elist (??& * toutput * * port))))))
