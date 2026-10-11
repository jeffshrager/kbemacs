;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_2.pdf
;;; PDF pages 132-201; listing coder.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts: MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;this file (which uses the prefix 5) contains functions for
;making LISP code corresponding to a plan.
;  the coding process works in three stages.  First temporal
;compositions are undone.  Then the plan is annotated with @seg, @io,
;and @creatvar information.  Lastly code is produced.
;finally the annotation produced is moved back to the temporally decomposed
;version, and the layers containing the composed version are discarded.
; the key to the matter is the annotation produced.
;DB contains the official definitions of the assertions.  This comment
;describes them in more detail.  (Some of the types created by the surface
;analyzer are not used here, and vis versa.)
;
;       ( <deep> @SEG <kind> <name> <code> )
;  <deep>   = a segcase
;  <kind>   = LAMBDA-EXP ! CONSTANT ! FUNCALL ! OPENCODE ! JOIN ! RECURSION
;  <code>   = a list of CONSs corresponding to the code for the seg
;  <name>   = if FUNCALL the name of the function
;             if CONSTANT the constant value
;             if top level opencode then the name the function is to have
;             otherwise nil
;this is used by the central dispatcher of the coder.  it classifies each
;segment.  Terminal segs are all of type CONSTANT or FUNCALL (except for joins
;and recursions).  All of the non-terminal segs are of type OPENCODE.  The
;code field does double duty showing what to display, and what to execute
;in order to get the return value.
;
;       ( <deep> @IO <kind> <name> <argno> )
;  <deep>   = an input or output
;  <kind>   = LAMBDA-ARG ! ACTUAL-ARG ! FREE-VAR ! RETURNVAL ! SIDE-EFFECT
;  <name>   = the variable name if the item
;             happend to be carried in a variable.
;  <argno>  = if LAMBDA-ARG or ACTUAL-ARG the argument position number.
;This controls how dflow will pass through the annotated port.
;LAMBDA-ARG is used only for inputs to the top level lambda
;expression, ACTUAL-ARG is used for inputs to a FUNCALL.  RETURNVAL is
;used when the data flow is going to be implemented by nesting of the
;returnval producing code in the receiving code. (precautions have to
;be taken so that this doesn't interact badly with FREE-VAR dataflow.)
;FREE-VAR indicates that the dflow is being carried in a variable.  If
;the same name is assigned to more than once you have to be careful.

;       ( <deep> @CREATEVAR <varname> )
;  <deep>   = the port where the variable is to be created
;  <varname> = the var name
;This indicates that a variable is being created by a setq.  It always happens
;AFTER the port (eg inside an input and outside an output).  It serves as a
;transducer to give something another name, or a name in the first place.
;Its primary use is to assign the returnval of a funcall to a variable for
;later use.

;the annotator which constructs the above assertions must construct
;a pattern of assertions which the coder can then code.  To do this it
;must follow certain restrictions.  The key coding difficulty is deciding
;how to implement the dataflow.  This is embedded in the @IO and @CREATEVAR
;assertions.  The problem for the coder comes when a data flow goes between
;two ports which are not of the same type.  (note that a join is constrained
;so that all of its inputs are the same type as the corresponding outputs
;so that no code is needed to correspond with the inside of the join.)
;the coder makes interfaces as follows
;
;NOTE a VAR-THING is a LAMBDA-ARG, FREE-VAR, or @CREATEVAR
;    from          to                    code produced
; VAR-THING  ACTUAL-ARG    nest var name in arglist
; VAR-THING  FREE-VAR      if name is the same then no code at all
;                         *do a setq before the code for the to seg
;                          or after it if free-var is an output
; VAR-THING  RETURNVAL     nest var name as last thing to be executed.
; VAR-THING  @CREATEVAR    var name must be different,
;                         *do a setq just inside the seg to the new var
;                          or just after the seg if thing is an output.
; RETURNVAL  ACTUAL-ARG    nest code for returnval in arg position
; RETURNVAL  FREE-VAR     *do a setq with the returnval code nested in it
;                          before or after the to seg code.
; RETURNVAL  RETURNVAL     nest returnval code as last thing to evaluate
; RETURNVAL  @CREATEVAR   *do a setq with the returnval code imbedded where you
;                          would have put the seg otherwise.
;
;There are two problamatical things in this.
;1. when a setq is to be done (the *ed items) you can get into two kinds of
;trouble.  first if a setq has to go after some seg then that seg can't
;have a returnval too unless you use PROG2s.
;Second if you have more than one setq in the
;same spot they may clash with each other.  You can't assign to a variable
;which you are going to use in another setq later.  The simple solution
;which will be used for now is using a PSETQ.
;2. if a returnval is used without an @CREATEVAR so somthing realy gets nested
;this can cause problems with other dataflows which are being carried in
;variables.  (the one time you are sure a returnval is completely safe is
;when it is the only output.)  this is delt with by really looking at
;what added constaints this produces.
;
;In order to be very safe on the above, and hopefully make the code prettier
;the annotator tries to follow the following guidelines
;1. use as few free-vars as possible
;2. don't assign to the same var twice unless you can prove that the domains
;   of use do not overlap.
;3. attempt to keep setqs to a minimum by avoiding name changes at
;   a port

;The coder has been extended to allow for PARTIAL plans.  This
;comment discusses just what ways the plan can be partial.  As will be
;seen, the key thing is that the data flow can be incomplete.  Each
;kind of thing that EDIT works with will be discussed seperately.

;1. A SEGMENT - On the one hand, if a segment isn't there, it just isn't
;there, and there is no problem.  On the other hand, to be well formed,
;some plans require some roles to definitely be there.  As a result,
;the coder will require them to be there.  (they don't have to have
;anything useful in them however)  These required actions include the
;PRED and JOIN of an XOR, and the BODY, REC and DFJOIN of an SSR.
;   Another way in which a segment can be incomplete is that it is just
;a place holder.  It has ports and cases, but no insides at all.  to
;represent this, you make it a funcall with a name field of NIL.  In
;this situation, it must be a SLOT, and the slot name will be used to
;code it up in #{} form.

;2. a CASE  In general whenever there are cases they are strongly
;interrelated with surrounding segments.  I cannot think of any
;situation where you could delete a case without having to change a lot
;of other things too in order to get a plan the coder would be able to
;code.  (Note that in a sense a case only exists because a cflow connects
;to it.)

;3. a CFLOW  This is the major place where the coder does not allow
;anything to be missing.  cflows are only allowed in xors, preds, ssrs,
;and tempcomps, and in these contexts they are absolutely mandatory,
;because the coder follows them around in order to tell what is going
;on.

;4. a DFLOW  This is the major place where the coder works to provide
;for incompleteness.  There are two ways in which a dflow can be
;missing, a port can have no dflow ending on it, or a port can have no
;dflow starting from it.  (The latter case doesn't even indicate that
;the program is actually incomplete and is no problem)
;the former case is treated by just trying to ignore it.  It
;shouldn't actually happen in normal programs since input slots
;are segs which properly interact with dflow.

;5. a PORT  If a port isn't there, then it just isn't there, and there
;is no problem.  I don't think that there is any situation where a port
;is required, except on a join, where a certain consitancy must be
;maintained.  (Note that if you remove an input from a call on some
;system fn, you may get an odd looking result.)  also note that if you
;delete a port, you must delete the dflow refering to it.

;this takes a seg and codes up a top level lambda expression corresponding
;to it.  any old @assertions are suppressed by pushing a context and
;deleting them.  they are used later to suggest variable names, and to
;give function names.

(declare (special SEGTYPES PSEGS NSEGS SEGFACTS
                  5TERMINAL? 5ORDERID REGIONS TOP-SEG TCs 5CLASSIFY
                  NAME-CLASSES SEGS-WITH-RETURNVALS VAR-NAMES-USED
                  5PTYPE 5FLAG 5IMMEDIATE 5PATTERN 5RELATED
                  5MAXID 5NEXTBIT 5LIMIT 5PLY 5BIT-PATTERN 5CLOSED 5IDS
                  NUM-SEGS CODE-FOR)
         (*expr db-verify-context1)
         (*fexpr dr))

(define-trace-option *CODER-Debug*
                     "Show trace of CODER internals"
  nil :boolean)

(def-alistq ORDERING)
(def-alistq roles)
(def-alistq mpred-roles)
(def-alistq lower-rets)
(def-alistq CODE-FOR)
(def-alist  REGIONS)
(def-alist  case-action)

(defun code (segcase)
  (let* ((mode (code-mode segcase))
         (result (code1 segcase mode))
         (def-name
           (caseq mode
             (defun (?@seg-name (~seg result)))
             (define-cliche (~cliche-def-name (?& result cliche-def))))))
    (if def-name
        (coder-confirmation def-name
                            (copytree (sel01 (?@seg-code (~seg result))))))
    result))

;Due to bad design of the assertions in the data base, there is
;no good way to tell the difference between a cliche instance and
;a cliche definition.  The following is a gross kludge that
;at least works much of the time.  Note that only compound non-top level
;forms can be cliches by the following definition.

(defun code-mode (segcase)
  (cond ((??& segcase @seg 'lambda-exp) 'defun)
        ((??& segcase cliche-def) 'define-cliche)
        (T 'opencode)))

(defun code1 (segcase mode)
  (declare (special *force-slots-up*))
  (let* (REGIONS 5CLASSIFY
         (TOP-SEG (~seg segcase))
         (original-context *db-context*) ;contains original version.
         5TERMINAL? VAR-NAMES-USED 5PTYPE ioc (NUM-SEGS 0)
         5FLAG 5IMMEDIATE 5PATTERN 5RELATED CODE-FOR
         5MAXID 5NEXTBIT 5LIMIT 5PLY 5BIT-PATTERN 5CLOSED 5IDS)
    (db-context-was <CODE)
    (5init-order)
    (push-db-context) ;make ply for the composed version.
    (setq ioc (5temporal-compose-all TOP-SEG))
    ;Must must make sure slot info is not lost.
    (let ((*force-slots-up* t))
      (3expr-simp TOP-SEG)) ;kill unneeded exprs created by editing.
    (5annotate TOP-SEG)
    (5codetop TOP-SEG mode)
    ;; restores & pushes context:
    (5fix-@assertions TOP-SEG ioc original-context)
    (db-context-was >CODE)
    segcase))

;run over the plan combining all tempcomps

(defun 5temporal-compose-all (seg)
  (let (TCs)
    (5find-tcs seg)
    (over TCs
      nconc (temporal-compose _))))

(defun 5find-tcs (seg)
  (cond ((eq (?plantype seg) 'tempcomp)
         (push seg TCs)
         (5tc-collapse seg)))
  (over (??subseg seg) (5find-tcs _)))

;This ungroups any nested TCs.

(defun 5tc-collapse (tc)
  (over (??& (&* tc) role) -> fact
    (let* ((seg (~seg~roleseg fact)))
      (5tc-fix-slots seg)
      (when (and (not (eq (~rolename fact) 'init))
                 (eq (?plantype (~seg~roleseg fact)) 'tempcomp))
        (if (eq (?plantype seg) 'tempcomp) (5tc-collapse seg))
        (let* ((roles (--& (&* seg) role))
               (dummy (genseg 'dummy))
               (dummy2 (genseg 'dummy)))
          (+subseg tc dummy)          ;prevents plan info from going up.
          (+subseg seg dummy2)        ;prevents plan info form going down.
          (ungroup seg)
          (5kill-dummy tc dummy)
          (5kill-dummy tc dummy2)
          (over roles (assert-fact (&~segcase _ (& tc)))))
        (let ((inits (~nset ~seg ~roleseg (??& (&* tc) role * 'init))))
          (if (cdr inits)
              (let ((new-init (group inits 'expr)))
                (+& (& new-init) plantype 'expr)
                (+& (& tc) role (& new-init) 'init)
                (over inits (+& (& new-init) role (& _) 'action)))))))))

;this saves any slot info so it will come out in the program text.
;note we are making this name with spaces in it that
;will have to be parsed out later.

(defun 5tc-fix-slots (seg)
  (let ((down-slotFs (??& (&* seg) slot))
        (up-slotF (~01 (??& * slot (&* seg)))))
    (when down-slotFs
      (letS ((down-slotF (Elist down-slotFs))
             (new-name (if (and (not-implemented? seg) up-slotF)
                           (intern (format nil "~A OF THE ~A"
                                           (~slotname down-slotF)
                                           (~slotname up-slotF))
                                   *kbe-package*)
                           (~slotname down-slotF))))
        (>& (if up-slotF
                (~segcase up-slotF)
                (& (?supseg seg)))
            slot (~slotseg down-slotF) new-name (~slottype down-slotF))
        (make-fact-be-out down-slotF))
      (if up-slotF (make-fact-be-out up-slotF)))))


(defun 5kill-dummy (sseg dummy)
  (let ((from (~01 ~from (??& (&* sseg) cflow * (&* dummy))))
        (to   (~01 ~to   (??& (&* sseg) cflow (&* dummy)))))
    (if (and from to) (>& (& sseg) cflow from to))
    (kill-seg dummy)))

(declare (special tcomp-order tcomp-order-tc))

;this takes in a tc and combines all of its fragments together into a
;single ssr.  it only works for loops at the moment.  the list (ioc
;[I/O Correspondances]) which is returned is a list of pairs of ports
;in the temporal plan, and the new ports they correspond to.  this is
;used by the coder.  (note that there are several places where less than
;the maximal amount of simplification is done when things are combined
;so that as few segments as possible have to be deleted.  this helps
;the correspondance between old and new ports.  [this is done primarily
;with the inits as they are combined.])

;NOTE we assume that all of the subsegs of the TC can be combined.
;This means that any slots must have dummy ssr insides!

(defun temporal-compose (tc)
  (let* ((tc-init (?role tc 'init))
         (tcomp-order-tc tc)
         (tcomp-order nil)
         (tc-bssr (?role tc 'bssr))
         (tc-subsegs (tc-sorted-ssrs tc))
         new-ssr
         ioc)
    (cond (tc-bssr
           (setq new-ssr tc-bssr)
           (setq tc-subsegs (delq tc-bssr (append tc-subsegs nil)))
           (td-maybe-should-be-split-seg new-ssr))
          (T (setq new-ssr (tcomp-prototype tc))))
    (setq ioc (over tc-subsegs
                nconc (temporal-combine tc new-ssr _)))
    (if tc-init
        (let* ((new-ssr-init (?role new-ssr 'init))
               (dummy-subseg (genseg '*dummy*)))
          (+subseg tc dummy-subseg)
          (move-seg-down new-ssr tc-init) ;dummy stops ungrouping of tc
          (-subseg tc dummy-subseg)
          (cond (new-ssr-init
                 ;note no simplification is done here!!
                 (let ((new-init (group [new-ssr-init tc-init] 'expr)))
                   (+& (& new-ssr) role (& new-init) 'init)
                   (+& (& new-init) plantype 'expr)
                   (+& (& new-init) role (& new-ssr-init) 'action)
                   (+& (& new-init) role (& tc-init) 'action)))
                (t
                 (>& (& new-ssr) role (& tc-init) 'init)))))
    (mini-analyze (& (?role new-ssr 'body)))
    (let* ((dummy-subseg (genseg '*dummy*)))
      (+subseg new-ssr dummy-subseg)
      (ungroup new-ssr) ;note dummy makes plantype info go up for sure!
      (5kill-dummy tc dummy-subseg))
    ioc))

;this checks to see if the new-ssr should be a split because it
;goes to a split case on the TC.

(defun td-maybe-should-be-split-seg (new-ssr)
  (let* ((cflows (??& (&* tc) cflow (&* new-ssr) (&* tc)))
         (case (and cflows (null (cdr cflows))
                    (~case (~to (car cflows))))))
    (when case
      (let ((body (?role new-ssr 'body))
            (rec (?role new-ssr 'rec)))
        (assert-fact
          (&~from (-& (&* body) cflow (&* rec)) (& rec case)))
        (assert-fact
          (&~to (-& (&* body) cflow * (&* body)) (& body case)))
        (assert-fact
          (&~from (-& (&* new-ssr) cflow (&* body)) (& body case)))
        (assert-fact
          (&~to (-& (&* new-ssr) cflow * (&* new-ssr)) (& new-ssr case)))
        (assert-fact
          (&~from (-& (&* tc) cflow (&* new-ssr)) (& new-ssr case)))))))

(defun tc-sorted-ssrs (tc)
  (let ((ssrs (letS ((seg (Elist (??subseg tc))))
                (Rbag (Fselect seg (??& (&* seg) plantype 'ssr)))))
        result)
    (loop do
      (cond ((null ssrs) (return (nreverse result))))
      (letS ((ssr (Elist ssrs))
             (prior-segs (~set ~seg~segcase ~from
                               (??& * dflow * (&* (&* ssr) tinput)))))
        (cond ((null (qset^ prior-segs ssrs))
               (push ssr result)
               (setq ssrs (delq ssr ssrs))
               (done)))))))

;this brings into play some of the capabilities of the analyzer.
;the absolute minimum is done in order not to disturb the basic grouping.
;we just must do some grouping in the body in order for the coder to work.
;Note that we have to do some dflow moving in order to make the output
;code look aesthetic.

(defun mini-analyze (supsegcase)
  (let* ((supseg (~seg supsegcase))
          SEGTYPES PSEGS NSEGS SEGFACTS) ;note that this is critical!
    (3group-up supseg)
    (setq segfacts nil)
    (3dflow-simp1 nil supseg)
;this ungrouping takes away room to move in dflow and so I don't
;think we realy want to have it, but I don't know why it was
;put here in the first place and so am scared to take it out. -- DICK
    (prog ()
      L (dolist (seg (??subseg supseg))
          (cond ((eq (?plantype seg) 'expr)
                 (ungroup seg)
                 (go L)))))
    supsegcase))

;this sets up an empty ssr to add things into

(declare (*fexpr deffacts))

(defun tcomp-prototype (tc)
  (let (bssr)
    (setq bssr
          (~seg (plan-def bssr nil
                          (deffacts bssr
                            (segtype (simple)) (plantype (ssr))
                            (role (bssrbody body) (rec rec) (dj dfjoin))
                            (subseg (bssrbody))
                            (cflow (bssr bssrbody) (bssrbody bssr)))
                          (deffacts bssrbody
                            (segtype (simple)) (plantype (ssr-body))
                            (subseg (rec) (dj))
                            (recursive (rec))
                            (cflow (bssrbody rec) (rec bssrbody)))
                          (deffacts rec
                            (segtype (simple)))
                          (deffacts dj
                            (segtype (join))))))
    (+subseg tc bssr)
    bssr))

(declare (special st-cf-order st-cf-order-body ioc))

;this combines a fragment (aug, filter, or term) with an
;ssr in order to create a bigger ssr.  (at the moment
;this only works with tcs which are loops.) this produces
;ioc for each combination.

(defun temporal-combine (tc bssr frag)
  (if (null (??subseg frag))
       (barf "CODER plan to code has empty ssr: ~A" bssr))
  (let* (dest-segcase ioc (frag-body (?1role frag 'body)))
    (pp-trace *CODER-Debug* `(temporal-combine ,tc ,bssr ,frag))
;look at the temporal inputs in order to
;decide where to put the keyseg in the bssr.  basically it goes as
;high up as possible consistant with the dflow constraints.
    (let* ((l-envs (~nset ~lower-env ~from
                          (??& (&* tc) dflow * (&* (&* frag) tinput))))
           (st-cf-order-body (?1role bssr 'body))
           (st-cf-order nil))
      (setq l-envs
            (letS ((env (Elist l-envs)))
              (Rlist (Fselect env (eq st-cf-order-body
                                      (?supseg (~seg env)))))))
      (if (cdr l-envs) (setq l-envs (sortcar l-envs #'l-env-sort)))
      (setq dest-segcase
            (~to (?& (& st-cf-order-body) cflow
                     (cond (l-envs (car l-envs))
                           (t (& st-cf-order-body)))))))
    (5tc-move-frag-ports tc bssr frag)
    (5tc-move-frag-init bssr frag)
    (5tc-move-frag-body-ports bssr frag)
    (5tc-move-frag-dj bssr frag)
    (5tc-fix-remaining-tports bssr frag)
    (5tc-move-fragkeyseg tc bssr frag dest-segcase)
;check that we have gotten everything
    (if (or (??& (&* frag-body) dflow)
            (??& (&* frag) tinput)
            (??& (&* frag) toutput))
        (barf "TEMPORAL-COMBINE: Some junk left over.~
                ~% TC:   ~S~
                ~% BSSR: ~S~
                ~% Frag: ~S"
                 tc bssr frag))
;kill all remaining junk
    (over (??subseg frag-body) (kill-seg _))
    (kill-seg frag-body)
    (kill-seg frag)
;return the ioc list
    ioc))

;move the inputs of the outer seg of the frag over.  some colapsing
;may occure because some of the values may already be going to the
;bssr.  move over the dflows which touch these inputs (e.g. switch to
;new port and change the supersegs).  Note that the stuff pushed onto ioc
;is odd due to the fact that the bssr will be ungrouped at the end.
;  Next move the outputs of the outer seg over.  no collapsing can happen.
;move dflow too.  Note some additional work has to be done on the
;body-ssr dflow later.

(defun 5tc-move-frag-ports (tc bssr frag)
  (over (--input frag) -> in
        (car (--& (&* tc) dflow * in)) -> df-to-in
        (and df-to-in
             (car (??& (&* tc) dflow (~from df-to-in)
                       (&* (&* bssr) input)))) -> parallel-df
        (cond (parallel-df (~to parallel-df))
              (t (+& (& bssr) input nil (genobj bssr (~obj in)) nil)))
          -> new-in
    (when df-to-in
      (if (null parallel-df) (assert-fact (&~to df-to-in new-in)))
      (push [in ! (~from df-to-in)] ioc))
    (over (--& (&* frag) dflow in) -> df
      (>& (& bssr) dflow new-in (~to df))))
  (over (--output frag) -> out
        (+& (& bssr) output nil (genobj bssr (~obj out)) nil) -> new-out
        (car (--& (&* frag) dflow * out)) -> df-to-out
        (??& (&* tc) dflow out) -> dfs-from-out
    (if dfs-from-out (push [out ! (~to (car dfs-from-out))] ioc))
    (cond (df-to-out (+& (& bssr) dflow (~from df-to-out) new-out))
          (t (barf "TC-MOVE-FRAG-INPUTS: Frag output with no dflow to it.~
                    ~% Out: ~:="
                      out)))
    (dolist (df dfs-from-out)
      (+& (& tc) dflow new-out (~to df)))))

;move init (if there is one) and dflow from it over (dflow to it came
;over above).  If there was a bssr init too, then combine them together
;into a single new init.  Note that the result must be an AND.
;note that no simplification is done, so that the old init will never
;be ungrouped.

(defun 5tc-move-frag-init (bssr frag)
  (let* ((fraginit (?role frag 'init))
         (bssrinit (?role bssr 'init)))
    (cond (fraginit
           (-& (&* frag) subseg (&* fraginit))
           (-& (&* frag) role (&* fraginit))
           (>& (& bssr) subseg (& fraginit))
           (>& (& bssr) role (& fraginit) 'init)
           (dolist (df (--& (&* frag) dflow (&* (&* fraginit) output)))
             (>& (& bssr) dflow (~from df) (~to df)))
           (if bssrinit
               (cond ((eq 'and (?plantype bssrinit))
                      (move-seg-down bssrinit fraginit)
                      (+& (& bssrinit) role (& fraginit) 'action))
                     (t
                      (let ((new-init (group [fraginit bssrinit] 'and)))
                        (+& (& bssr) role (& new-init) 'init)
                        (+& (& new-init) plantype 'and)
                        (+& (& new-init) role (& bssrinit) 'action)
                        (+& (& new-init) role (& fraginit) 'action)))))))))

;move the inputs of the body/rec over and all dflow touching them.
;Also move over temporal ports (note tinput only to rec, toutput only from
;body).  The tinputs and the associated tdflow are converted into normal
;dflow.  There can be collapsing of ordinary inputs iff there is direct
;body rec dflow (to coressponding ports) in both frag and bssr!
;Next move outputs of body/rec over. dflow and temporal ports too.
;(no collapsing is possible.)

(defun 5tc-move-frag-body-ports (bssr frag)
  (let* ((fragbody (?1role frag 'body))
         (fragrec (?1role frag 'rec))
         (bssrbody (?1role bssr 'body))
         (bssrrec (?1role bssr 'rec)))
    (over (??input fragbody) -> in
          (?& (&* fragrec) input * (~obj in)) -> rec-in
          (car (--& (&* bssr) dflow * in)) -> df-to-in  ;note moved
          (if df-to-in (car (??& (&* bssr) dflow (~from df-to-in)
                                 (&* (&* bssrbody) input))))
            -> parallel-df
          (and parallel-df
               (??& (&* bssrbody) dflow (~to parallel-df)
                    (&* (&* bssrrec) input * (~obj (~to parallel-df))))
               (??& (&* fragbody) dflow in rec-in))
            -> can-share
          (cond (can-share (~to parallel-df))
                (t (+& (& bssrbody) input nil
                       (genobj bssrbody (~obj in)) nil)))
            -> new-in
          (cond (can-share (?& (&* bssrrec) input * (~obj new-in)))
                (t (+& (& bssrrec) input nil
                       (genobj bssrrec (~obj new-in)) nil)))
            -> new-rec-in
      (if (and df-to-in (null can-share))
          (assert-fact (&~to df-to-in new-in)))
      (push [rec-in ! new-rec-in] ioc)
      (push [in     ! new-in    ] ioc)
      (over (--& * dflow in) -> df          ;note might have moved already!
        (>& (& bssrbody) dflow new-in (~to df)))
      (over (--& * dflow * rec-in) -> df    ;note might have moved already!
        (>& (& bssrbody) dflow (~from df) new-rec-in))
      (5tc-move-toutput in new-in bssr)
      (5tc-move-tinput rec-in new-rec-in bssrbody)
      (kill-port-and-commentary in)
      (kill-port-and-commentary rec-in))
    (over (??output fragbody) -> out
          (?& (&* fragrec) output * (~obj out)) -> rec-out
          (+& (& bssrbody) output nil
              (genobj bssrbody (~obj out)) nil) -> new-out
          (+& (& bssrrec) output nil
              (genobj bssrrec (~obj new-out)) nil) -> new-rec-out
      (push [rec-out ! new-rec-out] ioc)
      (push [out     ! new-out]     ioc)
      (over (--& (&* fragbody) dflow rec-out) -> df
        (>& (& bssrbody) dflow new-rec-out (~to df)))
      (over (--& * dflow * out) -> df  ;may have moved already
        (>& (& bssrbody) dflow (~from df) new-out))
      (over (--& (&* bssr) dflow out) -> df  ;half moved above.
        (assert-fact (&~from df new-out)))
      (5tc-move-toutput rec-out new-rec-out bssr)
      (5tc-move-tinput out new-out bssrbody)
      (kill-port-and-commentary out)
      (kill-port-and-commentary rec-out))))

;move dfjoin over e.g. move all its ports over to the bssr-dfjoin and
;move all of the dflow (most of which will aready probably have been
;moved).  No collapsing possible.  move tios too as above.
;moved-out is an alist linking old outs with new ones.

(def-alistq moved-out)

(defun 5tc-move-frag-dj (bssr frag)
  (let* (moved-out
         (fragdj (?1role frag 'dfjoin))
         (bssrdj (?1role bssr 'dfjoin))
         (bssrbody (?1role bssr 'body)))
    (over (??output fragdj) -> out
          (>& (& bssrdj) output nil
              (genobj bssrdj (~obj out)) nil) -> new-out
      (setf (moved-out out) new-out)
      (push [out ! new-out] ioc)
      (over (--& * dflow out) -> df
        (>& (& bssrbody) dflow new-out (~to df)))
      (5tc-move-toutput out new-out bssr))
;
    (over (--input fragdj) -> in
          (>& (& bssrdj (~case (~segcase in))) input 'id
              (genobj bssrdj (~obj in))
              (~obj (moved-out (~id-port in)))) -> new-in
      (push [in ! new-in] ioc)
      (over (--& * dflow * in) -> df
        (>& (& bssrbody) dflow (~from df) new-in))
      (5tc-move-tinput in new-in bssrbody))
    (over moved-out -> [out ! _] (make-fact-be-out out))))

;this little fn compares two segs in order to see which is further down
;in the control flow.

(defun l-env-sort (s1 s2)
  (memoize st-cf-order [s1 ! s2]
    (cond ((eq s1 st-cf-order-body) nil)
          ((eq s1 s2) t)
          (t
           (l-env-sort
            (~seg (~from (a?& (&* st-cf-order-body) cflow * (&* s1))))
            s2)))))

;these take care of moving tports when a port they might be touching
;is moved.

(defun 5tc-move-toutput (port new-port bssr)
  (dolist (tout (??& * toutput * * port))
    (5tc-move-toutput1 tout new-port bssr)))

(defun 5tc-move-toutput1 (tout new-port bssr)
  (let* ((port (~lower-io tout))
         (new-tout
          (>& (& bssr) toutput
              (cond ((and port (equal (~lower-env tout) (~segcase port)))
                     (~segcase new-port))
                    (t
                     (~lower-env tout)))
              (genobj bssr (~obj tout)) new-port)))
    (over (--& * dflow tout) (assert-fact (&~from _ new-tout)))
    (kill-port-and-commentary tout)))

(defun 5tc-move-tinput (port new-port bssrbody)
  (dolist (tin (??& * tinput * * port))
    (5tc-move-tinput1 tin new-port bssrbody)))

(defun 5tc-move-tinput1 (tin new-port bssrbody)
  (let* ((port (~lower-io tin))
         (source (~01 ~lower-io ~from (--& * dflow * tin))))
    (cond (port
           (when (and (null source) (??& * toutput * * tin))
             (let* ((seg (genseg 'dummy))
                    (in (>& (& seg) input nil
                           (genobj seg (~obj tin)) nil))
                    (out (>& (& seg) output nil
                             (genobj seg (~obj tin)) nil))
                    (cflow (-& (&* bssrbody) cflow (&* bssrbody))))
               (+& (& seg) @seg 'funcall '5noop nil)
               (+& (& seg) segtype 'simple)
               (+& in @io 'actual-arg nil 1)
               (+& out @io 'returnval nil nil)
               (+& (& bssrbody) subseg (& seg))
               (+& (& bssrbody) cflow (~from cflow) (& seg))
               (+& (& bssrbody) cflow (& seg) (~to cflow))
               (setq source out)))
           (cond (source
                  (cond ((or (eq bssrbody (?supseg (~seg~segcase source)))
                             (eq bssrbody (~seg~segcase source)))
                         (>& (& bssrbody) dflow source new-port)
                         (5tc-move-toutput tin source
                                           (?supseg bssrbody)))
                        (T ;special init->init dflow case
                         (implement-dflow source new-port))))))
          (source (5tc-move-toutput tin source (?supseg bssrbody)))
          (t
           (let* ((bssr (?supseg bssrbody))
                  (new-tin (+& (& bssr) tinput (& bssr)
                               (genobj bssr (~obj tin)) nil)))
             (5tc-move-toutput tin new-tin bssr))))
    (kill-port-and-commentary tin)))

;fix all of the remaining tinputs to frag.  by converting
;them to ordinary dflow.  Note that if a tinput goes
;directly to a toutput, then this is done by changing the
;toutput.  (note that a tinput can be used both ways.  if
;it is only used with a tout, then it will not have a
;~lower-io.)  Then move over the remaining temporal outputs.

(defun 5tc-fix-remaining-tports (bssr frag)
  (let* ((fragkeyseg (?role frag 'op))
         (bssrbody (?1role bssr 'body)))
    (dolist (tin (??& (&* frag) tinput))
      (5tc-move-tinput1 tin (~lower-io tin) bssrbody))
    (when fragkeyseg
      (dolist (tout (??& (&* frag) toutput (&* fragkeyseg)))
        (5tc-move-toutput1 tout (~lower-io tout) bssr)))
    (dolist (tout (??& (&* frag) toutput))
      (5tc-move-toutput1 tout (~lower-io tout) bssr))))

;now actually move the keyseg into the bssr (this is the only part
;which differs based on what the frag's plantype is).
;Note that in a few special pure dflow ssrs there is no keyseg!

(defun 5tc-move-fragkeyseg (tc bssr frag dest-segcase)
  (let* ((bssrbody (?1role bssr 'body))
         (bssrrec (?1role bssr 'rec))
         (fragtype (~rolename (?& (&* tc) role (&* frag))))
         (fragbody (?1role frag 'body))
         (fragrec (?1role frag 'rec))
         (fragkeyseg (?role frag 'op))
         (controlling-segcase
          (~from (?& (&* bssrbody) cflow * dest-segcase))))
    (if fragkeyseg
        (let ((old-cf (-& (&* bssrbody) cflow controlling-segcase)))
          (>& (& bssrbody) cflow controlling-segcase (& fragkeyseg))
          (>supseg bssrbody fragkeyseg)
          (cond ((memq fragtype '(gen acc trn))
                 (>& (& bssrbody) cflow (& fragkeyseg) (~to old-cf)))
                ((eq fragtype 'filter)
                 (let ((fjoin (~seg (~to (a?& (&* fragbody) cflow
                                                (&* fragkeyseg))))))
                   (>supseg bssrbody fjoin)
                   (>& (& bssrbody) cflow (& fjoin) (~to old-cf))
                   (dolist (cf (--& (&* fragbody) cflow (&* fragkeyseg)))
                     (assert-fact (&~segcase cf (& bssrbody))))))
                (t ;(eq fragtype 'term)
                   ;new case is taken from the case of the tc
   (dolist (cf (--& (&* fragbody) cflow (&* fragkeyseg)))
     (cond ((eq (~seg (~to cf)) fragrec)
            (>& (& bssrbody) cflow (~from cf) (~to old-cf)))
           (t (assert-fact (&~segcase cf (& bssrbody)))
              (>supseg bssrbody (~seg (~to cf)))
              (let* ((cf0 (-& (&* fragbody) cflow * (&* (~seg (~to cf)))))
                     (cf1 (-& (&* fragbody) cflow (&* (~seg (~to cf)))))
                     (cf2 (-& (&* frag) cflow (~to cf1)))
                     (cf3 (-& (&* tc) cflow (~to cf2)))
                     (ocf0 (car (??& (&* bssrbody) cflow (& bssrrec))))
                     newcf1 newcf2)
                (cond (ocf0
                       (make-fact-be-out ocf0)
                       (assert-fact
                         (&~from ocf0 (& bssrrec (~case (~to cf3)))))
                       (--& (&* bssr) cflow (& bssrbody) (& bssr))
                       (setq ocf0 (-& (&* bssrbody) cflow * (& bssrbody)))
                       (assert-fact (&~to ocf0 (~to cf0))))
                      (t (>& (& bssrbody) cflow
                             (& bssrrec (~case (~to cf3))) (~to cf0))))
                (setq newcf1 (>& (& bssrbody) cflow (~from cf1)
                                 (& bssrbody (~case (~to cf3)))))
                (setq newcf2 (>& (& bssr) cflow (~to newcf1)
                                 (& bssr (~case (~to cf3)))))
                (>& (& tc) cflow (~to newcf2) (~to cf3)))
              (when (cdr (??& (&* tc) cflow (&* bssr)))
                (-& (&* bssr) segtype)
                (>& (& bssr) segtype 'split)
                (-& (&* bssrbody) segtype)
                (>& (& bssrbody) segtype 'split)
                (-& (&* bssrrec) segtype)
                (>& (& bssrrec) segtype 'split)))))))))))

(declare (unspecial st-cf-order st-cf-order-body ioc))

;the programs which annotate the code work on two basic
;stores of knowledge a partial order keeping track of all
;of the ordering constraints embodied in the plan and in
;the variable names chosen, and the set of all the dflow
;regions in the program.  (a dflow region is a source
;port, together with all of the ports the data value
;travels through and to.)  the key facts are that each
;dflow region must be internally consistant (eg use the
;same variable name everywhere) and that if the same name
;is used in two regions, then they cannot over lap.

;each region has the following form
;  [SOURCE NAME [1ST 2ND 3RD] INTERMEDIATES ! DESTINATIONS]
;see 5FIND-REGIONS and 5SUGGEST-NAME for descriptions of the parts.

;note that this assumes that terminal segs are essentially already
;annotated right.

;note that the inputs to an ssr-body are treated specially all over the
;place.  this is due to the fact that they must be treated as
;starting regions even though they appear to be intermediate in
;the plan.

;{{there is some extent to which terminal segs which take
;  freevar inputs and outputs can still mess things up a lot.
;  if the value is carried to that seg in some other variable,
;  then a setq will have to be done, and the program doesn't check
;  that this will really work.}}

(defun 5annotate (seg)
  (let* (NAME-CLASSES SEGS-WITH-RETURNVALS (counter 0)
         (temp-vars '(x y z u v w a b c)))
    (5abstract-ordering seg nil)
    (5close)
    (5find-regions seg)
    (5suggest-names) ;uses old @ios
    (5clean@assertions seg) ;important is just here
    (setq REGIONS (nreverse REGIONS)) ;put important ones first.
    (over REGIONS (5determine-name-class _))
    (5determine-nesting seg
     (~01 ~deep (??& (&* (&* seg) output) @io 'returnval)))
    (5close)
    (format-trace *CODER-Debug* "~&The regions are ~Q~
                                 ~&The name classes are ~Q" #'dr #'dn)

;first annotate every region which has a good guess at the data flow.
    (over REGIONS -> [![_ name [1st 2nd 3rd] ! _] = region]
      select (and (null name) 1st)
      (cond ((eq 1st '_return) (5try-make-be-returnval region))
            (t (5try-make-be-name region 1st))))

;use name classes to suggest names for related regions.
    (5use-name-classes 20)

;next try any other names which have been suggested
    (over REGIONS -> [![_ name [1st 2nd 3rd] ! _] = region]
      select (and 2nd (null name)
                  (dolist (3name 2nd)
                    (return-if (5try-make-be-name region 3name)))))

;use name classes to suggest names for anybody who is left
    (5use-name-classes 0)

;next try and see if we can figure out a name from the function that
;creates the value.
   (over REGIONS -> [![_ name ! _] = region]
      select (null name)
      (dolist (var (3make-up-name region))
        (return-if (5try-make-be-name region var))))

;use name classes to suggest names for anybody who is left
    (5use-name-classes 0)

;next try any blah names that were suggested.
    (over REGIONS -> [![_ name [1st 2nd 3rd] ! _] = region]
      select (and 3rd (null name)
                  (dolist (3name 3rd)
                    (return-if (5try-make-be-name region 3name)))))

;use name classes to suggest names for anybody who is left
    (5use-name-classes 0)

;lastly, if you must, make up a temporary variable name and use it
    (over REGIONS -> [![_ name ! _] = region]
      select (null name)
      (let* ((temp (repeat (name)
                     (setq name (when-pop-fails temp-vars
                                  (symbolconc 'TEMP (incf counter))))
                     (if (not (memq name VAR-NAMES-USED))
                         (return name)))))
        (5try-make-be-name region temp)            ;cannot fail
        (dolist (r (5get-related-regions region 0)) ;use the name as many
          (when (null (cadr r))                     ;times as you can
            (5try-make-be-name r temp)))))
;
    (5cleanup seg)))

(defun 5use-name-classes (min)
  (prog (did-something)
    L (setq did-something nil)
      (over REGIONS -> [![_ name ! _] = region]
        select (null name)
        (over (5get-related-regions region min) -> [_ n ! _]
          select (and n (not (memq n '(_return _))))
          (when (5try-make-be-name region n)
            (setq did-something T)
            (return))))
      (if did-something (go L))))

(defun 3make-up-name (region)
  (let* ((source (car region))
         (seg (~seg~segcase source))
         var)
    (errset (and (??& source @io 'returnval)
                 (setq var (3make-up-name-for-returnval seg))) nil)
    (cond (var (list var)))))

;this is only a stub now.  It could do all kinds of things
;for more reasonable guessing. (this is used other places too.)

(defun 3make-up-name-for-returnval (seg)
  (caseq (?@seg-name seg)
    ((ada-fn~query_user_for_date query_user_for_date) 'date)
    (T nil)))

;this cleans up all the @ assertions, keeping only the minimally necessary
;information.  The information which must be kept is the @SEGs on the
;terminal segs, and the @IOs on the terminal segs and the TOP-SEG.
;in addition, it creates new @segs for every non-terminal seg.

(defun 5clean@assertions (seg)
  (--& (&* (&* seg) port) @createvar)
  (let* ((@seg (~01 (--& (&* seg) @seg)))
         (@segkind (if @seg (~kind @seg)))
         (@segname (if @seg (~name @seg))))
    (if (memq @segkind '(opencode funcall constant lambda-exp))
        (incf NUM-SEGS)) ;this is displayed by the coder
    (cond ((eq @segkind 'lambda-exp)
           (>& (& seg) @seg 'lambda-exp @segname nil))
          ((5terminal? seg)
           (cond ((??& (&* seg) segtype 'join) ;might not have had one
                  (>& (& seg) @seg 'join nil nil)
                  (--& (&* (&* seg) port) @io))
                 ((eq @segkind 'funcall)
                  (>& (& seg) @seg 'funcall (5get-lisp-name @segname) nil))
                 ((eq @segkind 'constant)
                  (>& (& seg) @seg 'constant
                      (5get-lisp-constant @segname) nil))
                 ((??& * recursive (&* seg)) ;might not have had one
                  (>& (& seg) @seg 'recursion nil nil)
                  (--& (&* (&* seg) port) @io))
                 (t
                  (>& (& seg) @seg 'opencode nil nil)
                  (if (?supseg seg) (--& (&* (&* seg) port) @io)))))
          ;; part of the gross cliche definition hack
          ((and (eq @segkind 'opencode) (get @segname 'cliche-comments))
           (>& (& seg) @seg 'opencode @segname nil))
          (t
           (>& (& seg) @seg 'opencode nil nil)
           (if (?supseg seg) (--& (&* (&* seg) port) @io))))
    (over (??subseg seg) (5clean@assertions _))))

;this runs over the whole plan and abstracts out all of
;the partial ordering information encoded in the cflow
;and dflow.  This is used below to determine whether or
;not two dflow regions overlap.  note special encodings
;of joins.

(defun 5abstract-ordering (seg supseg)
  (dolist (dflow (??& (&* seg) DFLOW))
    (5port-make-precede (~from dflow) (~to dflow)))
  (over (??& (&* seg) cflow) -> cflow
        (~from cflow) -> fsegcase
        (~to cflow) -> tsegcase
    (5make-precede
      (5order-id fsegcase (cond ((eq (~seg fsegcase) seg) 'input)
                                (t 'output)))
      (5order-id tsegcase (cond ((eq (~seg tsegcase) seg) 'output)
                                (t 'input)))))
  (let* ((segin (5order-id (& seg) 'input)) ;note segin=segout if terminal
         (segout (5order-id (& seg) 'output))
         (segins (makeset  ;segins=(& segin)/nil  if not join
                     [!(over (if supseg (??& (&* supseg) cflow * (&* seg)))
                         bag (5order-id (~to _) 'input))
                      !(over (??& (&* seg) input) bag (5port>order-id _))]))
         (segouts (makeset  ;segouts=(& segout)/nil if not split
                    [!(over (if supseg (??& (&* supseg) cflow (&* seg)))
                         bag (5order-id (~to _) 'output))
                      !(over (??output seg) bag (5port>order-id _))])))
    (over segins (5make-precede _ segin))
    (over segouts (5make-precede segout _))
    (dolist (subseg (??subseg seg))
      (5make-precede segin (5order-id (& subseg) 'input))
      (5make-precede (5order-id (& subseg) 'output) segout)
      (5abstract-ordering subseg seg))))

;this runs over the whole plan, and finds all of the dflow regions which
;it stores in REGIONS.  Each region is of the form
;   [source name [1st 2nd 3rd] intermediates ! destinations]
;regions are started by every port which does not have dflow to it
;(ie a top level input, a bottom level output, or an intermediate level
;port where the dflow is incomplete).  In addition, a region is started for
;every input of an SSR-BODY that doesn't have dflow directly to the
;corresponding input of the rec (ie which changes in the loop).
;(Note that data flow that carries data to places where it isn't
;actually used, is ignored in all this.  EG if some data flow starts
;at some output and then goes through some joins and stuff but
;isn't actually used anywher then no regions will be made for it.)

(defun 5find-regions (seg)
  (mapc #'5find-region (??input seg))
  (mapc #'5find-region (??output seg))
  (mapc #'5find-regions (??subseg seg)))

;returns the region for a port if any.
(defun 5find-region (port)
  (cond ((cdr (assq port regions)))
        ((eq (5region-classify port) 'head)
         (let (((int . dest) (5find-successors port)))
           (when dest
             (push [port nil nil int ! dest] REGIONS)
             (car regions))))))

;returns port description
(defun 5region-classify (port)
  (memoizeq 5CLASSIFY port
    (let* ((seg (~seg~segcase port))
           (ptype (5ptype seg))
           (dflow-from (??& * dflow port))
           (dflow-to (??& * dflow * port))
           (complex-bssr-port
             (and (input-fact? port)
                  (eq ptype 'ssr-body)
                  (null (??& (&* seg) dflow port
                             (&* (&* (?recurrence seg))
                                 input * (~obj port)))))))
      (cond ((and dflow-from dflow-to)
             (if complex-bssr-port 'head 'inter))
            ((and (not dflow-from) (not dflow-to)) 'ignore)
            (dflow-from 'head)
            ((output-fact? port)
             (if (or (null (?supseg seg))
                     ;;needed for trailing-pointer list enumeration
                     (memq ptype '(ssr-body ssr)))
                 'dest 'ignore))
            ((??subseg seg) 'ignore)
            ((eq (?segtype seg) 'join) ;NOTE RECURSION HERE
             (if (5find-region (?& (&* seg) output * (~id port)))
                 'dest 'ignore))
            (T 'dest)))))

;returns intermediate and destination ports if any.
(defun 5find-successors (port)
  (let ((inter) (dest))
    (letS ((to (~to (Elist (??& * dflow port)))))
      (caseq (5region-classify to)
        ((head dest) (push to dest))
        (inter (let (((sub-inter . sub-dest) (5find-successors to)))
                 (setq inter (append sub-inter [to] inter))
                 (setq dest (append sub-dest dest))))))
    (cons inter dest)))

;this is for debugging, it prints out REGIONS in a readable way.
(defun dr (&optional (full? nil) (supress_returns nil))
  (dolist (region REGIONS)
    (let (((s n suggestions is . ds) region))
      (when (not (and supress_returns (eq n '_return)))
        (pp:pp [(compress-port s) n suggestions
                !(if full? [(over is bag (compress-port _))
                            (over ds bag (compress-port _))])]))))
  (values))

;this prints out name-classes

(defun dn ()
  (dolist (class name-classes)
    (let (((a n b) class))
      (pp:pp [(compress-port (car a)) n (compress-port (car b))])))
  (values))

;this is one of the most critical parts of the coder,  It should really
;work by constraint propogation so that it can balance the choice in
;one region against choices in others in order to reduce the number of
;setqs.  It should also not reuse the same name unless there is some
;logical reason to do so.  (The name classes are a (not to great)
;attempt at this.)
;As a hack more or less, it looks at what the original user did in order
;to try and pick reasonable variable names.

;This fn looks at a region and makes suggestions as to the var name to use
;for dflow in the region.  it returns a triple [1st 2nd 3rd] where
;1st - 3rd -- are suggested names in order of how good an idea they are.
;1st -- name chosen because the source has a name specified for it, and
;    most of the uses have the same name specified.  (Determination of
;    nesting may eventually put _RETURN in this spot indicating that a
;    retval should be used)  The idea here is that in either case we
;    have solid evidence that this name was actually used before.
;2nd -- not used at the moment
;3rd -- a list of every other name that occures in some @ assertion for
;       the region.  they are ordered according to how often they
;       appeared.
;
;note that names beginning with "_" are assumed to be internally
;generated and are ignored.

;if the source is an intermediate level port with no dflow to it, then
;it is given the suggested name "_" which is then used.  (NOTE how this
;is not a suggestion that will get reused when the thing is recoded
;later.)

(defvar *blah-names* '("STRUCTURE" "DATA" "DATUM" "FILE" "RECORD")
  "list of var names suffixes we really don't want to use")

(defun blah-name? (name)
  (let* ((string (string name))
         (start (string-reverse-search-set "-_" string))
         (suffix (if start (substring string (1+ start)) string)))
    (member suffix *blah-names*)))

(defun 5suggest-names ()
  (letS ((region (Elist regions))
         ((port nil nil inter . dest) region)
         (name-info (5suggest-name port inter dest)))
    (setf (caddr region) name-info)))

(defun 5suggest-name (source intermediates dests)
  (let* ((source-name (and (null (?supseg (~seg~segcase source)))
                           (~01 5goodname ~name (??& source @io))))
         (other-names (over [source !! intermediates !! dests] -> port
                         (or (~01 5goodname ~slotname
                                    (??& * slot (~segcase port) * 'output))
                             (~01 5goodname ~varname (??& port @createvar))
                             (~01 5goodname ~name (??& port @io))) -> name
                         select name bag name))
         (blah-names (letS ((name (EList other-names)))
                       (Rlist (Fselect name (blah-name? name))))))
    (when source-name (setq other-names (delq source-name other-names)))
    (setq other-names (qset- other-names blah-names))
    (setq other-names (sort (over other-names classifyq [_])
                            #'(lambda (x y) (> (length x) (length y)))))
    (list source-name (mapcar #'car other-names) blah-names)))

;;; (5GOODNAME sym)
;;;
;;;  Used to filter out system generated names.
;;;
;;;  Returns NIL if sym is NIL or a system name that begins with
;;;  an underscore. Otherwise, returns the sym (which will obviously
;;;  be non-NIL).

(defun 5goodname (sym)
  (cond ((or (null sym)) nil)
        ((not (eq (aref (get-pname sym) 0) #/_)) sym)))

;Looks at a source of a region and checks to see if it is a side-effected
;thing.  if so it enters the appropriate partial order restrictions, and if
;the thing isn't specified directly as a free variable, then it indicates
;that this region should have the same var name as the linked region.
;Note that at this point all of the ordering is completly specified
;except in EXPRs and ANDs.  as a result ordering restrictions only have to
;be introduced if the two regions reach into the same EXPR/AND.  It is
;important that they only be introduced then, or things in CONDs can get
;messed up since the two branches of a COND must remain unordered.
;  In a similar vein, if the head of the region is an output of a join,
;then it suggests that the same name should be used for
;the regions terminating on the corrsponding inputs.
;Also, if the region starts on an input of an ssr-body
;[note no regions pass through an input of an ssr-body]
;then the program suggests that the same name should be
;used for the region ending on the input, and the one
;going to the corresponding input of the rec.  But note
;that we propogate names out of loop, but don't use
;outside name to modify in loop!  (ie we check that one won't
;propogate into the body unless it is really coming
;from a real thing in the init.)
;Note that in these last two cases,
;mere suggestions are being made, while the side-effect stuff is much more
;serious.
;  special care is taken so that the fact that something initializes two
;different ports of an ssr-body will not cause spurious collapsing of
;name-classes.  this is done by including the input region only in
;the port region which has the best chance of suggesting a good name
;[note that other dflow factors can still cause the regions to collapse
;we are ignoring this for now.]
; We also try to propogate the same name across any instances of 5noop.

(defun 5add-name-class (reg1 reg2 binding
                        &optional (reverse-binding binding))
  (when (and reg1 reg2)
    (if (plusp binding)
        (push (list reg1 binding reg2) NAME-CLASSES))
    (if (plusp reverse-binding)
        (push (list reg2 reverse-binding reg1) NAME-CLASSES))))

(defun 5determine-name-class (source-region)
  (let* ((source (car source-region))
         (e-input (and (??& source @io 'side-effect) (~id-port source))))
    (cond (e-input
           (let* ((prior-region (dolist (region REGIONS)
                                  (if (member e-input (cddddr region))
                                      (return region))))
                  (this-id (5port>order-id e-input))
                  (dest-ids (over (cddddr prior-region) -> dest
                                  (5port>order-id dest) -> dest-id
                                  select (not (eq dest-id this-id))
                                  bag dest-id))
                  (other-ports [!!(cadddr prior-region)
                                !!(cddddr prior-region) ! nil])
                  (these-ports (over p <- e-input
                                     by (~from (?& * dflow * p))
                                     until (not (member p other-ports))
                                     bag (setq other-ports
                                               (delete p other-ports))
                                     p)))
             (cond ((over dest-ids fastor (5precedes? this-id _))
                    (warn
                     "5DETERMINE-NAME-CLASS: dflow and side-effects clash.~
                    ~% Source: ~:="
                     source))
                   (t
                    (5constrain< other-ports these-ports '(expr and comp))
                    (5close)))
             (cond ((not (??& e-input @io 'free-var))
                    (5add-name-class source-region prior-region 90)))))
          ((??& (&* (~seg~segcase source)) @seg 'funcall '5noop)
           (over (??& (&* (~seg~segcase source)) input) -> in
                 (dolist (region REGIONS)
                   (if (member in (cddddr region))
                       (return region))) -> prior-region
                 (5add-name-class prior-region source-region 80)))
          ((??& (&* (~seg~segcase source)) segtype 'join)
           (over (??& (&* (~seg~segcase source)) input
                      'id * (~obj source)) -> in
                 (dolist (region REGIONS)
                   (if (member in (cddddr region))
                       (return region))) -> prior-region
                 (5add-name-class prior-region source-region 80 60)))
          ((and (input-fact? source)
                (eq (5ptype (~seg~segcase source)) 'ssr-body))
           (let* ((rec (?recurrence (~seg~segcase source)))
                  (rec-port (?& (&* rec) input * (~obj source)))
                  (prior-region (dolist (region REGIONS)
                                  (if (member source (cddddr region))
                                      (return region))))
                  (rec-region (dolist (region REGIONS)
                                (if (member rec-port (cddddr region))
                                    (return region)))))
             (if (not (eq (~seg~segcase source)
                          (~seg~segcase (car rec-region))))
                 (5add-name-class source-region rec-region 50 -100))
             (when (and prior-region
                        (over (cddddr prior-region) -> port
                              select (not (equal source port))
                              fastand (not (eq (~seg~segcase port)
                                               (~seg~segcase source)))))
;propogate name out of loop, but don't modify outside name in loop!
               (5add-name-class prior-region source-region 40 -100)
;can pull in a name from a true init however.
               (when (and (not (??& (~segcase (car prior-region))
                                    @seg 'funcall '5noop))
                          (2seg-inside-seg
                            (~seg~segcase (car prior-region))
                            (?supseg (~seg~segcase source))))
                 (5add-name-class
                   prior-region source-region -100 30))))))))

(defun 5constrain< (portsa portsb ptypes)
  (let* ((segsa (5constrain-segs portsa ptypes))
         (segsb (5constrain-segs portsb ptypes)))
    (over segsa -> [sega ! supa]
      (over segsb -> [segb ! supb]
        (if (and (eq supa supb) (not (eq sega segb)))
            (5make-precede (5order-id (& sega) 'output)
                           (5order-id (& segb) 'input)))))))

(defun 5constrain-segs (ports ptypes)
  (over ports -> p
        (~seg~segcase p) -> seg
        (?supseg seg) -> sup
    select (and sup (memq (?plantype sup) ptypes))
    bag [seg ! sup]))

;The next pgm runs over the plan for the seg, and decides what are all
;of the things which can be made returnvals.  As a side effect of
;this, it is determining a complete ordering in each expr, by adding
;orderings to ORDERING.  Note that due to this side-effect, it is very
;important that it looks at EVERY expr.  (note that since all of its
;activities are confined to single segments, and it keeps track of the
;changes it is making to ORDERING, the ordering does not have to be
;reclosed until after all of the nesting has been computed.)  When it
;finds something which can be a returnval, it signals this by putting
;_RETURN as the first suggestion for the region.

;The fn works recursively top down, seg by seg.  it works
;keying off of the plan type.  it takes advantage of the
;fact that since the analyzer did dflow simplyfication,
;NESTING WILL NEVER MOVE A SEGMENT OVER A SEGMENT
;BOUNDRY.  Note that this implies that the intermediate
;ports of a region which is marked as _RETURN must all be
;outputs!

;note that at each point, action is taken with respect to
;the subsegs, and not with respect to the seg you are on.
;info comes down as to whether any port on this seg
;should be a retval, and this info is passed down to the
;appropriate other place.  if the appropriate lower place
;cannot form a retval output, then the region in question
;is broken in two, and the upgoing portion is marked as
;_RETURN, while the lower portion is left alone.  Thus by
;hook or crook, the request for a retval is always
;satisfied.  for this to work, it must be that case that
;whenever 5determine-nesting is called on a terminal seg,
;then a retval can only be asked for where it is
;possible.  (the fact that there is no interlevel look
;ahead as to what can be conveniently be made a retval,
;speeds things up and misses some opportunities for
;optimal nesting.)

;the fn keys off of the following plantypes
;1. ssr -- something from the body may be able to be a retval.
;2. xor -- outputs from actions, and/or join might be a retval (note not init)
;3. pred -- an output of the init may be able to be a returnval.
;           (note it isn't minor that all preds have only one arg!
;            for otherwise we might miss some nesting here. with AND inits)
;          note that niether a pred (or an ssr used as one) can have a retval.
;4. and -- one of the inner pieces may have a retval.
;5. expr, comp --  here is the big work, where the heavy decisions about
;    nesting get made.  The algorithm works backwards from the retval output
;    (if any) trying to nest as many things as possible.
;6. ssr-body -- there is complete cflow in here, and nothing is last, so
;    there cannot be a real retval.  as a result, if one is needed above,
;    the region will have to be broken.  [Note we don't have to do anything
;    here because temporal-compose regroups the interior after it acts.
;    if there are any and/expr/xor regions in there they will have
;    been grouped.]

(declare (special SEGS-TO-DO FIRST-SEG-ID SEGS-PATTERN))

(defun 5determine-nesting (seg retoutput)
  (cond
    ((5terminal? seg)
     (if retoutput (5mark-returnval retoutput)))
    (t
     (let* ((ptype (5ptype seg))
            (inner-ret-source
              (if retoutput (~01 ~from (??& (&* seg) dflow * retoutput))))
            (inner-ret? (and inner-ret-source
                             (memq ptype '(and expr comp xor ssr ssr-body))
                             (5can-be-ret inner-ret-source)))
            (inner-ret-seg
              (if inner-ret? (~seg~segcase inner-ret-source)))
            (subsegs (??subseg seg))
            lower-rets)
       (if inner-ret?
           (setf (lower-rets inner-ret-seg) inner-ret-source))
       (if (and retoutput (not inner-ret?))
           (5mark-returnval (5break-region retoutput)))
       (cond ((memq ptype '(expr comp))
              (let* ((SEGS-TO-DO subsegs)
                     (SEGS-PATTERN (5make-multiple-pattern SEGS-TO-DO))
                     (FIRST-SEG-ID (5order-id (& seg) 'output)))
                (if inner-ret-seg
                    (cond ((5any-follow inner-ret-seg SEGS-PATTERN)
                           (5mark-returnval (5break-region retoutput)))
                          (t (5expr-nesting inner-ret-source inner-ret-seg))))
                (repeat ()
                  (if (null SEGS-TO-DO) (return))
                  (let* ((possible-segs
                           (over SEGS-TO-DO
                                 select (not (5any-follow _ SEGS-PATTERN))
                                 list _)))
                    ;; Some time we may want to sort possible-segs.
                    (5expr-nesting nil (car possible-segs))))))
             (t
              (caseq ptype
                ((AND)
                 (if inner-ret-seg
                     (let* ((id (5order-id (& inner-ret-seg) 'input)))
                       (over subsegs -> seg
                             select (not (eq seg inner-ret-seg))
                             (5make-precede (5order-id (& seg) 'output) id)))))
                ((SSR-BODY)
                 (let ((rec (?recurrence seg)))
                   (if retoutput
                       (setf (lower-rets rec)
                             (?& (&* rec) output * (~obj retoutput))))
                   ;;; This isn't the place to be doing this; we should be getting the
                   ;;; call on the rec right some other way. This gets wrong stuff
                   ;;; some times --DICK
                   (repeat ((ssr-sub rec) ports)
                     (cond ((eq ssr-sub seg) (return))
                           ((and (setq ports (??output ssr-sub))
                                 (null (cdr ports))
                                 (5can-be-ret (car ports)))
                            (setf (lower-rets ssr-sub) (car ports))))
                     (setq ssr-sub
                           (~seg (~from (?& (&* seg) cflow * (&* ssr-sub))))))))
                ((PRED)
                 (over (~1st ??output (??role seg 'init))
                       select (5can-be-ret _)
                       (setf (lower-rets (~seg~segcase _)) _)
                       (return)))
                ((XOR)
                 (if inner-ret?
                     (if (eq (?segtype inner-ret-seg) 'join)
                         (over (??& (&* inner-ret-seg) input
                                    'id * (~obj inner-ret-source)) -> input
                               (~from (?& (&* seg) dflow * input)) -> source
                               select (and (5can-be-ret source)
                                           (??& (&* seg) role
                                                (~segcase source) 'action))
                               (setf (lower-rets (~seg~segcase source)) source))))))
              (over subsegs (5determine-nesting _ (lower-rets _)))))))))

(defun 5mark-returnval (output)
  (let ((region (assoc output REGIONS)))
    (if region (rplaca (caddr region) '_return))))

; Break an intermediate or destination port off to start its own region.
(defun 5break-region (output)
  (cond ((not (5find-region output))
         (push [output nil [nil nil nil] nil output] regions)))
  (let* ((old-region (over (cdr REGIONS)
                           select (or (member output (cadddr _))
                                      (member output (cddddr _)))
                           (return _)))
         (new-region (assoc output REGIONS)))
    (cond (old-region
           (rplacd (cddr old-region)
                   [(delete output (set- (cadddr old-region)
                                         (cadddr new-region)))
                    !(setU [output] (set- (cddddr old-region)
                                         (cddddr new-region)))])
           (5add-name-class old-region new-region 10 0))))
  output)

(defvar 5un-nestable-fns
  '(ada-constant-dcl~pseudo ada-init-dcl~pseudo ada-dcl~pseudo ada-fn~read))

(defun 5can-be-ret (port)
  (and (output-fact? port)
       (or (??& * recursive (~segcase port))
           (and (or (assoc port REGIONS)
                    (??& (~segcase port) dflow * port))
                (not (eq (?segtype (~seg~segcase port)) 'split))
                (not (memq (~01 ~name (??& (~segcase port) @seg))
                           5un-nestable-fns))
                (memq (?@io-kind port) '(nil returnval))
                (null (cdr (??& * dflow port)))
                (null (cdr (??& (&* (~segcase port) output) @io 'returnval)))
                (not (eq (5ptype (~seg~segcase port)) 'ssr))
                (or (??& (~segcase port) @seg 'funcall '5noop)
                    (not (5goes-to-side-effect port)))))))

(defun 5goes-to-side-effect (port)
  (or (??& (&* (&* (~seg~segcase port)) output 'id * (~obj port))
           @io 'side-effect)
      (Ror-fast (5goes-to-side-effect (~to (Elist (??& * dflow port)))))
      (let ((upper (?recursive-supseg (~seg~segcase port))))
        (cond (upper (5goes-to-side-effect
                       (?& (&* upper) input * (~obj port))))))))

(defun 5expr-nesting (retoutput seg &aux maybe-later)
  (setq SEGS-TO-DO (delq seg SEGS-TO-DO)
        SEGS-PATTERN (5remove-from-pattern seg SEGS-PATTERN))
  (let ((this-id (5order-id (& seg) 'output)))
    (5make-precede this-id FIRST-SEG-ID)
    (setq FIRST-SEG-ID this-id))
  (5determine-nesting seg retoutput)
  (over (sort (??& (&* (&* seg) input) @io 'actual-arg)
              #'(lambda (x y) (> (~argno x) (~argno y)))) -> @io
        (car (??& * dflow * (~deep @io))) -> source-df
        (if source-df (~from source-df)) -> source
        select (and source (eq (~keyword source) 'output)
                    (not (5any-follow (~seg~segcase source) SEGS-PATTERN)))
        (cond ((5can-be-ret source)
               (5expr-nesting source (~seg~segcase source)))
              ((??& (&* seg) output 'id * (~obj (~deep @io)))
               (push (~seg~segcase source) maybe-later))))
  (dolist (seg (nreverse maybe-later))
    (if (not (5any-follow seg SEGS-PATTERN))
        (5expr-nesting nil seg))))

; These names come directly from cliches and are taken on faith.
(defvar *unquestionable-vars*
  '(current_output units repairs defects models unit_key repair_index repair))

(defun 5try-make-be-name (region name)
  (cond ((or (eq name '_)
             (if (not (memq name VAR-NAMES-USED)) (push name VAR-NAMES-USED))
             (memq name *unquestionable-vars*)
             (over REGIONS -> reg
                   select (and (eq name (cadr reg))
                               (not (5noop-related region reg)))
                   fastand (not (5regions-overlap region reg))))
         (setf (cadr region) name)
         (5annotate-var-dflow name region)
         t)))

(defun 5noop-related (region1 region2)
  (or (5nr region1 region2) (5nr region2 region1)))

(defun 5nr (region1 region2)
  (and (??& (~segcase (car region2)) @seg 'funcall '5noop)
       (memq (?& (~segcase (car region2)) input) (cddddr region1))))

; Reusing a name can introduce ordering constraints in an AND.
(defun 5regions-overlap (region1 region2)
  (let* (((sourcea nil nil intera . destsa) region1)
         ((sourceb nil nil interb . destsb) region2)
         (heada (5port>order-id sourcea))
         (tailsa (over destsa bag (5port>order-id _)))
         (headb (5port>order-id sourceb))
         (tailsb (over destsb bag (5port>order-id _)))
         (heada<headb (5precedes? heada headb))
         (headb<heada (5precedes? headb heada)))
    (cond ((cond (heada<headb (over tailsa fastor (5precedes? headb _)))
                 (headb<heada (over tailsb fastor (5precedes? heada _)))
                 (t (over [headb ! tailsb] -> idb
                          fastor (over [heada ! tailsa] -> ida
                                       fastor (or (5precedes? ida idb)
                                                  (5precedes? idb ida)))))))
          (t
           (cond (heada<headb
                  (5constrain< [sourcea !! intera ! destsa]
                               [sourceb !! interb ! destsb] '(and)))
                 (t (5constrain< [sourceb !! interb ! destsb]
                                 [sourcea !! intera ! destsa] '(and))))
           (5close)
           nil))))

(defun 5try-make-be-returnval (region)
  (when (5annotate-return-dflow region)
    (setf (cadr region) '_return)
    T))

(defun 5annotate-var-dflow (name region)
  (let* (((source nil nil intermediates . dests) region)
         (source@io (car (??& source @io))))
    (cond ((null source@io) (>& source @io 'free-var name nil))
          ((and (eq name (~name source@io)) (eq (~kind source@io) 'free-var)))
          ((memq (~kind source@io) '(free-var returnval))
           (>& source @createvar name))
          ((memq (~kind source@io) '(lambda-arg side-effect actual-arg))
           (assert-fact (&~name (make-fact-be-out source@io) name)))
          (t (barf "5ANNOTATE-VAR-DFLOW: Bad source @io.~% Data: ~:=" source@io)))
    (dolist (port intermediates)
      (>& port @io 'free-var name nil))
    (over dests -> port
          (car (??& port @io)) -> @io
          (cond ((and (??& * dflow port)
                      (eq (5ptype (~seg~segcase port)) 'ssr-body)))
                ((null @io) (>& port @io 'free-var name nil))
                ((or (eq (~name @io) name)
                     (memq (~kind @io) '(returnval side-effect free-var))))
                ((eq (~kind @io) 'actual-arg)
                 (assert-fact (&~name (make-fact-be-out @io) name)))
                (t (barf "5ANNOTATE-VAR-DFLOW: Bad dest @io.~% Data: ~:=" @io))))))

(defun 5annotate-return-dflow (region)
  (let* (((source nil nil intermediates . dests) region)
         (source@io (car (??& source @io)))
         (dest@io (car (??& (car dests) @io))))
    (prog ()
      (if (cdr dests) (return nil))
      (if (not (or (null source@io) (eq (~kind source@io) 'returnval)))
          (return nil))
      (if (Ror-fast (input-fact? (Elist intermediates))) (return nil))
      (cond ((null dest@io) (>& (car dests) @io 'returnval nil nil))
            ((memq (~kind dest@io) '(returnval side-effect free-var)))
            ((eq (~kind dest@io) 'actual-arg)
             (assert-fact (&~name (make-fact-be-out dest@io) nil)))
            (t (return nil)))
      (if source@io (make-fact-be-out source@io))
      (>& source @io 'returnval nil nil)
      (dolist (port intermediates) (>& port @io 'returnval nil nil))
      (return T))))

(defun 5cleanup (seg)
  (over (??subseg seg) -> subseg
        (?recursive-supseg subseg) -> supseg
        (cond (supseg
               (over (??& (&* (&* subseg) input) @io) -> in@io
                     (?& (&* (&* supseg) input *
                             (~obj (~deep in@io))) @io) -> top@io
                     select (or (not (eq (~kind top@io) (~kind in@io)))
                                (not (eq (~name top@io) (~name in@io))))
                     (make-fact-be-out in@io)
                     (assert-fact (&~deep top@io (~deep in@io)))))
              ((eq (?segtype subseg) 'join)
               (dolist (out@io (??& (&* (&* subseg) output) @io))
                 (let ((in@ios (??& (&* (&* subseg) input 'id
                                       * (~obj (~deep out@io))) @io)))
                   (dolist (in@io in@ios)
                     (when (or (not (eq (~kind out@io) (~kind in@io)))
                               (not (eq (~name out@io) (~name in@io))))
                       (make-fact-be-out in@io)
                       (assert-fact (&~deep out@io (~deep in@io))))))))
              (t (over (??& (&* (&* subseg) output) @io 'side-effect) -> se@io
                       (?& (~id-port (~deep se@io)) @io) -> in@io
                       (when (and (??& * dflow (~deep se@io))
                                  (~name in@io)
                                  (not (eq (~name se@io) (~name in@io))))
                         (assert-fact (&~name (make-fact-be-out se@io)
                                             (~name in@io)))
                         (>& (~deep se@io) @createvar (~name se@io))))))
        (5cleanup subseg)))

(defun 5terminal? (seg)
  (memoizeq 5TERMINAL? seg (null (??subseg seg))))

(defun 5ptype (seg)
  (memoizeq 5PTYPE seg (?plantype seg)))

(defun 5port>order-id (port)
  (5order-id (~segcase port) (~keyword port)))

(defun 5port-precedes? (porta portb)
  (5precedes? (5order-id (~segcase porta) (~keyword porta))
              (5order-id (~segcase portb) (~keyword portb))))

(defun 5port-make-precede (porta portb)
  (5make-precede (5order-id (~segcase porta) (~keyword porta))
                 (5order-id (~segcase portb) (~keyword portb))))

; NAME-CLASSES entries are (region1 priority region2).
(declare (special SO-FAR))
(defun 5get-related-regions (region min)
  (let ((SO-FAR [region]))
    (5get-related-regions1 region min)
    (cdr (nreverse SO-FAR))))

(defun 5get-related-regions1 (region min)
  (let* (hits rs)
    (over NAME-CLASSES -> [region1 num region2]
          select (> num min)
          (cond ((eq region region1) (push [num ! region2] hits))))
    (setq hits (sortcar hits #'<))
    (setq rs (over hits revlist (cdr _)))
    (dolist (r rs)
      (when (not (memq r SO-FAR))
        (push r SO-FAR)
        (5get-related-regions1 r min)))))

; Acyclic partial order, encoded as lists of fixnums in vectors.
(arraydefs (5flag t) (5immediate t) (5pattern t) (5related t))

(defun 5init-order ()
  (setq 5LIMIT       23.
        5FLAG        (*array nil t 5LIMIT)
        5IMMEDIATE   (*array nil t 5LIMIT)
        5PATTERN     (*array nil t 5LIMIT)
        5RELATED     (*array nil t 5LIMIT)
        5MAXID       -1
        5NEXTBIT     1
        5BIT-PATTERN (list 1)
        5PLY         1
        5IDS         nil
        5CLOSED      t))

(defun 5order-id (segcase key)
  (let* ((seg (~seg segcase)) (case (~case segcase)))
    (setq key (cond ((and (null case) (5terminal? seg)) seg)
                    (t [seg key ! case])))
    (or (cdr (assoc key 5IDS)) (5new-id key))))

(defun 5bd (bit-pattern)
  (5nd (over i <- 5maxid by (1- i) until (minusp i)
             select (5bit^ (5pattern i) bit-pattern)
             bag i)))

(defun 5nd (num)
  (cond ((numberp num)
         (over 5IDS -> [key ! i] (if (= i num) (return key))))
        (t (over num bag (5nd _)))))

(defun 5new-id (key)
  (when (> 5NEXTBIT 5LIMIT)
    (setq 5PLY (1+ 5PLY)
          5NEXTBIT 1
          5BIT-PATTERN [1 0 ! (cdr 5BIT-PATTERN)])
    (let ((i (* 5LIMIT 5PLY)))
      (adjust-array-size 5FLAG i)
      (adjust-array-size 5IMMEDIATE i)
      (adjust-array-size 5PATTERN i)
      (adjust-array-size 5RELATED i))
    (over i <- 5MAXID by (1- i) until (minusp i)
          (setf (5PATTERN i) (cons 0 (5PATTERN i)))
          (setf (5RELATED i) (cons 0 (5RELATED i)))))
  (setq 5MAXID (1+ 5MAXID))
  (setf (5PATTERN 5MAXID) (append 5BIT-PATTERN nil))
  (setf (5RELATED 5MAXID) [0 !! (cdr 5BIT-PATTERN) ! nil])
  (setq 5NEXTBIT (1+ 5NEXTBIT))
  (setf (car 5BIT-PATTERN) (lsh (car 5BIT-PATTERN) 1))
  (push [key ! 5MAXID] 5IDS)
  5MAXID)

(defun 5make-precede (ida idb)
  (cond ((not (or (= ida idb) (5precedes? ida idb)
                  (member idb (5immediate ida))))
         (setq 5CLOSED nil)
         (push idb (5IMMEDIATE ida)))))

(defun 5precedes? (ida idb)
  (cond ((= ida idb) nil)
        (t (5bit^ (5PATTERN idb) (5RELATED ida)))))

(defun 5bit^ (bitsa bitsb)
  (over bitsa -> a
        over bitsb -> b
        fastor (not (zerop (logand a b)))))

(defun 5close ()
  (cond ((not 5CLOSED)
         (over i <- 5MAXID by (1- i) until (minusp i)
               (setf (5FLAG i) nil))
         (over i <- 5MAXID by (1- i) until (minusp i) (5CLOSE1 i))
         (setq 5CLOSED t))))

(defun 5close1 (i)
  (let ((related (5RELATED i)))
    (when (not (5FLAG i))
      (setf (5FLAG i) t)
      (over (5IMMEDIATE i) -> j
            (5close1 j) -> bits
            (over b <- bits
                  over rel <- related
                  over p <- (5PATTERN j)
                  (rplaca rel (logior (car b) (car rel) (car p))))))
    related))

(defun 5make-multiple-pattern (segs)
  (repeat ((result (copylist (5PATTERN (5order-id (& (car segs)) 'input)))))
    (setq segs (cdr segs))
    (when (null segs) (return result))
    (over r <- result
          over p <- (5PATTERN (5order-id (& (car segs)) 'input))
          (rplaca r (logior (car r) (car p))))))

(defun 5remove-from-pattern (seg pattern)
  (over r <- pattern
        over p <- (5PATTERN (5order-id (& seg) 'input))
        (rplaca r (boole 4 (car r) (car p))))
  pattern)

(defun 5any-follow (seg pattern)
  (5bit^ (5RELATED (5order-id (& seg) 'output)) pattern))

; Cons up Lisp code for an annotated plan.
(defun 5codetop (seg mode)
  (let ((code (5codeup-top seg)))
    (caseq mode
      (define-cliche
       (let* ((cliche-defF (?& (&* seg) cliche-def))
              (comments (append '(nil) (~cliche-def-comments cliche-defF) nil)))
         (remprop comments 'role-tree)
         (remprop comments 'roles)
         (setq code `(define-cliche ,(~cliche-def-name cliche-defF)
                       ,(cdr comments) ,@code))
         (>@seg-code seg [code])))
      (defun
       (let* ((name (?@seg-name seg)))
         (setq code `(defun ,name ,@(cdar code)))
         (>@seg-code seg [code])))
      (T (>@seg-code seg code)))
    (mapc #'5coder-post-phase-hacks (?@seg-code top-seg))
    (5clean-noops seg)))

; Do not let program constant lists get into the database.
(defun 5coder-post-phase-hacks (code)
  (DECLARE (SPECIAL *input-language*))
  (if (and (listp code) (eq (car code) 'defun)) (5fix-depending-on code))
  (caseq *input-language*
    (lisp (5with-open-file-hack code)
          (5let-hack code)
          (5pretty-conds code)
          (5supress-output-roles code))
    (ada (5supress-output-roles code)
         (5no-cond-rets code)
         (cleanup-ada-code code))))

(pp:define-same-as kbe:let global:let)
(pp:define-same-as kbe:let* global:let*)

; Push SETQ into the clauses of a COND return value.
(defun 5no-cond-rets (thing)
  (cond ((and (listp thing) (not (eq (car thing) 'quote)))
         (5ncr-1 thing)
         (do ((it thing (cdr it))) ((or (not (listp it)) (null it)))
           (5no-cond-rets (car it))))))

(defun 5ncr-1 (thing)
  (when (and (eq (car thing) 'setq)
             (eq (length thing) 3)
             (listp (caddr thing))
             (eq (caaddr thing) 'cond))
    (let* ((clauses (cdaddr thing)) (var (cadr thing)))
      (mapc #'(lambda (clause)
                (let ((last (last clause)))
                  (rplaca last (list 'setq var (car last)))))
            clauses)
      (rplacd thing clauses)
      (rplaca thing 'cond))))

(defvar 5list-car nil)
(defun 5supress-output-roles (thing)
  (let ((5list-car nil)) (5supress-output-roles0 thing)))

(defun 5supress-output-roles0 (thing)
  (cond ((and (listp thing) (not (eq (car thing) 'quote)))
         (5sor-1 thing)
         (let ((5list-car (car thing)))
           (do ((it thing (cdr it))) ((or (not (listp it)) (null it)))
             (5supress-output-roles0 (car it)))))))

(defun 5sor-1 (thing)
  (when (and (eq (car thing) 'setq)
             (symbolp (cadr thing)) (listp (caddr thing)))
    (cond ((and (eq (car (caddr thing)) 'kbe-/#-comment-macro)
                (memq 'output (caddr (caddr thing))))
           (rplaca (cddr thing) (caadr (caddr thing))))
          ((and (eq (car (caddr thing)) '5noop)
                (listp (cadr (caddr thing)))
                (eq (car (cadr (caddr thing))) 'kbe-/#-comment-macro)
                (memq 'output (caddr (cadr (caddr thing)))))
           (rplaca (cddr thing) (caadr (cadr (caddr thing))))))
    (when (or (eq (cadr thing) (caddr thing))
              (and (listp (caddr thing))
                   (eq (caaddr thing) '5noop)
                   (eq (cadr thing) (cadr (caddr thing)))))
      (rplaca thing '5noop)
      (rplacd thing nil)))
  (when (and (eq (car thing) 'kbe-/#-comment-macro)
             (memq 'output (caddr thing))
             (or (symbolp (caadr thing))
                 (and (listp (caadr thing))
                      (eq (car (caadr thing)) '5noop)
                      (symbolp (cadr (caadr thing)))))
             (symbolp 5list-car)
             (not (memq 5list-car '(loop progn let)))
             (or (fboundp 5list-car)
                 (string-equal "ADA-FN~" 5list-car
                               0 0 7 (min 7 (string-length 5list-car)))
                 (string-equal "ADA-SELECT~" 5list-car
                               0 0 11 (min 11 (string-length 5list-car)))))
    (rplaca thing '5noop)
    (rplacd thing (ncons (caadr thing))))
  (when (and (eq (car thing) '5noop)
             (listp (cadr thing))
             (eq (car (cadr thing)) 'kbe-/#-comment-macro)
             (memq 'output (caddr (cadr thing)))
             (or (symbolp (caadr (cadr thing)))
                 (and (listp (caadr (cadr thing)))
                      (eq (car (caadr (cadr thing))) '5noop)
                      (symbolp (cadr (caadr (cadr thing))))))
             (symbolp 5list-car)
             (not (memq 5list-car '(loop progn let)))
             (or (fboundp 5list-car)
                 (string-equal "ADA-FN~" 5list-car
                               0 0 7 (min 7 (string-length 5list-car)))))
    (rplaca thing '5noop)
    (rplacd thing (ncons (caadr (cadr thing))))))

; Bring SETQs inside a LET into LET* initializations.
(defun 5let-hack (thing)
  (cond ((and (listp thing) (not (eq (car thing) 'quote)))
         (5lh-1 thing)
         (do ((it thing (cdr it))) ((or (not (listp it)) (null it)))
           (5let-hack (car it))))))

(defun 5lh-1 (thing)
  (when (eq (car thing) 'let)
    (let* ((vars (cadr thing)) (statements (cddr thing))
           pairs extra-statements)
      (do () (NIL)
        (cond ((and (listp (car statements))
                    (eq (caar statements) 'setq)
                    (memq (cadar statements) vars))
               (let ((var (cadar statements)))
                 (push (list var (caddr (pop statements))) pairs)
                 (setq vars (delq var vars))))
              ((and (listp (car statements)) (eq (caar statements) 'format))
               (push (pop statements) extra-statements))
              (T (return))))
      (setq statements (nreconc extra-statements statements))
      (setq pairs (append (mapcar #'ncons vars) pairs))
      (setq pairs (partial-order-sort (sort pairs #'5lh-lessp) #'5lh-order))
      (rplaca thing 'let*)
      (rplaca (cdr thing) pairs)
      (rplacd (cdr thing) statements)))
  thing)

(defun 5lh-lessp (x y)
  (cond ((string-equal (car x) "DATA") nil)
        ((string-equal (car y) "DATA") T)
        (T (alphalessp (car x) (car y)))))

(defun 5lh-order (pair1 pair2)
  (5po1 (cadr pair2) (car pair1)))

(defun 5fix-depending-on (thing)
  (cond ((and (listp thing) (not (eq (car thing) 'quote)))
         (do ((it thing (cdr it))) ((or (not (listp it)) (null it)))
           (5fd-1 it)
           (5fix-depending-on (car it))))))

(defun 5fd-1 (list)
  (when (and (listp (car list)) (eq (caar list) 'depending-on))
    (let ((first-arg (cadr (car list))))
      (cond ((or (symbolp first-arg)
                 (and (listp first-arg)
                      (eq (car first-arg) '5noop)
                      (symbolp (cadr first-arg))))
             (rplaca list (caddr (car list))))
            (T (warn "cannot handed depending-on a non variable yet"))))))

(defun 5pretty-conds (thing)
  (cond ((and (listp thing) (not (eq (car thing) 'quote)))
         (5pc-1 thing)
         (do ((it thing (cdr it))) ((or (not (listp it)) (null it)))
           (5pretty-conds (car it))))))

(defun 5pc-1 (thing)
  (when (eq (car thing) 'cond)
    (let* ((clauses (cdr thing))
           (first-clause (car clauses)) (second-clause (cadr clauses)))
      (cond ((and (null (cdr clauses))
                  (cdr first-clause) (null (cddr first-clause)))
             (rplaca thing 'if)
             (rplacd thing first-clause))
            ((and (null (cdr clauses)) (cddr first-clause))
             (rplaca thing 'when)
             (rplacd thing first-clause))
            ((and (null (cddr clauses))
                  (cdr first-clause) (null (cddr first-clause))
                  second-clause (eq (car second-clause) 'T)
                  (cdr second-clause) (null (cddr second-clause)))
             (rplaca thing 'if)
             (rplacd thing (nconc first-clause (cdr second-clause))))))))

(defun 5with-open-file-hack (code)
  (cond ((and (listp code) (memq (car code) '(defun define-cliche)))
         (let* (((nil nil nil . body) code)
                (let? (and (null (cdr body)) (listp (car body))
                           (eq (caar body) 'let)))
                (let-vars (if let? (cadar body)))
                (file-tripples (5find-io-L body)))
           (cond (file-tripples
                  (letS ((var (car (Elist file-tripples))))
                    (setq let-vars (delq var let-vars)))
                  (cond (let?
                         (rplaca (cdar body) let-vars)
                         (cond ((null let-vars)
                                (rplaca body (car (cddar body)))
                                (rplacd body (cdr (cddar body)))))))
                  (let (((first . rest) body))
                    (rplaca body (car (5make-with-open-file-forms
                                        file-tripples (cons first rest))))
                    (rplacd body nil)))))))
  code)

(defun 5make-with-open-file-forms (file-tripples body)
  (if file-tripples
      `((with-open-file ,(car file-tripples)
          ,. (5make-with-open-file-forms (cdr file-tripples) body)))
      body))

(declare (special 5opens-L))
(defun 5find-io-L (thing)
  (let (5opens-L)
    (5fio-L1 thing)
    (sort 5opens-L #'5cio-order-L)))

(defun 5fio-L1 (thing)
  (cond ((and (listp thing) (not (eq (car thing) 'quote)))
         (5fio-L thing)
         (do ((it thing (cdr it))) ((or (not (listp it)) (null it)))
          L (5fio-L1 (car it))
            (cond ((and (listp (car it)) (eq (caar it) '5kill-this))
                   (rplaca it (cadr it)) ;must not be last!
                   (rplacd it (cddr it))
                   (go L)))))))

(defun 5fio-L (list)
  (when (and (eq (car list) 'setq) (listp (caddr list))
             (symbolp (caaddr list))
             (equal (string (caaddr list)) "WITH-OPEN-FILE-OPEN"))
    (rplaca list '5kill-this)
    (push (list (cadr list) (cadr (caddr list)) (caddr (caddr list))) 5opens-L)))

(defun 5cio-order-L (io1 io2)
  (let* (((var1 file1 code1) io1) ((var2 file2 code2) io2))
    (cond ((equal code1 code2) (alphalessp file1 file2))
          (T (memq (cadr code2) (cdr (memq (cadr code1) '(:in :out))))))))

; Remove noops and update the @seg assertions to reflect the changes.
(declare (special 5NOOP-TRAN ALL-CODE))
(def-alistq 5NOOP-TRAN)
(defun 5clean-noops (top-seg)
  (let* (5NOOP-TRAN ALL-CODE (code (?@seg-code top-seg)))
    (5clean-noops1 code nil nil)
    (over (all-subsegs top-seg) -> seg
          (car (??& (&* seg) @seg)) -> @seg
          (and @seg (~code @seg)) -> seg-code
          (over seg-code -> c
                (or (5NOOP-TRAN c) (car (memq c ALL-CODE))) -> new-c
                select new-c list new-c) -> new-seg-code
          select (not (qset= seg-code new-seg-code))
          (assert-fact (&~code (make-fact-be-out @seg) new-seg-code)))))

(defun 5clean-noops1 (code prior-code parent-code)
  (push code ALL-CODE)
  (cond ((and code (listp code))
         (cond ((and (listp (car code)) (eq (caar code) '5noop))
                (cond ((null (cdar code))
                       (cond (prior-code (rplacd prior-code (cdr code)))
                             (parent-code (rplaca parent-code (cdr code))))
                       (setq code (cdr code)))
                      (t (setf (5NOOP-TRAN (car code)) ['*code-is-car* ! code])
                         (rplaca code (cadar code))))
                (5clean-noops1 code prior-code parent-code))
               (T (5clean-noops1 (car code) nil code)
                  (if (cdr code) (5clean-noops1 (cdr code) code nil)))))))

(defun 5codeup-top (seg)
  (let ((local-vars
          (sort (qset- VAR-NAMES-USED
                       (over (??& (&* (&* seg) port) @io)
                             select (memq (~kind _) '(lambda-arg free-var))
                             bag (~name _)))
                #'alphalessp)))
    (cond ((??& (&* seg) @seg 'lambda-exp) (5codelambda seg local-vars))
          (T (5codeopencode seg local-vars)))))

(defun 5codeopencode (seg local-vars)
  (let ((final-code (5codeup seg T)))
    (if local-vars (setq final-code `((let ,local-vars ,@final-code))))
    (>@seg-code seg final-code)
    final-code))

(defun 5codelambda (seg local-vars)
  (let* ((arglist (over (sort (??& (&* (&* seg) input) @io 'lambda-arg)
                              #'(lambda (x y) (< (~argno x) (~argno y)))) -> lin
                       list (~name lin)))
         (code (5codeupinner seg)))
    (if local-vars (setq code `((let ,local-vars ,@code))))
    (setq code [['lambda arglist ! code]])
    (>@seg-code seg code)
    code))

(defun 5codeup (seg &optional (top? nil))
  (let* (final-code)
    (setf (CODE-FOR seg)
          (append (if (not top?) (5assign (& seg) 'outside-input))
                  (5codeupinner seg)))
    (setq final-code (5assign (& seg) 'outside-output))
    ; Side effects force the above line to be separate (see 5REP).
    (setq final-code
          (cond ((car (??& (&* (&* seg) output) @io 'returnval))
                 (5make-prog2 (CODE-FOR seg) final-code))
                (t (append (CODE-FOR seg) final-code))))
    (setf (CODE-FOR seg) final-code)
    final-code))

(defun 5codeupinner (seg)
  (let* ((name (?@seg-name seg)) code)
    (caseq (?@seg-kind seg)
      ((funcall)
       (setq code
             [[(or name '...)
               !(over (sort (??& (&* (&* seg) port) @io 'actual-arg)
                             #'(lambda (x y) (< (~argno x) (~argno y)))) -> lin
                      append (cond ((input-fact? (~deep lin))
                                    [!(if (and (null name)
                                               (??& (&* (&* seg) output 'id *
                                                        (~obj (~deep lin)))
                                                    @io 'side-effect))
                                          ['&in-out])
                                     (5rep1 (5source lin))])
                                   (t [!(if (null name) ['&out]) (~name lin)])))]] )
       (cond ((or (?empty-slot seg) (??& * slot (&* seg) * 'output))
              (setq code (make-kbe-/#-comment code seg))))
       (>@seg-code seg code)
       code)
      ((constant)
       (setq code (cond ((or (memq (cadr name) '(t nil))
                             (numberp (cadr name)) (stringp (cadr name)))
                         [['5noop (copytree (cadr name))]])
                        (t [(copytree name)])))
       (>@seg-code seg code)
       code)
      ((opencode lambda-exp)
       (caseq (?plantype seg)
         (xor [!!(5assign (& seg) 'inside-input) !!(5codexor seg)])
         (ssr (5codessr (5assign (& seg) 'inside-input) seg nil))
         ((nil expr and comp)
          [!!(5assign (& seg) 'inside-input) !!(5codesimple seg)])
         (pred [!!(5assign (& seg) 'inside-input) !!(5codepred seg nil)])
         (t (barf "5CODEUPINNER: Unexpected plantype.~% Seg: ~:=" seg))))
      (t (barf "5CODEUPINNER: Unexpected @seg-kind.~% Seg: ~:=" seg)))))

(defun make-kbe-/#-comment (code seg)
  (let* ((name (external-slot-name seg)))
    (cond ((equal code '((...))) (setq code nil)))
    `((kbe-/#-comment-macro ,code ,name))))

(defun external-slot-name (seg)
  (let* ((slot-fact (~01 (??& * slot (&* seg))))
         (slot-name (~slotname slot-fact))
         (name (make-external-name-list slot-name))
         (cliche-fact (if slot-fact
                          (~01 (??& (~segcase slot-fact) cliche)))))
    (if (eq (~clichename cliche-fact) 'pseudo-subcliche)
        (setq name `(,@name of the ,@(external-slot-name
                                      (~seg~segcase cliche-fact)))))
    (if (memq (~slottype slot-fact) '(input output))
        (push (~slottype slot-fact) name))
    (cons 'the name)))

(defun make-external-name-list (name-symbol)
  (let ((first-blank (string-search-char #\space name-symbol)))
    (if (null first-blank) (ncons name-symbol)
        (let* ((name-string (string-append (string name-symbol) " ")))
          (letS* ((blank-spot
                    (enumerateS #'null
                                #'(lambda (x)
                                    (string-search-char #\space name-string (1+ x)))
                                first-blank)))
            (Rlist (intern (string-upcase
                             (substring name-string
                                        (1+ (Pvalue blank-spot -1)) blank-spot))
                           (symbol-package name-symbol))))))))

(defvar *nest-loop-epilog* nil
  "controls inclusion of loop epilog code in the terminating cond")

(defun 5codesimple (seg)
  (prog (returnval@io subsegs subseg c code action-code exit-code plantype)
    (setq plantype (?plantype seg))
    (setq returnval@io (car (??& (&* (&* seg) output) @io 'returnval))
          subsegs (sort (~nset ~subseg (??& (&* seg) subseg))
                        #'(lambda (x y)
                            (5precedes? (5order-id x 'output) (5order-id y 'input)))))
 lp (if (null subsegs) (return (5simple-stuff-at-end seg code)))
    (setq subseg (~seg (car subsegs)) subsegs (cdr subsegs))
    (cond ((and *nest-loop-epilog*
                (not (eq plantype 'and)) (eq (?plantype subseg) 'ssr)
                (5put-here subseg)
                (over subsegs fastand (not (eq (?plantype (~seg _)) 'ssr))))
           (setq action-code (over subsegs -> segcase
                                   (~seg segcase) -> s
                                   (5codeup s) -> c
                                   select (5put-here s) append c))
           (cond (returnval@io
                  (setq exit-code
                        (5make-prog2 (5rep (5source returnval@io))
                                     (5assign (& seg) 'inside-output))))
                 (t (setq action-code
                          (append action-code (5assign (& seg) 'inside-output)))))
           (return [!! code
                    !!(5codessr nil subseg [[nil action-code ! exit-code]])]))
          (t (setq c (5codeup subseg))
             (if (5put-here subseg) (setq code (append code c)))
             (go lp)))))

(defun 5simple-stuff-at-end (seg inside-code)
  (let* ((returnval@io (car (??& (&* (&* seg) output) @io 'returnval)))
         (ret-code (if returnval@io (5rep (5source returnval@io)))))
    (>@seg-code seg [!! inside-code !! ret-code])
    [!! inside-code !! (5make-prog2 ret-code (5assign (& seg) 'inside-output))]))

; CASE-ACTION entries are [case action-code ! ret-code].
(defun 5codexor (seg)
  (let* ((roles (??roles seg))
         (join (car (roles 'join))) (mpred (car (roles 'pred)))
         (mpred-segcases (~nset ~from (??& (&* seg) cflow (&* mpred))))
         (initcode (over (roles 'init) -> i
                         (5codeup i) -> c
                         select (5put-here i) append c))
         (case-action
           (over mpred-segcases -> mpsc
                 (~case mpsc) -> case
                 (~to (?& (&* seg) cflow mpsc)) -> actioncase
                 (cond ((memq (~seg actioncase) (roles 'join))
                        (prog1 actioncase (setq actioncase nil)))
                       (t (~to (?& (&* seg) cflow actioncase)))) -> joincase
                 (if actioncase (5codeup (~seg actioncase))) -> actioncode
                 (5assign joincase 'outside-input) -> jcode
                 (car (??& (&* joincase input) @io 'returnval)) -> ret
                 (if ret (5rep (5source ret))) -> retcode
                 bag (if (not (and actioncode (5put-here (~seg actioncase))))
                         (setq actioncode nil))
                     (cond (retcode (setq retcode (5make-prog2 retcode jcode)))
                           (t (setq actioncode [!! actioncode !! jcode])))
                     [case actioncode ! retcode]))
         (basic-code [!! initcode !! (5codepred mpred case-action)]))
    (setf (CODE-FOR join) basic-code)
    (5simple-stuff-at-end seg (if (5put-here join) basic-code))))

(declare (special join-info))
(defun 5codepred (pred case-action)
  (let* (code then thencase else elsecase)
    (cond ((eq (?@seg-kind pred) 'funcall)
           (desetq ((thencase . then) (elsecase . else))
                   (5case-action-sort case-action
                                      (5need-not (& pred (caar case-action)))))
           (setq then [!!(car then) !!(cdr then)]
                 else [!!(car else) !!(cdr else)])
           (setq code (car (5codeupinner pred)))
           (if (5need-not (& pred thencase)) (setq code ['not code]))
           (setq code
                 [['cond [code ! then]
                   !(cond ((5null-computation else) nil)
                          ((and (listp (car else)) (eq (caar else) 'cond)
                                (null (cdr else))
                                (over (and (boundp 'join-info) join-info)
                                      fastand (not (equal else (cdr _)))))
                           (cdar else))
                          (t [[t ! else]]))]]))
          ((eq (?plantype pred) 'ssr)
           (setq code (5codessr (5assign (& pred) 'outside-input)
                                pred case-action)))
          (t (let* ((roles (??roles pred))
                    (icode (over (roles 'init) -> iseg
                                 (5codeup iseg) -> c
                                 select (5put-here iseg) append c))
                    (first-predcase (~to (?& (&* pred) cflow (&* pred))))
                    (join-info (over (roles 'join) bag [_])))
               (setq code (5codepred2 pred first-predcase case-action))
               (if join-info (setq code (5consolodate-cond code)))
               (setq code (append icode code)))))
    code))

(defun 5null-computation (stuff)
  (member stuff '(nil (nil) ((5noop nil)) ('nil) ('(5noop nil)))))

(defun 5need-not (segcase)
  (over (or (~nset ~wff (??& segcase when))
            (~nset ~wff (??& segcase assert))) -> wff
        fastor (and (not (atom wff)) (eq (car wff) 'not))))

(defun 5case-action-sort (case-action negativethen)
  (let* ((((thencase . then) (elsecase . else)) case-action))
    (setq then (append (car then) (cdr then))
          else (append (car else) (cdr else)))
    (let* ((condelse (and (listp (car else)) (eq (caar else) 'cond)
                          (null (cdr else))
                          (over (and (boundp 'join-info) join-info)
                                fastand (not (equal else (cdr _))))))
           (condthen (and (listp (car then)) (eq (caar then) 'cond)
                          (null (cdr then))
                          (over (and (boundp 'join-info) join-info)
                                fastand (not (equal then (cdr _))))))
           (emptyelse (or (null else) (and (null (cdr else)) (null (car else)))))
           (emptythen (or (null then) (and (null (cdr then)) (null (car then))))))
      (cond ((cond (condelse (and condthen negativethen))
                   (emptyelse (or condthen (and emptythen negativethen)))
                   (t (or condthen emptythen negativethen)))
             (reverse case-action))
            (t case-action)))))

(defun 5codepred2 (pred subpredcase case-action)
  (let* ((subpred (~seg subpredcase)) (subcase (~case subpredcase)))
    (cond ((eq pred subpred)
           [!!(car (case-action subcase)) !!(cdr (case-action subcase))])
          ((assq subpred join-info)
           (or (cdr (assq subpred join-info))
               (cdr (rplacd (assq subpred join-info)
                            (5codepred2 pred
                                        (~to (?& (&* pred) cflow (&* subpred)))
                                        case-action)))))
          (t (5codepred subpred
                        (over (??& (&* pred) cflow (&* subpred)) -> cf
                              revlist [(~case (~from cf)) nil
                                       !(5codepred2 pred (~to cf) case-action)]))))))

; Combine identical COND actions, introducing ANDs and ORs.
(defun 5consolodate-cond (code)
  (let (cond-list)
    (setq cond-list (over (cdar code) list (cons (car _) (cdr _))))
    (repeat ((cl cond-list) head-pred head-action not-head-pred)
      (desetq (head-pred . head-action) (car cl))
      (when (null (cdr cl))
        (when (and (eq head-pred T) (caar head-action)
                   (eq (caar head-action) 'cond))
          (setf (car cl) [(caadar head-action) ! (cdadar head-action)])
          (setf (cdr cl) (over (cddar head-action)
                              list (cons (car _) (cdr _)))))
        (return))
      (cond ((or (over (cdr cl) fastor (equal head-action (cdr _)))
                 (and (null (cddr cl)) (5null-computation head-action)))
             (dolist (item (cdr cl))
               (let (((pred . action) item))
                 (cond ((equal action head-action)
                        (cond ((eq pred T))
                              ((and (not (atom head-pred))
                                    (eq (car head-pred) 'or))
                               (rplaca item ['or !!(cdr head-pred) pred]))
                              (t (rplaca item ['or head-pred pred])))
                        (return))
                       (t (setq not-head-pred
                                (cond ((and (not (atom head-pred))
                                            (eq (car head-pred) 'not))
                                       (cadr head-pred))
                                      (t ['not head-pred])))
                          (setq head-pred (copytree head-pred))
                          (cond ((and (not (atom pred)) (eq (car pred) 'and))
                                 (rplaca item ['and not-head-pred ! (cdr pred)]))
                                (t (rplaca item ['and not-head-pred pred])))))))
             (rplaca cl (cadr cl))
             (rplacd cl (cddr cl)))
            (t (setq cl (cdr cl)))))
    [['cond ! cond-list]]))

(defun 5codessr-prog (init body)
  `((prog nil ,@init LP ,@body ,(copylist '(go LP)))))

(defun 5codessr-loop (init body)
  `(,@init (loop do ,@body)))

(define-KBE-option *loop-code-template* "Iteration style"
  '5CODESSR-LOOP :assoc '(("DO-Loop" . 5CODESSR-LOOP)
                         ("GO-Tags" . 5CODESSR-PROG)))

(defun 5codessr (precomputation ssr case-action)
  (let* ((roles (??roles ssr))
         (dfjoin (car (roles 'dfjoin)))
         (body (car (roles 'body))) (rec (car (roles 'rec)))
         (df-retval@io (car (??& (&* (& dfjoin 'body) input) @io 'returnval)))
         (icode (append precomputation
                        (over (roles 'init) -> i
                              (5codeup i) -> c
                              select (5put-here i) append c)
                        (5assign (& body) 'outside-input)))
         code found-exit)
    (when (and df-retval@io case-action)
      (barf "5CODESSR: returnval and actions.~% ssr: ~S" ssr))
    (repeat ((subseg (~seg (~to (?& (&* body) cflow (&* body)))))
             incase outcase xcase action-code ret-code newsubseg)
      (cond ((eq subseg rec)
             (setq code (append code (5assign (& rec) 'outside-input)))
             (return))
            ((not (eq (?segtype subseg) 'split))
             (let* ((c (5codeup subseg)))
               (if (5put-here subseg) (setq code (append code c)))
               (setq newsubseg (~seg (~to (?& (&* body) cflow (&* subseg)))))))
            (t
             (dolist (cf (??& (&* body) cflow (&* subseg)))
               (cond ((eq (?segtype (~seg (~to cf))) 'join)
                      (setq outcase (~case (~from cf))
                            xcase
                            (~case
                              (~to (?& (&* ssr) cflow
                                       (prog (next)
                                         (setq next (~to cf))
                                        L(setq next
                                               (~to (?& (&* body) cflow
                                                        (& (~seg next)))))
                                         (if (eq (~seg next) body) (return next))
                                         (go L)))))))
                     (t (setq incase (~case (~from cf))
                              newsubseg (~seg (~to cf))))))
             (setq found-exit T
                   ret-code
                   (reverse (or (cdr (case-action xcase))
                                (and df-retval@io (5rep (5source df-retval@io)))))
                   action-code
                   [!!(5assign (& dfjoin 'body) 'outside-input)
                    !!(car (case-action xcase)) !!(reverse (cdr ret-code))]
                   ret-code [['return !(if ret-code [(car ret-code)])]])
             (setq code
                   (append code (5codepred subseg
                                          [[incase]
                                           [outcase action-code ! ret-code]])))))
      (setq subseg newsubseg))
    (setq code (funcall *loop-code-template* icode code))
    (if (and (null found-exit) df-retval@io)
        (setq code (append code (5rep (5source df-retval@io)))))
    (setf (CODE-FOR body) code)
    (5simple-stuff-at-end ssr (if (5put-here body) code))))

(defun 5source (@port)
  (let ((sourceio (~01 ~from (??& * dflow * (~deep @port)))))
    (cond ((null sourceio) (& (~deep @port) @io 'missing nil nil))
          ((~01 (??& sourceio @createvar)))
          (T (?& sourceio @io)))))

(defun 5rep (@port)
  (cond ((@createvar-fact? @port) [(~varname @port)])
        ((eq (~kind @port) 'returnval)
         (let ((code (CODE-FOR (~seg~segcase (~deep @port)))))
           (when (null code)
             (barf "5REP: returnval has no code.~% Data: ~:=" @port))
           (setf (CODE-FOR (~seg~segcase (~deep @port))) nil)
           code))
        (t (if (~name @port) [(~name @port)]))))

(defun 5rep1 (@port) (5make1 (or (5rep @port) (list '.V.))))

(defun 5assign (segcase type)
  (caseq type
    (outside-input
     (5assign1 (over (??& (&* segcase input) @io 'free-var)
                    nconc (5assign-pair (~name _) (5source _)))))
    (inside-input
     (5assign1 (over (??& (&* segcase input) @createvar)
                    nconc (5assign-pair (~varname _) (?& (~deep _) @io)))))
    (inside-output
     (5assign1 (over (??& (&* segcase output) @io 'free-var)
                    nconc (5assign-pair (~name _) (5source _)))))
    (outside-output
     (let ((pairs (over (??& segcase output) -> out
                        (~01 (??& out @createvar)) -> @createvar
                        nconc (cond (@createvar
                                     (5assign-pair (~varname @createvar)
                                                   (?& (~deep @createvar) @io)))))))
       (5assign1 pairs)))))

(defun 5assign-pair (p-name s)
  (cond ((fact-key-dispatch s
           (@io (or (eq (~kind s) 'returnval)
                    (and (~name s) (not (eq p-name (~name s))))))
           (@createvar (not (eq p-name (~varname s)))))
         [[p-name ! s]])))

(defun 5assign1 (pairs)
  (setq pairs
        (over pairs -> [n ! s]
              (5rep1 s) -> rep
              select (and (not (eq rep n)) (not (equal rep `(5noop ,n))))
              list [n rep]))
  (cond ((null pairs) nil)
        ((null (cdr pairs)) [['setq ! (car pairs)]])
        (t (over (partial-order-sort (sortcar pairs #'alphalessp) #'5psetq-order)
                 list (cons 'setq _)))))

(defun 5psetq-order (pair1 pair2)
  (5po1 (cadr pair1) (car pair2)))

(defun 5po1 (list var)
  (do ((from list (cdr from))) ((null from) nil)
    (cond ((eq var from) (return T))
          ((not (listp from)) (return nil))
          ((5po1 (car from) var) (return T)))))

(defun partial-order-sort (list predicate)
  (let* ((work-list (cons nil list)) (len (length work-list)))
    (prog ()
     L (do ((i 2 (1+ i))) ((>= i len) (return))
         (let ((item (nth i work-list)))
           (do ((j 1 (1+ j))) ((>= j i) (return))
             (when (funcall predicate item (nth j work-list))
               (pop (nthcdr i work-list))
               (push item (nthcdr j work-list))
               (go L))))))
    (cdr work-list)))

(defun 5progn-trans (arg)
  (cond ((and (not (atom arg)) (eq (car arg) 'progn)) (cdr arg))
        (t [arg])))

(defun 5make1 (code)
  (cond ((cdr code) ['progn ! code]) (t (car code))))

(defun 5make-prog2 (return-code final-code)
  (cond ((null final-code) return-code)
        ((null return-code) final-code)
        ((5null-computation return-code) [!! final-code nil])
        (t (if (cddr return-code) (setq return-code [(5make1 return-code)]))
           [[(cond ((cdr return-code) 'prog2) (t 'prog1))
             !! return-code ! final-code]])))

(defun 5put-here (seg)
  (let* ((ret (~01 ~deep (??& (&* (&* seg) output) @io 'returnval))))
    (or (null ret)
        (??& ret @createvar)
        (caseq (5region-classify ret)
          (ignore T) (head (not (assoc ret regions)))))))

(defun 5get-lisp-name (oldname)
  (let* ((new
           (assq oldname
                 '((_lt . minusp) (_eq . zerop) (_gt . plusp)
                   (_+i . +) (_-i . -) (_*i . *) (_//i . //) (_**i . ^) (_u-i . -)
                   (_+r . +/$) (_-r . -/$) (_*r . */$) (_//r . ///$)
                   (_**r . expt) (_**ri . ^/$) (_u-r . -/$)
                   (_+d . +/$) (_-d . -/$) (_*d . */$) (_//d . ///$)
                   (_**d . expt) (_**di . ^/$) (_u-d . -/$)
                   (_itor . float) (_itod . float) (_rtoi . fix) (_dtoi . fix)
                   (_rtod . 5noop) (_dtor . 5noop)
                   (_exp . exp) (_dexp . exp) (_alog . log) (_dlog . log)
                   (_sin . sin) (_dsin . sin) (_cos . cos) (_dcos . cos)
                   (_sqrt . sqrt) (_dsqrt . sqrt)
                   (_iabs . abs) (_abs . abs) (_dabs . abs)
                   (_float . float) (_dfloat . float) (_sngl . 5noop)
                   (_dble . 5noop) (non-null . 5noop) (5variable . 5noop)
                   (and-lexpr . and) (or-lexpr . or)))))
    (cond (new (cdr new)) (t oldname))))

(defun 5get-lisp-constant (oldconstant)
  (cond ((atom oldconstant)
         (barf "5GET-LISP-CONSTANT: doubtful constant.~% Data: ~S" oldconstant))
        ((eq (car oldconstant) 'quote) oldconstant)
        ((memq (car oldconstant) '(_i _r _d)) `',(cadr oldconstant))
        (t (barf "5GET-LISP-CONSTANT: doubtful constant.~% Data: ~S" oldconstant))))

; Restore annotations to the original temporal context.
(def-alistq loop-info)
(defun 5fix-@assertions (top-seg ioc original-context)
  (let* ((loop-info (over (all-subsegs top-seg) -> seg
                         bag [seg (?& (&* seg) @seg)
                              !!(??& (&* (&* seg) port) @io)
                              !!(??& (&* (&* seg) port) @createvar)])))
    (set-db-context original-context)
    (over (reverse (all-subsegs top-seg)) -> seg
          (cdr (assq seg loop-info)) -> [@seg ! @port-facts]
          (--& (&* seg) @seg)
          (--& (&* (&* seg) port) @io)
          (--& (&* (&* seg) port) @createvar)
          (cond (@seg
                 (make-fact-be-in @seg)
                 (over @port-facts
                       select (fact-is-in? (~deep _))
                       (make-fact-be-in _))
                 (if (null (?@seg-code seg))
                     (>@seg-code seg (5estimate-code seg)))
                 (over [!(??& (&* seg) input) !(??& (&* seg) output)]
                       (if (null (??& _ @io))
                           (if (null (??& _ @io))
                               (>& _ @io 'free-var nil nil))))
                 (over [!(??& (&* seg) tinput) !(??& (&* seg) toutput)]
                       (if (null (??& _ @io)) (>& _ @io 'free-var nil nil))))
                (T (5estimate-@info seg loop-info ioc))))
    (restore-db-context original-context)))

(defun 5estimate-code (seg)
  (let ((inner-code (over (??subseg seg) append (?@seg-code _))))
    (over inner-code -> sexpr
          select (and (not (atom sexpr))
                      (over inner-code
                            fastand (not (5sexpr-in (5root-sexpr sexpr)
                                                   (5root-sexpr _)))))
          bag sexpr)))

(defun 5root-sexpr (list)
  (cond ((and (listp list) (memq (car list) '(*code-is-car* *code-is-cdr*)))
         (5root-sexpr (cdr list)))
        (t list)))

(defun 5sexpr-in (sexpr list)
  (and list (listp list)
       (or (eq sexpr (car list)) (5sexpr-in sexpr (car list))
           (eq sexpr (cdr list)) (5sexpr-in sexpr (cdr list)))))

(defun 5estimate-/@info (seg loop-info ioc)
  (let* ((role-facts (??& * role (&* seg))))
    (caseq (~01 ~rolename role-facts)
      ((bssr gen acc trn term filter)
       (>& (& seg) @seg 'opencode nil
           (cond ((?role seg 'op) (?@seg-code (?role seg 'op)))
                 ((??& (&* seg) plantype 'tempcomp) (5estimate-code seg)))))
      (body
       (>& (& seg) @seg 'opencode nil
           (let ((op (?role (~seg~segcase (car role-facts)) 'op)))
             (if op (?@seg-code op)))))
      (dfjoin
       (let* ((ssr (~seg~segcase (car role-facts)))
              (supseg (?supseg ssr))
              (code (or (?@seg-code ssr)
                        (and supseg (eq (?plantype supseg) 'tempcomp)
                             (?@seg-code supseg)))))
         (>& (& seg) @seg 'join nil
             `((*CODE-IS-CAR* . ,(memq 'LP (car code)))))))
      (rec
       (let* ((ssr (~seg~segcase (car role-facts)))
              (ssr-code (car (?@seg-code ssr)))
              (supseg (?supseg ssr))
              (tc-code (and supseg (eq (?plantype supseg) 'tempcomp)
                            (car (?@seg-code supseg))))
              (loop (cond ((and (listp tc-code) (eq (car tc-code) 'PROG)) tc-code)
                          ((and (listp ssr-code) (eq (car ssr-code) 'PROG)) ssr-code))))
         (>& (& seg) @seg 'recursion nil [(car (last loop))])))
      (nil
       (when (and (eq (?segtype seg) 'join)
                  (eq (?plantype (?supseg seg)) 'ssr-body))
         (>& (& seg) @seg 'join nil nil)))
      (T (>& (& seg) @seg 'opencode nil (5estimate-code seg))))
    (over [!(??& (&* seg) input) !(??& (&* seg) output)
           !(??& (&* seg) tinput) !(??& (&* seg) toutput)] -> port
          (cdr (assq port ioc)) -> loop-port
          (if loop-port (cddr (assq (~seg~segcase loop-port) loop-info))) -> loop@s
          (over loop@s
                (if (and (eq (~deep _) loop-port) (@io-fact? _))
                    (return _))) -> loop@io
          (over loop@s
                (if (and (eq (~deep _) loop-port) (@createvar-fact? _))
                    (return _))) -> loop@createvar
          (fact-key-dispatch port
            ((input output)
             (cond ((null loop@io) (>& port @io 'free-var nil nil))
                   (T (assert-fact (&~deep loop@io port))))
             (if loop@createvar (assert-fact (&~deep loop@createvar port))))
            ((tinput toutput)
             (>& port @io 'free-var
                 (or (and loop@createvar (~varname loop@createvar))
                     (and loop@io (~name loop@io))
                     (and (~lower-io port)
                          (~01 ~name (??& (~lower-io port) @io)))
                     (and (null (~lower-io port))
                          (null (??& (&* seg) toutput * * port)) 'dummy))
                 nil))))))

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp REPEAT Indent:1;
;;; Lisp REPEAT* Indent:1;
;;; End:;
