;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_2.pdf
;;; PDF pages 43-61; listing dflow.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.



;;; Compute data flow from df-nums, treating the surface plan as straight-line
;;; sections between joins, splits, and the superseg. Hunks record section
;;; endpoints, graph links, written/live sets, parts, joins, and sources.
;;; Inputs to the superseg act as sources at the start and sinks at the end,
;;; allowing side-effects to be detected and represented by extra outputs.

;;; PDF page 44
(defstruct (dflow-hunk (:type :named-list))
  start end flag prev next written live part join source unknown)
(defmacro 6make-dflow-hunk (start)
  `(make-dflow-hunk start ,start end nil flag nil prev nil next nil
                    written nil live nil part nil join nil source nil unknown nil))
(pp:define-list-print-function dflow-hunk (hunk s)
  (format s "dflowH-~=" (start hunk)))
(defun 6h (hunk) ;hunk or list of hunks
  (cond ((listp hunk) (pp:pp (over hunk bag (6h _))))
        (t [[(start hunk) (end hunk) (flag hunk)]
            [(prev hunk) (next hunk)]
            [(6d (written hunk)) (6d (live hunk)) (6a (part hunk))]
            [(over (join hunk) bag (car (6d _)))
             (over (source hunk) bag [(car (6d (car _))) ! (cdr _)])
             (over (unknown hunk) bag (car (6d (car _))))]])))
(defun 6a (alist) ;dfnum -> dfset
  (over alist bag [(car (6d (car _))) ! (6d (cdr _))]))

;;; PDF pages 45-46
;;; Liveness uses fixed-point propagation. Parts model propagated side-effects;
;;; joins are selected only where live values have conflicting sources.
(defun 6data-flow (SUPERSEGCASE)
  (let (INFO LAST-HUNK FIRST-HUNK)
    (6construct-df-info SUPERSEGCASE nil)
    (setq FLOWS-HELPS-ANALYZE
          [(~seg SUPERSEGCASE) ! (over INFO bag (start _))])
    (6find-write-estimate-live)
    (6trans-live)
    (6find-parts)
    (6side-effects)
    (6find-joining)
    (over INFO (6do-joining _))
    (6create-dflow)))
(defun 6construct-df-info (segcase prior-hunk)
  (let* ((hunk (over INFO (if (equal segcase (start _)) (return _)))))
    (when (not hunk)
      (setq hunk (6make-dflow-hunk segcase))
      (push hunk INFO)
      (if (equal segcase SUPERSEGCASE) (setq FIRST-HUNK hunk))
      (repeat ()
        (setq segcase (~to (?& SUPERSEGCASE cflow segcase)))
        (when (equal segcase SUPERSEGCASE) (setq LAST-HUNK hunk) (return))
        (caseq (?segtype (~seg segcase))
          (join (6construct-df-info (& (~seg segcase)) hunk) (return))
          (split (over (??& SUPERSEGCASE cflow (&* (~seg segcase)))
                   (6construct-df-info (~from _) hunk)) (return))))
      (setf (end hunk) segcase))
    (cond (prior-hunk (push prior-hunk (prev hunk))
                     (push hunk (next prior-hunk))))))

;;; PDF pages 47-48
;;; Bit sets are lists of fixnums, with flag6 bits per word and zero extension.
;;; Set operations may modify their first argument but do not share the second.
(defun 6set^? (x y)
  (over x -> xe over y -> ye fastor (not (zerop (logand xe ye)))))
(defun 6set^ (x y)
  (cond ((or (null x) (null y)) nil)
        (t (repeat ((s1 x) (send y))
             (setf (car s1) (logand (car s1) (car send)))
             (cond ((or (null (cdr send)) (null (cdr s1)))
                    (setf (cdr s1) nil) (return x)))
             (pop s1) (pop send)))))
(defun 6setU (x y)
  (cond ((null x) (copylist y)) ((null y) x)
        (t (repeat ((s1 x) (send y))
             (setf (car s1) (logior (car s1) (car send)))
             (cond ((null (cdr send)) (return))
                   ((null (cdr s1)) (setf (cdr s1) (copylist (cdr send))) (return)))
             (pop s1) (pop send)) x)))
(defun 6set- (x y)
  (cond ((or (null x) (null y)) x)
        (t (repeat ((s1 x) (send y))
             (if (or (null s1) (null send)) (return x))
             (setf (car s1) (boole 4 (car s1) (car send)))
             (pop s1) (pop send)))))
(eval-when (eval load compile)
  (defprop 6setU (6setU over-acc over-body) u1overstep)
  (defprop 6setU over-acc u1overreturn)
  (defprop 6setU nil u1overinit)
  (defprop qsetU (qsetU over-acc over-body) u1overstep)
  (defprop qsetU over-acc u1overreturn)
  (defprop qsetU nil u1overinit))

;;; PDF page 49
;;; Move a live set backward through a straight-line span; return live/written.
(defun 6find-live1 (fsegcase tsegcase live)
  (setq live (copylist live))
  (repeat ((segcase tsegcase) this-w this-r written)
    (setq this-w (over (??& (&* (~seg segcase)) output) 6setU (6?df _))
          this-r (cond ((??& segcase @seg 'funcall 'k*psetq)
                        (over (??& segcase input) -> in
                              (?& segcase output 'id * (~obj in)) -> out
                          select (6set^? (6?df out) live)
                          6setU (6?df in)))
                       (t (over (??& (&* (~seg segcase)) input) 6setU (6?df _)))))
    (cond ((equal segcase SUPERSEGCASE) (setq live (6setU live this-w)))
          (t (setq live (6set- live this-w))
             (setq written (6setU written this-w))))
    (setq live (6setU live this-r))
    (setq segcase (~from (?& SUPERSEGCASE cflow * segcase)))
    (when (equal segcase fsegcase)
      (when (equal segcase SUPERSEGCASE)
        (setq this-w (over (??& (&* (~seg segcase)) input) 6setU (6?df _)))
        (setq live (6set- live this-w))
        (setq written (6setU written this-w)))
      (return [live ! written]))))

;;; PDF page 50
(defun 6find-write-estimate-live ()
  (repeat ((todo [LAST-HUNK]) hunk live written)
    (setq hunk (when-pop-fails todo (return)))
    (when (null (flag hunk))
      (setf (flag hunk) t)
      (desetq (live . written) (6find-live1 (start hunk) (end hunk) (live hunk)))
      (setf (written hunk) written)
      (over (prev hunk) (setf (live _) (6setU (live _) live)))
      (setq todo (append todo (prev hunk))))))
(defun 6trans-live ()
  (repeat ((to-do INFO) hunk live)
    (setq hunk (when-pop-fails to-do (return)))
    (when (flag hunk)
      (setf (flag hunk) nil)
      (setq live (over (next hunk)
                  6setU (car (6find-live1 (start _) (end _) (live _)))))
      (when (not (equal live (live hunk)))
        (setf (live hunk) live)
        (over (prev hunk) (push _ to-do) (setf (flag _) t))))))

;;; PDF pages 51-52
;;; Estimate transitively closed part relationships in a single pass.
(def-alistq parts)
(defun 6find-parts ()
  (repeat ((to-do [FIRST-HUNK]))
    ;find section with all predecessors done if any
    (repeat (waiting)
      (cond ((null to-do) (setq to-do (nconc to-do waiting)) (return))
            ((flag (top to-do)) (pop to-do))
            ((over (prev (top to-do)) fastand (flag _))
             (setq to-do (nconc to-do waiting)) (return))
            (t (push (pop to-do) waiting))))
    (if (null to-do) (return))
    (let* ((hunk (pop to-do))
           (live (if (prev hunk) (live (car (prev hunk)))))
           (the-parts (over (prev hunk) bag (6find-parts1 (start _) (end _) (part _)))))
      (setf (part hunk)
            (over DF-NUMS -> dfn
                  (if (6set^? dfn live)
                      (over the-parts -> parts 6setU (parts dfn))) -> new-part
              select new-part bag [dfn ! new-part]))
      (setf (flag hunk) t)
      (setq to-do (append (next hunk) to-do))))
  (over INFO (setf (flag _) nil)))
(defun 6find-parts1 (fsegcase tsegcase parts)
  (setq parts (over parts revlist (append _ nil))) ;note must keep dfns eq
  (repeat ((segcase fsegcase) obj-dfn id-dfn)
    (setq segcase (~to (?& SUPERSEGCASE cflow segcase)))
    (dolist (out (??& (&* (~seg segcase)) output))
      (when (memq (~kind out) '(id part))
        (setq obj-dfn (6?df out)
              id-dfn (6?df (?& (&* (~seg segcase)) input * (~id out))))
        (setf (parts obj-dfn)
              (6setU (or (parts obj-dfn) (append obj-dfn nil))
                     (or (parts id-dfn) (append id-dfn nil))))
        (if (eq (~kind out) 'id) (setf (parts id-dfn) (parts obj-dfn)))))
    (if (equal segcase tsegcase) (return parts))))

;;; PDF page 53
;;; Add extra ports required by side-effects.
(defun 6side-effects ()
  (let ((subsegs (??subseg (~seg SUPERSEGCASE))))
    (over subsegs -> subseg
          (??& (&* (&* subseg) output) @io 'side-effect) -> se@ios
      (if se@ios
          (let* ((hunk
                   (repeat ((segcase (& subseg)))
                     (if (or (equal segcase SUPERSEGCASE)
                             (not (eq (?segtype (~seg segcase)) 'simple)))
                         (return (over INFO
                                   (if (equal segcase (start _)) (return _)))))
                     (setq segcase (~from (?& SUPERSEGCASE cflow * segcase)))))
                 (live (car (6find-live1 (& subseg) (end hunk) (live hunk))))
                 (parts (6find-parts1 (start hunk) (& subseg) (part hunk))))
            (over se@ios -> se@io
                  (over (??& (&* subseg) output) 6setU (6?df _)) -> out-dfnums
                  (parts (6?df (~deep se@io))) -> part-dfnums
              (over DF-NUMS -> dfnum
                select (and (6set^? dfnum part-dfnums) (6set^? dfnum live)
                            (not (6set^? dfnum out-dfnums)))
                (cond ((and (??& * recursive (&* subseg))
                            (not (6set^? (6?df (~deep se@io)) live)))
                       ;this is a kludge so recursive side-effecting things work
                       (6+df (~deep se@io) dfnum))
                      (t (let* ((name (or (DF-NAME dfnum)
                                         (barf "6SIDE-EFFECT: ~
                                                Side-effecting a non-~
                                                variable.~
                                                ~% Subseg: ~S~
                                                ~% DFnum:  ~S" subseg dfnum)))
                                (new-in (+& (& subseg) input nil (genobj subseg name) nil))
                                (new-out (+& (& subseg) output 'id
                                             (genobj subseg name) (~obj new-in))))
                           (6+df new-in dfnum) (6+df new-out dfnum)
                           (setf (written hunk) (6setU (written hunk) dfnum))
                           (+& new-in @io 'free-var name nil)
                           (+& new-out @io 'side-effect name nil)))))))))))

;;; PDF pages 54-57
;;; Join only live dfnums written more than once. Sources are definite,
;;; deferred, provisional, or maybe-join lists, shared between sections.
;;; Relaxation ends when no change occurs, resolving interacting cycles.
(declare (special 6CHANGE))
(defun 6find-joining ()
  (let* ((join-info (over INFO select (cdr (prev _)) bag _))
         written-twice written-once w2-dfnums)
    (dolist (hunk INFO)
      (setq written-twice
            (6setU written-twice (6set^ (append written-once nil) (written hunk))))
      (setq written-once (6setU written-once (written hunk))))
    (setq w2-dfnums (over DF-NUMS select (6set^? _ written-twice) bag _))
    (6set-up-linkages first-hunk (over w2-dfnums -> dfn bag [dfn first-hunk]))
    (over join-info -> hunk (live (car (prev hunk))) -> live
          (over w2-dfnums -> dfn bag [dfn hunk nil]) -> alist
      (setf (unknown hunk) (over alist -> [! entry dfn ! _]
                            select (6set^? dfn live) bag entry))
      (6set-up-linkages hunk alist))
    (repeat (6CHANGE)
      (setq 6CHANGE nil)
      (dolist (hunk join-info) (6process-jhunk hunk))
      (if (null 6CHANGE) (return)))))
(defun 6set-up-linkages (hunk alist)
  (setq alist (over alist -> [dfn ! old]
               bag [dfn ! (cond ((6set^? (written hunk) dfn) [hunk]) (t old))]))
  (setf (source hunk) alist)
  (over (next hunk) select (null (cdr (prev _))) (6set-up-linkages _ alist)))
(defun 6process-jhunk (hunk)
  (let* (unknown join)
    (over (unknown hunk) -> [! item dfnum ! entry]
          nil -> source nil -> source-list nil -> section nil -> provisional
          (over (prev hunk) -> h (source h) -> alist bag (alist dfnum)) -> knowledge
      (cond ((over knowledge -> list
               fastand (over l <- list -> item select item
                         (return (cond ((and (eq item hunk) (cdr l)))
                                       (section (eq item section))
                                       (t (setq section item source-list list))))))
             (setf (car entry) nil) (setf (cdr entry) source-list) (setq 6CHANGE t))
            ((over knowledge -> list (car (last list)) -> item
               (cond ((null item))
                     (source (if (not (eq item source)) (return t)))
                     (t (setq source item provisional list))))
             (setf (cdr entry) nil) (push dfnum join) (setq 6CHANGE t))
            (t (push item unknown)
               (if (equal (cdr entry) '(nil))
                   (cond (provisional (setf (cdr entry) provisional)
                                      (setq 6CHANGE t)))))))
    (setf (unknown hunk) unknown)
    (setf (join hunk) (nconc join (join hunk)))))

;;; PDF page 58
;;; Create join ports during data-flow creation for more mnemonic names.
(defun 6do-joining (hunk)
  (let* ((joinsegcase (start hunk)))
    (over (join hunk) -> dfnum (or (DF-NAME dfnum) '_obj) -> name
          (+& joinsegcase output nil (genobj (~seg joinsegcase) name) nil) -> out
      (6+df out dfnum)
      (+& out @io 'free-var name nil)
      (over (prev hunk) -> h
            (+& (end h) input 'id (genobj (~seg (end h)) name) (~obj out)) -> in
        (6+df in dfnum) (+& in @io 'free-var name nil)))))
(defun 6create-dflow ()
  (let* ((alist (over (??& SUPERSEGCASE input) -> in bag [(6?df in) ! in])))
    (6create-dflow1 alist FIRST-HUNK)))
(defun 6create-dflow1 (alist hunk)
  (cond ((null (flag hunk))
         (setf (flag hunk) t)
         (repeat ((segcase (start hunk)))
           (setq segcase (~to (?& SUPERSEGCASE cflow segcase)))
           (when (equal segcase SUPERSEGCASE) (6make-super-outputs alist) (return))
           (dolist (in (??& segcase input))
             (+& SUPERSEGCASE dflow (alist (6?df in)) in))
           (dolist (out (??& (&* (~seg segcase)) output))
             (setf (alist (6?df out)) out))
           (when (equal segcase (end hunk))
             (over (next hunk) (6create-dflow1 (copyalist alist) _)) (return))))))

;;; PDF page 59
;;; Build superseg outputs, including side-effects and part relationships.
(defun 6make-super-outputs (alist)
  (let* ((out-nums (over (??& SUPERSEGCASE output) -> out 6setU (6?df out)))
         (parts (6find-parts1 (start last-hunk) (end last-hunk) (part last-hunk))))
    (over (??& SUPERSEGCASE input) -> in (6?df in) -> dfn
      select (and (not (6set^? dfn out-nums)) (not (eq in (alist dfn))))
      (let* ((@io (?& in @io)) out)
        (cond ((??& SUPERSEGCASE dflow in)
               (setq out (+& SUPERSEGCASE output 'id
                             (genobj (~seg SUPERSEGCASE) (~obj in)) (~obj in)))
               (+& out @io 'side-effect
                   (if (eq (~kind @io) 'free-var) (~name @io)) nil))
              (t (make-fact-be-out @io)
                 (setq out ;eg in fortran
                       (add-fact (&~keyword (make-fact-be-out in) 'output)))
                 (add-fact (&~deep (make-fact-be-out @io) out))))
        (6+df out dfn)))
    (over (??& SUPERSEGCASE output) -> out (6?df out) -> dfn
          (parts dfn) -> p (alist dfn) -> from
      (cond ((null from) (--& out @io) (--& out @createvar) (make-fact-be-out out))
            (t (over (??& SUPERSEGCASE input) -> in (6?df in) -> d
                 (when (6set^? d p)
                   (if (null (~kind out))
                       (let ((@io (-& out @io)))
                         (make-fact-be-out out)
                         (setq out (>& SUPERSEGCASE output 'part (~obj out) (~obj in)))
                         (assert-fact (&~deep @io out))))
                   (return)))
               (+& SUPERSEGCASE dflow from out))))))

;;; PDF page 60
;;; Remove psetq segments and unused computations; consolidate adjacent joins.
(defvar 6ignored-fns
  '(ada-use~pseudo ada-dcl~pseudo ada-constant-dcl~pseudo
    ada-init-dcl~pseudo ada-type-dcl~pseudo ada-subtype-dcl~pseudo
    ada-new-package~pseudo ada-new-fn~pseudo def-ada-fn~pseudo
    def-ada-generic~pseudo def-ada-package~pseudo
    def-ada-package-body~pseudo ada-file-def~pseudo
    ada-fn~create ada-fn~open ada-fn~reset ada-fn~read
    ada-fn~write ada-fn~set_index ada-fn~get))
(defun 6cleanup (SUPERSEGCASE)
  (dolist (subseg (??subseg (~seg SUPERSEGCASE)))
    (when (??& (&* subseg) @seg 'funcall 'k*psetq)
      (over (??& (&* subseg) output) -> out
            (?& (&* subseg) input * (~id out)) -> in
            (~from (-& SUPERSEGCASE dflow * in)) -> source
        (over (--& SUPERSEGCASE dflow out) (+& SUPERSEGCASE dflow source (~to _))))
      (assert-fact (&~from (-& SUPERSEGCASE cflow (&* subseg))
                          (~from (-& SUPERSEGCASE cflow * (&* subseg)))))
      (kill-seg subseg)))
  (dolist (subseg (??subseg (~seg SUPERSEGCASE)))
    (if (and (eq (?segtype subseg) 'simple)
             (null (??& SUPERSEGCASE dflow (&* (&* subseg) output)))
             (null (??& SUPERSEGCASE slot (&* subseg)))
             (null (??& (&* (&* subseg) output) @io 'side-effect))
             (not (memq (~01 ~name (??& (&* subseg) @seg 'funcall)) 6ignored-fns)))
        (let ((constant (~01 ~name (??& (&* subseg) @seg 'constant))))
          (cond ((null constant)
                 (grumble "6CLEANUP: No used outputs for fn.~% subseg: ~S" subseg))
                (t (if (not (member constant '('nil 'undefined)))
                       (grumble "6CLEANUP: Constant wasn't used.~% Data: ~S" subseg))
                   (assert-fact (&~from (-& SUPERSEGCASE cflow (&* subseg))
                                       (~from (-& SUPERSEGCASE cflow * (&* subseg)))))
                   (kill-seg-and-its-subsegs subseg))))))
  (dolist (subseg (??subseg (~seg SUPERSEGCASE)))
    (when (eq (?segtype subseg) 'join)
      (let ((next (~seg (~to (?& SUPERSEGCASE cflow (&* subseg))))))
        (if (and (??& (&* next) segtype 'join)
                 (over (??& SUPERSEGCASE dflow (&* (&* subseg) port))
                   fastand (eq (~seg (~segcase (~to _))) next)))
            (fuse-joins subseg next))))))
(defun 6set-up-flows () (db-init) (push-db-context) (db-context-was >I))
(6set-up-flows)

;;; PDF page 61
;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp REPEAT Indent:1;
;;; Lisp REPEAT* Indent:1;
;;; End:;
