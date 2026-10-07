;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_2.pdf
;;; PDF pages 70-85; listing segtrn.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.



(declare (special segfacts segtypes))
(define-trace-option *SEGTRN-Debug* "Show trace of SEGTRN internals" nil :boolean)
;;; Utilities for simple plan transformations. They handle input, output,
;;; segtype, subseg, dflow and cflow, and preserve other assertions where possible.
;;; Subseg sharing is allowed; links normally do not cross segment boundaries.

;;; PDF page 71
;;; Minimal specs: name, output type, then input types. Expects/asserts suspended.
(defun quicky-defspecs (specs)
  (let* (((name out . ins) specs) (seg (genseg name)) (segcase (& seg))
         (outobj (genobj seg 'z))
         (inobjs (over ins -> in
                   over '(x y) by (or (cdr _) '(arg)) until nil -> ob
                    list (genobj seg ob))))
    (+& segcase @seg 'lambda-exp name nil)
    (+& segcase segtype 'simple)
    (+& (+& segcase output nil outobj nil) @io 'returnval nil nil)
    (over inobjs -> inobj over ins until nil -> in
          over i <- 1 by (1+ i) until nil
      (+& (+& segcase input nil inobj nil) @io 'lambda-arg nil i))
    (~seg segcase)))
(defun kill-seg-and-its-subsegs (seg)
  (dolist (subseg (??subseg seg)) (kill-seg-and-its-subsegs subseg))
  (kill-seg seg))

;;; PDF page 72
;;; GROUP surrounds sibling segs with a new segment, repairing boundary links.
;;; At most one cflow may enter; outgoing data must pass through the NIL case.
;;; Plantype and roles must be supplied explicitly after grouping.
(defun group (a b) (group2 a b 'conditional))
(defun group2 (dsegs prefix conditional?)
  (let* ((sseg (?supseg (car dsegs))) useg)
    (cond ((and (eq conditional? 'conditional) (qset= dsegs (??subseg sseg)))
           (setq useg sseg))
          (t (setq useg (genseg prefix))
             (>& (& sseg) subseg (& useg))
             (dolist (dseg dsegs)
               (--& * role (&* dseg))
               (-& * subseg (&* dseg))
               (>& (& useg) subseg (& dseg)))
             (g2move-cflows sseg useg dsegs)
             (g2move-dflows sseg useg dsegs)
             ;(g2move-tports useg dsegs) ;see note on fn.
             ))
    (format-trace *SEGTRN-Debug* "~S ^X ~:=" useg `(group ,dsegs ,prefix))
    useg))

;;; PDF page 73
(defun g2move-cflows (sseg useg dsegs)
  (let* (ingoing inside outgoing)
    (dolist (cf (??& (&* sseg) cflow))
      (cond ((memq (~seg (~from cf)) dsegs)
             (cond ((memq (~seg (~to cf)) dsegs) (push cf inside))
                   (t (push cf outgoing))))
            (t (if (memq (~seg (~to cf)) dsegs) (push cf ingoing)))))
    (over inside (make-fact-be-out _)
      (>& (& useg) cflow (~from _) (~to _)))
    (if (cdr ingoing)
        (barf "Too many ingoing cflows in group.~% Data: ~S" [useg dsegs]))
    (if ingoing (cflow-splice-in (car ingoing) (& useg)))
    (cond ((cdr outgoing)
           (>& (& useg) segtype 'split)
           (over outgoing (cflow-splice-in _ (& useg (gencase useg)))))
          (t (>& (& useg) segtype 'simple)
             (if outgoing (cflow-splice-in (car outgoing) (& useg)))))))
(defun g2move-dflows (sseg useg dsegs)
  (let* (ingoing inside outgoing)
    (dolist (df (??& (&* sseg) dflow))
      (cond ((memq (~seg (~segcase (~from df))) dsegs)
             (cond ((memq (~seg (~segcase (~to df))) dsegs) (push df inside))
                   (t (push df outgoing))))
            (t (if (memq (~seg (~segcase (~to df))) dsegs) (push df ingoing)))))
    (over inside (make-fact-be-out _)
      (>& (& useg) dflow (~from _) (~to _)))
    (over (over ingoing classify [(~from _) ! _]) -> [from ! idflows]
          nil -> iport
      (cond ((io-fact? from)
             (setq iport (>& (& useg) input nil (genobj useg (~obj from)) nil))
             (>& (& sseg) dflow from iport)
             (over idflows (make-fact-be-out _)
               (>& (& useg) dflow iport (~to _))))
            (t (over idflows
                 (>& (& sseg) dflow from
                     (>& (& useg) tinput (~segcase (~to _))
                         (genobj useg (~obj (~to _))) (~to _)))))))
    (over (over outgoing classifyq [(~from _) ! _]) -> [from ! odflows]
          nil -> oport
      (cond ((io-fact? from)
             (setq oport (>& (& useg) output nil (genobj useg (~obj from)) nil))
             (>& (& useg) dflow from oport))
            (t (setq oport (>& (& useg) toutput (~segcase from)
                              (genobj useg (~obj from)) from))))
      (over odflows (make-fact-be-out _)
        (>& (& sseg) dflow oport (~to _))))))

;;; PDF page 74
;I <Dick> don't understand what this is doing!
;I have spliced it out for now. It seems to make some things blow
;up and is clearly partly redundant with g2move-dflows
(defun g2move-tports (useg dsegs)
  (over (over dsegs nconc (??& * tinput * * (&* (&* _) input))) -> ti
        (~lower-io ti) -> li
        (+& (& useg) input nil (genobj useg (~obj li)) nil) -> newli
    (+& (& useg) dflow newli li)
    (^dflow-to ti (+& (~segcase ti) tinput (& useg) (~obj ti) newli))
    (kill-port ti))
  (over (over dsegs nconc (??& * toutput * * (&* (&* _) port))) -> to
        (~lower-io to) -> lio (~lower-env to) -> le
        (cond ((memq (~seg le) dsegs)
               (cond ((~case le) (~to (?& (&* useg) cflow le (&* useg))))
                     (t (& useg))))
              (t le)) -> newle
        (cond ((input-fact? lio) (~from (?& (&* useg) dflow * lio)))
              (t lio)) -> newlio
    (if (output-fact? newlio)
        (setq newlio
              (cond ((~01 ~to (??& * dflow newlio (&* (&* useg) output))))
                    (t (~to (+& (& useg) dflow lio
                                (+& (& useg) output nil
                                    (genobj useg (~obj lio)) nil)))))))
    (^dflow-from to (>& (~segcase to) toutput newle
                       (genobj (~seg~segcase to) (~obj lio)) newlio))
    (kill-port to))
  (over (over dsegs nconc (??& * toutput (&* _))) -> to
        (~lower-env to) -> le
    (^dflow-from to
      (assert-fact (&~lower-env to
                    (cond ((~case le) (~to (?& (&* useg) cflow le (&* useg))))
                          (t (& useg))))))
    (kill-port to)))

;;; PDF page 75
;;; UNGROUP reverses grouping; transfer roles and plantype where possible.
(defun ungroup (useg)
  (format-trace *SEGTRN-Debug* "(UNGROUP ~S) ~S ~S"
                useg (?supseg useg) (??subsg useg))
  (cond ((?supseg useg) (ungroup-middle useg))
        ((cdr (??subseg useg))
         (barf "cannot ungroup top seg (~A) with >1 subseg" useg))
        (t (ungroup-middle (car (??subseg useg)))))
  nil)
;;; Supply cflow compatible with dflow; all subsegs are assumed simple.
(def-alistq dffrom) ;fsets showing where dflow comes from to a seg
(defun make-compatable-cflow (seg)
  (let* ((segpat (&* seg)) (segcase (& seg)) (subsegs (??subseg seg))
         (dffrom (over subsegs -> sub
                   list [sub ! (over (??& segpat dflow * (&* (&* sub) port))
                                 makefset (~seg~segcase (~from _)))]))
         (lastseg seg) (segsdone [seg])) ;this is an fset
    (repeat ()
      (if (null subsegs) (return))
      (dolist (sub subsegs)
        (when (fset< (dffrom sub) segsdone)
          (>& segcase cflow (& lastseg) (& sub))
          (setq subsegs (delq sub subsegs)
                segsdone (fsetU segsdone [sub]) lastseg sub))))
    (>& segcase cflow (& lastseg) (& seg))))

;;; PDF page 76
(defvar *force-slots-up* nil)
(defun ungroup-middle (useg)
  (let* ((sseg (?supseg useg)) (dsegs (??subseg useg))
         (brothers? (not (null (cdr (??subseg sseg)))))
         (init (?role useg 'init)))
    ;check that the cflow is compatable between useg and sseg, if not fix it.
    (cond ((??& (&* sseg) cflow (&* useg))
           (cond ((null (??& (&* useg) cflow))
                  (cond ((and (null (cdr (??subseg sseg)))
                              (eq (?segtype useg) 'simple))
                         (--& (&* sseg) cflow))
                        (t (make-compatable-cflow useg))))
                 ((and init (null (??& (&* useg) cflow * (&* init))))
                  (cflow-splice-in (?& (&* useg) cflow (&* useg)) (& init))
                  (>& (& useg) cflow (& useg) (& init)))))
          (t (if (??& (&* useg) cflow)
                 (cond ((or (null dsegs)
                            (and (null (cdr dsegs))
                                 (eq (?segtype (car dsegs)) 'simple)))
                        (--& (&* useg) cflow))
                       (t (make-compatable-cflow sseg))))))
    ;fix up subseg relations
    (-& (&* sseg) subseg (&* useg))
    (over dsegs (-& (&* useg) subseg (&* _)) (+subseg sseg _))
    ;fix up plantype and role information if possible
    (cond ((and (null (cdr dsegs)) (null (??& (&* useg) plantype 'tempcomp)))
           (--& (&* useg) plantype) (--& (&* useg) role)
           (--& (&* useg) cliche) (--& (&* useg) slot)
           (over (--& * role (&* useg))
             (assert-fact (&~roleseg _ (& (car dsegs)))))
           (ugmove-slots-down useg (car dsegs)))
          ((not brothers?)
           (--& (&* sseg) plantype) (--& (&* sseg) role)
           (over (--& (&* useg) plantype) (assert-fact (&~segcase _ (& sseg))))
           (over (--& (&* useg) role) (assert-fact (&~segcase _ (& sseg))))
           (let ((lower-cliche (~01 (--& (&* useg) cliche))))
             (when (and lower-cliche (null (??& (&* sseg) cliche)))
               (assert-fact (&~segcase lower-cliche (& sseg)))
               (over (--& (&* useg) slot) (assert-fact (&~segcase _ (& sseg))))))
           (ugmove-slots-up sseg useg))
          (t (--& (&* useg) plantype) (--& (&* useg) role)
             (--& (&* sseg) role (&* useg)) (--& (&* useg) cliche)
             (cond (*force-slots-up*
                    (over (--& (&* useg) slot) (assert-fact (&~segcase _ (& sseg)))))
                   (T (--& (&* useg) slot)))))
    (ugmove-tports useg)
    (ugmove-cflow sseg useg)
    (ugmove-dflow sseg useg)
    (kill-seg useg)))

;;; PDF page 77
(defun ugmove-slots-up (sseg useg)
  (over (--& * slot (&* useg))
    (if (null (??& * slot (&* sseg))) (assert-fact (&~slotseg _ (& sseg))))))
;note cannot be a tc
(defun ugmove-slots-down (useg dseg)
  (over (--& * slot (&* useg))
    (if (null (??& * slot (&* dseg))) (assert-fact (&~slotseg _ (& dseg))))))
;This must run before any dflow in useg is destroyed.
(defun ugmove-tports (useg)
  (over (??& (&* useg) tinput) (^dflow-to _ (~lower-io _)))
  (over (??& (&* useg) toutput) (^dflow-from _ (~lower-io _)))
  (dolist (ti (??& * tinput * * (&* (&* useg) input)))
    (dolist (df (??& (&* useg) dflow (~lower-io ti)))
      (=dflow-to ti (>& (~segcase ti) tinput (~segcase (~to df))
                       (genobj (~seg~segcase ti) (~obj ti)) (~to df))))
    (kill-port ti))
  (over (??& * toutput * * (&* (&* useg) port)) -> to
        (cond ((input-fact? (~lower-io to))
               (~to (a?& (&* useg) dflow (~lower-io to))))
              (t (~from (?& (&* useg) dflow * (~lower-io to))))) -> newlio
        (cond ((eq (~seg (~lower-env to)) useg) (~segcase newlio))
              (t (~lower-env to))) -> newle
    (^dflow-from to (>& (~segcase to) toutput newle
                       (genobj (~seg~segcase to) (~obj to)) newlio))
    (kill-port to)))
(defun ugmove-cflow (sseg useg)
  (let* ((in (~01 (--& (&* sseg) cflow * (&* useg))))
         (lower-to (and in (~01 ~to (--& (&* useg) cflow (~to in))))))
    (if (and in lower-to) (+cflow (~from in) lower-to)))
  (over (--& (&* useg) cflow * (&* useg)) -> inside
        (~01 ~to (--& (&* sseg) cflow (~to inside))) -> outer-to
    (if outer-to (+cflow (~from inside) outer-to)))
  (over (--& (&* useg) cflow) (assert-fact (&~segcase _ (& sseg)))))
(defun ugmove-dflow (sseg useg)
  (over (??input useg) -> port
        (~01 (--& (&* sseg) dflow * port)) -> dflowF
    (cond (dflowF (^dflow-from port (~from dflowF)))))
  (over (??output useg) -> port
        (~01 (--& (&* useg) dflow * port)) -> dflowF
    (cond (dflowF (^dflow-from port (~from dflowF)))))
  (over (--& (&* useg) dflow) (assert-fact (&~segcase _ (& sseg)))))

;;; PDF pages 78-79
;;; Move a segment into its sibling. Repair data flow and splice it out of cflow.
(defun move-seg-down (useg dseg)
  (let* ((dsegcase (& dseg)) (dsegpat (&* dseg))
         (usegcase (& useg)) (usegpat (&* useg))
         (sseg (-supseg dseg)) (ssegcase (& sseg)) (ssegpat (&* sseg)))
    (format-trace *SEGTRN-Debug* "(MOVE-SEG-DOWN ~S ~S)" useg dseg)
    (--& * role dsegpat)
    (>& usegcase subseg dsegcase)
    (over (??& ssegpat dflow (&* usegpat output) (&* dsegpat input)) -> dflow
          (~from dflow) -> source (?& usegpat dflow * source) -> sdflow
          (~from sdflow) -> ssource
      (make-fact-be-out dflow)
      (if (null (??& ssegpat dflow source)) (kill-port source))
      (>& usegcase dflow ssource (~to dflow)))
    (over (--& ssegpat dflow * (&* dsegpat input)) -> dflow
          (~from dflow) -> source
          (~01 ~to (??& ssegpat dflow source (&* usegpat input))) -> input
      (when (null input)
        (setq input (>& usegcase input nil (genobj useg (~obj source)) nil))
        (>& ssegcase dflow source input))
      (>& usegcase dflow input (~to dflow)))
    (over (--& ssegpat dflow (&* dsegpat output) (&* usegpat input)) -> dflow
          (~from dflow) -> source (~to dflow) -> dest
      (^dflow-from dest source)
      (kill-port dest))
    (over (--& ssegpat dflow (&* dsegpat output)) -> dflow
          (~from dflow) -> source
          (~01 ~to (??& usegpat dflow source (&* usegpat output))) -> exit
      (when (null exit)
        (setq exit (>& usegcase output nil (genobj useg (~obj source)) nil))
        (>& usegcase dflow source exit))
      (assert-fact (&~from dflow exit)))
    (let* ((cflowin (~01 (--& ssegpat cflow * dsegpat)))
           (cflowout (~01 (--& ssegpat cflow dsegpat))))
      (if (and cflowin cflowout)
          (assert-fact (&~from cflowout (~from cflowin)))))
    (if (null (cdr (??subseg sseg)))
        (cond ((null (?supseg sseg)) (ungroup useg))
              (t (ungroup sseg)))))
  nil)

;;; PDF page 80
;;; Inverse of MOVE-SEG-DOWN; ungroup useg if it has at most one subseg left.
(defun move-seg-up (dseg)
  (let* ((useg (-supseg dseg)) (sseg (?supseg useg)))
    (format-trace *SEGTRN-Debug* "(MOVE-SEG-UP ~S) ~S" dseg useg)
    (--& * role (&* dseg))
    (>& (& sseg) subseg (& dseg))
    (over (??& (&* useg) dflow (&* (&* useg) input) (&* (&* dseg) input)) -> dflow
          (~from dflow) -> source (?& (&* sseg) dflow * source) -> sdflow
      (make-fact-be-out dflow)
      (if (null (??& (&* useg) dflow source)) (kill-port source))
      (>& (& sseg) dflow (~from sdflow) (~to dflow)))
    (over (--& (&* useg) dflow * (&* (&* dseg) input)) -> dflow
          (~from dflow) -> source
          (~01 ~to (??& (&* useg) dflow source (&* (&* useg) output))) -> exit
      (when (null exit)
        (setq exit (>& (& useg) output nil (genobj useg (~obj source)) nil))
        (>& (& useg) dflow source exit))
      (>& (& sseg) dflow exit (~to dflow)))
    (over (--& (&* useg) dflow (&* (&* dseg) output) (&* (&* useg) output)) -> dflow
          (~from dflow) -> source (~to dflow) -> dest
      (^dflow-from dest source)
      (kill-port dest))
    (over (--& (&* useg) dflow (&* (&* dseg) output)) -> dflow
          (~from dflow) -> source
          (~01 ~to (??& (&* sseg) dflow source (&* (&* useg) input))) -> input
      (when (null input)
        (setq input (>& (& useg) input nil (genobj useg (~obj source)) nil))
        (>& (& sseg) dflow source input))
      (>& (& useg) dflow input (~to dflow)))
    (let* ((cflowin (~01 (--& (&* useg) cflow * (&* dseg))))
           (cflowout (~01 (--& (&* useg) cflow (&* dseg)))))
      (if (and cflowin cflowout)
          (assert-fact (&~from cflowout (~from cflowin)))))
    (if (null (cdr (??subseg useg))) (ungroup useg))))

;;; PDF pages 81-82
;;; Split a join into two connected joins; return (top . bottom).
(def-alistq objtran)
(def-alistq dflows-by-id-obj)
(defun break-up-join (top-cases bot-cases join)
  (let ((tseg join))
    (when (not (or (null (cdr top-cases)) (null bot-cases)))
      (setq tseg (genseg 'join))
      (let* ((tsegcase (& tseg)) (supseg (?supseg join))
             (supsegcase (& supseg)) (supsegpat (&* supseg))
             (t-jcase (& join (gencase join))) (objtran nil))
        ;create new join
        (+& supsegcase subseg tsegcase)
        (+& tsegcase @seg 'join nil (~code (?& (&* join) @seg)))
        (+& tsegcase plantype 'join)
        (+& tsegcase segtype 'join)
        (+& supsegcase cflow tsegcase t-jcase)
        ;set up objtran and the inter-join dflow
        (over (??& (&* join) output) -> output (~obj output) -> obj
              (car (??& output @io)) -> @io
              (+& tsegcase output nil (genobj tseg obj) nil) -> new-from
              (+& t-jcase input 'id (genobj join obj) obj) -> new-to
          (setf (objtran obj) (~obj new-from))
          (when @io
            (assert-fact (&~deep @io new-from))
            (assert-fact (&~deep @io new-to)))
          (+& supsegcase dflow new-from new-to))
        ;move dflow up to top join
        (over (??& supsegpat cflow * (&* join)) -> cflow
          select (member (~case (~to cflow)) top-cases)
          (let* ((jscase (~to cflow)) (tscase (& tseg (gencase tseg))))
            (make-fact-be-out cflow)
            (assert-fact (&~to cflow tscase))
            (over (--& supsegpat dflow * (&* jscase port)) -> dflow
                  (~to dflow) -> to (~01 (--& to @io)) -> @io
                  (>& tscase input 'id (genobj tseg (~obj to))
                      (objtran (~id to))) -> new-to
              (when @io (assert-fact (&~deep @io new-to)))
              (+dflow (~from dflow) new-to)
              (kill-port to))))
        ;remove dflow that need not pass through the top join
        (over (??& supsegpat dflow (&* (&* tseg) output)) -> dflow
              (makeqset (~nset ~from
                         (??& supsegpat dflow *
                              (&* (&* tseg) input * * (~obj (~from dflow)))))) -> froms
          select (null (cdr froms))
          (+dflow (car froms) (~to dflow))
          (kill-port (~from dflow))) ;also kills id ports
        ;remove dflow that need not pass through the lower join
        (over (??& (&* join) output) -> output
              (makeqset (~nset ~from
                         (??& supsegpat dflow *
                              (&* (&* join) input * * (~obj output))))) -> froms
              (over froms
                (when (eq (~seg~segcase _) tseg)
                  (setq froms (delq _ froms)) (return _))) -> tj-from
          select (and tj-from (equal froms [output]))
          (^dflow-from output tj-from)
          (kill-port output)))) ;also kills id ports
    (format-trace *SEGTRN-Debug* "~S ^X ~S"
                  `(,tseg . ,join) `(break-up-join ,top-cases ,bot-cases ,join))
    [tseg ! join]))

;;; PDF page 83
;;; Fuse two joins connected by direct cflow; return the lower join.
(defun fuse-joins (upj j)
  (let* ((upjpat (&* upj)) (jcase (& j)) (jpat (&* j))
         (supseg (?supseg upj)) (supsegpat (&* supseg))
         (upj-jcase (~to (-& supsegpat cflow upjpat jpat))) objtran)
    ;ports in both joins
    (over (??& supsegpat dflow (&* upjpat port) (&* jpat port))
      (setf (objtran (~obj (~from _))) (~id (~to _)))
      (kill-port (~to _)))
    ;ports in the upper join but not the lower
    (over (??& upjpat output) -> out select (null (objtran (~obj out)))
      (let* ((@io (~01 (??& out @io)))
             (new-from (>& jcase output nil (genobj j (~obj out)) nil)))
        (when @io (assert-fact (&~deep @io new-from)))
        (setf (objtran (~obj out)) (~obj new-from))
        (^dflow-from out new-from)
        (over (??& supsegpat cflow * jpat) -> cf
              (+& (~to cf) input 'id
                  (genobj j (~obj new-from)) (~obj new-from)) -> new-to
          (when @io (assert-fact (&~deep @io new-to)))
          (+dflow new-from new-to))))
    ;ports in the lower join but not the upper
    (over (??& supsegpat dflow * (&* upj-jcase port)) -> dflow
          (~01 (??& (~to dflow) @io)) -> @io
          (genobj upj (~obj (~to dflow))) -> new-obj
      (setf (objtran new-obj) (~id (~to dflow)))
      (over (??& supsegpat cflow * upjpat) -> cf
            (+& (~to cf) input 'id (genobj upj new-obj) new-obj) -> new-to
        (when @io (assert-fact (&~deep @io new-to)))
        (assert-fact (&~to dflow new-to)))
      (kill-port (~to dflow)))
    ;move all dflow down from upper to lower
    (over (--& supsegpat cflow * upjpat) -> cflow
          (~to cflow) -> j1case (& j (gencase j)) -> j2case
      (assert-fact (&~to cflow j2case))
      (over (--& supsegpat dflow * (&* j1case port)) -> dflow
            (~to dflow) -> to (~01 (??& to @io)) -> @io
            (+& j2case input 'id
                (genobj j (~obj to)) (objtran (~id to))) -> new-to
        (when @io (assert-fact (&~deep @io new-to)))
        (+dflow (~from dflow) new-to)))
    (kill-seg upj))
  (format-trace *SEGTRN-Debug* "~S ^X ~S" j `(fuse-joins ,upj ,j))
  j)

;;; PDF page 84
;;; Small transformations; ^ stands for move.
(defun =port-seg (port newseg)
  (if (not (eq (~obj port) (genobj newseg (~obj port))))
      (barf "=PORT-SEG: Copied port clashes.~
             ~% Port: ~S~
             ~% Newseg: ~S" port newseg))
  (assert-fact (&~seg~segcase port newseg)))
(defun ^dflow-from (old-port new-port)
  (over (--& * dflow old-port) (+dflow new-port (~to _))))
(defun ^dflow-to (old-port new-port)
  (over (--& * dflow * old-port) (+dflow (~from _) new-port)))
(defun =dflow-from (old-port new-port)
  (over (??& * dflow old-port) (+dflow new-port (~to _))))
(defun =dflow-to (old-port new-port)
  (over (??& * dflow * old-port) (+dflow (~from _) new-port)))
(defun cflow-splice-in (cf segcase)
  (make-fact-be-out cf)
  (+cflow (~from cf) segcase)
  (+cflow segcase (~to cf)))
(defun cflow-splice-out (seg)
  (+cflow (~from (-& * cflow * (& seg))) (~to (-& * cflow (& seg)))))

;;; PDF page 85
;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; End:;
