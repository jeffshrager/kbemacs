;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_2.pdf
;;; PDF pages 3-7; listing surface.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; the prefix in this file is 6

(declare (special DEFINING-NAME LASTSEGCASE SUPERSEGCASE ALIST DF-NUM
                  TAGS SIMPLE-EXPRS RETVAR FREE-IN FREE-OUT LAST-HUNK
                  FIRST-HUNK INFO FLOWS-HELPS-ANALYZE DF-NUMS DF-NAME
                  DEFINING-CODE RE-FLOW-EVAL INLINE-LIST UNGROUP-LIST
                  *db-context-names* 6DFN-PORT-LIST K*PROG-STACK
                  MNEMONIC-NAME)
         (*expr analyze)
         (*fexpr deffacts))

;this is a lisp to surface plan converter.  self contained, it works
;on LISP progrmas.  In combination with the program FREAD it can be
;used to convert FORTRAN.  In combination with the programs of GSB it
;can be used to convert COBOL.
; in order to simplify things, these programs always assume that the
;input program is COMPLETELY SYNTACTICALLY CORRECT.  it never goes
;out of its way to check anything.  as a result if there are syntactic
;problems, you can get some pretty dumb garbage produced.
; Note that the programs assume no aliassing of inputs to fns with
;side-effects.  It is only really tested to work with looping programs,
;though there is code in here to deal with recursive programs.  Further
;its knowledge of side-effects is weak.  it gets the right dflow in an
;interesting class of situations, but not all the time.  it is easy
;to write a program it will get mixed up on.

;The top-level function is surface-analyze.
;It works in several basic passes.
;1. the fn expand-special-forms is used to preprocess the input so that every fexpr
;   and macro is removed from it.  The result uses a set of only
;   8 6fexprs: K*LAMBDA, K*PROG, K*GO, K*TAG, K*IFNULL, K*IF,
;   K*PSETQ, and K*QUOTE.
;   everything else in the file only has to deal with these things,
;   and ordinary exprs.
;2. the fn 6flow-eval operating in analogy to a lisp evaluator, runs over the
;   program creating all of the segments, and all of the control flow.
;   Annotations (@IOs) concerning data flow are made, but the actual creation
;   of this flow is left until later.  Note that at this stage, segments
;   are put in corresponding to setqs in order to facilitate the next stages.
;   these segs are removed later.  Information about dflow is
;   communicated to the next stage as follows:  each input and output
;   object is given a DF property which is a number.  Every port that
;   reads or writes the same variable gets the same number.
;   when the next stage runs over the program, it uses these numbers in
;   order to reconstruct the data flow relationships.
;3. the fn 6data-flow works in four stages:
; a. first it computes what variables are LIVE in each strait line section
;    of the program.
; b. next it computes what data flow has to be added due to side-effects.
;    Note that this step uses a weak but useful model of when side-effects
;    create additional dflow.
; c. it determines what dflows have to be joined at each join.
; d. lastly, it constructs the actual dflow assertions.
;4. the fn 6cleanup runs over the program performing a few cleanup chores
;   such as getting rid of the setq segs.
;;; This is the top-level entry-point to the surface analyzer.
;;; Apply it to a defun or define-cliche or an sexpr
;;; The segcase of the outer segment produced is returned.

(defun surface-analyze (code)
  (declare (special *input-language*))
  (let* ((DEFINING-CODE code)
         (RE-FLOW-EVAL nil))
    (cond ((and (listp code) (eq (car code) 'defun))
           (letS ()                     ;kill all old definitions
             (kill-seg-and-its-subsegs
               (~seg~segcase
                 (Elist (??& * @seg 'lambda-exp (cadr code))))))
           (let* ((DEFINING-NAME (cadr code)) ;only works for simple defuns
                  (MNEMONIC-NAME (cadr code))
                  (segcase (6flow-analyze `(lambda ., (cddr code)))))
             (assert-fact (&~name (-& segcase @seg) DEFINING-NAME))
             segcase))
          ((and (listp code) (eq (car code) 'define-cliche))
           (letS ()                     ;kill all old definitions
             (kill-seg-and-its-subsegs
               (~seg~segcase (Elist (??& * cliche-def (cadr code))))))
           (let* ((DEFINING-NAME nil)
                  (MNEMONIC-NAME (cadr code))
                  (name (cadr code))
                  (cdr-plist (caddr code))
                  (segcase
                    (6flow-analyze
                      `(progn ., (6not-just-variable (cdddr code))))))
             (>& segcase cliche-def name cdr-plist)
             segcase))
;ada stuff
          ((and (listp code)
                (eq (car code) 'def-ada-top)
                (memq (caar (cddddr code)) '(def-ada-fn def-ada-package)))
           (letS ()                     ;kill all old definitions
             (kill-seg-and-its-subsegs
               (~seg~segcase
                 (Elist (??& * @seg 'lambda-exp (cadr code))))))
           (let* ((DEFINING-NAME (cadr code))
                  (MNEMONIC-NAME (cadr code))
                  (segcase (6flow-analyze
                             `(,(caseq (caar (cddddr code))
                                  (def-ada-fn 'ada-top-fn)
                                  (def-ada-package 'ada-top-package))
                               ., (cdar (cddddr code))))))
             (assert-fact (&~name (-& segcase @seg) DEFINING-NAME))
             segcase))
          ((and (listp code)
                (eq (car code) 'def-ada-top)
                (eq (caar (cddddr code)) 'def-ada-cliche))
           (letS () ;kill all old definitions
             (kill-seg-and-its-subsegs
               (~seg~segcase (Elist (??& * cliche-def (cadr code))))))
           (let* ((DEFINING-NAME nil)
                  (MNEMONIC-NAME (cadr code))
                  (name (cadr code))
                  (cdr-plist (caddr (car (cddddr code))))
                  (segcase
                    (6flow-analyze (car (cddddr code)))))
             (>& segcase cliche-def name cdr-plist)
             segcase))
;should proably add some more stuff here for ada case.
;the stuff that is below is making up for the fact that
;the separation between lisp and ada isn't clean.
          (t (when (eq *input-language* 'ada)
               (if (numberp code) (setq code `(int-lit ,code)))
               (if (stringp code) (setq code `(str-lit ,code)))
               (if (and (listp code) (equal (car code) '(string-literal)))
                 (set code `(str-lit ,(cadr code)))))
             (let* ((DEFINING-NAME nil)
                    (MNEMONIC-NAME nil))
               (6kill-unneeded-top-seg
                 (6flow-analyze
                   (car (6not-just-variable (list code))))))))))

(defun 6not-just-variable (list)
  (cond ((and (= 1 (length list)) (symbolp (car list)))
         `((5noop ,(car list))))
        (T list)))

;note that it is important that we keep the top seg
;if the inner seg is a slot, so that the slot will
;stay distinct from other slots.  The problem would be if
;we implemented a slot with another naked slot, then
;we would have two slots in the same place which would mess
;things up.

(defun 6kill-unneeded-top-seg (segcase)
  (cond ((and (= 1 (length (??subseg (~seg segcase))))
              (null (??& segcase slot)))
         (let* ((subsegcase (~subseg (-& segcase subseg))))
           (kill-seg (~seg segcase))
           subsegcase))
        (T segcase)))

;This is the internal entry into the flow analyzer.  It returns the
;segcase of the plan produced.  It follows the stages outlined in the
;first page of this file.

(defun 6flow-analyze (code)
  (db-context-was <FLOW)
  (let ((DF-NUMS nil)
        (DF-NAME nil)
        (6DFN-PORT-LIST nil)
        (K*PROG-STACK nil)
        segcase)
    (setq code (expand-special-forms code))
    (unwind-protect
      (progn (setq segcase (6flow-eval code))
             (6data-flow segcase))
      (dolist (seg *db-segs*) (remprop seg 'df)))
    (6cleanup segcase)
    (db-context-is >FLOW)
    (push-db-context)
    segcase))

;;; This tells you if two segments have the same io behavior.
(defun 6sameio (seg1 seg2)
  (set= (6abstractio seg1) (6abstractio seg2)))

(defun 6abstractio (seg)
  (append (6aio2 (??input seg)) (6aio2 (??output seg))))

(defun 6aio2 (ports)
  (over ports -> port
        (??& port @io) -> @ios
         (~01 ~kind @ios) -> kind
    bag [(~keyword port)
         (caseq kind ((actual-arg lambda-arg) 'arg) (t kind))
         (caseq kind
           ((lambda-arg actual-arg) (~01 ~argno @ios))
           (side-effect (car (6aio2 (??& (&* (~seg~segcase port))
                                         input * (~id port)))))
           (free-var (~01 ~name @ios)))]))

;;; This is for backward compatibility with old programs.
;;; note it hacks COBOL right.

(defmacro define (name &body rest)
  (cond ((and (listp (car rest)) (eq (caar rest) 'cobol-fn))
         `(surface-analyze '(defun ,name nil ,@rest)))
        (T (grumble "DEFINE is obsolete.  Use SURFACE-ANALYZE.")
           `(surface-analyze '(defun ,name ,@rest)))))

;this allows you to define a group of forms together as one.  they
;must be ordered in the call so that each definition FOLLOWS every
;call of a fn except possibly a call inside itself.  (This implies
;that there can be self-recursion, but no mutual recursion.)  The first
;form is the TOP form, and its segcase is returned.  the plans for the
;other forms are substituted in at the points of call, and then forgotten.

(defmacro define-consolidated (fn-names &body defs)
  (let ((INLINE-LIST fn-names) UNGROUP-LIST return-segcase)
    (dolist (form (reverse defs))
      (setq return-segcase (eval form)))
    (assert-fact (&~code (-& return-segcase @seg) defs))
    (over (cdr INLINE-LIST) (kill-seg-and-its-subsegs (?lambda-exp _)))
    (over UNGROUP-LIST
      select (null (??& (&* _) recursive))
      (ungroup _))
    `',return-segcase))
