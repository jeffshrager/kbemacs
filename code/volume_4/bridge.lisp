;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>BRIDGE.LISP.50, 1 June 1984 22:04:23
;;; Transcribed from Knowledge-Based_Editor_Jan-1985_volume_4.pdf,
;;; PDF pages 3-10 (printed source pages 1-8).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; BRIDGE: Functions to bridge the gap between the KBE internals
;;;         and the interface

;;; ASSOCIATED-INPUT-LINES
;;;
;;;  Making this a DB-PROPERTY means that if the associated seg is killed,
;;;  this property will be killed from its plist. Actually, this is a
;;;  cheat since there out to be a place for it as a database assertion,
;;;  but that really doesn't matter for now.

(def-db-property associated-input-lines)

;;; (EXECUTE request)
;;;
;;;   Generic request handler. Handles texts and strings.

(defun execute (request)
  (cond ((stringp request)
         '(eval-command (parse-command request)))
        ((not (atom request))
         (eval-command request))
        (t
         (barf "~S is not a valid request. -- EXECUTE" request))))

(defun parsed->code (parsed)
  (let ((things (eval-command parsed)))
    (if things (~code (sel1 (??& (sel1 things) @seg))))))

(defun code->facts (sexp)
  (cond ((not sexp) nil)
        (t (??& * @seg * * (list sexp)))))

(defun fact->parsed (fact)
  (make-ref (~deep fact) t))

(defun fact->seg (fact) (~seg (~deep fact)))

(defun superseg (fact) (?supseg fact))

(defun name->seg (name)
  (or (~01 ~seg~segcase (??& * @seg 'lambda-exp name))
      (~01 ~seg~segcase (??& * cliche-def name))))

(defun name->segcase (name)
  (let ((x (name->seg name))) (if x (ncons x))))

(defun get-toplevel-name (referent)
  (let ((top-seg (get-toplevel-seg referent)))
    (or (~01 ~cliche-def-name (??& (&* top-seg) cliche-def))
        (~01 ~name (??& (&* top-seg) @seg 'lambda-exp)))))

(defun get-name-from-seg (seg) ;nicer name
  (?@seg-name seg))

(defun get-toplevel-seg (referent)
  (plan-top (~seg (get-segcase referent))))

(defun slot-marker? (sexp)
  (and (not (atom sexp))
       (eq (car sexp) 'kbe-#-comment-macro)))

(defun code->parsed (sexp)
  (let ((facts (code->facts sexp)))
    (let ((filtered-facts
           (mapcan #'(lambda (x)
                       (if (or (eq (~kind x) 'lambda-exp)
                               (??& * slot (~deep x)))
                           (ncons x)))
                   facts)))
      (cond ((not filtered-facts) nil) ; no description?
            (t (fact->parsed (car filtered-facts)))))))

;;; (PARSED->FACTS parsed-form)
;;;
;;;  Gets a list of facts corresponding to what the parsed reference-
;;;  refers to if such a reference exists.
;;;
;;;  The second return value says if there was a failure. If the second
;;;  value is NIL, then the first value is the facts list. If the
;;;  second value is non-null, then there was some sort of problem
;;;  evaluating the parsed reference and the second value is an error
;;;  string.

(defun parsed->facts (parsed-form)
  (condition-case (result)
      (eval-command parsed-form)
    (sys:error (values nil "Internal error resolving a parsed reference."))
    (:no-error (values result
                       (if (not result) "That's meaningless to me.")))))

;;; Things used in VIEW...

(defun plan-top (seg)
  (do ((s seg ss)
       (ss (?supseg seg) (?supseg ss)))
      ((null ss) s)))

(defun get-segcase (thing)
  (cond ((not (fact? thing)) thing)
        ((port-fact? thing) (~segcase thing))
        ((cflow-fact? thing) (~from thing))
        (t (~segcase (~from thing)))))

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp WHEN-ERROR Indent:1;
;;; Lisp UNLESS Indent:1;
;;; End:;
