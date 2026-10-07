;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>PBM-HANDLERS.LISP
;;; PDF pages 236-241 (printed source pages 1-6).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; PBM-HANDLERS: Definitions of explanation stuff for PBMs.
;;; Abstraction support

;;; (DEFINE-EXPLANATION-TYPE type)
;;; Defines a function DEFINE-type using the given TYPE which will
;;; work for defining that kind of explanation.
(defmacro define-explanation-type (type)
  (let ((property (intern-keyword type))
        (defname  (symbolconc "DEFINE-" type)))
    `(defmacro ,defname (pbm-name bvl &body body)
      `(defun (:property ,pbm-name ,',property) ,bvl
        ,@bvl ;ignorable
        ,@body))))
;;; Define DEFINE-SUMMARY, etc. These definition forms allow us to
;;; define various kinds of explanations for PBMs.
(define-explanation-type SUMMARY )
(define-explanation-type DETAIL)

;;; Constants
;;; CONSTANT
(define-summary CONSTANT (relative-to type thing)
  relative-to ;ignored
  (format nil "~A" (cadr (~name (?& (segment-of thing) @seg)))))
(define-detail CONSTANT (relative-to type thing)
  relative-to ;ignored
  (let ((value (cadr (~name (?& (segment-of thing) @seg)))))
    (selectq type
             (seg (format nil "Produces the constant value ~A" value))
             (output (format nil "The value, ~A, of the constant" value)))))

 ;;; Expr
 ;;; EXPR
(define-summary EXPR (relative-to type thing)
  (selectq type
           (output (explain (from thing) relative-to ':summary))
           (seg (explain (from (?& thing output)) relative-to ':summary))))

;;; New View
;;; NEW-VIEW
(define-summary NEW-VIEW (relative-to type thing)
  (trace-explanation "~&Summarizing (New-View)~
                    ~% Relative to ~S~
                    ~% Type ~S~
                    ~% Thing ~S~%" relative-to type thing)
  (let ((function (~name (?& (segment-of thing) @seg))))
    (selectq type
             ((seg output)
              (let ((my-name (or (get function 'noun) function) ))
                (cond
                  ((and (eq my-name '5noop)
                        (let ((data (find-inputs (segment-of thing) relative-to)))
                          (and data (not (cdr data)) (text "~A" (car data))))))
                  (my-name
                   (format nil "the ~A of ~A"
                           my-name
                           (phrase-append "and"
                                          (find-inputs (segment-of thing)
                                                       relative-to))))
                  (t (explain thing relative-to  ':outer)))))
             (input (find-input thing relative-to)))))
(define-detail NEW-VIEW (relative-to type thing)
  (let ((segment (segment-of thing)))
    (let ((function (~name (?& segment @seg))))
      (selectq type
               (seg (let ((input-explanations
                           (find-inputs segment relative-to))
                          (output-explanations
                           (mapcar #'explain-output  (??& segment output) ))
                          (my-name  (get function 'verb))
                          front)
                      (setq front
                            (cond (my-name)
                                  ((length (??& segment input))
                                   (format nil "Takes the ~A of" function) )
                                  (t (format nil "Applies the function ~A to"
                                             function) )))
                      (format nil "~A ~A to produce ~A"
                              front
                              (phrase-append "and" input-explanations)
                              (phrase-append "and" output-explanations))))
               (input (format nil "~A of the function ~A"
                              (explain-input thing nil)
                              function) )
               (output (format nil "~A of the function ~A"
                               (explain-output thing)
                               function))))))

;;; Pred

(define-summary PRED (relative-to type thing)
  thing relative-to ; ignored
  (selectq type
           (seg (format nil
                        "I'm a predicate."))
           (input "I'm an input to a predicate.")))
(define-detail PRED (relative-to type thing)
  relative-to  thing ; ignored
  (selectq type
           (seg "predicate")
           (input "input to a predicate")))

;;; Nouns and verbs
(defprop + "sum" noun)
(defprop + "adds" verb)
(defprop - "difference" noun)
(defprop - "subtracts" verb)
(defprop // "division" noun)
(defprop // "divides" verb)
(defprop * "product" noun)
(defprop * "multiplies" verb)
(defprop EQ "EQ comparison" noun)
(defprop EQ "compares (using EQ)" verb)
(defprop = "numeric comparison" noun)
(defprop = "compares the numbers" verb)
