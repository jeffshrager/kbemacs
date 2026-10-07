;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 299-301; listing ddebug.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; PDF page 300
;;; For Debugging Only
;;; (SHOW-DB-CONTEXT)
;;; Calls to SHOW-DB-CONTEXT type out info about labeled points in the
;;; database context stack. These labels are assigned by the special
;;; forms DB-CONTEXT-IS and DB-CONTEXT-WAS.
;;; Note that all names are in fact just normal special variables, and
;;; are bound to the context they indicate.
(defun context-names (context)
  (do ((l *db-context-names* (cdr l)) (result))
      ((null l) result)
    (cond ((equal (symeval (car l)) context) (push (car l) result)))))
(defun show-db-context ()
  (format t "~&;Context stack:")
  (map #'(lambda (context)
           (format t "~%;~2D " (car context))
           (mapc #'(lambda (n) (format t "~A " n))
                 (context-names context)))
       *db-context*)
  '(:Gunterpri))
(setf #'sc #'show-db-context) ;short name
;;; PDF page 301
;;; (DB-STATS)
;;; This computes some rather copious statistics about the data base,
;;; and returns them to you in an alist. Hopefully each individual
;;; statistic is self explanatory.
(def-alistq facts-by-k)
(def-alistq facts-by-cm)
(def-alist facts-by-idx)
(defun db-stats ()
  (let* ((facts-by-k (mapcar #'(lambda (x) (cons x 0)) *db-keywords*))
         (facts-by-cm (cons (cons *null-cmarker* 0)
                            (mapcar #'(lambda (x) (cons x 0))
                                    *cmarkers*)))
         (facts-by-idx (over n <- 15. by (1- n) until (minusp n)
                        revlist (cons n 0))))
    (dolist (s *db-segs*)
      (let ((hunk (db-index s)))
        (over num <- 15. by (1- num) until (minusp num)
              (dbx num hunk) -> b
          (setf (facts-by-idx num) (+ (facts-by-idx num) (length b)))
          (if (< num 11.)
              (dolist (f b)
                (setf (facts-by-k (~keyword f))
                      (1+ (or (facts-by-k (~keyword f)) 0)))
                (setf (facts-by-cm (cmarker f))
                      (1+ (facts-by-cm (cmarker f)))))))))
    (terpri standard-output)
    (format T "~\vertical\"
      (let ((segnum (length *db-segs*))
            (fnum (over facts-by-k sum (cdr _))))
        `((*db-context* ,*db-context*)
          (number-of-segments ,segnum)
          (number-of-facts ,fnum)
          ,@(if (not (zerop segnum))
                `((average-facts-per-segment
                    ,(//$ (float (// (* fnum 10.) segnum)) 10.0))))
          (facts-per-cmarker
            ,(mapcar #'(lambda (x) `(,(in-layers (car x))
                                     ,(out-layers (car x))
                                     ,(cdr x)))
                     facts-by-cm))
          (facts-per-keyword
            ,(mapcar #'(lambda (x) `(,(car x) ,(cdr x)))
                     facts-by-k))
          ,@(if (not (zerop segnum))
                `((average-facts-per-index
                    ,(mapcar #'(lambda (x)
                                 `(,(car x)
                                   ,(//$ (float (// (* (cdr x) 10.)
                                                    segnum))
                                         10.0)))
                             facts-by-idx)))))))))
(pp:define-list-print-function facts-per-cmarker (x s)
  (format s "(~@l~{~=~%~\tabular\~}~.)" x))
(pp:define-list-print-function facts-per-keyword (x s)
  (format s "(~@l~{~=~%~\tabular\~}~.)" x))
(pp:define-list-print-function average-facts-per-index (x s)
  (format s "(~@l~{~=~%~\tabular\~}~.)" x))
