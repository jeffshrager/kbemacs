;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 249-250; listing dvars.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; PDF page 250
;;; Declarations for the DB
(defvar *db-lock* nil
  "Lock cell to assure only one consumer of the DB at a time.")
(def-db-var *db-segs* '())
(def-db-var *cmarkers* '())
(def-db-var *null-cmarker* (build-cmarker nil nil nil nil))
(defvar *initial-db-context-number* 1)
(def-db-var *initial-db-context* (build-layers-list
                                 *initial-db-context-number*))
(def-db-var *db-context* *initial-db-context*)
(defvar *db-context-count* *initial-db-context-number*)
(defvar *db-last-pattern-found* nil "Last pattern seen by FIND-FACTS")
(defvar *db-context-names* '())
(define-internal-option *db-check-facts* "Dynamically audit DB facts"
  t :boolean)
;; It doesn't work to run with this set to T yet because there
;; are TOO many bugs caught by it...
(define-internal-option *db-check-fact-ports*
  "Warn if a removed fact is still referred to" nil :boolean)
(defvar QF nil)
(defvar QS nil)
(defvar QP nil)
(defvar QPAS nil)
(defvar QPP nil)
(defvar QPWP nil)
(defvar *db-top-fn* nil)
(defvar *db-audit* nil)
(defun db-init-vars ()
  ;; This used to init the variables
  ;; *DB-CHECK-FACTS*
  ;; *DB-CHECK-FACT-PORTS*
  ;; but I don't see the point. I think they are not the sort of
  ;; thing that want to reset when the DB is reset. --kmp
  (setq *db-segs* '()
        *cmarkers* '()
        *null-cmarker* (build-cmarker nil nil nil nil)
        *db-context* *initial-db-context*
        *db-context-count* *initial-db-context-number*
        *db-context-names* nil
        QF nil
        QS nil
        QP nil
        QPAS nil
        QPP nil
        QPWP nil
        *db-top-fn* nil
        *db-trace* nil
        *db-audit* nil)
  'DONE)
