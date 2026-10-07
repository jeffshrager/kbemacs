;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 224-226; listing dtrace.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; Notes:
;;; This file uses ordinary Lisp control primitives only.
;;; It does not depend on (OVER ...), keyworded (IF ...), (LOOP ...),
;;; (BEGIN ...), (REPEAT ...), (REPEAT* ...), (MATCH-TO ...), etc.
;;; PDF page 225
;;; Tracing DB transactions
;;; This section implements a tracing facility that lets a user get
;;; get notified when transactions occur with the database. The key
;;; idea is that the trace package is woken up every time a fact is
;;; processed by FIND-FACTS, ADD-FACT, ASSERT-FACT, or ERASE-FACTS. The
;;; action taken then depends on trace packets which can be set up by
;;; the user. Such packets are set up by calling DB-TRACE. Calling
;;; DB-UNTRACE removes all trace information.
(defvar *db-trace* nil "If non-nil, information about active db traces.")
;;; (WHEN-DB-TRACED . forms)
;;; This macro should be wrapped around any code which references
;;; DB-DISPLAY-TRACE-INFO so that such code will not end up getting
;;; run in the case where *DB-TRACE* has no trace packets in it.
(defmacro when-db-traced (&body body) `(cond (*db-trace* ,@body)))
;;; (DB-TRACE pattern codes predicate action)
;;; pattern: tracing occurs only if this pattern matches the fact
;;; being processed. * means match every fact. Any other form is
;;; EVAL'd at DB-TRACE time and the result is placed in the packet.
;;; code-list: if NIL, trace always; otherwise ADD, FIND, ERASE.
;;; predicate: arbitrary code executed iff pattern and codes match.
;;; action: arbitrary code executed when tracing has occurred.
;;; The fact being processed will be bound to the free variable
;;; *DB-FACT* during the predicate and action in case user code needs
;;; to access it.
(defmacro db-trace (pattern codes predicate &optional action)
  (if (eq pattern '*) (setq pattern `',pattern))
  `(progn (push `(,,pattern ,',codes ,',predicate ,',action) *db-trace*)
          t))
(defun db-untrace () (setq *db-trace* nil))
;;; (DB-DISPLAY-TRACE-INFO code fact)
;;; CODE is not evaluated. It names the kind of trace.
;;; FACT is evaluated. It may need trace info displayed.
;;; This is a macro and not a function since tracing is likely to be
;;; slow and we need all the help we can get...
(defmacro db-display-trace-info (code fact)
  `(let ((*db-fact* ,fact))
     (declare (special *db-fact*))
     (dolist (packet *db-trace*)
       (let (((pattern codes predicate action) packet))
         (cond ((and (or (null codes) (memq ',code codes))
                     (or (eq pattern '*)
                         (matches-fact? pattern *db-fact*))
                     (or (eq predicate T) (eval predicate)))
                (format t "~%;Tracing - ~S - ~:=" ',code *db-fact*)
;;; PDF page 226
                (return)))))))
