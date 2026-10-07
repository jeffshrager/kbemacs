;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 227-227; listing dutil.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; (SEL01 facts) n= 0,1
;;; (SEL1 facts)  n= 1
;;; (SEL1+ facts) n> 0
;;; These functions are intended to provide convenience and error
;;; checking in database accesses. They accomplish the following:
;;; (1) Checking that the proper number of facts were found.
;;; (2) De-listing; Given a list of facts, they return the first in
;;;     the list, or NIL if no facts were given and that is not an error.
;;; The conditions for the number of facts is shown above beside the
;;; syntax information.
;;; A variety of macros are available for combining the use of these
;;; functions with FIND-FACTS and erase-facts.
;;; Note: The special variable *DB-LAST-PATTERN-FOUND*
;;;       is used to increase the usefulness of the error messages
;;;       produced. It should be noted that the error messages will be
;;;       very confusing if you use these functions anywhere but wrapped
;;;       directly around calls on FIND-FACTS or ERASE-FACTS, since the
;;;       error messages they type may otherwise reference stale data.
(defun sel1 (facts)
  (cond ((null facts)
         (barf "No facts found when 1 expected.~
               ~% Pattern: ~:="
               *db-last-pattern-found*))
        ((cdr facts)
         (cbarf "Let SEL1 return ~2*~:="
                "More than 1 fact found when 1 expected.~
               ~% Pattern: ~:=~
               ~% Facts found: ~:="
                *db-last-pattern-found* facts (car facts))
         (car facts))
        (t
         (car facts))))
(defun sel01 (facts)
  (cond ((not (cdr facts))
         (car facts))
        (t
         (cbarf "Let SEL01 return ~2*~:="
                "More than one fact found when 0 or 1 expected.~
               ~% Pattern: ~:=~
               ~% Facts found: ~:="
                *db-last-pattern-found* facts (car facts))
         (car facts))))
(defun sel1+ (facts)
  (cond ((not (null facts))
         (car facts))
        (t
         (barf "No facts found when one or more expected.~
               ~% Pattern: ~:="
               *db-last-pattern-found*))))
