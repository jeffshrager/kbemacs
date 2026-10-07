;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; PDF pages 233-234 (printed source pages 1-2).
;;;
;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; SYSLIB: All of the prototypes used by the system kernel

;CLICHES ARE IN THIS FILE AS OPPOSED TO ANOTHER CLICHE
;LIBRARY IF THE CLICHE IS REQUIRED FOR THE BASIC RUNNING OF THE
;SYSTEM EVEN IF THE USER DOES NOT MENTION ANY CLICHE BY NAME.
;OR IF A DEFPROTOTYPE MUST BE USED TO DEFINE IT.

;These first dummy guys are used by CREATE-CLICHE-FOR-FUNCTION.
;note that the renaming capability is used in a very cludgy way
;to get the right function names in the cliche.  Note that we
;cannot rename the cliche itself, or its slots because we
;have to be able to get at the slot-order information.

;note these are for internal use only.  If they were intended for
;external use, the fn would be a slot.

(define-cliche zeroary-function
    (main "Dummy zero argument function cliche")
  (fn))

(define-cliche unary-function
    (roles (x)
     main "Dummy one argument function cliche")
  (fn {input x}))

(define-cliche binary-function
    (roles (x y)
     main "Dummy one argument function cliche")
  (fn {input x} {input y}))

;These cliches are used for impedance matching by
;implement-seg.

(define-cliche simple-subseg
    (roles (subseg)
     main "Used to nest one seg inside another.")
  {subseg})

(DEFINE-CLICHE MAP
    (PRIMARY-ROLES (ITEM FN)
     DESCRIBED-ROLES (ITEM FN)
     COMMENT "maps {the function} over {the item}")
  (LOOP DO
    {({the fn} {the input item}), the output result}))
