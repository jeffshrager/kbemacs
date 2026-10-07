;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 118-120; listing flavor-magic.

;;; -*- Mode:LISP; Package:KBE; Base:10.; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; FLAVOR-MAGIC: Useful flavor utilities

;;;; Flavor Utilities

;;; Purpose
;;;
;;;   This module contains code which may tie heavily into the
;;;   internal workings of the Lisp Machine flavor system in order
;;;   to provide some higher level flavor abstractions useful
;;;   elsewhere in the KBE.
;;;
;;;
;;; Dependency Information
;;;
;;;   All code in this module is vanilla. It lives in the KBE package
;;;   because it is part of the KBE system, but it requires no special
;;;   support from the KBE system and can be lifted freely for other
;;;   applications.

;;;; Name Mixin

;;; NAME-MIXIN
;;;
;;; This mixin is intended to add flexible print methods to a flavor.
;;;
;;;
;;; :NAME a-name                      (NAME-MIXIN init option)
;;;  Initializes the name of the flavor to a-name. If not supplied,
;;;  it is initialized based on the type of the flavor being instantiated
;;;  and the number of previous anonymous instances created. eg, the
;;;  first anonymous instance of SPACE-MAN would be called "Space Man 1".
;;;
;;; :SET-NAME a-name                  (NAME-MIXIN message)
;;;  Sets the name of the flavor to a-name.
;;;
;;; :NAME                             (NAME-MIXIN message)
;;;  Gets the name of the flavor.
;;;
;;; Objects of this flavor handle the :PRINT-SELF message displaying as
;;; #<type name octal-address> for PRIN1, or as just their name for PRINC.
;;;
;;; eg,        (FORMAT T "~S" (MAKE-INSTANCE 'SPACE-MAN))
;;;
;;;            displays:   #<SPACE-MAN "Space Man 1" 123456>
;;;
;;;            (FORMAT NIL "~A" (MAKE-INSTANCE 'SPACE-MAN))
;;;
;;;            displays:   Space Man 1

(defflavor name-mixin (name) ()
  (:documentation :mixin "Adds a :NAME message to a flavor.")
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables
  (:default-init-plist :name nil))

(defmethod (name-mixin :after :init) (&rest ignore)
  (when (not name)
    (let ((my-type (typep self)))
      (setq name (format nil "~A ~D"
			 (string-capitalize-words my-type)
			 (setf (get my-type 'anonymous-instance-count)
			       (1+ (or (get my-type
					    'anonymous-instance-count)
				       0))))))))

(defmethod (name-mixin :print-self) (stream level prin1? &rest ignore)
  level ;ignored
  (let ((my-name (or (send self ':name) "Anonymous")))
    (cond ((not prin1?) (format stream "~A" my-name))
	  (t
	   (format stream "#<~S ~S ~O>"
		   (typep self) my-name (%pointer self))))))

;;;; Property List Mixin

;;; PROPERTY-LIST-MIXIN
;;;
;;;  This mixin allows an object to have its own property list.
;;;  This may be extremely useful for objects that change a lot,
;;;  so there is an easy way to simulate the effect of extra
;;;  instance variables during debugging.
;;;
;;;  :PROPERTY-LIST
;;;    Gets the property list.
;;;
;;;  :PROPERTY-LIST-POINTER
;;;    Gets a locative to the property list.
;;;
;;;  :GET-PROPERTY indicator
;;;    Gets the value of the property named by INDICATOR.
;;;
;;;  :PUT-PROPERTY indicator value
;;;    Places VALUE in the property slot named by INDICATOR.
;;;
;;;  :REMOVE-PROPERTY indicator
;;;    Removes the slot named by INDICATOR from the object's
;;;    property list.

(defflavor property-list-mixin ((property-list '())) ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

(defmethod (property-list-mixin :property-list-pointer) ()
  (locf property-list))

(defmethod (property-list-mixin :get-property) (indicator)
  (get (locf property-list) indicator))

(defmethod (property-list-mixin :put-property)
	   (indicator &optional (value (or (get (locf property-list)
						 indicator)
					    t)))
  (putprop (locf property-list) value indicator))

(defmethod (property-list-mixin :remove-property) (indicator)
  (remprop (locf property-list) indicator))
