;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 115-117; listing system-magic.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; SYSTEM-MAGIC: Miscellaneous LispM System Internals Magic

;;;; System Utilities

;;; Purpose
;;;
;;;   This module contains code which makes strong assumptions about the
;;;   internal workings of the Lisp Machine in order to provide some
;;;   higher level abstractions useful elsewhere in the KBE.
;;;
;;; Dependency Information
;;;
;;;   All code in this module is vanilla. It lives in the KBE package
;;;   because it is part of the KBE system, but it requires no special
;;;   support from the KBE system and can be lifted freely for other
;;;   applications.

;;; (LOADED-BAND-STRING)
;;;
;;;  Returns a string containing a phrase describing the loaded
;;;  band including the number, comment (if any), and microcode.
;;;  eg, the return value might be something like:
;;;
;;;    "band 9 (Black Magic) with microcode 17"

(defsubst system-additional-info ()
  (if (and (boundp 'si:system-additional-info)
	   (plusp (array-active-length si:system-additional-info)))
      si:system-additional-info))

(defun loaded-band-string ()
  #+3600 (format nil "~A~@[ (~A)~]"
		 si:loaded-band-name
		 (system-additional-info))
  #-3600 (format nil "band ~C~@[ (~A)~] with microcode ~D"
		 (ldb #o2010 si:current-loaded-band)
		 (system-additional-info)
		 si:%microcode-version-number))

;;; (SITE-STRING)
;;;
;;;  Returns a string containing the site name. eg, the return value
;;;  might be something like:
;;;
;;;              "Lisp Machine Twenty-Two"

(defun site-string ()
  (values (chaos:host-data)))

;;; (RELEASED-SYSTEM-VERSION-ACTIVE? system)
;;;
;;;  Returns true if system is the name of a system which is on the
;;;  release list and its current patch version is the version effective
;;;  as of the release.

(defun released-system-version-active? (system)
  (not (assoc system (si:unreleased-patch-systems))))

;;; (KBE-VERSION-STRING-FOR-PATHNAME)
;;;
;;;  Returns a string which is suitable for use as an FN1 in most
;;;  filesystems and which is characteristic of the current KBE system.
;;;  eg, In KBE 103.7, (KBE-VERSION-STRING-FOR-PATHNAME) => 103-07

(defun kbe-version-string-for-pathname ()
  (multiple-value-bind (major minor)
      (si:get-system-version "KBE")
    (cond ((zerop minor) (format nil "~3,'0D" major))
	  (t (format nil "~3,'0D-~2,'0D" major minor)))))

;;; (SYSTEM-VERSION-STRING system-name)
;;;
;;;  Returns a string representing the id of a given system name. Uses a
;;;  compact format for system status when possible. eg, here are some
;;;  sample conditions and return values:
;;;
;;;    (SYSTEM-VERSION-STRING "KBE")    => "KBE 27.3x"      ; Experimental
;;;    (SYSTEM-VERSION-STRING "System") => "System 210.128" ; Released
;;;    (SYSTEM-VERSION-STRING "FOO")    => "FOO 0.0 (non-existent)"
;;;
;;;  By special exception, accepts the keyword :RELEASE in place of a
;;;  string and returns a release system version string. eg,
;;;
;;;    (SYSTEM-VERSION-STRING ':RELEASE) => "Release 4.0"

(defun system-version-string (system)  ;used in com-Bug-KBE
  (multiple-value-bind (major minor state)
      (cond ((eq system ':release)
	     (setq system "Release")
	     (si:get-release-version))
	    ((not (stringp system))
	     (barf "Bad argument to SYSTEM-VERSION-STRING: ~S" system))
	    (t
	     (si:get-system-version system)))
    (format nil "~A ~D.~D~A"
	    system (or major 0.) (or minor 0.)
	    (caseq state
	      ((:released) "")
	      ((:experimental) "x")
	      ((nil) " (non-existent)")
	      (t (format nil " (~A)" (string-downcase state)))))))
