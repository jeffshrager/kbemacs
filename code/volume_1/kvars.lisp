;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 141-141; listing kvars.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; KVARS: This module contains declarations and simple initializations
;;;        for variables used by more than one module.

(defvar *KBE-package* (pkg-find-package "KBE")
  "Cache for the KBE package object.")

(defvar *Vanilla-package* (pkg-find-package "VANILLA")
  "Special purpose package for handling cliches. Not for KBE code!")

(defvar *KBE-user-package* *KBE-package*
  "Package KBE should build code in.")

(defvar *keyword-package* (pkg-find-package "")
  "Catch for the KEYWORD package object." )
