;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 149-151; listing interface-macros.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; INTERFACE-MACROS: Macros needed by the interface portion but
;;;                   not by the core system.
;;; PDF page 150
;;; Packaging
;;; The concept of *KBE-user-package* is ill-formed, but we live with
;;; it for now because things tend to build symbols in the wrong
;;; packages and we haven't tracked down all the places that happens.
;;; As soon as we do, all references to the "user" package probably
;;; want to go away.
;;; I say probably because actually when communicating across machine
;;; boundaries, some interesting problems (like what package to use
;;; on the foreign host) may come up and the thing may have its place
;;; again.
(defmacro with-user-package (&body body)
  `(local-declare ((special *KBE-user-package*))
     (let ((package *KBE-user-package*))
       ,@body)))
(defmacro let-user-package (form &body body)
  `(let ((*KBE-user-package* ,form))
     (declare (special *KBE-user-package*))
     ,@body))
(defmacro with-kbe-package (&body body)
  `(local-declare ((special *KBE-package*))
     (let ((package *KBE-package*))
       ,@body)))
;;; PDF page 151
;;; The variable *DELEGATE* (defined in another file) is the
;;; rough analog of a self-variable in our message passing world.
;;; It wants to always hold a valid actor.
(defmacro with-delegate (handler &body forms)
  `(let ((*delegate* ,handler))
     (declare (special *delegate*))
     ,@forms))
(defvar *reply-to* nil
  "If non-NIL, a message to reply to instead of handling events locally.")
(defmacro with-reply-to (message &body forms)
  `(let ((*reply-to* ,message))
     (declare (special *reply-to*))
     ,@forms))
(defmacro tell (actor key &rest args)
  `(*tell ,actor ',key ,@args))
(defmacro ask (actor key &rest args)
  `(*ask ,actor ',key ,@args))
(defmacro delegate (actor key &rest args)
  `(*delegate ,actor ',key ,@args))
(defmacro reply (key &rest args)
  `(*reply ',key ,@args))
