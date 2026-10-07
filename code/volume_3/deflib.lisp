;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; PDF pages 227-232 (printed source pages 1-6).
;;;
;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; DEFLIB: The definition of defining forms DEFPROTOTYPE and DEFINE-CLICHE,
;;;         upon which SYSLIB, KBELIB, and DEMLIB depend.

;some general comments on cliches
;1. it is good for every cliche to have a return value, but for this not to
;be an explicit slot since it is easy to refer to anyway.
;2. By convention essentially every cliche is a simple segment.
;  it cannot be an ssr, ssrbody, split, or join.

;;; this allows you to create a cliche by giving pseudo code for it.
;;; note that the stuff in flows only creates slot assertions.
;;; this is the only place where the cliche assertion is made.
;;; you just specify what that should be yourself.
;;; Note that in order to run this macro you have to have the whole
;;; system up and going.

;Note that a naked defprototype is only used when you
;cannot use define-cliche for example if you need to create
;a naked pred or ssr, or need to have either one as a part.
;The funny little guys used by the system need this,
;(they are all in this file)
;but I don't think any of the normal ones do.

;Note that this cleans out the DB after creating the cliche
;if the DB was empty to start with.  This
;helps keep things fast and doesn't leave any garbage about.

(defmacro define-cliche (name comments &body body)
  (let ((default-cons-area working-storage-area)) ;Avoid screw related to
                                                  ;compiler temporary area
    (condition-case-if (not *debug*) (cond)
        `(local-declare ()
           ;; This really shouldn't work since it will create
           ;; DEFINE-CLICHE style source file info, but we later
           ;; say that DEFINE-CLICHE really creates DEFUN stuff.
           ;; It "works" because ZWEI's sectionization stuff is
           ;; such a mess that it finds the "wrong" definition...
           (record-source-file-name ',name 'define-cliche)
           ;; Now do main expansion...
           ,(expand-define-cliche name comments
              `(define-cliche ,name ,comments ., body)))
      (sys:error (grumble "The cliche (~A) failed to be defined." name)))))

(define-editor-definition-type DEFINE-CLICHE DEFUN)

(defmacro def-ada-cliche (name comments &body body)
  (let ((default-cons-area working-storage-area)) ;Avoid screw related to
                                                  ;compiler temporary area
    (condition-case-if (not *debug*) (cond)
        `(local-declare ()
           ;; This really shouldn't work since it will create
           ;; DEF-ADA-CLICHE style source file info, but we later
           ;; say that DEFINE-CLICHE really creates DEFUN stuff.
           ;; It "works" because ZWEI's sectionization stuff is
           ;; such a mess that it finds the "wrong" definition...
           (record-source-file-name ',name 'def-ada-top)
           ;; Now do main expansion...
           ,(expand-define-cliche name comments
              `(def-ada-top ,name nil nil
                 (def-ada-cliche ,name ,comments
                   ., body))))
      (sys:error (grumble "The cliche (~A) failed to be defined." name)))))

(define-editor-definition-type DEF-ADA-TOP DEFUN)

;note the way this converts the dffn into an instance
(defun expand-define-cliche (name comments body)
  ;must do this here because normal reader read this.
  (fix-up-semi-kbe-/#-comment-macros body)
  (let* ((empty-db (null *db-segs*))
         (segcase (create-plan-for-code body))
         (slot-names (get (locf comments) 'roles)))
    (-& segcase cliche-def)
    (--& segcase cliche)
    (+& segcase cliche name)
    (if (null slot-names)
        (setq comments (get-cliche-roles segcase comments)))
    (setq comments (get-cliche-role-tree segcase comments))
    (prog1 `(defprototype ,name ,comments
              ,(construct-plan-def (~seg segcase)))
           (if empty-db (db-init)))))

(defun get-cliche-roles (segcase comments)
  (let* ((primary-roles (get (locf comments) 'primary-roles))
         (actual-roles (~set ~slotname (??& segcase slot)))
         (roles (append primary-roles (qset- actual-roles primary-roles))))
    `(roles ,roles  .,comments)))

(defun get-cliche-role-tree (segcase comments)
  (let* ((roles (get (locf comments) 'roles))
         (role-tree))
    (dolist (role roles)
      (let ((slotseg (~slotseg (?& segcase slot * role))))
        (if (??& slotseg cliche 'pseudo-subcliche)
            (push (cons role (~set ~slotname (??& slotseg slot)))
                  role-tree)
            (push (ncons role) role-tree))))
    `(role-tree ,(nreverse role-tree) ., comments)))

(pp:define-list-print-function define-cliche (list s)
  (format s "(~@!~4I~=~^ ~@_~=~^~%~
                  (~!~@{~\kbe:define-cliche-plist-entry\~^~%~}~.)~
                ~2I~{~%~=~^~}~.)"
          (car list) (cadr list) (pairup (caddr list)) (cdddr list)))

(defun pairup (list)
  (do ((l list (cddr l))
       (result nil (cons (list (car l) (cadr l)) result)))
      ((null l) (nreverse result))))

(pp:define-printing-directive define-cliche-plist-entry (entry s)
  (let ((prop (car entry)) (value (cadr entry)))
    (caseq prop
      ((primary-roles described-roles roles role-tree)
       (format s "~S ~@_~1\fill\" prop value))
      (constraints (format s "~@!~2I~S~_~1\linear\~." prop value))
      (comment
        (format s "~S /"~@!~{~A~^ ~:_~}~./""
                prop (decompose-cliche-comment value)))
      (T (format s "~S ~@_~=" prop value)))))

(defun decompose-cliche-comment (string)
  (if (not (stringp string)) (ncons string)
      (let ((*kbe-role-marker* "{"))
        (mapcar #'(lambda (word)
                    (if (symbolp word)
                        (string-downcase (string word))
                        (format nil "~:=" word)))
                (fix-up-semi-kbe-/#-comment-macros
                  (read-from-string
                    (string-append "(" string ")")))))))

;this is used to define cliches in the library.
;this does a bunch of things to interface with the editor
;as well as define the cliche.  Note that the CLICHE and SLOT assertions
;must be in the plan.  The cliches are not actually put into the
;data base but are rather put on property lists.  The plan-def
;goes on the CLICHE-PLAN property and the comments go on the
;CLICHE-COMMENTS property.

;each call on defprototype has the following form:
;(defprototype CLICHE-NAME       ;same as below
;              COMMENTS          ;general comments (the cdr of a plist)
;  (plan-def SEG-NAME            ;the name of the segment for the cliche
;    code               ;for everything to work right there must be code
;    (deffacts SEG-NAME
;      (cliche (CLICHE-NAME))    ;the internal name of the cliche (one word)
;      ...)
;    ...))
;a lower case version of the cliche-name and each slot name is used
;externally

;for example

;(defprototype TRAILING-POINTER-LIST-ENUMERATION
;              (roles (LIST CURRENT PREVIOUS)
;               MAIN "trailing pointer list enumeration loop.")
;  (plan-def TPLE-LOOP
;    code
;    (deffacts TPLE-LOOP
;      (cliche (TRAILING-POINTER-LIST-ENUMERATION))
;      ...)
;    ...))
;the external names are: trailing-pointer-list-enumeration,
;list, current, and previous.

;note that one of the primary pieces of work this does is
;set up the name of the cliche and the names of the roles as things
;you can refer to in a command.

(defmacro defprototype (&body body)
  (let* (((cliche-name comments plan) body)
         (role-tree (get (locf comments) 'role-tree)))
    `(progn 'compile
            (if (get ',cliche-name 'cliche-plan)
                (grumble "Redefining cliche ~A" ',cliche-name))
            (*define-cliche ',cliche-name)
            (mapc #'(lambda (l)
                      (mapc #'*define-role l))
                  ',role-tree)
            (putprop ',cliche-name ',plan 'cliche-plan)
            (putprop ',cliche-name (cons nil ',comments) 'cliche-comments)
            ',cliche-name)))

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; End:;
