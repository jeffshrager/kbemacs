;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>BELIEF.LISP.21, 21 May 1984 18:43:28
;;; PDF pages 137-141 (printed source pages 1-5).

;;; (c) Copyright 1984, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; BELIEF: Belief structure for editing actors, etc.
;;; BELIEF-STRUCTURE
;;; A collection of beliefs.
(defflavor belief-structure (storage lock-cell) (name-mixin)
           :initable-instance-variables
           :settable-instance-variables
           :gettable-instance-variables
           (:default-init-plist
               :name   "Anonymous"
             :storage (ncons nil)
             :lock-cell nil))
(defmethod (belief-structure  :reset) ()
  (with-lock  (lock-cell :whostate "Await Beliefs")
    (setq storage (ncons nil))))
(defmethod  (belief-structure :believe) (belief-type belief-subject val)
  (with-lock (lock-cell :whostate "Await Beliefs")
    (let ((subject-cell (get storage belief-subject)))
      (when (not subject-cell)
        (setq subject-cell (ncons belief-subject))
        (setf (get storage belief-subject) subject-cell))
      (setf (get subject-cell belief-type) val)
      val)))
(defmethod (belief-structure :belief) (belief-type belief-subject)
  (with-lock (lock-cell :whostate "Await Beliefs")
    (get (get storage belief-subject) belief-type)))
(defmethod (belief-structure :disbelieve) (belief-type belief-subject)
  (with-lock (lock-cell :whostate "Await Beliefs")
    (let ((subject-cell (get storage belief-subject)))
      (remprop subject-cell belief-type)
      (when (not (cdr subject-cell))
        (and (remprop storage belief-subject))))
    nil))
(defmethod (belief-structure :map-over-beliefs) (fn)
  (with-lock (lock-cell :whostate "Await Beliefs")
    (do ((b (cdr storage) (cddr b)))
        ((null b))
      (do ((l (cdadr b) (cddr l)))
          ((null l))
        (funcall fn (car l) (car b) (cadr l))))))
(defmethod (belief-structure :map-over-beliefs-about) (belief-subject fn)
  (with-lock (lock-cell :whostate "Await Beliefs")
    (do ((l (cdr (get storage belief-subject)) (cddr l)))
        ((null l))
      (funcall fn (car l) (cadr l)))))
(defmethod (belief-structure :map-over-beliefs-of-type) (belief-type fn)
  (with-lock (lock-cell :whostate "Await Beliefs")
    (do ((b (cdr storage) (cddr b)))
        ((null b))

      (do ((l (cdadr b) (cddr l)))
          ((null l))
        (when (eq (car l) belief-type)
          (funcall fn (car b) (cadr l)))))))


;;; Initialization
;;; Request compile-time compilation of flavors. By default, the
;;; methods would not get compiled until fasload-time otherwise.
(compile-flavor-methods belief-structure)


;;; Believer Mixin
(defvar *beliefs* (make-instance 'belief-structure ':name "Main"))
(defflavor believer-mixin (beliefs) ()
           :initable-instance-variables
           :settable-instance-variables
           :gettable-instance-variables
           (:default-init-plist
               :beliefs *beliefs*))
(defmethod (believer-mixin :believe) (belief-type belief-subject val)
  (send beliefs ':believe belief-type belief-subject val))
(defmethod (believer-mixin :belief) (belief-type belief-subject)
  (send beliefs ':belief belief-type belief-subject))
(defmethod (believer-mixin :disbelieve) (belief-type belief-subject)
  (send beliefs ':disbelieve belief-type belief-subject))
(defmethod (believer-mixin :map-over-beliefs-about) (belief-subject fn)
  (send beliefs ':map-over-beliefs-about belief-subject fn))
(defmethod (believer-mixin :map-over-beliefs-of-type)  (belief-type fn)
  (send beliefs ':map-over-beliefs-of-type belief-type fn))
(defmethod (believer-mixin  :map-over-beliefs) (fn)
  (send beliefs ':map-over-beliefs fn))
(define-behavior (believer-mixin belief)  (&rest args)
  (return (lexpr-send self  ':belief args)))
(define-behavior (believer-mixin believe) (&rest args)
  (lexpr-send self ':believe args))

 ;;; Local Modes:;
 ;;; Mode:LISP;
 ;;; Lisp WHEN Indent:1;
 ;;; Lisp UNLESS Indent:1;
 ;;; Lisp WITH-LOCK Indent:1;
 ;;; End:;
