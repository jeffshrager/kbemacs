;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 96-97; listing noise.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; The KBE will want to be able to generate noises for various different
;;; events to alert the user to things that he might want to do. This
;;; section provides some tools so that users dis-satisfied with the
;;; noise they get can pick another...
;;;
;;; (INIT-NOISE-FOR name &rest args-info)
;;;
;;;  Sets up a noise given by noise named NAME. The args-info arguments
;;;  should be strings which describe each of the args the given noise
;;;  desires.
;;;
;;; (SET-NOISE-FOR name [function])
;;;
;;;  Sets the noise-making function for a particular named noise.
;;;  Noise-making functions are functions of any number of arguments.
;;;  The first argument will be the noise NAME. The remaining arguments,
;;;  if any, are specific to that kind of noise.
;;;
;;;  If NAME is NIL, sets the default noise making function.
;;;
;;;  If no function is supplied, the default function will be used.
;;;  If the supplied function is NIL, the noise is ignored.
;;;
;;; (MAKE-NOISE-ABOUT name &rest other-args)
;;;
;;;  Provides an abstraction for ringing the bell in various interesting
;;;  ways. In fact, the bell needn't be rung at all if the user wants
;;;  to define handlers that do something completely different with
;;;  the notifications. The noise name must have been previously defined
;;;  by an INIT-NOISE-FOR form.
;;;
;;; (LIST-NOISES)
;;;
;;;  Displays information about the currently defined kinds of KBE
;;;  noises in table format.

(defun default-noise (&optional name &rest name-specific-data)
  name name-specific-data ;ignored
  (beep))

(defvar *noises* (ncons #'default-noise))

(defun noise-for (thing)
  (do ((l (cdr *noises*) (cdr l)))
      ((null l) (ferror nil "Undefined noise ~S" thing))
    (cond ((eq (caar l) thing)
           (return (cond ((eq (cadar l) 't)
                          (car *noises*))
                         (t
                          (cadar l))))))))

(defun make-noise-about (thing &rest other-args)
  (let ((handler (noise-for thing)))
    (cond (handler (lexpr-funcall handler thing other-args)))))

(defun set-noise-for (name &optional (handler 't handler?))
  (cond ((null name)
         (cond ((not handler?)
                (ferror nil "Default noise handler may not be defaulted.")))
         (setf (car *noises*) handler))
        (t
         (let ((slot (or (assq name (cdr *noises*))
                         (ferror nil "Noise ~S is not defined." name))))
           (setf (cadr slot) handler))))
  handler)

(defun init-noise-for (name &rest args-info)
  (let ((old-handler (assq name (cdr *noises*))))
    (cond ((not old-handler)
           (push (list* name t (copylist args-info)) (cdr *noises*))
           t)
          ((equal (cddr old-handler) args-info)
           nil)
          (t
           (ferror nil "Conflicting noise initialization for ~S."
                   name args-info)))))

(defun list-noises ()
  (cond ((not *noises*)
         (format t "~&No noises defined."))
        ((cdr *noises*)
         (let* ((package (pkg-find-package "GLOBAL")) ;display package info
                (flag nil)
                (column  (+ (apply #'max
                                   (mapcar #'(lambda (x)
                                               (string-length
                                                 (text "~S" (car x))))
                                           (cdr *noises*)))
                            3)))
           (format t "~&Name~VTHandler Function~%" column)
           (dolist (slot (cdr *noises*))
             (caseq (cadr slot)
               ((NIL) (format t "~&~S~VTNo handler.~%"
                              (car slot) column)
                      (setq flag t))
               ((T)   (format t "~&~S~VTDefault handler.~%"
                              (car slot) column))
               (T     (format t "~&~S~VT~S~%"
                              (car slot) column (cadr slot))))
             (cond ((cddr slot)
                    (do ((args (cddr slot) (cdr args))
                         (i 1 (1+ i)))
                        ((null args))
                      (format t "~&~VT Arg ~D: ~A~%"
                              column i (string-capitalize-words
                                         (car args)))))))
           (format t "~&Arg 0 for all handlers is the noise name.~@[~*~
                      ~%Noises without handlers ignored.~]~%"
                   flag))))
  (cond ((car *noises*)
         (format t "~&The default handler is ~S.~%"
                 (car *noises*))))
  t)
