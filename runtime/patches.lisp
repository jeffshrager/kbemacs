;;; -*- Mode: LISP; Package: KBE -*-
;;;
;;; Headless stand-ins for parts of the KBE that live in the actor and
;;; window layers (volume 4), which this runtime does not load. Loaded
;;; with KBE-LOAD after the core, so it reads like a KBE source file.

;;; The delegate. KBE verbs dispatch with (SEND-DELEGATE ':INTERPRET verb
;;; ...), and DEFINE-DB-ACTION puts them on PROGRAMMER-VOCABULARY
;;; (refer.lisp). On the Lisp Machine *DELEGATE* is an actor
;;; (standard-actor.lisp, editing actors); headless, a flavor with just
;;; the vocabularies is enough.
(defflavor headless-delegate () (programmer-vocabulary default-vocabulary))

(define-system-initialization default-delegate
  (setq *delegate* (make-instance 'headless-delegate)))

;;; From volume_4/parse.lisp (the editor's parser, not loaded here): the
;;; language programs are written in. The coder and surface analyzer
;;; consult it.
(define-KBE-option *input-language*
    "Input language for use by KBE"
  'LISP :assoc '(("Lisp"  . LISP)
                 ("PL//I" . PLI)
                 ("Ada"   . ADA)))

;;; From volume_4/parse.lisp (not loaded: it is mostly an operator
;;; precedence parser for Algol, PL/I and Ada input). Only its Lisp path
;;; is reproduced. For LISP every character's syntax is READ-HACKING-/{
;;; (def-syntax LISP ((0 . 255) #'read-hacking-/{) ...), so parsing a
;;; code string reads one Lisp form, with {...} comment syntax.
(defun parse-error (&rest stuff)
  (lexpr-funcall #'complain stuff))

(defprop parse-error t :error-reporter)

(defun string-parser (string &rest ignore)
  (with-input-from-string (stream string)
    (read-hacking-/{ stream)))
