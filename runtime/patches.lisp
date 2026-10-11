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
