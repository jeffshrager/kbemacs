;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>KBE-MODE.LISP.3, 8 March 1984 19:31:42
;;; PDF pages 192-192 (printed source pages 1-1).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; KBE-MODE: Editor support for KBE Mode and KBE-style
;;;           command completion.
;;; Complete (when completing)          Complete KBE Word
(define-editor-command com-complete-KBE-word (ignore)
    (#\Super-Altmode #\Hyper-Altmode)
  "Complete current word"
  (complete-current-token *completion-table*)
  t) ;stuff in buffer may have been expanded
;;;; Auto Complete  KBE Word
;;; Space, Period, Comma, and Exclam  (when completing)
;;; Complete the current word and  self-insert the character
;;; used to invoke this command.
(define-editor-command com-auto-complete-KBE-word (ignore)
    (#\Super-Space #\Hyper-Space)
  "Complete and self-insert"
  (multiple-value-bind  (ignore start-bp)
      (bound-token)
    (editor-window-must-redisplay-rest-of-line
     (or start-bp  (copy-buffer-pointer (editor-point))))
    (when start-bp
      (insert-into-buffer-after-point #\space) ;make room
      (let ((pt (editor-point)))
        (unwind-protect
             (cond ((when-error
                     (and (not (backward-up-list pt)) ;toplevel?
                          (not (backward-up-list start-bp))
                          (not (breakchar-p (char-after (backward-sexp pt))))
                          (not (breakchar-p (char-before pt)))
                          (not (uppercase-p (char-before pt))))
                     nil)
                    (complete-current-token *completion-table* nil)
                    (multiple-value-bind (completion? msg n)
                        (get-current-token-completion *completion-table*)
                      (if (and (not completion?)
                               (> n 1))
                          (editor-barf msg)))))
          (delete-from-buffer-between pt (forward-char pt)))))
    (insert-into-buffer-before-point (editor-last-command-char))
    nil)) ;buffer redisplay attended to elsewhere above
 ;;; Help (when completing)
(define-editor-command com-document-KBE-completions (ignore)
    (#\super-Help #\hyper-Help)
  "Document what the completion would do at this buffer point."
  (document-completions *completion-table*)
  nil) ;no buffer mods
