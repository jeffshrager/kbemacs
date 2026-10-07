;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>EDIT-SIMPLE.LISP.5, 3 April 1984 15:58:30
;;; PDF pages 182-183 (printed source pages 1-2).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; EDIT-SIMPLE: Editor commands intended for use in the KBE simple pane.
(defvar *old-submit-requests*     '() "KBE request history list.")
(defvar *old-submit-request-ring* nil "KBE request history ring.")
;;; Abort (in KBE Simple Frame)         Kill KBE Requests
;;; Kills the text in the buffer without submitting it.
(define-editor-command com-kill-KBE-requests (ignore) ()
  "Kills text in buffer (aborts submitting it)."
  (delete-from-buffer-between (buffer-start) (buffer-end) t))
;;; End (in KBE.Simple Frame)           Submit And Kill KBE Requests

;;; Submits the text in the buffer to the KBE, then kills that text.
(define-editor-command com-submit-and-kill-KBE-requests (ignore) ()
  "Submits text in buffer."
  (let ((buffer (current-editor-buffer)))
    (let ((start (buffer-start buffer))
          (end   (buffer-end buffer)))
      (let ((n (submit-stream-contents-to-KBE
                (interval-stream start  end))))
        (remark (text "~D request~:P accepted."  n))
        (push (copy-from-buffer-between  start end)
              *old-submit-requests*)
        (delete-from-buffer-between start end  t))))
  t)  ;buffer changed
 ;;; Control-Meta-Y (in KBE Simple Frame)        Yank Pop KBE Request
 ;;; Inserts the text of  the (nth) most-recent KBE request, killing
 ;;; previously yanked  text if. any.
(define-editor-command com-yank-pop-KBE-request (arg) ()
  "Inserts  the text of the (nth) most-recent KBE request, killing
previously yanked  text if any."
  (let ((len (length *old-submit-requests*) )
        (already-yanking? (eq (last-editor-command-type) 'yank)))
    (set-editor-command-type 'yank)
    (when arg
      (cond ((or (minusp arg) (> arg len))
             (editor-barf "Argument out of range."))
            ((zerop arg)
             (editor-barf "I can't yet handle an argument of zero."))))
    (when (and (or arg (not already-yanking?))
               *old-submit-requests*)
      (setq *old-submit-request-ring*
            (circular-list* *old-submit-requests*)))
    (when already-yanking?
      (delete-from-buffer-between (editor-mark) (editor-point)))
    (dotimes (n (1- (or arg 1))) ;pop specified number of nodes
      (pop *old-submit-request-ring*))
    (move-mark (editor-point))
    (move-point (yank-into-buffer-at-point
                 (pop *old-submit-request-ring*))))

  t) ;buffer might have changed


