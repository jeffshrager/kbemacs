;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>EDIT-MISC.LISP
;;; PDF pages 177-181 (printed source pages 1-5).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; EDIT-MISC: Miscellaneous KBE commands which do not
;;; work by submitting TASK requests to background.

;;; Option Variables
(define-editor-command com-Edit-KBE-Options (full?) ()
  "Edit a table of KBE-related control variables.
With an argument, edit KBE internal variables as well."
  (edit-KBE-options (or full? *debug*)))

;;; Bug Reporting
(define-editor-command com-Bug-KBE (ignore) ()
  "Enter a buffer which is set up to send a KBE bug report."
  (let ((oddities (odd-KBE-options)))
    (send-mail *bug-mail-address*
               (text "In ~A on ~A,~%using ~A in ~A~@[, ~A~]~
                        ~@[~* with special option~P:~2:*~
                        ~{~%* ~A~}~] ...~2%"
                     (loaded-band-string)
                     (site-string)
                     (system-version-string "KBE")
                     (system-version-string ':release)
                     (cond ((not (released-system-version-active? "System"))
                            (system-version-string "System")))
                     oddities (length oddities))))
  nil) ;no changes to this buffer

;;; Hyper-Control-Y / Super-Control-Y: Yank KBE Bug Message
(define-editor-command com-yank-KBE-bug-message (arg)
    (#\Super-Control-Y #\Hyper-Control-Y)
  "Inserts the text of the (nth) most-recent KBE bug message"
  (insert-into-buffer-before-point (display-bug-message arg nil))
  t) ;need to redisplay buffer since we probably modified it.

;;; Cursor Motion
;;; Moves to the (nth) next (or with arg, previous) visible role,
;;; by scanning for KBE role markers. This is a completely textual operation.
(defun move-to-next-KBE-role (arg &optional (rev? nil))
  (declare (special *kbe-user-role-marker*))
  (if (not arg) (setq arg 1))
  (if rev? (setq arg (- arg)))
  (let ((pt (search-buffer *kbe-user-role-marker*
                           (if (plusp arg)
                               (forward-char (editor-point))
                               (editor-point))
                           arg)))
    (cond (pt
           (if (plusp arg)
               (setq pt (backward-char pt
                                       (string-length *kbe-user-role-marker*))))
           (move-point pt))
          (t (editor-barf "No role~A"
                          (cond ((minusp arg)
                                 (format nil "~@[ ~R~] before this."
                                         (if (< arg -1) (minus arg))))
                                ((zerop arg) "to search for?")
                                (t (format nil "~@[ ~R~] after this."
                                           (if (> arg 1) arg)))))))))

;;; Super-F / Super-Control-F: Forward Marked KBE Role
(define-editor-command com-forward-marked-KBE-role (arg)
    (#\Super-F #\Super-Control-F)
  "Goes forward one KBE role. Searches forward for a KBE role marker.
With an argument, N, goes forward N roles.
The argument may be negative (to search backward)."
  (move-to-next-KBE-role arg)
  nil) ;no changes to buffer

;;; Super-B / Super-Control-B: Backward Marked KBE Role
(define-editor-command com-backward-marked-KBE-role (arg)
    (#\Super-B #\Super-Control-B)
  "Goes back one KBE role. Searches backward for a KBE role marker.
With an argument, N, goes backward N roles.
The argument may be negative (to search forward)."
  (move-to-next-KBE-role arg t)
  nil) ;no changes to buffer

;;; Utilities for editing Pseudo-English
;;; Super-Rubout: Trim KBE Reference
;;; Looks backward for the most recent definite reference and,
;;; if it's of the form "the ... of the ... of the ...",
;;; removes the leading "the ... of" (or arg leading ones, if
;;; arg is given) from that phrase. eg, s-2 s-Rubout will change
;;; "the w of the x of the y of the z" into "the y of the z".
(define-editor-command com-trim-KBE-reference (arg) (#\super-rubout)
  "Delete the first /"the ... of/" in most recent definite reference.
eg, changes /"the x of the y/" to just /"the y/"."
  (let ((mod? nil))
    (dotimes (i (or arg 1))
      (setq mod? (or mod? (trim-KBE-reference))))
    (cond (mod?)
          (t (editor-barf "Can't find reference.")))))

(defun trim-KBE-reference ()
  (do ((the-pt (search-buffer-backward-delimited "the" (editor-point))
               (search-buffer-backward-delimited "the" pt))
       (pt nil))
      ((not the-pt) nil)
    (let ((pt1 (backward-word the-pt)))
      (let ((pt2 (if pt1 (forward-word pt1))))
        (cond ((if pt1 (string-equal (string-buffer-between pt1 pt2) "of"))
               (setq pt pt1))
              (pt (return (trim-KBE-reference-internal pt)))
              (t (return nil)))))))

(defun trim-KBE-reference-internal (of-pt)
  (when of-pt
    (let ((the-pt (search-buffer-backward-delimited "the" of-pt)))
      (with-local-buffer-pointer
          (end-pt (copy-buffer-pointer (forward-char of-pt 2) ':moves))
        (delete-from-buffer-between the-pt end-pt nil)
        (delete-chars-in-set '(#\space #\tab) end-pt)
        (insert-into-buffer-before end-pt " ")
        t))))
