;;; -*- Mode:LISP; Package:KBE; Base:10.; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>DOC.LISP.24, 18 March 1984 17:55:10
;;; Knowledge-Based_Editor_Jan-1985_volume_4.pdf, PDF pages 205-206.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; DOC: Automatic documentation for the KBE

(define-internal-option *documentation-pathname-default*
    "Self-documentation pathname default"
  "KBE: DOC; ^W TEXT >" :sexp)

(defun document-KBE-to-file (&optional file)
  (let ((file
         (or file
             (send (fs:parse-pathname *documentation-pathname-default*)
                   ':new-pathname
                   ':name (kbe-version-string-for-pathname)))))
    (with-open-file (stream file ':out)
      (format stream "-*- Mode:Text; Fonts:CPTFONT; -*-~%")
      (document-KBE-to-stream stream t)
      file)))

(defun document-KBE-to-stream (&optional stream print-dictionary?)
  (let ((output-stream (or stream standard-output)))
    (format output-stream "~%Documentation for ~A, produced ~\datime\~%"
            (system-version-string "KBE"))
    (document-KBE output-stream print-dictionary?)))

(defun document-KBE (&optional stream print-dictionary?)
  (let ((standard-output (or stream standard-output)))
    (format t
            "~%The Knowledge-Based-Editor (KBE) is an extension to the~
             ~%normal editing environment developed by the Programmer's~
             ~%Apprentice group at MIT.~
             ~%~
             ~%Interaction with the KBE may occur in different ways:~
             ~%~
             ~%  Some commands are directly available as normal editor~
             ~%  commands which can be invoked by a single keystroke.~
             ~%~
             ~%  Some commands are extended text commands, which can be~
             ~%  done either immediately (an execute request), or in~
             ~%  background (a submit request).~
             ~%~
             ~%Commands related to immediate action tend to be named~
             ~%Super-something. Commands which relate to the background~
             ~%KBE's background server process tend to be called~
             ~%Hyper-something.~
             ~%~
             ~%In particular,...
             ~%  ~A requests immediate execution of a text request~
             ~%  ~A submits a request without waiting for it to be~
             ~%     executed. The status of such requests can be checked~
             ~%     with ~A, and their results can later be~
             ~%     picked up with ~A.~
             ~%~
             ~%Here is a full list of KBE-related Editor commands...~%"
            (simple-command-key t)
            (simple-command-key nil)
            (command-key-name 'com-KBE-status)
            (command-key-name 'com-resume-KBE-edits))
    (document-editor-commands)
    (when print-dictionary?
      (format t "~%~/|~2%")
      (print-dictionary ':group)
      (format t "~%~/|~2%"))
    (format t "~2&Send bugs, queries, etc. to ~A.~%"
            *bug-mail-address*)))

(define-editor-command com-Document-KBE (arg)
    (#\Super-Control-Help #\Hyper-Control-Help)
  "Gives help information about KBE commands.
With an arg, shows KBE dictionary information instead."
  (cond ((not arg) (document-KBE nil))
        (t (print-dictionary ':group)))
  nil) ;no changes to buffer
