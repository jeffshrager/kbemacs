;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>KAOS.LISP.27, 16 January 1984 02:27:17
;;; PDF pages 11-15 (printed source pages 1-5).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; KAOS: A package for communicating via the Chaos net.
;;;
;;; The general shape of this code was originally patterned after
;;; code written by CWH at Symbolics in the file
;;;   PTR:>cwh>hacks>hostls.lisp
;;; Adapted for use with the KBE by KMP, 20-Dec-82.

(defvar *kaos-connect-name* "KAOS"
  "Connect name for use by the KAOS protocol.")

(defvar *kaos-syntax-environment*
  `((PACKAGE ',(pkg-find-package "USER"))
    (PRINLEVEL NIL)
    (PRINLENGTH NIL)
    (PRINENDLINE NIL)
    (IBASE 10.)
    (BASE 10.)
    (*NOPOINT NIL))
  "Bindings that should be in effect when KAOS is doing I/O")

(defmacro kaos-handler (x) `(get ,x 'kaos-handler))

(defdefmacro define-kaos-handler "KBE Chaos handler" (name bvl . body)
             `(defun (:property ,name kaos-handler) ,bvl
               ,@body))

(defun kaos-server ()
  (ignore-errors
    (let ((conn (chaos:listen *kaos-connect-name*)))
      (chaos:accept conn)
      (with-open-stream (stream (chaos:make-stream conn
                                                   ':direction
                                                   ':bidirectional))
        (multiple-value-bind (message-name arglist)
            (progw *kaos-syntax-environment*
                   (values (read stream) (read stream)))
          (send stream ':finish)
          (let ((result (call-kaos-handler message-name arglist)))
            (progw *kaos-syntax-environment*
                   (format stream "~S" result)))
          (send stream ':force-output) ; Get the bits out on the net
          (send stream ':finish) ; Keep retransmitting until
                                        ; the other side has them
          (send stream ':close ':abort)))))) ; Close without sending an EOF

(define-internal-option *kaos-debug*
    "Debug KBE network errors"
  nil :boolean)

(defun handle-kaos-error (cond)
  (catch-error-restart ((sys:error sys:abort)
                        "Continue processing this error.")
                       (if *kaos-debug* (funcall #'dbg))) ;maybe run the debugger
  (*throw 'kaos-handler-error-return (format nil "~A" cond)))

(defun call-kaos-handler (message-name arglist)
  (let ((handler (kaos-handler message-name)))
    (cond ((not handler)
           (format nil "No handler for the ~S message." message-name))
          (t
           (*catch 'kaos-handler-error-return
                   (condition-bind ((sys:error #'handle-kaos-error))
                                   (multiple-value-list
                                    (lexpr-funcall handler arglist))))))))

(add-initialization
 *kaos-connect-name*
 '(process-run-temporary-function "Kaos Server" 'kaos-server)
 nil
 'chaos:server-alist)

(defun kaos-send-returning-list (host-name message-name &rest args)
  (let ((host (cond ((null host-name) si:local-host)
                    ((stringp host-name) (si:parse-host host-name))
                    (t host-name))))
    (progw *kaos-syntax-environment*
           (with-open-stream (stream
                              (chaos:open-stream host *kaos-connect-name*
                                                 ':direction ':bidirectional))
             (format stream "~S ~S" message-name args)
             (send stream ':finish)
             (prog1 (read stream)
               ;; We're done, so close without sending an EOF ...
               (send stream ':close ':abort))))))

(defun kaos-send (host-name message-name &rest args)
  (let ((result (lexpr-funcall #'kaos-send-returning-list
                               host-name message-name args)))
    (cond ((stringp result)
           (barf "~A" result))
          (t (values-list result)))))

(defun kaos-lexpr-send (&rest host-message-and-lexpr-args)
  (lexpr-funcall #'lexpr-funcall #'kaos-send host-message-and-lexpr-args))

;;;; Debugging Tools

(comment ;This stuff needn't be dumped with the main system...

  ;;; This page constructs a KAOS user window for debugging purposes.

 (defvar *user-window*
   (tv:make-window 'tv:window
                   ':edges (multiple-value-bind (width height)
                               (send tv:mouse-sheet ':size)
                             height ;ignored
                             `(0 0. ,width 100.))
                   ':label "KAOS User Window"
                   ':save-bits t)
   "KAOS user debugging window")

                                        ; Note: This server test program works only for one-arg handlers.

 (defun kaos-server-test (&optional host-name (terminal-io *user-window*))
   (unwind-protect
        (progn (send terminal-io ':expose)
               (do ((message-name (kaos-prompt-for "Message Type")
                                  (kaos-prompt-for "Message Type")))
                   ((null message-name)
                    (send terminal-io ':bury))
                 (let ((data (kaos-prompt-for "Message Data")))
                   (let ((result
                          (kaos-send-returning-list host-name
                                                    message-name
                                                    data)))
                     (cond ((stringp result)
                            (format t "~&Failed: ~A~%" result))
                           (t
                            (format t "~{~&^Y ~S~}~%" result)))))))
     (send *user-window* ':bury)))

 ) ;tnemmoc

(comment ;This stuff needn't be dumped with the main system...

  ;;; This page constructs some sample KAOS servers for
  ;;; debugging purposes.

 (defvar *server-window*
   (tv:make-window 'tv:window
                   ':edges (multiple-value-bind (width height)
                               (send tv:mouse-sheet ':size)
                             height ;ignored
                             `(0 100. ,width 200.))
                   ':label "Odd Eval Server Window"
                   ':save-bits t)
   "Server window")

 (defvar *kaos-prompt* "")

 (defun kaos-prompt (&rest ignore)
   (format t "~A: " *kaos-prompt*))

 (defun kaos-prompt-for (*kaos-prompt*)
   (send terminal-io ':rubout-handler '((:prompt kaos-prompt)) #'read))

 (define-kaos-handler :EVAL (form)
   (eval form))

 (define-kaos-handler :FUNNY-EVAL (form)
   (let ((terminal-io *server-window*))
     (send terminal-io ':select)
     (unwind-protect (eval (kaos-prompt-for
                            (format nil "Return value for ~S" form)))
       (send *server-window* ':bury))))

 ) ;tnemmoc
