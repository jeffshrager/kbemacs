;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 152-159; listing mbox.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; MBOX: A mailbox implementation to support "real" message
;;;       passing between actors in the KBE.
;;; PDF page 153
;;; Message
;;; A message has the following components:
;;; DATE: Date the message was 'posted' (a universal time).
;;; FROM: Sender's address (a string).
;;; TO: Recipient's address (a string).
;;; ID: An ID to reply to (a number).
;;; RE: A string for self-documentation (or NIL).
;;; TYPE: The type of this transaction (a symbol).
;;; IN-REPLY-TO: An ID that this is a reply to (or NIL if none).
;;; DATA: Any additional data needed for this transaction.
;;; NOTIFY-UPON-TERMINATION: True if sender wants to know
;;;                         when this has been fully processed.
(defflavor message ((date nil) (from nil) (to nil) (re nil) (id nil)
                   (type nil) (in-reply-to nil) (data nil)
                   (notify-upon-termination nil))
           ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)
(defmethod (message :after :init) (&rest ignore)
  (if (not date) (setq date (time:get-universal-time)))
  (if (not from) (ferror "No sender for ~S" self))
  (if (not to) (ferror "No recipient for ~S" self))
  (if (not type) (ferror "No type for ~S" self)))
(defun make-message (message-spec)
  (lexpr-funcall #'make-instance 'message (cdr message-spec)))
(defmethod (message :print-self) (stream level prin1? &rest ignore)
  level ;ignored
  (cond (prin1?
         (let ((package (pkg-find-package "")))
           (format stream "~%:(Message~@[ date ~S~]~@[ from ~S~]~
                                     ~@[ to ~S~]~@[ re ~S~]~@[ id ~S~]~
                                     ~@[ type ~S~]~@[ in-reply-to ~S~]~
                                     ~@[ data ~S~]~
                                     ~@[ notify-upon-termination ~S~])"
                   date from to re id type in-reply-to data
                   notify-upon-termination)))
        (t
         (format stream "{Message ~A^Y~A(~D)~@[ Re: ~A~]}"
                 from to id re))))
(defmethod (message :display) (&optional (stream standard-output))
  (format stream "~@[~
       ~&Date: ~\time\~]~@[~
       ~%From: ~A~]~@[~
       ~%To:   ~A~]~@[~
       ~%Re:   ~A~]~@[~
       ~%ID:   ~A~]~@[~
       ~%Type: ~A~]~@[~
       ~%Notify-Upon-Termination: ~A~]~@[~
       ~%In-Reply-To: Your message ~A~]~2%~{~A~%~}"
          date from to re id type notify-upon-termination in-reply-to data))
;;; PDF page 154
;;; Mailbox
;;; Internally, a mailbox has the following state variables, none of
;;; which should be accesssed directly externally except for debugging:
;;; TIMESTAMP: Used internally by :AWAIT to make noticing changes
;;;            to the mailbox more efficient.
;;; READ-LOCK: Used by methods which intend to "read" the mail.
;;; WRITE-LOCK: Used by methods which intend to modify the mailbox.
;;; STORAGE: The actual stuff stored by the mailbox.
(defflavor mailbox ((owner NIL) (xmailbox NIL) (timestamp 0)
                   (read-lock NIL) (write-lock NIL) (storage NIL))
           ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)
(defflavor xmailbox () (mailbox))
(defflavor inbox () (mailbox)
  (:default-init-plist :xmailbox t))
(defmethod (mailbox :after :init) (&rest ignore)
  (when (eq xmailbox t)
    (setq xmailbox (make-instance 'xmailbox ':owner self))))
;;; PDF page 155
;;; For debugging
(defmethod (mailbox :print-self) (stream level prin1? &rest ignore)
  level prin1? ;ignored
  (cond (prin1?
         (format stream "#<~:[Anonymous~;~:*~A's~] ~A ~@[(~A) ~]~O>"
                 owner (string-downcase (typep self))
                 (send self ':describe-storage) (%pointer self)))
        (owner
         (format stream "~A's ~A" owner (string-downcase (typep self))))
        (t
         (format stream "~A[~O]"
                 (string-downcase (typep self)) (%pointer self)))))
(defmethod (mailbox :describe-storage) ()
  (let ((len (length storage)))
    (cond ((zerop len) "no messages")
          (t (format nil "~D message~:P" len)))))
;;; PDF page 156
;;; Mailbox (exported methods)
;;; Externally, the only defined operations are:
;;; :INSERT-MESSAGE message
;;;   Places MESSAGE in the mailbox's storage queue.
;;; :AWAIT-MESSAGE msg-id [from] [type 'ANSWER]
;;;   Awaits a message with a given MSG-ID.
;;;   The FROM may be NIL or an actor name. It is used only in
;;;   generating a whostate if the message is not already available.
;;;   The type (default RESULT) names the kind of result being expected.
;;; :NEXT-MESSAGE [timeout]
;;;   Reads the next message in the mailbox.
;;;   If timeout is not supplied, does not time out.
;;;   Otherwise, should be the number of 60ths of a second
;;;   until the wait should time out.
(defmethod (mailbox :insert-message) (msg)
  ;; No READ-LOCK needed to queue a message.
  (with-lock (write-lock :whostate "MBox Write Access")
    (incf timestamp)
    (setq storage (nconc storage (ncons msg)))
    (send msg ':id)))
(defmethod (mailbox :send-locked) (object key &rest args)
  (with-lock (read-lock :whostate "MBox Read Access")
    (lexpr-send object key args)))
;; This can only be called inside a :SEND-LOCKED contour
(defmethod (mailbox :await-message) (msg-id
                                     &optional from (type 'ANSWER)
                                     (process? t))
  (send self ':assure-read-lock)
  (do ((old-timestamp timestamp timestamp) ;May change asynchronously
       (state nil))
      (nil)
    (let ((message (send self ':extract-message msg-id type)))
      (when message
        (if process?
            (do ((m (send self ':extract-message msg-id)
                    (send self ':extract-message msg-id)))
                ((not m))
              (send owner ':process-message m))) ;process any other replies
        (return-list (send message ':data))))
    (let ((message (send self ':extract-message msg-id 'COMPLAINT)))
      (when message
        (if process?
            (do ((m (send self ':extract-message msg-id)
                    (send self ':extract-message msg-id)))
                ((not m)))) ;ignore any other replies
        (lexpr-funcall #'complain "~A" (send message ':data))))
    (process-wait (or state
                      (setq state
                            (cond ((not from) "Wait For Result")
                                  (t (format nil "Await ~A" from)))))
                  #'(lambda (loc val) (> (cdr loc) val))
                  (locf timestamp) old-timestamp)))
;; This can only be called inside a :SEND-LOCKED contour
(defmethod (mailbox :next-reply) (msg-id)
;;; PDF page 157
  (send self ':assure-read-lock)
  (with-lock (write-lock :whostate "MBox Write Access")
    (do ((p (locf storage) (cdr p))
         (s storage (cdr s)))
        ((null s) nil)
      (if (equal (send (car s) ':in-reply-to) msg-id)
          (return (prog1 (car s)
                         (incf timestamp)
                         (setf (cdr p) (cdr s))))))))
(comment ;tried this definition 4/26/84, but it failed.
         ;restored old definition 4/30/84 until this is
         ;better debugged -kmp
  (defmethod (mailbox :next-message) (&optional (wait? t) prefer-fn)
    (do () (nil) ;forever
      (with-lock (read-lock :whostate "MBox Read Access")
        (if wait? (process-wait "Await Message" #'cdr (locf storage)))
        (cond (storage
               (with-lock (write-lock :whostate "MBox Write Access")
                 (incf timestamp)
                 (return (or (if prefer-fn
                                 (dolist (msg storage)
                                   (when (funcall prefer-fn msg)
                                     (setq storage (delete msg storage))
                                     (return msg))))
                             (pop storage)))))
              ((not wait?) (return nil))
              (t
               (cbarf "Ignore ~A's problem and hope it's self correcting."
                      "Mailbox is inconsistent: ~S" self))))))
) ;tnemmoc
(defmethod (mailbox :next-message) (&optional (wait? t) prefer-fn)
  (do () (nil)
    (with-lock (read-lock :whostate "MBox Read Access")
      (if wait?
          (process-wait-with-timeout "Await Message" 60. #'cdr (locf storage)))
      (cond (storage
             (with-lock (write-lock :whostate "MBox Write Access")
               (incf timestamp)
               (return (or (if prefer-fn
                               (dolist (msg storage)
                                 (when (funcall prefer-fn msg)
                                   (setq storage (delete msg storage))
                                   (return msg))))
                           (pop storage)))))
            ((not wait?) (return nil))))
    (process-sleep 10. "Avoid Deadlock")))
;;; PDF page 158
;;; Mailbox (internal methods)
(defmethod (mailbox :assure-read-lock) ()
  (let ((lock read-lock))
    (if (neq lock current-process)
        (fsignal "Mailbox error: ~S had read lock on ~S." lock self))))
(defmethod (mailbox :find-message) (msg-id &optional type)
  (send self ':assure-read-lock)
  (dolist (message storage)
    (if (and (equal (send message ':in-reply-to) msg-id)
             (or (not type) (eq (send message ':type) type)))
        (return message))))
(defmethod (mailbox :extract-message) (msg-id &optional type)
  (send self ':assure-read-lock)
  (let ((message (send self ':find-message msg-id type)))
    (if message
        (with-lock (write-lock :whostate "MBox Write Access")
          (incf timestamp)
          (setq storage (delete message storage))
          message))))
;;; PDF page 159
;;; Request these flavors get fully compiled at compile time
;;; rather than on demand at runtime...
(compile-flavor-methods message mailbox xmailbox inbox)
