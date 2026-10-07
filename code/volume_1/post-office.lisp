;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 160-161; listing post-office.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; POST-OFFICE: A post office implementation to coordinate message
;;;              delivery between actors in the KBE.
(defflavor post-office ((mailing-lists (make-equal-hash-table ':size 60)))
           ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)
(defmethod (post-office :add-to) (mailing-list mailbox)
 (let  ((data (gethash mailing-list mailing-lists)))
   (when  (not data)
      (ferror "No such mailing list: ~S" mailing-list))
    (when (not (memq mailbox mailing-lists))
      (push mailbox (cdr data)))))
(defmethod (post-office :remove-from) (mailing-list mailbox)
  (let ((data (gethash mailing-list mailing-lists)))
    (when (not data)
      (ferror "No such mailing list: ~S" mailing-list))
    (setf (cdr data) (delq mailbox (cdr data)))))
(defmethod (post-office :add) (mailing-list
                                &optional (initial-contents '())
                                          (err-if-exists? t))
  (let ((data (gethash mailing-list mailing-lists)))
    (when (and data err-if-exists?)
      (ferror "List already exists: ~S" mailing-list))
    (when (not data)
      (setq data (ncons mailing-list))
      (puthash mailing-list data mailing-lists))
    (dolist (mailbox initial-contents)
      (if (not (memq mailbox (cdr data)))
          (push mailbox (cdr data))))))
(defmethod (post-office :remove) (mailing-list)
  (remhash mailing-list mailing-lists))
(defmethod (post-office :resolve) (mailing-list)
  (cond ((typep mailing-list 'mailbox) (ncons mailing-list))
        (t
         (let ((data (gethash mailing-list mailing-lists)))
           (cond (data
                  (mapcan #'(lambda (address)
                              (send self ':resolve address))
                          (cdr data)))
                 (t
                  (send self ':resolve-remote mailing-list)))))))
(defmethod (post-office :resolve-remote) (mailing-list)
  (multiple-value-bind (actor-name host-name remote?)
      (parse-actor-full-name mailing-list)
    host-name ; ignored
    (cond ((not remote?)
           (when (string-equal actor-name mailing-list)
             (ferror "No such mailing list: ~S"
                     mailing-list))
           (prog1 (send self ':resolve actor-name)
;;; PDF page 161

                        (list actor-name))))
          (t
           (let ((actor (parse-remote-actor mailing-list)))
             (prog1 (ncons (send actor ':inbox))
                    (let ((indirect-name (send actor ':name)))
                      (send self ':add mailing-list
                            (cond ((not (equal mailing-list indirect-name))
                                   (list indirect-name) )
                                  (t ;Should never really happen...
                                   (list (send actor ':inbox)))))))))))

(compile-flavor-methods post-office)
(defvar *post-office* (make-instance 'post-office))
(defun deliver (address message)
  (dolist (mailbox (send *post-office* ':resolve address) )
    (send mailbox ':insert-message message) )
  (send message ':id))
(defun add-to-mailing-list (mailing-list &optional addresses (err? t))
  (send *post-office* ':add
        mailing-list
        (if (atom addresses) (ncons addresses) addresses)
        err?))
