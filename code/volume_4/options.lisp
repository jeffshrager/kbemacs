;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>OPTIONS.LISP.40, 9 April 1984 19:09:25
;;; PDF pages 48-54 (printed source pages 1-7).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

(defflavor options-window (display-handler-function)
  (associated-process-mixin
   tv:mouse-sensitive-regions-window)
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:default-init-plist
      :display-handler-function nil))
(defmethod (options-window :redisplay) ()
  (if display-handler-function
      (funcall display-handler-function self)))
(defmethod (options-window :after :refresh) (&rest ignore)
  (send self ':redisplay))
(defmethod (options-window :process-toplevel) ()
  (multiple-value-bind (choice button)
      (send self ':choose)
    (process-run-function (format nil "Option: ~A" choice)
                          #'(lambda (choice window button)
                              (with-user-package
                                  (funcall choice window button)))
                          choice
                          terminal-io
                          button)))
(defflavor options-pane () (simple-pane-mixin options-window)
           (:default-init-plist
               :activate-p t
             :deexposed-typeout-action ':permit
             :display-handler-function 'display-options
             :blinker-p nil
             :more-p nil
             :label nil))
(defmethod (options-pane :after :activate) (&rest ignore)
  (send (send self ':process) ':run-reason self))
(defvar *options* '() "List of options for standard options window.")
(defun maybe-display-options-sample (window button)
  (cond ((and *debug* (eq button ':right))
         (debug-options-window window button))
        (t (display-options-sample window button))))
(defun display-options-sample (window button)
  window ; ignored
  (repeat ()
          (return-if
           (tv:menu-choose
            (list (format nil "Signed, ~A" user-id))
            (format nil "I, ~A, do hereby promise that I will faithfully~
                    ~%read and heed the mouse documentation line and~
                    ~%not click ~A on things I have warned not to~
                    ~%click on.~
                    ~%            Click to confirm:"
                    user-id (string-downcase button))))))

(defun debug-options-window (window button)
  button ; ignored
  (unwind-protect (let ((terminal-io window))
                    (send terminal-io ':clear-screen)
                    (send terminal-io ':select)
                    (format t
                            "Remember to reselect another pane manually.")
                    (break "Debug Options"))
    (send window ':refresh)))
(defun display-options (&optional (standard-output standard-output) )
  (send standard-output ':clear-screen)
  (format t "By touching ~M with the mouse, you can ...~
              ~{~% ^@ ~{~:[~A~;~M~]~}~.~}"
          '("parts of this window" maybe-display-options-sample
            "Not here! Don't take the words too literally; touch the text below.")
          (mapcar #'cdr *options*)))
(defmacro define-option-line (name &body data)
  `(let ((entry (assq ',name *options*))
         (data  ',(mapcon #'(lambda (x)
                              (list (not (atom (car x)))
                                    (car x)))
                          data)))
    (cond (entry (setf (cdr entry) data))
          (t (setq *options*
                   (nconc *options* (ncons `(,',name . ,data))))))
    ',name))
(defmacro define-option-function (name bvl &body body)
  `(defun ,name ,bvl ,@bvl
    (with-delegate (window-delegate ,(car bvl))
      ,@body)))

;;; Apprentice-Related Options
(define-option-function apprentice-select (window button)
  window button ;ignored
  (remark (select-apprentice)))
(define-option-function apprentice-status (window button)
  window button ; ignored
  (let ((apprentice (associated-apprentice)))
    (remark (cond ((not apprentice)
                   "No apprentice yet selected for this window.")
                  (t (ask apprentice task "Report status."))))))
(define-option-function apprentice-reset (window button)
  button ; ignored
  (remark (reset-apprentice)))                                              ;
(define-option-line hack-apprentice
    ("Select the associated apprentice's window" apprentice-select) ", "
    ("poll its status"                           apprentice-status) "or  "
    ("reset it"                                  apprentice-reset))

;;; Debug Options
(define-option-function options-edit-all (window button)
  window button ;ignored
  (edit-KBE-options))
(define-option-function options-toggle-debug (window button)
  window  ; ignored
  (caseq button
         ((:LEFT)   (setq *debug* (not *debug*)))
         ((:MIDDLE) (setq *debug* nil))
         ((:RIGHT)  (setq *debug* t)))
  (remark (text "Debug Mode ~:[off~;on~]." *debug*)))
(define-option-line options
    ("Edit KBE options"  options-edit-all)
  " or "
  ("toggle debug mode" options-toggle-debug
                       "L:Complement Debug Mode, M:Debug Mode Off, R:Debug Mode On"))

;;;; Font display        :
(define-option-function fonts-any (window button)
  (selectq button
           ((:LEFT)    (fonts-small  window button))
           ((:MIDDLE) (fonts-medium window button))
           ((:RIGHT)  (fonts-large  window button))))
(define-option-function fonts-small (window button)
  button ; ignored
  (let ((display-pane (get-any-pane window 'display-pane)))
    (cond ((not display-pane) (beep))
          (t (send display-pane ':set-font-map (list fonts:cptfontb) )
             (send display-pane ':refresh)
             (remark "Now using small font (CPTFONTB).")))))
(define-option-function fonts-medium (window button)
  button ; ignored
  (let ((display-pane (get-any-pane window 'display-pane)))
    (cond ((not display-pane) (beep))
          (t (send display-pane ':set-font-map (list fonts:medfnb))
             (send display-pane ':refresh)
             (remark "Now using medium font (MEDFNB).")))))
(define-option-function fonts-large (window button)
  button ; ignored
  (let ((display-pane (get-any-pane window 'display-pane)))
    (cond ((not display-pane) (beep) )
          (t (send display-pane ':set-font-map (list fonts:bigfnt))
             (send display-pane ':refresh)
             (remark "Now using large font (BIGFNT).")))))
(define-option-line fonts
    ("Change the display window font" fonts-any
                                      "L:Small (CPTFONTB), M:Medium (MEDFNB), R:Large (BIGFNT)")
  " to be "
  ("small"  fonts-small  "Select small display font. (CPTFONTB)")  ", "
  ("medium" fonts-medium "Select medium display font. (MEDFNB)")   " or "
  ("large"  fonts-large  "Select large display font. (BIGFNT)"))

;;;; Self Testing
(define-option-function utility-test (window button)
  button window ; ignored
  (let ((actor (associated-apprentice)))
    (cond ((not actor)
           (remark "No associated apprentice."))
          ((require-mouse-confirmation "Run Standard Test Sequence.")
           (tell actor task "Test yourself."))
          (t
           (remark "No test sequence selected.")))))
(define-option-function utility-init-db (window button)
  button ; ignored
  (cond ((not *debug*)
         (remark "Only works in Debug Mode."))
        ((require-mouse-confirmation "Reset entire database")
         (remark "Re-initializing KBE database...")
         (db-init)
         (remark "KBE Database has been re-initialized."))
        (t
         (remark "Request aborted. KBE Database left intact."))))
(define-option-line utility
    ("Ask the KBE to test itself" utility-test)
  "or"
  ("re-initialize its database" utility-init-db))

;;; Initialization
;;; Request compile-time compilation of flavors. By default, the
;;; methods would not get compiled until fasload-time otherwise.    =
(compile-flavor-methods options-window options-pane)
;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; Ends;
