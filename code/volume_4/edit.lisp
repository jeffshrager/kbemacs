;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>EDIT.LISP.193, 21 May 1984 16:50:59
;;; Knowledge-Based_Editor_Jan-1985_volume_4.pdf, PDF pages 142-146.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;;; EDIT: The "Edit" Frame for the KBE.

;;;; Edit Frame

;;; EDIT-FRAME
;;;
;;; A constraint frame for holding onto the other KBE window
;;; frames in our demo configuration.

(defflavor edit-frame ()
  (delegate-mixin                               ; We can have an associated actor
   tv:constraint-frame)                         ; We can have sub-windows
  :initable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables
  (:default-init-plist
      :delegate *initial-editing-actor-secretary*
    :deexposed-typeout-action ':permit
    :blinker-p nil
    :activate-p t
    :panes '((editor-pane   editor-pane      )
             (status-pane   status-pane       :borders (2 0 2 2)))
    :constraints '((edit  . ((editor-pane
                              status-pane)
                             ((status-pane   1     :lines    ))
                             ((editor-pane   :even           )))))
    :selected-pane 'editor-pane))

(defmethod (edit-frame :remark-window) ()
  (get-pane self 'status-pane))

(defmethod (edit-frame :typeout-window) ()
  (make-syn-stream 'terminal-io))

;;; :NAME-FOR-SELECTION
;;;
;;;  The LispM window system will send this message to a window to find
;;;  if it wants to be selected. If it returns NIL, it does not want to
;;;  be selected. If it returns non-NIL, the return value should be a
;;;  string to be used in selecting the window.

(defmethod (edit-frame :name-for-selection) ()
  (format nil "KBE ~A" (send tv:selected-pane ':name-for-selection)))

;;; :SELECTABLE-WINDOWS
;;;
;;;  The LispM window system will send this message to a window to find
;;;  what parts of it can be selected. The default for frames is that they
;;;  and any of their inferiors will be selectable if they answer non-NIL
;;;  to the :NAME-FOR-SELECTION message. The return value expected is an
;;;  alist of the form ((window-name . window) ...)
;;;
;;;  To avoid cluttering the system menu with info about our inferiors,
;;;  KBE edit frames explicitly handle this message, returning information
;;;  only about the frame itself and not its inferiors.

(defmethod (edit-frame :selectable-windows) ()
  (list (list (send self ':name-for-selection) self)))

;;;; EDITOR-PANE

(defflavor editor-pane ()
  (tv:borders-mixin                          ; We need a border around us
   tv:pane-mixin                             ; Allow us to be a sub-pane
   vanilla-editor-frame)                     ; And, of course, an editor!
  (:default-init-plist
      :border-margin-width 0
    :deexposed-typeout-action ':permit
    :save-bits t
    :activate-p t))

;;; :MOUSE-SELECT
;;;
;;;  Handling this message allows us to do something interesting if the
;;;  user does a left click on our window. We do the obvious thing here;
;;;  ie, we select our pane.

(defmethod (editor-pane :mouse-select) (&rest ignore)
  (send self ':select))

;;;; Methods Edit Actor will prefer

;;; :TYPEOUT
;;;
;;;  We intentionally don't handle this message.
;;;  Let the guy type on our typeout window if he wants to.
;;;  This message must return T or NIL.

(comment ;this doesn't get used, does it? --kmp 1/1/84

 (defmethod (editor-pane :typeout) (string)
   string
   nil) ;not handled

 )

;;;; Initialization

;; We have to match the way ZWEI defaults its window size...

(let ((main-screen-info (assq tv:main-screen
                              tv:*default-window-size-alist*)))
  (when main-screen-info
    (let ((zmacs-frame-info (assq 'zwei:zmacs-frame
                                  (cdr main-screen-info))))
      (when zmacs-frame-info
        (lexpr-funcall #'tv:set-default-window-size
                       'edit-frame tv:main-screen nil
                       (cdr zmacs-frame-info))))))

;;; Request compile-time compilation of flavors. By default, the
;;; methods would not get compiled until fasload-time otherwise.

(compile-flavor-methods edit-frame editor-pane)

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; End:;
