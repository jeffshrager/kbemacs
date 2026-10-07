;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>SIMPLE.LISP.256, 13 March 1984 19:15:36
;;; PDF pages 55-62 (printed source pages 1-8).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; SIMPLE: The "Simple" Frame for the KBE.
;;; The following comment tries to give an overview of the intent of the
;;; Simple Window. Because the code is changing very fast these days, this
;;; documentation is not guaranteed to have been kept up to date. I'11
;;; periodically modify the documentation as major milestones are reached.
;;; For complete details, come talk to me or consult the code. ~-KMP
;;; The Simple Frame, which can be created by typing System K
;;; is a frame which looks roughly like:             :

;;;   +---------------+
;;;   |
;;;   | Display Area  |
;;;   |
;;;   +---------------+
;;;   | Status Window |
;;;   +---------------+
;;;   | Command Menu  |
;;;   +---------------+
;;;   | Input Window  |
;;;   +---------------+
;;;   | Lisp Listener |
;;;   +---------------+
;;; purpose of the sections is as follows:
;;; Display Area. The  large [initially] empty area at the top of the
;;; screen is reserved for displays of code in focus, and for
;;; answering various kinds of questions specifically asked by
;;; the user.
;;; Window. There  is a very small window (defaultly 1 line high)
;;; which  is used to type information of interest to this window's
;;; session.
;;; Command Menu. This is a menu pane filled with useful commands,
;;; most of which can  be gotten by other means, but which are useful
;;; enough  that they warrant having on the screen for easy access via
;;; the mouse.
;;; Input Window. This is an editor window that one can type commands
;;; to. Typing <End> causes the commands to actually get submitted
;;; to the apprentice for execution.
;;; Lisp Listener. This is a normal lisp listener pane, for debugging.
;;; SIMPLE-FRAME
;;; A constraint frame for holding onto the other KBE window
;;; frames in our demo configuration.
(defconst *status-pane-size* 1. "Default status pane height in lines")
(defflavor simple-frame ()
  (delegate-mixin
   tv:bordered-constraint-frame)
  :initable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables
  (:default-init-plist
      :delegate-flavor 'simple-actor
    :deexposed-typeout-action ':permit
    :x 0 :y 0
    :borders 1
    :blinker-p nil
    :save-bits t
    :edges-from (multiple-value-list (send tv:mouse-sheet ':size))
    :activate-p t
    :panes '((display-pane  display-pane        :borders (2 1 2 1))
             (status-pane    status-pane        :borders (2 1 2 1))
             (options-pane  options-pane        :borders (2 1 2 1))
             (input-pane    editing-input-pane  :borders (6 1 2 1)
              :margin-widths (6 1 2 1)
              :scroll-bar-width 3
              :label nil
              :border-margin-width 2
              :font-map (fonts:bigfnt))
             (listener-pane listener-pane       :borders (2 1 2 1)))
    :constraints
    `((main . ((display-pane
                status-pane
                options-pane
                input-pane
                listener-pane)
               ((display-pane  0.4                             ))
               ((status-pane   ,*status-pane-size*      :lines ))
               ((options-pane  ,(1+ (length *options*)) :lines ))
               ((input-pane    0.25                            ))
               ((listener-pane :even                            ))))
      (bare  . ((display-pane
                 status-pane
                 listener-pane)
                ((display-pane  0.33                       ))
                ((status-pane   ,*status-pane-size* :lines ))
                ((listener-pane :even)))))))
(defmethod (simple-frame :name-for-selection) ()
  (text "KBE ~A~@[: ~A~]"
        (send self ':name)
        (let ((my-label (send (get-pane self 'display-pane) ':label)))
          (if (listp my-label) ; ignore random font, vsp, etc
              (setq my-label (nth 5 my-label))) ;ignore font
          (if (equal my-label "") nil my-label))))
(defmethod (simple-frame :remark-window) () (get-pane self 'status-pane))
(defmethod (simple-frame :typeout-window) () (get-pane self 'display-pane))
(defmethod (simple-frame :after :init) (&rest ignore)
   ;;; Label this frame. (using the Listener's label)
  (send (get-pane self 'listener-pane) ':set-label
        `(:bottom
          :font fonts:medfnb
          :string ,(text "KBE ~A" (send self ':name))))

  ;; Select appropriate pane           2
  (send self ':select-pane (get-pane self 'input-pane)))

;;; DISPLAY-PANE
;;; A window which can be used for the KBE's display output.
;;; It allows for character-measured boxes and complemented regions,
;;; instead of pixel-measured ones.
(defflavor display-pane () (simple-pane-mixin code-window)
           (:default-init-plist
               :label '(:bottom :font fonts:medfnb :string "")
             :more-p nil
             :borders nil
             :blinker-p nil
             :font-map (list fonts:bigfnt)))

;;;; STATUS-PANE
;;; A window which actually controls what the KBE is doing.
(defflavor status-pane ()
  (line-truncating-mixin             ; Line overflows can be discarded
   simple-pane-mixin                 ; Normal pane stuff
   tv:window)                        ; Normal window stuff
  (:default-init-plist
      :label nil
    :more-p nil
    :blinker-p nil))

;;; EDITING-INPUT-PANE
(defflavor editing-input-pane () (tv:dont-select-with-mouse-mixin
                                  tv:pane-mixin
                                  associated-process-mixin
                                  edit-window-input-commands-mixin
                                  edit-window) )
                                        ;this new version assumes that End is set up with something that submits
                                        ;buffer for us.
(defmethod (editing-input-pane :process-toplevel) ()
  (with-delegate (window-delegate self)
    (with-user-package ;temp fix
        (send self ':edit)
      (beep)))) ;should never get to here.

;;;; LISTENER-PANE
;;; Just a lisp listener with pane-mixin thrown in to make things work.
(defflavor listener-pane ()
  (tv:notification-mixin        ; normal notifications
   tv:listener-mixin           ; listener stuff
   simple-pane-mixin            ; Normal pane stuff...
   tv:window)                   ; normal window stuff
  (:default-init-plist
      :selectable-p t))

;;;; Initialization
;;; Request compile-time compilation of flavors. By default, the
;;; methods would not get compiled until fasload-time otherwise.
(compile-flavor-methods simple-frame
                        display-pane
                        status-pane
                        editing-input-pane
                        listener-pane)
;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; Ends;
