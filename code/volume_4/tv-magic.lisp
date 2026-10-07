;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>TV-MAGIC.LISP.35, 5 June 1984 00:54:00
;;; PDF pages 72-80 (printed source pages 1-9).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; TV-MAGIC: Miscellaneous LispM Window system utilities and other magic

;;; (GET-PANE window pane-name)
;;; Gets the pane of window with the given name.
(defun get-pane (window pane-name)
  (send window ':get-pane pane-name))

;;; (GET-ANY-PANE window pane-name)
;;; Like GET-PANE but is willing to search upward in the window
;;; hierarchy if the given window doesn't have such a frame
;;; in case one of its superiors has the given pane instead.
(defun get-any-pane (window pane-name)
  (send-any-superior-if-handles window ':get-pane pane-name) )
;;; (REQUIRE-MOUSE-CONFIRMATION message [window])
;;; Does a pop-up type query of a given message, returning T or NIL.
;;; If window is given, the pop-up window tries to be near that window,
(defun require-mouse-confirmation (message &optional window)
  (cond ((tv:menu-choose (list message)
                         "Confirm:"
                         (cond (window (list ':window window))
                               (t '(:mouse))))
         t)))

;;; Simple Pane Mixin
;;; SIMPLE-PANE-MIXIN
;;; This mixin collects useful commonalities of the windows we want in our
;;; simple frame.
(defflavor simple-pane-mixin (selectable-p) (tv:pane-mixin)
           (:documentation :mixin "Pane mixin plus some stylized defaults.")
           :initable-instance-variables
           :settable-instance-variables
           :gettable-instance-variables
           (:default-init-plist
               :border-margin-width 2
             :selectable-p nil
             :borders 1
             :reverse-video-p nil
             :more-p t
             :activate-p t
             :blinker-p t
             :save-bits t
             :font-map (list fonts:medfnb)
             :deexposed-typeout-action ':permit))
(defmethod (simple-pane-mixin :name-for-selection) () nil)
(defmethod (simple-pane-mixin :mouse-select) (&rest ignore)
  (cond (selectable-p (send self ':select))
        (t (send self ':beep))))

;;; Command Menu Mixin
;;; COMMAND-MENU-MIXIN
;;; This is a mixin which makes a window into a "command menu".
(defflavor command-menu-mixin () (tv:basic-menu tv:basic-scroll-bar)
           (:documentation :mixin
                           "Any mouse input is put in the window's 10 buffer")
           (:required-methods :io-buffer :chosen-item :set-chosen-item) )
;;; t:AFTER :MOUSE-BUTTONS
 ;;;
;;; This is a hack that takes any mouse input and arranges to have it
;;; end up in the io-buffer of the associated command pane. This allows
;;; a process to simply TYI from the window to get input.
;;; TV:COMMAND-MENU uses this tactic, which is where we got the idea.
(defmethod (command-menu-mixin :after :mouse-buttons) (bd ignore ignore)
  (let ((buffer (send self ':io-buffer))
        (item   (send self ':chosen-item)))
    (when item
      (tv:io-buffer-put buffer `(:menu ,item ,bd ,self))
      (send self ':set-chosen-item nil))))

;;;; Margin Scroll Bar Mixin
;;; MARGIN-SCROLL-BAR-MIXIN
;;; This mixin causes a window to have a continuously displayed
;;; scroll bar imbedded in its margin,
;;; The :MARGIN-WIDTHS is used to calculate an appropriate sized
;;; scroll bar. It does not actually set the size of the borders.
;;; It is necessary to use this in conjunction with a : BORDERS
;;; message in order to win totally.
(defflavor margin-scroll-bar-mixin (margin-widths scroll-bar-width) ()
           (:documentation :mixin "Scroll bar is part of margin.")
           (:required-flavors tv:borders-mixin)
           (:required-instance-variables
            tv:scroll-bar tv:scroll-bar-always-displayed)
           :initable-instance-variables
           (:default-init-plist
               :margin-widths '(6 2 2 2)
             :scroll-bar-width 2))
;;; :AFTER : ACTIVATE
;;; This is hacked in at a place where it will probably work, It is not
;;; really the right time, but I don't know what is. Anyway, it seems to
;;; do the job. The problem seems to be that sending a window a
;;; :SET-SCROLL-BAR message doesn't do the right thing because we want
;;; the scroll bar to be inside the margin and something seems to like
;;; to look at our scroll bar spec and add the width of the margin to
;;; it because it thinks we couldn't possibly really mean that we want
;;; to be backed into the margin.
(defmethod (margin-scroll-bar-mixin :after :activate) (&rest ignore)
  (let ((top    (cadr margin-widths))
        (right  (caddr margin-widths))
        (bottom (cadddr margin-widths)))
    (setq tv:scroll-bar  `(,right
                           ,top
                           ,(+ right scroll-bar-width)
                           ,(- bottom) ))
    (setq tv:scroll-bar-always-displayed t)))

;;;; Line Truncating Mixin
;;; LINE-TRUNCATING-MIXIN
(defflavor line-truncating-mixin () (tv:line-truncating-mixin)
           (:documentation :mixin "Sets up to have line overflows be truncated."))
;;; :AFTER :INIT
;;; Set up the relevant state data so window will know we intend to have
;;; it doing truncation.
(defmethod (line-truncating-mixin :after :init) (&rest ignore)
  (setf (tv:sheet-truncate-line-out-flag self) 1))


;;; Box Drawing Mixin
;;; BOX-DRAWING-MIXIN
(defflavor box-drawing-mixin () ()
           (:documentation :mixin
                           "Methods for drawing outlined or complemented boxes.")
           (:required-methods :draw-rectangle :draw-lines
                              :char-width :line-height))
;;; In this :COMPLEMENT-BOX and :OUTLINE-BOX, give the character position
;;; for the top right character IN the box, and the bottom right character
;;; IN the box. (SEND FOO ':COMPLEMENT-BOX 4 4 4 4) will complement the
;;; character at position 4 4.  Characters are measured from 0.
;;; Optimized this so it looks good with a single box
;;; even though this may look odd with adjacent boxes due to overlap.
;;; It is intended that this will be better in most cases.
(defmethod (box-drawing-mixin :complement-box) (left top right bottom)
  ;; Convert to pixels
  (let ((char-width  (send self ':char-width))
        (line-height (send self ':line-height)))
    (let ((p-left (- (* left char-width) 2))
          (p-width  (+ 4 (* (+ 1 (- right left)) char-width)))
          (p-top    (- (* top line-height) 1))
          (p-height (* (+ 1 (- bottom top)) line-height) ))
      (send self ':draw-rectangle
            p-width p-height p-left p-top
            tv:alu-xor))))
;;; Draws lines around the area, then complements the corners. (This makes
;;; boxes that are constructed row by row look nicer)
(defmethod (box-drawing-mixin  :outline-box) (left top right bottom)
  ;;; Convert to pixels
  (let ((char-width  (send self  ':char-width) )
        (line-height (send self  ':line-height)))
    (let ((p-left   (- (* left char-width) 2))
          (p-width  (+ 4 (* (+ 1 (- right  left)) char-width)))
          (p-top    (-  (* top line-height) 2))
          (p-height (1+  (* (+ 1 (- bottom top)) line-height))))
      (let ((p-right  (+ p-left p-width  -1))
            (p-bottom  (+ p-top p-height -1)))
        (send self  ':draw-lines tv:alu-xor
              p-left  p-top
              p-right p-top
              p-right p-bottom
              p-left  p-bottom
              p-left  p-top)))))

;;; Associated Process Mixin
(defflavor associated-process-mixin () (tv:process-mixin)
           (:default-init-plist :process
   ;;; Initial parameters for the process stack are smaller than
   ;;; for normal Lisp Listeners, so we boost this artificially
   ;;; to keep users from pestering for an init option to this flavor
    ;;; letting them specify rational values. ~kmp
               '(process-toplevel . (:regular-pdl-size 10000.
                                     :special-pdl-size  1500.))))
(defun process-toplevel (terminal-io)
  (do () (nil) ;loop forever
    (send terminal-io ':process-toplevel)))
(defmethod (associated-process-mixin :default :process-toplevel) ()
  (process-wait "Wait Forever" #'(lambda () nil)))

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WITH-OUTPUT-TO-STRING Indent:1;
;;; Lisp WITH-INPUT-FROM-STRING Indent:1;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; End:;
