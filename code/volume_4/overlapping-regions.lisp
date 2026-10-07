;;; -*- Mode:LISP; Package:TV; Base:10; Fonts:MEDFNT; -*-

;;; Source: OZ:<KBE.SOURCE>OVERLAPPING-REGIONS.LISP.9, 30 July 1983 22:19:50
;;; PDF pages 39-43 (printed source pages 1-5).

;;; (c) 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; This file defines an abstraction for dealing with a window with mouse
;;; Sensitive regions, some of which may be (but are not constrained to be)
;;; ;;; TV:OVERLAPPING-REGIONS-MIXIN (mixin)
;;; This is the base mixin which supports the overlapping regions.
;;; It is built out of TV:ANY-TYI-MIXIN. Here's a description of the
;;; variables...
;;; REGIONS
;;; By default, this is NIL, meaning the window has no mouse sensitive regions.
;;; It may be set (with :REGIONS init option or with a :SET-REGIONS
;;; message  later) to be an alist of the form:
;;; ((coordinates1 . datal) (coordinates2 . data2) (coordinates3 . data3) ...)
;;; where coordinates have the form:
;;; (x1 y1 x2 y2)
;;; and item has the form of a normal item as in other flavors using regions.
;;; MOUSE-REGION
;;; This variable holds the region currently under the mouse, or NIL if no
;;; region is under the mouse. You can use the :MOUSE-REGION message to read its
;;; value. The :MOUSE-REGION init options and :SET-MOUSE-REGION message are
;;; provided for debugging consistency, but won't have any useful effect, so you
;;; should probably pretend they don't exist unless you've carefully studied
;;; the code and understand what they do.
;;; a bee Most users will be happy with the default, which is a hollow rectangular
;;; blinker. This will be used to highlight the selected area. If you want
;;; another kind of blinker, you can use the :MOUSE~REGION-BLINKER init
;;; message or :SET-MOUSE-REGION-BLINKER message to set this.
;;; REGION-FILTER
;;; If not NIL, this should be a function of one argument which takes an
;;; region which is about to be returned by :MOUSE-SENSITIVE-REGION
;;; and returns true if acceptable and false if not. This allows one to place
;;; constraints on which of the possible regions in the REGIONS are active
;;; at any given time. The default is NIL, meaning no filtering is done.
;;; WHO-LINE-DOCUMENTATION-STRING
;;; Lisp will check the window under the mouse periodically (actually, quite
;;; often) to see if this has changed. If it has, it will update the mouse
;;; documentation line. You can read the window's documentation yourself with
;;; the :WHO-LINE-DOCUMENTATION-STRING message or set it with the    :
;;; :SET-WHO-LINE-DOCUMENTATION-STRING message or :WHO-LINE-DOCUMENTATION-STRING
;;; init option. In normal use, this isn't necessary. See whe
;;; DOCUMENTATION-HANDLER function.
;;; DOCUMENTATION-HANDLER    'i
;;; Readable and settable in the obvious ways. If this variable is NIL, then
;;; the documentation for the window won't change except by explicit calls to
;;; :SET-WHO-LINE-DOCUMENTATION-STRING. However, if it is non-NIL, it can be
;;; a handler function which is called when the mouse moves, receiving the
;;; new region (or NIL) as an arg and returning new mouse documentation.
;;; If you allocate an element of the region list as reserved for documentation,
;;; this can be very fast. Eg, if your regions are of the form
;;; (coordinates object doc) then you can use #'CADDR as your
;;; DOCUMENTATION-HANDLER.
;;; Overlapping Regions Mixin
;;; TV: OVERLAPPING-REGIONS-MIXIN
;;; Basic definition of how Overlapping Regions work.
(defflavor overlapping-regions-mixin (mouse-region-blinker
                                      mouse-region
                                      regions
                                      region-filter
                                      documentation-handler
                                      who-line-documentation-string)
  (any-tyi-mixin)
  (:required-flavors tv:sheet)
  (:documentation :mixin
                  "An abstraction for using possibly-overlapping mouse sensitive regions")
  :initable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables
  (:default-init-plist
      :who-line-documentation-string nil
    :documentation-handler nil
    :regions             nil
    :mouse-region        nil
    :mouse-region-blinker nil
    :region-filter       nil))
(defmethod (overlapping-regions-mixin :after :init) (&rest ignore)
  (unless mouse-region-blinker
    (setq mouse-region-blinker
          (make-blinker self 'hollow-rectangular-blinker ':visibility nil))))
;;; :MOUSE-CLICK button x y
;;; :MOUSE-MOVES x y
;;; These two messages get sent by the window system whenever the mouse is
;;; played with. This window abstraction just builds on those messages.
(defmethod (overlapping-regions-mixin :mouse-click) (button x y)
  (let ((inside-x (- x (sheet-inside-left)))
        (inside-y (- y (sheet-inside-top))))
    (send self ':select-region
          (send self ':mouse-sensitive-region inside-x inside-y)
          button) ))
(defmethod (overlapping-regions-mixin :mouse-moves) (x y)
  (mouse-set-blinker-cursorpos) ;Move mouse blinker
  (let ((inside-x (- x (sheet-inside-left)))
        (inside-y (- y (sheet-inside-top))))
    (multiple-value-bind (new-region)
        (send self ':mouse-sensitive-region inside-x inside-y)
      (when (neq mouse-region new-region)
        (setq mouse-region new-region)
        (send self ':move-mouse-region new-region)))))
;;; :WHOPPER ;:HANDLE-MOUSE
;;; This corrects half of a bug that is caused when another window pops up over
;;; the top of the current one, allowing the mouse to "move" off of the current
;;; window without changing its x-y coordinates. The half it fixes is where the
;;; mouse moves off of the window; it does not fix the case of the mouse moving
;;; onto the window because we don't know how to reliably read the mouse x-y
;;; information.
(defwhopper (overlapping-regions-mixin :handle-mouse) (&rest args)
  (values-list (prog1 (multiple-value-list (lexpr-continue-whopper args))
                 (when mouse-region
                   (setq mouse-region nil)

                   (send self ':move-mouse-region nil)))))
;;; :CLEAR-SCREEN
(defmethod (overlapping-regions-mixin :before :clear-screen) (&rest ignore)
  (setq regions nil)
  (move-blinker mouse-region-blinker  nil))
;;; :MOUSE-SENSITIVE-REGION x y
;;; Given  inside X and Y coordinates, this function returns the active region
;;; from the  REGIONS corresponding to X and Y. That region is the first
;;; region  in the alist which contains the point (X,Y) and which is not filtered
;;; Out by the REGION-FILTER if one exists.
(defmethod (overlapping-regions-mixin :mouse-sensitive-region) (inside-x inside-y)
  (dolist (region regions)
    (let ((coordinates (car region)))
      (when (and (>= inside-x (nth 0 coordinates))
                 (>= inside-y (nth 1 coordinates) )
                 (<= inside-x (nth 2 coordinates) )
                 (<= inside-y (nth 3 coordinates) )
                 (or (not region-filter)
                     (funcall region-filter region)))
        (return region)))))
;;; :SELECT-REGION region button
;;; Whenever  a button is clicked, this method gets sent the region and
;;; the button. If there is no associated region, it gets NIL instead.
;;; Flavors built on this flavor may wish to redefine this.
(defmethod (overlapping-regions-mixin :select-region) (region button)

  (when region
    (send self ':force-kbd-input `(:mouse-click ,region ,button ,self))))
;;; :MOVE-MOUSE-REGION region
;;; Given an region from the REGIONS (or NIL meaning to deselect the
                                        ;33  active region), this sets the mouse blinkers appropriately.
;;; This may be a good function to define :BEFORE and :AFTER daemons
;;; for when building flavors on this mixin. It is called only when the
;;; Mouse-region has actually changed.
(defmethod (overlapping-regions-mixin :move-mouse-region) (region)
  (move-blinker mouse-region-blinker region))
(defun-method move-blinker overlapping-regions-mixin (blinker region)
              (unless (not blinker) ;just in case user kills the blinker
                (cond (region
                       (let ((coordinates (car region)))
                         (let ((x1 (nth 0 coordinates))
                               (y1 (nth 1 coordinates) )
                               (x2 (nth 2 coordinates) )
                               (y2 (nth 3 coordinates)))
                           (blinker-set-cursorpos blinker x1 y1)
                           (blinker-set-size blinker (- x2 x1) (- y2 y1))
                           (blinker-set-visibility blinker t))))
                      (t
                       (blinker-set-visibility blinker nil)))
                (send self ':set-who-line-documentation-string
                      (if documentation-handler
                          (funcall documentation-handler region)))))

;;; Overlapping Regions Window/Pane
;;; TV:OVERLAPPING-REGIONS-WINDOW
;;; This is just a TV:WINDOW with TV:OVERLAPPING-REGIONS-MIXIN.   -    .   yee
(defflavor overlapping-regions-window () (overlapping-regions-mixin window)
           (:documentation
            "A window which handles possibly-overlapping mouse sensitive regions"))

;;; TV:OVERLAPPING-REGIONS-PANE
;;; This is just adds TV:PANE-MIXIN to a TV:OVERLAPPING-REGIONS-WINDOW.

(defflavor overlapping-regions-pane () (pane-mixin overlapping-regions-window)
           (:documentation
            "A window pane which handles possibly-overlapping mouse sensitive regions"))
(compile-flavor-methods overlapping-regions-window overlapping-regions-pane)
