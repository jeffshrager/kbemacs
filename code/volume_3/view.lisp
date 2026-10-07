;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; PDF pages 212-214 (printed source pages 1-3).
;;;
;;; (c) Copyright 1984, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;;; Flavor support

;;; CODE-WINDOW-MIXIN

(defflavor code-window-mixin ((offset 0)
                              (dhunk nil) ;not needed
                              (focus nil)
                              (focus-name nil)) ()
  (:documentation
   :mixin "A mixin to support display of pieces of code.")
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

(defmethod (code-window-mixin :before :clear-screen) (&rest ignore)
  (send self ':set-label "")
  (setq offset      0
        focus       nil
        focus-name  nil))

(defmethod (code-window-mixin :change-focus) (reference paths)
  (when reference
    (when (neq (car paths) (car focus))
      (send self ':clear-screen)
      (send self ':display-program (car paths)))
    (send self ':set-label
          `(:bottom
            :font fonts:medfnb
            :string ,(text "Focus: ~A." (string-capitalize reference))))
    (setq focus-name reference focus paths)))

(defmethod (code-window-mixin :display-program) (program)
  (with-user-package ;...sigh... --kmp
    (let ((entry (send (window-delegate self) ':belief ':code program)))
      (when entry
        (multiple-value-bind (x y)
            (send self ':size-in-characters)
          (let ((pp:*print-right-margin* x)) ;for PPRINT
            (if (not (zerop offset))
                (barf "non-zero offset not supported now")) ; dick
            (print-code entry self (1- (+ offset y)))))))))

(defmethod (code-window-mixin :scroll) (&optional (arg 1))
  (when focus
    (let ((new-offset (+ offset arg)))
      (cond ((minusp new-offset) (beep))
            (t
             (setq offset new-offset)
             (send self ':refresh))))))

(defmethod (code-window-mixin :home) ()
  (when focus
    (cond ((= offset 0) (beep))
          (t
           (setq offset 0)
           (send self ':refresh)))))

(defmethod (code-window-mixin :after :refresh) (&optional mode)
  (when (and (neq mode ':use-old-bits) focus)
    (send self ':set-cursorpos 0 0)
    (send self ':clear-eof)
    (send self ':display-program (car focus))))

(defmethod (code-window-mixin :who-line-documentation-string) ()
  (cond ((not focus) "R2: System Menu")
        ((zerop offset)
         "M: Scroll Up, M2: Scroll Half-Screen Up, R2: System Menu")
        (t
         #.(format nil "L:Home, ~
                        M:Scroll Up, ~
                        M2:Scroll Half-Screen Up, ~
                        R:Scroll Down, ~
                        R2:System Menu"))))

(defmethod (code-window-mixin :mouse-click) (button x y)
  x y ;ignored
  (selectq button
    (#\mouse-l-1 (send self ':home))
    (#\mouse-m-1 (send self ':scroll))
    (#\mouse-m-2 (send self ':scroll (multiple-value-bind (x y)
                                         (send self ':size-in-characters)
                                       x ;ignored
                                       (// y 2))))
    (#\mouse-r-1 (send self ':scroll -1))
    (#\mouse-r-2 (tv:mouse-call-system-menu))
    (t (beep)))
  t)

;;; CODE-WINDOW
;;;
;;;  This is just a TV:WINDOW with CODE-WINDOW-MIXIN

(defflavor code-window () (box-drawing-mixin code-window-mixin tv:window)
  (:documentation
   "A window which handles display of a piece of code.")
  (:default-init-plist
    :blinker-p nil
    :save-bits t
    :more-p nil))

;;; CODE-PANE
;;;
;;;  This is just adds TV:PANE-MIXIN to a CODE-WINDOW

(defflavor code-pane () (tv:pane-mixin code-window)
  (:documentation
   "A window pane which handles display of a piece of code."))

(compile-flavor-methods code-window code-pane)
