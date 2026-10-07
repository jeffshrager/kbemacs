;;; -*- Mode:LISP; Package:TV; Base:10; Fonts:MEDFNT; -*-

;;; Source: OZ:<KBE.SOURCE>MOUSE-SENSITIVE-REGIONS.LISP.15, 30 July 1983 22:20:58
;;; PDF pages 44-47 (printed source pages 1-4).

;;; (c) 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; This file defines an abstraction for dealing with a window with mouse
;;; sensitive output to a window (built on OVERLAPPING-REGIONS-WINDOW.
;;; A TV:MOUSE-SENSITIVE-REGIONS window is intended to have the following
;;; style of interface:
;;; ESETQ x
;;; (TV: MAKE-WINDOW 'TV:MOUSE-SENSITIVE-REGIONS-WINDOW ':EDGES-FROM ':MOUSE))
;;; (FORMAT X "Hi, ~M is mouse sensitive" "foo")
;;; (SEND X ':CHOOSE) ;left click on "foo"
;;; foo",  tLEbt
;;; Here's another example:
;;; (FORMAT X "~%~M"
;;; '(("This whole ~M is ~M and so are some of its ~M."
;;; ("sentence" SENTENCE "Click here for word 'sentence'")
;;; sensitive" MOUSE-SENSITIVE "Click here for 'mouse sensitive'")
;;; ("words" WORDS "Click here for 'words'"))
;;; WHOLE-SENTENCE
;;; "Click here for whole sentence") )
;;; (SEND X ':CHOOSE) ;middle click on "This whole ... its words"
;;; => WHOLE~SENTENCE, :MIDDLE
;;; The format command this sets up is ~M. It works on non-mouse-sensitive
;;; windows, but doesn't make its output mouse-sensitive.
;;; NOTE: Currently, ~M items must not extend over more than one line. This
;;; is fixable in principle, but I haven't gotten to it yet.
;;; ~M takes one argument.
;;; If that argument is an atom, it is displayed with PRINC.
;;; The result of :CHOOSE on this thing will be the atom itself.
;;; The mouse documentation will say something vanilla about clicking to select.
;;; If it is a list, its car will be displayed with PRINC.
;;; The result of :CHOOSE will be the cadr of the list
;;; The mouse documentation will be the caddr of the list or something
;;; vanilla about clicking to select otherwise.
;;; ~:M also takes one argument. It allows recursive entries.
;;; The argument must be a list.
;;; The car of the list must be a list of form (format-string . format-args).
;;; The result of :CHOOSE will be the cadr of the list.
;;; The mouse documentation will be the caddr of the list or something
;;; vanilla about clicking to select otherwise.

(defflavor mouse-sensitive-regions-mixin () ()
           (:required-flavors overlapping-regions-mixin minimum-window)
           (:default-init-plist
               :blinker-p nil
             :save-bits t
             :region-filter #'mouse-sensitive-regions-mixin-filter
             :documentation-handler #'caddr))
(defmethod (mouse-sensitive-regions-mixin :after :end-of-page-exception)
    (&rest ignore)
  (let ((new-regions nil))
    (dolist (region (send self ':regions))
      (let ((marker-cell (nthcdr 4 (car region))))
        (when (not (car marker-cell))
          (setf (car marker-cell) t)                                        ;
          (push region new-regions))))
    (send self ':set-regions new-regions)))
(defwhopper (mouse-sensitive-regions-mixin :mouse-sensitive-region) (&rest stuff)
  (multiple-value-bind (ignore y)
      (send self ':read-cursorpos)
    (let ((*y-pos* (+ y (send self ':line-height))))
      (declare (special *y-pos*))
      (lexpr-continue-whopper stuff))))
(defun mouse-sensitive-regions-mixin-filter (x)
  (declare (special *y-pos*))
  (let ((coordinates (car x)))
    (or (< *y-pos* (nth 3 coordinates))
        (not (nth 4 coordinates)))))
(defflavor mouse-sensitive-regions-window ()
  (mouse-sensitive-regions-mixin
   overlapping-regions-window) )
(defflavor mouse-sensitive-regions-pane  ()
  (mouse-sensitive-regions-mixin
   overlapping-regions-pane) )
(defmethod (mouse-sensitive-regions-mixin  :choose) ()
  (do ((blip  (send self ':any-tyi) (send self ':any-tyi)))
      (nil)  ;loop forever
    (cond  ((and (not (atom blip))
                 (eq (car blip) ':mouse-click))
            (let ((region (cadr  blip))
                  (button (caddr blip)))
              (caseq button
                     ((#\mouse-r-1) (return (cadr region) ':right))
                     ((#\mouse-m-1) (return (cadr region) ':middle))
                     ((#\mouse-l-1) (return (cadr region) ':left))
                     ((#\mouse-r-2) (mouse-call-system-menu) )
                     (t (beep)))))
           (t (beep)))))
 ;;; Interaction using FORMAT.
 ;;; (WITH-MOUSE-SENSITIVE-REGION window . body)
 ;;; This is a sub-primitive for implementing the format option described below.
 ;;; It lets you output to a window with MOUSE-SENSITIVE-REGIONS-MIXIN
 ;;; making the region mouse-sensitive. The body of the form should do output
 ;;; to STANDARD-OUTPUT.
 ;;; The return values will become the entries in the region. eg,
 ;;; if you return the values 3, 4 then (coordinates 3 4) will be the
 ;;; region data.

(defmacro with-mouse-sensitive-region (window &body body)
  (let ((start-x (gensym))
        (start-y (gensym)))
    `(let ((standard-output  ,window) )
      (multiple-value-bind  (,start-x ,start-y)
          (send standard-output  ':read-cursorpos)
        (let  ((data (multiple-value-list (progn ,@body))))
          (when (typep standard-output 'mouse-sensitive-regions-mixin)
            (multiple-value-bind (end-x end-y)
                (send standard-output ':read-cursorpos)
              (cond ((> end-y ,start-y) ;output extends over multiple lines?
                     (ferror "We can't handle multi-line mouse things yet."))
                    (t
                     (send standard-output
                           ':set-regions
                           (append
                            (send standard-output ':regions)
                            (ncons
                             (cons (list ,start-x ,start-y end-x
                                         (+ end-y ;bottom of line, not top
                                            (send standard-output
                                                  *: line-height) )
                                         nil)
                                   data))))))))
          (values-list data))))))
(defprop format:m mouse-sensitive-output format:format-ctl-one-arg)
(defun mouse-sensitive-output (obj ignore)
  (with-mouse-sensitive-region standard-output
    (let ((documentation "Any button to select this item.")
          item)
      (cond (format:colon-flag
             (lexpr-funcall #'format t (car obj))
             (setq item (or (cadr obj) item))
             (setq item (cadr obj)
                   documentation (or (caddr obj) documentation)))
            (t
             (cond ((atom obj)
                    (princ obj)
                    (setq item obj))
                   (t                                            ;
                    (princ (car obj))
                    (setq item (cadr obj)
                          documentation  (or (caddr obj) documentation))))))
      (values item documentation) )))

(compile-flavor-methods mouse-sensitive-regions-window
                        mouse-sensitive-regions-pane)
