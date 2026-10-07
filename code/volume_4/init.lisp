;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>INIT.LISP
;;; PDF pages 242-245 (printed source pages 1-4).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; Jazzy PA Herald

(defvar *print-PA-herald* t)
(defun print-PA-herald ()
  (when *print-PA-herald*
    (send standard-output ':fresh-line)
    (let ((extra (* 2 (+ 5 ;pseudo-VSP
                         3 ;for the :increment-cursorpos below
                         (font-char-height ;1 line of typeout in this font
                          (send standard-output ':current-font)))))
          (y (cadr (multiple-value-list
                    (send standard-output ':read-cursorpos))))
          (line-height (send standard-output ':line-height)))
      (send standard-output ':clear-eof)
      (send standard-output ':increment-cursorpos 3 3)
      (format t "The MIT Artificial Intelligence Laboratory presents...")
      (let ((h (+ extra (font-char-height fonts:43vxms)))
            (w (send standard-output ':inside-size)))
        (send standard-output ':display-x-y-centered-string
              "Programmer's Apprentice" 0 y w (+ y h) fonts:43vxms)
        (let ((b (+ y h)) (l (- w 2)))
          (send standard-output ':draw-line 0 y      l y      tv:alu-xor)
          (send standard-output ':draw-line 0 (1+ y) 0 (1- b) tv:alu-xor)
          (send standard-output ':draw-line 0 b      l b      tv:alu-xor)
          (send standard-output ':draw-line l (1+ y) l (1- b) tv:alu-xor)
          (let* ((tab-stop (+ b 5)) ;pseudo-VSP
                 (delta (\ tab-stop line-height)))
            (if (not (zerop delta))
                (setq tab-stop (+ tab-stop (- line-height delta))))
            (send standard-output ':set-cursorpos 0 tab-stop)))))))

;;; Support Functions
;;; IRRITATING-NOISE
;;; Two long low-pitched tones. Ideal for errors and such,
                                        ; This noise is currently not assigned.    :
(defun irritating-noise (&optional name &rest ignore)
  name ; ignored
  (sys:%beep 5000. 600000.)
  (sys:%beep 6000. 600000.))
 ;;; ZIPPY-NOISE-UP
;;; ZIPPY-NOISE-DOWN
;;; A long slide up/down. Good for something that toggles.
                                        ; This noise is currently not assigned.
(defun zippy-noise-internal (&optional (upward?  t)
                               (start    2700.)
                               (end      300. )
                               (step     25.)
                               (duration 6000.))
  (cond (upward? (setq step (- step)))
        (t       (setq end (prog1 start (setq start end)))))
  (do ((pitch start (+ pitch step)))
      ((= pitch end))
    (sys:%beep pitch duration)))
(defun zippy-noise-down (&optional name &rest ignore)
  name ; ignored
  (zippy-noise-internal nil))
(defun zippy-noise-up (&optional name &rest ignore)
  name ; ignored
  (zippy-noise-internal t))
;;; PLEASANT-NOISE-1
;;; PLEASANT-NOISE-2
;;; Short high-pitched noises. #2 is slightly longer than #1.
(defun pleasant-noise-1 (&optional name &rest  ignore) ;short
  name ; ignored
  (sys:%beep 290. 100000.)
  (sys:%beep 300. 100000.)
  (sys:%beep 200. 100000.))
(defun pleasant-noise-2 (&optional name &rest  ignore) ;slightly longer
  name ; ignored
  (dotimes (i 3)
    (sys:%beep 200. 100000.)
    (sys:%beep 400. 100000.)))

;;; Initialization
;;; Noise initializations
#-3600 ;The 3600 makes no noise
(set-noise-for 'code-ready    #'pleasant-noise-1)
#-3600 ;The 3600 makes no noise                '
(set-noise-for 'display-ready #'pleasant-noise-1)
(define-system-initialization system-keys
  (add-system-key #/A t   "Ada"                      'ada-listener)
  (add-system-key #/B t   "Background Window (KBE)"  'actor-window nil)
  (add-system-key #/E nil "Editor (Zmacs//KBE)"      'edit-frame)
  (add-system-key #/K t   "KBE"                      'simple-frame)
  (add-system-key #/Z t   "Editor (Zmacs)"           'zwei:zmacs-frame) )
(define-system-initial ization setup-Gprint
  (setq user:Gcheckrecursion nil))
(define-system-initialization setup-pp
  (setf (pp:generalized-print-function-enabled-p  'fact) T)
  (setq prin1 #'pp:top-write))
