;;; -*- Mode: LISP; Package: ZL -*-
;;;
;;; Stand-ins for Lisp Machine system facilities the KBE sources mention
;;; at load time: the PP pretty printer (LMLIB; PP), TIME:, SI:, TV:, FS:.
;;; Headless, most of them do nothing.

(in-package "ZL")

(defmacro %zl-define-stub-macro (name)
  `(cl:defmacro ,name (&rest ignore) (declare (ignore ignore)) nil))

;;; PP: printing directives for code (used for display only)
(%zl-define-stub-macro pp::define-list-print-function)
(%zl-define-stub-macro pp::define-same-as)
(%zl-define-stub-macro pp::define-printing-directive)
(%zl-define-stub-macro pp::define-generalized-print-function)
(cl:defun pp::pp (object &optional (stream *standard-output*))
  (cl:let ((*print-pretty* t)) (cl:prin1 object stream) (terpri stream)))
(defvar pp::*print-right-margin* 79)

;;; TIME:
(cl:defun time::get-universal-time () (get-universal-time))
(cl:defun time::print-current-date (&optional (stream *standard-output*))
  (multiple-value-bind (s mi h d mo y dow) (decode-universal-time (get-universal-time))
    (cl:format stream "~A the ~:R of ~A, ~D; ~D:~2,'0D:~2,'0D ~:[am~;pm~]"
               (nth dow '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                          "Saturday" "Sunday"))
               d (nth (1- mo) '("January" "February" "March" "April" "May" "June" "July"
                                "August" "September" "October" "November" "December"))
               y (cl:let ((h12 (mod h 12))) (cl:if (zerop h12) 12 h12)) mi s (cl:>= h 12))))
(cl:defun time::print-current-time (&optional (stream *standard-output*))
  (time::print-current-date stream))
(cl:defun time::fixnum-microsecond-time ()
  (mod (floor (* (get-internal-real-time) 1000000) internal-time-units-per-second)
       most-positive-fixnum))

(dolist (s '(pp::define-list-print-function pp::define-same-as
             pp::define-printing-directive pp::define-generalized-print-function
             pp::pp pp::*print-right-margin*
             time::get-universal-time time::print-current-date
             time::print-current-time time::fixnum-microsecond-time
             sys::error sys::condition sys::abort sys::package-locked
             sys::read-end-of-file sys::pdl-overflow sys::proceed-types
             dbg::proceedable-ferror))
  (export (list s) (symbol-package s)))
