;;; -*- Mode: LISP; Package: CL-USER -*-
;;;
;;; Headless KBE self-test driver.
;;;
;;;   sbcl --script runtime/run-test.lisp sqrt
;;;
;;; Mirrors RUN-KBE-TEST (volume_4/test.lisp): each line of
;;; runtime/demo/<name>.kbe is run with (EVAL-COMMAND (PARSE-COMMAND line))
;;; inside WITH-INTERFACE-DISABLED, then the code KBE generated for each
;;; DEFUN in <name>.lisp is compared with EQUAL. Progress goes to
;;; runtime/build/progress.log as it happens.

(in-package :cl-user)

(load (merge-pathnames "load.lisp" (make-pathname :name nil :type nil :defaults *load-truename*)))
(load-kbe)
(load-kbe '("../runtime/patches"))

(defun log-progress (fmt &rest args)
  (with-open-file (log *kbe-progress-log* :direction :output :if-exists :append
                       :if-does-not-exist :create)
    (apply #'format log fmt args) (terpri log) (finish-output log)))

(defun read-zl-forms (path)
  (let ((*package* (find-package "KBE")) (*readtable* zl::*zl-readtable*) (eof (list nil)))
    (with-open-file (in path)
      (loop for f = (read in nil eof) until (eq f eof) collect f))))

(defun read-lines (path)
  (with-open-file (in path)
    (loop for l = (read-line in nil) while l
          unless (zerop (length (string-trim " " l))) collect l)))

(require :sb-posix)

(defun eval-command-string (command)
  (eval (list (intern "WITH-INTERFACE-DISABLED" "KBE")
              (list (intern "EVAL-COMMAND" "KBE")
                    (list (intern "PARSE-COMMAND" "KBE") command)))))

(defun run-kbe-test-headless (name)
  (let* ((demo (merge-pathnames "demo/" *kbe-runtime-dir*))
         (commands (read-lines (make-pathname :name name :type "kbe" :defaults demo)))
         (expected (read-zl-forms (make-pathname :name name :type "lisp" :defaults demo)))
         (*package* (find-package "KBE"))
         (*print-circle* t) (*print-length* 50) (*print-level* 12)
         (fail nil) (n 0))
    (funcall (intern "INITIALIZE-SYSTEM" "KBE"))
    (log-progress "== test ~A: ~D commands" name (length commands))
    (dolist (command commands)
      (incf n)
      (log-progress "command ~D: ~A" n command)
      (format t "~&;;; >~A~%" command)
      (handler-case
          (let ((result
                  (handler-bind ((error (lambda (c)
                                          ;; KBE_BACKTRACE=1 prints where errors happen
                                          (when (sb-posix:getenv "KBE_BACKTRACE")
                                            (format t "~&;;; ERROR ~A~%" c)
                                            (sb-debug:print-backtrace :count 25)))))
                    (eval-command-string command))))
            (format t ";;;   => ~S~%" result))
        (error (c)
          (setq fail (format nil "Error during ~S: ~A" command c))
          (format t "~&;;; ERROR: ~A~%" c)
          (log-progress "  ERROR: ~A" c)
          (return))))
    (dolist (good expected)
      (let* ((fn (cadr good))
             (code (ignore-errors (copy-tree (funcall (intern "CODE-FOR-DEFUN" "KBE") fn)))))
        (format t "~&~%;;; ~S: ~:[MISMATCH~;matches~]~%;; generated:~%~S~%;; expected:~%~S~%"
                fn (equal code good) code good)
        (unless (equal code good) (setq fail (or fail "Bad code generated.")))))
    (format t "~&~%;;; ~A sequence ~:[failed: ~A~;was successful.~]~%" name (not fail) fail)
    (log-progress "== result: ~:[FAILED ~A~;OK~]" (not fail) fail)
    (not fail)))

(run-kbe-test-headless (or (second sb-ext:*posix-argv*) "sqrt"))
