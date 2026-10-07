;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_2.pdf
;;; PDF pages 62-69; listing util.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.



(declare (*expr analyze code))
;;; PROCESS reads a program, analyzes it, and writes a readable PLAN file.
;;; Symbols select tagged entries in PGMS; strings specify filenames.
(defun process (arg)
  (cond ((symbolp arg)
         (process-aux arg nil (file-default-fn2 (get-pname arg) 'PLAN ':newest)))
        ((stringp arg)
         (process-aux nil (file-default-fn2 arg nil ':newest)
                      (file-default-fn2 arg 'PLAN ':newest)))
        (t (ferror "Arg to PROCESS must be a symbol or a string: ~S" arg))))
(defun process-aux (name input-filename output-filename)
  (cond ((and input-filename (not (probef input-filename)))
         (ferror "Input file doesn't exist: ~A" input-filename))
        ((probef output-filename)
         (ferror "Output file exists: ~A" output-filename)))
  (with-open-file (outstream output-filename ':OUT)
    (cond (name ;if we got a name, get it from the PGMS file...
           (process-1 (*pread name) outstream nil))
          (t ;else munch up everything in the file we were given...
           (with-open-file (instream input-filename ':IN)
             (do ((pgm (read instream nil) (read instream nil)) (flag nil t))
                 ((null pgm))
               (process-1 pgm outstream flag))))))
  (db-audit)
  (beep))
(defun process-1 (pgm outstream page?)
  ;;; PDF page 63
  (let ((segcase (analyze (eval pgm))))
    (if page? (tyo #\page outstream))
    (disp-print (~name (??& segcase @seg)) outstream)))

;;; PDF page 64
;;; Non-standard functions referenced by code produced by KBE.
(defun *equal (x y) (equal x y))
(defun arrayfetch (array index) ;only vectors for now
  (arraycall t array index))
(defun arraystore (array index item) ;only vectors for now
  (store (arraycall t array index) item))

;;; PDF page 65
(defun bdb (context) (set-db-context context) (push-db-context))
(defun rdb (context)
  (if (null context) (barf "No context -- RDB"))
  (restore-db-context context))

;;; PDF page 66
;;; Tagged example program records start with ;^Akey, followed by a form.
;;; ^A is the second character on the line; the final tag is |eof|.
(defvar *pgms-filename* "KBE: MISC; PGMS LISP >" "Sample program storage")
(defvar last-*pread nil "Used by *PREAD as a history mechanism.")
(defun *pread (thing)
  (setq last-*pread (or (*pread-1 thing *pgms-filename*)
                       (grumble "Test program not found: ~S" thing)))
  last-*pread)
(defun *pread-1 (key file) ;should probably bind PACKAGE --kmp
  (with-open-file (instream file ':in)
    (do ((line (readline instream nil) (readline instream nil)))
        ((null line) nil) ;Return NIL if we fall off the end
      (cond ((and (> (string-length line) 2)
                  (eq (aref line 1) #/^A)
                  (eq (read-from-string line nil 2) key))
             (return (read instream))))))) ;Return tagged form if found
(defun *snarf (item)
  (let ((*db-check-facts* nil)) (code (analyze (eval (*pread item))))))

;;; PDF page 67
(defun *disp (form) (disp-print form standard-output))
(defun *disp-dump (name &optional (filename (get-pname name)))
  (let ((output-filename (file-default-fn2 filename 'PLAN ':newest)))
    (cond ((probef output-filename)
           (ferror "Output file exists: ~A" output-filename)))
    (with-open-file (outstream output-filename ':OUT)
      (disp-print name outstream))
    'done))
;;; Print a readable plan that can be read back to restore the database.
(defun disp-print (name f)
  (let ((segcase (~01 ~segcase (??& * @seg 'lambda-exp name))))
    (terpri f)
    (multiple-value-bind (def pas) (construct-plan-def (~seg segcase))
      (cond ((??& segcase plantype) (format f "~%'(~:=)~%" pas)))
      (pp:pp def f))))

;;; PDF page 68
;;; Do not load this output: it contains triple definitions of everything.
(declare (special <anal <code))
(defun *print-test (name)
  (let ((saved-context *db-context*)
        (output-filename (file-default-fn2 (get-pname name) 'TEST ':newest)))
    (cond ((probef output-filename)
           (ferror "Output file exists: ~A" output-filename)))
    (with-open-file (outstream output-filename ':OUT)
      (unwind-protect
          (progn (disp-print name outstream)
                 (tyo #\form outstream)
                 (set-db-context <code)
                 (disp-print name outstream)
                 (tyo #\form outstream)
                 (set-db-context <anal)
                 (disp-print name outstream))
        (set-db-context saved-context)))
    (beep)
    'done))

;;; PDF page 69
;;; Convenience macros for type-in; they should not be called by code.
(defmacro pread (&rest stuff)
  (cond (stuff `(*pread ',(car stuff))) (t 'last-*pread)))
(defmacro peval (item) `(eval (*pread ',item)))
(defmacro snarf (item) `(*snarf ',item))
(defmacro disp (form) `(*disp ',form))
(defmacro disp-dump (name) `(*disp-dump ',name))
(defmacro print-test (x) `(*print-test ',x))
(defmacro run-test (test)
  `(progn (code (analyze (peval ,test))) (print-test ,test)))
