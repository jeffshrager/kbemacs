;;; -*- Mode: LISP; Package: CL-USER -*-
;;;
;;; Loads the headless KBE core under SBCL:
;;;
;;;   sbcl --script runtime/load.lisp
;;;
;;; or, from a REPL, (load "runtime/load.lisp"). The module order follows
;;; volume_1/sysdef.lisp, minus the editor, window and actor layers.

(in-package :cl-user)

(require :sb-cltl2)

(defparameter *kbe-runtime-dir*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defparameter *kbe-code-dir* (merge-pathnames "../code/" *kbe-runtime-dir*))

(defparameter *kbe-build-dir* (merge-pathnames "build/" *kbe-runtime-dir*))

(defun runtime-file (name) (merge-pathnames name *kbe-runtime-dir*))

;;;; Common Lisp parts: vendored libraries and the Zetalisp runtime

(defvar *recompiled* nil
  "Set once any runtime file is recompiled; later files then recompile too,
   since they may depend on its packages and macros.")

(defun compile-and-load (path)
  (let ((fasl (merge-pathnames
               (make-pathname :name (format nil "~A-~A"
                                            (car (last (pathname-directory path)))
                                            (pathname-name path))
                              :type "fasl")
               *kbe-build-dir*)))
    (ensure-directories-exist fasl)
    (handler-bind ((warning #'muffle-warning))
      (when (or *recompiled*
                (not (probe-file fasl))
                (> (file-write-date path) (file-write-date fasl)))
        (setq *recompiled* t)
        (compile-file path :output-file fasl :verbose nil :print nil))
      (load fasl))))

(dolist (f '("flavors/package" "flavors/util" "flavors/kkernel"
             "flavors/kernel" "flavors/flavors" "flavors/vanilla"
             "lets/package" "lets/utils" "lets/lets"))
  (compile-and-load (runtime-file (concatenate 'string f ".lisp"))))

(dolist (f '("packages" "zl-compat" "flavors-ext" "zl-conditions"
             "zl-reader"))
  (compile-and-load (runtime-file (concatenate 'string f ".lisp"))))

(compile-and-load (runtime-file "lispm-stubs.lisp"))
(compile-and-load (runtime-file "lispm-functions.lisp"))

;;; Export the Zetalisp vocabulary from ZL, so KBE (which uses only ZL)
;;; sees it: Common Lisp, Flavors and LetS externals, plus every symbol ZL
;;; defines. Other symbols that merely occur in the runtime's code (LOOP
;;; keywords such as FROM) stay internal; KBE must get its own, since it
;;; interns into their packages (FACTDEF: ~FROM in (SYMBOL-PACKAGE 'FROM)).
;;; Internal helpers are named %ZL-... Also exported: the stream and
;;; printer variable aliases (symbol macros, so not BOUNDP), and data
;;; shared with the KBE code, which compares against it.
(defparameter *zl-shared-data-symbols*
  (append (mapcar #'car zl::%zl-variable-aliases)
          '(zl::macro zl::fexpr zl::expr zl::lexpr zl::subst zl::named-subst
            zl::named-lambda
            ;; Zetalisp lambda-list keywords and declarations
            zl::&special zl::&local zl::&quote zl::&eval zl::&functional
            zl::&list-of zl::*expr zl::*fexpr zl::*lexpr)))

(let ((zl (find-package "ZL"))
      (externals (list (find-package "COMMON-LISP") (find-package "LETS")
                       (find-package "SPICE-FLAVORS"))))
  (do-symbols (s zl)
    (when (and (not (eql (search "%ZL-" (symbol-name s)) 0))
               (not (eq (symbol-package s) (find-package "KEYWORD")))
               (or (member s *zl-shared-data-symbols*)
                   (fboundp s) (boundp s) (macro-function s)
                   (some (lambda (p)
                           (eq (nth-value 1 (find-symbol (symbol-name s) p))
                               :external))
                         externals)))
      (export (list s) zl))))


;;;; Loading Zetalisp source

(defvar *kbe-load-errors* '()
  "List of (file form-summary condition) for forms that failed to load.")

(defvar *kbe-stop-on-error* nil
  "If true, enter the debugger on a load error instead of recording it.")

(defun form-summary (form)
  (if (consp form)
      (let ((*print-length* 3) (*print-level* 2))
        (prin1-to-string (if (consp (cdr form))
                             (list (car form) (cadr form))
                             form)))
      (prin1-to-string form)))

(defvar *kbe-compiler-log* (make-string-output-stream)
  "Compiler diagnostics from loading KBE source (see KBE-COMPILER-LOG).")

(defun continue-past-package-errors (c)
  ;; PP:FOO where FOO isn't external (or doesn't exist yet) in our stand-in
  ;; packages: use the symbol anyway.
  (let ((r (find-restart 'continue c)))
    (when r (invoke-restart r))))

(defun kbe-macro-form-p (form)
  "True for a call to a macro defined outside the COMMON-LISP package."
  (and (consp form) (symbolp (car form))
       (macro-function (car form))
       (not (eq (symbol-package (car form)) (find-package "COMMON-LISP")))))

(defun eval-toplevel (form)
  "Evaluates FORM as a compiler processes a toplevel form: toplevel PROGNs
   are processed subform by subform, and KBE/Zetalisp macros are expanded
   first. Maclisp-style toplevel (DECLARE (SPECIAL ...)) proclaims; other
   toplevel declarations, such as (DECLARE (DEFUN ...)) for the compiler,
   are dropped."
  (cond ((and (consp form) (eq (car form) 'declare))
         (loop for spec in (cdr form)
               when (and (consp spec) (eq (car spec) 'special))
                 do (proclaim spec)))
        ((and (consp form) (eq (car form) 'progn))
         (let ((value nil))
           (dolist (f (cdr form) value)
             (setq value (eval-toplevel f)))))
        ((kbe-macro-form-p form)
         (eval-toplevel (macroexpand-1 form)))
        (t (eval form))))

(defparameter *kbe-progress-log* (merge-pathnames "progress.log" *kbe-build-dir*)
  "Each form loaded is noted here as it starts (flushed), so a long or
   stuck run can be watched with tail -f.")

(defun note-progress (path n form)
  (with-open-file (log *kbe-progress-log* :direction :output
                       :if-exists :append :if-does-not-exist :create)
    (format log "~A ~A form ~D ~A~%"
            (multiple-value-bind (s m h) (get-decoded-time) (format nil "~2,'0D:~2,'0D:~2,'0D" h m s))
            (pathname-name path) n (form-summary form))
    (finish-output log)))

(defun eval-kbe-form (form path n)
  "Evaluates FORM. SBCL reports compile-time errors in a form as
   diagnostics (and defers them to run time), so those are recorded as load
   errors too."
  (note-progress path n form)
  (let* ((diagnostics (make-string-output-stream))
         (*error-output* diagnostics))
    (flet ((note-error (what)
             (push (list path (form-summary form) what) *kbe-load-errors*)
             (format t "~&;; ERROR in ~A, form ~D ~A:~%;;   ~A~%"
                     (pathname-name path) n (form-summary form) what)))
      (if *kbe-stop-on-error*
          (handler-bind ((warning #'muffle-warning)) (eval-toplevel form))
          (handler-case
              (handler-bind ((warning #'muffle-warning)) (eval-toplevel form))
            (error (c) (note-error c))))
      (let ((text (get-output-stream-string diagnostics)))
        (when (plusp (length text))
          (format *kbe-compiler-log* "~&;;; ~A form ~D ~A~%~A"
                  (pathname-name path) n (form-summary form) text)
          (when (search "caught ERROR" text)
            (note-error (string-trim '(#\Newline #\Space #\;)
                                     (subseq text 0 (min 600 (length text)))))))))))

(defun kbe-load (path)
  "Loads a Zetalisp source file into the KBE package, form by form."
  (let ((*package* (find-package "KBE"))
        (*readtable* zl::*zl-readtable*)
        (*read-base* 10)
        ;; DB facts contain circular context markers; printing one in an
        ;; error message must not recurse forever.
        (*print-circle* t)
        (*print-length* 50)
        (*print-level* 10)
        (eof (list :eof))
        (n 0))
    (with-open-file (in path :external-format :utf-8)
      (loop
        (let ((form
                (handler-case
                    (handler-bind ((sb-int:simple-reader-package-error
                                     #'continue-past-package-errors)
                                   ;; e.g. "Ignoring unmatched close
                                   ;; parenthesis": usually a transcription
                                   ;; error, so it is recorded.
                                   (warning
                                     (lambda (w)
                                       (push (list path (format nil "read after form ~D" n) w)
                                             *kbe-load-errors*)
                                       (format t "~&;; READ WARNING in ~A after form ~D (line ~D):~%;;   ~A~%"
                                               (pathname-name path) n
                                               (ignore-errors (1+ (count #\Newline (with-open-file (s path) (let ((str (make-string (file-position in)))) (read-sequence str s) str)))))
                                               w)
                                       (muffle-warning w))))
                      (read in nil eof))
                  (error (c)
                    (push (list path (format nil "read after form ~D" n) c)
                          *kbe-load-errors*)
                    (format t "~&;; READ ERROR in ~A after form ~D:~%;;   ~A~%"
                            (pathname-name path) n c)
                    (return)))))
          (when (eq form eof) (return))
          (incf n)
          (eval-kbe-form (zl::%zl-resolve-splices form) path n))))
    n))

(defun kbe-compiler-log ()
  (get-output-stream-string *kbe-compiler-log*))

;;;; The headless KBE core, in sysdef.lisp order

(defparameter *kbe-core-files*
  '(;; BASE
    "volume_1/basic" "volume_1/noise" "volume_1/sets" "volume_1/conditions"
    "volume_1/english" "volume_1/system-magic" "volume_1/flavor-magic"
    "volume_1/macro" "volume_1/kvars" "volume_1/kmac"
    "volume_1/interface-macros"
    ;; MBOX, BEHAVE, KUTIL
    "volume_1/mbox" "volume_1/post-office" "volume_1/behavior"
    "volume_1/events" "volume_1/actor" "volume_1/complete"
    ;; COMMNT RDMAC OUTIL DTRACE DUTIL DMAC DVARS DB GC DDEBUG
    "volume_1/comment-macro" "volume_1/readmacro" "volume_1/outil"
    "volume_1/dtrace" "volume_1/dutil" "volume_1/dmac" "volume_1/dvars"
    "volume_1/db" "volume_1/gc" "volume_1/ddebug"
    ;; SURFACE EXPAND PRIMOP DFLOW FNS
    "volume_2/surface" "volume_2/expand" "volume_2/primop" "volume_2/dflow"
    "volume_2/util" "volume_2/segtrn" "volume_2/analyze" "volume_2/coder"
    ;; PSEUDO, BRIDGE (ED0 depends on it in sysdef.lisp), ED0 ED1 ED2 ED3
    "volume_3/pseudo" "volume_4/bridge" "volume_3/dict" "volume_3/gram"
    "volume_3/refer" "volume_3/verbs" "volume_3/implement"
    ;; DEFLIB KBELIB DEMLIB
    "volume_3/deflib" "volume_3/syslib" "volume_3/kbelib" "volume_3/demo-lib"
    ;; EXPLAIN HANDLERS
    "volume_4/explain" "volume_4/document" "volume_4/pbm-handlers"
    ;; From CMDS: FOCUS has the code<->path functions (CODE->PATH,
    ;; PATH->CODE) that verbs.lisp uses, and FORMAT-EXPLANATION-FOR-COMMENT,
    ;; which TEST uses; its editor-only functions are never called headless.
    "volume_4/focus"
    ;; self-test
    "volume_4/test"))

(defun load-kbe (&optional (files *kbe-core-files*))
  (setq *kbe-load-errors* '())
  (ensure-directories-exist *kbe-progress-log*)
  (with-open-file (log *kbe-progress-log* :direction :output :if-exists :supersede)
    (format log "load-kbe started~%"))
  (dolist (f files)
    (let ((path (merge-pathnames (concatenate 'string f ".lisp") *kbe-code-dir*)))
      (format t "~&;; Loading ~A~%" f)
      (kbe-load path)))
  (format t "~&;; ~D load error~:P.~%" (length *kbe-load-errors*))
  (length *kbe-load-errors*))
