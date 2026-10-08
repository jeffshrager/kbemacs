;;; -*- Mode: LISP; Package: ZL -*-
;;;
;;; A readtable for Zetalisp source text.
;;;
;;;   /x       escapes x, in symbols and in strings ("Retry /"~A/"", //)
;;;   \        is an ordinary constituent (so \\ is the remainder function)
;;;   #/x      the character x
;;;   #\name   Lisp Machine character names (#\End, #\system, #\c-Z, ...)
;;;   #+3600   feature tests; this runtime has the features of the 3600
;;;            that produced 358-test.lisp
;;;   #Q/#M    Lisp Machine / Maclisp conditionals
;;;
;;; KBE installs its own [ ] { } and #{ read macros at load time through
;;; SET-SYNTAX-MACRO-CHAR and friends (readmacro.lisp, comment-macro.lisp).

(in-package "ZL")

(defvar *zl-features* '("3600" "SYMBOLICS" "LISPM" "ZETALISP")
  "Feature names true for #+ and #-.")

(defvar *zl-readtable* (copy-readtable nil))

;;;; Characters

(defparameter %zl-char-names
  `(("SPACE" . #\Space) ("SP" . #\Space)
    ("RETURN" . #\Newline) ("CR" . #\Newline) ("NEWLINE" . #\Newline)
    ("LINE" . #\Linefeed) ("LINEFEED" . #\Linefeed) ("LF" . #\Linefeed)
    ("TAB" . #\Tab) ("PAGE" . #\Page) ("FORM" . #\Page) ("FF" . #\Page)
    ("RUBOUT" . #\Rubout) ("DELETE" . #\Rubout)
    ("BACKSPACE" . #\Backspace) ("BS" . #\Backspace)
    ("ALTMODE" . #\Esc) ("ESCAPE" . #\Esc) ("ALT" . #\Esc)
    ("NULL" . #\Nul) ("NUL" . #\Nul)))

(defvar %zl-private-chars (make-hash-table :test #'equalp)
  "Lisp Machine character names with no ASCII equivalent (#\End, #\c-Z),
   each mapped to a distinct private-use character.")

(cl:defun %zl-char-named (name)
  (cl:let ((name (string-upcase name)))
    (or (cdr (cl:assoc name %zl-char-names :test #'string=))
        (gethash name %zl-private-chars)
        (setf (gethash name %zl-private-chars)
              (code-char (cl:+ #xE000 (hash-table-count %zl-private-chars)))))))

(cl:defun %zl-read-slash-char (stream sub-char arg)
  (declare (ignore sub-char arg))
  (read-char stream t nil t))

(cl:defun %zl-read-named-char (stream sub-char arg)
  (declare (ignore sub-char arg))
  (cl:let ((chars (list (read-char stream t nil t))))
    (loop for c = (peek-char nil stream nil nil t)
          while (and c (or (alphanumericp c) (char= c #\-)))
          do (push (read-char stream) chars))
    (cl:let ((name (coerce (nreverse chars) 'string)))
      (cond (*read-suppress* nil)
            ((= (length name) 1) (char name 0))
            (t (%zl-char-named name))))))

;;;; Features

(cl:defun %zl-feature-true-p (expr)
  (cond ((or (symbolp expr) (numberp expr))
         (cl:member (string-upcase (princ-to-string expr)) *zl-features*
                    :test #'string=))
        ((eq (car expr) :and) (cl:every #'%zl-feature-true-p (cdr expr)))
        ((eq (car expr) :or) (cl:some #'%zl-feature-true-p (cdr expr)))
        ((eq (car expr) :not) (not (%zl-feature-true-p (cadr expr))))
        (t nil)))

(cl:defun %zl-read-feature-expr (stream)
  (cl:let ((*package* (find-package "KEYWORD"))
           (*read-suppress* nil))
    (cl:read stream t nil t)))

(cl:defun %zl-read-conditional (stream wanted)
  "Reads a feature expression and the next form; returns the form if the
   feature's truth is WANTED, else no values."
  (cl:if (eq (not (%zl-feature-true-p (%zl-read-feature-expr stream)))
             (not wanted))
      (cl:read stream t nil t)
      (cl:let ((*read-suppress* t)) (cl:read stream t nil t) (values))))

(cl:defun %zl-read-plus (stream sub-char arg)
  (declare (ignore sub-char arg))
  (%zl-read-conditional stream t))

(cl:defun %zl-read-minus (stream sub-char arg)
  (declare (ignore sub-char arg))
  (%zl-read-conditional stream nil))

(cl:defun %zl-read-lispm-only (stream sub-char arg)
  (declare (ignore sub-char arg))
  (cl:read stream t nil t))

(cl:defun %zl-read-maclisp-only (stream sub-char arg)
  (declare (ignore sub-char arg))
  (cl:let ((*read-suppress* t)) (cl:read stream t nil t) (values)))

;;;; Dots
;;;
;;; Zetalisp reads a token of dots, as in '(...) or (eq x '...), as a
;;; symbol; the CL reader signals "too many dots". So in this readtable "."
;;; is a non-terminating macro character (inside tokens like 1.5 or foo.bar
;;; it is still a constituent), and "(" has its own list reader, which
;;; recognizes the consing dot itself.

(defvar %zl-token-readtable nil
  "The Zetalisp readtable without the dot and list readers, for parsing
   tokens that start with a dot (.5).")

(cl:defun %zl-delimiter-p (c)
  (or (null c)
      (cl:member c '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed))
      (multiple-value-bind (fn non-terminating?) (get-macro-character c)
        (and fn (not non-terminating?)))))

(cl:defun %zl-read-dot (stream char)
  (declare (ignore char))
  ;; Collect the token that starts with this dot.
  (cl:let ((chars (list #\.)))
    (loop for c = (peek-char nil stream nil nil t)
          until (%zl-delimiter-p c)
          do (push (read-char stream) chars)
             (when (char= c #\/)               ;escaped character
               (push (read-char stream t nil t) chars)))
    (cl:let ((token (coerce (nreverse chars) 'string)))
      (cond (*read-suppress* nil)
            ((cl:every (lambda (c) (char= c #\.)) token)
             (cl:if (= (length token) 1)
                 (cl:error 'reader-error :stream stream)
                 (cl:intern token *package*)))
            (t (cl:let ((*readtable* %zl-token-readtable))
                 (values (read-from-string token))))))))

(cl:defun %zl-read-maybe-nothing (stream)
  "Reads the next object, returning no values for a comment or a
   suppressed #+ form."
  (cl:let* ((c (read-char stream t nil t))
            (fn (get-macro-character c)))
    (cond (fn (funcall fn stream c))
          (t (unread-char c stream)
             (cl:read stream t nil t)))))

(cl:defun %zl-consing-dot-p (stream)
  "True if the stream is at a lone dot (consumes it in that case)."
  (cl:when (eql (peek-char nil stream nil nil t) #\.)
    (read-char stream)
    (cl:if (%zl-delimiter-p (peek-char nil stream nil nil t))
        t
        (progn (unread-char #\. stream) nil))))

(cl:defun %zl-read-list (stream char)
  (declare (ignore char))
  (cl:let ((items '()))
    (loop
      (cl:let ((c (peek-char t stream t nil t)))
        (cond ((char= c #\))
               (read-char stream)
               (cl:return (nreverse items)))
              ((%zl-consing-dot-p stream)
               (cl:when (and (null items) (not *read-suppress*))
                 (cl:error 'reader-error :stream stream))
               (cl:let ((tail (cl:read stream t nil t)))
                 ;; skip comments up to the close paren
                 (loop for c2 = (peek-char t stream t nil t)
                       until (char= c2 #\))
                       do (cl:when (multiple-value-list (%zl-read-maybe-nothing stream))
                            (cl:unless *read-suppress*
                              (cl:error 'reader-error :stream stream))))
                 (read-char stream)
                 (cl:return (nreconc items tail))))
              (t
               (cl:let ((vals (multiple-value-list (%zl-read-maybe-nothing stream))))
                 (cl:when vals (push (car vals) items)))))))))

;;;; The readtable

(cl:let ((rt *zl-readtable*))
  (set-syntax-from-char #\/ #\\ rt)     ;single escape
  (set-syntax-from-char #\\ #\a rt)     ;constituent
  (set-dispatch-macro-character #\# #\/ #'%zl-read-slash-char rt)
  (set-dispatch-macro-character #\# #\\ #'%zl-read-named-char rt)
  (set-dispatch-macro-character #\# #\+ #'%zl-read-plus rt)
  (set-dispatch-macro-character #\# #\- #'%zl-read-minus rt)
  (set-dispatch-macro-character #\# #\Q #'%zl-read-lispm-only rt)
  (set-dispatch-macro-character #\# #\M #'%zl-read-maclisp-only rt)
  (setq %zl-token-readtable (copy-readtable rt))
  (set-macro-character #\. #'%zl-read-dot t rt)
  (set-macro-character #\( #'%zl-read-list nil rt))

;;;; Read macros defined by Zetalisp programs
;;;
;;; A Zetalisp macro-character function takes (list-so-far stream). It may
;;; return three values (object nil t) to replace the list being read with
;;; OBJECT. KBE's {X, modified} does this, returning list-so-far with
;;; elements NCONCed on. We pass a fresh one-element list as list-so-far
;;; and turn such a result into a splice marker; %ZL-RESOLVE-SPLICES then
;;; splices the added elements into place.

(cl:defstruct (%zl-splice (:constructor %zl-make-splice (items))) items)

(cl:defun %zl-call-zl-macro (fn stream)
  (cl:let ((list-so-far (list '%zl-list-so-far)))
    (multiple-value-bind (object ignore replace?) (funcall fn list-so-far stream)
      (declare (ignore ignore))
      (cl:if (and replace? (consp object) (eq object list-so-far))
          (%zl-make-splice (cdr object))
          object))))

(cl:defun %zl-resolve-splices (form)
  (cond ((%zl-splice-p form) (%zl-splice-items form))
        ((atom form) form)
        (t (cl:let ((out '()))
             (cl:do ((l form (cdr l)))
                 ((atom l) (nreconc out (cl:if l (%zl-resolve-splices l))))
               (cl:let ((x (car l)))
                 (cl:if (%zl-splice-p x)
                     (setq out (revappend (%zl-splice-items x) out))
                     (push (%zl-resolve-splices x) out))))))))

(cl:defun set-syntax-macro-char (char fn &optional (readtable *readtable*))
  (set-macro-character (%zl-char char)
                       (lambda (stream c)
                         (declare (ignore c))
                         (%zl-call-zl-macro fn stream))
                       nil
                       readtable)
  t)

(cl:defun |SET-SYNTAX-#-MACRO-CHAR| (char fn &optional (readtable *readtable*))
  (set-dispatch-macro-character #\# (%zl-char char)
                                (lambda (stream c arg)
                                  (declare (ignore c arg))
                                  (%zl-call-zl-macro fn stream))
                                readtable)
  t)

(cl:defun set-syntax-from-description (char description &optional (readtable *readtable*))
  (cl:let ((char (%zl-char char)))
    (cond ((string-equal (symbol-name description) "SINGLE")
           ;; The character reads as a symbol by itself.
           (set-macro-character char
                                (lambda (stream c)
                                  (declare (ignore stream))
                                  (cl:intern (string c) *package*))
                                nil readtable))
          (t (cl:error "Unknown syntax description ~S" description))))
  t)

(cl:defun set-editor-list-syntax-from-character (&rest ignore)
  (declare (ignore ignore))
  nil)

;;;; Reading Zetalisp text from strings

(cl:defun zl-read-from-string (string &optional eof-error-p eof-value)
  (cl:let ((*readtable* *zl-readtable*))
    (read-from-string string eof-error-p eof-value)))
