;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 85-95; listing basic.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; BASIC: Module of basic utility functions which may be used
;;;        by any other module in the KBE system.

;;; (CIRCULAR-LIST* x1 x2 ... xN l)
;;;
;;;  Like CIRCULAR-LIST, but takes LIST* conventions.

(defun circular-list* (&rest l)
  (declare (values circular-list))
  (lexpr-funcall #'lexpr-funcall #'circular-list l))

;;; (TEXT format-string . format-args)
;;;
;;;  Returns the string which would have been printed by the given
;;;  format string and format args.

(defun text (format-string &rest format-args)
  (declare (values text-string))
  (lexpr-funcall #'format nil format-string format-args))

;;; (GRUMBLE format-string . format-args)

(defun grumble (format-string &rest format-args)
  (format error-output "~&")
  (lexpr-funcall #'format error-output format-string format-args))

;;; (MAYBE-INTERN string pkg)
;;;
;;;  Like INTERN but if pkg is NIL, uses MAKE-SYMBOL to make an
;;;  uninterned symbol rather than using INTERN and interning on
;;;  the current package. If the specified package is locked, the
;;;  default package is assumed.

(defun maybe-intern (string pkg)
  (declare (values symbol))
  (setq string (string string))
  (values (cond ((not pkg) (make-symbol string t))
                (t (condition-case ()
                       (intern string pkg)
                     ((sys:package-locked) (intern string)))))))

;;; (SYMBOLCONC x y)
;;;
;;;  Returns a symbol which is the result of concatenating the
;;;  print names of two given objects.
;;;
;;;  The package used will be the package of the first of its
;;;  arguments that is a symbol. If no such argument or that
;;;  symbol is not interned, a gensym is produced.

(defun symbolconc (x y)
  (declare (values sym))
  (let ((base 10.) (*nopoint t))
    (maybe-intern (format nil "~A~A" x y)
                  (cond ((symbolp x) (symbol-package x))
                        ((symbolp y) (symbol-package y))))))

;;; (SYMBOL-PLURALIZE x)
;;;
;;;  Returns a symbol which is the result of pluralizing its
;;;  argument. The result is in the same package as the argument.

(defun symbol-pluralize (sym)
  (declare (values plural-sym))
  (maybe-intern (string-pluralize sym) (symbol-package sym)))

;;; (ADD-BUCKY-BITS char . bit-names)
;;; (REMOVE-BUCKY-BITS char . bit-names)
;;;
;;;  CHAR should be a fixnum character.
;;;  BIT-NAMES should be a list of bucky-bit names such as
;;;   :CONTROL, :META, :SUPER, or :HYPER.

(defun add-bucky-bits (char &rest bit-names)
  (lexpr-funcall #'set-bucky-bits 1 char bit-names))

(defun remove-bucky-bits (char &rest bit-names)
  (lexpr-funcall #'set-bucky-bits 0 char bit-names))

(defun set-bucky-bits (bit char &rest bit-names)
  (do ((c char (dpb bit
                    (selectq (car n)
                      ((:control) %%kbd-control)
                      ((:meta)    %%kbd-meta)
                      ((:super)   %%kbd-super)
                      ((:hyper)   %%kbd-hyper)
                      (otherwise
                       (ferror "~S is not the name of a bucky bit."
                               (car n))))
                    c))
       (n bit-names (cdr n)))
      ((null n) c)))

(defun bucky-char-name (char)
  (format nil "~:[~;Hyper-~]~:[~;Super-~]~:C"
          (not (zerop (ldb %%kbd-hyper   char)))
          (not (zerop (ldb %%kbd-super   char)))
          (remove-bucky-bits char ':hyper ':super)))

;;; (STRING-COPY string)
;;;
;;; Returns a copy of the given string.

(defun string-copy (string) (string-append string))

;;; (COPY tree)
;;;
;;;  This copies all levels of list structure in a given tree.
;;;
;;;  This is the same as COPYTREE. Why aren't we using COPYTREE? --kmp

(defun copy (tree) (subst nil nil tree))

;;; (GET-PREFIX sym)
;;;
;;;  Takes a symbol as an argument. If the symbol's print name contains
;;;  no trailing digits, the symbol is simply returned. If it does
;;;  simply returns it. If it does contain trailing digits, those digits
;;;  are stripped and the resulting string is interned in whatever package
;;;  sym was interned in (or not interned if sym wasn't).
;;;
;;;  (get-prefix 'foo1)          => FOO
;;;  (get-prefix '5fo3o)         => 5FO3O
;;;  (get-prefix (gensym))       => G ;not interned
;;;  (get-prefix ':foo3)         => :FOO

(defun get-prefix (sym)
  (declare (values sym-without-trailing-digits))
  (let ((pname (get-pname sym)))
    (let ((pos (1+ (string-reverse-search-not-set "0123456789" pname))))
      (cond ((= pos (string-length pname)) sym)
            (t (maybe-intern (substring pname 0 pos)
                             (symbol-package sym)))))))

;;; (PUT-LAST atom prop value)
;;;
;;;  This is like PUTPROP, but:
;;;   (1) argument order is better
;;;   (2) puts the property at the end of the plist. This allows the
;;;       DB-INDEX property to stay close to the head of the plist,
;;;
;;;  This used to be APPEND. I changed to to NCONC. I bet no one cares
;;;  and it'll probably waste less conses. --kmp

(defun put-last (atom prop value)
  (remprop atom prop)
  (setplist atom (nconc (plist atom)
                        (list*-in-area sys:property-list-area
                                       prop value nil)))
  value)

;;; (ASSQ-CAREFULLY obj list)
;;;
;;;  Like ASSQ, but ignores any elements of list which are atomic.
;;;
;;;  (Historical note: This was the behavior of ASSQ in Maclisp.)

(defun assq-carefully (obj list)
  (do ((l list (cdr l)))
      ((null l) nil)
    (cond ((and (listp (car l))
                (eq (caar l) obj))
           (return (car l))))))


;;; (READSTRING [stream])
;;;   is like readline but reads a string to either and end of file.
;;;
;;;   By default, an <End> char can also behave like an end of file.
;;;   This is changeable by giving an END-CHARS argument of chars,
;;;   if any, which can be treated like eof.

(defun readstring (&optional (stream standard-input)
                             (end-chars '(#\End)))
  (declare (values string))
  (with-output-to-string (string-stream)
    (do ((c (tyi stream -1) (tyi stream -1)))
        ((or (= c -1) (memq c end-chars)))
      (tyo c string-stream))))

;;; (DE-HYPHENATE x)
;;;
;;;  Returns a string which is lowercase the lowercase form of x
;;;  and which has hyphens changed to spaces. eg,
;;;
;;;     (DE-HYPHENATE 'FOO)              => "foo"
;;;     (DE-HYPHENATE 'FOO-OF-THE-BAR)   => "foo of the bar"

(defun de-hyphenate (string-or-symbol)
  (declare (values lowercase-dehyphenated-string))
  (let ((string (string-downcase string-or-symbol)))
    (dotimes (n (string-length string))
      (when (eq (aref string n) #/-)
        (setf (aref string n) #\space)))
    string))

;;; (STRING-CAPITALIZE x)
;;;
;;;  Returns a string identical to x, but with the first
;;;  character in uppercase. eg,
;;;
;;;   (STRING-CAPITALIZE "FOO")          => "FOO"
;;;   (STRING-CAPITALIZE "Foo")          => "Foo"
;;;   (STRING-CAPITALIZE "foo")          => "Foo"
;;;   (STRING-CAPITALIZE "foo bar")      => "Foo bar"

(defun string-capitalize (x)
  (declare (values capitalized-string))
  (string-append (char-upcase (character x))
                 (substring x 1)))

;;; These are utilities for the KBE's internal referencing to
;;; particular interning styles.
;;;
;;; (INTERN-KBE string)
;;;
;;;  Returns the interned copy of string on the KBE's internal package
;;;  (which is currently 'KBE but which might change). This function
;;;  should be, but probably isn't always, called to do interning on
;;;  that package.
;;;
;;; (INTERN-KEYWORD string)
;;;
;;;  Returns the interned copy of string on the keyword package. That
;;;  is currently the USER package, but may change if/when Common Lisp
;;;  goes in.

(defun intern-KBE (pname)
  (declare (values a-symbol-in-the-KBE-package))
  (intern (string pname) 'KBE))

(defun intern-keyword (pname)
  (declare (values a-symbol-in-the-KEYWORD-package))
  (intern (string pname) ""))

;;; (*DEFINE-SYSTEM-KEY key            ; a character
;;;                     doc            ; a string
;;;                     flavor         ; a window flavor
;;;                     query?         ; if T, query about conflicts
;;;                     re-init-form)  ; a form to EVAL to make more of them
;;;
;;; Defines a key sequence "System <key>" which will select a window
;;; of the specified flavor.
;;;
;;; This is not intended to be called directly from user code. It is
;;; support for DEFINE-SYSTEM-KEY to expand into.

(defun add-system-key (key query? doc flavor &optional (re-init-form 't))
  (let ((entry (assq key tv:*system-keys*)))
    (when (or (not entry)
              (not query?)
              (equal (caddr entry) doc)
              (fquery '() "Key sequence ~:C ~:C was ~S~
                          ~%Redefine it to be ~S? "
                      #\system key (caddr entry) doc))
      (tv:add-system-key key flavor doc re-init-form))))

;;;; Utilities for User Option Facility

;;; These functions are useful in conjunction with the
;;; User Options Facility described on page 18 of
;;; "Lisp Machine Choice Facilities".

;;; (ODD-OPTIONS options-alist)
;;;
;;;  The options-alist must have been defined by DEFINE-USER-OPTION-ALIST.
;;;  This returns a list of exactly one string per option variable which
;;;  differs from the default; the string describes the difference.

(defun odd-options (options-alist)
  (do ((oddities nil)
       (options options-alist (cdr options)))
      ((null options) oddities)
    (let ((default (get (caar options) 'tv:default-value)))
      (cond ((neq (symeval (caar options)) default)
             (push (odd-options-1 (car options)) oddities))))))

;;; (ODD-OPTIONS-1 alist-entry)
;;;
;;;  Does formatting of the string to be returned by ODD-OPTIONS.

(defun odd-options-1 (options-alist-entry)
  (let (((option name type type-data) options-alist-entry))
    (let ((val (symeval option)))
      (format nil "~A: ~A"
              (or name option)
              (selectq type
                ((:BOOLEAN) (cond ((not val) "No.")
                                  ((eq val 't) "Yes.")
                                  (t (format nil "~S ;Yes" val))))
                ((:ASSOC)   (let ((name-info
                                    (rassoc val type-data)))
                              (cond (name-info (car name-info))
                                    (t (format nil "~S ;Non-standard value"
                                               val)))))
                (otherwise  (format nil "~S" val)))))))

;;;; General message passing utilities

;;; (HANDLES? object message)
;;;
;;;  Returns true if object handles message, else false.

(defun handles? (object message)
  (send object ':operation-handled-p message))

;;; (SEND-IF-HANDLES object message . args)
;;;
;;;  If object handles message, works like
;;;       (SEND object message . args).
;;;  Otherwise, returns NIL.

(defun send-if-handles (object message &rest args)
  (lexpr-send object ':send-if-handles message args))

;;; (LEXPR-SEND-IF-HANDLES object message . lexpr-args)
;;;
;;;  If object handles message, works like
;;;       (LEXPR-SEND object message . lexpr-args).
;;;  Otherwise returns NIL.

(defun lexpr-send-if-handles (object message &rest args)
  (cond ((send object ':operation-handled-p message)
         (lexpr-funcall #'lexpr-send object message args))))

;;;; Parsing argument lists

;;; (PARSE-ARGUMENT-LIST arglist)
;;;
;;;  This parses an argument list, arglist, returning two values:
;;;   1: A list of arguments suitable for FUNCALL or LEXPR-FUNCALL
;;;   2: NIL if the last argument is not spread (ie, the FUNCALL case),
;;;      or T otherwise (ie, the LEXPR-FUNCALL case).
;;;   3: The bvl up to but not including any &AUX info.
;;;   4: The &AUX info (suitable for use in a LET*) or NIL if none.
;;;
;;;  For example,
;;;    (PARSE-ARGUMENT-LIST '(X Y &OPTIONAL (Q 3) &REST Z &AUX W))
;;;      => (X Y Q Z), T, (X Y &OPTIONAL (Q 3) &REST Z), (W)
;;;

(defun parse-argument-list (bvl)
  (declare (values arglist spread-p bvl aux-list))
  (do ((l bvl (cdr l))
       (arglist nil (cons var arglist))
       (spread-p nil)
       var)
      ((or (null l) (eq (car l) '&aux))
       (values (nreverse arglist) spread-p (ldiff bvl l) (cdr l)))
    (let ((current (car l)))
      (do ()
          ((or (not (atom current))
               (not (eq (character current) #/&))))
        (cond ((memq current '(&rest &body))
               (setq spread-p t)))
        (pop l)
        (setq current (car l)))
      (setq var (cond ((atom current) current)
                      (t (car current)))))))

;;;; Message utilities for hierarchical systems

;;; These utilities are intended primarily to support the window system
;;; where if a message isn't handled by a window, you want to ask the
;;; superior window, but in fact they are general enough to use on any
;;; object which answers a :SUPERIOR message.

;;; (SEND-ANY-SUPERIOR obj message . args)
;;;
;;; Finds the first x in obj and its superiors which handles
;;; the given message and tries sending w the given message,
;;; returning the result.
;;;
;;; Returns NIL, NIL if no such x is found.
;;;
;;; If a x is found, the first return value is the result of sending
;;; that object the given message and the second return value is T.

(defun send-any-superior (obj message &rest args)
  (do ((x obj (send-if-handles x ':superior)))
      ((null x) (values nil nil))
    (cond ((send x ':operation-handled-p message)
           (return (lexpr-send x message args) t)))))

;;; (LEXPR-SEND-ANY-SUPERIOR obj message . args)
;;;
;;; Lexpr version of SEND-ANY-SUPERIOR.

(defun lexpr-send-any-superior (obj message &rest args)
  (lexpr-funcall #'lexpr-funcall #'send-any-superior obj message args))

;;; (SEND-ANY-SUPERIOR-IF-HANDLES obj message . args)
;;;
;;; Tries each x in obj and its superiors with the given message.
;;; If no x handles the message or if all x's which handle the message
;;; return NIL, then NIL is returned. Otherwise, the first non-NIL
;;; result is returned and no succeeding (superior) x's are tried.

(defun send-any-superior-if-handles (obj message &rest args)
  (do ((x obj (send-if-handles x ':superior)))
      ((null x) nil)
    (let ((test (lexpr-send-if-handles x message args)))
      (cond (test (return test))))))

;;; (LEXPR-SEND-ANY-SUPERIOR-IF-HANDLES obj message . args)
;;;
;;; Lexpr version of SEND-ANY-SUPERIOR-IF-HANDLES.

(defun lexpr-send-any-superior-if-handles (obj message &rest args)
  (lexpr-funcall #'lexpr-funcall
                 #'send-any-superior-if-handles obj message args))

;;;; Support for DEFDEFMACRO

(defun make-defdefmacro-source-file-wrapper (definition-type fn-spec body)
  (let ((canonical-definition-type
          (or (get definition-type 'zwei:definition-function-spec-type)
              definition-type))
        (canonical-function-spec
          (cond ((and (not (atom fn-spec))
                      (= (length fn-spec) 2))
                 `(:property ,@fn-spec))
                (t fn-spec))))
    `(local-declare ((sys:function-parent ,canonical-function-spec
                                          ,canonical-definition-type))
       (record-source-file-name ',canonical-function-spec
                                ',canonical-definition-type)
       ,body)))
