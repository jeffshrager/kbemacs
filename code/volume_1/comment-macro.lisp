;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 185-191; listing comment-macro.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;THIS HAS THE STUFF FOR SUPPORTING THE READMACROS FOR LISP ROLES.
;THE ONLY FILES WHICH DEPEND ON THIS TO COMPILE ARE THE ACTUAL
;CLICHE LIBRARY FILES.
;;; PDF page 185
(defun kbe-comment? (x)
  (and (not (atom x)) (eq (car x) 'kbe-/#-comment-macro)))
(defun make-kbe-comment (code name)
  `(kbe-/#-comment-macro ,code ,name))
(defun kbe-comment-code (x) (cadr x))
(defun kbe-comment-name (x) (caddr x))
(defun kbe-comment-name->string (name)
  (if (stringp (car name))
      (car name)
      (format nil "~{~a~^ ~}" (mapcar #'string-downcase name))))
(defun string->kbe-comment-name (string)
  (let ((package *kbe-package*))
    (if (and (> (string-length string) 2)
             (member (substring string 0 2) '("a " "an")))
        (ncons string)
        (read-from-string (string-append "(" string ")")))))
(defun kbe-comment-type (comment)
  (let ((name (kbe-comment-name comment)))
    (cond ((null name) ':dummy)
          ((stringp (car name)) ':indefinite)
          ((memq 'input name) ':input)
          ((memq 'output name) ':output)
          (t ':segment))))
(defun kbe-comment-needs-implementation? (comment)
  (let ((code (kbe-comment-code comment)))
    (and (or (null code) (and (listp (car code)) (eq (caar code) '...)))
         (memq (kbe-comment-type comment) '(:input :segment)))))
;obsolete fn version
(defun part-marker? (x)
  (and (not (atom x)) (eq (car x) 'kbe-/#-comment-macro)))
;;; PDF page 186
;this has been hacked up yet again to add too new kinds of
;external sugar to the syntax.
;external {X, depending on Y} goes to
; internal (depending-on Y X)
;external {X, modified} goes to
; &IN-OUT X -- not. this is spliced into the list.
;The following implements reading the
;new syntax for slot comment things in code.
;it has the following two forms:
;1 {<name>} ==> (semi-kbe-/#-comment-macro <name>)
;is any THE or A phrase or "".
;a phrases are left as strings. the names are lists.
;2 {<code> , <name>} ==> (kbe-/#-comment-macro (<code>) <name>)
; <name> is any THE phrase in the command language.
; It becomes a list of the words in the phrase
; The name can be nil.
; <code> is any lisp sexpr.
;in order to indicate that a role takes parameters you write
;({<name>} . args) You can put &IN-OUT in the arglist.
;input and output roles are indicated by the keywords INPUT and OUTPUT
;in their names.
; instances of SEMI-KBE-/#-COMMENT-MACRO get changed
;o the correct kbe-/#-comment-macro
;immediately after reading so that all the internal stuff can
;stay the same.
;;; This sets things up for the text editor so that it can do
;;; more or less the right thing with #{...} expressions.
(set-editor-list-syntax-from-character #/{ #/()
(set-editor-list-syntax-from-character #/} #/))
(set-syntax-macro-char #/{ 'KBE-/{-reader)
(set-syntax-from-description #/} 'si:single)
(defun kbe-/{-reader (list-so-far stream)
  (let* ((stuff (read-to-matching-/} stream))
         (comma (string-reverse-search #/, stuff)))
    (if (null comma)
        `(semi-kbe-/#-comment-macro ,(string->kbe-comment-name stuff))
        (let ((code (get-code-for-kbe-comment (substring stuff 0 comma)))
              (comment (string->kbe-comment-name (substring stuff (1+ comma)))))
          (cond ((and (eq (car comment) 'depending)
                      (eq (cadr comment) 'on)
                      (= (length comment) 3)
                      (car code) (null (cdr code)))
                 `(depending-on ,(car (last comment)) ,(car code)))
                ((and (eq (car comment) 'depending)
                      (eq (cadr comment) 'on)
                      (eq (caddr comment) 'the)
                      (> (length comment) 4)
                      (car code) (null (cdr code)))
                 `(depending-on (kbe-/#-comment-macro nil ,(cddr comment))
                                ,(car code)))
                ((and (equal comment '(modified))
                      (car code) (null (cdr code)))
                 (values (nconc list-so-far `(&in-out ,(car code))) nil T))
                (T `(kbe-/#-comment-macro ,code ,comment)))))))
(defvar kbe-/#-comment-length 1000
  "The number of characters to read before complaining about no } after {")
;this will not work if there is a #/} or something like that in the way.
(defun get-code-for-kbe-comment (string)
;;; PDF page 187
  (let ((code (read-from-string string)))
    (if (equal code '(...)) (setq code nil))
    (ncons code)))
(defun read-to-matching-/} (stream)
  (do ((char (tyi stream) (tyi stream))
       (chars nil (cons char chars))
       (level 0)
       (i 0 (1+ i)))
      ((> i kbe-/#-comment-length)
       (complain "No '}' found where one expected"))
    (cond ((= char #/{) (incf level))
          ((= char #/}) (decf level)
           (if (minusp level)
               (return (apply #'string-append (nreverse chars))))))))
;this does output-inversion of depending-on
(pp:define-list-print-function depending-on (list stream &rest ignore)
  (format stream "~@!{~=, depending on ~A<}~."
          (caddr list)
          (if (kbe-comment? (cadr list))
              (kbe-comment-name->string (kbe-comment-name (cadr list)))
              (cadr list))))
;;; PDF page 188
;THE FOLLOWING OLD SYNTAX IS STILL SUPPORTED AT THE MOMENT, and
;it is still being used internally.
;#{{CODE,} {INPUT | OUTPUT} name} ==>
; (kbe-/#-comment-macro list-of-code list-of-keyword-and-names)
;INPUT specifies it is an input slot
;OUTPUT specifies it is an output slot
;otherwise it is a segment slot
;If code is of the form "(... . args)" then it specifies that
; the result of each arg is a data input to the slot
;If the code is of the form "sexpr" then it speicifies the code
; which fills the role.
;NAME is the slot name. the name can be omitted.
; The name may be compound of the form
; "name of the other-name" in which case it specifies that there
; is a higher level slot which this is part of. (This only works
; for loop enumerators at the moment.)
(defun kbe-/#-comment-reader (ignore stream)
  (read-kbe-/#-stuff stream))
(set-syntax-/#-macro-char #/{ #'kbe-/#-comment-reader)
(defun read-kbe-/#-stuff (&optional (stream standard-input))
  (let* ((stuff (read-to-matching-/} stream))
         (comma (string-reverse-search #/, stuff)))
    (if comma
        `(kbe-/#-comment-macro
           ,(get-code-for-kbe-comment (substring stuff 0 comma))
           ,(string->kbe-comment-name (substring stuff (1+ comma))))
        `(kbe-/#-comment-macro () ,(string->kbe-comment-name stuff)))))
;;; PDF page 189
;this takes a piece of code and gets rid
;of every semi-kbe-/#-comment-macro thing
;in it in favor of kbe-/#-comment-macro things.
;It does the following transformations by side-effect
;((semi-kbe-/#-comment-macro <name>) . <args>)
; => (kbe-/#-comment-macro ((... . <args>)) <name>)
;when not in fn position.
; (semi-kbe-/#-comment-macro <name>) => (kbe-/#-comment-macro () <name>)
(defun fix-up-semi-kbe-/#-comment-macros (code)
  (cond ((not (listp code)))
        ((eq (car code) 'semi-kbe-/#-comment-macro)
         (rplacd code (list nil (cadr code)))
         (rplaca code 'kbe-/#-comment-macro))
        ((and (listp (car code))
              (eq (caar code) 'semi-kbe-/#-comment-macro))
         (mapc #'fix-up-semi-kbe-/#-comment-macros (cdr code))
         (let ((new-code (if (cdr code) `((... .,(cdr code))))))
           (rplacd code `(,new-code ,(cadar code)))
           (rplaca code 'kbe-/#-comment-macro)))
        ((eq (car code) 'cond)
         (mapc #'(lambda (c) (mapc #'fix-up-semi-kbe-/#-comment-macros c))
               (cdr code)))
        (T (do ((list code (cdr list)))
               ((not (listp list)))
             (fix-up-semi-kbe-/#-comment-macros (car list)))))
  code)
(defun read-hacking-/{ (&optional (stream standard-input))
  (fix-up-semi-kbe-/#-comment-macros (read stream)))
;;; PDF page 190
;this stuff prints out kbe-/#-comment-macro in either old
;or new style.
(define-internal-option *kbe-role-marker* "KBE role marker (internal)"
  "#{" :assoc '(("{" . "{") ("#{" . "#{")))
(pp:define-list-print-function kbe-/#-comment-macro (x s)
  (cond ((equal *kbe-role-marker* "{")
         (new-style-kbe-/#-comment-macro-output x s))
        (T (old-style-kbe-/#-comment-macro-output x s))))
(defun new-style-kbe-/#-comment-macro-output (x s)
  (let* (((code name) (cdr x))
         (comment (kbe-comment-name->string name)))
    (cond ((null code) (format s "{~A}" comment))
          ((and (listp (car code)) (eq (caar code) '...))
           (format s "(~@!{~A}~^ ~@_~:I~{~=~^ ~_~}~.)"
                   comment (handle-&in-out (cdar code))))
          (T (format s "{~@l~{~=, ~:_~}~A~.}" code comment)))))
(defun old-style-kbe-/#-comment-macro-output (x s)
  (let* (((code name) (cdr x))
         (comment (kbe-comment-name->string name)))
    (format s "#{~@l~{~=, ~:_~}~A~.}" code comment)))
(pp:define-list-print-function |...| (x s)
  (format s "(~!~A~^ ~:I~@_~@{~=~^ ~}~.)" x))
;this looks for &in-out in a list and turns it into the right {} thing
;note we only do this in the new style, because the old style
;reader cannot hack this.
(defun handle-&in-out (list)
  (if (not (memq '&in-out list)) list
      (do ((l list (cdr l)) result) ((null l) (nreverse result))
        (if (not (eq (car l) '&in-out)) (push (car l) result)
            (pop l)
            (push `(kbe-#-comment-macro (,(car l)) ,(ncons 'modified))
                  result)))))
;;; PDF page 191
;this defines how kbe-/#-comment-macro is evaluated. It is important
;to note that there are three different ways in which it is evaluated
;1- in actual code (it tries just to be invisable)
;   Note that this is the only case that ever gets compiled.
;2- in constraints and comments in cliches where it computes
;   the segcase that it coressponds to.
;3- when a cliche is initially being instanciated where it
;   computes the parameter to the cliche it corresponds too.
;4- also A phrase comments are handled specially by CREATE-PLAN-FOR-CODE
(declare (special 2creation-time-plan *2focus-seg* 2role-fillers 2cliche-name))
(defmacro kbe-/#-comment-macro (code comment)
  (cond ((and code (not (and (listp (car code)) (eq (caar code) '...))))
         (car code))
        (2creation-time-plan
         `(2creation-time-kbe-comment ',comment))
        (*2focus-seg*
         `(2focus-seg-kbe-comment ',comment))
        (T `(kbe-/#-comment-complaint ',code ',comment))))
(defun kbe-/#-comment-complaint (ignore comment)
  (complain "The ~{~A~^ ~} has not been implemented yet" comment))
(defun 2creation-time-kbe-comment (name-list)
  (let* ((role-name (car (last name-list)))
         (primary-roles (get (get 2cliche-name 'cliche-comments) 'primary-roles))
         (spot (memq role-name primary-roles))
         (ordinal (- (length primary-roles) (length spot))))
    (cond ((and spot (> (length 2role-fillers) ordinal))
           (car (nth ordinal 2role-fillers)))
          (*2focus-seg* (2focus-seg-kbe-comment name-list)))))
(defun 2focus-seg-kbe-comment (name)
  (if (not (equal (car name) 'the)) (push 'the name))
  (setq name (string-append (kbe-comment-name->string name)
                            " of the segment " *2focus-seg* "."))
  (car (eval-command (parse-command name #'parse-definite-reference))))
