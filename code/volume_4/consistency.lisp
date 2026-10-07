;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>CONSISTENCY.LISP.88, 1 October 1984 15:29:50
;;; Knowledge-Based_Editor_Jan-1985_volume_4.pdf, PDF pages 159-163.
;;;
;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.
;;;
;;; CONSISTENCY: Tools for editor consistency checks

;;; (REMOVE-VANILLA form)
;;;
;;;  This recursively descends form (not recursing into atoms, including
;;;  arrays or structures, and changes references to VANILLA:anything to
;;;  be the active package.
;;;
;;;  This might not be the most obvious place for this definition to be,
;;;  but it will do for now... Note that the reason it's here and not
;;;  in VERBS with 2RENAME is that in fact, we don't want to bind
;;;  ourselves to a package until the very last minute. This gives the
;;;  user the option of changing his package in the editor buffer without
;;;  the KBE having to know and still maximizing the chances that the
;;;  local variables, go tags, etc. come out in the preferred package.

(defun remove-vanilla (form &optional (pkg package))
  (cond ((null form) nil)
        ((symbolp form)
         (cond ((eq (symbol-package form) *Vanilla-package*)
                (intern (string form) pkg))
               (t form)))
        ((atom form) form)
        (t
         (do ((f form (cdr f))
              (l nil (cons (remove-vanilla (car f) pkg) l)))
             ((atom f)
              (nreconc l (cond (f (remove-vanilla f pkg))
                               (t nil))))))))

(defun replace-definition-verbose (name code)
  (multiple-value-bind (success-indicator msg)
      (replace-definition name (remove-vanilla code))
    (when success-indicator
      (code-is-consistent name code))
    (when (not (eq success-indicator ':ignored))
      (editor-echo-output "~A" msg))))

(defun code->language (code)
  (cond ((ada? code) ':ada)
        (t ':lisp)))

(defun replace-or-create-definition (name code &optional type)
  (if (not type) (setq type (infer-definition-type code)))
  (cond ((not (find-single-definition name type))
         (validate-buffer-mode (code->language code) name)
         (validate-buffer-package)
         (create-dummy-definition name type)
         (replace-definition-verbose name code))
        ((not (code-is-consistent? name type))
         (editor-echo-output "~S's definition has changed." name)
         (if (y-or-n-p "Shall I show you how? ")
             (explain-how-code-is-superseded name))
         (cond ((y-or-n-p "Update definition anyway? ")
                (replace-definition-verbose name code))
               (t
                (editor-echo-output "Ok, not updating ~S's definition."
                                    name))))
        (t
         (let ((pointer (find-single-definition name type)))
           (let ((saved-buffer (current-editor-buffer))
                 (new-buffer   (containing-buffer pointer)))
             (unwind-protect
                  (progn (make-buffer-current new-buffer)
                         (validate-buffer-mode (code->language code) name)
                         (validate-buffer-package)
                         (replace-definition-verbose name code))
               (make-buffer-current saved-buffer)))))))

;;;; Code consistency checking

(define-KBE-option *check-code-consistency*
    "Check code consistency during buffer updates"
  t :boolean)

;; Yuck! This check for kbe-/#-comment-macro
;; seems somehow ill-placed here. It seems
;; to violate the current level of abstraction. Probably it should
;; go in some other module.

(defun code-equal (x y)
  (cond ((or (and (not (atom x))
                  (eq (car x) 'kbe-/#-comment-macro))
             (and (not (atom y))
                  (eq (car y) 'kbe-/#-comment-macro)))
         (let ((code-x (list x)) (name-x nil)
               (code-y (list y)) (name-y nil))
           (cond ((and (not (atom x)) (eq (car x) 'kbe-/#-comment-macro))
                  (setq code-x (cadr x))
                  (setq name-x (caddr x))))
           (cond ((and (not (atom y)) (eq (car y) 'kbe-/#-comment-macro))
                  (setq code-y (cadr y))
                  (setq name-y (caddr y))))
           (unless (and name-x name-y (not (equal name-x name-y)))
             (code-equal code-x code-y))))
        ((atom x) (equal x y))
        ((atom y) nil)
        (t
         (and (code-equal (car x) (car y))
              (code-equal (cdr x) (cdr y))))))

(define-KBE-option *auto-submit*
    "Attempt auto-correction of inconsistent code"
  t :boolean)

(defun code-is-consistent? (name &optional type)
  (cond ((not *check-code-consistency*) t)
        (t
         (let ((believed-buffered-code (belief ':buffered-code name)))
           (cond ((not believed-buffered-code) t)
                 (t
                  (let ((buffered-code (get-definition name type))
                        (good-code (belief ':code name)))
                    (or
                     ;; Buffered-code went away?
                     (not buffered-code)
                     ;; This is necessary to avoid problems caused by
                     ;; the auto-submit stuff. If the changes it requests
                     ;; are not inserted atomically, we'll find it
                     ;; complaining where it shouldn't be that the code
                     ;; is inconsistent.
                     (code-equal buffered-code believed-buffered-code)
                     ;; Technically this is the only good reason
                     ;; for assuming code equivalence
                     (code-equal buffered-code good-code)
                     ;; This forces the code to be consistent if it's
                     ;; not viewable as consistent already and if the
                     ;; force feature is enabled...
                     (and *auto-submit*
                          (eq (car buffered-code) 'defun)
                          (make-code-consistent? name
                                                 buffered-code))))))))))

;; The following functionality should probably be in the Apprentice
;; internals and should work by actor negotiation, rather than function
;; call. In that case, the :BELIEF :CODE (marked with ***) would be a
;; reference to the KBE's internal (code-for-defun ...) information and
;; the EXECUTE-STREAM-CONTENTS of the string-stream would just be
;; a mapc of #'execute ... A context would have to be pushed
;; here so that the whole command sequence could be undone if any of
;; the generated requests failed.

(defun make-code-consistent? (name buffered-code)
  (let ((good-code (belief ':code name))) ;***
    (let ((changes (compare-code name good-code buffered-code)))
      (cond ((not changes) nil)
            (t
             (believe ':buffered-code name buffered-code);!
             (with-input-from-string (string-stream changes)
               (execute-stream-contents string-stream nil)   ;%%%
               t))))))

(defun compare-code (name buffered-code old-code)
  (*catch 'compare-code
          (if (or (atom buffered-code)
                  (atom old-code)
                  (neq (car buffered-code) 'defun)
                  (neq (car old-code) 'defun)
                  (neq (cadr buffered-code) (cadr old-code)))
              (*throw 'compare-code nil))
          (let ((arg-changes (compare-args (caddr buffered-code)
                                           (caddr old-code)))
                (result (compare-code-1 (cdddr buffered-code)
                                        (cdddr old-code)
                                        nil nil 0)))
            (if (or arg-changes result)
                (text "~{~A~^~%~}~{~&~A~^~%~}"
                      (mapcar #'(lambda (datum)
                                  (append-delimiter
                                   (text "Append a parameter ~S ~
                                     to the program ~S"
                                         datum name)))
                              arg-changes)
                      (nreverse
                       (mapcar #'(lambda (datum)
                                   (let (((path . code) datum))
                                     (let ((string
                                            (path->string name
                                                          (cons
                                                           (+ 3 (car path))
                                                           (cdr path)))))
                                       (cond (string
                                              (append-delimiter
                                               (text "Implement ~A as $~S"
                                                     string code)))
                                             (t
                                              (*throw 'compare-code nil))))))
                               result)))))))

(defun compare-args (x y)
  (cond ((null x)
         (if (memq '&aux y) (*throw 'compare-code nil))
         y)
        ((null y) (*throw 'compare-code nil))
        ((eq (car x) '&aux)
         (let ((thing (memq '&aux y)))
           (if (equal x thing) (ldiff y thing)
               (*throw 'compare-code nil))))
        ((eq (car x) (car y)) (compare-args (cdr x) (cdr y)))
        (t (*throw 'compare-code nil))))

(defun compare-code-1 (old new data path idx)
  (cond ((atom old)
         (cond ((equal old new) data)
               (t  (*throw 'compare-code nil))))
        ((eq (car old) 'kbe-/#-comment-macro)
         (cond ((code-equal old new) data)
               (t
                (cons (cons (cdr (reverse (cons idx path))) new) data))))
        ((atom new)  (*throw 'compare-code nil))
        (t
         (do ((i 0 (1+ i))
              (o old (cdr o))
              (n new (cdr n))
              (d data (compare-code-1 (car o) (car n)
                                      d (cons idx path) i)))
             ((or (atom o) (atom n))
              (cond ((equal o n) d)
                    (t (*throw 'compare-code nil))))))))

(define-KBE-option *automatic-reanalysis-when-needed*
    "Automatically re-analyze inconsistent code"
  t :boolean)

(defmacro with-consistent-code (name &body forms)
  (once-only (name)
             `(cond ((or (code-is-consistent? ,name)
                         (when *automatic-reanalysis-when-needed*
                           (editor-echo-output "Code for ~S being re-analyzed."
                                               ,name)
                           (com-KBE-execute-analyze)
                           t))
                     ,@forms)
                    (t
                     (editor-barf "Definition of ~S needs to be re-analyzed."
                                  ,name)))))

(defun code-is-consistent (name &optional (code (get-definition name)))
  (believe ':buffered-code name code))

(defun explain-how-code-is-superseded (name
                                       &optional
                                         (code (get-definition name)))
  (let ((old-code (belief ':buffered-code name)))
    (format t "The definition of ")
    (mouse-sensitive-princ ':function name)
    (let ((prinlevel nil)
          (prinlength nil)
          (prinendline nil)
          (base 10.)                    ; For display, force base to ten
          (*nopoint nil))               ;  and make sure user can tell.
      (format t " has changed. It was:~2%~:=~2&but is now:~2%~:=~&"
              old-code code))))
