;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>ADA-EVAL.LISP
;;; PDF pages 124-154 (printed source pages 1-31).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

(defmacro ada-value-type (x) `(car ,x))
(defmacro ada-value-data (x) `(cadr ,x))
(defmacro ada-value-build (type value) `(list ,type ,value))
;The declarations create initial values for many things.
;setq (and hence output args) are handled by RPLACDing the values.
;the following is used in the support of USE.
;we don't do anything with WITH at the moment.  everything is WITHed.
(defvar ada-use-packages nil "full contour names USEd")
(defvar ada-prefix-stack nil "full name of current contour")
(defun ada-prefix-it (name) (cons name (car ada-prefix-stack)))
(defun ada-add-prefix (name)
  (push (ada-prefix-it name) ada-prefix-stack))
(defun ada-use  (&quote &rest names)
   (mapc #'ada-add-use names)
  nil)
(defun ada-add-use (name)
   (push (cdar (ada-find 'package name T))
        ada-use-packages) )
;;; PDF page 125

;the following DB shows what we know about each root name. each entry
;has the form ((type root-name . qualifiers) . data). The first part
;of the entry is called a TOKEN as is the retrieval key. Several  fns
;support looking up with a partial key. All  entries are on one huge-
;ASSOC list ADA-INFO.  This alist is never bound below, but rather just
;keeps accumulating information.  This is done so that you can always
;get access to everything.  (You can bind it if you want to add some
;temporary information.)  Special care is taken so that if you
;redefine something this does not cause excess junk on ada-info.
;The following table shows the various types and what data goes with
;them.  Note that even if there is no data, the type itself and the
;qualifiers are often of interest.
;     object
;function/operator/procedure  FN          alist of arg types and fn bodies
;generic package or function  GENERIC     parameter list and list of templates
;data type
;package name
;exception name   :           EXCEPTION   none
;variable name
;record field name            FIELD       none
;enumeration element          ENUM        none
(defvar ada-info nil "info on all ada identifiers")
;;; PDF page 126

;these functions are used for retrieval from ada-info in this file.
;for the basic get and put, we assume that a fully qualified token is
;supplied
(defun ada-find-fast (type name signal-error)
 (or (assoc (cons type name) ada-info)
     (if signal-error (barf "cannot find ~a" (cons type name)))))
(defun ada-store-fast (type name data)
 (let* ((token (cons type name) )
        (spot (assoc token ada-info)))
   (cond (spot  (rplacd spot data))
          (T (push (cons token data) ada-info)))))
;this finds the info associated with the supplied token following USE
;information. Note  it assumes that the order of uses is irrelevant.
;any hiding must be supported by order of definition.
(defun ada-find (type name signal-error)
  (or (car (ada-find-1 type name nil))
      (if signal-error (barf "cannot find ~A" (cons type name)))))
;this finds the first hit and can continue on.
(defun ada-find-1 (type name spot)
  (do ((entries (or spot ada-info) (cdr entries)))
      ((null entries) nil)
    (if (and (eq (car name) (cadaar entries))  ;test most likely to fail
             (eq type (caaar entries))
             (do ((ns (cdr name) (cdr ns))
                  (es (cddr (caar entries)) (cdr es)))
    ((null es) (null ns))
               (cond ((null ns)
                      (return
                        (not (null (or (member es ada-prefix-stack)
                                       (member es ada-use-packages))))))
                     ((not (eq (car ns) (car es))) (return nil)))))
        (return entries))))
;;; PDF page 127

; this supports overloading on the assumption that you can tell what a
;thing refers to just from looking at it (and its arguments) (but
;never at how it is used). This works out pretty well for
;most things (eg fns types).
; It fails badly for literal things like numbers, strings, and
;enumeration literals.  For numbers and strings we just assume that
;they will match up ok with any numeric (or string) parameter without
;trouble.  this is much less restrictive than ada but works fine.
; For enumeration literals we simply assume that the curresponding
;type is USEd and thaat there is no prefix on the literal and that the
;basic name is unique enough. we  don't check that it is in the
;correct type or anything.  this is pretty gross but works in the
;programs being demonstrated.
;we handle variable names and exception names the same simple way as
;enumeration literals even though you could figure them out pretty
;will by themselves.  It just that it is much simpler this way and
;works fine in our examples.
; Note all this implies that explicit prefixes can only appear on fns
;and types.  Everthing else must be globally unique namewise.
;this is a pretty gross cludge in that it assumes that
;the args to a fn can be evaled twice without harml
;this is true in our demo, and saves a great deal of time.
(defmacro ada-fn (fn . args)
  (cons (cdr (ada-find-right-fn fn
                (mapcar #'(lambda (a) (ada-value-type (eval a))) args)
                nil))
        args))
(defmacro ada-attribute (var-or-type attribute)
  (let ((type (cond ((not (symbolp var-or-type)) var-or-type)
                    (T (ada-value-type (eval var-or-type))))))
    (cons (cdr (ada-find-right-fn (ncons attribute) (ncons type) nil))
          `',type)))
(defmacro ada-convert (type arg)
  `(ada-value-build ',(ada-find-true-type type) (ada-value-data ,arg)))
;You must always use a constructor name when making a record, because
;of the way we do overload resolution.  this has to do copying on the
;values because of the way ada-setq happens by side-effect.
(defmacro ada-construct (type . args)
  `(ada-construct-int ',(ada-find-true-type type) ., args))
(defun ada-construct-int (record-type &rest values)
  (ada-value-build record-type
                   (mapcar #'(lambda (v f)
                               (ada-value-build (cadr f)
                                                (ada-value-data v)))
                           values
                           (cddr (ada-find-fast 'type record-type T)))))
;;; PDF page 128

;this supports accessing of records. Note the way it
;uses the stored type information.
(defmacro ada-select (var field-name)
 (let* ((record-dcl
         (cdr (ada-find-fast 'type (ada-value-type (eval var)) T))))
   (do ((i 0 (1+ i))
        (field-info (cdr record-dcl) (cdr field-info)))
       ((null field-info) (barf "bad record access"))
     (if (eq field-name (caar field-info))
         (return `(nth ,i (ada-value-data ,var)))))))
;returns cons of full fn name and internal name.
(defun ada-find-right-fn (fn type-list spot)
 (let* ((hit (ada-find-1  'fn fn spot))
        (internal-name
         (do ((alist (cdar hit) (cdr alist)))
             ((null alist) nil)
           (if (ada-types-match type-list (caar alist))
                (return (cdar alist))))))
   (cond (internal-name  (cons (cdaar hit) internal-name) )
          ((null (cdr hit)) (barf "cannot find fn ~a" fn))
          (T (ada-find-right-fn fn type-list (cdr hit))))))
(defun ada-find-true-type (type)
  (cond ((memq (car type) '(ada-string ada-exception ada-enumeration))
        type)
        ((eq (car type) 'ada-range)
         (cond ((cadr type)
                `(ada-range ,(ada-find-true-type (cadr type) )
                            .,(cddr type)))
               (T type)))
        ((eq (car type) 'ada-rec)
         (cons 'ada-rec
               (mapcar #'(lambda (p)
                           (list (car p) (ada-find-true-type (cadr p))))
                       (cdr type))))
        ((memq (car type) '(any private limited-private))
         type)
        (t (or ;kludge for now so cliches can parse easier.
               (ignore-errors (cdar (ada-find 'type type T)))
               (and (string-search "_type" (string (car type)))
                    type)))))
;;; PDF page 129

;We treat NEW types just like subtypes.
;we have the following basic type guys
; ANY, PRIVATE, and LIMITED-PRIVATE.
; any symbol which is a TYPE on ada-info
;built in type constructors
; (ada-range {<type>} first last)
; (ada-enumeration . literals)
; (ada-string first last)
; (ada-rec . <field-name-<type>-pairs>)
; (ada-exception {superior exception})
(defun ada-type-match (arg-type def-type)
  (cond ((eq (car def-type) 'any) T)
        ((memq (car arg-type) '(ada-enumeration ada-exception ada-rec))
         nil)
        ((eq (car arg-type) 'ada-range)
         (ada-type-match (or (cadr arg-type) '(integer)) def-type))
        ((eq (car arg-type) 'ada-string) (equal def-type '(ada-string)))
        ((memq (car arg-type) '(nil private limited-private)) nil)
        ((equal arg-type def-type) T)
        ((memq (car arg-type)
               '(integer-literal string-literal enumeration-literal))
         (eq (car arg-type) (ada-literal-class def-type)))
        (T (ada-type-match (cdr (ada-find-fast 'type arg-type T))
                           def-type))))
(defun ada-literal-class (type)
  (cond ((eq (car type) 'ada-range)
         (ada-literal-class (or (cadr type) '(integer))))
        ((eq (car type) 'ada-string) 'string-literal)
        ((eq (car type) 'ada-enumeration) 'enumeration-literal)
        ((memq (car type) '(exception record)) nil)
        ((eq (car type) 'integer) 'integer-literal)
        ((memq (car type) '(nil any private limited-private))
         nil)
        (T (ada-literal-class (cdr (ada-find-fast 'type type T))))))
(defun ada-types-match (arg-type-list def-type-list)
  (and (= (length arg-type-list) (length def-type-list) )
       (do ((arg-types arg-type-list (cdr arg-types))
            (def-types def-type-list (cdr def-types)))
           ((null arg-types) T)
         (if (not (ada-type-match (car arg-types) (car def-types)))
             (return nil)))))
;;; PDF page 130

(defun ada-type-dcl (&quote &rest body)
  (ada-do-subtype-dcl (car body) (cadr body)))
(defun ada-subtype-dcl (&quote &rest body)
  (ada-do-subtype-dcl (car body) (cadr body) ))
(defun ada-do-subtype-dcl (subtype type)
  (let* ((true-subtype (ada-prefix-it subtype) )
         (true-type (ada-find-true-type type)))
    (caseq (car true-type)
      (ada-rec
       (ada-store-fast 'type true-subtype true-type)
       (mapcar #'(lambda (x)
                   (ada-store-fast 'field (ada-prefix-it (car x)) nil))
               (cdr true-type)))
      (ada-enumeration
       (ada-store-fast 'type true-subtype true-type)
       (mapcar #'(lambda (x)
                   (ada-store-fast 'enum (ada-prefix-it x) nil))
               (cdr true-type)))
      (ada-exception (ada-store-fast 'exception true-subtype nil))
      (T (ada-store-fast 'type true-subtype true-type)))
    nil))
(defun str-lit (&quote &rest body)
  (ada-value-build '(string-literal) (car body)))
(defun enum-lit (&quote &rest body)
  (ada-value-build '(enumeration-literal) (car body)))
(defun int-lit (&quote &rest body)
  (ada-value-build '(integer-literal) (car body)))
;this makes variables of the given types.
(defun ada-dcl (&quote &rest body)
  (ada-do-dcl (car body) (cadr body) nil))
(defun ada-init-dcl (&quote &rest body)
  (ada-do-dcl (car body) (cadr body) (caddr body)))
(defun ada-constant-dcl (&quote &rest body)
  (ada-do-dcl (car body) (cadr body) (caddr body)))
(defun ada-do-dcl (var type value)
  (ada-store-fast 'variable (ncons var) nil)
  (if (not (eq (car type) 'ada-exception))
      `((,var (ada-value-build ',(ada-find-true-type type)
                               (ada-value-data  ,value))))))
;;; PDF page 131

;def-ada-top needs to be a macro so that ada cliches will
;compile before they load.
(defmacro def-ada-top (name ignore use first-def &rest other-defs)
 (if (eq (car first-def)  'def-ada-cliche)
     first-def
      `(def-ada-top-internal ,name nil ,use ,first-def ., other-defs)))
(defun def-ada-top-internal
      (&quote name ignore use first-def &optional second-def)
  (let ((ada-use-packages nil))
    (apply 'ada-use use)
    (mapc #'(lambda (init)            ;global vars
              (if init (set (car init) (eval (cadr init)))))
          (eval first-def))
    (if second-def
        (mapc #'(lambda (init)
                  (if init (set (car init) (eval (cadr init)))))
              (eval second-def))))
  name )
(defun def-ada-package (&quote &rest body)
  (let* ((name (car body))
         (dcls (cadr body))
         (ada-prefix-stack ada-prefix-stack)
         (ada-use-packages ada-use-packages)
         (pkg-name (ada-prefix-it name))
         (generic (ada-find-fast 'generic pkg-name nil))
         inits)
    (cond (generic
           (rplacd (cdr generic) (ncons (cons 'def-ada-package body)))
           nil)
          (T (ada-add-prefix name)
             (setq inits (mapcan #'eval dcls))
             (ada-store-fast 'package pkg-name ada-use-packages)
             inits))))
(defun def-ada-package-body (&quote &rest body)
  (let* ((name (car body))
         (dcls (cadr body))
         (ada-prefix-stack ada-prefix-stack)
         (pkg-name (ada-prefix-it name))
         (ada-use-packages (or (cdr (ada-find-fast 'package pkg-name nil))
                               ada-use-packages) )
         (generic (ada-find-fast 'generic pkg-name nil)))
    (cond (generic
           (rplacd (cddr generic)
                   (ncons (cons 'def-ada-package-body body)))
           nil)
          (T (ada-add-prefix name)
             (mapcan #'eval dcls)))))
;;; PDF page 132

(defun def-ada-fn (&quote &rest body)
 (let* ((spec (car body))
        (name (car spec)) (params (cadr spec)) (return (caddr spec))
        (dcls (cadr body)) (stmnts (caddr body)) (except (cadddr body)))
   (if stmnts
       (let*  ((ada-use-packages ada-use-packages)
              (ada-prefix-stack ada-prefix-stack)
              (fn-name  (ada-prefix-it name))
              (args  (mapcar #'car params))
               (generic (ada-find-fast 'generic fn-name nil))
               inits)
          (cond (generic
                 (rplacd (cdr generic) (ncons (cons 'def-ada-fn body))))
                (T (ada-add-prefix name)
                   (setq inits (mapcan #'eval dcls))
                   (let* ((type-list
                           (mapcar #'(lambda (p)
                                       (ada-find-true-type (cadr p)))
                                   params) )
                          (bod `(ada-begin ((prog () ,@stmnts))
                                           ,except)))
                     (cond (return (setq bod
                                         `(ada-value-build
                                            ',(ada-find-true-type return)
                                            (ada-value-data ,bod))))
                           (T (setq bod `(progn ,bod (values)))))
                     (def-ada-fn-internal fn-name type-list
                       `(lambda ,args
                          (let* ((ada-use-packages ',ada-use-packages)
                                 (ada-prefix-stack ',ada-prefix-stack)
                                 .,inits)
                            ,bod)))))))))
  nil)
(defun def-ada-fn-internal (name type-list lambda)
  (let* ((internal-name (ada-store-fn name type-list)))
    (eval `(defun ,internal-name .,(cdr lambda)))))
;we are careful to reuse names here in order to help maclisp GCing.
(defvar ada-gensym-counter 1 "protects from name collisions")
(defun ada-store-fn (fn type-list)
  (let* ((fn-info (cdr (ada-find-fast 'fn fn nil)))
         (spot (assoc type-list fn-info)))
    (cond ((or (null fn-info) (null spot))
           (setq ada-gensym-counter (1+ ada-gensym-counter))
           (let ((internal-name
                  (intern (format nil "~A-~D"
                                  (car fn) ada-gensym-counter))))
             (ada-store-fast 'fn fn (cons (cons type-list internal-name)
                                          fn-info))
             internal-name))
          (T (cdr spot)))))
;;; PDF page 133

; DEF-ADA-CLICHE is defined just like DEFINE-CLICHE and next to it.
;this handles generics by just doing wholesale substitutions
(defun def-ada-generic (&quote &rest body)
 (let* ((basic-spec (cadr body))
        (name (caseq (car basic-spec)
                (def-ada-package (cadr basic-spec))
                 (def-ada-fn (caadr basic-spec))))
        (gen-name (ada-prefix-it name) ))
    (ada-store-fast 'generic gen-name (ncons (cons 'def-ada-generic body)))
    (eval basic-spec) ;so if is package will store its body
   nil))
(defun ada-new-package (&quote &rest body)
  (ada-instance (cadr body) (car body) (cddr body)))
(defun ada-new-fn (&quote &rest body)
  (ada-instance (cadr body) (caar body) (cddr body)))
(defun specs-for-new-ada-fn (generic-name name)
  (let* ((generic-info (cdr (ada-find 'generic generic-name T)))
         (fn-template (cadr generic-info))
         (arg-info (cdadr fn-template)))
    (cons name arg-info)))
(defun ada-instance (generic-name name args)
  (let* ((generic-info (cdr (ada-find 'generic generic-name T)))
         (params (cadr (car generic-info)))
         (template (cdr generic-info))
         (substitutions
           (ada-determine-subs generic-name params name args)))
    (car (mapcar #'eval (ada-sublis substitutions template)))))
;works on equal not eq -- assumes no atomic cdrs
(defvar ada-sublis-alist)
(defun ada-sublis (ada-sublis-alist tree)
  (ada-sublis-1 tree))
(defun ada-sublis-1 (tree)
  (let ((hit (assoc tree ada-sublis-alist)))
    (cond (hit (cdr hit))
          ((and tree (listp tree)) (mapcar #'ada-sublis-1 tree))
          (T tree))))
;;; PDF page 134

(defun ada-determine-subs (generic-name params name args)
 (let ((subs `((,(car generic-name) ., name))))
   (do ((ps params (cdr ps))
        (as args (cdr as)))
       ((null ps))
     (let ((pair (ada-determine-sub (car ps) (car as))))
       (setq ps (ada-sublis (ncons pair) ps))
       (push pair subs)))
   subs))
(defun ada-determine-sub (param arg)
 (caseq (car param)
   ((ada-type-dcl ada-subtype-dcl)
    (cons (ncons (cadr param)) (ada-find-true-type arg)))
   (ada-with
    (if (and (null arg) (not (and (eq (car param) 'ada-with)
                                  (eq (car (last param))  '<>))))
        (barf "required generic arg missing"))
    (let ((fn-name (ncons (caadr param) ))
           (arg-name
             (car (ada-find-right-fn (or arg fn-name)
                                     (mapcar #'cadr
                                              (cadadr param)) nil))))
      (cons fn-name arg-name)))))
;;; PDF page 135

;unlike the things above, many of these forms are macros, because
;being macros explains them both to the evaluator and the analyzer.
;the things above have to have special analyzer understanding in any case.
(defvar ada-last-exception nil "supports ada-errors")
(defmacro ada-begin (body &optional exceptions)
  (setq body (cond ((cdr body) `(progn .,body)) (T (car body))))
  (cond ((null exceptions) body)
        (T `(if (eq 'ada-error (*catch 'ada-exception ,body))
                (cond .,(mapcar #'(lambda (except)
                                    `((ada-test-when ',(car except))
                                      .,(cdr except)))
                                exceptions) )))))
(defun ada-test-when (exception-names)
  (or (member '(others) exception-names)
      (member ada-last-exception exception-names) ))
(defmacro ada-loop (body &optional exceptions)        ;
  `(ada-begin ((prog () L ,@body (go L))) ,exceptions))
(defmacro ada-while (test body &optional exceptions)
  `(ada-begin ((prog () L (ada-exit (ada-fn (not) ,test)) ,@body (go L)))
              ,exceptions))
(defmacro ada-cond (&rest clauses)
  `(cond ., (mapcar #'(lambda (c) `((ada-test-true ,(car c)) .,(cdr c)))
                    clauses)))
(defmacro ada-return (&optional value) ;not supported inside a loop
  `(return ,value))
(defmacro ada-exit (&optional condition) ;only local loop exit
  (cond ((null condition) '(return nil))
        (T `(if (ada-test-true ,condition) (return nil)))))
(defun ada-test-true (ada-bool)
  (ada-unbool (ada-value-data ada-bool)))
(defun ada-raise (&quote &rest names)
  (if names (setq ada-last-exception (car names)))
  (ada-reraise))
(defun ada-reraise ()
  (*throw 'ada-exception 'ada-error))
(defun ada-setq (left right)
  (setf (ada-value-data left) (ada-value-data right)))
;;; PDF page 136

;After this point we have the definitions of the standard ada fns and types
;Note that we have to define them all in occordance with the way things are
;handled above in the general case so that everything fits together.
(ada-type-dcl integer (private))
(ada-type-dcl boolean (ada-enumeration true false))
(ada-type-dcl character (private))
;this is used only in this file to define the standard ada fns below
;note you have to use the full correct names when you call this.
(defmacro def-ada (name var-type-pairs return-type &body body)
  (let* ((type-list (mapcar #'cadr var-type-pairs))
         (vars (mapcar #'car var-type-pairs))
         (full-vars (mapcar #'(lambda (v) (symbolconc v '|-full|)) vars)))
    (cond (return-type
           (cond ((cdr body) (setq body (cons 'progn body)))
                 (T (setq body (car body))))
           (setq body `((ada-value-build ',return-type ,body))))
          (T (setq body (append body '((values))))))
    `(def-ada-fn-internal ',name ',type-list
       `(lambda ,full-vars
          (let ,(mapcar #'(lambda (v fv) `(,v (ada-value-data ,fv)))
                        vars full-vars)
            .,body)))))
;this is used to change OUT parameters
(defmacro setq-param (param value)
  `(progn (setq ,param ,value)
          (setf (ada-value-data ,(symbolconc param '|-full|)) ,param)))
;;; PDF page 137

;basic operators and functions
(defun ada-bool (x)
 (cond (x 'true) (T 'false)))
(defun ada-unbool (x)
 (eq x 'true))
(def-ada (OR) ((x (boolean)) (y (boolean))) (boolean)
  (ada-bool (or (ada-unbool x) (ada-unbool y))))
(def-ada (XOR) ((x (boolean)) (y (boolean))) (boolean)
 (ada-bool (and (or (ada-unbool x) (ada-unbool y))
                 (not (and (ada-unbool x) (ada-unbool y))))))
(def-ada (AND) ((x (boolean)) (y (boolean) )) (boolean)
  (ada-bool (and (ada-unbool x) (ada-unbool y))))
(def-ada (NOT) ((x (boolean))) (boolean)
  (ada-bool (not (ada-unbool x))))
(def-ada (=) ((x (ada-string)) (y (ada-string))) (boolean)
  (ada-bool (equal x y)))
(def-ada (//=) ((x (ada-string)) (y (ada-string))) (boolean)
  (ada-bool (not (equal x y))))
(def-ada (=) ((x (ada-enumeration)) (y (ada-enumeration))) (boolean)
  (ada-bool (eq x y)))
(def-ada (//=) ((x (ada-enumeration)) (y (ada-enumeration))) (boolean)
  (ada-bool (not (eq x y))))
(def-ada (=) ((x (integer)) (y (integer))) (boolean)
  (ada-bool (= x y)))
(def-ada (//=) ((x (integer)) (y (integer))) (boolean)
  (ada-bool (not (= x y))))
(def-ada (=) ((x (character)) (y (character))) (boolean)
  (ada-bool (= x y)))
(def-ada (//=) ((x (character)) (y (character))) (boolean)
  (ada-bool (not (= x y))))
(def-ada (&) ((x (ada-string)) (y (ada-string))) (ada-string)
   (format nil "~A~A" x y))
(def-ada (+) ((x (integer)) (y (integer))) (integer)
   (+ x y))
(def-ada (-) ((x (integer)) (y (integer))) (integer)
   (- x y))
(def-ada (u-) ((x (integer))) (integer)
   (- x))
(def-ada (u+) ((x (integer))) (integer)
   x)
(def-ada (ABS) ((x (integer))) (integer)
   (abs x))
(def-ada (*) ((x (integer)) (y (integer))) (integer)
   (* x y))
(def-ada (//) ((x (integer)) (y (integer))) (integer)
   (// x y))
(def-ada (MOD) ((x (integer)) (y (integer))) (integer)
   (rem x y))
(def-ada (rem) ((x (integer)) (y (integer))) (integer)
   (rem x y))
(def-ada (**) ((x  (integer)) (y (integer))) (integer)
   (^ x y))
;;; PDF page 138

(def-ada (<) ((x (integer)) (y (integer))) (boolean)
  (ada-bool (< x y)))
(def-ada (>) ((x (integer)) (y (integer))) (boolean)
  (ada-bool (> x y)))
(def-ada (<=) ((x (integer)) (y (integer))) (boolean)
   (ada-bool (or (= x y) (< x y))))
(def-ada (>=) ((x (integer)) (y (integer))) (boolean)
   (ada-bool (or (= x y) (> x y))))
(def-ada (<) ((x (ada-string)) (y (ada-string))) (boolean)
   (ada-bool (alphalessp x y)))
(def-ada (>) ((x (ada-string)) (y (ada-string))) (boolean)
   (ada-bool (alphalessp y x)))
(def-ada (<=) ((x (ada-string)) (y (ada-string))) (boolean)
   (ada-bool (not (alphalessp y x))))
(def-ada (>=) ((x (ada-string)) (y (ada-string))) (boolean)
  (ada-bool (not (alphalessp x y))))
(def-ada (<) ((x (character)) (y (character))) (boolean)
   (ada-bool (< x y)))
(def-ada (>) ((x (character)) (y (character))) (boolean)
   (ada-bool (> x y)))
(def-ada (<=) ((x (character)) (y (character))) (boolean)
   (ada-bool (or (= x y) (< x y))))
(def-ada (>=) ((x (character)) (y (character))) (boolean)
   (ada-bool (or (= x y) (> x y))))
;;; PDF page 139

(def-ada-top calendar nil nil
    (def-ada-package CALENDAR
  ((ada-type-dcl time (private))
   (ada-subtype-dcl year_number (ada-range (integer) 1901 2099))
   (ada-subtype-dcl month_number (ada-range (integer) 1 12))
   (ada-subtype-dcl day_number (ada-range (integer) 1 31)))))
(def-ada (time_of calendar) ((year (year_number calendar))
                             (month (month_number calendar))
                             (day (day_number calendar)))
    (time calendar)
  (if (not (check-date year month day))
      (ada-raise (time_error)))
  (make-time year month day))
(defun make-time (year month day)
  (+ (* year 10000.) (* month 100.) day))
(def-ada (>= calendar) ((x (time calendar)) (y (time calendar)))
    (boolean)
  (ada-bool (> x y)))
(def-ada (year calendar) ((time (time calendar)))
  (year_number calendar)
  (// time 10000.))
(def-ada (month calendar) ((time (time calendar)))
    (month_number calendar)
  (remainder (// time 100.) 100.))
(def-ada (day calendar) ((time (time calendar)))
    (day_number calendar)
  (remainder time 100.))
(def-ada (clock calendar) () (time calendar)
  (multiple-value-bind (second minutes hours date month year)
      (time:get-time)
    (make-time (+ 1900 year) month date)))
(defun check-date (year month day)
  (and (fixnump year)
       (fixnump month)
       (fixnump day)
       (and (<= 1950 year) (<= year 2050))
       (and (<= 1 month) (<= month 12))
       (and (<= 1 day) (<= day (days-in-month month year)))))
(defun days-in-month (month year)
  (cond ((member month  '(1 3 5 7 8 10 12)) 31)
        ((member month  '(4 6 9 11)) 30)
        ((and (zerop (\ year 4))
              (or (not (zerop (\ year 100)))
                  (zerop (\ year 1000))))
         ;;; )
        (T 28)))
;;; PDF page 140

;here are some file operation utilities.
;In lisp the files are just stored as lists, and the functions
;operate on these lists. In other languages, real file IO would occure.
;Note that these functions are not themselves part of the demo
;they are just used by it. As a resuit, they can use
;LISP features not understood by the PA.   :
;this stores the info about a file including the data.
    (defstruct (file :conc-name (:type :named-list))
  name ;name of the file
  mode ;CLOSED, IN_FILE, OUT_FILE, INOUT_FILE
;for non-TEXT_IO files
  index ;position pointer for sequential reading
  data ;list of the records in the file
;for TEXT_IO files
  page ;the current page number. init 1
  line ;the current line number. init 1
  charpos ;the current character number. init 1
  obj) ;the external file object if open.
    (defvar ada-old-files nil "for protection when errors happen")
    (defvar ada-current-input)
    (defvar ada-current-output)
    (defvar ada-standard-input)
    (defvar ada-standard-output)
;you must call this any time before calling a fn that does 10.
    (defun ada-init ()
  (mapcar #'close ada-old-files)
  (setq ada-old-files nil)
  (setq ada-standard-input
        (make-file mode 'in_file obj standard-input) )
  (setq ada-standard-output
        (make-file mode 'out_file
                   page 1
                   line 1
                   charpos 1
                   obj standard-output) )
  (setq ada-current-input ada-standard-input)
  (setq ada-current-output ada-standard-output) )
;;; PDF page 141

    (def-ada-top text_io nil nil
    (def-ada-package text_io
 ((ada-type-dcl file_type (limited-private) )
  (ada-type-dcl file_mode (ada-enumeration in_file out_file))
  (ada-type-dcl count (ada-range nil 0 10000))
  (ada-subtype-dcl positive_count (ada-range  (count) 0 10000))
   (def-ada-generic
     ((ada-type-dcl num (ada-range nil <>)))
     (def-ada-package integer_io))))
;real package body needed for INTEGER IO so that generic will work right.
 (def-ada-package-body text_io
  ((def-ada-package-body integer_io
     ((def-ada-fn (put ((obj (num)) (width (integer))))
        () ((ada-fn (ada_int_put) obj width) ))
      (def-ada-fn (put ((obj (num))))
        () ((ada-fn (ada_int_put) obj (int-lit 9))))
      (def-ada-fn (put ((var (ada-string)) (obj (num))) )
        () ((ada-fn (ada_int_put_str) var obj)))
      (def-ada-fn (get ((var (num))) )
        () ((ada-fn (ada_int_get) var))))))))
    (def-ada (create text_io)
    ((file (file_type  text_io))
     (mode (file_mode text_io))
     (name (ada-string))) nil
  (let ((disk-file (open name 'out)))
    (push disk-file ada-old-files)
    (setq-param file
                (make-file name name mode mode
                           index nil data nil
                           page 1 line 1
                           charpos 1 obj disk-file))))
    (def-ada (close text_io) ((file (file_type text_io))) nil
  (close (file-obj file))
  (setf (file-mode file) 'closed))
    (def-ada (set_output text_io) ((file (file_type text_io))) nil
  (setq ada-current-output file))
    (def-ada (standard_output text_io) () (file_type text_io)
  ada-standard-output)
    (def-ada (newline  text_io) () nil
  (setf (file-line ada-current-output)
        (1+ (file-line ada-current-output)))
  (setf (file-charpos ada-current-output) 1)
  (format (file-obj ada-current-output) "~%"))
    (def-ada (new_line text_io) ((num (positive_count text_io))) nil
  (setf (file-line ada-current-output)
        (+ num (file-line ada-current-output)))
  (setf (file-charpos ada-current-output) 1)
  (format (file-obj ada-current-output) "~V%" num) )
    (def-ada (new_page text_io) () nil
  (setf (file-page ada-current-output)
        (1+  (file-page ada-current-output)))
   (setf (file-line ada-current-output) 1)
   (setf (file-charpos ada-current-output) 1)
   (format (file-obj ada-current-output) "~/|"))
;;; PDF page 142

    (def-ada (set_col text_io) ((num (positive_count text_io))) nil
  (format (file-obj ada-current-output) "~VX"
          (- num (file-charpos ada-current-output) ))
  (setf (file-charpos ada-current-output) num) )
    (def-ada (page text_io) () (positive_count text_io)
  (file-page ada-current-output) )
    (def-ada (line text_io) () (positive_count text_io)
  (file-line ada-current-output) )
;TEXT_IO for characters
    (def-ada (get text_io) ((var (character))) nil
  (setq-param var (tyi (file-obj ada-current-input))))
    (def-ada (put text_io) ((obj (character))) nil
  (setf (file-charpos ada-current-output)
        (1+ (file-charpos ada-current-output)))
  (tyo obj (file-obj ada-current-output)))
;TEXT_IO for strings
    (def-ada (get text_io) ((var (ada-string))) nil
  (setq-param var (readline (file-obj ada-current-input))))
    (def-ada (put text_io) ((obj (ada-string))) nil
  (setf (file-charpos ada-current-output)
        (+ (string-length obj) (file-charpos ada-current-output) ))
  (princ obj (file-obj ada-current-output)))

;TEXT_IO for integers
    (def-ada (ada_int_put) ((obj (integer)) (width (integer))) nil
  (setf (file-charpos ada-current-output)
        (+ width (file-charpos ada-current-output)))
  (format (file-obj ada-current-output) "~VD" width obj))
    (def-ada (ada_int_put_str) ((var (ada-string)) (obj  (integer))) nil
   (setq-param var (format nil "~VD" (ada-string-length
                                      (ada-value-type |VAR-full|)) obj)))
    (def-ada (ada_int_get)  ((var (integer))) nil
   (setq-param var
               (readlist
                 (exploden (readline (file-obj ada-current-input))))))
 (defun ada-string-length (type)
   (cond ((eq (car type) 'ada-string) (1+ (- (caddr type) (cadr type))))
         (T (ada-string-length (cdr (ada-find 'type type T))))))
;;; PDF page 143

    (def-ada-top sequential_io nil nil
 (def-ada-generic
  ((ada-type-dcl element_type (private) ))
  (def-ada-package sequential_io nil                                <
    ((ada-type-dcl file_type (limited-private))
     (ada-type-dcl file_mode (ada-enumeration in_file out_file)))))
 (def-ada-package-body sequential_io nil    :
  ((def-ada-fn (create
                ((file (file_type)) (mode (file_mode)) (name (ada-string))) )
     () ((ada-fn (ada-file-open) file mode name (enum-lit TRUE) )))
   (def-ada-fn (create ((file (file_type)) (mode (file_mode))))
     () ((ada-fn (ada-file-open) file mode (str-lit "") (enum-lit TRUE))))
   (def-ada-fn (open
       ((file (file_type)) (mode (file_mode)) (name (ada-string))) )
     () ((ada-fn (ada-file-open) file mode name (enum-lit FALSE))))
   (def-ada-fn (reset ((file (file_type))) )
     () ((ada-fn (ada-file-reset) file)))
   (def-ada-fn (close ((file (file_type))) )
     () ((ada-fn (ada-file-close) file)))
   (def-ada-fn (read  ((file (file_type)) (var (element_type))) )
     () ((ada-fn (ada-file-read) file var (int-lit 0))))
   (def-ada-fn (write ((file (file_type)) (item (element_type))) )
     () ((ada-fn (ada-file-write) file item (int-lit 0))))
   (def-ada-fn (end_of_file ((file (file_type))) (boolean) )
     () ((ada-return (ada-fn (ada-end_of_file) file)))))))
;;; PDF page 144

    (def-ada-top direct_io nil nil
 (def-ada-generic
  ((ada-type-dcl element_type (private) ))
  (def-ada-package direct_io
    ((ada-type-dcl file_type (limited-private) )
     (ada-type-dcl file_mode (ada-enumeration in_file out_file))
     (ada-type-dcl count (ada-range nil 0 10000))
     (ada-subtype-dcl positive_count (ada-range (count) 1 10000)))))
 (def-ada-package-body direct_io
  ((def-ada-fn (create
       ((file (file_type)) (mode (file_mode)) (name (ada-string))) )
     () ((ada-fn (ada-file-open) file mode name (enum-lit TRUE))))
   (def-ada-fn (create ((file (file_type)) (mode (file_mode))))
     () ((ada-fn (ada-file-open) file mode (str-lit "") (enum-lit TRUE) )))
   (def-ada-fn (open      =
       ((file (file_type)) (mode (file_mode)) (name (ada-string))) )
     () ((ada-fn (ada-file-open) file mode name (enum-lit FALSE))))
   (def-ada-fn (reset ((file (file_type))) )
     () ((ada-fn (ada-file-reset) file)))
   (def-ada-fn (close ((file (file_type))) )
     () ((ada-fn (ada-file-close) file)))
   (def-ada-fn (read  ((file (file_type)) (var (element_type))) )
     () ((ada-fn (ada-file-read) file var (int-lit 0))))
   (def-ada-fn (read
       ((file (file_type) )
        (var (element_type) )
        (index (positive_count))) )
     () ((ada-fn (ada-file-read) file var index)))
   (def-ada-fn (write ((file (file_type)) (item (element_type))) )
     () ((ada-fn (ada-file-write) file item (int-lit 0))))
   (def-ada-fn (write
       ((file (file_type))
        (item (element_type) )
        (index  (positive_count))) )
      () ((ada-fn (ada-file-write) file item index)))
   (def-ada-fn  (end_of_file ((file (file_type))) (boolean) )
      () ((ada-return (ada-fn (ada-end_of_file) file))))
    (def-ada-fn (set_index
        ((file (file_type)) (index (positive_count))) )
      () ((ada-fn (ada-file-set-index) file index)))
    (def-ada-fn (index ((file (file_type))) (positive_count) )
      () ((ada-return (ada-fn (ada-file-index) file))))
    (def-ada-fn (size  ((file (file_type))) (count))
      () ((ada-return (ada-fn (ada-file-size) file)))))))
;;; PDF page 145

;these are the underlying implementation functions for direct and
;sequential 10.  Note use of (any) so these can get called by
;every instance.
    (def-ada (ada-file-open)
    ((file (any))
     (mode (any))
     (name (ada-string))
     (createp (boolean))) nil
  (setq-param file
              (make-file name name
                         mode mode
                         index 1
                         data (if (not (ada-unbool createp))
                                  (ada-stored-data name)))))
    (def-ada (ada-file-reset) ((file (any))) nil
  (setf (file-index file) 1))
    (def-ada (ada-file-close) ((file (any))) nil
  (ada-store-data (file-name file) (file-data file))
  (setf (file-mode file) 'closed))
    (def-ada (ada-file-read)
    ((file (any)) (var (any)) (index (integer))) nil
  (if (not (zerop index)) (setf (file-index file) index))
  (if (ada-end_of_file file) (ada-raise (end_error)))
  (setf (file-index file) (1+ (file-index file)))
  (setq-param var (nth (- (file-index file) 2) (file-data file))))
    (def-ada (ada-file-write)
    ((file (any)) (item (any)) (index (integer))) nil
   (if (not (zerop index)) (setf (file-index file) index))
   (prog ()
    L  (if (not (ada-end_of_file file)) (return nil))
       (setf (file-data file) (nconc (file-data file) (ncons nil)))
       (go L))
   (setf (file-index file) (1+ (file-index file)))
   (setf (nth (- (file-index file) 2) (file-data file)) item))
 (def-ada (ada-file-set-index) ((file (any)) (index (integer))) nil
   (setf (file-index file) index))
 (def-ada (ada-file-index) ((file (any))) (integer)
   (file-index file))
 (def-ada (ada-file-size) ((file (any))) (integer)
   (length (file-data file)))
 (def-ada (ada-end_of_file) ((file (any))) (boolean)
   (ada-bool (ada-end_of_file file)))
 (defun ada-end_of_file (file)
   (> (file-index file) (length (file-data file))))
;;; PDF page 146

;This stores data items between uses of a file.
    (defvar ada-data-storage nil "simulates disk storage")
    (defun ada-stored-data (name)
  (cdr (assoc name ada-data-storage)))
    (defun ada-store-data (name data)
  (let* ((spot (assoc name ada-data-storage) ))
    (cond ((equal name ""))
          ((null spot)
           (push (cons name data) ada-data-storage))
          (T (rplacd spot data)))))
;after here we have basic stuff you need to run the demo.
;;; isn't part of standard ada, but is part of what we take to be standard.
    (def-ada-top functions
   ((CALENDAR) (IO_EXCEPTIONS) (TEXT_IO)) ((CALENDAR) (TEXT_IO))
 (def-ada-package functions
  ((ada-new-package INT_IO (INTEGER_IO) (INTEGER))
   (ada-subtype-dcl INDEX_TYPE
                    (ada-range (INTEGER) 1 (ada-attribute (INTEGER) last)))
   (ada-type-dcl DATE_STRING_TYPE (ada-string 1 10))
   (def-ada-fn (FORMAT_DATE ((DATE (TIME))) (date_string_type)))
   (def-ada-fn (QUERY_USER_FOR_DATE () (TIME)))
   (def-ada-generic
    ((ada-type-dcl ITEM_TYPE (private))
     (ada-type-dcl FILE_TYPE (limited-private))
     (ada-type-dcl INDEX_TYPE (ada-range nil <>))
     (ada-with (> ((X (ITEM_TYPE)) (Y (ITEM_TYPE))) (BOOLEAN)) <>)
     (ada-with (READ ((FILE (FILE_TYPE)) (ITEM (ITEM_TYPE) out))) <>)
     (ada-with (WRITE ((FILE (FILE_TYPE)) (ITEM (ITEM_TYPE))
                                            (TO (INDEX_TYPE)))) <>)
     (ada-with (SET_INDEX ((FILE (FILE_TYPE)) (TO (INDEX_TYPE)))) <>)
     (ada-with (INDEX ((FILE (FILE_TYPE))) (INDEX_TYPE)) <>)
     (ada-with (END_OF_FILE ((FILE (FILE_TYPE))) (BOOLEAN)) <>))
    (def-ada-fn (SORT_file ((FILE (FILE_TYPE))))))
   (def-ada-generic
    ((ada-type-dcl FROM_ITEM_TYPE (private))
     (ada-type-dcl FROM_FILE_TYPE (limited-private))
     (ada-type-dcl TO_ITEM_TYPE (limited-private) )
     (ada-type-dcl TO_FILE_TYPE (limited-private) )
     (ada-with (BUILD ((COUNT (INTEGER)) (ITEM (FROM_ITEM_TYPE)))
                      (TO_ITEM_TYPE) ))
     (ada-with (READ ((FILE (FROM_FILE_TYPE) )
                     (ITEM  (FROM_ITEM_TYPE) out))) <>)
     (ada-with (WRITE ((FILE (TO_FILE_TYPE)) (ITEM (TO_ITEM_TYPE)))) <>)
     (ada-with (END_OF_FILE ((FILE (FROM_FILE_TYPE))) (BOOLEAN)) <>)
     (ada-with (RESET ((FILE (TO_FILE_TYPE)))) <>))
    (def-ada-fn (SUMMARIZE_file
    ((FROM_FILE (FROM_FILE_TYPE)) (TO_FILE (TO_FILE_TYPE))))))
;;; PDF page 147

   (def-ada-generic       :
     ((ada-type-dcl ELEMENT_TYPE (private) )
      (ada-type-dcl INDEX_TYPE (ada-range nil <>)))
     (def-ada-package CHAINED_IO
       ((ada-new-package UNDERLYING_IO (DIRECT_IO) (ELEMENT_TYPE) )
        (ada-subtype-dcl FILE_TYPE (FILE_TYPE  UNDERLYING_IO))
        (ada-type-dcl FILE_MODE (ada-enumeration IN_FILE INOUT_FILE OUT_FILE))
        (def-ada-fn (CREATE ((FILE (FILE_TYPE) in-out)
                            (MODE (FILE_MODE)) (NAME (ADA-STRING)))))
        (def-ada-fn (OPEN ((FILE (FILE_TYPE) in-out)
                              (MODE (FILE_MODE)) (NAME (ADA-STRING) ))))
        (def-ada-fn (CLOSE ((FILE (FILE_TYPE)  in-out))))
        (def-ada-fn (read ((FILE (FILE_TYPE))
    (ITEM (ELEMENT_TYPE) out)
    (FROM (INDEX_TYPE)))))
        (def-ada-fn (write ((FILE (FILE_TYPE))
                               (ITEM (ELEMENT_TYPE) )
                               (to (INDEX_TYPE)))))
        (def-ada-fn (END_OF_FILE ((FILE (FILE_TYPE))) (BOOLEAN) ))
        (def-ada-fn (free_index  ((FILE (FILE_TYPE))) (index_type)))
        (def-ada-fn (NULL_INDEX  ((INDEX  (INDEX_TYPE))) (BOOLEAN)))
        (def-ada-fn (NULL_INDEX  () (INDEX_TYPE)))
        (ada-dcl STATUS_ERROR
                 (ada-exception (STATUS_ERROR IO_EXCEPTIONS) ))
        (ada-dcl MODE_ERROR (ada-exception (MODE_ERROR  IO_EXCEPTIONS)))
        (ada-dcl NAME_ERROR (ada-exception  (NAME_ERROR IO_EXCEPTIONS)))
        (ada-dcl USE_ERROR  (ada-exception (USE_ERROR IO_EXCEPTIONS) ))
        (ada-dcl DEVICE_ERROR
                 (ada-exception  (DEVICE_ERROR IO_EXCEPTIONS) ))
        (ada-dcl END_ERROR  (ada-exception (END_ERROR IO_EXCEPTIONS) ))
        (ada-dcl DATA_ERROR
                  (ada-exception (DATA_ERROR IO_EXCEPTIONS))))))
   (def-ada-generic
      ((ada-type-dcl ELEMENT_TYPE (private))
       (ada-type-dcl key_TYPE (private)))
      (def-ada-package KEYED_IO
        ((ada-type-dcl combined_type
                       (ada-rec (key (key_type))
                                   (element (element_type))))
         (ada-new-package UNDERLYING_IO (DIRECT_IO) (combined_TYPE) )
         (ada-subtype-dcl FILE_TYPE (FILE_TYPE  UNDERLYING_IO))
         (ada-type-dcl FILE_MODE (ada-enumeration IN_FILE INOUT_FILE OUT_FILE))
         (def-ada-fn (CREATE ((FILE (FILE_TYPE) in-out)
                                 (MODE (FILE_MODE)) (NAME (ADA-STRING) ))))
         (def-ada-fn (OPEN  ((FILE (FILE_TYPE) in-out)
                               (MODE (FILE_MODE)) (NAME (ADA-STRING)))))
         (def-ada-fn (CLOSE ((FILE (FILE_TYPE) in-out))))
         (def-ada-fn (read  ((FILE (FILE_TYPE))
                               (ITEM (ELEMENT_TYPE) out)
                               (FROM (KEY_TYPE)))))
         (def-ada-fn (write ((FILE (FILE_TYPE))
                                (ITEM (ELEMENT_TYPE) )
                                (to (key_TYPE)))))
         (def-ada-fn (SET_KEY ((FILE (FILE_TYPE))
                                  (KEY (KEY_TYPE)))))
         (def-ada-fn (END_OF_FILE ((FILE (FILE_TYPE))) (BOOLEAN)))
         (ada-dcl STATUS_ERROR
                  (ada-exception (STATUS_ERROR IO_EXCEPTIONS) ))
         (ada-dcl MODE_ERROR (ada-exception (MODE_ERROR IO_EXCEPTIONS)))
         (ada-dcl NAME_ERROR (ada-exception (NAME_ERROR IO_EXCEPTIONS) ))
         (ada-dcl USE_ERROR (ada-exception (USE_ERROR IO_EXCEPTIONS)))
         (ada-dcl DEVICE_ERROR
                  (ada-exception (DEVICE_ERROR IO_EXCEPTIONS) ))
         (ada-dcl END_ERROR (ada-exception (END_ERROR IO_EXCEPTIONS) ))
         (ada-dcl DATA_ERROR
                  (ada-exception (DATA_ERROR IO_EXCEPTIONS))))))))
;;; PDF page 148

 (def-ada-package-body FUNCTIONS
  ((def-ada-fn (FORMAT_DATE ((DATE (TIME))) (DATE_STRING_TYPE) )
     ((ada-use (INT_IO))
      (ada-dcl MONTH_STRING (ada-string 1 2))
      (ada-dcl DAY_STRING (ada-string 1 2))
      (ada-dcl YEAR_STRING (ada-string 1 4)))
     ((ada-fn (PUT) MONTH_STRING (ada-fn (MONTH) DATE))
      (ada-fn (PUT) day_STRING (ada-fn (day) DATE))
      (ada-fn (PUT) year_STRING (ada-fn (year) DATE))
      (ada-return (ada-fn (&) (ada-fn (&) (ada-fn (&) (ada-fn (&) MONTH_STRING
                                                           (str-lit "//"))
                                 DAY_STRING)
                            (str-lit "//"))
                       YEAR_STRING) )))
   (def-ada-fn (QUERY_USER_FOR_DATE ((PROMPT (ADA-STRING) )) (TIME) )
     ((ada-use (INT_IO))
      (ada-dcl month (month_number))
      (ada-dcl day (day_number))
      (ada-dcl year (year_number) )
      (ada-dcl date (time)))
     ((ada-loop
         ((ada-begin
           ((ada-fn (new_line))
            (ada-fn (put) (ada-fn (&) (str-lit "Enter Date of ") PROMPT))
            (ada-fn (new_line)) (ada-fn (put) (str-lit "Month: "))
            (ada-fn (get) month)
            (ada-fn (put) (str-lit "Day: ")) (ada-fn (get) day)
            (ada-fn (put) (str-lit "Year: ")) (ada-fn (get) year)
            (ada-setq date (ada-fn (TIME_OF) year month day))
            (ada-exit))
           ((((DATA_ERROR)
              (CONSTRAINT_ERROR)
              (TIME_ERROR) )
              (ada-fn (PUT) (str-lit "Invalid DATE")) (ada-fn (NEW_LINE)))))))
        (ada-return date)))
;;; PDF page 149

   (def-ada-fn (SORT_file ((FILE (FILE_TYPE))))
     ((ada-init-dcl EDGE (INDEX_TYPE) (int-lit 1))
      (ada-dcl MAX (INDEX_TYPE))
      (ada-dcl EDGE_ITEM (item_type) )
      (ada-dcl MAX_ITEM (item_type) )
      (ada-dcl CURRENT_ITEM (item_type)))
     ((ada-loop
       ((ada-fn (SET_INDEX) FILE EDGE)
        (ada-exit (ada-fn (END_OF_FILE) FILE))
        (ada-fn (READ) FILE EDGE_ITEM)
        (ada-setq MAX EDGE)
        (ada-setq MAX_ITEM EDGE_ITEM)
        (ada-while (ada-fn (not) (ada-fn (END_OF_FILE) FILE))
         ((ada-fn (READ) FILE CURRENT_ITEM)
          (ada-cond ((ada-fn (>) CURRENT_ITEM MAX_ITEM)
                     (ada-setq max (ADA-FN (-) (ada-fn (INDEX) FILE) (INT-lit 1)))
                     (ada-setq MAX_ITEM CURRENT_ITEM)))))
        (ada-cond ((ada-fn (//=) max edge)
                   (ada-fn (WRITE) FILE MAX_ITEM EDGE)
                  -(ada-fn (WRITE) FILE EDGE_ITEM MAX)))
        (ada-setq edge (ada-fn (+) edge (int-lit 1)))))
      (ada-fn (SET_INDEX) FILE (int-lit 1))))
   (def-ada-fn (SUMMARIZE_file (( FROM_FILE (FROM_FILE_TYPE))
                                (TO_FILE (TO_FILE_TYPE))))
     ((ada-init-dcl COUNT (INTEGER) (int-lit 1))
      (ada-dcl CURRENT (from_item_type))
      (ada-dcl PRIOR (FROM_ITEM_TYPE)))
     ((ada-cond ((ada-fn (not) (ada-fn (END_OF_FILE) FROM_FILE))
                 (ada-fn (READ) FROM_FILE PRIOR)
                 (ada-while (ada-fn (not) (ada-fn (END_OF_FILE) FROM_FILE))
                  ((ada-fn (READ) FROM_FILE CURRENT)
                   (ada-cond ((ada-fn (//=) current prior)
                              (ada-fn (WRITE) TO_FILE
                                   (ada-fn (BUILD) COUNT PRIOR))
                              (ada-setq count (int-lit 1))
                              (ada-setq prior current))
                             ((enum-lit TRUE)
                              (ada-setq count (ada-fn (+) count (int-lit 1))))))))
                 (ada-fn (WRITE) TO_FILE (ada-fn (BUILD) COUNT CURRENT))
                 (ada-fn (RESET) TO_FILE)))))
;;; PDF page 150

   (def-ada-package-body CHAINED_IO
     ((def-ada-fn (FILE_MODE_CONVERT ((MODE (FILE_MODE)))
                  (FILE_MODE UNDERLYING_IO))
        () ((ada-cond ((ada-fn (=) MODE (enum-lit IN_FILE))
                       (ada-return (enum-lit IN_FILE UNDERLYING_IO)))
                      ((ada-fn (=) MODE (enum-lit out_FILE))
                       (ada-return (enum-lit out_FILE UNDERLYING_IO)))
                      ((enum-lit True)
                       (ada-return (enum-lit inout_FILE UNDERLYING_IO))))))
      (def-ada-fn (CREATE ((FILE (FILE_TYPE) in-out)
                              (MODE (FILE_MODE)) (NAME (ADA-STRING))))
        () ((ada-fn (CREATE UNDERLYING_IO) FILE
                 (ada-fn (FILE_MODE_CONVERT) MODE) NAME)))
      '(def-ada-fn (OPEN ((FILE (FILE_TYPE) in-out)
    (MODE (FILE_MODE)) (NAME (ADA-STRING))))
        () ((ada-fn (OPEN UNDERLYING_IO) FILE
                 (ada-fn (FILE_MODE_CONVERT) MODE) NAME)))
      (def-ada-fn (CLOSE ((FILE (FILE_TYPE) in-out)))
        () ((ada-fn (CLOSE UNDERLYING_IO) FILE)))
      (def-ada-fn (read ((FILE (FILE_TYPE))
                            (ITEM (ELEMENT_TYPE) out)
                            (FROM (INDEX_TYPE))))
        () ((ada-fn (READ UNDERLYING_IO)  FILE ITEM
                    (ada-convert (POSITIVE_COUNT UNDERLYING_IO) FROM))))
      (def-ada-fn (write ((FILE (FILE_TYPE))
                              (ITEM (ELEMENT_TYPE))
                              (to (INDEX_TYPE))))
        () ((ada-fn (write UNDERLYING_IO) FILE ITEM
                     (ada-convert (POSITIVE_COUNT UNDERLYING_IO) to))))
      (def-ada-fn  (END_OF_FILE ((FILE (FILE_TYPE))) (BOOLEAN))
        () ((ada-return  (ada-fn (END_OF_FILE UNDERLYING_IO) FILE))))
      (def-ada-fn  (free_index ((FILE (FILE_TYPE))) (index_type) )
         () ((ada-return (ada-fn (+) (int-lit 1)
                              (ada-fn (SIZE UNDERLYING_IO) FILE)))))
       (def-ada-fn (NULL_INDEX ((INDEX (INDEX_TYPE))) (BOOLEAN) )
         () ((ada-return (ada-fn (=) index (int-lit 0)))))
       (def-ada-fn (NULL_INDEX () (INDEX_TYPE))
         () ((ada-return (int-lit 0))))))
;;; PDF page 151

   (def-ada-package-body KEYED_IO
     ((def-ada-fn (FILE_MODE_CONVERT ((MODE (FILE_MODE)))
                  (FILE_MODE UNDERLYING_IO))
        () ((ada-cond ((ada-fn (=) MODE (enum-lit IN_FILE))
                       (ada-return (enum-lit IN_FILE UNDERLYING_IO)))
                      ((ada-fn (=) MODE (enum-lit out_FILE))
                       (ada-return (enum-lit out_FILE UNDERLYING_IO)))
                      ((enum-lit True)
                       (ada-return
                        (enum-lit inout_FILE UNDERLYING_IO))))))
      (def-ada-fn (CREATE ((FILE (FILE_TYPE) in-out)
                              (MODE (FILE_MODE)) (NAME (ADA-STRING))))
        () ((ada-fn (CREATE UNDERLYING_IO) FILE
                 (ada-fn (FILE_MODE_CONVERT) MODE) NAME)))
      (def-ada-fn (OPEN ((FILE (FILE_TYPE) in-out)
                            (MODE (FILE_MODE)) (NAME (ADA-STRING))))
        () ((ada-fn (OPEN UNDERLYING_IO) FILE
                 (ada-fn (FILE_MODE_CONVERT) MODE) NAME)))
      (def-ada-fn (CLOSE ((FILE (FILE_TYPE) in-out)))
        () ((ada-fn (CLOSE UNDERLYING_IO) FILE)))
      (def-ada-fn (SET_KEY ((FILE (FILE_TYPE)) (KEY (KEY_TYPE))))
        ((ada-dcl COMBINED (COMBINED_TYPE))
         (ada-dcl I (POSITIVE_COUNT UNDERLYING_IO)))
        ((ada-fn (RESET UNDERLYING_IO) FILE)
         (ada-while (ada-fn (not) (ada-fn (END_OF_FILE UNDERLYING_IO) FILE))
          ((ada-fn (READ UNDERLYING_IO) FILE COMBINED)
           (ada-cond ((ada-fn (=) (ada-select COMBINED KEY) KEY)
    (ada-setq I (ada-fn (-)
                                       (ada-fn  (index underlying_io) file)
                                       (int-lit  1)))
                      (ada-fn (set_index underlying_io) FILE I)
                      (ada-exit)))))))
      (def-ada-fn (read ((FILE (FILE_TYPE))
                            (ITEM (ELEMENT_TYPE) out)
                            (FROM (KEY_TYPE))))
        ((ada-dcl combined  (combined_type) ))
        ((ada-fn (set_key) file from)
    (ada-fn (READ UNDERLYING_IO)  FILE combined)
         (ada-setq item  (ada-select combined element))))
      (def-ada-fn (read  ((FILE (FILE_TYPE))
                             (ITEM (ELEMENT_TYPE) out)))
        ((ada-dcl combined  (combined_type)))
        ((ada-fn  (READ UNDERLYING_IO) FILE combined)
          (ada-setq item (ada-select combined element))))
       (def-ada-fn (write ((FILE (FILE_TYPE))
                              (ITEM (ELEMENT_TYPE)) (to (key_TYPE))))
         () ((ada-fn (set_key) file to)
             (ada-fn (write UNDERLYING_IO) FILE
                  (ada-construct (COMBINED_TYPE) TO ITEM))))
       (def-ada-fn (END_OF_FILE ((FILE (FILE_TYPE))) (BOOLEAN))
         () ((ada-return (ada-fn (END_OF_FILE UNDERLYING_IO) FILE)))))))))
;;; PDF page 152

(def-ada-top maintenance_files
  ((CALENDAR) (FUNCTIONS) (TEXT_IO)) ((CALENDAR) (FUNCTIONS) )
 (def-ada-package maintenance_files
  ((ada-dcl report (FILE_TYPE TEXT_IO))
   (ada-constant-dcl models_name (ada-string) (str-lit "models.data"))
   (ada-type-dcl model_key_type (ada-string 1 3))
   (ada-type-dcl model_type
       (ada-rec (NAME (ada-string 1 16))
                (MAKER (ada-string 1 16))))
   (ada-new-package model_io (keyed_io) (model_type) (model_key_type) )
   (ada-dcl models (file_type model_io))
   (ada-constant-dcl defects_name (ada-string) (str-lit "defects.data"))
   (ada-type-dcl defect_key_type (ada-string 1 5))
   (ada-type-dcl defect_type
     (ada-rec (NAME (ada-string 1 32))
              (model (model_key_type))))
   (ada-new-package defect_io (keyed_io)  (defect_type) (defect_key_type))
   (ada-dcl defects (file_type defect_io))
   (ada-constant-dcl repairs_name  (ada-string) (str-lit "repairs.data"))
   (ada-subtype-dcl repair_index_type  (index_type))
   (ada-type-dcl repair_type
     (ada-rec (DATE (TIME))
              (DEFECT (DEFECT_KEY_TYPE) )
              (NEXT (REPAIR_INDEX_TYPE) )
              (COMMENT  (ada-string 1 32))))
   (ada-new-package repair_io  (chained_io)
                     (repair_type) (repair_index_type) )
   (ada-dcl repairs  (file_type repair_io))
   (ada-constant-dcl units_name (ada-string) (str-lit "units.data"))
   (ada-type-dcl unit_key_type  (ada-string 1 7))
   (ada-type-dcl unit_type
       (ada-rec  (MODEL (MODEL_KEY_TYPE) )
                 (REPAIR (REPAIR_INDEX_TYPE) )))
   (ada-new-package unit_io  (keyed_io) (unit_type) (unit_key_type) )
   (ada-dcl units  (file_type unit_io)))))
;;; PDF page 153
(def-ada-top create_test_records
  ((CALENDAR) (Maintenance_FILES)) ((CALENDAR) (Maintenance_FILES))
 (def-ada-fn (CREATE_TEST_RECORDS ())
  ((ada-use (DEFECT_IO) (MODEL_IO) (REPAIR_IO) (UNIT_IO))
   (def-ada-fn (CLEAN_UP ()) ()
     ((ada-fn (CLOSE) DEFECTS)
      (ada-fn (CLOSE) MODELS)
      (ada-fn (CLOSE) REPAIRS)
      (ada-fn (CLOSE) UNITS))
     ((((status_error)) (ada-return)))))
  ((ada-fn (CREATE) DEFECTS (ENUM-lit OUT_FILE) defects_name)
   (ada-fn (CREATE) MODELS (ENUM-lit OUT_FILE) models_name)
   (ada-fn (CREATE) REPAIRS (ENUM-lit OUT_FILE) repairs_name)
   (ada-fn (CREATE) UNITS (ENUM-lit OUT_FILE) units_name)
   (ada-fn (WRITE) MODELS (ada-construct (model_type)
                         (str-lit "Opal Sorter     ")
                         (str-lit "Perth Mining    "))
           (str-lit "OS1"))
   (ada-fn (WRITE) MODELS (ada-construct (model_type)
                         (str-lit "Gas Analyzer    ")
                         (str-lit "Benson Labs     "))
           (str-lit "GA2"))
   (ada-fn (WRITE) DEFECTS (ada-construct (defect_type)
                          (str-lit "Control board cold solder joint ")
                          (str-lit "GA2"))
           (str-lit "GA-11"))
   (ada-fn (WRITE) DEFECTS (ada-construct (defect_type)
                          (str-lit "Clogged gas injection port      ")
                          (str-lit "GA2"))
           (str-lit "GA-32"))
   (ada-fn (WRITE) DEFECTS (ada-construct (defect_type)
                          (str-lit "Power supply thermistor blown   ")
                          (str-lit "OS1"))
           (str-lit "OS-03"))
   (ada-fn (WRITE) REPAIRS
           (ada-construct (repair_type)
             (ada-fn (TIME_OF) (int-lit 1982) (int-lit 9) (int-lit 14))
             (str-lit "GA-32")
             (int-lit 0)
             (str-lit "Probably caused by humidity.    "))
           (int-lit 1))
   (ada-fn (WRITE) REPAIRS
           (ada-construct (repair_type)
             (ada-fn (TIME_OF) (int-lit 1984) (int-lit 2) (int-lit 25))
             (str-lit "OS-03")
             (int-lit 0)
             (str-lit "Sorter arm got stuck.           "))
           (int-lit 2))
   (ada-fn (WRITE) REPAIRS
           (ada-construct (repair_type)
             (ada-fn (TIME_OF) (int-lit 1984) (int-lit 1) (int-lit 23))
             (str-lit "GA-11")
             (int-lit 1)
             (str-lit "Took two days to find.          "))
           (int-lit 3))
   (ada-fn (WRITE) REPAIRS
           (ada-construct (repair_type)
             (ada-fn (TIME_OF) (int-lit 1984) (int-lit 3) (int-lit 19))
             (str-lit "GA-32")
             (int-lit 3)
             (str-lit "Port Diameter seems below specs."))
           (int-lit 4))
   (ada-fn (WRITE) UNITS (ada-construct (unit_type) (str-lit "OS1") (int-lit 2))
           (str-lit "OS1-271"))
   (ada-fn (WRITE) UNITS (ada-construct (unit_type) (str-lit "GA2") (int-lit 4))
           (str-lit "GA2-342"))
   (ada-fn (CLEAN_UP)))
  ((((DEVICE_ERROR) (END_ERROR)
;;; PDF page 154
     (NAME_ERROR) (STATUS_ERROR))
    (ada-fn (CLEAN_UP)) (ada-fn (PUT) (str-lit "Data Base Inconsistent")))
   (((others)) (ada-fn (CLEAN_UP)) (ada-raise)))))
