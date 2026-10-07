;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>ADA-UNPARSE.LISP
;;; PDF pages 203-211 (printed source pages 1-9).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;this is a general program.for printing code without abbreviation.
;Anybody anywhere who wants to print code in any langauge should call
;this fn.  Strictly speaking you should not be printing code any other way
;(except maybe in debugging).
;(there should be a better place for this fn than in this file.)
;Note that we use GPRINT to print ADA and PLI, but the intention
;;; that we don't use it anywhere else in the kbe any more.
(defun print-code (code-list stream &optional pp:*print-lines*)
  (let ((prinmargin pp:*print-right-margin*)
        (prinlevel nil)
        (prinlength nil)
        (prinendline pp:*print-lines*)
        (base 10.)
        (*nopoint t))
    (caseq *display-language*
      (lisp (let ((*print-pretty* T))
              (print code-list stream) ))
      (pli (print-as-pli code-list stream) )
      (ada (let (ada-supress)
             (declare (special ada-supress))
              ;;this sends messages that mess up any pprinting
             (Gprint1 code-list stream))))))
(comment
    (progn (setq prinlength 4)
       (setq prinmargin 83)
       (defun x () (pl test))) )
 ; some things for the editor interface.
(defprop def-ada-top "ADA definition" si:definition-type-name)
(defvar ada-functions nil "list of all ada functions")
(defun make-ada-fn (name)
   (if (not (memq name ada-functions)) (push name ada-functions)))
(defun ADA? (form)
   (and (listp form) (memq (car form) ada-functions)))
(defun ada-definition-type-name (form)
   (or (when (and (listp form) (eq (car form) 'def-ada-top))
         (let ((inner (nth 4 form)))
           (caseq (car inner)
             (def-ada-cliche "cliche")
             (def-ada-package "package")
             (def-ada-fn
               (if (caddr (cadr inner)) "function" "procedure")))))
       "program"))
;;; PDF page 204
(defvar ada-supress t "turns off ada printing")
(defmacro def-ada-gf (name ada-template)
  `(def-ada-gformat ,name form
     (gf ,ada-template form)))
(defmacro def-ada-gformat (name args &body ada-format)
  `(progn 'compile
     (make-ada-fn ',name)
     (defun (,name :Gformat) (|form|)
       (cond (ada-supress (non-ada-format |form|))
             (T (let ,(ada-process-argl '|form| args)
                  .,ada-format))))))
(defun non-ada-format (form)
  (cond ((memq (car form) '(def-ada-fn def-ada-package def-ada-package-body
                          def-ada-top def-ada-generic ada-begin ada-loop
                          ada-while def-ada-cliche def-ada-entry))
         (gf "(1*<-*>)" form))
        (T (:Gfn-format form))))
(eval-when (eval load compile)
    (defun ada-process-argl (var args)
  (do ((argl args (cdr argl))
       (i 1 (1+ i))
       (result))
      ((null argl) (reverse result))
    (cond ((listp argl) (push `(,(car argl) (nth ,i ,var)) result))
          (T (push `(,argl (nthcdr ,i ,var)) result) (setq argl nil))))))
;;; PDF page 205
;helping functions for common subparts of formats.
(defvar *nested-ada-fn-spec* nil)
(defun ada-fn-spec (fn-spec)
  (let ((name (car fn-spec))
        (params (cadr fn-spec))
        (return (caddr fn-spec)))
    (if *nested-ada-fn-spec*
        (cond (return (gf "{3'function'_"))
              (T (gf "{3'procedure'_")))
        (cond (return (gf "{0'function'_"))
              (T (gf "{0'procedure'_"))))
    (gf "*{3[.(1<%ada-param-gf ';',>)]" name params)
    (cond (return (gf "-'return '%ada-type " return)))
    (gf "}")))
(defun ada-param-gf (param-spec)
  (gf "{0*':'" (car param-spec))
  (if (cddr param-spec)
      (gf "_S" (cdr (assq (caddr param-spec)
                         '((in . |in|) (out . |out|)
                           (in-out . |in out|))))))
  (gf "_%ada-type }" (cadr param-spec)))
(defun ada-exceptions (list)
  (if list (gf "{3'exception'[<!%ada-when-clause >]}-" list)))
(defun ada-when-clause (pair)
  (gf "{3'when'[[<,%ada-qname ' /|'>]' =>'-<*';',>]';'}" pair))
;;; PDF page 206

(defun ada-dcls (body)
  (let ((*nested-ada-fn-spec* T))
    (gf "+3")
    (do ((forms body (cdr forms))
         (last-form nil (car forms)))
        ((null forms))
      (if (and last-form (ada-gap-between-dcls last-form forms))
          (gf "!' '"))
      (gf "!*';'" (car forms))
      (setq last-form (car forms)))
    (gf "+-3!")))
(defun ada-gap-between-dcls (dcl1 dcls)
  (or (and (not (ada-interesting-def dcl1))
           (ada-interesting-def (car dcls)))
      (ada-starts-file-def dcl1 dcls)
      (ada-ends-file-def dcl1)))
(defun ada-interesting-def (x)
  (or (memq (car x) '(ada-new-fn def-ada-generic
                      def-ada-package def-ada-package-body) )
      (and (eq (car x)  'def-ada-fn)
           (or (nth 2 x) (nth 3 x) (nth 4 x)))))
(defun ada-ends-file-def (dcl)
  (and (eq (car dcl)  'ada-dcl) (eq (caaaddr dcl) 'file_type)))
(defun ada-starts-file-def  (dcl1 dcls)
   (let (root)
      (dolist (dcl dcls)
       (when  (ada-ends-file-def dcl)
          (setq root (singular-form (cadr dcl)))
          (return)))
      (and root
           (not (eq 0 (string-search (string root)
                                     (first-symbol (cadr dcl1)))))
           (eq 0 (string-search (string root)
                                (first-symbol (cadar dcls)))))))
;;; PDF page 207

(defun ada-statements (body)
  (gf "+3")
  (do ((forms body (cdr forms))
       (in-group nil)
       (after-group nil))
      ((null forms) (if in-group (gf "}")))
    (caseq (ada-statement-classify (car forms))
      (out-of-group
        (cond (in-group (gf "}-*';'" (car forms)))
              (after-group (gf "-*';'" (car forms)))
              (T (gf "-*';'" (car forms))))
        (setq in-group nil after-group nil))
      (in-group
        (cond (in-group (gf ",*';'" (car forms)))
              (after-group (gf ",{0*';'" (car forms)))
              (T (gf "-{0*';'" (car forms))))
        (setq in-group T after-group nil))
      (start-group
        (cond (in-group (gf "},{0*';'" (car forms)))
              (after-group (gf ",{0*';'" (car forms) ))
              (T (gf "-{0*';'" (car forms))))
        (setq in-group T after-group nil))
      (end-group
        (cond (in-group (gf ",*';'}" (car forms)))
              (after-group (gf ",*';'" (car forms) ))
              (T (gf "-*';'" (car forms))))
        (setq in-group nil after-group T))))
  (gf "+-3-"))
 ;four kinds - starts-group ends-group in-group out-of-group.
(defun ada-statement-classify (call)
  (cond ((not (eq (car call)  'ada-fn)) 'out-of-group)
        (T (caseq (caadr call)
              ((put open close create) 'in-group)
              ((set_col) 'start-group)
              ((get new_line new_page) 'end-group)
              (T 'out-of-group)))))
;;; PDF page 208
;formats for ada pseudo-lisp
(def-ada-gformat def-ada-top (ignore with use . defs)
  (let ((*nested-ada-fn-spec* nil))
    (if with (gf "{5'with '[<%ada-qname ',',>]';'}!" with))
    (if use (gf "{4'use '[<%ada-qname ',',>]';'}!" use))
    (gf "[<*';'! ' '!>]';'" defs)))
(def-ada-gformat def-ada-package (name dcls)
  (gf "{0'package '*' is'%ada-dcls 'end '*}" name dcls name))
(def-ada-gformat def-ada-package-body (name dcls)
  (gf "{0'package body '*' is'%ada-dcls 'end '*}" name dcls name))
(def-ada-gformat def-ada-entry (fn-spec)
  (gf "'entry '*" (car fn-spec)))
(def-ada-gformat def-ada-fn (fn-spec dcls body except)
  (ada-fn-spec fn-spec)
  (if (or dcls body except)
      (gf "' is'%ada-dcls {0'begin'%ada-statements %ada-exceptions 'end '*}"
          dcls body except (car fn-spec)))
  (gf "}"))
(def-ada-gformat def-ada-cliche (name plist . rest)
  (GF "{0'cliche '*" name)
  (when (or plist (car rest) (cadr rest) (caddr rest))
    (GF "' is'%ada-cliche-plist " plist)
    (cond ((cdr rest)
           (if (not (or (cadr rest) (caddr rest)))
               (setq rest (ncons (car rest))))
           (gf "[%ada-dcls 'begin'%ada-statements %ada-exceptions ]" rest))
          (T (gf "!*" (car rest))))
    (GF "!'end '*" name))
  (gf "}"))
(defun ada-cliche-plist (plist)
  (gf "+3!{0")
  (do ((list plist (cddr list)))
      ((null list))
    (caseq (car list)
      (primary-roles (gf "!{14'primary roles '[<*',',>]';'}" (cadr list)))
      (described-roles (gf "!{16'described roles '[<*',',>]';'}" (cadr list)))
      (comment (gf "!{8'comment /"'{0[<S,>]}'/"';'}"
                   (decompose-cliche-comment (cadadr list))))
      (constraints
       (gf "!{0'constraints'%ada-statements 'end constraints;'}" (cadr list)))))
  (gf "!' '}+-3"))
;;; PDF page 209
(def-ada-gf def-ada-generic "{0['generic'%ada-dcls *]}")
(def-ada-gf ada-with "'with '[%ada-fn-spec ' is '*]}")
(def-ada-gf ada-new-package
  "{3'package '[*' is'-'new '%ada-qname .(1<%ada-qname ',',>)]}")
(def-ada-gf ada-new-fn
  "[%ada-fn-spec ' is'-'new '%ada-qname .(1<%ada-qname ',',>)]}")
(def-ada-gf ada-dcl "{3[*': '%ada-type ]}")
(def-ada-gf ada-init-dcl "{3[*': '%ada-type ' := '*]}")
(def-ada-gf ada-constant-dcl "{3[*': constant '%ada-type ' := '*]}")
(def-ada-gf ada-type-dcl "{3['type '*' is'-%ada-type ]}")
(def-ada-gf ada-subtype-dcl "{3['subtype '*' is'-%ada-type ]}")
(def-ada-gf ada-use "{4'use '[<%ada-qname ',',>]}")
(defun ada-type (args)
  (cond ((memq (car args) '(ada-enumeration ada-string ada-rec
                          ada-range ada-exception kbe-/#-comment-macro))
         (gf "*" args))
        (T (ada-qname args))))
(def-ada-gf ada-enumeration "(<*',',>)")
(def-ada-gf ada-string "'STRING'[.['('*'..'*')']]")
(def-ada-gf ada-exception "{3'exception'[-'renames '%ada-qname ]}")
(def-ada-gf ada-range "{0[[.%ada-qname _]'range '*'..'*]}")
(def-ada-gf ada-rec
  "{0{3'record'[<!%ada-field ';'>]';'}!'end record'}")
(defun ada-field (args)
  (cond ((eq (car args) 'kbe-/#-comment-macro) (gf "*" args))
        (T (gf "[*': '%ada-type ]" args))))
(def-ada-gformat kbe-/#-comment-macro (code name)
  (let* ((comment (kbe-comment-name->string name)))
    (cond ((null code) (GF "{1'{'S'}'}" comment))
          ((and (listp (car code)) (eq (caar code) '...))
           (GF "{1'{'S'}'" comment)
           (if (cdar code)
               (gf "(1<*','->)}" (handle-&in-out (cdar code)))))
          (T (GF "{2'{'[*', 'B.]S'}'}" code comment)))))
;note that for internal ease in general the comment guy
;has to have the same name it has elsewhere.
;unfortunately that means that the place it is
;defined has to know to call this to print it in ada.
;when appropriate.
(defun ada-kbe-/#-comment-macro-printer (form)
  (declare (special *input-language*))
  (if ada-supress
      (let ((*input-language* 'lisp)) (gf "*" form))
      (let* (((code name) (cdr form))
             (comment (kbe-comment-name->string name)))
        (cond ((null code) (GF "{1'{'S'}'}" comment))
              ((and (listp (car code)) (eq (caar code) '...))
               (GF "{1'{'S'}'" comment)
               (if (cdar code)
                   (gf "(1<*','->)}" (handle-&in-out (cdar code)))))
              (T (GF "{2'{'[*', 'B.]S'}'}" code comment))))))
;;; PDF page 210
(def-ada-gf ada-begin "{0['begin'%ada-statements %ada-exceptions ]'end'}")
(def-ada-gf ada-loop
  "{0['loop'%ada-statements %ada-exceptions ]'end loop'}")
(def-ada-gf ada-while
  "{0['while '*' loop'%ada-statements %ada-exceptions ]'end loop'}")
(def-ada-gformat ada-cond clauses
  (prog (first)
        (setq first T)
      L (cond ((null clauses) (GF "!'end if'") (return nil)))
        (cond (first (setq first nil)
                     (GF "!'if '*' then '%ada-statements "
                         (caar clauses) (cdar clauses)))
              ((equal (caar clauses) '(enum-lit true))
               (GF "!'else '%ada-statements " (cdar clauses)))
              (T (GF "!'elsif '*' then '%ada-statements "
                     (caar clauses) (cdar clauses))))
        (setq clauses (cdr clauses))
        (go L)))
(def-ada-gf ada-return "'return'[_*]")
(def-ada-gf ada-exit "'exit'[' when '*]")
(def-ada-gf ada-raise "'raise'[_*]")
(def-ada-gf ada-setq "[*' := '*]")
(def-ada-gf ada-convert "{0[%ada-qname .(*)]}")
(def-ada-gf ada-attribute "{0[%ada-type '''* ]}")
(def-ada-gf ada-construct "{0[%ada-qname '''.(1<*',',>)]}")
(def-ada-gf ada-select "{0[*'.'*]}")
(def-ada-gf str-lit "{'/"'[S]'/"'}")
(def-ada-gf enum-lit "%ada-qname ")
(def-ada-gf int-lit "[*]")
;note that the qualifiers in a name are in revese order than in ada
;this is a bit confusing but makes much of the computation much easier
;Also note how three keywords are handled here.
(defun ada-qname (args)
  (cond ((equal args '(private)) (gf "'private'"))
        ((equal args '(limited-private)) (gf "'limited private'"))
        ((equal args '(others)) (gf "'others'"))
        (T (gf "{[<*'.'>]}" (reverse args)))))
(defun ada-lower-case (list)
  (mapcar #'ada-lower-case-1 list))
(defun ada-lower-case-1 (symbol)
  (maybe-intern (string-downcase (string symbol)) (symbol-package symbol)))
(defun ada-lower-case-0 (char)
  (cond ((or (< char #/A) (> char #/Z)) char)
        (t (+ char #.(- #/a #/A)))))
;;; PDF page 211
;Operators are converted to infix. Priority and parentheses are
;kept track of by the variable ADA-BIND. This has the binding
;strength of the parent operator. If an op is a left operand
;and has less binding strength than its parent then parentheses are
;use. If an op is a right operand and has less than or equal to
;binding strength than its parent then parentheses are used.
;We take advantage of the fact that statements cannot occur in
;expressions and only bind ada-bind in the stuff below.
(defvar ada-bind 0 "the binding strength of the parent operation")
(def-ada-gformat ada-fn (fn . args)
  (let* ((parent-ada-bind ada-bind)
         (ada-bind (ada-bind-level fn args))
         (need-parens (not (> ada-bind parent-ada-bind))))
    (cond ((not (ada-op-? (car fn)))
           (GF "{0%ada-qname " fn)
           (if args (GF "(1<*',',>)" args))
           (GF "}"))
          (T (GF "{0")
             (if need-parens (GF "'('"))
             (if (cdr args)
                 (let ((ada-bind (1- ada-bind))) (GF "*B" (pop args))))
             (GF "S*" (ada-op-name fn) (car args))
             (if need-parens (GF "')'"))
             (GF "}")))))
(defvar ada-binary-op-info
  '((OR 1 | or |) (XOR 1 | xor |) (AND 1 | and |) (= 2 | = |)
    (//= 2 | //= |) (<= 2 | <= |) (>= 2 | >= |) (< 2 | < |)
    (> 2 | > |) (& 3 | & |) (+ 3 +) (- 3 -)
    (* 5 *) (// 5 //)
    (MOD 5 | mod |) (rem 5 | rem |) (** 6 **)))
(defvar ada-unary-op-info
  '((NOT 4 |not |) (ABS 4 |abs |) (- 4 -) (+ 4 +)))
(defun ada-op-? (name)
  (or (assq name ada-binary-op-info) (assq name ada-unary-op-info)))
(defun ada-bind-level (fn args)
  (cond ((not (ada-op-? (car fn))) 0)
        ((cdr args) (cadr (assq (car fn) ada-binary-op-info)))
        (T (cadr (assq (car fn) ada-unary-op-info)))))
(defun ada-op-name (fn)
  (or (caddr (assq (car fn) ada-binary-op-info))
      (caddr (assq (car fn) ada-unary-op-info))))
