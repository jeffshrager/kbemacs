;;; -*- Mode: LISP; Package: ZL -*-
;;;
;;; Zetalisp (Symbolics Release 4.5 era) compatibility for SBCL.
;;;
;;; Only what the KBE sources need, defined as narrowly as the sources
;;; allow. Internal helpers are named %ZL-...; every other symbol defined
;;; here is exported from ZL at the end of the load (see load.lisp).

(in-package "ZL")

(declaim (declaration arglist return-list sys::function-parent
                      *expr *fexpr *lexpr))

;;; (DECLARE (VALUES ...)) documents return values. VALUES is a CL symbol,
;;; so it can't be proclaimed a declaration; %ZL-SPLIT-BODY drops it.
(cl:defun %zl-clean-declare (form)
  "Returns FORM without (VALUES ...) specifiers, or NIL if none remain."
  (cl:let ((specs (cl:remove-if (lambda (s) (and (consp s) (eq (car s) 'values)))
                                (cdr form))))
    (cl:if specs (cons 'declare specs) nil)))

;;;; SETF
;;;
;;; Lisp Machine SETF consults a :SETF property on the place's operator,
;;; a function of (place value) returning the expansion. KBE's
;;; DEFINE-SETF-HANDLER (macro.lisp) uses it for destructuring assignment,
;;; (SETF [A B] X) and (SETF (LIST A B) X). SETF also takes #'name as a
;;; place: (setf #'sc #'show-db-context).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun %zl-setf-1 (place value)
    (cond ((and (consp place) (eq (car place) 'function))
           `(cl:setf (fdefinition ',(cadr place)) ,value))
          ((and (consp place) (symbolp (car place)) (cl:get (car place) :setf))
           (funcall (cl:get (car place) :setf) place value))
          (t `(cl:setf ,place ,value)))))

(cl:defmacro setf (&rest pairs)
  (cl:let ((forms (loop for (place value) on pairs by #'cddr
                        collect (%zl-setf-1 place value))))
    (cl:if (cdr forms) `(progn ,@forms) (car forms))))

;;;; Lambda lists and bodies

;;; Zetalisp accepts things CL does not:
;;;   - &BODY in ordinary lambda lists (same as &REST);
;;;   - variables named IGNORE, possibly repeated;
;;;   - &SPECIAL / &LOCAL markers;
;;;   - DECLARE forms that are not at the head of the body.

(cl:defun %zl-fix-lambda-list (ll &optional macro?)
  "Returns a CL lambda list and a list of variables to declare IGNORABLE."
  (cl:let ((out '()) (ignorable '()) (specials '()) (special-mode nil))
    (cl:do ((l ll (cdr l)))
        ((atom l)
         (when l (push '&rest out) (push l out))  ;dotted rest arg
         (values (nreverse out) ignorable specials))
      (cl:let ((x (car l)))
        (cond ((eq x '&body) (push (cl:if macro? '&body '&rest) out))
              ((eq x '&special) (setq special-mode t))
              ((eq x '&local) (setq special-mode nil))
              ((cl:member x '(&quote &eval &functional &list-of)) nil)
              ((member x lambda-list-keywords) (push x out))
              (t
               (cl:let* ((var (cl:if (consp x) (car x) x))
                         (name (cl:if (consp var) (cadr var) var)))
                 (cond ((and (symbolp name)
                             (string= (symbol-name name) "IGNORE"))
                        (cl:let ((g (cl:gensym "IGNORE")))
                          (push g ignorable)
                          (push (cl:if (consp x) (cons g (cdr x)) g) out)))
                       (t
                        (when special-mode (push name specials))
                        (push x out))))))))))

(cl:defun %zl-split-body (body &optional doc-allowed)
  "Returns (values declarations docstring forms). DECLARE forms anywhere
   at top level of BODY are hoisted, as the Lisp Machine compiler allowed."
  (cl:let ((decls '()) (doc nil) (forms '()))
    (cl:do ((l body (cdr l)))
        ((null l))
      (cl:let ((x (car l)))
        (cond ((and (consp x) (eq (car x) 'declare))
               (cl:let ((d (%zl-clean-declare x))) (when d (push d decls))))
              ((and doc-allowed (stringp x) (not doc) (cdr l) (null forms))
               (setq doc x))
              (t (push x forms)))))
    (values (nreverse decls) doc (nreverse forms))))

(cl:defun %zl-alias-params (ll)
  "Parameters named like a Zetalisp stream/printer variable, e.g.
   (&optional (standard-input standard-input)) in pseudo.lisp's
   READ-COMMAND: on the Lisp Machine that binds the special variable.
   Returns (values new-ll ((cl-variable . temp) ...))."
  (cl:let ((rebinds '()))
    (values
     (mapcar (lambda (x)
               (cl:let* ((var (cl:if (consp x) (car x) x)))
                 (cl:if (and (symbolp var)
                             (cl:assoc var (symbol-value '%zl-variable-aliases)))
                        (cl:let ((g (cl:gensym (symbol-name var))))
                          (push (cons (%zl-alias var) g) rebinds)
                          (cl:if (consp x) (cons g (cdr x)) g))
                        x)))
             ll)
     (nreverse rebinds))))

(cl:defun %zl-lambda-parts (ll body &key macro? doc-allowed)
  "Returns (values cl-lambda-list body-with-declarations)."
  (multiple-value-bind (ll ignorable specials) (%zl-fix-lambda-list ll macro?)
    (multiple-value-bind (ll rebinds) (%zl-alias-params ll)
      (multiple-value-bind (decls doc forms) (%zl-split-body body doc-allowed)
        (values ll
                `(,@(cl:if doc (list doc))
                  ,@(cl:if ignorable `((declare (ignorable ,@ignorable))))
                  ,@(cl:if specials `((declare (special ,@specials))))
                  ,@decls
                  ,@(cl:if rebinds
                           `((cl:let ,(mapcar (lambda (r) (list (car r) (cdr r))) rebinds)
                               ,@forms))
                           forms)))))))

;;;; DEFUN

;;; Forms handled:
;;;   (DEFUN name lambda-list . body)
;;;   (DEFUN (name indicator) lambda-list . body)   ;stored on the plist
;;;   (DEFUN (:PROPERTY name indicator) ...)
;;;   (DEFUN name FEXPR (arg) . body)                ;Maclisp-style
;;;   (DEFUN name MACRO (form) . body)
;;;   (DEFUN (name MACRO) (form) . body)
;;; A MACRO definition gets the whole form, as in Maclisp.

(cl:defun %zl-function-spec (spec)
  "Returns (values name indicator) for a function spec."
  (cond ((symbolp spec) (values spec nil))
        ((eq (car spec) :property) (values (cadr spec) (caddr spec)))
        ((eq (car spec) :method) (values spec :method))
        (t (values (car spec) (cadr spec)))))

(cl:defmacro defun (spec &rest rest)
  (multiple-value-bind (name indicator) (%zl-function-spec spec)
    (cl:let ((kind nil) (ll (car rest)) (body (cdr rest)))
      (when (and (symbolp ll) ll (cl:member ll '(macro fexpr expr lexpr)))
        (setq kind ll ll (cadr rest) body (cddr rest)))
      (when (eq indicator 'macro) (setq kind 'macro indicator nil))
      (cond
        ((eq kind 'macro)
         (cl:let ((form (car ll)))
           (multiple-value-bind (cll cbody) (%zl-lambda-parts (list form) body)
             (declare (ignore cll))
             `(cl:defmacro ,name (&whole ,form &rest ,(cl:gensym "IGNORE"))
                ,@cbody))))
        ((eq kind 'fexpr)
         ;; Arguments arrive unevaluated, as a list: the body becomes an
         ;; ordinary function of that list, called by a macro.
         (cl:let ((impl (%zl-quoted-impl-name name))
                  (args (cl:gensym "ARGS")))
           (multiple-value-bind (cll cbody) (%zl-lambda-parts ll body)
             `(progn (cl:defun ,impl ,cll ,@cbody)
                     (cl:defmacro ,name (&rest ,args)
                       (list ',impl (list 'quote ,args)))
                     ',name))))
        ((and (null indicator) (consp ll) (memq '&quote ll))
         (%zl-quote-arg-defun name ll body))
        (indicator
         (multiple-value-bind (cll cbody) (%zl-lambda-parts ll body)
           `(progn (setf (cl:get ',name ',indicator)
                         (named-lambda (,name ,indicator) ,cll ,@cbody))
                   ',spec)))
        (t
         (multiple-value-bind (cll cbody)
             (%zl-lambda-parts ll body :doc-allowed t)
           `(cl:defun ,name ,cll ,@cbody)))))))

;;; &QUOTE in a lambda list: the arguments that follow arrive unevaluated
;;; (until &EVAL). (defun plan-def (&quote &rest body) ...) in db.lisp.
;;; The body becomes an ordinary function; NAME becomes a macro that
;;; quotes the &QUOTE arguments and calls it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun %zl-quoted-impl-name (name)
    (cl:intern (concatenate 'string (symbol-name name) " (quoted args)")
               (symbol-package name)))

  (cl:defun %zl-quote-arg-defun (name ll body)
    (cl:let ((modes '()) (rest-mode nil) (quote? nil) (in-rest nil) (clean '()))
      (dolist (x ll)
        (cond ((eq x '&quote) (setq quote? t))
              ((eq x '&eval) (setq quote? nil))
              ((cl:member x '(&rest &body)) (setq in-rest t) (push '&rest clean))
              ((cl:member x lambda-list-keywords) (push x clean))
              (t (cl:if in-rest (setq rest-mode quote?) (push quote? modes))
                 (push x clean))))
      (cl:let ((impl (%zl-quoted-impl-name name))
               (modes (nreverse modes))
               (args (cl:gensym "ARGS")))
        `(progn
           (defun ,impl ,(nreverse clean) ,@body)
           (cl:defmacro ,name (&rest ,args)
             (cons ',impl
                   (loop for a in ,args
                         for i from 0
                         collect (cl:if (cl:if (cl:< i ,(length modes))
                                               (nth i ',modes)
                                               ,rest-mode)
                                        (list 'quote a)
                                        a))))
           ',name)))))

(cl:defmacro named-lambda (name ll &body body)
  (declare (ignore name))
  `(lambda ,ll ,@body))

(cl:defmacro defmacro (name ll &body body)
  (cond ((consp name)                   ;(defmacro (name indicator) ...)
         (cl:let ((form (cl:gensym "FORM")))
           `(defun ,name macro (,form)
              (destructuring-bind ,ll (cdr ,form) ,@body))))
        (t
         (multiple-value-bind (cll cbody)
             (%zl-lambda-parts ll body :macro? t :doc-allowed t)
           `(cl:defmacro ,name ,cll ,@cbody)))))

(cl:defmacro defsubst (name ll &body body)
  `(progn (declaim (inline ,name))
          (defun ,name ,ll ,@body)))

(cl:defmacro defconst (name value &optional doc)
  `(defparameter ,name ,value ,@(cl:if doc (list doc))))

(cl:defmacro local-declare (declarations &body body)
  ;; Only SPECIAL matters to us; the rest are compiler advice.
  (cl:let ((specials (loop for d in declarations
                           when (eq (car d) 'special) append (cdr d))))
    (cl:if specials
        `(progn (declaim (special ,@specials)) ,@body)
        `(progn ,@body))))

(cl:defmacro declare-flavor-instance-variables (flavor &body body)
  (declare (ignore flavor))
  `(progn ,@body))

(cl:defmacro displace (form expansion)
  (declare (ignore form))
  expansion)
(cl:defun displace (form expansion) (declare (ignore form)) expansion)

(cl:defmacro comment (&rest ignore) (declare (ignore ignore)) ''comment)

(cl:defun record-source-file-name (&rest ignore)
  (declare (ignore ignore)) nil)

(cl:defmacro compile-flavor-methods (&rest ignore)
  (declare (ignore ignore)) nil)

(cl:defmacro eval-when-zl (situations &body body)
  (declare (ignore situations))
  `(progn ,@body))

;;;; Standard stream and printer variables
;;;
;;; Zetalisp names them without stars. They are symbol macros onto the
;;; CL variables, and LET/LET* below rebind the CL variable when one of
;;; these names is bound, so (LET ((STANDARD-OUTPUT x)) ...) works.

(defparameter %zl-variable-aliases
  '((standard-output . *standard-output*)
    (standard-input  . *standard-input*)
    (terminal-io     . *terminal-io*)
    (error-output    . *error-output*)
    (trace-output    . *trace-output*)
    (query-io        . *query-io*)
    (debug-io        . *debug-io*)
    (base            . *print-base*)
    (ibase           . *read-base*)
    (package         . *package*)
    (readtable       . *readtable*)
    (prinlevel       . *print-level*)
    (prinlength      . *print-length*)))

(dolist (a %zl-variable-aliases)
  (eval `(define-symbol-macro ,(car a) ,(cdr a))))

;;; Zetalisp printer variables without CL equivalents.
(defvar *nopoint nil "Zetalisp: if true, print integers without a trailing point.")
(defvar prinendline nil)
(defvar user-id "KBE")

(cl:defun %zl-alias (var)
  (cl:let ((a (cl:assoc var %zl-variable-aliases)))
    (cl:if a (cdr a) var)))

;;;; LET and LET* (destructuring, as LISPM2;LET provided for KBE)
;;;
;;;  (LET (((A B . C) list) ((NIL X) pair) (Y 3) Z) ...)
;;; A pattern is matched leniently: missing elements give NIL, extra
;;; elements are ignored, and NIL in a pattern ignores that position.

(cl:defun %zl-destructure (pattern value-var)
  "Returns a list of (var form) bindings, in order, binding PATTERN to the
   value in VALUE-VAR."
  (cl:let ((bindings '()))
    (labels ((walk (pat place)
               (cond ((null pat))
                     ((symbolp pat) (push (list (%zl-alias pat) place) bindings))
                     (t
                      (cl:let ((g (cl:gensym "D")))
                        (push (list g place) bindings)
                        (walk (car pat) `(car ,g))
                        (walk (cdr pat) `(cdr ,g)))))))
      (walk pattern value-var))
    (nreverse bindings)))

(cl:defun %zl-binding-parts (binding)
  "Returns (values pattern init-form)."
  (cond ((atom binding) (values binding nil))
        (t (values (car binding) (cadr binding)))))

(cl:defmacro let (bindings &body body)
  (cl:if (every* (lambda (b) (atom (%zl-binding-parts b))) bindings)
      `(cl:let ,(mapcar (lambda (b)
                          (multiple-value-bind (pat init) (%zl-binding-parts b)
                            (list (%zl-alias pat) init)))
                        bindings)
         ,@body)
      ;; Evaluate all init forms first (parallel binding), then destructure.
      (cl:let ((temps '()) (inner '()))
        (dolist (b bindings)
          (multiple-value-bind (pat init) (%zl-binding-parts b)
            (cond ((atom pat) (cl:let ((g (cl:gensym "V")))
                                (push (list g init) temps)
                                (cl:when pat
                                  (push (list (%zl-alias pat) g) inner))))
                  (t (cl:let ((g (cl:gensym "V")))
                       (push (list g init) temps)
                       (setq inner (revappend (%zl-destructure pat g) inner)))))))
        (multiple-value-bind (decls doc forms) (%zl-split-body body)
          (declare (ignore doc))
          `(cl:let ,(nreverse temps)
             (cl:let* ,(nreverse inner)
               ,@decls
               ,@forms))))))

(cl:defmacro let* (bindings &body body)
  (cl:if (every* (lambda (b) (atom (%zl-binding-parts b))) bindings)
      `(cl:let* ,(mapcar (lambda (b)
                           (multiple-value-bind (pat init) (%zl-binding-parts b)
                             (list (%zl-alias pat) init)))
                         bindings)
         ,@body)
      (cl:let ((out '()))
        (dolist (b bindings)
          (multiple-value-bind (pat init) (%zl-binding-parts b)
            (cond ((atom pat) (push (list (%zl-alias pat) init) out))
                  (t (cl:let ((g (cl:gensym "V")))
                       (push (list g init) out)
                       (setq out (revappend (%zl-destructure pat g) out)))))))
        (multiple-value-bind (decls doc forms) (%zl-split-body body)
          (declare (ignore doc))
          `(cl:let* ,(nreverse out) ,@decls ,@forms)))))

(cl:defun every* (pred list) (cl:every pred list))

(cl:defmacro progw (vars-and-vals &body body)
  "Zetalisp PROGW: bind variables named in a list of (var value-form) pairs."
  (cl:let ((pairs (cl:gensym)))
    `(cl:let ((,pairs ,vars-and-vals))
       (progv (mapcar (lambda (p) (%zl-alias (car p))) ,pairs)
              (mapcar (lambda (p) (eval (cadr p))) ,pairs)
         ,@body))))

;;; (DESETQ pattern value ...): destructuring SETQ, with LET's pattern rules.
(cl:defmacro desetq (&rest pairs)
  (cl:let ((forms '()))
    (loop for (pattern value) on pairs by #'cddr
          do (cl:if (atom pattern)
                    (push `(setq ,pattern ,value) forms)
                    (cl:let ((g (cl:gensym "V")) (sets '()))
                      (labels ((walk (pat place)
                                 (cond ((null pat))
                                       ((symbolp pat) (push `(setq ,pat ,place) sets))
                                       (t (walk (car pat) `(car ,place))
                                          (walk (cdr pat) `(cdr ,place))))))
                        (walk pattern g))
                      (push `(cl:let ((,g ,value)) ,@(nreverse sets) ,g) forms))))
    `(progn ,@(nreverse forms))))

;;;; DO (old Maclisp form and new form), IF, RETURN

(cl:defmacro do (&rest args)
  (cond ((and (car args) (symbolp (car args)))
         ;; (DO var init step end-test . body)
         (destructuring-bind (var init step test &rest body) args
           `(cl:do ((,var ,init ,step)) (,test) ,@body)))
        (t
         (destructuring-bind (vars end &rest body) args
           (cl:if (null end)
               ;; (DO (...) () body) loops once in Maclisp/Zetalisp.
               `(cl:do ,vars (nil) ,@body (cl:return nil))
               `(cl:do ,vars ,end ,@body))))))

(cl:defmacro do* (vars end &body body)
  `(cl:do* ,vars ,end ,@body))

(cl:defmacro do-named (name vars end &body body)
  `(cl:block ,name (do ,vars ,end ,@body)))

(cl:defmacro if (test then &rest else)
  `(cl:if ,test ,then ,@(cl:if (cdr else) `((progn ,@else)) else)))

(cl:defmacro return (&rest values)
  `(cl:return ,(cl:if (cdr values) `(values ,@values) (car values))))

;;; (PROG name vars . body): Zetalisp allows a name, giving a block that
;;; RETURN-FROM can exit (implement.lisp: (prog outer () ...)).
(cl:defmacro prog (&rest args)
  (cl:if (and (car args) (symbolp (car args)))
         `(cl:block ,(car args) (cl:prog ,@(cdr args)))
         `(cl:prog ,@args)))
(cl:defmacro prog* (&rest args)
  (cl:if (and (car args) (symbolp (car args)))
         `(cl:block ,(car args) (cl:prog* ,@(cdr args)))
         `(cl:prog* ,@args)))

(cl:defmacro return-list (list) `(cl:return (values-list ,list)))

(cl:defmacro caseq (key &body clauses) `(case ,key ,@clauses))
(cl:defmacro selectq (key &body clauses) `(case ,key ,@clauses))

(cl:defmacro select (key &body clauses)
  (cl:let ((k (cl:gensym)))
    `(cl:let ((,k ,key))
       (cond ,@(mapcar (lambda (c)
                         (cl:if (cl:member (car c) '(t otherwise))
                             `(t ,@(cdr c))
                             `((eql ,k ,(car c)) ,@(cdr c))))
                       clauses)))))

(cl:defmacro *catch (tag &body body) `(cl:catch ,tag ,@body))
(cl:defmacro *throw (tag value) `(cl:throw ,tag ,value))

(cl:defun lexpr-funcall (fn &rest args) (apply #'apply fn args))
(define-compiler-macro lexpr-funcall (fn &rest args) `(apply ,fn ,@args))

(cl:defmacro without-interrupts (&body body) `(progn ,@body))

(cl:defmacro check-arg (var pred description)
  (cl:let ((test (cl:if (symbolp pred) `(,pred ,var) pred)))
    `(unless ,test
       (ferror "The argument ~S was ~S, which is not ~A." ',var ,var
               ,description))))

(cl:defmacro check-arg-type (var type &optional description)
  `(check-arg ,var (typep ,var ',type) ,(or description (string type))))

(cl:defmacro once-only (vars &body body)
  "Zetalisp ONCE-ONLY for macro writers."
  (cl:let ((gensyms (mapcar (lambda (v) (declare (ignore v)) (cl:gensym))
                            vars)))
    `(cl:let ,(mapcar (lambda (g) `(,g (cl:gensym))) gensyms)
       (list 'cl:let
             (list ,@(mapcar (lambda (g v) `(list ,g ,v)) gensyms vars))
             (cl:let ,(mapcar (lambda (v g) `(,v ,g)) vars gensyms)
               ,@body)))))

(cl:defmacro <- (&rest pairs) `(setf ,@pairs))

;;;; Lists

(cl:defun memq (x list) (cl:member x list :test #'eq))
(cl:defun assq (x alist) (cl:assoc x alist :test #'eq))
(cl:defun rassq (x alist) (cl:rassoc x alist :test #'eq))
(cl:defun member (x list) (cl:member x list :test #'equal))
(cl:defun assoc (x alist) (cl:assoc x alist :test #'equal))
(cl:defun rassoc (x alist) (cl:rassoc x alist :test #'equal))
(cl:defun mem (pred x list) (cl:member x list :test pred))
(cl:defun ass (pred x alist) (cl:assoc x alist :test pred))
(cl:defun delq (x list &optional count)
  (cl:delete x list :test #'eq :count count))
(cl:defun remq (x list &optional count)
  (cl:remove x list :test #'eq :count count))
(cl:defun delete (x list &optional count)
  (cl:delete x list :test #'equal :count count))
(cl:defun remove (x list &optional count)
  (cl:remove x list :test #'equal :count count))
(cl:defun del (pred x list &optional count)
  (cl:delete x list :test pred :count count))
(cl:defun rem (pred x list &optional count)
  (cl:remove x list :test pred :count count))
(cl:defun rem-if (pred list) (cl:remove-if pred list))
(cl:defun rem-if-not (pred list) (cl:remove-if-not pred list))
(cl:defun del-if (pred list) (cl:delete-if pred list))
(cl:defun del-if-not (pred list) (cl:delete-if-not pred list))
(cl:defun subset (pred list) (cl:remove-if-not pred list))
(cl:defun subset-not (pred list) (cl:remove-if pred list))
(cl:defun subst (new old tree) (cl:subst new old tree :test #'equal))
(cl:defun rest1 (l) (cdr l))
(cl:defun rest2 (l) (cddr l))
(cl:defun rest3 (l) (cdddr l))
(cl:defun rest4 (l) (cddddr l))
(cl:defun ncons (x) (list x))
(cl:defun xcons (a b) (cons b a))
(cl:defun copylist (list &optional area) (declare (ignore area)) (copy-list list))
(cl:defun copylist* (list) (copy-list list))
(cl:defun copytree (tree) (copy-tree tree))
(cl:defun copyalist (alist) (copy-alist alist))
(cl:defun firstn (n list) (loop repeat n for l on list collect (car l)))
(cl:defun nleft (n list) (last list n))
(cl:defun circular-list (&rest items)
  (cl:let ((l (copy-list items))) (setf (cdr (last l)) l) l))
(cl:defun list*-in-area (area &rest args)
  (declare (ignore area)) (apply #'list* args))
(cl:defun list-in-area (area &rest args) (declare (ignore area)) args)
(cl:defun cons-in-area (a b area) (declare (ignore area)) (cons a b))
(cl:defun make-list (length &rest options)
  ;; Zetalisp: (make-list length &key area initial-value)
  (make-list-1 length (getf options :initial-value (getf options :initial-element))))
(cl:defun make-list-1 (n init) (cl:make-list n :initial-element init))
;;; Maclisp/Zetalisp MAP applies FN to successive tails (CL MAPL).
(cl:defun map (fn list &rest more-lists) (apply #'mapl fn list more-lists) list)
(cl:defun sortcar (list pred) (sort list pred :key #'car))
(cl:defun every (list pred &optional (step #'cdr))
  (cl:do ((l list (funcall step l))) ((null l) t)
    (unless (funcall pred (car l)) (cl:return nil))))
(cl:defun some (list pred &optional (step #'cdr))
  (cl:do ((l list (funcall step l))) ((null l) nil)
    (when (funcall pred (car l)) (cl:return l))))
(cl:defun union (&rest lists)
  (cl:let ((result '()))
    (dolist (l lists (nreverse result))
      (dolist (x l) (unless (memq x result) (push x result))))))
(cl:defun intersection (&rest lists)
  (cl:if (null lists) nil
      (cl:remove-if-not (lambda (x) (cl:every (lambda (l) (memq x l)) (cdr lists)))
                        (car lists))))
;;; In Zetalisp (LISTP NIL) is false: LISTP means CONSP. KBE depends on
;;; it, e.g. FIX-UP-SEMI-KBE-/#-COMMENT-MACROS stops on (NOT (LISTP X)).
(declaim (inline listp))
(cl:defun listp (x) (consp x))
(cl:defun neq (a b) (not (eq a b)))
;;; Release 4.5 characters were fixnums, and KBE relies on it:
;;; pseudo.lisp's IN-CHARS? recognizes a character with (FIXP thing). We
;;; read characters as CL characters, so FIXP and FIXNUMP accept them.
(cl:defun fixp (x) (or (integerp x) (characterp x)))
(cl:defun fixnump (x) (or (typep x 'fixnum) (characterp x)))
(cl:defun bigp (x) (and (integerp x) (not (typep x 'fixnum))))
(cl:defun flonump (x) (floatp x))
(cl:defun small-floatp (x) (floatp x))
(cl:defun nlistp (x) (atom x))
(cl:defun nsymbolp (x) (not (symbolp x)))

;;;; Symbols and property lists
;;;
;;; GET, PUTPROP and REMPROP also accept a locative (from LOCF) or a
;;; disembodied property list (a cons whose cdr is a property list).

(cl:defstruct (locative (:constructor make-locative (getter setter)))
  getter setter)

(cl:defmacro locf (place)
  (cl:let ((v (cl:gensym)))
    `(make-locative (lambda () ,place)
                    (lambda (,v) (setf ,place ,v)))))

(cl:defun location-contents (loc) (funcall (locative-getter loc)))
(cl:defun car-location (cons) (locf (car cons)))

(cl:defun %zl-plist-get (object)
  (cond ((locative-p object) (location-contents object))
        (t (cdr object))))

(cl:defun %zl-plist-set (object new)
  (cond ((locative-p object) (funcall (locative-setter object) new))
        (t (setf (cdr object) new))))

(cl:defun get (object indicator &optional default)
  (cond ((symbolp object) (cl:get object indicator default))
        ;; Lisp Machine GET scans indicator/value pairs leniently: pseudo.lisp
        ;; asks (GET '(CODE "1.") 'PRONOUN) of any token, expecting NIL.
        (t (loop for l on (%zl-plist-get object) by #'cddr
                 when (eq (car l) indicator) do (cl:return (cadr l))
                 finally (cl:return default)))))

(cl:defun (cl:setf get) (value object indicator &optional default)
  (declare (ignore default))
  (cond ((symbolp object) (setf (cl:get object indicator) value))
        (t (cl:let ((pl (%zl-plist-get object)))
             (setf (getf pl indicator) value)
             (%zl-plist-set object pl)
             value))))

(cl:defun putprop (object value indicator)
  (setf (get object indicator) value))

(cl:defmacro defprop (symbol value indicator)
  `(putprop ',symbol ',value ',indicator))

(cl:defun remprop (object indicator)
  (cond ((symbolp object) (cl:remprop object indicator))
        (t (cl:let ((pl (%zl-plist-get object)))
             (prog1 (remf pl indicator) (%zl-plist-set object pl))))))

(cl:defun getl (object indicators)
  (cl:let ((pl (cl:if (symbolp object) (symbol-plist object)
                      (%zl-plist-get object))))
    (loop for l on pl by #'cddr
          when (memq (car l) indicators) do (cl:return l))))

(cl:defun plist (symbol) (symbol-plist symbol))
(cl:defun setplist (symbol plist) (setf (symbol-plist symbol) plist))
(cl:defun symeval (symbol) (symbol-value symbol))
;;; A Lisp Machine macro's function cell holds (MACRO . expander).
(cl:defun fsymeval (symbol)
  (cl:if (macro-function symbol)
         (cons 'macro (macro-function symbol))
         (fdefinition symbol)))
(cl:defun fset (symbol definition)
  (cl:if (and (consp definition) (eq (car definition) 'macro))
         (setf (macro-function symbol) (cdr definition))
         (setf (fdefinition symbol) definition)))
(cl:defun fdefine (symbol definition &rest ignore)
  (declare (ignore ignore))
  (setf (fdefinition symbol) definition))
(cl:defun get-pname (symbol) (symbol-name symbol))
(cl:defun samepnamep (a b) (string= (string a) (string b)))
(cl:defun alphalessp (a b)
  (string< (cl:if (symbolp a) (symbol-name a) (princ-to-string a))
           (cl:if (symbolp b) (symbol-name b) (princ-to-string b))))

(cl:defun %zl-package (pkg)
  (cond ((null pkg) *package*)
        ;; KBE interns into the package of symbols it reads, as in
        ;; (intern "~KEYWORD" (symbol-package 'keyword)). On the Lisp
        ;; Machine those were global symbols; here they belong to the locked
        ;; COMMON-LISP package, so such symbols go to KBE instead.
        ((and (packagep pkg) (sb-ext:package-locked-p pkg))
         (or (find-package "KBE") *package*))
        ((packagep pkg) pkg)
        ((and (stringp pkg) (string= pkg "")) (find-package "KEYWORD"))
        (t (or (find-package pkg)
               (cl:error "No package named ~S" pkg)))))

(cl:defun intern (string &optional pkg)
  (multiple-value-bind (sym status) (cl:intern (string string) (%zl-package pkg))
    (%zl-note-symbol sym)
    (values sym status)))
(cl:defun intern-soft (string &optional pkg)
  (find-symbol (string string) (%zl-package pkg)))
(cl:defun intern-local (string &optional pkg) (intern string pkg))
(cl:defun intern-local-soft (string &optional pkg)
  (intern-soft string pkg))
(cl:defun make-symbol (string &optional permanent?)
  (declare (ignore permanent?))
  (%zl-note-symbol (cl:make-symbol (string string))))
(cl:defun copysymbol (symbol copy-props?)
  (%zl-note-symbol (copy-symbol symbol copy-props?)))
(cl:defun gensym (&optional prefix)
  (%zl-note-symbol
   (cond ((null prefix) (cl:gensym))
         ((integerp prefix) (cl:gensym prefix))
         (t (cl:gensym (string prefix))))))
(cl:defun pkg-goto (&optional (pkg "USER"))
  (setq *package* (%zl-package pkg)))
(cl:defun pkg-find-package (name &rest ignore)
  (declare (ignore ignore))
  (find-package name))
(cl:defun globalize (&rest ignore) (declare (ignore ignore)) nil)

;;;; %POINTER
;;;
;;; KBE sorts "fsets" by object address (sets.lisp), which on the 3600
;;; with the garbage collector off follows allocation order. SBCL moves
;;; objects, so instead we give each object a serial number: symbols made
;;; with MAKE-SYMBOL, GENSYM, COPYSYMBOL or INTERN (the KBE creates its
;;; plan names this way) get one when they're created; anything else gets
;;; one the first time %POINTER sees it.

(defvar %zl-serials (make-hash-table :test #'eq :weakness :key))
(defvar %zl-next-serial 0)

(cl:defun %zl-note-symbol (sym)
  (unless (gethash sym %zl-serials)
    (setf (gethash sym %zl-serials) (incf %zl-next-serial)))
  sym)

(cl:defun %pointer (object)
  (cond ((typep object 'fixnum) object)
        ((characterp object) (char-code object))
        (t (or (gethash object %zl-serials)
               (setf (gethash object %zl-serials) (incf %zl-next-serial))))))

(cl:defun maknum (object) (%pointer object))

;;;; DEFSTRUCT (Lisp Machine style)
;;;
;;; The constructor is a macro taking slot names and values:
;;;   (make-dflow-hunk start x end nil)
;;; and, unless :CONC-NAME says otherwise, accessors are named by the slot
;;; names alone: (end hunk), not (dflow-hunk-end hunk).
;;; Options :TYPE :NAMED-LIST, :LIST, :ARRAY, :NAMED-ARRAY are mapped to
;;; CL equivalents; other Lisp Machine options are ignored.

(cl:defmacro defstruct (name-and-options &rest slots)
  (cl:let* ((name (cl:if (consp name-and-options) (car name-and-options)
                         name-and-options))
            (options (cl:if (consp name-and-options) (cdr name-and-options)))
            (cl-options '())
            (constructor (cl:intern (cl:format nil "MAKE-~A" name)
                                    (symbol-package name)))
            (internal (cl:intern (cl:format nil "%MAKE-~A" name)
                                 (symbol-package name))))
    (dolist (o options)
      (cl:let ((key (cl:if (consp o) (car o) o)) (arg (cl:if (consp o) (cadr o))))
        (case key
          (:type (case arg
                   (:named-list (push '(:type list) cl-options) (push :named cl-options))
                   (:list (push '(:type list) cl-options))
                   (:named-array (push '(:type vector) cl-options) (push :named cl-options))
                   (:array (push '(:type vector) cl-options))
                   (t nil)))
          (:constructor (cl:when arg (setq constructor arg)))
          ((:conc-name :include :copier :predicate) (push o cl-options))
          (t nil))))
    (unless (cl:find :conc-name options :key (lambda (o) (cl:if (consp o) (car o) o)))
      (push '(:conc-name nil) cl-options))
    (cl:let ((slot-names (mapcar (lambda (s) (cl:if (consp s) (car s) s)) slots)))
      `(progn
         (cl:defstruct (,name (:constructor ,internal (&key ,@slot-names))
                              ,@(nreverse cl-options))
           ,@slots)
         (cl:defmacro ,constructor (&rest slot-value-pairs)
           (cons ',internal
                 (loop for (slot value) on slot-value-pairs by #'cddr
                       collect (cl:intern (symbol-name slot) "KEYWORD")
                       collect value)))
         ',name))))

;;;; Strings and characters

(cl:defun string-length (x) (length (string x)))
(cl:defun substring (string start &optional end)
  (subseq (string string) start end))
(cl:defun nsubstring (string start &optional end)
  (subseq (string string) start end))
(cl:defun string-append (&rest things)
  (apply #'concatenate 'string
         (mapcar (lambda (x) (cl:if (integerp x) (string (code-char x)) (string x)))
                 things)))
(cl:defun string-equal (a b &optional (start1 0) (start2 0) end1 end2)
  (cl:string-equal (string a) (string b) :start1 start1 :start2 start2
                   :end1 end1 :end2 end2))
(cl:defun %string-equal (a start1 b start2 count)
  (cl:let ((a (string a)) (b (string b)))
    (cl:if (null count)
        (cl:string-equal a b :start1 start1 :start2 start2)
        (and (cl:<= (cl:+ start1 count) (length a))
             (cl:<= (cl:+ start2 count) (length b))
             (cl:string-equal a b :start1 start1 :end1 (cl:+ start1 count)
                              :start2 start2 :end2 (cl:+ start2 count))))))
(cl:defun %zl-char (x)
  (cond ((characterp x) x)
        ((integerp x) (code-char x))
        (t (char (string x) 0))))
(cl:defun character (x) (%zl-char x))
(cl:defun string-search (key string &optional (from 0) to)
  (search (string key) (string string) :start2 from :end2 to))
(cl:defun string-search-char (char string &optional (from 0) to)
  (position (%zl-char char) (string string) :start from :end to))
(cl:defun string-search-not-char (char string &optional (from 0) to)
  (position (%zl-char char) (string string) :start from :end to :test-not #'eql))
(cl:defun string-search-set (set string &optional (from 0) to)
  (cl:let ((set (cl:if (stringp set) (coerce set 'list) (mapcar #'%zl-char set))))
    (position-if (lambda (c) (cl:member c set)) (string string) :start from :end to)))
(cl:defun string-search-not-set (set string &optional (from 0) to)
  (cl:let ((set (cl:if (stringp set) (coerce set 'list) (mapcar #'%zl-char set))))
    (position-if-not (lambda (c) (cl:member c set)) (string string) :start from :end to)))
(cl:defun string-reverse-search (key string &optional from)
  (search (string key) (string string) :from-end t :end2 from))
(cl:defun string-reverse-search-char (char string &optional from)
  (position (%zl-char char) (string string) :from-end t :end from))
(cl:defun string-reverse-search-set (set string &optional from)
  (cl:let ((set (cl:if (stringp set) (coerce set 'list) (mapcar #'%zl-char set))))
    (position-if (lambda (c) (cl:member c set)) (string string) :from-end t :end from)))
(cl:defun string-reverse-search-not-set (set string &optional from)
  (cl:let ((set (cl:if (stringp set) (coerce set 'list) (mapcar #'%zl-char set))))
    (position-if-not (lambda (c) (cl:member c set)) (string string)
                     :from-end t :end from)))
(cl:defun string-nreverse (s) (nreverse s))
(cl:defun string-reverse (s) (reverse (string s)))
(cl:defun string-capitalize-words (x)
  (substitute #\Space #\- (cl:string-capitalize (string x))))
(cl:defun string-pluralize (x)
  (cl:let* ((s (string x)) (n (length s)))
    (cond ((zerop n) s)
          ((and (cl:> n 1) (char-equal (char s (1- n)) #\y)
                (not (find (char s (cl:- n 2)) "aeiouAEIOU")))
           (concatenate 'string (subseq s 0 (1- n))
                        (cl:if (upper-case-p (char s (1- n))) "IES" "ies")))
          ((find (char s (1- n)) "sxzSXZ")
           (concatenate 'string s (cl:if (upper-case-p (char s (1- n))) "ES" "es")))
          ((and (cl:> n 1) (char-equal (char s (1- n)) #\h)
                (find (char s (cl:- n 2)) "csCS"))
           (concatenate 'string s (cl:if (upper-case-p (char s (1- n))) "ES" "es")))
          (t (concatenate 'string s (cl:if (upper-case-p (char s (1- n))) "S" "s"))))))
(cl:defun string-search-all (key string)
  (loop with key = (string key) and string = (string string)
        for p = (search key string) then (search key string :start2 (1+ p))
        while p collect p))

;;;; Arithmetic

;;; Zetalisp characters are fixnums: (>= c #/a), (= c -1). We read #/a as
;;; a CL character, so the comparisons accept characters as their codes.
(declaim (inline %zl-num))
(cl:defun %zl-num (x) (cl:if (characterp x) (char-code x) x))
(macrolet ((def (name cl-name)
             `(progn
                (cl:defun ,name (a &rest more)
                  (cl:if (and (numberp a) more (null (cdr more)) (numberp (car more)))
                      (,cl-name a (car more))
                      (apply #',cl-name (%zl-num a) (mapcar #'%zl-num more))))
                (define-compiler-macro ,name (&whole form a &rest more)
                  (cl:if (and (cl:every #'numberp (cons a more)))
                      `(,',cl-name ,a ,@more)
                      form)))))
  (def = cl:=) (def /= cl:/=) (def < cl:<) (def > cl:>)
  (def <= cl:<=) (def >= cl:>=))

;;; Numeric predicates on characters. The tokenizer tests TYI's result
;;; for end of file with (MINUSP c) (pseudo.lisp REALLY-SCAN-WORD), more
;;; evidence that Release 4.5 characters were fixnums.
(cl:defun minusp (x) (cl:minusp (%zl-num x)))
(cl:defun plusp (x) (cl:plusp (%zl-num x)))
(cl:defun zerop (x) (cl:zerop (%zl-num x)))
(cl:defun max (x &rest more) (apply #'cl:max (%zl-num x) (mapcar #'%zl-num more)))
(cl:defun min (x &rest more) (apply #'cl:min (%zl-num x) (mapcar #'%zl-num more)))

;;; Arithmetic on characters, as in #.(1- #/a), gives their codes.
(cl:defun + (&rest args)
  (cl:if (and args (cdr args) (null (cddr args)) (numberp (car args)) (numberp (cadr args)))
      (cl:+ (car args) (cadr args))
      (apply #'cl:+ (mapcar #'%zl-num args))))
(cl:defun - (a &rest args)
  (cl:if (and (numberp a) args (null (cdr args)) (numberp (car args)))
      (cl:- a (car args))
      (apply #'cl:- (%zl-num a) (mapcar #'%zl-num args))))
(cl:defun 1+ (x) (cl:1+ (%zl-num x)))
(cl:defun 1- (x) (cl:1- (%zl-num x)))
(define-compiler-macro 1+ (&whole form x) (cl:if (numberp x) `(cl:1+ ,x) form))
(define-compiler-macro 1- (&whole form x) (cl:if (numberp x) `(cl:1- ,x) form))

(cl:defun / (&rest args)
  "Zetalisp //: integer division truncates."
  (cond ((null args) 1)
        ((cl:every #'integerp args)
         (cl:if (null (cdr args)) (truncate 1 (car args))
                (cl:let ((r (car args)))
                  (dolist (d (cdr args) r) (setq r (truncate r d))))))
        (t (apply #'cl:/ args))))
;;; Maclisp/Zetalisp BOOLE takes a PDP-10 Boolean-instruction code, not
;;; a CL BOOLE-... constant: (boole 4 x y) is ANDCM, x and (not y), which
;;; dflow.lisp's 6SET- uses for set difference. (In CL, 4 is BOOLE-C1.)
;;; More than two arguments combine left to right.
(defparameter %zl-boole-ops
  (vector boole-clr boole-and boole-andc1 boole-2 boole-andc2 boole-1
          boole-xor boole-ior boole-nor boole-eqv boole-c1 boole-orc1
          boole-c2 boole-orc2 boole-nand boole-set))
(cl:defun boole (code x &rest more)
  (cl:let ((op (svref %zl-boole-ops code)) (result x))
    (dolist (y more result) (setq result (cl:boole op result y)))))
(cl:defun \\ (a b) (cl:rem a b))
(cl:defun remainder (a b) (cl:rem a b))
(cl:defun quotient (&rest args) (apply #'/ args))
(cl:defun plus (&rest args) (apply #'cl:+ args))
(cl:defun difference (&rest args) (apply #'cl:- args))
(cl:defun times (&rest args) (apply #'cl:* args))
(cl:defun greaterp (&rest args) (apply #'cl:> args))
(cl:defun lessp (&rest args) (apply #'cl:< args))
(cl:defun add1 (x) (1+ x))
(cl:defun sub1 (x) (1- x))
(cl:defun minus (x) (cl:- x))
(cl:defun fix (x) (cl:if (integerp x) x (values (floor x))))
(cl:defun ^ (a b) (expt a b))
(cl:defun lsh (n count) (ash n count))
(cl:defun +$ (&rest args) (apply #'cl:+ args))
(cl:defun -$ (&rest args) (apply #'cl:- args))
(cl:defun *$ (&rest args) (apply #'cl:* args))
(cl:defun //$ (&rest args) (apply #'cl:/ args))

;;;; Reading and printing

(cl:defun read (&optional (stream *standard-input*) (eof nil eof-p))
  (cl:read (cl:if (eq stream t) *standard-input* stream) (not eof-p) eof))

(cl:defun %zl-stream (stream)
  (cond ((eq stream t) *standard-output*)
        ((null stream) *standard-output*)
        (t stream)))
(cl:defun tyo (char &optional (stream *standard-output*))
  (write-char (%zl-char char) (%zl-stream stream)) char)
(cl:defun tyi (&optional (stream *standard-input*) (eof nil eof-p))
  (cl:let ((c (read-char (cl:if (eq stream t) *standard-input* stream) nil nil)))
    (cond (c c)
          (eof-p eof)
          (t (cl:error 'end-of-file :stream stream)))))
(cl:defun untyi (char &optional (stream *standard-input*))
  (unread-char char (cl:if (eq stream t) *standard-input* stream)))
;;; (TYIPEEK peek-type stream eof): peek-type NIL peeks at the next
;;; character; T first skips whitespace; a character skips up to it.
;;; At end of file it returns EOF, by default -1 as TYI does.
(cl:defun tyipeek (&optional peek-type (stream *standard-input*) (eof -1))
  (cl:let ((stream (cl:if (eq stream t) *standard-input* stream)))
    (cl:or (cond ((null peek-type) (peek-char nil stream nil nil))
                 ((eq peek-type t) (peek-char t stream nil nil))
                 (t (peek-char (%zl-char peek-type) stream nil nil)))
           eof)))
(cl:defun readline (&optional (stream *standard-input*) (eof nil eof-p))
  (cl:let ((l (read-line (cl:if (eq stream t) *standard-input* stream) nil nil)))
    (cond (l l) (eof-p eof) (t (cl:error 'end-of-file :stream stream)))))

(cl:defun format (destination control &rest args)
  (apply #'cl:format destination (%zl-format-control control) args))

(cl:defun %zl-format-control (control)
  ;; Zetalisp directives with no CL equivalent:
  ;;   ~Q  call the argument (a function of a stream)
  ;;   ~:= print the argument as code (we use ~S)
  (cl:if (not (stringp control))
      control
      (cl:let ((out (make-string-output-stream)) (i 0) (n (length control)))
        (loop while (cl:< i n) do
          (cl:let ((c (char control i)))
            (cond ((and (char= c #\~) (cl:< (1+ i) n))
                   ;; copy the directive's prefix parameters and modifiers
                   (cl:let ((j (1+ i)))
                     (loop while (and (cl:< j n)
                                      (or (digit-char-p (char control j))
                                          (find (char control j) ",:@#v'")))
                           do (cl:if (char= (char control j) #\')
                                     (incf j 2)
                                     (incf j)))
                     (cl:let ((d (cl:if (cl:< j n) (char control j) #\~)))
                       (cond ((char-equal d #\Q)
                              (write-string "~/ZL::%ZL-FORMAT-CALL/" out))
                             ((and (char= d #\=) (search ":" control :start2 i :end2 j))
                              (write-string "~S" out))
                             (t (write-string control out :start i :end (1+ j))))
                       (setq i (1+ j)))))
                  (t (write-char c out) (incf i)))))
        (get-output-stream-string out))))

(cl:defun %zl-format-call (stream arg colon at)
  (declare (ignore colon at))
  (funcall arg stream))

;;;; Arrays

;;; Maclisp (*ARRAY name type dims...): NAME NIL means an anonymous array.
;;; Type T arrays start out NIL, numeric ones 0 (coder.lisp 5INIT-ORDER).
;;; Arrays are adjustable, since ADJUST-ARRAY-SIZE grows an array in place
;;; on the Lisp Machine and code holds on to it.
(cl:defun *array (name type &rest dims)
  (cl:let ((array (make-array dims :adjustable t
                              :initial-element (cl:if (cl:member type '(fixnum flonum))
                                                      0 nil))))
    (when name (setf (symbol-function name) array))
    array))

(cl:defun adjust-array-size (array new-size)
  (adjust-array array new-size :initial-element nil))

(cl:defun aset (value array &rest subscripts)
  (setf (apply #'aref array subscripts) value))
(cl:defun array-length (array) (array-total-size array))
(cl:defun array-active-length (array) (length array))
(cl:defun array-push (array x) (vector-push x array))
(cl:defun array-push-extend (array x &optional extension)
  (vector-push-extend x array (or extension 64)))
(cl:defun listarray (array &optional limit)
  (cl:let ((l (coerce array 'list))) (cl:if limit (firstn limit l) l)))
(cl:defun arrayfetch (array &rest subscripts) (apply #'aref array subscripts))

;;;; Hash tables

(cl:defun make-equal-hash-table (&rest options)
  (make-hash-table :test #'equal :size (getf options :size 64)))
(cl:defun puthash (key value table) (setf (gethash key table) value))

;;;; Packages and system information (headless stand-ins)

(cl:defun add-initialization (name form &optional keywords list-name)
  (declare (ignore keywords))
  (when (and list-name (symbolp list-name))
    (unless (boundp list-name) (setf (symbol-value list-name) '()))
    (cl:let ((entry (cl:assoc name (symbol-value list-name) :test #'equal)))
      (cl:if entry
          (setf (cdr entry) (list form))
          (setf (symbol-value list-name)
                (append (symbol-value list-name) (list (list name form)))))))
  name)

(cl:defun delete-initialization (name &optional keywords list-name)
  (declare (ignore keywords))
  (when (and list-name (boundp list-name))
    (setf (symbol-value list-name)
          (cl:remove name (symbol-value list-name) :key #'car :test #'equal)))
  name)

(cl:defun initializations (list-name &optional redo? flag)
  (declare (ignore redo? flag))
  (when (boundp list-name)
    (dolist (entry (symbol-value list-name))
      (eval (cadr entry)))))

(cl:defmacro with-lock ((lock &rest ignore) &body body)
  (declare (ignore lock ignore))
  `(progn ,@body))
;;; One process here. CURRENT-PROCESS stands for it; locks are cells
;;; (locatives, from LOCF) holding the owning process or NIL, as in
;;; dmac.lisp's WITH-DB-LOCKED.
(defvar current-process '%zl-the-process)
(cl:defun process-lock (locative &rest ignore)
  (declare (ignore ignore))
  (funcall (locative-setter locative) current-process))
(cl:defun process-unlock (locative &rest ignore)
  (declare (ignore ignore))
  (funcall (locative-setter locative) nil))
(cl:defun process-wait (&rest ignore) (declare (ignore ignore)) t)
