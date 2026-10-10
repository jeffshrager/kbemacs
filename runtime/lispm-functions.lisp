;;; -*- Mode: LISP; Package: ZL -*-
;;;
;;; Lisp Machine function introspection the KBE analyzer uses
;;; (volume_2/expand.lisp, volume_3/implement.lisp):
;;;
;;;   (ARGLIST fn)                  lambda list of a function or lambda
;;;   (SI:DECLARED-DEFINITION sym)  the function cell: (MACRO . expander)
;;;                                 for a macro
;;;   (SI:OPEN-CODE-P sym)          open-coded (subst) functions: none here
;;;
;;; The analyzer expands macros in the code it analyzes, except those it
;;; handles itself through K*EXPANDER properties (COND, AND, LET, DO, SETF,
;;; LOOP, ...). On the Lisp Machine the remaining Zetalisp macros, such as
;;; IF and WHEN, expanded into forms the analyzer knows (COND, SETQ, DO).
;;; SBCL's expansions of the CL macros use internals (TAGBODY,
;;; SB-IMPL::...) that it does not, so SI:DECLARED-DEFINITION supplies
;;; Lisp Machine–style expansions for those.

(in-package "ZL")

;;;; ARGLIST

(cl:defun arglist (fn &optional real-flag)
  (declare (ignore real-flag))
  (cond ((consp fn)
         (case (car fn)
           ((lambda subst) (cadr fn))
           ((named-lambda named-subst) (caddr fn))
           (t (ferror "~S is not a function." fn))))
        ((and (symbolp fn) (macro-function fn))
         (sb-kernel:%fun-lambda-list (macro-function fn)))
        ((and (symbolp fn) (fboundp fn))
         (sb-kernel:%fun-lambda-list (fdefinition fn)))
        ((functionp fn) (sb-kernel:%fun-lambda-list fn))
        (t (ferror "~S is not a function." fn))))

;;;; Lisp Machine expansions of Zetalisp macros

(defvar %zl-lispm-expansions (make-hash-table :test #'eq)
  "Symbol -> function of a form returning its Lisp Machine expansion.")

(cl:defmacro %zl-def-lispm-expansion (name (form) &body body)
  `(setf (gethash ',name %zl-lispm-expansions)
         (lambda (,form) ,@body)))

;;; (IF test then . else) => (COND (test then) (T . else))
(%zl-def-lispm-expansion if (form)
  (destructuring-bind (test then &rest else) (cdr form)
    (cl:if else
           `(cond (,test ,then) (t ,@else))
           `(cond (,test ,then)))))

(%zl-def-lispm-expansion when (form)
  `(cond (,(cadr form) ,@(cddr form))))

(%zl-def-lispm-expansion unless (form)
  `(cond ((not ,(cadr form)) ,@(cddr form))))

(%zl-def-lispm-expansion push (form)
  (destructuring-bind (item place) (cdr form)
    `(setf ,place (cons ,item ,place))))

(%zl-def-lispm-expansion pop (form)
  (cl:let ((place (cadr form)))
    `(prog1 (car ,place) (setf ,place (cdr ,place)))))

(%zl-def-lispm-expansion incf (form)
  (destructuring-bind (place &optional (delta 1)) (cdr form)
    (cl:if (eql delta 1)
           `(setf ,place (1+ ,place))
           `(setf ,place (+ ,place ,delta)))))

(%zl-def-lispm-expansion decf (form)
  (destructuring-bind (place &optional (delta 1)) (cdr form)
    (cl:if (eql delta 1)
           `(setf ,place (1- ,place))
           `(setf ,place (- ,place ,delta)))))

(%zl-def-lispm-expansion dolist (form)
  (destructuring-bind ((var list &optional result) &rest body) (cdr form)
    (cl:let ((tail (cl:make-symbol ".LIST.")))
      `(do ((,tail ,list (cdr ,tail)) (,var))
           ((null ,tail) ,result)
         (setq ,var (car ,tail))
         ,@body))))

(%zl-def-lispm-expansion dotimes (form)
  (destructuring-bind ((var count &optional result) &rest body) (cdr form)
    (cl:let ((limit (cl:make-symbol ".UPPER.")))
      `(do ((,var 0 (1+ ,var)) (,limit ,count))
           ((>= ,var ,limit) ,result)
         ,@body))))

(cl:defun %zl-caseq-expansion (form)
  (cl:let ((key (cl:make-symbol ".SELECTQ-ITEM.")))
    `(let ((,key ,(cadr form)))
       (cond ,@(mapcar (lambda (clause)
                         (cl:let ((keys (car clause)))
                           (cond ((cl:member keys '(t otherwise))
                                  `(t ,@(cdr clause)))
                                 ((consp keys)
                                  `((memq ,key ',keys) ,@(cdr clause)))
                                 (t `((eq ,key ',keys) ,@(cdr clause))))))
                       (cddr form))))))

(setf (gethash 'caseq %zl-lispm-expansions) #'%zl-caseq-expansion
      (gethash 'selectq %zl-lispm-expansions) #'%zl-caseq-expansion)

;;;; SI:DECLARED-DEFINITION, SI:OPEN-CODE-P

(defvar %zl-warned-macros '())

(cl:defun si::declared-definition (symbol)
  (cl:let ((lispm (gethash symbol %zl-lispm-expansions)))
    (cond (lispm (cons 'macro lispm))
          ((not (macro-function symbol))
           (cl:if (fboundp symbol) (fdefinition symbol) nil))
          (t
           (when (and (eq (symbol-package symbol) (find-package "COMMON-LISP"))
                      (not (memq symbol %zl-warned-macros)))
             (push symbol %zl-warned-macros)
             (warn "Analyzing a call to the Common Lisp macro ~S, which has ~
                    no Lisp Machine expansion in lispm-functions.lisp."
                   symbol))
           (cl:let ((expander (macro-function symbol)))
             (cons 'macro (lambda (form) (funcall expander form nil))))))))

(cl:defun si::open-code-p (symbol)
  (declare (ignore symbol))
  nil)

(export '(si::declared-definition si::open-code-p) "SI")
