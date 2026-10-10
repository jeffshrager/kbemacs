;;; -*- Mode: LISP; Package: CL-USER -*-
;;;
;;; Package skeleton for running the KBE sources under SBCL.
;;;
;;;   ZL       Zetalisp "global" vocabulary: Common Lisp, plus Zetalisp
;;;            functions and macros, plus Flavors and LetS. Nickname GLOBAL,
;;;            because the sources refer to GLOBAL:LET.
;;;   KBE      The package all KBE sources are read into (see sysdef.lisp).
;;;   SYS, SI, TV, FS, TIME, PP, ZWEI, DBG, COMPILER, CHAOS
;;;            Stand-ins for Lisp Machine system packages, so prefixed
;;;            symbols such as SYS:ERROR and PP:PP read. The loader
;;;            accepts references to symbols they don't export (see
;;;            load.lisp).

(in-package :cl-user)

(dolist (name '("SYS" "SI" "TV" "FS" "TIME" "PP" "ZWEI" "DBG" "COMPILER"
                "CHAOS"))
  (unless (find-package name)
    (make-package name :use '())))

(defpackage "ZL"
  (:nicknames "GLOBAL")
  (:use "COMMON-LISP" "LETS")
  ;; Flavors (Spice) supplies the object system. Zetalisp SEND also
  ;; accepts functions and streams, so ZL defines its own SEND.
  (:shadowing-import-from "SPICE-FLAVORS"
   "MAKE-INSTANCE" "SELF" "DEFWRAPPER" "CONTINUE-WHOPPER"
   "LEXPR-CONTINUE-WHOPPER" "CONTINUE-WHOPPER-ALL" "FUNCALL-SELF"
   "LEXPR-FUNCALL-SELF" "INSTANCEP" "SYMEVAL-IN-INSTANCE" "SET-IN-INSTANCE"
   "GET-HANDLER-FOR" "VANILLA-FLAVOR" "COMPILE-FLAVOR")
  ;; Zetalisp definitions that differ from the Common Lisp ones.
  ;; Common Lisp symbols whose Zetalisp definitions differ. Each is
  ;; defined in zl-compat.lisp, zl-conditions.lisp or flavors-ext.lisp.
  (:shadow
   ;; definitions
   "DEFUN" "DEFMACRO" "DEFFLAVOR" "DEFMETHOD" "DEFWHOPPER" "DEFSTRUCT"
   ;; binding, control
   "LET" "LET*" "DO" "DO*" "IF" "RETURN" "SETF"
   ;; lists
   "MEMBER" "ASSOC" "RASSOC" "DELETE" "REMOVE" "REM" "SUBST" "EVERY" "SOME"
   "UNION" "INTERSECTION" "MAKE-LIST" "LISTP" "MAP"
   ;; symbols
   "INTERN" "MAKE-SYMBOL" "GENSYM" "GET" "REMPROP" "TYPEP"
   ;; strings, characters
   "STRING-EQUAL" "CHARACTER"
   ;; arithmetic: (// 7 2) is 3 in Zetalisp; characters compare as
   ;; numbers (we read #/a as a CL character, Zetalisp as a fixnum)
   "/" "=" "/=" "<" ">" "<=" ">=" "+" "-" "1+" "1-" "BOOLE"
   ;; I/O
   "FORMAT" "READ"
   ;; conditions
   "ERROR" "SIGNAL" "WARN" "CERROR" "MAKE-CONDITION"
   ;; messages
   "SEND"
   ;; Zetalisp special variables (symbol macros on *PACKAGE*, *READTABLE*)
   "PACKAGE" "READTABLE"))

(defpackage "KBE"
  (:use "ZL")
  ;; sysdef.lisp shadows <- LET LET*; KBE's own LET lives in
  ;; runtime/zl-compat.lisp (Zetalisp LET is already destructuring here).
  ;; KBE also defines these names, which are Common Lisp symbols:
  (:shadow "<-" "DEFINE-CONDITION" "FIND" "STRING-CAPITALIZE" "WARN"
           ;; KBE declares this one special (analyze.lisp):
           "TYPE"))
