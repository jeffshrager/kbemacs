;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "http://pdp-10.trailing-edge.com/clisp#flavors"
  (:nicknames :spice-flavors)
  (:use)
  (:export send
           funcall-self
           lexpr-funcall-self
           self
           instancep
           symeval-in-instance
           set-in-instance
           get-handler-for
           iv-bound-p
           vanilla-flavor
           *all-flavor-names*
           *undefined-flavor-names*
           *flavor-compile-methods*
           *default-combination*
           *dirty-flavors*
           cleanup-all-flavors
           cleanup-flavor
           without-cleaning-flavors
           defflavor
           undefflavor
           defmethod
           defwrapper
           defwhopper
           continue-whopper
           lexpr-continue-whopper
           continue-whopper-all
           undefmethod
           recompile-flavor
           make-instance
           flavor-allowed-init-keywords
           flavor-allows-init-keyword-p
           compile-flavor
           compiler-compile-flavors
           reconstruct-defflavor
           method-list
           call-methods
           wrapper-mix
           defcombination-ordering
           defcombination
           order-methods
           order-wrappers ))


(defpackage "http://pdp-10.trailing-edge.com/clisp#flavors-internals"
  (:use "http://pdp-10.trailing-edge.com/clisp#flavors"
        cl)  ; KBE: fiveam dependency dropped (test.lisp not vendored)
  (:shadowing-import-from
   "http://pdp-10.trailing-edge.com/clisp#flavors"
   :make-instance
   :defmethod)
  (:shadow :method
           :make-method
           :find-method))


;;; *EOF*
