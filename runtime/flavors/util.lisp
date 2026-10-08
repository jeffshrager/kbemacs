(in-package "http://pdp-10.trailing-edge.com/clisp#flavors-internals")

(defun fill* (sequence item &key (start 0) end)
  (if (array-has-fill-pointer-p sequence)
      (let ((fp (fill-pointer sequence)))
        (unwind-protect (progn
                          (setf (fill-pointer sequence)
                                (array-dimension sequence 0))
                          (fill sequence item :start start :end end))
          (setf (fill-pointer sequence)
                fp)))
      (fill sequence item :start start :end end)))


(defmacro compiler-let (binds &body body)
  `(#+sbcl sb-cltl2:compiler-let
    #+lispworks lw:compiler-let
    #+ccl ccl:compiler-let
    #-(:or sbcl lispworks ccl) compiler-let
    ,binds
    ,@body))


(defun logbitp* (index integer)
  (logbitp index integer))

(define-setf-expander logbitp* (index integer)
  (multiple-value-bind (temps vals stores store-form access-form)
                       (get-setf-expansion integer)
    (let ((itemp (gensym))
          (store (gensym))
          (stemp (first stores)))
      (values (cons itemp temps)
              (cons index vals)
              (list store)
              `(let ((,stemp (dpb (if ,store 1 0)
                                  (byte 1 ,itemp) ,access-form)))
                 ,store-form
                 ,store)
              `(logbitp* ,itemp ,access-form)))))
