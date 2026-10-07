;;; -*- Mode:Lisp; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>PARSE.LISP.60, 3 June 1984 23:14:03
;;; Knowledge-Based_Editor_Jan-1985_volume_4.pdf, PDF pages 207-222.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;;; Declarations

;;; Based on a theory of parsing presented in:
;;;
;;;     Pratt, Vaughan R., ''Top Down Operator Precedence,''
;;;     ACM Symposium on Principles of Programming Languages
;;;     Boston, MA; October, 1973.
;;;
;;; Glossary of useful terms...
;;;
;;;   A NUD operator has NUll left Denotation. It has nothing to its
;;;    left. Nofix, Prefix and Matchfix operators are examples of
;;;    NUD operators.
;;;
;;;   An LED operator has LEft Denotation. They have something to their
;;;    left. Postfix, Infix and n-ary operators are LED operators.
;;;
;;;   The terms LBP and RBP are left and right binding power, respectively.
;;;    They refer to the stickiness toward the right or left of an
;;;    operator. Note that some parsing routines may be liberal with
;;;    the meaning of directionality. eg, for n-ary operators, there
;;;    must be equivalent stickiness in both directions, so the LBP
;;;    is used for stickiness in both directions and the RBP is not used.

;;; Special Variables

(define-KBE-option *input-language*
    "Input language for use by KBE"
  'LISP :assoc '(("Lisp"  . LISP)
                 ("PL//I" . PLI)
                 ("Ada"   . ADA)))

(defvar *language* nil
  "Dynamically bound to the active parser language structure")

(defvar *casification-hook* #'char-upcase ;this doesn't get used yet
  "Hook is a function called on input chars to adjust case if needed")

(defvar *character-set-size* 256.
  "The size of the set of characters to be seen by the parser")

;;;; Structure Descriptions

;;; Language Structure

(defmacro def-language (language &body morphemes)
  `(let ((language ',language)
         (language-structure (make-language language ',language)))
    (setf (language-structure language) language-structure)
    ,@(mapcar #'(lambda (x)
                  `(setf (language-morpheme ',(car x) language-structure)
                    (make-morpheme
                     name ',(car x)
                     language ',language
                     ,@(cond ((get x 'nud-lisp-alias) '())
                             (t `(nud-lisp-alias ',(car x))))
                     ,@(cond ((get x 'led-lisp-alias) '())
                             (t `(led-lisp-alias ',(car x))))
                     ,@(cdr x))))
              morphemes)))

(defmacro def-operator (operator language &body data)
  `(setf (language-morpheme ',operator (language-structure ',language))
    (make-morpheme
     name ',operator
     language ',language
     ,@(cond ((get (locf data) 'nud-lisp-alias) '())
             (t `(nud-lisp-alias ',operator)))
     ,@(cond ((get (locf data) 'led-lisp-alias) '())
             (t `(led-lisp-alias ',operator)))
     ,@data)))

(defstruct (language :named-array :conc-name)
  (language "??")
  (default-lbp 200.)
  (default-rbp 200.)
  (whitespace-characters '(#\tab #\space #\return #\line #\form))
  (syntax-table (make-syntax-table)))

(defmacro active-language ()
  '(language-language *language*))

(defmacro active-default-lbp ()
  '(language-default-lbp *language*))

(defmacro active-default-rbp ()
  '(language-default-rbp *language*))

(defmacro active-whitespace-chars ()
  '(language-whitespace-chars *language*))

(defmacro active-syntax-table ()
  '(language-syntax-table *language*))

(defmacro language-structure (language)
  `(get ,language 'language-structure))

(defun (language :named-structure-invoke) (op self &optional arg1
                                           &rest ignore)
  (selectq op
           ((:which-operations) '(:print-self))
           ((:print-self) (format arg1 "#<Language ~A ~O>"
                                  (language-language self)
                                  (%pointer self)))))

;;; Morpheme Structure

(defmacro semantics (sym) `(language-morpheme ,sym *language*))

(defmacro language-morpheme (sym language)
  `(get ,sym ,language))

(defmacro set-up-morpheme-structure (&body parts)
  `(progn 'compile (defstruct (morpheme :conc-name :named-array)
                     name
                     ,@(mapcar #'(lambda (x) (cond ((atom x) x)
                                                   (t (car x))))
                               parts))
    ,@(mapcar #'(lambda (x)
                  (let ((symbol (cond ((symbolp x) x)
                                      (t (car x))))
                        (default (cond ((symbolp x) nil)
                                       (t (cadr x)))))
                    (let ((y (symbolconc "MORPHEME-" symbol)))
                      `(defmacro ,symbol (data)
                        (let ((morph (gensym)))
                          `(let ((,morph (semantics ,data)))
                            (cond (,morph
                                   (or (,',y ,morph)
                                       ,',default))
                                  (t ,',default))))))))
              parts)))

(set-up-morpheme-structure
 language                          ; backpointer
 (lbp (active-default-lbp))        ; left binding power
 (rbp (active-default-rbp))        ; right binding power
 led-fn nud-fn                     ; led and nud parse functions
 pos rpos lpos                     ; parts of speech
 led-lisp-alias                    ; lisp alias
 nud-lisp-alias                    ;  ditto
 matchfix separator                ; matchfix-related info
 )

(defun (morpheme :named-structure-invoke) (op self &optional arg1
                                           &rest ignore)
  (selectq op
           ((:which-operations) '(:print-self))
           ((:print-self) (format arg1 "#<Morpheme ~A ~A ~O>"
                                  (morpheme-language self)
                                  (%pointer self)))))

;;; Syntactic Structure

(defmacro syntax (char) `(aref (active-syntax-table) ,char))

(defmacro syntax-definition (char language)
  `(aref (language-syntax-table ,language) ,char))

(defun make-syntax-table ()
  (make-array *character-set-size*
              ':leader-length 2
              ':named-structure-symbol 'syntax-table))

(defmacro def-syntax (language &body syntax-definitions)
  `(let ((language (language-structure ',language)))
    ,@(mapcar
       #'(lambda (x)
           (cond ((atom (car x))
                  `(setf (syntax-definition ',(car x) language)
                    ,(cadr x)))
                 ((and (atom (cdar x)) (cdar x))
                  `(do ((i ',(caar x) (1+ i))
                        (val ,(cadr x)))
                    ((> i ',(cdar x)))
                    (setf (syntax-definition i language) val)))
                 (t
                  `(let ((val ,(cadr x)))
                    (mapcar #'(lambda (i)
                                (setf (syntax-definition i language)
                                      val))
                     ',(car x))))))
       syntax-definitions)))

;;;; Syntax Analysis

(defun char-peek (stream) (tyipeek nil stream #\end))
(defun char-read (stream) (tyi         stream #\end))

(defun tokenize (stream)
  (do ((fn (syntax (char-peek stream))
           (syntax (char-peek stream))))
      (fn (funcall fn stream))
    (char-read stream)))

(defun alphabetp (c) (or (and (^] c #/A) (^\ c #/Z))
                         (and (^] c #/a) (^\ c #/z))))

(defun digitp (c) (and (^] c #/0) (^\ c #/9)))

(defun tokenize-number (stream)
  (let ((ibase 10.))
    (let ((chars (tokenize-number-internal stream)))
      (let ((result (ignore-errors (read-from-string chars))))
        (cond ((not (numberp result))
               (parse-error "Number parse failed: ~S" chars))
              (t result))))))

(defun tokenize-number-internal (stream) ;returns a string
  (with-output-to-string (output)
    (selectq (char-peek stream)
             ((#/+) (char-read stream))
             ((#/-) (tyo (char-read stream) output)))
    (do ((num-flag nil) (dot-flag nil) (e-flag nil)) (nil)
      (cond ((digitp (char-peek stream))
             (setq num-flag t)
             (tyo (char-read stream) output))
            ((eq (char-peek stream) #/.)
             (cond (dot-flag (parse-error "Too many .'s")))
             (setq dot-flag t)
             (tyo (char-read stream) output))
            ((memq (char-peek stream) '(#/E #/e))
             (cond ((not dot-flag)
                    (parse-error "Exponential form requires a dot."))
                   (e-flag
                    (parse-error
                     "Only one E allowed in exponential form."))
                   (t
                    (setq e-flag t)
                    (tyo (char-read stream) output)
                    (cond ((memq (char-peek stream) '(#/- #/+))
                           (tyo (char-read stream) output))))))
            (t
             (cond ((and dot-flag (not e-flag))
                    (cond ((not num-flag)
                           (parse-error "No digits in number.")))
                    (tyo #/0 output))) ;force floating point!
             (return nil))))))

(defun casify (x) (funcall *casification-hook* x))

(defun tokenize-symbol (stream)
  (intern-KBE (with-output-to-string (output)
                (do () (nil)
                  (cond ((or (digitp (char-peek stream))
                             (alphabetp (char-peek stream)))
                         (tyo (casify (char-read stream)) output))
                        (t (return nil)))))))

(defun tokenize-single-char (stream)
  (intern-KBE (string (char-read stream))))

(defun tokenize-stars (stream)
  (char-read stream) ;the first * is there for sure
  (cond ((= (char-peek stream) #/*)
         (char-read stream)
         '**)
        (t '*)))

(defun tokenize-dash (stream)
  (char-read stream)
  (cond ((= (char-peek stream) #/>)
         (char-read stream)
         '->)
        (t '-)))

;;;; Semantic Analysis

(defun parse-error (&rest stuff)
  (lexpr-funcall #'complain stuff))

(defprop parse-error t :error-reporter)

;;; type-checking stuff

(defmacro def-coercion (name arglist &body body)
  `(progn 'compile
    (defvar ,name nil
      ,(format nil
               "Parser-datatype marker/function for type ~A" name))
    (defun ,name ,arglist ,@body)
    (if (or (not (boundp ',name)) (not ,name))
        (setq ,name #',name))))

(defun typed-parse (type item) (cons type item))

(defun coerce-parsed-form (pos item) (funcall pos item))

(defmacro type-field (item) `(car ,item))
(defmacro data-field (item) `(cdr ,item))

(def-coercion *any* (item) (data-field item))

(def-coercion *pointer* (item)
  (local-declare ((special *expression*)) ;forward reference
                 (cond ((or (eq (type-field item) *any*)
                            (eq (type-field item) *expression*))
                        (data-field item))
                       (t (parse-error "Odd pointer expression seen")))))

(def-coercion *expression* (item)
  (cond ((or (eq (type-field item) *any*)
             (eq (type-field item) *expression*)
             (eq (type-field item) *pointer*))
         (data-field item))
        (t (parse-error "~&Expression not seen where expected.~
                         ~% Data: ~S" (type-field item)))))

(def-coercion *statement* (item)
  (let ((result (data-field item)))
    (cond ((atom result) result)
          ((memq (car result) '(= eq)) ;good ol' infix overloading!
           (cond ((symbolp (cadr result))
                  `(setq ,@(cdr result)))
                 (t
                  `(setf ,@(cdr result)))))
          (t result)))) ;be lax about error checking

;;; functions to handle nud/led operators

(defun nud (op)
  (let ((fn (nud-fn op)))
    (cond ((not fn) (parse-error "not a prefix operator: ~A" op))
          (t (funcall fn op)))))

(defun led (op left)
  (let ((fn (led-fn op)))
    (cond ((not fn) (parse-error "not an infix operator: ~A" op))
          (t (funcall fn op left)))))

;;; buffer abstraction

(defvar *buffer* nil)
(defvar *buffer-storage* nil)

(defun token-peek-2 ()
  (cond ((not *buffer-storage*) (token-peek) (token-peek-2))
        ((cdr *buffer-storage*) (cadr *buffer-storage*))
        (t (setq *buffer-storage*
                 (nconc *buffer-storage* (ncons (tokenize *buffer*))))
           (token-peek-2))))

(defun token-peek ()
  (cond (*buffer-storage* (car *buffer-storage*))
        (t (car (setq *buffer-storage* (ncons (tokenize *buffer*)))))))

(defun token-read ()
  (cond (*buffer-storage* (pop *buffer-storage*))
        (t (tokenize *buffer*))))

;;; interface to real world

(defun parser (&rest args)
  (lexpr-send terminal-io ':rubout-handler nil #'parser-internal args))

(defun string-parser (string &rest args)
  (with-input-from-string (stream string)
    (lexpr-funcall #'parser-internal stream args)))

(defun parser-internal (&optional
                          (stream terminal-io)
                          (language *input-language*))
  (let ((*language* (language-structure language))
        (*buffer-storage* nil)
        (*buffer* stream))
    (parse *any* 0)))

;;; driver loop

(defun parse (part-of-speech rbp) ; right binding power
  (do ((left (cond ((semantics (token-peek))
                    (nud (token-read)))
                   (t (typed-parse *any* (token-read))))
             (led (token-read) left)))
      ((^] rbp (lbp (token-peek)))
       (coerce-parsed-form part-of-speech left))))

;;; canned routines for easy parses

(defun parse-prefix (op)
  (typed-parse (pos op)
               (list (nud-lisp-alias op) (parse (rpos op) (rbp op)))))

(defun parse-infix (op left)
  (typed-parse (pos op)
               (list (led-lisp-alias op)
                     (coerce-parsed-form (lpos op) left)
                     (parse (rpos op) (rbp op)))))

(defun parse-postfix (op left)
  (typed-parse (pos op)
               (list (led-lisp-alias op)
                     (coerce-parsed-form (lpos op) left))))

(defun parse-nary (op left)
  (typed-parse (pos op)
               (list* (led-lisp-alias op)
                      (coerce-parsed-form (lpos op) left)
                      (parse-nary-internal op (lpos op) (lbp op)))))

(defun parse-nary-internal (op pos rbp)
  (do ((result (list (parse pos rbp))
               (cons (parse pos rbp) result)))
      ((not (eq op (token-peek)))
       (reverse result))
    (token-read)))

(defun parse-matchfix (op)
  (typed-parse (pos op)
               (cons (nud-lisp-alias op)
                     (parse-matchfix-internal
                      (matchfix op)
                      (separator op)
                      (rpos op)
                      (rbp op)))))

;;; m1 obj1 sep obj2 sep obj3 ... sep objn m2
;;;

(defun parse-matchfix-internal (match separator pos rbp)
  (do ((result nil (cons (parse pos rbp) result)))
      ((eq (token-peek) match)
       (token-read)
       (reverse result))
    (cond ((not result) (comment do nothing))
          ((eq separator (token-peek)) (token-read))
          (t (parse-error "missing ~A" match)))))

(defun parse-nofix (op)
  (typed-parse (pos op) (list (nud-lisp-alias op))))

(defun parse-noop (op)
  (typed-parse (pos op) nil))

(defun parse-left (op left)
  (typed-parse (pos op) (coerce-parsed-form *statement* left)))

;;; definitions of actual morphemes

                                        ;(defun parse-sign (op left)
                                        ;  (setq left (coerce-parsed-form *expression* left))
                                        ;  (do ((result (list (cond ((eq op '/-)
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;      ((not (memq (token-peek) '(/+ /-)))
                                        ;       (typed-parse *expression*
                                        ;
                                        ;    (cond ((eq (token-peek) '/+) (token-read)))))

(defun parse-nud-lparen (op)
  (cond ((eq (token-peek) (matchfix op))
         (parse-error "Invalid parenthesized expression"))
        (t
         (let ((right (parse-matchfix-internal
                       (matchfix op)
                       (separator op) ;shouldn't get used
                       (rpos op)
                       (rbp op))))
           (cond ((or (null right) (cdr right))
                  (parse-error "Too many expressions?")) ;shouldn't happen
                 (t
                  (typed-parse (pos op) (car right))))))))

(defun parse-led-lparen (op left)
  (setq left (coerce-parsed-form (lpos op) left))
  (cond ((not (symbolp left))
         (parse-error "Invalid expression in operator position"))
        (t
         (let ((right (parse-matchfix-internal
                       (matchfix op)
                       (separator op)
                       (rpos op)
                       (rbp op))))
           (typed-parse (pos op) (cons left right))))))

;;;; Algol

(def-language algol

    (/^       lbp             140.
              rbp             139.
              pos             *expression*
              lpos            *expression*
              rpos            *expression*
              led-fn          #'parse-infix)

  (/*       lbp             120.
            pos             *expression*
            lpos            *expression*
            led-fn          #'parse-nary)

  (/+       lbp             100.
            rbp             100.
            pos             *expression*
            rpos            *expression*
            lpos            *expression*
            led-fn          #'parse-nary
            nud-fn          #'parse-prefix)

  (/-       lbp             100.
            rbp             134.
            pos             *expression*
            rpos            *expression*
            lpos            *expression*
            led-fn          #'parse-nary
            nud-fn          #'parse-prefix)

  (begin    rbp             10.
            pos             *any*
            rpos            *expression*
            nud-fn          #'parse-matchfix
            nud-lisp-alias 'algol-block
            matchfix        'end
            separator       '/;)

  (/;       lbp             5)

  (end      lbp             5)

  (#.(values (intern (string #\end)))
     lbp             -1.) )

(def-syntax algol
    ((#/A . #/Z)                        #'tokenize-symbol      )
  ((#/a . #/z)                        #'tokenize-symbol      )
  ((#/0 . #/9)                        #'tokenize-number      )
  ((#/+ #/- #/* #// #/^ #\END #/;) #'tokenize-single-char ))

;;;; PL/1

(defun parse-PL/1-do (op)
  (typed-parse (pos op)
               (parse-PL/1-do-internal op (separator op) (matchfix op))))

(defun parse-PL/1-do-internal (op separator match)
  (do ((result nil (cons statement result))
       (statement (parse-PL/1-statement op) (parse-PL/1-statement op)))
      ((eq statement match)
       (let ((forms (reverse result)))
         (cond ((null (car forms))
                `(PROGN ,@(cdr forms)))
               (t
                `(PL1-ITER ,@forms)))))
    (cond ((eq (token-peek) separator)
           (token-read))
          (t (parse-error "Missing ~A" separator)))))

(defun parse-PL/1-statement (op)
  (let ((statement (cond ((eq (token-peek) '/;) NIL)
                         (t (parse *statement* (rbp op))))))
    statement))

(defun parse-PL/1-end (op)
  (cond ((not (eq (token-peek) '/;)) (token-read))) ;optional tag
  (typed-parse (pos op) op))

(defun parse-PL/1-if (op)
  (let ((condition (parse *expression* (rbp op))))
    (cond ((not (eq (token-peek) 'then))
           (parse-error "Missing THEN in ~A statement" op)))
    (let ((consequent (parse-PL/1-statement (token-read))))
      (typed-parse (pos op)
                   `(cond (,condition ,consequent)
                     ,@(cond ((eq (token-peek-2) 'else)
                              (token-read) ;flush the ";"
                              `((t ,(parse-PL/1-statement
                                     (token-read)))))))))))

(defun parse-PL/1-pointer (op left)
  (setq left (coerce-parsed-form (lpos op) left))
  (cond ((not (symbolp left))
         (parse-error "Unrecognized pointer expression seen"))
        (t
         (do ((result left) (flag nil t))
             ((and flag (not (eq (token-peek) '->)))
              (typed-parse (pos op) result))
           (cond (flag (token-read))) ;pop arrows
           (setq result
                 (list (caseq (token-peek)
                              (next  'cdr)
                              (value 'car)
                              (t
                               (parse-error "Unrecognized pointer usage.~
                                        ~% Data: ~A" (token-peek))))
                       result))
           (token-read) ;pop the used token
           ))))

(defun parse-PL/1-= (op left)
  (typed-parse (pos op)
               (list (cond ((eq (type-field left) *pointer*) 'EQ)
                           (T '=))
                     (coerce-parsed-form (lpos op) left)
                     (parse (rpos op) (rbp op)))))

(def-language PL/1

    (->       lbp             180.
              rbp             180.
              pos             *pointer*
              lpos            *expression*
              rpos            *expression*
              led-fn          #'parse-PL/1-pointer)

  (**       lbp             140.
            rbp             139.
            pos             *expression*
            lpos            *expression*
            rpos            *expression*
            led-lisp-alias '/^
            led-fn          #'parse-infix)

  (//       lbp             120.
            pos             *expression*
            lpos            *expression*
            led-fn          #'parse-nary)

  (*        lbp             120.
            pos             *expression*
            lpos            *expression*
            led-fn          #'parse-nary)

  (+        lbp             100.
            rbp             100.
            pos             *expression*
            rpos            *expression*
            lpos            *expression*
            led-fn          #'parse-nary
            nud-fn          #'parse-prefix)

  (-        lbp             100.
            rbp             134.
            pos             *expression*
            rpos            *expression*
            lpos            *expression*
            led-fn          #'parse-nary
            nud-fn          #'parse-prefix)

  (do       lbp             30
   rbp             25.
   pos             *statement*
   rpos            *statement*
   nud-fn          #'parse-PL/1-do
   matchfix        'end
   separator       '/;)

  (if       rbp             45.
                                        ;no lbp (default as high as possible)
            pos             *any*
            rpos            *any*
            nud-fn          #'parse-PL/1-if
            )

  (then     lbp             5.
            rbp             25.)

  (else     lbp             5.
            rbp             25.)

  (>        lbp             80.
            rbp             80.
            pos             *expression*
            rpos            *expression*
            lpos            *expression*
            led-fn          #'parse-infix)

  (<        lbp             80.
            rbp             80.
            pos             *expression*
            rpos            *expression*
            lpos            *expression*
            led-fn          #'parse-infix)

  (=        lbp             80.
            rbp             80.
            pos             *expression*
            rpos            *expression*
            lpos            *expression*
            led-fn          #'parse-PL/1-=)

  (/(       lbp             200.
            rbp             10.
            pos             *expression*
            lpos            *expression*
            rpos            *expression*
            led-fn          #'parse-led-lparen
            nud-fn          #'parse-nud-lparen
            matchfix        '/)
            separator       '/,)

  (/,       lbp             10.) ;probably a nud-err/led-err should be here

  (/)       lbp             5.) ;probably a nud-err/led-err should be here

  (/;       lbp             5.
            rbp             10.
            pos             *statement*
            led-fn          #'parse-left)

  (end      lbp             5
            pos             *statement*
            nud-fn          #'parse-PL/1-end)

  (#.(values (intern (string #\end)))
     lbp             -1.) )

(def-syntax PL/1
    ((#/A . #/Z)                        #'tokenize-symbol      )
  ((#/a . #/z)                        #'tokenize-symbol      )
  ((#/0 . #/9)                        #'tokenize-number      )
  (#/.                                #'tokenize-number      )
  (#/*                                #'tokenize-stars       )
  (#/-                                #'tokenize-dash        )
  ((#/+ #// #/^
        #/( #/) #/,
        #/= #/< #/>
        #\END #/; #/:)                    #'tokenize-single-char ))

;;; Temporary cross-file compatibility measure

(setf (language-structure 'PLI) (language-structure 'PL/1))

;;;; LISP

(def-language LISP
    (#.(values (intern (string #\end   ))) lbp -1)
  (#.(values (intern (string #\tab   ))) lbp -1)
  (#.(values (intern (string #\space ))) lbp -1)
  (#.(values (intern (string #\return))) lbp -1)
  (#.(values (intern (string #\line  ))) lbp -1)
  (#.(values (intern (string #/.     ))) lbp -1)
  (#.(values (intern (string #/,     ))) lbp -1)
  (#.(values (intern (string #/)     ))) lbp -1) )

(def-syntax LISP
    ((0 . 255) #'read-hacking-/{) ; 255 = #.(1- *character-set-size*)
  ((#/. #/, #/) #\END #\TAB #\SPACE #\RETURN #\LINE)
   #'tokenize-single-char))

;;;; ada

(def-language ada
    (#.(values (intern (string #\end   ))) lbp -1) )

(def-syntax ada
    ((0 . 255) #'ada-parse-to-end) ; 255 = #.(1- *character-set-size*)
  ((#\END) #'tokenize-single-char))
