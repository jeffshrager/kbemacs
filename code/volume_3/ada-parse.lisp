;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: ADA-PARSE.LISP; PDF pages 179-202.

;note idea of having points in rules that you
;cannot back up through in order to aid debugability.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; Support for letting the apprentice hack a small subset of ADA.

;This file is a grammar for the subset of ADA supported in the 84 demo.
;The grammar below is executable and conforms to the following rules.

;Each production has the form:
;(def-rule ada-pattern lisp-pattern)
;the lisp-pattern is occasionally omitted this means that it
;is exactly like the ada-pattern
;If the correspondence between the two lines is not obvious, non terminal
;names of the form <non-terminal>1 <non-terminal>2 will sometimes be used.
;The numbers do not change the meaning of the non terminal, but
;just serve to identify individuals.

;<...>  is a non terminal grammar symbol.
;other lisp atoms in rules are terminal symbols

;(... ...) indicates a sequence of forms
;(optional ... ...) indicates an optional sequence of forms
;(one-of <...> <...>) indicates alteration of non-terminals
;  note that it is faster if the common cases are first, but
;  if one case is an initial substring of another than it must be first.
;(zero-or-more <...> after after-last?)
;  This indicates repetitions of a non-terminal.  it expects to find the
;  AFTER terminal (if any) between the items.  if the AFTER-LAST? flag
;  is present then the AFTER must be after every instance.
;(one-or-more <...> between after)
;  this is the same but requires at least one.

;The grammar here is quite weak.  (Ie it can generate a lot of
;junk that is not ada.)  However it can parse the ada subset correctly,
;and making it weaker makes it simpler in a lot of places.

(comment

    (defun parse-test ()
  (declare (special pt test result))
  (setq result (ada-parse pt))
  (parse-comp)
  (let (ada-supress) (pl result)))

    (defun parse-comp ()
  (declare (special test result ada-supress))
  (let ((ada-supress t))
    (parse-comp1 result test)))

    (defun parse-comp1 (a b)
  (when (not (equal a b))
    (format T "~%~:=~% is not the same as the required~%~:=" a b)
    (if (and (listp a) (listp b) (not (= (length a) (length b))))
        (format t "~2% **** note their length is not the same"))
    (if (y-or-n-p "go deeper?")
        (mapcar #'parse-comp1 a b))))
)

;the parser here works in a very straightforward (and probably slow) way.
;it takes in a string and returns a parse tree list.
;As a first pass over the string it is converted to a list of tokens.
;the grammer rules parse this list of tokens producing a new list of
;tokens.  This is then grouped up into a parse tree.

(defvar *token-in-stream* nil "parse point for ada-parser")
(defvar *last-form-parsed* nil "last stmnt or dcl successfully parsed")

(defun ada-parse-to-end (&optional (stream standard-input)
                                   (end-chars '(#\End)))
  (ada-parse (readstring stream end-chars)))

(defun ada-parse (ada-string &optional (node '<sentence>))
  (declare (special ada-use-packages ada-info ada-prefix-stack))
  (let* ((ada-use-packages nil)
         (ada-info ada-info)
         (ada-prefix-stack (ncons '*parsing*))
         (ada-tokens (ada-tokenize ada-string))
         (lisp-tokens (ada-tokens-parse ada-tokens node)))
    (ada-gather-lisp-tokens lisp-tokens)))

(defun ada-gather-lisp-tokens (*token-in-stream* &optional error-supress)
  (setq *token-in-stream* (delq '*dummy* *token-in-stream*))
  (let* ((token (pop *token-in-stream*))
         (result (if (eq token '/() (sub-gather) token)))
    (if (and *token-in-stream* (not error-supress))
        (grumble "parse- output not just one form"))
    result))

(defun sub-gather ()
  (let (result token)
    (loop do
      (when (null *token-in-stream*)
        (grumble "parse- too few close parens in output")
        (return result))
      (setq token (pop *token-in-stream*))
      (caseq token
        (/( (push (sub-gather) result))
        (/) (return (nreverse result)))
        (T (push token result))))))

(defun ada-tokens-parse (*token-in-stream* node)
  (let* (*last-form-parsed* (result (funcall node)))
    (when (or (null result) *token-in-stream*)
      (barf "ada parse failed last dcl//stmnt parsed:~% ~:="
            (ada-gather-lisp-tokens *last-form-parsed* T)))
    result))

;this works from the back so that it can get the tokens in the
;right order.  also it just so happens that some things are easier
;to tokenize from the back. (and some aren't.)

(eval-when (eval load compile)

    (defvar *ada-break-chars*
        (string-append #\newline #\tab #\lf "/" //.:;=<>,+-&*#{}()'"))

    (defun ada-remove-comments (string)
  (let (chunks (start 0) hit)
    (loop do
      (setq hit (string-search "--" string start))
      (when (null hit)
        (when (null chunks) (return string))
        (push (substring string (min start (string-length string))) chunks)
        (return (apply #'string-append (nreverse chunks))))
      (push (substring string start hit) chunks)
      (setq start (or (string-search-char #\newline string hit)
                      (string-length string))))))

    (defun ada-tokenize (string)
  (setq string (ada-remove-comments string))
  (let (tokens (scan (string-length string)) char)
    (loop do
      (setq scan
            (or (string-reverse-search-not-set
                  #.(string-append #\space #\newline #\tab #\lf)
                  string scan)
                -1))
      (if (minusp scan) (return tokens))
      (setq char (aref string scan))
      (cond
        ((= char #/")
         (let ((start (string-reverse-search-char #/" string scan)))
           (push (substring string (1+ start) scan) tokens)
           (setq scan start)))
        ((= char #/})
         (push '/} tokens)
         (let ((start (string-reverse-search-set "{," string scan)))
           (when start
             (push (substring string (1+ start) scan) tokens)
             (push (ascii (aref string start)) tokens)
             (setq scan start))))
        ((string-search-char char *ada-break-chars*)
         (let ((pair (and (> scan 0)
                          (substring string (1- scan) (1+ scan)))))
           (cond ((member pair '(".." "//=" "<=" ">=" "=>" "<>" ":="))
                  (push (intern pair) tokens)
                  (decf scan))
                 (T (push (intern (substring string scan (1+ scan)))
                          tokens)))))
        (T (let* ((break (or (string-reverse-search-set
                               *ada-break-chars* string scan) -1))
                  (non-number (or (string-reverse-search-not-set
                                    "0123456789" string (1+ scan)) -1))
                  (substring (substring string (1+ break) (1+ scan))))
             (if (= break non-number)
                 (push (read-from-string substring) tokens)
                 (push (intern (string-upcase substring)) tokens))
             (setq scan (1+ break))))))))
);end of eval-when

;;; PDF page 182

(defmacro def-rule (name ada-pattern &optional (lisp-pattern ada-pattern))
  (let* (((new-ada-pattern new-lisp-pattern vars)
          (process-rule-body ada-pattern lisp-pattern)))
    `(defun ,name ()
       (let ,vars
         (if ,new-ada-pattern ,new-lisp-pattern)))))
(eval-when (eval load compile)
    (defun process-rule-body (ada-pattern lisp-pattern)
  (let* (((new-ada-pattern in-vars) (process-pattern-in ada-pattern))
         ((new-lisp-pattern out-vars) (process-pattern-out lisp-pattern)))
    (if (< (length in-vars) (length (makeqset in-vars)))
        (barf "def-rule: input var used twice."))
    (if (< (length out-vars) (length (makeqset out-vars)))
        (barf "def-rule: output var used twice."))
    (if (not (qset< out-vars in-vars))
        (barf "def-rule: output of rule refs vars not in input"))
    (list new-ada-pattern new-lisp-pattern in-vars)))
    (defvar *pattern-vars* nil)
    (defun process-pattern-in (pattern)
  (let (*pattern-vars*)
    (list (pp-in pattern) *pattern-vars*)))
    (defun process-pattern-out (pattern)
  (let (*pattern-vars*)
    (list (pp-out pattern) *pattern-vars*)))
    (defun non-terminal-root (symbol)
  (cond ((not (and (symbolp symbol)
                   (> (string-length symbol) 2)
                   (= (aref (string symbol) 0) #/<))) nil)
        ((= (aref (string symbol) (1- (string-length symbol))) #/>) symbol)
        (T (intern (substring symbol 0
                     (1+ (string-reverse-search-char #/> symbol)))))))
;;; PDF page 183

    (defun pp-in (form)
  (cond ((symbolp form)
         (let ((root (non-terminal-root form)))
           (cond ((null root)
                  (let ((tokens (ada-tokenize (string form))))
                    (if (not (= (length tokens) 1))
                        (barf "def-rule: unrecognizable token ~a" form)))
                  `(terminal-in ,form))
                 (T (push form *pattern-vars*)
                    `(non-terminal-in ,form ,root)))))
        ((not (listp form)) (barf "def-rule: bad pattern form ~a" form))
        ((eq (car form) 'optional)
         `(optional-in ., (mapcar #'pp-in (cdr form) )))
        ((eq (car form) 'one-of)
         (dolist (f (cdr form))
           (if (not (symbolp f))
               (barf "def-rule: one-of too complex ~a" form)))
         `(one-of-in ., (mapcar #'pp-in (cdr form))))
        ((memq (car form) '(one-or-more zero-or-more))
         (let ((root (non-terminal-root (cadr form))))
           (if (not (and root (symbolp (caddr form))
                         (not (non-terminal-root (caddr form)))
                         (memq (cadddr form) '(T nil))))
               (barf "def-rule: bad repetition ~a" form) )
           (push (cadr form) *pattern-vars*)
           `(repetition-in ,(cadr form) ,root ,(eq (car form) 'one-or-more)
                           ., (cddr form))))
        ((eq (car form) 'computation)
         (if (not (and (symbolp (cadr form))
                       (non-terminal-root (caddr form))
                       (memq (caddr form) *pattern-vars*)
                       (null (cdddr form))))
             (barf "def-rule: computation too complex ~a" form))
         `(computation-in ., (cdr form)))
        (T `(sequence-in ., (mapcar #'pp-in form)))))
;;; PDF page 184

    (defun pp-out (form)
  (cond ((symbolp form)
         (let ((root (non-terminal-root form)))
           (cond ((null root) `(terminal-out ,form))
                 (T (push form *pattern-vars*)
                    `(non-terminal-out ,form)))))
        ((not (listp form)) (barf "def-rule: bad pattern form ~a" form))
        ((eq (car form) 'optional)
         (dolist (f (cdr form))
           (if (not (or (symbolp f)
                        (and (listp f)
                             (memq (car f) '(one-or-more zero-or-more)))))
               (barf "def-rule: optional form too complex ~a" form)))
         `(optional-out ., (mapcar #'pp-out (cdr form))))
        ((eq (car form) 'one-of)
         (dolist (f (cdr form))
           (if (not (non-terminal-root f))
               (barf "def-rule: output one-of too complex ~a" form))
           (push f *pattern-vars*) )
         `(one-of-out ., (cdr form)))
        ((memq (car form) '(one-or-more zero-or-more))
         (let ((root (non-terminal-root (cadr form))))
           (if (not (and root (null (cddr form))))
               (barf "def-rule: bad output repetition ~a" form))
           (push (cadr form) *pattern-vars*)
           `(repetition-out ,(cadr form))))
        ((eq (car form) 'computation)
         (if (not (and (symbolp (cadr form))
                       (non-terminal-root (caddr form))
                       (null (cdddr form))))
             (barf "def-rule: computation too complex ~a" form))
         ;note allowing any kind of var ref here
          ;you must be carfull of nconcs in code
          `(computation-out ., (cdr form)))
        (T  `(sequence-out ., (mapcar #'pp-out form)))))
)  ;end of eval-when
;;; PDF page 185

;a form returns NIL iff it fails in parsing.
;also every var assigned by a successful parse is non-NIL,
(defmacro sequence-in (&body forms)
  `(and ., forms))
(defmacro optional-in (&body forms)
  (let ((form (if (cdr forms) `(and ., forms) (car forms))))
    `(let ((old-token-in-stream *token-in-stream*))
       (cond ((null ,form) (setq *token-in-stream* old-token-in-stream) ))
       T)))
(defmacro one-of-in (&body forms)
  `(let ((old-token-in-stream *token-in-stream*) )
     (one-of-in1 ., forms)))
(defmacro one-of-in1 (&body forms)
  (if (null (cdr forms)) (car forms)
      `(cond (,(car forms))
             (T (setq *token-in-stream* old-token-in-stream)
                (one-of-in1 ., (cdr forms))))))
;note use of *dummy* so that NIL is never found.
(defmacro repetition-in (var name require-one?
                         &optional after after-every?)
  (let* ((new-var (symbolconc "NEW-" var))
         (list-var (symbolconc "LIST-" var))
      (form     `(non-terminal-in ,new-var ,name)))
    (when after
      (if after-every?
           (setq form `(and ,form (terminal-in ,after)))
           (setq form `(and (or (null ,list-var)
                               (terminal-in  ,after))
                            ,form))))
     `(prog (,new-var ,list-var saved-token-in-stream)
        L  (setq saved-token-in-stream *token-in-stream*)
           (cond (,form        :
    (setq ,list-var (append ,list-var ,New-var))
                  (go L)))
           (setq *token-in-stream* saved-token-in-stream)
           ,@(if require-one? `((cond ((null ,list-var) (return nil)))))
           (setq ,var (cons '*dummy* ,list-var))
           (return T))))
(defmacro non-terminal-in (var name)
   `(setq ,var (,name)))
(defmacro terminal-in (terminal)
   `(eq ', terminal (pop *token-in-stream*) ))
(defmacro computation-in (fn var)
   `(,fn ,var))
;;; PDF page 186

(defmacro sequence-out (&body forms)
  `(append ., forms))
;cannot have a sequence-out optional-out or one-of-out form         :
(defmacro optional-out (&body forms)
  (if (null (cdr forms))
      (car forms)
      (let (test-vars)
        (dolist (form forms)
          (if (memq (car form) '(non-terminal-out repetition-out))
              (push (cadr form) test-vars)))
        (if (cdr test-vars)
            (setq test-vars `(and ., test-vars))
            (setq test-vars (or (car test-vars) T)))
        `(cond (,test-vars (append ., forms))))))
(defmacro one-of-out (&body vars)
  `(or ., vars))
;must make sure we-always have something.
(defmacro repetition-out (var)
  `(or ,var (ncons '*dummy*)))
(defmacro non-terminal-out (var)
  `(progn ,var)) ;to help macroexpandall
(defmacro terminal-out (terminal)
  `(ncons ',terminal))
(defmacro computation-out (fn var)
   `(,fn ,var))
;;; PDF page 187

;A problem arises parsing ada from the fact that
;in ada all of the following things are realized as symbols and
;cannot be distinguished in isolation.  As a result I have a function
;ADA-SYMBOL-TYPE which when passed a symbol will return the indicated
;code showing its type in the local context.
;many of the problems with symbols are actually disambiguated by the
;syntactic context.  However, several things can still be confused which
;appear in the same context.  For example the following indistinguishable
;pairs of things can all be <exp>s.
; A zero arg <fn call> looks like a <variable>.
; A <converter> looks like a one argument <fn call>.
; A <field selector> looks like a zero arg <fn call>.
; A <vVariable> looks like an <enumeration literal>.
;we are experimenting with making these as weak as possible
;(defmacro <variable> () '(parse-token-type *variable))
(defmacro <variable> () '(<symbol>) )
;(defmacro <field-name> () '(parse-token-type  'field))
;should do better than this for the ada parsability problems.
;must think about it.
(defun <field-name> ()
  (let ((token (pop *token-in-stream*) ))
    (if (or (memq token  '(key count name) )
            (memq (ada-symbol-type token)  '(field variable)))
        (ncons token))))
(defmacro <attribute-name>  () '(parse-token-type 'attribute))
(defmacro <fn-name-root>  () '(parse-token-type 'fn))
(defmacro <generic-name-root> () '(parse-token-type 'generic))
(defmacro <enumeration-element-root> () '(parse-token-type 'enum))
(defmacro <package-name-root> () '(parse-token-type 'package "_IO"))
 ;(defmacro <exception-name-root> () '(parse-token-type 'exception))
(defun <exception-name-root> ()
   (let ((token (pop *token-in-stream*) ))
     (if (or (eq token 'others) (eq (ada-symbol-class token) 'symbol))
         (ncons token))))
(defmacro <basic-type-name-root> () '(parse-token-type 'type "_TYPE"))
(def-rule <type-name-root>
   (one-of <limited-private> <basic-type-name-root>) )
(def-rule <limited-private> (LIMITED PRIVATE) LIMITED-PRIVATE)
(defmacro <basic-mode> () '(parse-token-type 'mode))
(def-rule <mode> (one-of <in-out> <basic-mode>))
(def-rule <in-out> (IN OUT) IN-OUT)
;;; PDF page 188

(defmacro <symbol> () '(parse-token-class 'symbol))
(defmacro <operator> () '(parse-token-class 'operator))
(defmacro <integer> () '(parse-token-class 'integer))
(defmacro <string> () '(parse-token-class 'string))
(defun <word> ()
  (let ((token (pop *token-in-stream*)))
    (if (symbolp token) (ncons token))))
(defun parse-token-type (required-type &optional name_string)
  (when *token-in-stream*
    (let* ((token (pop *token-in-stream*))
           (type (ada-symbol-type token) ))
      (if (or (eq required-type type)
              (and name_string (string-search name_string token)))
          (ncons token)))))
(defun parse-token-class (required-class)
  (when *token-in-stream*
    (let ((token (pop *token-in-stream*) ))
      (if (eq required-class (ada-symbol-class token)) (ncons token)))))
(defun ada-symbol-class (token)
  (cond ((numberp token) 'integer)
        ((stringp token) 'string)
        ((eq token 'entry) 'keyword) ;KBE pseudo-op for dummy definitions.
        ((memq token
               '(with use package end body
                 procedure function generic cliche in out when exception
                 ;;; new return constant type subtype renames record
                 string range begin loop while if then elsif else exit
                 raise private limited others))
          'keyword)
        ((memq token  '(or xor and = //= <= >= < > + - &
                          not abs * // mod rem **))
          'operator)
        (T  'symbol)))
 ;In order for ADA-SYMBOL-TYPE to work, you have to bind the variable
 ;ADA-INFO each time you cross a <def form> contour and evaluate each
 ;<dcl> immediately after it is parsed.
 ;this takes in a root symbol and tells you what kind of thing it is.
 ;;; can be buggy if a non local root symbol has been defined to be
 ;things of more than one catagory. It will just  tell you the first
 ;answer it finds. It  is not used in this file.  It is here to help
 ;the parser.
(defun ada-symbol-type (token)
   (declare (special ada-info))
   (cond ((memq token '(length last)) 'ATTRIBUTE)
         ((memq token '(in out)) 'mode)
         ((eq token 'others) 'exception)
         ((eq token 'private) 'type)
         ((do ((info ada-info (cdr info))) ((null info))
            (when (eq token (cadaar info))
              (return (caaar info)))))
         (T 'unknown)))
;;; PDF page 189

(def-rule <sentence> (one-of <top-level-form> <dcl> <stmnt> <exp>))
(def-rule <top-level-form>
  ((optional WITH <contour-names+> /;)
   (optional <top-use>)
   <def-form>1 /;
   (optional <def-form>2 /;))
  (/( DEF-ADA-TOP (computation get-def-name <def-form>1)
      /( <contour-names+> /)
      /( <top-use> /)
      <def-form>1 (optional <def-form>2) /) ))
(defun <top-use> ()
  (let* ((use (<basic-top-use>)))
    (when use
      (eval (ada-gather-lisp-tokens `( /( ada-use ,@use /))))
      use)))
(def-rule <basic-top-use>
  (USE <contour-names+> /;)
  <contour-names+>)
(def-rule <def-form>
  (one-of <fn> <cliche> <package> <package-body> <generic> <dummy> ) )
(defun get-def-name (defs)
  (ncons (caseq (cadr defs)
           ((def-ada-package def-ada-package-body def-ada-cliche)
            (nth 2 defs))
           ((def-ada-fn def-ada-entry) (nth 3 defs))
    (def-ada-generic )
           (T (barf "parse- unknown def form")))))
(def-rule <package>
  (PACKAGE <symbol> (optional IS <dcls> END <symbol>2))
  (/( DEF-ADA-PACKAGE <symbol> /( <dcls> /) /)))
(def-rule <package-body>
  (PACKAGE BODY <symbol> <dcls> END (optional <symbol>2))
  (/( DEF-ADA-PACKAGE-BODY <symbol> /( <dcls> /) /)))
(def-rule <dummy> (ENTRY <fn-spec>)
  (/( DEF-ADA-ENTRY <fn-spec> /) ))
(def-rule <fn>
  ((one-of PROCEDURE FUNCTION) <fn-spec>
   (optional IS <dcls> BEGIN <stmnts+> <exception-clauses>
             END (optional <fn-designator>)))
  (/( DEF-ADA-FN <fn-spec>
        (optional /( <dcls> /) /( <stmnts+> /)
                  /( <exception-clauses> /)) /) ))
(def-rule <generic>
  (GENERIC <dcls> (one-of <fn> <package>))
  (/( DEF-ADA-GENERIC /( <dcls> /) (one-of <fn> <package>) /)))
(def-rule <fn-spec>
  (<fn-designator> <params> (optional RETURN <type>))
  (/( <fn-designator> <params> (optional <type>) /) ))
(def-rule <fn-designator>
  (one-of <symbol> <operator-string>) )
(def-rule <params>
  (optional /( (one-or-more <param> /;) /))
  (/( (zero-or-more <param>) /)))
(def-rule <param>
;;; PDF page 190

 (<symbol> /: (optional <mode>) <type>)
 (/( <symbol> <type> (optional <mode>) /)))
;;; PDF page 191

(def-rule <cliche>
  (one-of <full-cliche> <dcl-cliche> <expr-cliche> <empty-cliche>))
(def-rule <empty-cliche>
 (CLICHE <symbol>)
 (/( DEF-ADA-CLICHE <symbol> NIL /)))
(def-rule <full-cliche>
 (CLICHE <symbol> IS <cliche-comments>
         <dcls> BEGIN <stmnts+> <exception-clauses>
    END (optional <symbol>2))
 (/( DEF-ADA-CLICHE <symbol>
     <cliche-comments> /( <dcls> /) /( <stmnts+> /)
     /( <exception-clauses> /) /)))
(def-rule <dcl-cliche>
 (CLICHE <symbol> IS <cliche-comments> <dcls>
    END (optional <symbol>2))
 (/( DEF-ADA-CLICHE <symbol>
     <cliche-comments> /( <dcls> /) /( /) /( /) /)))
(def-rule <expr-cliche>
  (CLICHE <symbol> IS <cliche-comments> <exp>
    END (optional <symbol>2))
  (/( DEF-ADA-CLICHE <symbol> <cliche-comments> <exp> /)))
(def-rule <cliche-comments>
  ((optional <cliche-primary-roles-comment>)
   (optional <cliche-described-roles-comment>)
   (optional <cliche-comment-comment>)
   (optional <cliche-constraints-comment>) )
  (/( (optional <cliche-primary-roles-comment>)
      (optional <cliche-described-roles-comment>)
      (optional <cliche-comment-comment>)
      (optional <cliche-constraints-comment>) /)))
(def-rule <cliche-primary-roles-comment>
  (PRIMARY ROLES (one-or-more <word> /,) /;)
  (PRIMARY-ROLES /( (one-or-more <word>) /)))
(def-rule <cliche-described-roles-comment>
  (DESCRIBED ROLES (one-or-more <word> /,) /;)
  (DESCRIBED-ROLES /( (one-or-more <word>) /)))
(def-rule <cliche-comment-comment>
  (COMMENT <string-literal> /;)
  (COMMENT <string-literal>))
(def-rule <cliche-constraints-comment>
  (CONSTRAINTS <stmnts+> END CONSTRAINTS /;)
  (CONSTRAINTS /( <stmnts+> /)))
;;; PDF page 192

(def-rule <dcls>
 (zero-or-more <dcl> /; T)
 (zero-or-more <dcl>))
(defun <dcl> ()
 (let* ((dcl (<basic-dcl>)))
   (when dcl
     (setq *last-form-parsed* dcl)
      ;to do vars right must add more cases here.
     (cond ((eq (cadr dcl)  'ada-use)
            (ignore-errors (eval (ada-gather-lisp-tokens dcl))))
           ((memq (cadr dcl)  '(ada-type-dcl ada-subtype-dcl))
            (ignore-errors  ;cludge for now
              (eval  (ada-gather-lisp-tokens dcl))))
            ((memq (cadr dcl) '(def-ada-fn ada-new-fn))
             (push (ncons (list 'fn (cadddr dcl)))
                   ada-info))
            ((memq (cadr dcl) '(ada-new-package def-ada-package
                               def-ada-cliche  def-ada-generic))
             'think)))
   dcl))
(def-rule <basic-dcl>
  (one-of <init-dcl> <constant-dcl> <variable-dcl>
          <use> <with> <type-dcl> <subtype-dcl>
          <new-package> <new-fn> <def-form>
          <role> <dcl-annotation>) )
(def-rule <dcl-annotation>
  (/{ <dcl> /, <role-name-string> /})
  (/( kbe-/#-comment-macro /( <dcl> /)
      (computation fix-role-name-string <role-name-string>) /)))
(def-rule <new-package>
  (PACKAGE <symbol> IS NEW <instance>)
  (/( ADA-NEW-PACKAGE <symbol> <instance> /)))
(def-rule <new-fn>
  ((one-of FUNCTION PROCEDURE) <fn-spec> IS NEW <instance>)
  (/( ADA-NEW-FN <fn-spec> <instance> /)))
(def-rule <instance> (<generic-name> <generic-args>))
(def-rule <generic-args>
  (optional /( (zero-or-more <generic-arg> /,) /))
  (zero-or-more <generic-arg>))
(def-rule <generic-arg> (one-of <fn-name> <type-name>) )
(def-rule <variable-dcl>
  (<symbol> /: <type>)
  (/( ADA-DCL <symbol> <type> /)))
(def-rule <init-dcl>
  (<symbol> /: <type> /:= <exp>)
  (/( ADA-INIT-DCL <symbol> <type> <exp> /)))
(def-rule <constant-dcl>
  (<symbol> /: CONSTANT <type> /:= <exp>)
  (/( ADA-CONSTANT-DCL <symbol> <type> <exp> /)))
(def-rule <use>
  (USE <contour-names+>)
  (/( ADA-USE <contour-names+> /)))
(def-rule <with>
  (WITH (one-of FUNCTION PROCEDURE) <fn-spec> (optional IS <box>))
  (/( ADA-WITH <fn-spec> (optional <box>) /)))
;;; PDF page 193

(def-rule <box> <>)
;;; PDF page 194

(def-rule <type-dcl>
 (TYPE <symbol> IS <type>)
 (/( ADA-TYPE-DCL <symbol> <type> /)))
(def-rule <subtype-dcl>
 (SUBTYPE <symbol> IS <type>)
 (/( ADA-SUBTYPE-DCL <symbol> <type> /)))
(def-rule <type>
 (one-of <type-name> <enumeration> <exception> <record>
         <string-type> <range> <role>))
(def-rule <enumeration>
 (/(  (one-or-more <symbol> /,) /))
  (/( ADA-ENUMERATION (one-or-more <symbol>) /)))
(def-rule <exception>
  (EXCEPTION (optional RENAMES <exception-name>) )
  (/( ADA-EXCEPTION (optional <exception-name>) /)))
(def-rule <record>
  (RECORD <fields+> END RECORD)
  (/( ADA-REC <fields+> /)))
(def-rule <fields+>
  (one-or-more <field> /; T)
  (one-or-more <field>))
(def-rule <field>
  (one-of <true-field> <role>))
(def-rule <true-field>
  (<symbol> /: <type>)
  (/( <symbol> <type> /)))
(def-rule <string-type>
  (STRING (optional <range-exp>))
  (/( ADA-STRING (optional <range-exp>) /)))
(def-rule <range>
  ((optional <type-name>) range <range-exp>)
  (/( ADA-range (computation stuff-or-nil <type-name>) <range-exp> /)))
(defun stuff-or-nil (stuff)
  (or stuff '(nil)))
(def-rule <range-exp> (one-of <range-pair> <box>))
(def-rule <range-pair>
  (/( <integer>1 |..| (one-of <integer>2 <role>) /))
  (<integer>1 (one-of <integer>2 <role>)))
;;; PDF page 195

(def-rule <stmnts+>
 (one-or-more <stmnt> /; T)
 (one-or-more <stmnt>))
(def-rule <stmnt>
 <basic-stmnt>
 (computation mark-check-point <basic-stmnt>))
(defun mark-check-point (stuff)
  (setq *last-form-parsed* stuff))
(def-rule <basic-stmnt>
  (one-of <begin> <loop> <while> <cond> <return> <exit>
         <raise> <setq>  <fn-call> <role> <stmnt-annotation>) )
(def-rule <stmnt-annotation>
  (/{ <stmnt> /, <role-name-string> /})
  (/( kbe-/#-comment-macro /( <stmnt> /)
      (computation fix-role-name-string <role-name-string>) /)))
(def-rule <begin>
  (BEGIN <stmnts+> <exception-clauses> END)
  (/( ADA-BEGIN /( <stmnts+> /) /( <exception-clauses> /) /)))
(def-rule <loop>
  (LOOP <stmnts+> <exception-clauses> END LOOP)
  (/( ADA-LOOP /( <stmnts+> /) /( <exception-clauses> /) /)))
(def-rule <while>
  (WHILE <exp> LOOP <stmnts+> <exception-clauses> END LOOP)
  (/( ADA-WHILE <exp> /( <stmnts+> /) /( <exception-clauses> /) /)))
(def-rule <cond>
  (<if> (zero-or-more <elsif>) (optional <else>) END IF)
  (/( ADA-COND <if> (zero-or-more <elsif>) (optional <else>) /)))
(def-rule <if>
  (IF <exp> THEN <stmnts+>)
  (/( <exp> <stmnts+> /)))
(def-rule <elsif>
  (ELSIF <exp> THEN <stmnts+>)      ;note ELSIF not ELSEIF
  (/( <exp> <stmnts+> /)))
(def-rule <else>
  (ELSE <stmnts+>)
  (/( /( ENUM-LIT TRUE /) <stmnts+> /)))
(def-rule <return>
  (RETURN (optional <exp>))
  (/( ada-return (optional <exp>) /)))
(def-rule <exit>
  (EXIT (optional WHEN <exp>))
  (/( ADA-EXIT (optional <exp>) /)))
(def-rule <raise>
  (RAISE (optional <exception-name>) )
  (/( ADA-RAISE (optional <exception-name>) /)))
(def-rule <setq>
  (<variable> /:= <exp>)
  (/( ADA-SETQ <variable> <exp> /)))
(def-rule <exception-clauses>
  (optional EXCEPTION (one-or-more <exception-clause>) )
  (zero-or-more <exception-clause>) )
;;; PDF page 196

(def-rule <exception-clause>
 (WHEN (one-or-more <exception-name> /|)  => <stmnts+>)
 (/( /( (one-or-more <exception-name>) /) <stmnts+> /)))
;;; PDF page 197

(def-rule <exps>
 (optional /( (one-or-more <exp> /,) /))
 (zero-or-more <exp>))
(def-rule <exps+>
 (/( (one-or-more <exp> /,) /))
 (one-or-more <exp>))
(def-rule <basic-exp-unit>
 (one-of <literal> <parenthesised-exp>
         <attribute> <record-constructor> <field-selector>
         <fn-call> <role> <converter> <variable>))
(def-rule <parenthesised-exp>
  (/( <exp> /))
 <exp>)
(def-rule <role> (one-of <complex-role> <simple-role> <exp-annotation>) )
(def-rule <simple-role>
  (/{ <role-name-string> /})
  (/( kbe-/#-comment-macro /( /)
      (computation fix-role-name-string <role-name-string>) /)))
(def-rule <maybe-modified-exps+>
  (/( (one-or-more <maybe-modified-exp> /,) /))
  (one-or-more <maybe-modified-exp>) )
(def-rule <maybe-modified-exp>
  (one-of <modified-exp> <exp>))
(def-rule <modified-exp>
  (/{ <exp> /, <modified> /})
  (&in-out <exp>))
(defun <modified> ()
  (let ((token (pop *token-in-stream*)))
    (if (and (stringp token)
             (or (string-equal token "modified")
                 (string-equal token " modified")))
        (ncons token))))
(def-rule <complex-role>
  (/{ <role-name-string> /} <maybe-modified-exps+>)
  (/( kbe-/#-comment-macro /( /( |...| <maybe-modified-exps+> /) /)
     (computation fix-role-name-string <role-name-string>) /)))
(defun fix-role-name-string (stuff)
  (ncons (string->kbe-comment-name (car stuff))))
(def-rule <exp-annotation>
  (/{ <exp> /, <role-name-string> /})
  (/( kbe-/#-comment-macro /( <exp> /)
      (computation fix-role-name-string <role-name-string>) /)))
(def-rule <fn-call>
  (<fn-name> <exps>)
  (/( ADA-FN <fn-name> <exps> /)))
(def-rule <converter>
  (<type> /( <exp> /))
  (/( ADA-CONVERT <type> <exp> /)))
(def-rule <attribute>
  (<type-name> /' <attribute-name>)
  (/( ADA-ATTRIBUTE <type-name> <attribute-name> /)))
(def-rule <record-constructor>
;;; PDF page 198

  (<type> /' <exps+>)
  (/( ADA-CONSTRUCT <type> <exps+> /)))
(def-rule <field-selector>
  ((one-of <role> <variable>) /. <field-name>)
  (/( ADA-SELECT (one-of <role> <variable>) <field-name> /)))
;;; PDF page 199

(defun <exp> ()
 (if (could-possibly-be-op-exp)  (<op-exp>) (<basic-exp-unit>)))
;having this loop ahead speeds up the parser by a factor of more than 2.
(defun could-possibly-be-op-exp ()
  (let ((paren-count 0))
    (dolist (token *token-in-stream*)
      (caseq token
        ((or xor and = //= <= >= <>  + - & not abs * // mod rem **)
         (return T))
        (/; (return nil))
        (/( (incf paren-count))
        (/) (if (plusp paren-count) (decf paren-count) (return nil)))
    (/, (if (not (plusp paren-count)) (return nil)))))))
;this takes care of operator precedence and the like
(defun <op-exp> ()
  (let (op-stack arg-stack (last-kind 'op))
    (loop do
      (let (((thing kind op-priority) (ada-get-exp-thing) )
            (op-type 'binary))
        (caseq kind
          (arg
            (if (eq last-kind 'arg) (return nil))
            (setq last-kind 'arg)
            (push thing arg-stack))
          (op
            (cond ((eq last-kind 'op)
                   (if (not (memq thing '(not abs + -)))
                       (return nil))
                   (setq op-priority 4)
                    (setq op-type 'unary))
                  (T (if (memq thing '(not abs)) (return nil))
                     (setq last-kind 'op)))
            (if (loop do
                  (if (null op-stack) (return))
                  (let (((top-op top-type top-priority) (car op-stack)))
                    (if (> op-priority top-priority) (return nil))
                    (cond ((eq top-type 'binary)
                           (if (not (> (length arg-stack) 1)) (return t))
                           (let ((second-arg (pop arg-stack))
                                 (first-arg (pop arg-stack)))
                             (push  `(/( ada-fn /( ,top-op /)
                                         ,@first-arg ,@second-arg /))
                                   arg-stack)))
                          (T (if (null arg-stack) (return nil))
                             (push  `(/( ada-fn /( ,top-op /)
                                         ,@(pop arg-stack) /))
                                   arg-stack)))
                    (pop op-stack)))
                (return nil))
            (push (list thing op-type op-priority) op-stack)))
        (when (eq thing  '*done*)
          (if (not (and (= (length arg-stack)  1)
                         (= (length op-stack) 1)))
              (return nil)
              (return (car arg-stack))))))))
;;; PDF page 200

(defun ada-get-exp-thing ()
 (let* ((saved-token-in-stream *token-in-stream*)
        (thing (<operator>)))
   (cond (thing
           `(,(car thing) op
             ,(caseq (car thing)
                ((OR XOR AND) 1)
                ((= //= <= >= < >) 2)
                ((+ - &) 3)
                ((not abs) 4)
                ((* // mod rem) 5)
                ((**) 6))))
          (T (setq *token-in-stream* saved-token-in-stream)
             (setq thing (<basic-exp-unit>))
             (cond (thing `(,thing arg))
                   (T (setq *token-in-stream* saved-token-in-stream)
                      '(*done* op 0)))))))
;the operator priorities are used to disambiguate the nesting
;order of expressions (higher numbers bind more tightly, and things
;of equal priority are done leftmost first).  Parentheses can also be used.
;;; PDF page 201

(def-rule <literal>
 (one-of <integer-literal> <string-literal> <enumeration-literal>))
(def-rule <fn-name>
 (<qualifications> <fn-name-root>)
 (/( <fn-name-root> <qualifications>  /)))
(def-rule <type-name>
 (<qualifications> <type-name-root>)
 (/( <type-name-root>  <qualifications> /)))
(def-rule <package-name>
  (<qualifications> <package-name-root>)
  (/( <package-name-root> <qualifications> /)))
(def-rule <generic-name>
  (<qualifications> <generic-name-root>)
  (/( <generic-name-root> <qualifications> /)))
(def-rule <exception-name>
  (<qualifications> <exception-name-root>)
  (/( <exception-name-root> <qualifications> /)))
(def-rule <enumeration-element-name>
  (<qualifications> <enumeration-element-root>)
  (<enumeration-element-root> <qualifications>))
(def-rule <contour-name>
  (<qualifications> <contour-name-root>)
  (/( <contour-name-root> <qualifications> /)))
(def-rule <contour-names+>
  (one-or-more <contour-name> /,)
  (one-or-more <contour-name>) )
(def-rule <contour-name-root>
  (one-of <package-name-root> <fn-name-root> <generic-name-root>) )
;;; PDF page 202

(def-rule <qualifications>
  (zero-or-more <contour-name-root> /. T)
  (computation reverse <contour-name-root>) )
;IN REVERSE ORDER eg: FOO.BAR.BAZ.  (BAZ BAR FOO)
;note that these literals depend on things having been tokenized right
;in the first place.
(def-rule <integer-literal> <integer> (/( int-lit <integer> /)))
(def-rule <string-literal> <string> (/( str-lit <string> /)))
(def-rule <enumeration-literal>
  <enumeration-element-name>
  (/( ENUM-LIT <enumeration-element-name> /)))
;the tokenizer has to work hard for this one.
(def-rule <role-name-string> <string>)
(defun <operator-string> ()
  (car (member (pop *token-in-stream*)
                '( "<=" "+")))) ;etc
