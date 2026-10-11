;;; -*- Mode: LISP; Package: KBE; Base: 10; Fonts: MEDFNB -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; PDF pages 3-29 (printed source pages 1-27).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; PSEUDO: Support for the Pseudo-English grammar.

;;;; Declarations, etc.

(defvar *cliches*  nil "A list of all active cliches.")
(defvar *roles*    nil "A list of all active roles.")

;;;; Dictionary Support

;;; The dictionary, which is held by the variable *DICTIONARY-LIST*
;;; has the following format, which is relied upon only by these
;;; dictionary utilities.
;;;
;;;   <dictionary-list> ::= (<entry> . <dictionary-list>)
;;;   <entry> ::= (<word> <part-of-speech> <syntax> <description>)

(defvar *dictionary-list* '() "A list of dictionary entries")

;;;  Externally, the functions ADD-DICTIONARY-ENTRY and PRINT-DICTIONARY
;;;  should be the only things that get used.

;;;  (ADD-DICTIONARY-ENTRY word part-of-speech syntax description)
;;;    Adds a word to the dictionary. Warns if a definition already
;;;    exists which conflicts with the definition being added.

(defun add-dictionary-entry (word part-of-speech syntax description)
  (let ((new-entry (list word part-of-speech syntax description)))
    (let ((probe (mem #'(lambda (x y)
                          (and (eq (car  x) (car  y))
                               (eq (cadr x) (cadr y))))
                      new-entry
                      *dictionary-list*)))
      (cond (probe
             (let ((entry (if probe (car probe))))
               (if (not (equal (cddr entry) (cddr new-entry)))
                   (format error-output
                           "~&Dictionary entry for ~A ~A being redefined."
                           (string-downcase part-of-speech) word))
               (setf (cddr entry) (cddr new-entry))))
            (t
             (push new-entry *dictionary-list*))))))

;;;  (PRINT-DICTIONARY [[style] stream])
;;;    STYLE may be one of:
;;;      :RAW           -- Output the dictionary with no sorting
;;;                         or grouping.
;;;      :SORT          -- Output the dictionary sorted alphabetically.
;;;      :GROUP         -- Output the dictionary grouped by part of speech
;;;                         but with no sorting done within groups.
;;;      :GRUP-AND-SORT -- Output the dictionary grouped by part of speech
;;;                         and sorted alphabetically within each group.
;;;    Probably a STYLE of :RAW is fastest and :GROUP looks nicest.
;;;    STREAM controls where the output goes.

(defun print-dictionary (&optional
                          (style ':raw)
                          (stream standard-output))
  (cond ((or (not style) (eq style ':raw))
         (format stream "~&KBE Dictionary")
         (print-dictionary-internal *dictionary-list* stream))
        ((eq style ':sort)
         (format stream "~&KBE Dictionary (Sorted)")
         (print-dictionary-internal
           (sortcar (copylist *dictionary-list*) #'string-lessp)
           stream))
        ((eq style ':group)
         (let ((parts-of-speech '()) (pl nil))
           (dolist (entry *dictionary-list*)
             (if (not (memq (cadr entry) parts-of-speech))
                 (push (cadr entry) parts-of-speech)))
           (dolist (entry *dictionary-list*)
             (push entry (get (locf pl) (cadr entry))))
           (format stream "~&KBE Dictionary (Grouped)")
           (do ((l pl (cddr l))
                (result nil (cons (cons (car l) (cadr l)) result)))
               ((null l)
                (dolist (l (sortcar result #'string-lessp))
                  (format stream "~&~/|~2%~A~2%"
                          (string-capitalize-words (car l)))
                  (print-dictionary-internal (cdr l) stream))))))
        ((eq style ':group-and-sort)
         (let ((parts-of-speech '()) (pl nil))
           (dolist (entry *dictionary-list*)
             (if (not (memq (cadr entry) parts-of-speech))
                 (push (cadr entry) parts-of-speech)))
           (dolist (entry *dictionary-list*)
             (push entry (get (locf pl) (cadr entry))))
           (format stream "~&KBE Dictionary (Grouped and Sorted)")
           (do ((l pl (cddr l))
                (result nil (cons (cons (car l) (cadr l)) result)))
               ((null l)
                (dolist (l (sortcar result #'string-lessp))
                  (format stream "~&~/|~2%~A~2%"
                          (string-capitalize-words (car l)))
                  (print-dictionary-internal
                    (sortcar (cdr l) #'string-lessp)
                    stream))))))
        (t (ferror "~S is not a valid KBE dictionary style." style)))
  (format stream "~%~/|~2%Cliche~2%")
  (dolist (cliche *cliches*)
    (let ((comments (get cliche 'cliche-comments)))
      (let ((parts  (get comments'roles)))
        (format stream "~2%~A, cliche" cliche)
        (when parts
          (format stream "~% Role~P: ~A"
                  (length parts)
                  (phrase-append "and" (mapcar #'string-downcase parts))))
        (format stream ".~% ~A~&"
                (let ((data (or (get comments 'detail)
                                (get comments 'main))))
                  (cond ((null data) "No available documentation.")
                        ((symbolp data)
                         (string-capitalize-words data stream))
                        ((stringp data) data)
                        (t
                         (auto-fill-string
                           (remove-misplaced-whitespace
                             (with-output-to-string (str)
                               (dolist (text data)
                                 (format str "~A "
                                         (cond ((stringp text) text)
                                               ((get comments text))
                                               (t (string-downcase
                                                    text)))))))
                           nil " "))))))))
  (values))

(defun print-dictionary-internal (entry-list stream)
  (dolist (entry entry-list)
    (format stream "~&~A, ~A~@[~% Syntax: /"~A/"~]~% ~A"
            (car entry)
            (string-downcase (cadr entry))
            (caddr entry)
            (auto-fill-string (cadddr entry) nil " "))))

;;;; Utilities

;;; Type predicates

(defun setup-plural (name)
  (putprop (intern (string-pluralize name) *kbe-package*)
           name
           'singular))

;for the convenience of other places in the KBE that call this function,
;it works harder than just doing a get.  It may still easily fail
;however.

(defun singular-form (name)
  (cond ((get name 'singular))
        ((< (string-length name) 2) nil)
        (t (let (singular-string)
             (prog ()
               (when (> (string-length name) 3)
                 (setq singular-string
                       (string-append
                         (substring name 0 (- (string-length name) 3))
                         "Y"))
                 (if (string-equal (string name)
                                   (string-pluralize singular-string))
                     (return nil)))
               (setq singular-string
                     (substring name 0 (1- (string-length name))))
               (if (string-equal (string name)
                                 (string-pluralize singular-string))
                   (return nil))
               (setq singular-string nil))
             (if singular-string
                 (intern singular-string (symbol-package name)))))))

(defvar *completion-table* nil "List of completions")

(defun *define-completion (name)
  (if (symbolp name) (putprop name t 'in-dictionary))
  (setq *completion-table*
        (add-to-completion-table *completion-table*
                                 (cond ((stringp name) name)
                                       (t (string-downcase name))))))

(defun in-dictionary? (name) (get name 'in-dictionary))

(defun *define-cliche  (name)
  (when (not (memq name *cliches*))
    (push name *cliches*)
    (*define-completion name)
    (setup-plural name)))

(defun *define-role    (name)
  (when (not (memq name *roles*))
    (push name *roles*)
    (*define-completion name)
    (setup-plural name)))

(defun *undefine-cliche  (name) (setq *cliches*  (delq name *cliches*)))
(defun *undefine-role    (name) (setq *roles*    (delq name *roles*)))

(defun cliche-name?  (x) (mem #'string-equal x *cliches*))
(defun role-name?    (x) (or (mem #'string-equal x *roles*)
                             (and (symbolp x) (not (in-dictionary? x))
                                  (not (memq x '(nil s))))))
(defun program-name? (x) (fboundp x))

(defun in-chars? (thing chars)
  (cond ((fixp thing) (memq thing chars))
        ((not (or (symbolp thing) (stringp thing))) nil)
        ((> (string-length thing) 1) nil)
        (t (in-chars? (character thing) chars))))

(defvar comment-chars         '(#/;)          "list of comment chars")
(defvar sentence-terminators '(#/. #/? #/!) "list of sentence terminators")
(defvar word-separators       '(#/,)          "list of word separators")
(defvar whitespace-chars      '(#\space
                                #\tab
                                #\return)    "list of whitespace chars")
(defvar code-char-matches     '((#/[ #/])
                                (#/{ #/})
                                (#/( #/))
                                (#/" #/"))    "code-bracket pairs")
(defvar code-chars (append '(#/' #/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
                           (mapcar #'car code-char-matches))
  "list of code-bracket chars")

;This is prepared to be called with either something in balanced
;() {} or "" which is read in literally as a string;
;or something in balanced [] in which case the stuff between them
;is read in as a string; or any other thing (eg a number or symbol)
;in which case the the lisp reader is used to read the thing.

;note that [] is now the way to indicate an arbitrary code string
;instead of "".  The unparser has been changed to reflect that.

;also, $ is now used to quote an arbitrary lisp thing rather than
;ordinary quote.  The unparser has been changed to reflect this too.

(defun read-code (c)
  (cond ((assq (character c) code-char-matches)
         (let ((string (read-matched-code (tyi))))
           (if (= c #/[)
               (setq string
                     (substring string 1 (1- (string-length string)))))
           `',string))
        (T (let ((thing (read-hacking-dots)))
             `',(let ((base 10.) (*nopoint nil))
                  (format nil "~S" thing))))))

(defun read-matched-code (c)
  (let* ((c1 (character c))
         (c2 (cadr (assq c1 code-char-matches))))
    (do ((chars (ncons c1) (cons c chars))
         (c (tyi) (tyi))
         (level 0))
        ((and (= level 0) (= c c2))
         (apply #'string-append (nreverse (cons c chars))))
      (cond ((= c c1) (incf level))
            ((= c c2) (decf level))))))

(defun comment-char?         (thing) (in-chars? thing comment-chars))
(defun sentence-terminator? (thing)
  (or (typep thing 'sys:error)
      (in-chars? thing sentence-terminators)))
(defun word-separator?       (thing) (in-chars? thing word-separators))
(defun whitespace-char?      (thing) (in-chars? thing whitespace-chars))
(defun code-char?            (thing) (in-chars? thing code-chars))

(defun punctuation? (thing)
  (or (word-separator? thing)
      (sentence-terminator? thing)))

(defun word-break? (thing)
  (or (comment-char? thing)
      (punctuation? thing)
      (whitespace-char? thing)))

;;;; Tokenization

;;; To get a fresh tokenization environment locally, use the
;;; special form:
;;;  (WITH-FRESH-TOKEN-BUFFER ...forms...)
;;;
;;; To peek at a token, use PEEK-TOKEN. To read a token, use
;;; READ-TOKEN. These each take an optional extra argument saying
;;; whether to force the word to be a dictionary token in spite of
;;; its case.

(defvar *token-buffer* nil "Buffer for token peeking.")

(defmacro with-fresh-token-buffer (&rest body)
  `(let ((*token-buffer* nil)) ,@body))

(defun peek-token (&optional (force-keyword? nil))
  (cond (*token-buffer*
         (car *token-buffer*))
        (t
         (push (read-token-internal force-keyword?) *token-buffer*)
         (car *token-buffer*))))

(defun unread-token (token)
  (push token *token-buffer*))

(defun really-read-token (force-keyword?)
  (cond (*token-buffer* (pop *token-buffer*))
        (t (read-token-internal force-keyword?))))

(defun read-token (&optional (force-keyword? nil))
  (let ((result (really-read-token force-keyword?)))
    (cond ((sentence-terminator? result)
           (parse-error "Premature termination of input."))
          (t result))))

(defvar code-predicted nil
  "T if the next thing to parse is predicted to be code")

(defvar entered-code-environment nil
  "T if we have gotten to place in the sentence where we had code.")

(defun read-token-internal (&optional (force-keyword nil))
  (condition-case (condition)
      (let ((c (tyipeek t)))
        (cond ((comment-char? c)
               (readline) ;for effect, discard rest of line
               (read-token-internal force-keyword))
              ((punctuation? c)
               (let ((package *kbe-package*))
                 (intern (tyi))))
              ;; here is where we interface new $ to old ' read in style
              ((= c #/$) (tyi)
               (list 'quote (list 'quote (read-hacking-dots))))
              ((code-char? c)
               (read-code c))
              ((or force-keyword (and (>= c #/a) (<= c #/z)))
               (let* ((package *kbe-package*)
                      (word (scan-word)))
                 (setq code-predicted
                       (or (memq word '(as of compute))
                           ;;triggering of and like this isn't quite right.
                           (and entered-code-environment (eq word 'and))))
                 (setq entered-code-environment
                       (or entered-code-environment code-predicted))
                 word))
              ((and (>= c #/A) (<= c #/Z))
               (if code-predicted (read-code c) (list 'quote (scan-word))))
              (t
               (list 'quote (read-hacking-dots)))))
    (sys:read-end-of-file condition)))

(defun read-hacking-dots ()
  (let ((c (tyipeek)))
    (cond ((= c #/')
           (tyi) (tyipeek t) ;eat char and whitespace
           (list 'quote (read-hacking-dots)))
          ((= c #/;)
           (readline)
           (read-hacking-dots))
          ((or (and (>= c #/A) (<= c #/Z))
               (and (>= c #/a) (<= c #/z)))
           (scan-word))
          ((or (and (>= c #/0) (<= c #/9)) (= c #/.) (= c #/-))
           (read-from-string
             (with-output-to-string (str)
               (do ((dot nil)) (nil)
                 (let ((c (tyi standard-input -1)))
                   (cond ((= c #/.)
                          (when dot
                            (send standard-input ':untyi c)
                            (return))
                          (setq dot t))
                         ((minusp c) (return))
                         ((word-break? c)
                          (send standard-input ':untyi c)
                          (return)))
                   (tyo c str))))))
          (t (read-hacking-/{)))))

(defun scan-word ()
  (symbolize-scanned-word (really-scan-word)))

(defun symbolize-scanned-word (token)
  (let ((the-symbol (read-from-string token)))
    (cond ((symbolp the-symbol) the-symbol)
          (t (parse-error "Invalid word syntax: ... ~A ..." the-symbol)))))

(defun really-scan-word ()
  (with-output-to-string (str)
    (do ((c (tyi standard-input -1) (tyi standard-input -1)))
        ((cond ((minusp c) t)
               ((word-break? c) (send standard-input ':untyi c) t)))
      (tyo c str)
      (if (= c #//) (tyo (tyi standard-input) str)))))

;;; (TOKEN-STRING token)
;;;
;;;  This function returns a string which represents a rough approximation
;;;  to the input format probably used for TOKEN. It is intended only for
;;;  use with error diagnostics, so is allowed to be heuristic.

(eval-when (eval compile load) ;needed, for example, in define-preposition

    (defun token-string (token)
  (cond ((symbolp token)
         (format nil "/"~A/"" (string-downcase token)))
        (t
         (format nil "~S" (cadr token)))))

) ;nehw-lave

;;;; Driver Functions

(defun parse-command (string &rest parse-options)
  (with-input-from-string (str string)
    (lexpr-funcall #'read-command str parse-options)))

(defun read-command (&optional
                     (standard-input standard-input)
                     (parse-handler #'parse-sentence)
                     (parse-type "sentence"))
  (with-user-package
    (with-fresh-token-buffer
      (prog1 (funcall parse-handler)
             (when (not (sentence-terminator? (peek-token)))
               (parse-error
                 "Text follows logical end of ~A." parse-type))))))

;;; (PROMPT-AND-READ-COMMAND [stream])
;;;
;;;  This command is an entry point to the text reader which reads
;;;  whole sentences, interfacing appropriately to a rubout-handler
;;;  facility if one is available.

(defun prompt-and-read-command (&optional (stream standard-input)
                                &rest read-command-args)
  (cond ((send stream ':operation-handled-p ':rubout-handler)
         (read-command-prompt standard-input nil)
         (do () (nil) ;forever
           (when (eq (tyipeek) #\c-Z)
             (tyi)
             (null-sentence)      ;Allow graceful handling if possible
             (signal 'sys:abort)) ;Else simulate an Abort
           (condition-restart-case
             (return-if
               (lexpr-send-if-handles stream
                        ':rubout-handler '((:reprompt read-command-prompt)
                                           (:full-rubout t))
                      #'read-command stream read-command-args))
             ((sys:abort "Start over with typing this command.")
              (read-command-prompt standard-input nil)))))
        (t
         (lexpr-funcall #'read-command stream read-command-args))))

;;; Prompting function for the sentence reader.

(define-internal-option *command-prompt*
                      "Prompt for text input to KBE"
  ">" :sexp)

(defun read-command-prompt (stream char)
  char ;ignored
  (format stream "~&~A" *command-prompt*))

;;;; Sentence Parser

(define-condition null-sentence ())

(defun parse-sentence ()
  (peek-token t) ; Capitalization DWIM for first word
  (let ((token (peek-token)) code-predicted entered-code-environment)
    (cond ((typep token 'sys:error) nil)
          ((sentence-terminator? token)
           (null-sentence)
           (parse-error "Empty sentence parsed; ~
                         the delimiter ~A was encountered prematurely."
                       (token-string token)))
          (t
           (let ((fn (get token 'verb-semantics)))
             (cond ((not fn)
                    ;; Only if these names are not defined as verbs,
                    ;; we allow some interfaces to use these as a
                    ;; simulated end of input.
                    (if (memq token '(abort exit quit done terminate))
                        (null-sentence))
                    (parse-error "Not a verb: ~A"
                                 (token-string token)))
                   (t
                    (read-token)
                    (funcall fn))))))))

;;;; Name Parser

(defvar *token-type* nil "Keyword saying what token type to peek for.")

(defun parse-name (type)
  (declare (values token type))
  (let ((*token-type* type))       ;for the sake of an interactive reader
    (let ((token (peek-token)))
      (when (punctuation? token)
        (parse-error "Found punctuation out of place: ~A"
                     (token-string token)))
      (selectq type
        ((:STRING)
         (cond ((and (not (atom token))
                     (stringp (cadr token)))
                (values (cadr (read-token)) ':expression))
               (t (parse-error "Found ~A where a string was expected."
                               (token-string token)))))
        ((:KEYWORD)
         (cond ((symbolp token)
                (values (intern (string (read-token)) "") ':keyword))
               ((symbolp (cadr token))
                (values (intern (string (cadr (read-token))) "")
                        ':keyword))
               (t
                (parse-error "Found ~A where a keyword was expected."
                             (token-string token)))))
        ((:TOKEN)
         (cond ((symbolp token)
                (values (read-token) ':token))
               (t
                (parse-error
                  "Found ~A where a pseudo-English word was expected."
                  (token-string token)))))
        ((:TOKEN-NAME)
         (cond ((or (symbolp token)
                    (not (symbolp (cadr token))))
                (parse-error
                  "Found ~A where a name was expected."
                  (token-string token)))
               (t
                (values (intern-KBE (string (cadr (read-token))))
                      ':token))))
        ((:IDENTIFIER)
         (cond ((symbolp token)
                ;; Use default package, not KBE package!
                (values (intern (string (read-token))) ':identifier))
               ((symbolp (cadr token))
                (values (cadr (read-token)) ':identifier))
               (t
                (parse-error "Found ~A where an identifier was expected."
                             (token-string token)))))
        ((:NAME)
         (cond ((symbolp token)
                (values (read-token) ':token))
               ((symbolp (cadr token))
                (values (cadr (read-token)) ':identifier))
               (t
                (parse-error "Found ~A where a name was expected."
                             (token-string token)))))
        ((:EXPRESSION)
         (cond ((symbolp token)
                (values (intern (string (read-token))) ':expression))
               (t
                (values (cadr (read-token)) ':expression))))
        ((:CODE)
         (cond ((or (atom token)
                    (and (not (stringp (cadr token)))
                         (or (atom (cadr token))
                             (neq (car (cadr token)) 'quote))))
                (parse-error "Found ~A where a quoted expression expected."
                             (token-string token)))
               (t
                (values (cadr (read-token)) ':expression))))
        ((:ANY)
         (cond ((symbolp token)
                (values (read-token) ':token))
               ((symbolp (cadr token))
                (values (cadr (read-token)) ':identifier))
               (t
                (values (cadr (read-token)) ':expression))))
        (otherwise
         (ferror "Bad argument to PARSE-NAME: ~S" type))))))

;;;; Miscellaneous Utilities

(defun require-one-of (words &optional context)
  (cond ((let ((w (peek-token)))
           (dolist (word words)
             (if (or (eq w word)
                     (memq w (get word 'synonyms)))
                (return t))))
         (read-token))
        (t (parse-error "Missing ~A~@[ ~A~]."
                        (phrase-append "or" (mapcar #'token-string words))
                        context))))

(defmacro define-synonyms (&rest words)
  `(progn 'compile
          ,@(mapcan
              #'(lambda (a)
                  (mapcan #'(lambda (b)
                              (if (neq a b)
                                  (ncons `(push ',a
                                                (get ',b 'synonyms)))))
                          words))
              words)))

(defun parse-definition-spec (&optional (error? nil))
  (caseq (peek-token)
    ((program function package procedure)
     (let ((first-token (read-token)))
       `(name program-definition
              ,(if (not (atom (peek-token)))
                   (parse-name ':identifier))
              ,(string-downcase first-token))))
    ((cliche)
     (read-token)
     `(name cliche-definition
            ,(if (not (atom (peek-token)))
                 (parse-name ':token-name))))
    (otherwise
      (if error? (parse-error "Missing name of definition.")))))

(defun parse-variable-name ()
  `(name variable ,(parse-name ':identifier)))

;;;; Generalized References

(defun parse-reference ()
  (if (or (pronoun? (peek-token)) (eq (peek-token) 'the))
      (parse-definite-reference)
      (parse-indefinite-reference)))

;;;; Definite References

(defun parse-definite-reference ()
  (or (parse-seg-reference)
      (parse-port-reference-minus-the)
      (parse-error "Missing definite reference.")))

(defun parse-seg-reference ()
  (cond ((parse-pronoun-reference))
        (t
         (when (neq (peek-token) 'the)
           (parse-error "Missing /"the/" in definite reference."))
         (read-token)
         (parse-the-reference))))

(defun parse-toplevel-reference ()
  (let ((ref (parse-seg-reference)))
    (when (neq (car ref) 'root-reference)
      (parse-error "Not a toplevel reference: ~A" (unparse-command ref)))
    ref))

(defun parse-pronoun-reference ()
  (if (pronoun? (peek-token))
      (list (read-token))))

(defun parse-the-reference ()
  (or (parse-top-reference)
      (parse-root-or-relative-reference (parse-ordinal))))

(defun parse-top-reference ()
  (if (memq (peek-token) '(top))
      (list (read-token))))

(defun parse-root-or-relative-reference (ordinal)
  (let ((descriptors (do ((descriptor (parse-descriptor)
                                      (parse-descriptor))
                          (l '() (cons descriptor l)))
                         ((not descriptor) (reverse l)))))
    (cond ((not (memq (peek-token) '(program cliche segment
                                      function procedure package)))
           (parse-relative-reference ordinal descriptors))
          (ordinal
           (parse-error "Invalid syntax in definite reference."))
          (t
           (parse-root-reference descriptors)))))

(*define-completion 'program)
(*define-completion 'cliche)
(*define-completion 'segment)
(*define-completion 'function)
(*define-completion 'package)
(*define-completion 'procedure)

(defun parse-descriptor ()
  (let ((word (peek-token)))
    (cond ((punctuation? word) nil)
          ((not (atom word))
           (if (program-name? (cadr word))
               `(name program ,(cadr (read-token)))))
          ((cliche-name? word)
           `(name cliche ,(read-token)))
          ((role-name? word)
           `(name role ,(read-token)))
          ((descriptor? word)
           (read-token))
          (t
           (let ((singular (singular-form word)))
             (cond ((descriptor? singular)
                    (read-token)
                    (unread-token 's)
                    singular)
                   ((role-name? singular)
                    (read-token)
                    (unread-token 's)
                    `(name role ,singular))
                   ((cliche-name? singular)
                    (read-token)
                    (unread-token 's)
                    `(name cliche ,singular))))))))

(defun parse-plural ()
  (cond ((eq (peek-token) 's)
         (read-token)
         t)))

(defun add-descriptor (reference descriptor)
  (selectq (car reference)
    (root-reference `(root-reference ',(cons descriptor
                                             (cadr (cadr reference)))
                                     ,@(cddr reference)))
    (relative-reference `(relative-reference
                           ,(cadr reference)
                           ',(cons descriptor (cadr (caddr reference)))
                           ,@(cdddr reference)))
    (otherwise reference))) ;eg, pronouns

(defun parse-relative-reference (ordinal descriptors)
  (cond ((not descriptors)
         (if ordinal (unread-token
                       (intern (string-upcase ;yuck
                                 (ordinal-unparse ordinal))
                               *kbe-package*)))
         nil)
        (t
         (let* ((plural (parse-plural))
                (preposition (if (memq (peek-token) '(in of))
                                 (read-token)))
                (reference (cond (preposition
                                  (or (parse-seg-reference)
                                      (parse-error
                                        "Invalid relative reference.")))
                                 (t '(top)))))
           `(relative-reference ,ordinal
                                ',descriptors
                                ,plural
                                ',(or preposition 'in)
                                ,reference)))))

(defun parse-root-reference (descriptors)
  (let ((name
          (cond ((parse-definition-spec))
                ((eq (peek-token) 'segment)
                 `(name ,(read-token) ,(parse-name ':identifier)))
                (descriptors
                 (parse-error
                   "Root reference must name a definition or segment.")))))
    (if name `(root-reference ',descriptors ,name))))

(defun parse-port-reference-minus-the ()
  (let ((ordinal (parse-ordinal)))
    (let ((port-descriptors
            (do ((p '() (cons d p))
                 (d (parse-port-descriptor)
                    (parse-port-descriptor)))
                ((not d)
                 (if (not p)
                     (parse-error
                       "Missing descriptor in definite reference."))
                 (nreverse p)))))
      (let ((plural (parse-plural)))
        (let ((ref (cond ((eq (peek-token) 'of)
                          (read-token)
                          (parse-seg-reference))
                         (t '(top)))))
          `(port-reference ,ordinal ',port-descriptors ,plural ,ref))))))

;;;; Indefinite References

(defun parse-indefinite-reference ()
  (let ((expected-type ':any))
    (when (memq (peek-token) '(a an))
      (read-token)
      (setq expected-type ':name))
    (multiple-value-bind (token type)
        (parse-name expected-type)
      (caseq type
        ((:TOKEN :IDENTIFIER)
         `(cliche-instance
            ,(caseq type
                ((:TOKEN) `(name cliche ,token))
                ((:IDENTIFIER) `(name program ,token))
                (T (ferror "PARSE-INDEFINITE-REFERENCE lost.")))
            ,@(when (eq (peek-token) 'of)
                (read-token) ;pop the OF
                (do ((l () (cons (parse-reference) l))
                     (at-least-once? nil t))
                    ((and at-least-once?
                          (not (memq (peek-token) '(/, and))))
                     (nreverse l))
                  (when (eq (peek-token) '/,) ;maybe pop ","
                    (read-token))
                  (when (eq (peek-token) 'and)  ;maybe pop AND
                    (read-token))))))
        ((:EXPRESSION)
         `(code ',token))             ; =>    (code (quote (quote exp)))
                                      ;    or (code (quote "..."))
        (t (ferror "PARSE-INDEFINITE-REFERENCE lost."))))))

;;;; Modifier phrases

(defun parse-optional-cliche-name ()
  (if (cliche-name? (peek-token))
      `(name cliche ,(read-token))))

(defun parse-position ()
  (cond ((eq (peek-token) 'in)
         (read-token)
         (parse-definite-reference))
        ((eq (peek-token) 'here)
         (read-token)
         '(top))
        (t nil)))

(defun parse-roles-or-parameters (&optional spec)
  (selectq (cadr spec)
    (program-definition
     (parse-roles-or-parameters-internal
       '(parameter) '(parameters)
       "in a program specification"))
    (cliche-definition
     (parse-roles-or-parameters-internal
       '(role) '(roles)
       "in a cliche specification"))
    (otherwise
     (parse-roles-or-parameters-internal
       '(parameter role)
       '(parameters roles)
       "somewhere"))))

(defun parse-roles-or-parameters-internal (singular plural context)
  (cond ((eq (peek-token) 'a)
         (read-token)
         (let ((key (require-one-of singular context)))
           (cons key (list (parse-variable-name)))))
        (t
         (let ((key (require-one-of plural context)))
           (do ((p nil))
               ((let ((next (peek-token)))
                   (and (atom next) (not (memq next '(/, and)))))
                (cons key (nreverse p)))
             (push (parse-variable-name) p)
             (when (eq (peek-token) '/,)
               (read-token))
             (when (eq (peek-token) 'and)
               (read-token)))))))

;;; Ordinal Adjectives

(defvar *ordinals* () "data about ordinal adjectives")

(defmacro define-ordinal (name numeric-value)
  (check-arg numeric-value fixnump "a fixnum")
  `(*define-ordinal ',name ,numeric-value))

(defun *define-ordinal (name numeric-value)
  (check-arg numeric-value fixnump "a fixnum")
  (let ((entry (assq name *ordinals*)))
    (cond (entry (setf (cdr entry) numeric-value))
          (t (push (cons name numeric-value) *ordinals*))))
  (*define-completion name)
  (add-dictionary-entry name 'ordinal nil
                        (format nil "Selects the ~A of several of things."
                                (string-downcase name)))
  name)

(defun parse-ordinal ()
  (let ((word (peek-token)))
    (let ((val (cdr (assq word *ordinals*))))
      (if val (read-token))
      val)))

;;;; Port Descriptors

(defvar *port-descriptors* '() "List of defined port descriptors")

(defun port-descriptor? (word) (memq word *port-descriptors*))

(defmacro define-port-descriptor (name &optional documentation)
  `(progn 'compile
          (if (not (memq ',name *port-descriptors*))
              (push ',name *port-descriptors*))
          (setup-plural ',name)
          (*define-completion ',name)
          (add-dictionary-entry
            ',name 'port-descriptor nil
            ,(or documentation
                 (format nil "Used to designate ~A in a port reference."
                         (a-or-an (string-downcase name)))))
          ',name))

(defun parse-port-descriptor ()
  (let ((word (peek-token)))
    (cond ((punctuation? word) nil)
          ((port-descriptor? word) (read-token))
          ((let ((singular (singular-form word)))
             (cond (singular
                     (read-token)
                     (unread-token 's)
                     singular))))
          ((symbolp word) nil)
          (t (parse-variable-name)))))

;;;; Verbs

(eval-when (eval compile load)

    (defun construct-verb-bindings (name bvl form)
  (cond ((null bvl) form)
        (t
         (construct-verb-bindings
           name
           (cdr bvl)
           (cond ((eq (car bvl) '...) form)
                 ((symbolp (car bvl))
                  `(progn (require-one-of
                            '(,(car bvl))
                            ,(format nil "in a usage of the verb /"~A/""
                                     (string-downcase name)))
                          ,form))
                 (t
                  `(let ((,(caar bvl)
                          ,(if (cadddr (car bvl))
                               `(or ,(cadr   (car bvl))
                                    ',(cadddr (car bvl)))
                               (cadar bvl))))
                     ,form)))))))

) ;NEHW-LAVE

(defmacro define-verb-parse (name bvl documentation &body body)
  (check-arg documentation stringp
             "a documentation string for the KBE dictionary")
  (let* ((capitalized-name (string-capitalize (string-downcase name))))
    `(progn 'compile
            (defun (,name verb-semantics) ()
              ,(construct-verb-bindings name
                                        (reverse bvl)
                                        (cond ((cdr body) `(progn ,@body))
                                              (t (car body)))))
            ,@(mapcan #'(lambda (x)
                          (if (and (atom x) (neq x '...))
                              (ncons `(*define-completion ',x))))
                      bvl)
            (*define-completion ',name)
            (*define-completion ,capitalized-name) ;Allow stylized casing
            (add-dictionary-entry
              ',name 'verb
              ,(format nil "~A~{ ~A~}."
                       capitalized-name
                       (mapcar #'(lambda (x)
                                   (cond ((symbolp x) (string-downcase x))
                                         (t (format nil "<~A>"
                                                    (string-downcase
                                                      (car x))))))
                               bvl))
              ,documentation))))

(defdefmacro define-verb-unparse "KBE verb unparser" (name bvl . body)
  (let ((var (gensym)))
    `(local-declare ()
       (defun (,name unparse) (,(if bvl var 'ignore))
         (let ,(if bvl `(((nil ,@bvl) ,var)) '())
           ,@body))
       ',name)))

(defdefmacro define-verb-action "KBE verb action" (name bvl . body)
  (let ((var (gensym)))
    `(local-declare ()
       (defun (,name verb-action) (&rest ,(if bvl var 'ignore))
         (let ,(if bvl `((,bvl ,var)) '())
           ,@body))
       ',name)))

(defflavor vocabulary () ()
  (:method-combination (:case :base-flavor-last :interpret)))

(defmethod (vocabulary :otherwise :interpret) (verb &rest verb-args)
  (ferror "~S is not a verb known to ~A.~% Context: ~A"
          verb self (unparse-command (cons verb verb-args))))

(defmacro define-vocabulary (name &optional bvl &body forms)
  `(local-declare ()
     (defflavor ,name () (vocabulary))
     ,(if forms
          `(defmethod (,name :otherwise :interpret) ,bvl
             ,@forms))
     ',name))

(comment ;too bad this doesn't work.

    (defdefmacro define-additional-interpretation "KBE verb interpretation"
             (name bvl . forms)
  (let ((flavor (if (atom name) 'default-vocabulary (car name)))
        (method (if (atom name) name (cadr name)))
        (var (gensym)))
    (multiple-value-bind (arglist spread-p)
        (parse-argument-list bvl)
      `(defwhopper (,flavor :interpret) (,var ,@bvl)
         (cond ((eq ,var ',method)
                (progn ,@forms)))
         (,(if spread-p 'lexpr-continue-whopper 'continue-whopper)
          ,var ,@arglist)))))

; something like this would almost work but needs more support

    (defmacro define-additional-interpretation (name bvl &body forms)
  (let ((flavor (if (atom name) 'default-vocabulary (car name)))
        (method (if (atom name) name (cadr name))))
    (let ((proxy (symbolconc flavor '-interpretation-proxy)))
      (multiple-value-bind (arglist spread-p)
          (parse-argument-list bvl)
        `(defwhopper (,proxy ,method) (,var ,@bvl)
             (progn ,@forms)
             (,(if spread-p 'lexpr-continue-whopper 'continue-whopper)
              ,@arglist))))))

);tnemmoc

(defdefmacro define-interpretation "KBE verb interpretation"
             (name bvl . forms)
  (let ((flavor (if (atom name) 'default-vocabulary (car name)))
        (method (if (atom name) name (cadr name))))
    `(defmethod (,flavor :case :interpret ,method) ,bvl ,@forms)))

(define-vocabulary default-vocabulary)

(defprop define-verb "KBE verb" si:definition-type-name)

(defmacro define-verb (name bvl doc)
  (let* ((question? (let ((qmark (mem #'(lambda (x y)
                                          (and (atom y)
                                               (string-equal x y)))
                                      "?" bvl)))
                      (cond ((not qmark) nil)
                            (t
                             (delq (car qmark) bvl)
                             t))))
         (bvl-vars (mapcan #'(lambda (x)
                               (if (not (atom x)) (ncons (car x))))
                           bvl)))
    (make-defdefmacro-source-file-wrapper
      'define-verb
      name
      `(local-declare ()
         (define-verb-parse ,name ,bvl ,doc
           (list ',name ,@bvl-vars))
         (define-verb-unparse ,name ,bvl-vars
           (append-delimiter
             (text ,(text "~A~{~A~}"
                          (string-capitalize (string-downcase name))
                          (mapcar #'(lambda (x)
                                      (if (atom x)
                                          (text " ~A" (string-downcase x))
                                          (text "~~@[~@[ ~A~] ~~A~~]"
                                                (caddr x))))
                                  bvl))
                   ,@(mapcar #'(lambda (x data)
                                 (cond ((cadddr data)
                                        `(if (not (equal ,x ',(cadddr data)))
                                             (unparse-command ,x)))
                                       (t `(unparse-command ,x))))
                             bvl-vars
                             (mapcan #'(lambda (x) (if (not (atom x))
                                                       (ncons x)))
                                     bvl)))
             ,(if question? '#/? '#/.)))
         (define-verb-action ,name ,bvl-vars
           (send-delegate ':interpret ',name ,@bvl-vars))
         ',name))))

(defprop define-action "KBE action" si:definition-type-name)

(defmacro define-action (name bvl doc &body forms)
  (make-defdefmacro-source-file-wrapper
    'define-action
    name
    `(local-declare ()
       (define-verb ,(cond ((atom name) name)
                           (t (cadr name)))
                    ,bvl ,doc)
       ,(if forms
            `(define-interpretation ,name
                                    ,(mapcan #'(lambda (x)
                                                 (if (not (atom x))
                                                     (ncons (car x))))
                                             bvl)
               ,@forms))
       ',name)))

;;; Other Word Types
(defmacro define-part-of-speech (name pos default-doc &rest context-info)
  `(progn 'compile
          (*define-completion ',name)
          (putprop ',name t ',pos)
          (add-dictionary-entry
            ',name ',pos nil
            ,(format nil "~A~%~@[(See also ~A)~]"
                     (cond ((stringp (car context-info))
                            (pop context-info)) ;exploits evaluation order!
                           (t default-doc))
                     (if context-info
                         (phrase-append "and" (mapcar #'token-string
                                                      context-info)))))
          ',name))
(defmacro define-preposition (name &rest context-info)
  `(define-part-of-speech ,name preposition
     "Used as a relational or guide word."
     ,@context-info))
(defmacro define-pronoun (name &rest context-info)
  `(define-part-of-speech ,name pronoun
     "Used as a context-dependent definite reference."
     ,@context-info))
(defun pronoun? (word) (get word 'pronoun))
(defmacro define-article (name doc)
  `(define-part-of-speech ,name article ,doc))
(defmacro define-descriptor (name doc)
  `(progn 'compile
          (define-part-of-speech ,name descriptor ,doc)
          (setup-plural ',name)
          ',name))
(defun descriptor? (word) (get word 'descriptor))
(defmacro define-adverb (name doc)
  `(define-part-of-speech ,name adverb ,doc))

;;; Testing Utilities
(defun test-parser ()
  (catch-error-restart ((sys:abort sys:error null-sentence)
                       "Exit parser test routine.")
    (let ((*command-prompt* "^X"))
      (do ((c (prompt-and-read-command)
              (prompt-and-read-command)))
          ((not c))
        (let ((u (ignore-errors (unparse-command c))))
          (let ((package *kbe-package*))
            (let (prinlevel prinlength)
              (format t "~& ^Y ~S~@[~% ** ~A~]" c u)))))))
  (format t "~&Leaving the KBE test-parser loop.~%")
  (values))
