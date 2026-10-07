;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>GRAM.LISP
;;; PDF pages 36-47 (printed source pages 1-12).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

; :This describes the grammer of the command language used with the KBE.
; :It shows both what the surface form is, and what it parses as.
; for  example consider the following rule:
; <if-then-else> =: if <exp> then <statementi>; {else <statement2>;}
;                => (cond (<exp> <statementi>) {(T <statement2>)})
; :This rule could be used to specify what an if-then-else statement looks
; :like and how it is parsed into an internal representation (Lisp). The
;  language used to specify the grammar is BNF like. It uses the following
; ;constructs
; :=:  specifies what a non-terminal expands into (ie a grammer rule)
; ;=>  specifies the parse result corresponding to a grammar rule
; <name>  this is a non-terminal grammer symbol. When a non-terminal appears
;         in a parse-result it means that the corresponding parse should be
;         included at this point.  Numerical suffixes can be used on
;         non-terminal names in order to make the correspondance between
;         rule and parse clear.
; foo   anything else corresponds to terminal symbols in the rule or result
; +1  OR means that one of two things can occur.
; {}  means that something is optional (it may occur 0 or 1 times)
; {}1  means that this must occur exactly once (used if there is a set of                               é
;      alternatives inside).
; {}*  means that this can occur 0 or more times.
; {}+  means that this must occur 1 or more times.

;               :
;;; PDF page 37

; ;the top level node in the grammer specifies the commands which can be
; sused to request actions from the system. It is intended that this
; :list will expand. Also note that the verbs make heavy use of noise
; ;words for euphony.
; <command> =: <display> | <move-to> | <clear-display> | <describe>
;             <what-needs-to-be-done>  | <use-language> | <implement> !
; f            <reimplement>  | <make-definition> | <reinitialize>
;             <add-parameter>  | <add-role> | <remove> | <share> |!
;             <analyze>  | <eval> | <compute> | <code-up> | <document> |
;              <explain> | <locate>
; smug.         (MIR  Jen ee
; ;:     => ; ignored just signals end of sentance.
; <display> =: Display <definite-reference> <end>
; cs         => (DISPLAY <definite-reference>)
; <move-to> =: Move to <definite-reference> <end>
;           => (MOVE-TO <definite-reference>)
; <document> =: Document <definite-reference> <end>
;            => (DOCUMENT <def inite-reference>)
; <explain> =: Explain <definite-reference> <end>
;           => (EXPLAIN <definite-reference>)
; <clear-display> =: Clear display <end>
;                 => (CLEAR-DISPLAY)
; ;(This is not meaningful in the ZWEL interface mode.)
; <reinitialize> =: Reinitialize <end>
;                => (REINITIALIZE)
; <describe> =: Describe <definite-reference> <end>
;            => (describe <definite-reference>)
; <what-needs-to-be-done> =: What needs to be
;                              done {in <seg-reference> | here | } <end>
;                         a> (WHAT-NEEDS-TO-BE-DONE
;                                {<seg-reference> | (TOP) | })    :
;  If HERE is used, <seg-reference> becomes (TOP).
;  If HERE and IN <seg-reference> are omitted, so is arg to result.
; <use-language> =: Use language <keyword> <end>
;               => (USE-LANGUAGE (QUOTE <keyword>))
;  ;Note that the language name is passed in quoted.
; <keyword> =: <string> | <STRING>
;          => a symbol on the keyword package with the given print name.
; <implement> =: Implement <seg-reference> as <reference> <end>
;            =>  (IMPLEMENT <seg-reference> <reference>)
;  sDemands that <seg-reference> not be implemented yet. Forces the adjective
;  ;UNIMPLEMENTED to appear in the reference.
; <reimplement> =: Reimplement <seg-reference> as <reference> <end>
;               => (REIMPLEMENT <seg-reference> <reference>)
;  :Demands that <seg-reference> already be implemented.  Forces the adjective
;  ;IMPLEMENTED to appear in the reference.
;;; PDF page 38

; <make-definition> =: Define a
;                     {<program-def inition-name>
;                      {{with a parameter <variable-name>} |!
;                       {with parameters
;                             {{and} <variable-name> {,}}+}}} !
;                     {<cliche-definition-name>
;                      {{with a role <variable-name>} |
;                       {with roles
;                             {{and} <variable-name> {,}}+}}}1
;                     <end>
;                  => (MAKE-DEFINITION {<cliche-name> | NIL}
;                                      {<program-definition-name> |
;                                       <cliche-def inition-name>}1
;     :                                . {<variable-name>}*)
; ;Note that here the program/cliche name cannot already be defined.
; <add-parameter> =: Add a parameter <variable-name>
;                   {to <seg-reference>} <end>
;                => (ADD-PARAMETER <variable-name> <seg-reference>)
; ;Adding a parameter is the only kind of adding which works now.
; sOthers may follow. The <seg-reference> must refer to a program
; sdefinition. If it is omitted it defaults to "to the top".
; <add-role> =: Add a role <variable-name>
;              {to <seg-reference>} <end>
;           =>  (ADD-ROLE <variable-name> <seg-reference>)
; ;Adding a role to a define-cliche is the only other kind of adding
; swhich works now. The <seg-reference> must refer to a define-cliche.
; sIf it is omitted it defaults to "to the top".
; <remove> =: Remove <definite-reference> <end>
;          => (REMOVE <definite-reference>)
; ;this isn't currently used and probably doesn't work well.
; ;not implemented at the current time.
; <share> =: Share <definite-reference>
;                  {{,} {and} <definite-reference>}+ <end>
;         e> (SHARE . {<definite-reference>}+)
; <analyze> =: Analyze <code> <end>
;           => <code>
; ;analyzes the code supplied.
; <code-up> =: Code <reference> <end>
;           => (CODE-UP <reference>)
; <compute> =: Compute <reference> <end>
;            => (COMPUTE <reference>)
; ;shows you what a reference command-evals to.
; <locate> =: Locate
;              {<program-definition-name> |! <cliche-definition-name>}1
;              {{,} <path>} <end>
;           => (LOCATE <program-def inition-name> {<path>})
; ;says where the virtual cursor should be.
; <path> =: ( {<number>}* )
;        => ( {<number>}* )
; ;a path into an expression, O-indexed.
; seg,  () is toplevel.                                                  :
;      (1 3 4) is the 4th thing in the 3rd thing in the 1st thing.
; <eval> =: Eval '<lisp-expr> <end>
;        => (EVAL (QUOTE <lisp-expr>))
; ;LISP EVALS the thing so that you can see the value.
;;; PDF page 39

; :The heart of the language is reference expressions.
; <reference> =: <definite-reference> ! <indefinite-reference>
;            => <definite-reference> ! <indefinite-reference>
; ;First we have indefinite noun phrases. These create new pieces of
; ;plan rather than referring to existing pieces of plan. Note that
; ;there is a very great reduction in what you can say here compared to
; ;the past. The only thing you can create is a segment.         :
; <indefinite-reference> =: <code> | <cliche-instance>
; swe work very hard to make it easy for the user to type code.
; ;basically he can just type anything he wants that is code,
;     we try to figure it out.  There are also two escapes.
; :first you can type $ (dollar sign) followed by any lisp
; :sexpr. This forces the code to be lisp code no matter what the
; scurrent language is. Second, you can type {...] in which case everything
; sbetween the [] is interpreted as a string which should be parsed
; ;in the current language.
; <code> =: $ <lisp-expr> | [<current-language-expr>] !
;           <naked-current-language-expr>
;        => (CODE (QUOTE {(QUOTE <lisp-expr>) |!
;                         "<current-language-expr>" |
; t                        "<naked-current-language-expr>"}1))
; ;note that in the above we make several key assumptions in order to
; ;be able to handle the <naked-current-language-expr> and to
; :differentiate it from other things.
; :first it must be a single piece. ie
;  a number.
; ya  string.
;  a symbol.
;  a sequence of characters surrounded by matching () or {}.
;  a lisp expression that can be read with a single call on read.
; snote that if it is a symbol there could be ambiguities as to whether
; sit was a keyword or a program name or something.  To prevent this,
; tit must be in upper case, and the preceding keyword must be AS, OF,
; ;or COMPUTE.
; <cliche-instance> =: {a | an} {<cliche-name> | <program-name>}1
;                               {of {{and} <reference> {,}}+}
;                   => (CLICHE-INSTANCE {<cliche-name> 1 <program-name>}1
;                                       . {<reference>t)
; :Note that the A/AN is optional.  Also, you can use commas and "and" in
; ;the list of role fillers if you want.
;;; PDF page 40

; peeeeo-
; :The most complex form in the command Janguage-is the definite
; sreference expression. They make it possible to identify any segment
; ;or port in a plan. Note however, that the segments are the key
; ;thing. Port  references are only used as output by commands that
; sdescribe things. Also good reference expressions are only available
; ;for segments that play some role in a program.
; <definite-reference> =: <seg-reference> | <port-reference>
;                      => <seg-reference> | <port-reference>
; <seg-reference> =: <pronoun-reference> | <the-reference>
;                 => <pronoun-reference> |! <the-reference>
; <pronoun-reference> =: <it> | <this> | <that>
;                     => <it> | <this> | <that>
; pedex es At
;      => (it)     :
; "it" refers to the segment which is both the "conversational"
; sand "presentational" reference. When "this" and "that" designate
; :the same reference, so does "it". At any other time, "it" is ambiguous.
; <that> =: that
;        => (that)
; "that" refers to the segment most recently referred to in conversation.
; ;The apprentice notifies the interface when the meaning of this
;  reference changes.
; <this> =: this
;        => (this)
; "this" refers to the segment most recently referred to in presentation,
; ;The interface notifies the apprentice when the meaning of this
;  reference changes.
;;; PDF page 41

; <the-reference> =: <top> ! <root-reference> | <relative-reference>
;                => <top> | <root-reference> | <relative-reference>
; <top> =: the top
;      => (TOP)
; :This refers to the current program or cliche definition.
; ;Its value is computed from the active meaning of "this".
; <root-reference> =: the {<descriptor>}* {<segment-name> !
;                                         <program-definition-name>  |!
; q                                        <cliche-definition-name>}1
;                 => (ROOT-REFERENCE (QUOTE ({<descr iptor>}*))
; ;:                                  {<segment-name>  |
;                                     <program-definition-name>  |
;                          5          <cliche-def init ion-name>}1)
; <relative-reference> =: the {<ordinal>} {<descriptor>}+ {<plural>}
;                             {of <seg-reference> ! in <seg-reference>}
;                      => (RELATIVE-REFERENCE
;                           <ordinal> (QUOTE ({<descriptor>}+)) <plural>
; ;:                         (QUOTE {OF | IN}1) <seg-reference>)
; splurality is usually specified by making the descriptor plural.  If
; seither <ordinal> or <plural> is left out in the expression, then it
; sis specified as NIL in the parse.  If neither an "of" or "in" phrase
; sis specified, it defaults to "in the top".
; sNote that the verbs <implement> and
; <reimplement> force the adjectives unimplemented and implemented
; srespectively to appear in their reference pharses.
; <port-reference> =: the {<ordinal>} {<port-descriptor>}+ {<plural>}
;                         {of <seg-reference>}
;                _ => (PORT-REFERENCE
;                       <ordinal> (QUOTE ({<port-descriptor>}+)) <plural>
;                       <seg-reference>)
; ;plurality is usually specified by making the last port-descriptor
; ;plural.  If either <ordinal> or <plural> is left out in the
; sexpression, then it is specified as NIL in the parse.  Tf-anot*
; ;phrase is not specified, it defaults to "of the top".  Note that you
; scannot tel] whether you have a relative-ref or a rel-port-ref until
; ;you see whether you get a descriptor or a port-descriptor.
;;; PDF page 42

; ;In the above,
; <descriptor> =: <program-name> | <cliche-name> | <role-name> |
;                implemented | unimplemented | role | mark
;             => <program-name> | <cliche-name> | <role-name> !
;                IMPLEMENTED | UNIMPLEMENTED
; ;A segment can be described either in terms of what program it calls,
; :what cliche it is an instance of, what role it plays in a plan,
; sor whether it has been implemented yet. Note
; ;that you cannot have a program, cliche, or role called implemented,
; sor unimplemented. Note that MARK is implemented as a special role
; :name, and is treated as such by the parser and command evaluater.
; <port-descriptor> =: <variable-name> |! parameter ! argument !
;                     free-variable  !| value | side-effect | input 1
;                      output  | temporal ! non-temporal ! port
;                  => <variable-name>  | PARAMETER  | ARGUMENT !
;                      FREE-VARIABLE | VALUE | SIDE-EFFECT | INPUT
;                     OUTPUT  | TEMPORAL ! NON-TEMPORAL ! PORT
; :Note that a variable name can be used as an adjective describing a
; sfree-variable port.  This is the only place a variable-name can appear.
; <plural> =: §
; =!        => T
; :This can be expressed as a separate adjective (an isolated "s") to
; sindicate that the prior word is plural.  However, it is usually
; ;specified by making that word itself be plural.  Note however, that
; sthis can only be done if that word is lower case.  This is handled
; ;special in the parsing of <definite-ref> as will be discussed below.
; <ordinal> =: first ! second | third ! fourth | fifth | sixth ! last
;           => 1     re       13      14       {5      16      eet
; sThis is used to identify segments based on their position in the
; scode, and to identify the arguments to a funcall by their order.
;;; PDF page 43

; :One basic underlying concept is that of a name. There are many
; :different kinds of names that the user can typé.
; <number> =: an integer in base 10 (with optional decimal point)    :
;         =>  a lisp integer.
; <STRING> =: an uppercase string.  (No completion applied.)
;          => a symbol (corresponding to the string) interned in the package
;             in effect when the user was doing his type in. You can
;             just read it.
; :This refers to an identifier: either a program name or a variable name.
; <string>  =: a lowercase string of characters.  (Note that in general lower
;             case strings are subject to completion by the input reader.)
;           => the corresponding symbol for a cliche, role name, or segment
;             name.  Note that these are all symbols with upper case
;             pnames in the KBE package

; <cliche-name> ®=: <string>
;               => (NAME CLICHE <string>)
; ;The name of a cliche.  Completion is done based on the list *CLICHES*.
; <role-name> =: <string>
;             => (NAME ROLE <string>)
; :The name of a role in a cliche.  Completion is done based on the list
; s*roles*.  In order to avoid confusion, it is required that a role
; snever have the same name as a cliche.
; <program-name> =: <STRING>
;                => (NAME PROGRAM <STRING>)
; ;The name of a program.
; <variable-name> =; <STRING>
;                 => (NAME VARIABLE <STRING>)
; ;Any name.  You have to disambiguate between program names and var
; ;names from the gramatical context.
; <program-definition-name> =: program {<STRING>}
;                           => (NAME PROGRAM-DEFINITION {<STRING> -| NIL})
; :This is used to specify a program definition being edited.
; :The string must correspond to some program in a buffer that the KBE
; :knows about.  A missing <STRING> (ie NIL) means the current top program
; <cliche-definition-name> =: cliche {<STRING>}
;                         => (NAME CLICHE-DEFINITION {<STRING>  | NIL})
;  :This is used to specify a cliche definition being edited. The string must
;  :correspond to some cliche in a buffer that the KBE knows about.
;  +A missing <string> (ie NIL) means the current top cliche.
; <segment-name> =: segment <string>
;               => (NAME SEGMENT <string>)
;  :This kind of name only exists for robustness so that it is possible to
;  screate a definite reference to any arbitrary segment. The string must
;  sbe one of the segment names on the list *DB-SEGS*.
;  :Note that NAME is a fexpr. All of the other  functions in the
;  ;parse results are ordinary exprs.
;;; PDF page 44

;  This takes a command and-converts it into a string which would
; shave parsed into it.  Several of the commands result in the printing of
; ;such strings.
(defun sexp->string (sexp)
  (let ((base 10.)
        (*nopoint nil))
    (with-user-package (let (prinlevel prinlength) (text "~S" sexp)))))
(defun unparse-command (command &aux fn)
  (cond ((null command) nil)
        ((symbolp command) (string-downcase command) )
        ((stringp command) (text "[~A]" command) )
        ((atom command)
         (barf "UNPARSE-COMMAND: Invalid KBE command?~% Data: ~S"
               command) )
        ((setq fn (get (car command) 'unparse))
         ;;; Try to standardize the numeric format a bit.
         ;;; Too bad packages are not so easy... -kmp
         (let ((base 10.) (*nopoint nil))
           (funcall fn command) ))
        ;; Put these after the previous clause to make sure they can
        ;;; be overridden.
        ((eq (car command) 'quote)
         (string-append #/$ (sexp->string (cadr command) )))
        (T
         (barf "UNPARSE-COMMAND: Invalid KBE command?~% Data: ~S"
               command) )))
(defvar *alphanumeric* (string-append *alphabet* "0123456789"))
(defvar *quote-chars* "'")
(defvar *toplevel-chars* (string-append *alphanumeric*
                                         *quote-chars*))
(defun append-delimiter-if  (text) (if text (append-delimiter text)))
(defun append-delimiter  (string &optional (char #/.))
   (text "~A~:[ ~;~]~C"
        string
         (OK-to-append-delimiter-directly? string)
        char))
(defun OK-to-append-delimiter-directly? (string)
   (or (zerop (string-length string)) ;pathological
       (let ((last-char (aref string (1- (string-length string)))))
         (or (= last-char #/))
             ; This next is heuristic, but loses only on symbols
             ; containing only / and | and ending in / such as ///
             (= last-char #/|)
             (= last-char #/")
             (let* ((p1 (or (string-reverse-search-not-set
                              *toplevel-chars* string)
                            -1))
                    (p2 (string-search-not-set *quote-chars* string
                                               (1+ p1))))
               (and (or (minusp p1)
                        (whitespace-char? (aref string p1)))
                    p2
                    (string-search-char (aref string p2)
                                        *alphabet*)))))))
 ;Each little unparse fn returns its result without blanks around it.
;;; PDF page 45

;;; Non-Verbs
(defun (it unparse) (ignore)
  "it")
(defun (this unparse) (ignore)
  "this")
(defun (that unparse) (ignore)
  "that")
(defun (here unparse) (ignore)
  "here")
(defun (top unparse) (ignore)
  "the top")
;;; PDF page 46

(defun (root-reference unparse) (command)
  (let* (((nil (nil dscs) name) command)
         (dscs-string (dsc-unparse dscs nil)))
    (text "the~@[ ~A~] ~A"
          dscs-string
          (unparse-command name))))
(defun (relative-reference unparse) (command)
  (reference-unparse command) )
(defun (port-reference unparse) (command)
  (let* (((a b c d e) command))
    (reference-unparse (list a b c d ''of e))))
(defun reference-unparse (command)
  (let* (((nil ordinal (nil dscs) plural (nil of-in) reference) command)
         (ordinal-string (ordinal-unparse ordinal))
         (dscs-string (dsc-unparse dscs plural)))
    (let ((relation (text "~A ~A"
                          (string-downcase of-in)
                          (unparse-command reference) )))
      (if (equal relation "in the top") (setq relation nil))
      (text "the~@[ ~A~] ~A~@[ ~A~]"
            ordinal-string dscs-string
            relation))))

(defun ordinal-unparse (ordinal)
  (if ordinal
      (caseq ordinal
        (-1 "last") (1 "first")  (2 "second") (3 "third")
        (4 "fourth")  (5 "fifth") (6 "sixth") (7 "seventh"))))
(defun dsc-unparse (dscs plural)
  (if dscs
      (let* ((strings  (mapcar #'dsc-unparse1 dscs)))
        (if plural
             (let ((last (last strings)))
               (if (not (< #.(1- #/a) (aref (car last) 0) #.(1+ #/z)))
                   (rplacd last (ncons "s"))
                   (rplaca last (string-pluralize (car last))))))
        (text  "~{~A~^ ~}" strings))))
(defun dsc-unparse1  (dsc)
  (if (memq  dsc '(parameter argument free-variable value side-effect input
                   output temporal non-temporal port
                   implemented unimplemented role))
       (string-downcase dsc)
       (unparse-command dsc)))
;;; PDF page 47

(defun (code unparse) (command)
  (let* (((nil (nil code)) command))
    (if (stringp code)
        (text "[~A]" code)
        (let (prinlevel prinlength)  (text "$~S" (cadr code))))))
(defun (cliche-instance unparse)  (command)
  (let* (((nil name  . refs) command)
         (name-string  (unparse-command name))
         (ref-strings  (mapcar #'unparse-command refs))
         (refs-string  (caseq (length ref-strings)
                         (0 nil)
                         (1 (car ref-strings))
                         (2 (text "~{~A and ~A~}" ref-strings))
                         (T (text "~{~#[~1;and ~]~A~*, ~}" ref-strings)))))
     (text "~A ~A~@[ of ~A~]"
           (select-a-or-an name-string) name-string refs-string)))
(defun (name unparse) (command)
   (let* (((nil type name type-name) command) )
     (caseq type
       ((cliche role) (string-downcase name))
       ((program variable) (with-user-package (text "~S" name)))
       (program-definition
         (let ((descriptor (or type-name "program")))
           (cond (name (with-user-package (text "~A ~S"
                                                descriptor name)))
                 (t descriptor))))
       (cliche-definition
         (cond (name (with-kbe-package (text "cliche ~S" name)))
               (t "cliche")))
       (segment (string-append "segment " (string-downcase name) ))
       (T (barf "bad args to NAME ~S" command)))))
