;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>DICT.LISP
;;; PDF pages 30-35 (printed source pages 1-6).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; DICT: The main core of the dictionary.
;;; This is not the complete dictionary. Some additional
;;; special-purpose terms may be defined in other files.

;;; Adjective Definitions
;;; Ordinals
(define-ordinal first    1)
(define-ordinal second   2)
(define-ordinal third    3)
(define-ordinal fourth   4)
(define-ordinal fifth    5)
(define-ordinal sixth    6)
(define-ordinal seventh  7)
(define-ordinal last    -1)
;;; PDF page 32

;;; Port Descriptors
(define-port-descriptor parameter)
(define-port-descriptor argument)
(define-port-descriptor free-variable)
(define-port-descriptor value)
(define-port-descriptor side-effect)
(define-port-descriptor input)
(define-port-descriptor output)
(define-port-descriptor temporal)
(define-port-descriptor non-temporal)
(define-port-descriptor port)
;;; PDF page 33

;;; z3 Verb Definitions
(define-action typeout ((text (parse-name ':string)))
               "Requests that the given <text> be typed out."
  (format t "~&~:A~&"  text))
(define-action remark ((text (parse-name ':string)))
               "Requests that the given <text> be remarked about."
  (format t "~&* ~A~&" text))
(define-action clear (display)
               "Requests that the display area be cleared."
  (values)) ;Ignored in most contexts
(define-action how (is (cliche-or-program (parse-toplevel-reference))
                       defined ?)
               "Asks what how <cliche-or-program> is defined."
  cliche-or-program ; ignored
  nil) ;Default is to not know.
(define-action Enable ((switch (parse-name  ':keyword)) mode)
               "Enables the KBE feature named <switch>."
  (multiple-value-bind (success? comment-string)
      (set-switch switch t)
    (cond ((not success?)
           (complain "~A" comment-string))
          (t  `(:text :text ,comment-string)))))
(define-action Disable  ((switch (parse-name ':keyword)) mode)
               "Disables  the KBE feature named <switch>."
  (multiple-value-bind  (success? comment-string)
      (set-switch  switch nil)
    (cond  ((not success?)
            (complain "~A" comment-string))
           (t `(:text :text ,comment-string)))))
(*define-completion  'patches)
(define-action  Load ((thing-to-load (parse-name ':any)))
                "Loads the <thing-to-load>. Normally, this is a filename.
                 If it is the word /"patches/" then KBE patches will
                 be loaded instead."
   (cond ((stringp thing-to-load)
          (list ':text
                ':text (text "The file ~A was loaded."
                             (load thing-to-load))))
         ((eq thing-to-load 'patches)
          (multiple-value-bind (old-major old-minor)
              (si:get-system-version "KBE")
            old-major ;ignore
            (load-patches 'kbe ':silent)
            (multiple-value-bind (new-major new-minor)
                (si:get-system-version "KBE")
              new-major ;ignore
              (list ':text
                    ':text (cond ((= old-minor new-minor)
                                  "No patches were loaded.")
                                 (t
                                  (text "Patches loaded for ~A"
                                        (system-version-string
                                          "KBE"))))))))
         (t
          (complain "I don't know how to load ~A."
                    (string-downcase thing-to-load)))))
(define-action Compile ((thing-to-compile (parse-name ':any)))
                "Compiles the <thing-to-compile>, which should be a
                 filename."
;;; PDF page 34

  (cond ((stringp thing-to-compile)
         (list ':text
               ':text (text "The file ~A was compiled.~
                                      The file ~A contains the output."
                            thing-to-compile
                            (compiler:compile-file thing-to-compile))))
        (t
         (complain "I don't know how to compile ~A."
                   (string-downcase thing-to-compile)))))
;;; PDF page 35

 ;;; Misc other words     :
 ;;; Prepositions, guide words
(define-preposition in)
(define-preposition of)

 ;;; Pronouns  :
(define-pronoun it)
(define-pronoun this)
(define-pronoun that)
 ;;; Articles
(define-article a  "Designates an indefinite reference.")
(define-article an "Synonym for /"a/".")
(define-article a<n> "Synonym for /"a/" and /"an/".")
(define-synonyms a an a<n>)
(define-article the "Designates a definite reference.")
 ;;; Definite reference descriptors
(define-descriptor implemented
                    "Declares a definite reference to already exist.")
(define-descriptor unimplemented
  "Declares a definite reference to not already exist.")
(define-descriptor role
                    "Denotes the named roles of a program fragment.")
 ;;; Adverbs
(define-adverb here "In /"What needs to be done here/" means like
                      /"in the top/".")
