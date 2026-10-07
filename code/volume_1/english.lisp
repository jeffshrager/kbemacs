;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 111-114; listing english.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; ENGLISH: Utilities for manipulating strings containing English text.

;;; This library is part of the explanation facility originally
;;; written by Scott Cyphers. For more info on that, see comments
;;; in the EXPLAIN module.

;;; (AUTO-FILL-STRING text-string [line-length] [fill-prefix] [whitespace])
;;;
;;;  Returns a version of TEXT-STRING which has been filled to the given
;;;  LINE-LENGTH. FILL-PREFIX is used as a leader on any extra lines
;;;  produced by filling. WHITESPACE can be a list of characters to
;;;  be treated as whitespace.

(defun auto-fill-string (text-string
                         &optional line-length fill-prefix whitespace)
  (with-output-to-string (string-stream)
    (auto-fill-stream string-stream
                      text-string
                      line-length
                      fill-prefix
                      whitespace)))

;;; (AUTO-FILL-STREAM stream text-string
;;;    [line-length] [fill-prefix] [whitespace])
;;;
;;;  Fills TEXT-STRING to a given stream.
;;;
;;;  STREAM is expected to handle string output operations and
;;;   a :READ-CURSORPOS message.
;;;
;;;  LINE-LENGTH defaults to 60. and specifies the target length of
;;;   the filled lines.
;;;
;;;  FILL-PREFIX, if supplied, is used to pad after any carriage returns
;;;   output by filling.
;;;
;;;  WHITESPACE defaults to the value of *AUTO-FILL-CHARS*, and specifies
;;;   which characters are to be treated as whitespace.

(defvar *auto-fill-chars* '(#\return #\tab #\space))

(defun auto-fill-stream (stream text-string
                         &optional line-length fill-prefix whitespace)
  ;; Default our args (allow NIL to mean not-supplied)
  (setq line-length (or line-length 60.))
  (setq whitespace (or whitespace *auto-fill-chars*))
  ;; Main algorithm
  (do ((cursor 0 new-cursor)
       (new-cursor))
      ((not cursor))
    (setq new-cursor (string-search-set whitespace text-string cursor))
    (let ((token (substring text-string cursor new-cursor))
          (pos   (send stream ':read-cursorpos)))
      (cond ((zerop pos))
            ((> (+ pos                    ; current pos
                   1                      ; the space we might output
                   (string-length token)) ; the chars we might output
                line-length)
             (format stream "~%~@[~A~]" fill-prefix))
            (t
             (format stream " ")))
      (format stream "~A" token)
      (when new-cursor
        (setq new-cursor
              (string-search-not-set whitespace
                                     text-string
                                     new-cursor))))))

;;;; Phrase Manipulation

;;; (PHRASE-APPEND conjunction phrase-list)
;;;
;;;  Appends some number of text phrases (strings) using the given
;;;  conjunction (if more than one phrase given) and appropriate
;;;  numbers of commas (if more than two phrases given).
;;;
;;;  eg,
;;;     (phrase-append "and" '("a"))             => "a"
;;;     (phrase-append "and" '("a" "b"))         => "a and b"
;;;     (phrase-append "and" '("a" "b" "c"))     => "a, b, and c"
;;;
;;;  If any of the phrases contains a "," we use a ";" instead to
;;;  help avoid confusion.
;;;
;;;  eg,
;;;     (phrase-append "or" '("a, b, and c" "d" "e"))
;;;        => "a, b, and c; d; or e"

(defun phrase-append (conjunction phrase-list)
  (let ((reversed (reverse phrase-list))
        (comma (cond ((some phrase-list #'(lambda (phrase)
                                            (string-search "," phrase)))
                      "; ")
                     (t ", "))))
    (selectq (length reversed)
      ((0) "-??-")
      ((1) (car reversed))
      ((2) (format nil "~A ~A ~A"
                   (cadr reversed) conjunction (car reversed)))
      (otherwise
           (let ((phrase nil))
             (push (pop reversed) phrase)
             (push (format nil "~A~A " comma conjunction) phrase)
             (push (pop reversed) phrase)
             (push comma phrase)
             (do ()
                 ((null reversed)
                  (apply 'string-append phrase))
               (push (pop reversed) phrase)
               (if reversed (push comma phrase))))))))

;;; (A-OR-AN noun-string)
;;;
;;;  Prepends the appropriate one of either "a " or "an " to
;;;  the given noun-string.

(defun a-or-an (noun-string)
  (format nil "~A ~A"
          (select-a-or-an noun-string)
          noun-string))

;;; (SELECT-A-OR-AN noun-string)
;;;
;;;  Selects the appropriate one of either "a" or "an" for the
;;;  given noun-string. Tries to hack as many screw cases as it
;;;  can get in a general way...
;;;
;;;  eg,
;;;     (SELECT-A-OR-AN "foo")                   => "a"
;;;     (SELECT-A-OR-AN "euphemism")             => "a"
;;;     (SELECT-A-OR-AN "example")               => "an"
;;;     (SELECT-A-OR-AN "unicorn")               => "a"
;;;     (SELECT-A-OR-AN "unimportant detail")    => "an"
;;;     (SELECT-A-OR-AN "unique item")           => "a"
;;;
;;;  but,
;;;     (SELECT-A-OR-AN "unidentified loss")     => "a"

(defvar *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defun select-a-or-an (string)
  (setq string (string string))
  (let ((len (string-length string))
        (idx (string-search-not-set *alphabet* string)))
    (cond ((zerop len) "a") ;random
          ((= len 1)
           (cond ((memq (char-upcase (aref string 0))
                        '(#/A #/E #/F #/H #/I #/L #/M #/N
                          #/O #/R #/S #/X))
                  "an")
                 (t "a")))
          (idx (select-a-or-an (substring string 0 idx)))
          ((or (%string-equal "EU" 0 string 0 2)
               (and (> len 4)
                    (%string-equal "UNI" 0 string 0 3)
                    (or (memq (char-upcase (aref string 3))
                              '(#/A #/E #/I #/O #/U #/Y))
                        (memq (char-upcase (aref string 4))
                              '(#/A #/E #/I #/O #/U #/Y)))))
           "a")
          ((and (> len 2)
                (= (char-upcase (aref string 0)) #/U)
                (= (char-upcase (aref string 2)) #/E))
           "a")
          ((memq (char-upcase (aref string 0))
                 '(#/A #/E #/I #/O #/U))
           "an")
          (t "a"))))

(defun select-is-or-are (symbol)
  (or (ignore-errors
        (if (member (aref (string symbol) (1- (string-length symbol)))
                    '(#/s #/S))
            "are" "is"))
      "is//are"))
