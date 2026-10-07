;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 182-184; listing complete.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; COMPLETE: Abstractions for computing the completion of a given
;;;           string from a table of possible completions.
;;; Probably the LispM provides a more efficient table lookup
;;; abstraction but we don't know what it is yet. This stuff will do
;;; for now...
;;; PDF page 182
;;; (ADD-TO-COMPLETION-TABLE completion-table item)
;;; Adds a new item to an already-existing completion table.
(defun add-to-completion-table (completion-table new-entry)
  (let ((new-string (string new-entry)))
    (let ((alphabetic-case-affects-string-comparison t))
      (cond ((null completion-table) (list new-string))
            ((alphalessp new-string (car completion-table))
             (cons new-string completion-table))
            ((string-equal new-string (car completion-table))
             completion-table)
            (t
             (do ((l completion-table (cdr l))
                  (old-l nil l))
                 ((or (null l) (alphalessp new-string (car l)))
                  (setf (cdr old-l) (cons new-string l)))
               (if (string-equal new-string (car l))
                   (return)))
             completion-table)))))
;;; (MAKE-COMPLETION-TABLE initial-contents)
;;; Coerces initial-contents from an arbitrary list (of chars, strings,
;;; or symbols) into a well-formed completion-table for use in these
;;; other functions.
;;; eg, (MAKE-COMPLETION-TABLE '(foo "bar" #/Q))
;;; makes a table containing the entries "FOO", "bar", and "Q"
(defun make-completion-table (&optional list-of-strings)
  (let ((alphabetic-case-affects-string-comparison t))
    (sort (mapcar #'string list-of-strings) #'alphalessp)))
;;; (GET-COMPLETION-FROM-TABLE string completion-table)
;;; Returns a string which is the characters which can be used as
;;; a completion for string in completion-table.
;;; eg, (SETQ TABLE (MAKE-COMPLETION-TABLE '("FOOBAR" "FOOBAZ")))
;;; (GET-COMPLETION-FROM-TABLE "FOO" TABLE) => "BA"
(defun get-completion-from-table (string completion-table)
  (*catch 'completion
    (do ((c completion-table (cdr c)))
        ((null c) "")
      (if (match-completion (car c) string)
          (let ((first-match (car c)))
;;; PDF page 183
            (do () (nil)
              (if (or (null (cdr c))
                      (not (match-completion (cadr c) string)))
                  (*throw 'completion
                    (get-completion-from-range string first-match (car c))))
              (pop c)))))))
;;; (MATCH-COMPLETION pattern string)
;;; Returns true if string is a substring of pattern beginning
;;; with the first character.
;;; (MATCH-COMPLETION "foobar" "foo") => T
;;; (MATCH-COMPLETION "foo" "foo") => T
;;; (MATCH-COMPLETION "foobar" "bar") => NIL
;;; (MATCH-COMPLETION "foobar" "abc") => NIL
;;; (MATCH-COMPLETION "f" "foo") => NIL
(defun match-completion (pattern string)
  (let ((alphabetic-case-affects-string-comparison t))
    (%string-equal pattern 0 string 0 (string-length string))))
;;; (GET-COMPLETION-FROM-RANGE seed lower upper)
;;; Returns a string which is the set of characters which are common
;;; to the string lower and upper in order and immediately following
;;; seed, which is assumed to be common to both lower and upper.
;;; (GET-COMPLETION-FROM-RANGE "foo" "foobar" "foobaz") => "ba"
;;; (GET-COMPLETION-FROM-RANGE "foo" "foo" "foobar") => ""
;;; (GET-COMPLETION-FROM-RANGE "foo" "foo" "foo") => ""
(defun get-completion-from-range (seed lower upper)
  (let ((seed-length (string-length seed)))
    (substring lower
               seed-length
               (do ((i seed-length (1+ i))
                    (end (min (string-length lower) (string-length upper))))
                   ((= i end) i)
                 (if (not (= (aref lower i) (aref upper i)))
                     (return i))))))
;;; (POSSIBLE-COMPLETIONS string table)
(defun possible-completions (string table)
  (do ((l table (cdr l)))
      ((null l) nil)
    (cond ((match-completion (car l) string)
           (return (do ((l l (cdr l))
                        (r nil (cons (car l) r)))
                       ((or (null l)
                            (not (match-completion (car l) string)))
                        (nreverse r))))))))
;;; PDF page 184
;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; End:;
