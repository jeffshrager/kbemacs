;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>DEMO-LIB.LISP
;;; PDF pages 243-248 (printed source pages 1-6).

;;; (c) Copyright 1984, Massachusetts Institute of Technology

;this program should be run just before a demo in order
;to set things up to work right.
(defvar original-with-open-file (fsymeval 'with-open-file))
(fset 'original-with-open-file original-with-open-file)
(defun demo-setup ()
  (declare (special *predefined-files*))
;general things
  (pkg-goto 'kbe) ;won't work in any other package.
  (setq si:gc-report-stream nil)
  (setq si:address-space-warning-low-words 2000000.)
;for lisp stuff
  (fset 'with-open-file (fsymeval 'with-one-open-file))
;for ada stuff
  (ada-init)
  (eval '(ada-fn (create_test_records)))
  (if (y-or-n-p "do you want to test implmentation of MAINTENANCE_FILES ")
      (setq *predefined-files*
            (delq (assq 'repairs *predefined-files*) *predefined-files*)))
  'done)
(defun demo-unsetup ()
; (setq si:gc-report-stream T)
  (fset 'with-open-file original-with-open-file))
(pp:define-list-print-function format (list s)
  (format s "(~I~=~^ ~:I~@_~=~^ ~:_~=~^ ~:_~
                ~@I~@{~=~^ ~:_~}~.~.)" list))

;;; PDF page 244

(DEFINE-CLICHE SIMPLE-REPORT
    (PRIMARY-ROLES (ENUMERATOR PRINT-ITEM EPILOG)
    DESCRIBED-ROLES  (FILE-NAME TITLE ENUMERATOR
                     COLUMN-HEADINGS PRINT-ITEM EPILOG)
    COMMENT  "prints a report of {the input structure of the enumerator}"
     CONSTRAINTS
      ((DEFAULT {the file-name} "report.txt")
       (DERIVED {the line-limit}
                (- 65
                   (SIZE-IN-LINES {the print-item})
                   (SIZE-IN-LINES {the epilog})))))
  (WITH-OPEN-FILE (REPORT {the file-name} ':OUT)
    (LET* ((DATE (TIME:PRINT-CURRENT-TIME NIL))
           (LINE 66)
           (PAGE 0)
           (TITLE {the title})
           (DATA {the input structure of the enumerator}))
      (FORMAT REPORT "~5%~66:@<~A~>~2%~66:@<~A~>~%" TITLE DATE)
      (LOOP DO
        (IF ({the empty-test of the enumerator} DATA) (RETURN))
        (WHEN (> LINE {the line-limit})
          (SETQ PAGE (+ PAGE 1))
          (FORMAT REPORT "~/|~%Page:~3D~50:@<~A~>~17A~2%" PAGE TITLE DATE)
          (SETQ LINE 3)
          ({the column-headings} {REPORT, modified} {LINE, modified}))
        ({the print-item}
         {REPORT, modified}
         {LINE, modified}
         ({the element-accessor of the enumerator} DATA))
        (SETQ DATA ({the step of the enumerator} DATA)))
      ({the epilog} {REPORT, modified}))))
;;; PDF page 245

;note cannot use the name number-of-lines due to internal artifacts of KBE
(DEFINE-CLICHE PRINT-OUT
    (PRIMARY-ROLES (FORMAT-STRING ITEM)
     DESCRIBED-ROLES (FORMAT-STRING ITEM)
     COMMENT "prints out {the item}"
     CONSTRAINTS
      ((DEFAULT {the format-string} "~%~A")
       (DERIVED {the size-in-lines} (SIZE-IN-LINES {the format-string}))))
   (FORMAT REPORT {the format-string} {the input item})
   (SETQ LINE (+ LINE {the size-in-lines})))
;note if we count columns here this becomes a loop fragment and
; implement cannot hack it.
(DEFINE-CLICHE TABULARIZED-PRINT-OUT
    (PRIMARY-ROLES (FORMAT-STRING ITEM)
     DESCRIBED-ROLES (FORMAT-STRING ITEM NUMBER-OF-COLUMNS )
     COMMENT "prints out {the item} in columns"
     CONSTRAINTS
      ((DEFAULT {the format-string} "~15A")
       (DERIVED {the maximum-charpos}
                (- 75 (SIZE-IN-CHARACTERS {the format-string})))))
  (WHEN (> (CHARPOS REPORT) {the maximum-charpos})
    (FORMAT REPORT "~&")
    (SETQ LINE (+ LINE 1)))
  (FORMAT REPORT {the format-string} {the input item}))
;coder gets confused when depending-on is around +
(DEFINE-CLICHE COUNT
    (PRIMARY-ROLES (ITEM)
     DESCRIBED-ROLES (ITEM)
     COMMENT "accumulates a count of {the item}")
  (LET* ((COUNT 0))
    (LOOP DO
      (SETQ COUNT (+ COUNT {1, depending on the input item})))
    COUNT))
 ;note this reads in as the following old form.
 ; (+ COUNT (DEPENDING-ON {the input object} 1))
(comment
 ;this only works in loops. In order to get a final value out
 ;you would have to use a slightly different cliche.
    (DEFINE-CLICHE PREVIOUS-VALUE
     (PRIMARY-ROLES (ITEM ITEM-BEFORE-FIRST-ITEM)
     DESCRIBED-ROLES  (ITEM ITEM-BEFORE-FIRST-ITEM)
     COMMENT  "keeps track of the previous value of {the item}")
   (LET* ((PREVIOUS {the item-before-first-item}))
     (LOOP DO
      {PREVIOUS,  the output previous}
       (SETQ PREVIOUS {the input item}))))
   )
 ;a very interesting bug happens if instead of using the
 ;following you you use the cliche previous-value and set
 ;;; init to (NCONS NIL) and then try to use the value of
;this NCONS in the body of a loop.  The trouble is that
 ;the NCONS gets analyzed into just one of the ssr's and
 ;you cannot use it in the others.  This is yet another
 ;problem with over analysis.
;also if you make there be a.place for an action in this it
;ends up being a filter, and the code chokes for some
 ;reason I never figured out.
;;; PDF page 246

(DEFINE-CLICHE LEADING-BOUNDARY-DETECTOR
    (PRIMARY-ROLES (ITEM BOUNDARY-TEST)
     DESCRIBED-ROLES (ITEM BOUNDARY-TEST)
     COMMENT "locates the beginning of each subgroup in {the item}")
  (LET* ( (DATUM)
         (UNIQUE-DATUM (NCONS NIL))
         (PRIOR-DATUM UNIQUE-DATUM) )
    (LOOP DO
      (SETQ DATUM {the input item})
      {(OR (EQ PRIOR-DATUM UNIQUE-DATUM)
           ({the boundary-test} PRIOR-DATUM DATUM) ),
       the output boundary-indicator}
      (SETQ PRIOR-DATUM DATUM) )))
;;; PDF page 247

;helping fn for lisp demo.
(defun display-report ()
  (with-open-file (file "report.txt" ':in)
    (do ((char (tyi file nil) (tyi file nil))
         (newlines 1))
        (nil)
      (do () ((not (eq char #\newline)))
        (incf newlines)
        (setq char (tyi file nil)))
      (cond ((= newlines 1) (terpri))
            ((= newlines 2) (terpri) (terpri))
            ((> newlines 2)
             (format T "~%<~D blank lines>~%" (1- newlines))))
      (setq newlines 0)
      (if (null char) (return))
      (tyo char)))
  (values))
;this stuff supports charpos for the lisp demo.
;this is supported in later lispm systems but not in the current one.
;;; (WITH-OPEN-FILES (spec1 spec2 spec3 ...) . body)
;;; acts like
;;; (WITH-OPEN-FILE spec1
;;;  (WITH-OPEN-FILE spec2
;;;   (WITH-OPEN-FILE spec3
;;;     . body)))
;;; except that it does additional work to assure that the
;;; given file is open in a mode which :READ-CURSORPOS
;;; and :SET-CURSORPOS messages work in a limited way on the
;;; stream (so that FORMAT's ~T can work).
;;; Also, this file defines (CHARPOS stream).
(defun charpos (stream)
  (values (send stream ':read-cursorpos ':character)))
(defmacro with-open-files (bindings &body forms)
  (cond ((null bindings) `(progn ,@forms))
        (t
         (let ((reversed-bindings (reverse bindings)))
           (do ((f `(with-one-open-file ,(car reversed-bindings) ,@forms)
                   `(with-one-open-file ,(car b) ,f))
                (b (cdr reversed-bindings) (cdr b)))
               ((null b) f))))))
(defmacro with-one-open-file (spec &body forms)
  (let ((var1 (gensym)))
    `(original-with-open-file (,var1 ,@(cdr spec))
       (let ((,(car spec) (make-instance 'stream-with-charpos
                                       ':indirect-stream ,var1)))
         ,@forms))))
;;; PDF page 248
(defflavor stream-with-charpos ((indirect-stream nil) (charpos 0)) ()
  (:initable-instance-variables indirect-stream))
(defmethod (stream-with-charpos :read-cursorpos) (&optional units)
  (check-arg units (eq units ':character) "the keyword :CHARACTER")
  (values charpos nil)) ;ugh, no second value
(defmethod (stream-with-charpos :set-cursorpos) (x y &optional units)
  x y ;ignored
  (check-arg units (eq units ':character) "the keyword :CHARACTER")
  (when (> x charpos)
    (dotimes (i (- x charpos))
      (send self ':tyo #\space))))
(defmethod (stream-with-charpos :fresh-line) ()
  (unless (zerop charpos)
    (send self ':tyo #\newline)))
(defmethod (stream-with-charpos :tyo) (char)
  (cond ((= char #\newline) (setq charpos 0))
        ((= char #\tab)
         (setq charpos (+ charpos (- 8 (\ charpos 8)))))
        (t (incf charpos)))
  (send indirect-stream ':tyo char))
(defmethod (stream-with-charpos :operation-handled-p) (op)
  (or (memq op (send self ':which-operations))
      (send indirect-stream ':operation-handled-p op)))
(defmethod (stream-with-charpos :unclaimed-message) (op &rest args)
  (selectq op
    ((:line-out :string-out)
     (stream-default-handler self op (car args) (cdr args)))
    (otherwise (lexpr-send indirect-stream op args))))
