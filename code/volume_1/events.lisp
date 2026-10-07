;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 165-169; listing events.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; EVENT: Default event descriptions.
;;; PDF page 166
;;; (ANSWER data)
;;; This is the kind of event used for returning values.
(define-event answer (data)
  (cond ((atom data)
         (typeout (text "^Y ~S" data)))
        ((eq (car data) ':text)
         (typeout (get data ':text)))
        (t
         (typeout (let (prinlevel prinlength)
                    (text "~&The result was of type ~S.~{~% ~A:~11T ~S~}"
                          (car data) (cdr data)))))))
;;; (COMPLAINT string)
;;; This can be used in place of ANSWER to return error information.
;;; Frequently, this behaves like REMARK (unsolicited typeout) but
;;; its text is less likely to be less expected and is more important
;;; than that of a REMARK, so it should be given better placement.
(define-event complaint (text)
  (remark (text "~&Error! ~A~&" text)))
;;; PDF page 167
;;; Display Format Directives
;;; (TYPEOUT string)
;;; Requests typeout of the given string. This is typeout typically
;;; in direct response to a user query.
(defun typeout (string) (task (text "Typeout [~A]." string)))
(comment ;not needed? -kmp 4/8/84
  (define-event typeout (string)
    (format t "~&~:A~&" string))
) ;tnemmoc
;;; (REMARK string)
;;; Requests typeout of the given string. This is typically unsolicited
;;; typeout which the user may find useful, but which s/he didn't
;;; explicitly request.
(defun remark (string) (task (text "Remark [~A]." string)))
(comment ;not needed? -kmp 4/8/84
  (define-event remark (string)
    (format t "~&* ~A~&" string))
) ;tnemmoc
;;; (CLEAR-DISPLAY)
;;; If the customer has a display of some sort, suggests that it be
;;; cleared, usually in preparation for other data yet to come.
;;; This is probably a data abstraction violation since the apprentice
;;; should not make assumptions about what is in view in the first place.
(defun clear-display () (task "Clear display."))
;;; PDF page 168
;;; Beliefs
(define-event belief (belief-type belief-subject)
  (declare (values val))
  belief-type belief-subject ;ignored
  nil)
(define-event believe (belief-type belief-subject belief-val)
  (cbarf "Skip trying to assert this belief."
         "Cannot assert beliefs in this context.~% Data: ~S ~S ~S"
         belief-type belief-subject belief-val))
;;; PDF page 169
;;; KBE-specific requests
;;; (TASK string)
;;; This requests that an actor perform the action specified by
;;; the KBE command given by the STRING.
(define-event task (request)
  (cbarf "Ignore this task request."
         "Task requests aren't handled in this context:~% Request: ~A"
         request))
;;; (CODER-CONFIRMATION function-name code)
;;; Says that new code is available for the given function-name.
(define-event coder-confirmation (function-name code))
;;; (DOCUMENT-CODE function documentation)
(define-event document-code (function-name documentation))
;;; (SUGGEST-FOCUS focus-info)
;;; Suggests that the focus be changed to item designated by the
;;; given FOCUS-INFO.
(define-event suggest-focus (focus-info))
(define-event presentational-focus ()
  (declare (values focus))
  (return nil))
(define-event suggested-focus ()
  (declare (values focus))
  (return nil))
;;; (SUGGEST-CODE-HOME definition-name where)
;;; Suggests that a home be found for the given definition, NAME,
;;; in some location, WHERE. The location is in property list format
;;; with possible keys of:
;;; :BUFFER "...string name of buffer..."
;;; :FILE   "...string name of file..." ;unimplemented
;;; :NEAR   symbolic-name
;;; If more than one item is present, all are intended to be satisfied
;;; (otherwise, a user should be queried for confirmation).
(define-event suggest-code-home (definition-name where))
(define-event suggested-code-home (definition-name)
  (declare (values where))
  definition-name ;ignored
  (return nil))
(define-event associated-apprentice ()
  (declare (values apprentice-or-NIL))
  (return nil))
