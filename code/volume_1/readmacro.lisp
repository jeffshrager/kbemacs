;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 192-193; listing readmacro.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; Readmacro Characters
;;; The following are used to implement a syntactically sugared list
;;; construction operation a la plasma. This form is the dual of
;;; backquote.
;;; The characters [ ] and ! !! have special meaning in this context.
;;; For example, [1 2 !'(3 4) !!'(5) 6 !'(7 8 9)]
;;; evaluates to (1 2 3 4 5 6 7 8 9).
;;; The only difference between ! and !! is that ! uses NCONC
;;; and !! uses APPEND. there is no difference between ! and
;;; !! if they precede the last element in a [] list.
;;; The read macros on these characters transform the input
;;; into functional form. eg, the above example reads in as:
;;; (/[ 1 2 |!| '(3 4) |!!| '(5) 6 |!| '(7 8 9)).
;;; Note that ! and !! work only when reading in the KBE package
;;; because they are not readmacros.
;;; Someday, if we flush the [...] syntax, we may need to retain
;;; this readmacro for things like LOAD-DUMP. In that case, the number
;;; 200. should become simply 2, since 2-lists are the largest that
;;; occur in plan dumps.
;;; PDF page 192
(defun KBE-/[-readmacro (list-so-far stream)
  list-so-far ;ignored
  (do ((l (ncons '/[) (cons item l))
       (item (read stream) (read stream))
       (i 200. (1- i)))
      ((eq item '/]) (nreverse l))
    (if (zerop i)
        (barf "No /"']/" seen scanning a [...] expression.~% ~S"
              (reverse l)))))
(set-syntax-macro-char #/[ #'KBE-/[-readmacro)
; KMP 24-Jul-82 2:54am
; The following was wrong because si:single means intern on the current
; package. We need to have the ] come back always in the KBE package so
; that the [ reader will match it regardless of the binding of PACKAGE.
;(set-syntax-from-description #/] 'si:single)
; Here's the fix:
(defun KBE-/]-readmacro (list-so-far stream)
  list-so-far stream ;ignored
  '/]) ;not
(set-syntax-macro-char #/] #'KBE-/]-readmacro)
;;; PDF page 193
;;; This does the work of translating the construction to ordinary LISP
;;; code. eg, from the example above, you'd get back:
;;; (LIST* 1 2 (NCONC '(3 4) (APPEND '(5) (LIST* 6 '(7 8 9)))))
(defun (/[ macro) (body)
  (prog (args items code fn)
        (setq args (cdr body))
     L  (setq fn (cdr (or (assq (car args) '((|!| . nconc)
                                            (|!!| . append)))
                         '(nil . list*))))
     M  (cond ((memq (car args) '(|!| |!!|))
               (setq args (cdr args))
               (go M)))
        (cond (args
               (setq items (cons (cons fn (car args)) items)
                     args (cdr args))
               (go L)))
        (cond ((null items)
               (return (displace body ''nil))))
        (cond ((eq (caar items) 'list*)
               (setq fn 'list))
              ((setq fn (or (caadr items) 'list*)))) ;cute eh?
        (setq code (list (cdar items)))
     P  (setq items (cdr items))
        (cond ((null items)
               (return (displace body (cons fn code)))))
        (cond ((or (eq (caar items) fn)
                   (and (eq (caar items) 'list*)
                        (eq fn 'list)))
               (setq code (cons (cdar items) code)))
              ((setq code (list (cdar items) (cons fn code))
                     fn (caar items))))
        (go P)))
;;; This sets things up for the text editor so that it can do
;;; more or less the right thing with [...] expressions.
(set-editor-list-syntax-from-character #/[ #/()
(set-editor-list-syntax-from-character #/] #/))
;;; This sets things up so that [...] expressions print out
;;; the way they were typed in.
(pp:define-list-print-function /[ (list s)
  (format s "[~@l~{~#~^ ~@_~}~.]" (cdr list)))
