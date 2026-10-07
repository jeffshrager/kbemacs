;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>EXPLAIN.LISP.1015, 13 December 1984 14:25:49
;;; Knowledge-Based_Editor_Jan-1985_volume_4.pdf, PDF pages 223-231.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; EXPLAIN: Module for generation of automatic explanations of code.

;;; This code was originally written by Scott Cyphers in Spring, 1982.
;;; See MIT-AI-WP-237, "Automatic Program Description" for details.
;;; On 6-Dec-82, the file OZ:PS:<KBE.EXPLANATION>EXPLAIN.LISP.4 was
;;; snarfed and merged into the KBE sources with some minor modifications
;;; by KMP.

;;; Debugging Utilities

(defvar *trace-explanations* nil "Flag: type out debugging info?")

(defmacro trace-explanation (&rest args)
  `(cond (*trace-explanations* (format t ,@args))))

(define-internal-option *EXPLAIN-debug* "Debug explanation errors"
  nil :boolean)

;; this sets up a gard to detect recursive calls to a function.

(defvar recursion-stack nil "used by recursion-check")

(defmacro recursion-check (call &body body)
  `(let ((recursion-stack (cons ,call recursion-stack)))
    (cond ((member (car recursion-stack) (cdr recursion-stack))
           (when *EXPLAIN-debug*
             (cbarf "Allow this explanation request to fail."
                    "Explain: Recursive call ~:N"
                    (car recursion-stack)))
           nil)
          (t
           ,@body))))

;;;; Abstraction from KBE internals

(defun cliche? (segcase)
  (~01 (??& segcase CLICHE)))

(defun cliche-name (cliche)
  (~01 ~clichename (??& cliche CLICHE)))

(defun plan-type (segcase)
  (~01 ~type (??& segcase PLANTYPE)))

(defun has-plan-type (segcase type)
  (~01 ~type (??& segcase PLANTYPE type)))

(defun has-slot (segcase thing)
  (and (not (fact? thing))
       (??& segcase SLOT thing)))

(defun slot-name (segcase thing)
  (and (not (fact? thing))
       (~01 ~slotname (??& segcase SLOT thing))))

(defun segment-of (thing &aux (type (thing-type thing)))
  (selectq type
           (seg thing)
           (@io (~segcase (~deep thing)))
           (t (~segcase thing))))

(defun subseg* (seg1 seg2)
  (cond ((equal seg1 seg2) t)
        ((null seg1) nil)
        (t (subseg* (& (?supseg (~seg seg1))) seg2))))

(defun from (port &aux (data-flow (??& * DFLOW * port)))
  (cond (data-flow (~from (car data-flow)))
        (t (~01 (??& * TINPUT * * port)))))

(defun to (port &aux (data-flow (??& * DFLOW port *)))
  (cond (data-flow (~nset ~to data-flow))
        (t (~nset (??& * TOUTPUT * * port)))))

(defun dest-is-term (port)
  (do ((ports (to port) (mapcan #'to losers))
       (winners nil)
       (losers nil nil))
      ((null ports) winners)
    (dolist (port ports)
      (cond ((and (eq (thing-type port) 'tinput)
                  (??& * ROLE (segment-of port) 'term))
             (push (segment-of port) winners))
            (t (push port losers))))))

(defun find (cliche-name)
  (~nset ~segcase (??& * CLICHE cliche-name)))

;;;; Automatic Explanations

(defun explain-fn (fn)
  (let ((sc (name->segcase fn)))
    (when sc
      (let ((exp (condition-case-if (not *EXPLAIN-debug*) (cond)
                                    (explain sc)
                                    (sys:pdl-overflow
                                     (text "Got a ~A pdl overflow generating explanation!"
                                           (send cond ':pdl-name)))
                                    (sys:error
                                     (text "Got an error generating explanation!~@[~%~A~]"
                                           (if *debug* cond))))))
        (when exp
          `(:text ,exp :auto-fill t))))))

(defun explain1 (thing relative-to implementation)
  (cond ((has-slot relative-to thing)
         (cond (implementation
                (format nil "~A, implemented as ~A"
                        (explain thing relative-to ':outer)
                        implementation))
               (t (explain thing relative-to ':outer))))
        (t (let ((part (and (not (fact? thing))
                            (~01 (??& * SLOT thing)))))
             (cond (part (explain thing (segment-of part) ':outer))
                   (t implementation))))))

(defun explain (thing &optional (relative-to thing) (i-level ':detail)
                &aux (level i-level))
  (trace-explanation "~%Explaining ~S: ~S-- ~A" thing relative-to level)
  (recursion-check (list 'explain thing relative-to i-level)
                   (setq relative-to (segment-of relative-to))
                   (let ((explanation-segment (selectq i-level
                                                       ((:DETAIL)  relative-to)
                                                       ((:SUMMARY) (segment-of thing))
                                                       ((:OUTER)   relative-to)))
                         type)
                     (if (eq i-level ':outer) (setq level ':summary))
                     (setq type
                           (cond ((symbolp thing) (prog1 thing (setq thing nil)))
                                 ((slot-name explanation-segment thing))
                                 ((thing-type thing))))
                     (cond ((and (eq type 'seg)
                                 (not (equal thing explanation-segment)))
                            (explain thing relative-to ':summary))
                           ((and (eq type 'toutput)
                                 (not (equal (segment-of thing) relative-to)))
                            (or (explain thing (segment-of thing) ':summary)
                                (explain (segment-of thing) relative-to ':outer)))
                           ((and (eq type 'output)
                                 (not (equal (segment-of thing) relative-to)))
                            (or (explain (segment-of thing) relative-to ':outer)
                                (explain thing (segment-of thing) ':summary)))
                           (t
                            (let ((cliche-name (cliche-name explanation-segment))
                                  explanation-handler
                                  plantype)
                              (cond ((get cliche-name 'cliche-comments)
                                     (handle cliche-name level
                                             relative-to explanation-segment type thing))
                                    (t
                                     (setq plantype (plan-type explanation-segment))
                                     (setq explanation-handler (get plantype level))
                                     (cond (explanation-handler
                                            (funcall explanation-handler
                                                     relative-to type thing))
                                           ((and (null plantype)
                                                 (neq i-level ':outer))
                                            (explain thing relative-to ':outer)))))))))))

(defun explain-input (input default)
  (let ((name (~01 ~name (??& input @io))))
    (cond (name (format nil "~A" name))
          (default)
          (t (or (explain-input1 (from input))
                 (format nil "the input {~A}" (~obj input)))))))

(defun explain-input1 (input)
  (cond ((memq (thing-type input) '(output toutput))
         nil)
        ((null input) nil)
        (t (let ((name (~01 ~name (??& input @io))))
             (cond (name (format nil "~A" name))
                   (t (explain-input1 (from input))))))))

(defun find-inputs (segment relative-to)
  (mapcar #'(lambda (x) (find-input x relative-to))
          (??& segment INPUT)))

(defun find-input (input relative-to)
  (trace-explanation "~&FIND-INPUT ~S~S" input relative-to)
  (cond ((equal (segment-of input) relative-to)
         (explain-input input nil))
        (t (or (find-input1 (from input) relative-to)
               (explain-input input nil)))))

(defun find-input1 (input relative-to)
  (trace-explanation "~&FIND-INPUT1 ~S~S" input relative-to)
  (cond ((null input) nil)
        ((or (equal (segment-of input) relative-to)
             (??& relative-to SLOT (segment-of input)))
         (explain input relative-to ':outer))
        ((memq (thing-type input) '(output toutput))
         (explain input relative-to ':summary))
        (t (find-input1 (from input) relative-to))))

(defun explain-output (output &aux (kind (~kind (?& output @IO))))
  (cond ((eq kind 'returnval)
         (format nil "the returned value, {~A}," (~obj output)))
        (t (format nil "the output {~A}" (~obj output)))))

(defun explain-parts (relative-to partname)
  (let ((parts (??& relative-to SLOT * partname)))
    (cond (parts
           (phrase-append "and"
                          (mapcar #'(lambda (master)
                                      (explain-part relative-to
                                                    master
                                                    partname))
                                  parts)))
          (t (or (explain partname relative-to ':outer)
                 (format nil "{MISSING PART ~A IN ~A}"
                         partname relative-to))))))

(defun explain-part (relative-to part partname)
  (cond ((and (eq (thing-type (~slotseg part)) 'seg)
              (not (cliche? (~slotseg part)))
              (has-plan-type (~slotseg part) 'pred))
         (explain-termination relative-to partname))
        (part
         (or (explain (~slotseg part) relative-to ':summary)
             (explain (~slotseg part) relative-to ':outer)))
        (t (format nil "{MISSING PART ~A IN ~A}" partname relative-to))))

(defun explain-termination (relative-to partname)
  (let ((part (~01 ~slotseg (??& relative-to slot * partname))))
    (cond (part (explain-wff part relative-to))
          (t (format nil "{MISSING PART ~A IN ~A}"
                     partname relative-to)))))

(defun explain-wff (segment relative-to)
  (phrase-append "or" (make-termination-wff
                       (~segcase (?& * ROLE segment))
                       relative-to)))

(defun make-termination-wff (ssr relative-to)
  (let* ((body (~from (?& ssr CFLOW * ssr)))
         (join (~from (?& body CFLOW * body)))
         (other-terms
          (mapcan #'(lambda (gronk)
                      (make-termination-wff gronk relative-to))
                  (mapcan #'dest-is-term (??& ssr TOUTPUT)))))
    (cons
     (do ((term-segcase
           (~from (?& body CFLOW * (& (~seg join) 'pred)))
           (~from (?& (& (~seg term-segcase)) CFLOW * term-segcase)))
          (wff nil nil)
          (term-part nil))
         ((or (null term-segcase)
              (setq wff (~01 ~wff (??& term-segcase WHEN))))
          (explain1
           (or term-part
               (& (~seg term-segcase)))
           relative-to
           (cond ((null wff) nil)
                 ((and (listp wff)
                       (eq (car wff) 'not))
                  (format nil "not ~A"
                          (find-input
                           (?& (& (~seg term-segcase)) INPUT
                               * (cadr wff))
                           relative-to)))
                 (t (find-input (?& (& (~seg term-segcase)) INPUT
                                    * wff)
                                relative-to)))))
       (if (and (null term-part)
                (??& * slot (& (~seg term-segcase))))
           (setq term-part (& (~seg term-segcase)))))
     other-terms)))

(defun handle (cliche level relative-to explanation-segment type thing)
  (let ((exp (handler-format relative-to cliche type level)))
    (selectq level
             ((:DETAIL)
              (cond ((eq type 'seg)
                     (handle-format exp relative-to thing level))
                    (exp
                     (format nil "~A, implemented as ~A"
                             (handle-format exp relative-to thing level)
                             (explain thing relative-to ':summary)))
                    (t "Don't know.")))
             ((:SUMMARY)
              (cond (exp (cond
                           ((memq (thing-type thing) '(input tinput))
                            (explain-input thing
                                           (handle-format
                                            exp relative-to thing level)))
                           (t (let ((explanation
                                     (handle-format
                                      exp explanation-segment thing level)))
                                (selectq type
                                         (seg (a-or-an explanation))
                                         (t explanation))))))
                    (t (selectq type
                                ((input tinput) (find-input thing relative-to))
                                (t (explain thing relative-to ':summary)))))))))

(defun handle-format (format relative-to thing level)
  level thing ;ignored? --KMP
  (or (listp format) (setq format (list format)))
  (do ((string "" (format nil "~A~A~A" string space next-string))
       (space "" " ")
       (next-string)
       (format format (cdr format)))
      ((null format) string)
    (setq next-string
          (selectq (typep (car format))
                   (:symbol (explain-parts relative-to (car format)))
                   (:string (car format))))))

(defun handler-format (relative-to cliche type level &aux handler)
  (setq type
        (cond ((eq type 'seg)
               (selectq level
                        (:detail 'detail)
                        (:summary 'main)))
              (t type)))
  (setq handler
        (or (get (get cliche 'cliche-comments) type)
            (if (not (memq type
                           '(detail main input output tinput toutput)))
                (de-hyphenate type))))
  (cond ((and (neq type 'detail)
              (neq type 'main))
         (let ((parts (or (??& relative-to slot * type)
                          (if (memq type '(action init pred join
                                           rec op body dfjoin
                                           bssr gen acc trn
                                           term filter))
                              (??& relative-to ROLE * type)))))
           (if handler
               (setq handler
                     (list
                      (selectq (length parts)
                               (0 handler)
                               (1 (format nil "the ~A" handler))
                               (t (a-or-an handler)))))))))
  handler)

(defun constant-handler? (format)
  (and (= (length format) 1)
       (stringp (car format))))
