;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>DOCUMENT.LISP
;;; PDF pages 232-235 (printed source pages 1-4).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

(defun document-fn (fn)
  (let* ((sc (~01 ~segcase (??& * @seg 'lambda-exp fn)))
         (cliche (and sc (??& sc cliche))))
    (when cliche
      `(:text ,(document-fn1 (~seg sc) fn) :auto-fill nil))))

(declare (special *2focus-seg*))  ;see definition of kbe-/#-comment-macro

(defun document-fn1 (seg name)
  (let* ((*2focus-seg* seg)
         (cliche-name (~clichename (?& (&* seg) cliche)))
         (comments (get cliche-name 'cliche-comments))
         (role-tree (get comments 'role-tree))
         (described-roles (get comments 'described-roles))
         (main-sentence `(+doc-sentence "The" "program" ,name "is"
                          ,(select-a-or-an cliche-name)
                          ,(string-downcase cliche-name)))
         (role-doc-sentences))
    (prog (role-name roles-to-do)
       (setq roles-to-do described-roles)
     L (when (null roles-to-do)
         (setq role-doc-sentences (nreverse role-doc-sentences))
         (return))
       (setq role-name (pop roles-to-do))
       (let* ((segcase
               (eval (make-kbe-comment nil (list 'the role-name))))
              (cliche (if segcase
                          (~01 ~clichename (??& segcase cliche)))))
         (cond ((null segcase)
                (push `(+doc-sentences
                        (+doc-sentence "There"
                         ,(select-is-or-are role-name)
                         "no" ,(string-downcase role-name)))
                      role-doc-sentences))
               ((?empty-slot (~seg segcase))
                (push `(+doc-sentences
                        (+doc-sentence "The"
                         ,(string-downcase role-name)
                         ,(select-is-or-are role-name)
                         "not" "implemented"))
                      role-doc-sentences))
               ((null cliche)
                (let ((code (or (possible-embedded-code segcase 60)
                                "an idiosyncratic computation")))
                  (if code
                      (push `(+doc-sentences
                              (+doc-sentence "The"
                               ,(string-downcase role-name)
                               ,(select-is-or-are role-name) ,code))
                            role-doc-sentences))))
               ((eq cliche 'pseudo-subcliche)
                (setq roles-to-do
                      (append (reverse (cdr (assq role-name role-tree)))
                              roles-to-do)))
               (T (push `(+doc-sentences
                          (+doc-sentence "The"
                           ,(string-downcase role-name)
                           ,(select-is-or-are role-name)


                           ,(select-a-or-an cliche)
                           ,(string-downcase cliche) )
                          ,(process-comment-string
                            (~seg segcase) cliche))
                        role-doc-sentences) )))
       (go L))
    (let ((pp:*print-right-margin* 70.))
      (format nil "~:="
              (list* '+doc-whole main-sentence role-doc-sentences)))))

(pp:define-list-print-function +doc-whole (sentences s)
  (format s "~@I~2I~{~=~^~%~}~." (cdr sentences)))
(pp:define-list-print-function +doc-sentences (sentences s)
  (format s "~@I~3I~{~=~^  ~:_~}~." (cdr sentences))) ;odd if >2
(pp:define-list-print-function +doc-sentence (sentence s)
  (let ((*print-escape* nil))
    (format s "~@I~{~=~^ ~:_~}.~." (cdr sentence) )))
(defun process-comment-string (seg cliche)
  (let* ((*2focus-seg* seg)
         (comments (get cliche 'cliche-comments) )
         (string (get comments 'comment))
         (words))
    (if (and (listp string) (eq (car string) 'str-lit)) ;for ada            ;
        (setq string (cadr string)))
    (setq words (fix-up-semi-kbe-/#-comment-macros
                 (read-from-string (string-append "(" string ")"))))
    (list* '+doc-sentence
           '"It"
           (mapcan #'(lambda (w)
                       (if (symbolp w) (ncons (string-downcase (string w)))
                           (let ((code (possible-embedded-code
                                        (eval w)
                                        20)))
                             (if code (ncons code)
                                 (make-a-phrase (kbe-comment-name w))))))
                   words))))
                                        ;the y -> a y
                                        ;the x -> an x
                                        ;the values -> values
(defun make-a-phrase (name-list)
  (do ((l name-list (cdr l))
       (result nil))
      ((null l) (nreverse result))
    (cond ((not (eq (car l) 'the)) (push (string-downcase  (car l)) result))
          ((or (null (cdr l)) (equal (select-is-or-are  (cadr l)) "are")))
          (T (push (select-a-or-an (cadr l)) result)))))
(defun possible-embedded-code (role-segcase max-length)
  (if role-segcase
      (let* ((@seg (~01 (??& role-segcase @seg)))
             (code (or (~01 ~varname (??& (&* role-segcase output)
                                          @createvar) )
                       (~01 ~name (ignore-errors
                                    (??& (&* role-segcase output * 'ret)
                                         @io 'free-var))) ;kludge
                       (and (eq  (~kind @seg) 'constant)
                            (cadr (~name @seg)))
                       (car (~code @seg))))
             (good-code  (doc-decode-code code))
             (code-string
              (with-output-to-string  (str)
                (print-code good-code str))))
        (cond ((and good-code
                    (not (?empty-slot (~seg role-segcase) ))
                    (< (string-length code-string) max-length))
               (cond ((stringp good-code) code-string)
                     ((not (listp good-code)) good-code)
                     ((multiple-value-bind (ignore last)
                          (read-from-string code-string)
                        (< last (string-length code-string)))
                      (string-append "(" code-string ")"))
                     (T code-string)))
              ((and (null code)
                    (eq (~kind @seg) 'funcall)
                    (eq (~name @seg) '5noop))
               (let* ((to (~to (car (??& * dflow
                                         (&* role-segcase output))))))
                 (if     to (~01 ~name (??& to @io)))))))))
(defun doc-decode-code (code)
  (cond ((not (listp code)) code)
        ((memq (car code) '(*code-is-car* *code-is-cdr*))
         (doc-decode-code (cadr code)))
        ((eq (car code) 'kbe-/#-comment-macro)
         (cond ((null (cadr code)) nil)
               ((and (listp (cadr code)) (eq (cadr (cadr code)) '...)) nil)
               (T (caadr code))))
        (T code)))
