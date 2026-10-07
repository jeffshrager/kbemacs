;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>FOCUS.LISP
;;; PDF pages 193-198 (printed source pages 1-6).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; Utilities
(defun code->path (name subpart)
  (declare (special subpart))
  (*catch 'path-seen
          (cond ((eq (car subpart) '*code-is-cdr*)
                 (barf "Code format not supported: ~S" subpart))
                ((eq (car subpart) '*code-is-car*)
                 (setq subpart (cdr subpart))
                 (code->path-1-for-code-is-car (code-for-defun name) nil))
                (t
                 (code->path-1 (code-for-defun name) nil)))
          (ferror "(CODE->PATH ~S ~S) failed." name subpart)))
(defun code->path-1-for-code-is-car (code path)
  (declare (special subpart))
  (cond ((not (atom code))
         (do ((l code (cdr l))
              (i 0 (1+ i)))
             ((atom l))
           (if (eq l subpart)
               (*throw 'path-seen (nreverse (cons i path))))
           (code->path-1-for-code-is-car (car l) (cons i path))))))
(defun code->path-1 (code path)
  (declare (special subpart) )
  (if (eq code subpart) (*throw 'path-seen (nreverse path)))
  (cond ((not (atom code))
         (do ((l code (cdr l))
              (i 0 (1+ i)))
             ((atom l))
           (code->path-1 (car l) (cons i path))))))

;;; Inverse
(defun path->string (name path)
  (declare (values string code))
  (let ((string nil) (code nil))
    (ignore-errors
      (let ((parsed (code->parsed (setq code (path->code name path)))))
        (if parsed (setq string (unparse-command parsed)))))
    (values string (copytree code))))
(defun path->code (name &optional (path '()))
  (let ((code (code-for-defun name)))
    (path->code-1 code path nil)))
(defun path->code-1 (code path backpointer)
  (cond ((null path)
         (cond ((atom code)
                (cond ((null backpointer) nil)
                      (t (cons '*code-is-car* backpointer))))
               (t code)))
        ((atom code)
         ;;; this happens when the user's code and the KBE's idea of what
         ;;; is in the code differ. probably it means that the definition
         ;;; in the buffer has not been analyzed recently enough.
                                        ; hide icky message for now...
                                        ;(barf "PATH->CODE-1 saw atomic code: ~S" code)
                                        ; and replace with something friendlier...
         (editor-barf
          "Sorry, the smarts needed to implement that aren't there yet.~
          ~%Maybe if you re-analyze this function and try again..."))
        (t
         (let ((backpointer (nthcdr (car path) code) ))
           (path->code-1 (car backpointer) (cdr path) backpointer)))))

;;; Notes
;;; In order for the  :TEMPORARY marks to to work at all, we rely
;;; on a patch to ZWEI which allows WINDOW-MARK-P to take on other
;;; than boolean values. The patch  is not part of the KBE system,
;;; $0 this may or may not always win.  In the case that the patch
;;; is not available, the effect of using  :TEMPORARY here should be
;;; the same as using T  (ie, all marks should be permanent). --KMP
(defun mark-sexp-from-path (name path &optional (region-style ':temporary)
                                        (at-end? nil))
  (let ((pt (find-single-definition name) ))
    (when pt
      (make-buffer-current (containing-buffer pt))
      ;; Ignore possible problems with #{...} syntax.
      (ignore-errors
        (dolist (n path)
          (setq pt (or (forward-down-list pt) (editor-barf)))
          (dotimes (i (1+ n))
            (setq pt (or (forward-sexp pt) (editor-barf))))
          (setq pt (or (backward-sexp pt) (editor-barf)))))
      (if at-end?
          (establish-region (forward-sexp pt) pt region-style)
          (establish-region pt (forward-sexp pt) region-style)))))
(defun compute-offset (bp x y)
  (dotimes (i y)
    (setq bp (or (forward-line bp) bp)))
  (dotimes (i (min (string-length
                    (string-buffer-between bp (end-of-line bp))) x))
    (setq bp (or (forward-char bp) bp)))
  bp)
(defun mark-sexp-from-offsets (&optional
                                 (name (current-editor-definition-name) )
                                 (x1-offset 0)
                                 (y1-offset 0)
                                 (region-style nil)
                                 (x2-offset)
                                 (y2-offset))
  (let ((start-pt (bounds-of-definition name) ))
    (when start-pt
      (make-buffer-current (containing-buffer start-pt))
      (let* ((offset1 (compute-offset start-pt x1-offset y1-offset))
             (offset2 (if (not x2-offset) offset1
                          (compute-offset start-pt x2-offset y2-offset))))
        (establish-region offset1 offset2 region-style)))))

;;; Set up keys...       :
(defun comment-definition (name text &optional (err-if-no-comment? nil))
  (cond ((and err-if-no-comment? (not text) )
         (editor-barf "Unable to provide a comment for ~S." name)
         nil) ;no changes to buffer
        (t
         (insert-comment-definition name text
                                        ;supressing this DICK
                                        ;(syntax-for-definition name)
                                    )
         (editor-window-redisplay)
         t)))
(defun syntax-for-definition (name &optional (code (belief ':code name)))
  (selectq (car code)
           (defun (text "(~A~{ ~A~})"
                        name
                        (mapcar #'string-downcase
                                (parse-argument-list (caddr code)))))
           (define-cliche
               (text "Cliche ~A" name) )
           (def-ada-top
               (syntax-for-definition name (nth 4 code)))
           (def-ada-fn
               (text "~:[Procedure~;Function~] ~A~@[ (~{~A~^, ~})~]"
                     (caddr (cadr code))
                     (car  (cadr code))
                     (mapcar #'(lambda (x) (string-downcase (car x)))
                             (cadr (cadr code)))))
           (otherwise nil)))  ;no banner line for comment in this case
(defun remove-misplaced-whitespace (string)
  (with-output-to-string (str)
    (do ((i 0 (1+ i))
         (space-flag nil)
         (len (string-length string)))
        ((= i len))
      (let ((c (aref string i)))
        (cond ((or (eq c #\space) (eq c #\tab)  (eq c #\return))
               (setq space-flag t))
              (t
               (format str "~@[~* ~]~C"
                       (and space-flag
                            (not (memq c '(#/, #/. #/! #/; #/?))))
                       c)
               (setq space-flag nil)))))))
(defun format-explanation-for-comment  (explanation &optional syntax)
  (string-for-comment  (text "~@[~A~2%~]~A"
                             syntax
                             (format-explanation explanation))))
(defun format-explanation  (explanation)
  (cond ((stringp explanation) explanation)
        (t
         (let ((text (get (locf explanation) ':text)))
           (cond ((get (locf explanation) ':auto-fill)
                  (auto-fill-string (remove-misplaced-whitespace text)))
                 (t text))))))
 ;;; (LONGEST-LINE~LENGTH string)
 ;;; Returns the length of the longest line in string.
 ;;; Lines are defined to end with either a Return or the end of
 ;;; the string. For example,
  ;;;     (longest-line-length (format nil "123~%1234")) => 4

(defun longest-line-length (text-string)
  (do ((i 0 (1+ i))
       (j 0)
       (longest-so-far 0)
       (text-length (string-length text-string)))
      ((= i text-length) (max j longest-so-far))
    (cond ((= (aref text-string i) #\return)
           (setq longest-so-far (max j longest-so-far))          sei
           (setq j 0))
          (t
           (incf j)))))
;;; (STRING-FOR-COMMENT string [mode] [leader] [trailer])
;;; Takes a multi-line string and puts comment markers  in
;;; appropriate places, returning something  in comment syntax.
;;; Mode should be one of  :TOPLEVEL, :IMBEDDED, or :MARGIN to
;;; indicate where the comment will appear.
;;; The LEADER and TRAILER args are mostly  for debugging. In
;;; practice, they will usually want  to just default to
;;; the globally selected ZWEI options.
(defun string-for-comment  (string &optional
                                     (mode  ':toplevel)
                                     (begin (editor-comment-begin))
                                     (end   (editor-comment-end)))
  (let ((trailer-tab nil)
        (leader   (string-right-trim '(#\space #\tab) begin) )
        (trailer  (if end (string-left-trim  '(#\space #\tab) end))))
     ;;; A null trailer is the same as no trailer.
    (when (equal trailer "")
      (setq trailer nil))
     ;;; If a single character (eg, ";") and not matchfix, ...
    (when (and (not trailer) (= (string-length leader) 1))
      (setq leader
            (caseq mode
                   ((:toplevel) (string-append leader leader leader) )
                   ((:imbedded) (string-append leader leader))
                   ((:margin)   leader)
                   (t (barf "Bad MODE argument to STRING-FOR-COMMENT -- ~S~
                       ~%Should be one of :TOPLEVEL, :IMBEDDED, :MARGIN."
                            mode)))))
     ;;; Add spaces if necessary
    (setq leader  (string-append leader " "))
     ;;; Compute position of right margin
    (setq trailer-tab (if trailer (+ 1
                                     (string-length leader)
                                     (longest-line-length string))))
     ;;; Actually do it...
    (with-output-to-string (str)
      (format str "~A" leader)
      (do ((i 0 (1+ i))
           (len (string-length string)))
          ((= i len))
        (let ((c (aref string i)))
          (cond ((= c #\return)
                 (format str "~@[~VT~]~@[~A~]~%~A"
                         trailer-tab trailer leader))
                (t
                 (format str "~C" c)))))
      (format str "~@[~VT~]~@[~A~]" trailer-tab trailer))))
