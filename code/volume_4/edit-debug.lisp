;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>EDIT-DEBUG.LISP
;;; PDF pages 184-191 (printed source pages 1-8).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; For typing convenience
;;; Hyper-Super-Meta-Control-A         Automatic Ada Mode
;;; Enters Ada mode and maybe puts buffer in KBE package.
(define-editor-command com-automatic-ada-mode (ignore)
    #\Hyper-Super-Meta-Control-A
  "Enters Ada Mode and updates the Attribute List"
  (validate-buffer-mode ':ada)
  (validate-buffer-package)
  t) ;this command may have modified the buffer
;;; Hyper-Super-Meta-Control-L          Automatic Lisp Mode
;;; Enters Lisp mode and maybe puts buffer in KBE package
;;; (if that's the default for consistent buffers).
(define-editor-command com-automatic-lisp-mode (ignore)
    #\Hyper-Super-Meta-Control-L
  "Enters Lisp Mode and updates the Attribute List"
  (validate-buffer-mode ':lisp)
  (validate-buffer-package)
  t) ;this command may have modified the buffer
;;; Hyper-Super-Meta-Control-N          Name This Expression
;;; Name the s-expression (assign some special variable to the
;;; result of reading it from buffer).
(define-editor-command com-name-this-expression (ignore)
    #\Hyper-Super-Meta-Control-N
  "Name the s-expression at the cursor."
  (let* ((exp (condition-case (cond)
                  (read (rest-of-buffer-stream) )
                (sys:read-error
                 (editor-barf "Can't find sexp to name.~%~A" cond))))
         (prinlevel  2)
         (prinlength 3)
         (prompt (text "Name for ~S:" exp)))
    (do ((form (ignore-errors (read (mini-buffer-stream prompt nil)))
               (ignore-errors (read (mini-buffer-stream prompt nil)))))
        ((and form
              (symbolp form)
              (or (not (boundp form) )
                  (fquery  '() "~S is already bound. Clobber its value? "
                           form)))
         (set form exp)
         (editor-echo-output "~S now set to ~S." form exp))))
  nil)  ;no changes have been made to buffer

;;; Slide Preparation
;;; Hyper-Super-Meta-Control-Y         Yank Last Several KBE Requests
;;; Yanks the text for KBE requests since this command was last used.
;;; Text of recent requests are yanked onto separate lines.
;;; If this is not done on a blank line, anything before the cursor is
;;; repeated before every request yanked. Anything after the cursor is
;;; repeated after every request yanked.
(define-editor-command com-yank-last-several-KBE-requests (ignore)
    #\Hyper-Super-Meta-Control-Y
  "Yanks the text for KBE requests since this command was last used.
Text of recent requests are yanked onto separate lines.
If this is not done on a blank line, anything before the cursor is
repeated before every request yanked. Anything after the cursor is
repeated after every request yanked."
  (cond (*saved-requests*
         (let ((b (beginning-of-line))
               (e (end-of-line))
               (pt (editor-point)))
           (let ((head (string-buffer-between b pt))
                 (tail (string-buffer-between pt e)))
             (delete-from-buffer-between b
                                         (or (forward-line)
                                             (buffer-end) )
                                         t)
             (dolist (request (reverse *saved-requests*))
               (insert-into-buffer-before-point
                (text "~A~A~A~&" head request tail)))
             (setq *saved-requests* nil)
             t))) ;changes to buffer
        (t
         (editor-echo-output "No recent requests."))))

;;; Style reformatting
(define-editor-command com-change-this-expression (ignore)
    #\Hyper-Super-Meta-Control-C
  "Change the style of the enclosing form."
  (do ((pt (editor-point) (backward-up-list pt))
       (error-message nil))
      ((not pt)
       (cond ((not error-message)
              (editor-echo-output "No obvious form to change."))
             (t
              (editor-barf "Syntax error in surrounding expression:~%~A"
                           error-message) ))
       nil) ;no changes
    (let ((exp (condition-case (cond)
                   (read (rest-of-buffer-stream pt))
                 (sys:read-error
                  (setq error-message (or error-message
                                          (let ((prinlevel 2)
                                                (prinlength 2))
                                            (text "~A" cond))))
                  nil))))
      (when (and (not (atom exp)) (atom (car exp)))
        (let ((fn (get (car exp) 'style-macro)))
          (when fn
            (let ((result (funcall fn exp)))
              (cond (result
                     (insert-into-buffer-before
                      pt
                      (let ((prinmargin (- (send terminal-io
                                                 ':size-in-characters)
                                           (cadr pt)))) ;cheat
                        (text "~:=" result)))
                     (delete-from-buffer-between pt (forward-sexp pt))
                     (move-point (backward-sexp pt))
                     (zwei:com-indent-sexp)
                     (move-point (forward-down-list (editor-point)))
                     (let ((prinlevel 1) (prinlength 1))
                       (editor-echo-output "~S rewritten as ~S"
                                           exp result))
                     (return t)) ;changed buffer
                    (t
                     (editor-echo-output "No way to change this ~S form."
                                         (car exp))
                     (return nil))))))))))
(defun (cond style-macro) (form)
  (let ((clauses (cdr form)))
    (cond ((null clauses) nil)  ;no idea
          ((null (cdr clauses))
           (let ((clause (car clauses)))
             (cond ((null (cdr clause)) (car clause) )
                   ((null (cddr clause)) `(if ,@clause))
                   ((and (not (atom (car clause)))
                         (eq (car clause) 'not))
                    `(unless ,(cadr (car clause)) ,@(cdr clause) ))
                   (t `(when ,@clause)))))
          (t nil))))
(defun (when style-macro) (form)
  `(cond ,(cdr form)))
(defun (unless style-macro) (form)
  `(cond ((not ,(cadr form)) ,@(cddr form))))
(defun (if style-macro) (form)
  `(cond (,(cadr form) ,(caddr form))
    ,@(if (cdddr form)

          `((t ,@(cdddr form))))))




;;; Debugging
;;; Hyper-Super-Meta-Control-D          Toggle KBE Debug Mode
;;; Toggles KBE  debug mode.
(define-editor-command com-toggle-kbe-debug-mode (off?)
    #\Hyper-Super-Meta-Control-D
  "Enable debug mode, or with an argument, disable it."
  (debug (not off?))
  nil) ;no changes have been made to buffer
;;; Hyper-Super-Meta-Control-Abort      Reset Apprentice
;;; Tries to reset the associated apprentice's process.
(define-editor-command com-reset-apprentice (ignore)
    #\Hyper-Super-Meta-Control-Abort
  "Resets the KBE process. Works in Debug Mode only."
  (editor-echo-output (reset-apprentice))
  nil) ;no changes have been made to buffer
;;; Hyper-Super-Meta-Control-System     Select Apprentice
;;; Tries to select the associated apprentice's process.
(define-editor-command com-select-apprentice (ignore)
    #\Hyper-Super-Meta-Control-System
  "Selects the KBE process. Works in Debug Mode only."
  (editor-echo-output (select-apprentice) )
  nil) ;no changes have been made to buffer

;;; Error Recovery
;;; Hyper-Super-Meta-Control-Resume     Make Buffer Consistent For KBE         -        eee
;;; Tries to recover from some kinds of KBE interface lossage.         :
;;; Makes definitions in buffer consistent with definitions known
;;; to the interface. With an arg, queries for each definition.
(define-editor-command com-Make-buffer-consistent-for-KBE (ignore)
    #\Hyper-Super-Meta-Control-Resume
  "Tries to recover from some kinds of KBE interface lossage.
Makes definitions in buffer consistent with definitions known
to the interface. With an arg, queries for each definition."
  (assure-real-editor-buffer)
  (let ((definitions-to-update nil))
    (declare (special definitions-to-update) )
    (send-delegate ':map-over-beliefs-of-type ':code
                   #'(lambda (fn code)
                       (cond ((not (code-equal code (get-definition fn)))
                              (push (list fn code) definitions-to-update)))))
    (dolist (entry definitions-to-update)
      (let (((fn code) entry))
        (if (fquery '() "Update definition of ~S? " fn)
            (replace-or-create-definition fn code)))))
  t) ;possible mods to buffer

;;; Cliche Definilions
;;; Hyper-Super-Meta-Control-;          Compile Cliche Comments
;;; Pick up the comments from the cliche definition at the cursor
;;; and put them in the right place without the overhead of actually
;;; compiling this definition.
(define-editor-command com-compile-cliche-comments (ignore)
    #\Hyper-Super-Meta-Control-/;
  "Compile cliche comment for current form."
  (cond ((not *debug*)
         (editor-barf "Only works when KBE  is in debug mode."))
        (t
         (let ((cliche-name (current-editor-definition-name) ))
           (let* ((pgm (get-definition cliche-name) )
                  (comments  (if (and pgm (eq (car pgm) 'define-cliche))
                                 (caddr pgm))))
             (cond (comments
                    (putprop cliche-name
                             (cons nil comments)
                             'cliche-comments)
                    (editor-echo-output "Cliche comments for ~S updated."
                                        cliche-name))
                   (t
                    (editor-barf
                     "Only works for cliche definition forms.")))))))
  nil) ;no changes have been made to buffer
