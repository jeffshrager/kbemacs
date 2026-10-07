;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_4.pdf
;;; PDF pages 81-131 (printed source pages 1-51).
;;; -*- Mode:LISP; Package:KBE; Base:10.; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; ZMAGIC: Interface to ZWEI internals

;;; This file contains most of the references to ZWEI in the KBE system
;;; so that the rest of the system doesn't get overwhelmed by ZWEI's
;;; crufty interface.

;;;; Editor command definition

(defvar *editor* nil)

(defvar *initial-buffer* nil
  "In a ZWEI command, the buffer it was invoked from.")

;;;

(defmacro with-buffer-pointer ((variable buffer-pointer type) &body body)
  `(zwei:with-bp (,variable ,buffer-pointer ,type)
     ,@body))

(defmacro with-local-buffer-pointer ((var bp) &body body)
  (let ((val (gensym)))
    `(let ((,val nil))
       (setq ,val ,bp)
       (unwind-protect (let ((,var ,val))
                         ,@body)
         (zwei:flush-bp ,val)))))

(eval-when (eval load compile)

  (defun fix-if-KBE-command-name (command-name)
    (setq command-name (string-copy command-name)) ;lossage avoidance
    (do ((pos (string-search "kbe" command-name)
              (string-search "kbe" command-name (1+ pos))))
        ((not pos) command-name)
      (setf (aref command-name (+ pos 0)) #/K)
      (setf (aref command-name (+ pos 1)) #/B)
      (setf (aref command-name (+ pos 2)) #/E)))

  )

;;; Our DEFINE-ZWEI-COMMAND thing does (KM) automatically.
;;; This means the mark stays unless the command goes to work
;;; to make it go away.

(advise zwei:make-command-name :around casification-control nil
        (let ((test-result (get (car arglist) 'zwei:command-name))
              (real-result :do-it))
          (if (string-equal real-result test-result)
              test-result ;prefer old case
              real-result)))

(defmacro define-zwei-command (name bvl key doc &body body)
  `(progn 'compile
          (defprop ,name ,(fix-if-KBE-command-name
                           (zwei:make-command-name name))
            zwei:command-name)
          (zwei:defcom ,name ,doc (km)
                       (cond (((lambda ,bvl
                                 (let ((*initial-buffer* zwei:*interval*))
                                   ,@body))
                               (and zwei:*numeric-arg-p*
                                    zwei:*numeric-arg*))
                              zwei:dis-text)
                             (t zwei:dis-bps)))
          ,(cond (key
                  `(progn ,@(mapcar #'(lambda (key)
                                        `(zwei:set-comtab
                                          zwei:*standard-comtab*
                                          `(,key ,name)))
                                    (cond ((listp key) key)
                                          (t (list key))))))
                 (t
                  `(zwei:set-comtab zwei:*standard-comtab*
                                    '()
                                    (zwei:make-command-alist
                                     `(,name)))))))

(defvar *zwei-commands* '() "List of KBE commands available in ZWEI.")

(define-internal-option *debug-zwei-errors*
    "Debug ZWEI errors in KBE commands"
  nil :boolean)

(defmacro define-editor-command (name bvl key doc &body body)
  `(progn 'compile
          (define-zwei-command ,name ,bvl ,key ,doc
                               (with-delegate (window-delegate)
                                 (condition-case-if (not *debug-zwei-errors*) (cond)
                                                    (with-automatic-error-recovery-if
                                                        (or (not *debug*)
                                                            (not *debug-zwei-errors*))
                                                      (progn ,@body))
                                                    ((error-condition-mixin)
                                                     (editor-barf "~A" cond)))))
          (if (not (memq ',name *zwei-commands*))
              (push ',name *zwei-commands*))
          ',name))

;;;; Editor command documentation

(defun mouse-sensitive-princ (type data &rest format-args)
  (cond ((send standard-output ':operation-handled-p ':item)
         (caseq type
                ((:function)
                 (lexpr-send standard-output ':item
                             'zwei:function-name data format-args))
                (otherwise
                 (barf "Invalid keyword to MOUSE-SENSITIVE-PRINC: ~S" type))))
        (format-args
         (lexpr-funcall #'format t format-args))
        (t ;for use, eg, outside of ZWEI
         (format t "~A" data))))

(defmacro do-editor-commands ((command) &body body)
  `(dolist (,command (reverse *zwei-commands*)) ;order of definition
     ,@body))

(defun document-editor-commands (&optional (comtab
                                            (if (boundp 'zwei:*comtab*)
                                                zwei:*comtab*
                                                zwei:*zmacs-comtab*)))
  (do-editor-commands (command)
    (document-one-editor-command command comtab)))

(defun document-one-editor-command (command
                                    &optional (comtab zwei:*zmacs-comtab*))
  (multiple-value-bind (key-list extended? name)
      (command-key-names command nil comtab)
    (format t "~2&")
    (cond ((not key-list)
           (setq name (or name command))
           (cond (extended?
                  (format t "~A " extended?)
                  (mouse-sensitive-princ ':function command "~A" name))
                 (t
                  (mouse-sensitive-princ ':function command "~A"
                                         name)
                  (format t " (not implemented)"))))
          (t
           (format t "~A (~@[~A ~]"
                   (phrase-append "and" key-list) extended?)
           (mouse-sensitive-princ ':function command "~A" name)
           (format t ")" name)))
    (format t " ... ~&")
    (zwei:print-doc ':full command)
    (format t "~&")))

(defun extended-command-p (symbol
                           &optional (zwei:*comtab* zwei:*zmacs-comtab*))
  (zwei:extended-command-p symbol))

(defun editor-command-name (command) (get command 'zwei:command-name))

(defun command-key-names (command
                          &optional limit (comtab zwei:*zmacs-comtab*))
  (declare (values key-names extended? name))
  (let ((key-names (mapcar #'bucky-char-name
                           (command-keys command limit comtab)))
        (extended? nil)
        (name (zwei:command-name command t)))
    (when (and name (extended-command-p command comtab))
      (setq extended? "m-X"))
    (when (and name
               (not key-names)
               (not extended?)
               (rassq command zwei:*command-alist*))
      (setq extended? "c-m-X"))
    (values key-names extended? name)))

(defun command-keys (command &optional limit (comtab zwei:*zmacs-comtab*))
  (do ((commands '())
       (i 0 (1+ i)))
      ((and limit (= i limit)) (nreverse commands))
    (multiple-value-bind (found? key)
        (zwei:key-for-command command
                              comtab
                              (1+ (or (car commands) -1)))
      (if (not found?) (return (nreverse commands)))
      (push key commands))))

(defun command-key-name (command)
  (multiple-value-bind (key-names extended? name)
      (command-key-names command)
    (cond (key-names (car key-names))
          (extended? (text "~A ~A" extended? name))
          (t nil))))

;;;; Buffers

(defun funcall-in-editor (fn &rest args)
  (lexpr-send zwei:*zmacs-command-loop* ':funcall-inside-yourself fn args))

(defun lexpr-funcall-in-editor (fn &rest args)
  (lexpr-funcall #'lexpr-funcall #'funcall-in-editor fn args))

;;; (BIND-EDITOR-BUFFER buffer . body)
;;;
;;;  Executes body with the editor's default buffer bound to the
;;;  given buffer.

(defmacro bind-editor-buffer (buffer &body body)
  `(let ((zwei:*interval* ,buffer))
     ,@body))

;;; (FIND-BUFFER buffer prompt create?)
;;;
;;;  Returns a pointer to the specified buffer.
;;;
;;;  If the buffer name is:
;;;
;;;    NIL, then the CREATE? and PROMPT arguments must have been
;;;
;;;
;;;    a string, then the specified buffer will be searched for and
;;;
;;;
;;;
;;;
;;;    anything else, then that thing will be returned. This is so that
;;;
;;;
;;;

(defun find-buffer (buffer-name &optional (prompt nil) (create? t))
  (let ((buffer (cond ((stringp buffer-name)
                       (zwei:find-buffer-named buffer-name create?))
                      (t buffer-name))))
    (cond (buffer buffer)
          ((not prompt)
           (barf "FIND-BUFFER got null buffer but no prompt."))
          (t
           (zwei:read-buffer-name prompt zwei:*interval* create?)))))

;;; (KILL-BUFFER name)

(defun kill-buffer (name)
  (zwei:kill-buffer
   (cond ((stringp name)
          (or (zwei:find-buffer-named name nil)
              (barf "KILL-BUFFER: No such buffer ~S" name)))
         ((null name)
          (zwei:read-buffer-name name zwei:*interval* nil))
         (t name))))

;;; (BUFFER-NAME buffer)

(defun buffer-name (buffer)
  (send buffer ':name))

;;; (BUFFER-NAME-IF-RELEVANT buffer)

(defun buffer-name-if-relevant (buffer)
  (unless (eq buffer *initial-buffer*)
    (buffer-name buffer)))

;;; (BUFFER-PATHNAME buffer)
;;;
;;;  Get the associated pathname of a buffer.

(defun buffer-pathname (buffer)
  (send-if-handles buffer ':pathname))

;;; (EDITOR-BUFFER-GET buffer propname)
;;; (EDITOR-BUFFER-PUT buffer propname val)

(defun editor-buffer-get (buffer propname)
  (send buffer ':get propname))

(defun editor-buffer-put (buffer propname val)
  (setf (send buffer ':get propname) val))

(eval-when (compile load eval)

  (defprop editor-buffer-get
      ((editor-buffer-get flavor-name indicator)
       (editor-buffer-put flavor-name indicator si:val))
    setf)

  ) ; NEHW-LAVE

;;; (MAKE-BUFFER-CURRENT buffer)

(defun make-buffer-current (buffer)
  (funcall-in-editor #'zwei:make-buffer-current buffer))

;;; (BUFFER-PACKAGE [buffer])

(defun buffer-package (&optional (buffer zwei:*interval*))
  (send buffer ':get ':package))

;;; (BUFFER-RADIX [buffer])

(defun buffer-radix (&optional (buffer zwei:*interval*))
  (send buffer ':get ':base))

;;; (BUFFER-FONT [buffer])

(defun buffer-font (&optional (buffer zwei:*interval*))
  (or (cdar (or (send buffer ':get ':font-alist)
                (zwei:window-font-alist zwei:*window*))) ;fall-back
      (tv:screen-default-font tv:mouse-sheet)))          ;desperation

;;; (CURRENT-EDITOR-BUFFER)

(defun current-editor-buffer ()
  (if (variable-boundp zwei:*interval*) zwei:*interval* nil))

;;; (BUFFER-END [buffer])

(defun buffer-end (&optional (buffer zwei:*interval*))
  (zwei:interval-last-bp buffer))

;;; (BUFFER-START [buffer])

(defun buffer-start (&optional (buffer zwei:*interval*))
  (zwei:interval-first-bp buffer))

;;; (CONTAINING-BUFFER buffer)

(defun containing-buffer (bp)
  (do ((node (zwei:bp-node bp)))
      (nil)
    (let ((superior-node (zwei:node-superior node)))
      (cond ((not superior-node) (return node))
            (t (setq node superior-node))))))

;;;; Editor I/O

;;; These are functions in case we need to funcall them.

;;; (EDITOR-BARF string . args)

(defun editor-barf (&rest args)
  (lexpr-funcall #'zwei:barf args))

(defprop editor-barf t :error-reporter)

;;; (EDITOR-ECHO-OUTPUT string . args)
;;;
;;;  Takes args like FORMAT and types them out to the editor echo area.

(defun editor-echo-output (string &rest args)
  (cond ((boundp 'zwei:*typein-window*)
         (lexpr-funcall #'zwei:typein-line string args))
        (t ; Allow some ZWEI utilities to run outside of ZWEI
         (lexpr-funcall #'format t string args))))

;;; (EDITOR-MAIN-OUTPUT string . args)

(defun editor-main-output (string &rest args)
  (lexpr-funcall #'format t string args))

;;;; Syntax tables

;;; (SET-EDITOR-LIST-SYNTAX-FROM-CHARACTER char from-char)
;;;
;;;  Sets the list-syntax of CHAR in the editor (ZWEI) to be the same
;;;  as the list syntax of FROM-CHAR.

(defun set-editor-list-syntax-from-character (char from-char)
  (zwei:set-char-syntax (zwei:char-syntax from-char
                                          zwei:*list-syntax-table*)
                        zwei:*list-syntax-table* char))

;;;; Pathnames

;;; (READ-DEFAULTED-PATHNAME prompt default)
;;;
;;; Prompts (a la mini-buffer) for a filename, which it returns.

(defun read-pathname (prompt default)
  (zwei:read-defaulted-pathname prompt default))

;;; (READ-DEFINITION-NAME prompt [default])

(defun read-definition-name (prompt
                             &optional default must-be-defined-p)
  (zwei:symbol-from-string
   (zwei:read-function-spec-string prompt default must-be-defined-p
                                   "You are typing the name of a function spec.")))

;;; (CURRENT-EDITOR-DEFINITION-NAME)
;;;
;;; Returns NIL if no current definition name or
;;; two values:
;;;  The name of the current editor definition.
;;;  The kind of definition (the car of the defining form).
;;;
;;; This definition could be much simpler if not for various
;;; sectionization bugs which are known to occur in Release 4.5
;;; and 5.0. Some of the bugs are claimed to be fixed in a later
;;; release...

(defun current-editor-definition-name (&optional (bp (zwei:point))
                                         (err? nil))
  (declare (values spec type node))
  (let ((spec nil) (type nil) (node nil))
    (cond ((and err? (zwei:window-mark-p zwei:*window*))
           (editor-barf "This command works on definitions, not regions."))
          (t
           (multiple-value-bind (bounding-node start-bp)
               (zwei:definition-interval bp)
             (cond (bounding-node
                    (zwei:check-interval-sections bounding-node)
                    (let ((start-line (zwei:bp-line start-bp)))
                      (setq spec (zwei:get-definition-function-spec
                                  start-bp))
                      (setq type
                            (when (and start-line
                                       (> (string-length start-line) 2)
                                       (char-equal (aref start-line 0)
                                                   #/())
                              (let ((package
                                     (buffer-package
                                      (containing-buffer bp))))
                                (read-from-string start-line nil 1))))
                      (setq node bounding-node)))
                   ((let ((n (zwei:bp-node bp)))
                      ;; Ugh. At this point, we must rely on sectionization
                      ;; to have been set up right. Not much chance. -kmp
                      (when (setq spec (send-if-handles n ':function-spec))
                        (setq node n)
                        (setq type (send n ':definition-type))
                        t)))
                   (t
                    (when err?
                      (editor-barf
                       "The cursor isn't inside a well-formed definition."
                       )))))))
    (when (not (or (get type 'zwei:definition-function-spec-type)
                   (stringp (si:definition-type-name type))))
      (setq spec nil)
      (if err?
          (editor-barf "The enclosing~@[ ~S~] form is not a defining form."
                       type)))
    (values spec type node)))

;;; (WITH-CURRENT-EDITOR-DEFINITION-NAME name . body)
;;;
;;;  Binds name to the current editor definition name within the scope
;;;  of body. If no current definition, body is never executed.

(defmacro with-current-editor-definition-name (name-and-maybe-type
                                               &body forms)
  `(multiple-value-bind ,name-and-maybe-type
       (current-editor-definition-name)
     (cond ((not ,(car name-and-maybe-type))
            (zwei:barf "The cursor isn't in a definition."))
           (t
            ,@forms))))

;;; (CURRENT-EDITOR-PATH bp)
;;;
;;;  Returns a path into the current toplevel expression as a list
;;;  (n1 n2 n3 ..) such that the current expression is denoted by
;;;  (nth ... (nth n3 (nth n2 (nth n1 <toplevel>)))).
;;;
;;;  Because it may be slow, it caches its result in case called
;;;  more than once.

(defvar *current-editor-path* nil)
(defvar *current-editor-path-time* nil)
(defvar *current-editor-path-bp* nil)

(defun current-editor-path ()
  (declare (values path toplevel?))
  (cond ((and *current-editor-path-time*
              (zwei:bp-= *current-editor-path-bp* (zwei:point))
              (>= *current-editor-path-time* (zwei:node-tick
                                              (zwei:bp-node
                                               (zwei:point)))))
         (values-list *current-editor-path*))
        (t
         (let ((fn (get 'current-editor-path (buffer-editing-type))))
           (let ((path (multiple-value-list
                        (if fn (funcall fn (zwei:point) '() fn)))))
             (setq *current-editor-path-time* nil) ;Mark dirty
             (setq *current-editor-path* path)
             (setq *current-editor-path-bp* (copy-buffer-pointer
                                             (zwei:point)
                                             ':normal))
             (setq *current-editor-path-time* (editor-current-time))
             (values-list path))))))

(comment ;obsolete? -kmp

 (defun (current-editor-path :ada) (&rest ignore) (values nil t))

 );tnemmoc

(defun (current-editor-path :ada) (&rest ignore)
  (condition-case ()
      (multiple-value-bind (start-pt end-pt)
          (bounds-of-definition)
        (cond ((not start-pt) (values nil t))
              (t
               (let ((pt (search-buffer-backward ";")))
                 (cond ((or (not pt)
                            (not (setq pt (forward-char pt)))
                            (zwei:bp-< pt start-pt)
                            (zwei:bp-< end-pt pt))
                        (values nil t))
                       (t
                        (setq pt (forward-char pt))
                        (let ((part1 (string-buffer-between start-pt pt))
                              (part2 (string-buffer-between pt end-pt)))
                          (let ((magic-text (string-append
                                             part1
                                             "{Zmacs-Cursor};"
                                             part2)))
                            (find-path (ada-parse "{Zmacs-Cursor}")
                                       (ada-parse magic-text))))))))))
    (sys:error (values nil nil))))

(defun find-path (marker sexp)
  (let ((*marker* marker))
    (declare (special *marker*))
    (values (*catch 'find-path (find-path-1 sexp '())))))

(defun find-path-1 (sexp path)
  (declare (special *marker*))
  (cond ((equal *marker* sexp) (*throw 'find-path (nreverse path)))
        ((atom sexp) nil)
        (t
         (do ((i 0 (1+ i))
              (s sexp (cdr s)))
             ((null s))
           (find-path-1 (car s) (cons i path))))))


(defun (current-editor-path :lisp) (start-pt path myself)
  (declare (special *kbe-role-marker*) (values path toplevel?))
  (let ((up (zwei:forward-up-list-or-string start-pt -1))) ;backward-up
    (cond ((not up)
           (ignore-errors
             (check-sectionization start-pt) ;Now while it's cheap...
             (with-buffer-pointer (bp start-pt)
               (check-sectionization (let ((pt (forward-sexp bp)))
                                       (if pt (backward-sexp pt))))
               (check-sectionization (backward-sexp bp))))
                                        ;needn't reverse it since we worked inside->out
           (cond ((or (not path)
                      (< (car path) 3)
                      (and (= (car path) 3)
                           (not (cdr path))
                           (not (nthcdr 4 (get-definition)))))
                  ;; If at toplevel, say we are actually at the
                  ;; end of the definition we are near. Eg,
                  ;; Toplevel of (DEFUN F (X) ) is the same as
                  ;; being (DEFUN F (X) **HERE**).
                  (let ((sexp (get-definition)))
                    (values (if sexp (list (length sexp)))
                            t))) ;pretend toplevel
                 (t (values path nil))))
          (t (do ((i 0 (1+ i))
                  (pt start-pt))
                 (nil)
               (let ((new-pt (zwei:forward-sexp pt -1 nil 0 nil t t)))
                 (cond (new-pt (setq pt new-pt))
                       (t
                        (setq i (ncons i))
                        ;; Hair to recognize #{...} syntax.
                        ;; Makes rash assumptions about the format
                        ;; of the KBE-#-COMMENT-MACRO expression.
                        (when (cond ((= (character *kbe-role-marker*) #/#)
                                     (and (char-equal (char-after up) #/#)
                                          (char-equal (char-after
                                                       (forward-char up))
                                                      #/{)))
                                    (t (char-equal (char-after up) #/{)))
                          (cond ((let ((bp (forward-sexp
                                            (forward-char
                                             (forward-char up)))))
                                   (zwei:search bp "," nil nil nil
                                                (forward-sexp up)))
                                 (cond ((= (car i) 0)
                                        (setq i (list* 1 0 nil)))
                                       (t
                                        (setq i (list* 2
                                                       (1- (car i))
                                                       nil)))))
                                (t
                                 (setq i (cons 2 i)))))
                        (return
                          (funcall myself
                                   up (nconc i path) myself))))))))))

;;;; Buffer Insertion/Deletion

;;; (INSERT-INTO-BUFFER-BEFORE-POINT text)
;;;
;;;  Inserts text at point, moving point.

(defun insert-into-buffer-before-point (text)
  (zwei:insert-moving (zwei:point) text))

;;; (INSERT-INTO-BUFFER-AFTER-POINT text)
;;;
;;;  Inserts text after point, leaving point fixed.
;;;
;;;  There's probably a better way to do this...

(defun insert-into-buffer-after-point (text)
  (zwei:insert (zwei:point) text))

;;; (INSERT-INTO-BUFFER-AFTER  pointer text)
;;; (INSERT-INTO-BUFFER-BEFORE pointer text)

(defun insert-into-buffer-before (bp text)
  (let ((cursor (zwei:point)))
    (let ((point-must-move? (zwei:bp-= bp cursor)))
      (zwei:insert-moving bp text)
      (if point-must-move? (move-point bp))
      bp)))

(defun insert-into-buffer-after (bp text)
  (zwei:insert bp text))

;;; (DELETE-CHARS-IN-SET set [pointer])
;;;
;;;  Deletes chars in the given SET from near the given POINTER,
;;;  which defaults to point.

(defun delete-chars-in-set (set &optional (pt (editor-point)))
  (zwei:delete-around set pt))

;;; (DELETE-FROM-BUFFER-BETWEEN pointer1 pointer2 [save?])
;;;
;;;  Deletes the region between pointer1 and pointer2, which need
;;;  not be in order.
;;;
;;;  If SAVE? is non-NIL, then the deleted section will be saved in
;;;  the kill ring.

(defun delete-from-buffer-between (p1 p2 &optional (save? nil))
  (cond (save? (zwei:kill-interval p1 p2 t))
        (t     (zwei:delete-interval p1 p2 t))))

;;; (FLUSH-CHARS char-set [pt])
;;;
;;;  Deletes all the characters in CHAR-SET on both sides of PT.

(defun flush-chars (char-set &optional (pt (zwei:point)))
  (zwei:delete-around char-set pt))

;;; (COPY-FROM-BUFFER-BETWEEN pointer1 pointer2)
;;;
;;;  Returns a copy of the given region.

(defun copy-from-buffer-between (bp1 bp2)
  (zwei:copy-interval bp1 bp2))

;;; (YANK-INTO-BUFFER-AT pointer node)

(defun yank-into-buffer-at (pointer node)
  (zwei:insert-interval pointer node))

;;; (YANK-INTO-BUFFER-AT-POINT node)
;;;
;;; Returns the bp at the end of inserted text.

(defun yank-into-buffer-at-point (node)
  (zwei:insert-interval (zwei:point) node))

;;; (STRING-BUFFER-BETWEEN point1 point2)

(defun string-buffer-between (point1 point2)
  (zwei:string-interval point1 point2))

;;; (FILL-BUFFER-BETWEEN point1 point2)
;;;
;;;  Does autofilling on a given region of the buffer.

(defun fill-buffer-between (pt1 pt2)
  (zwei:fill-interval pt1 pt2 t))

;;; (INDENT-THIS-LINE-FOR-LISP)

(defun indent-this-line-for-lisp () (zwei:com-indent-for-lisp))

;;;; BP manipulation

;;; (create-buffer-pointer line [index])
;;;
;;;  Returns a buffer-pointer to the given line. Index defaults to 0,
;;;  meaning the head of the line.

(defun create-buffer-pointer (line &optional (index 0))
  (zwei:create-bp line index))

;;; (copy-buffer-pointer bp)
;;;
;;;  Returns a copy of the given buffer-pointer.

(defun copy-buffer-pointer (pointer &optional status)
  (zwei:copy-bp pointer status))

;;;; BP movement

;;; (EDITOR-POINT)

(defun editor-point ()
  (zwei:point))

;;; (EDITOR-MARK)

(defun editor-mark ()
  (zwei:mark))

;;; (MOVE-POINT where)

(defun move-point (where)
  (zwei:move-bp (zwei:point) where))

;;; (MOVE-MARK where)

(defun move-mark (where)
  (zwei:move-bp (zwei:mark) where))

;;; (ESTABLISH-REGION bp1 bp2 [style])
;;;
;;;  Makes a region from BP1 to BP2.
;;;  STYLE can be used to set the style.
;;;   By default, NIL means invisible, T means always visible.
;;;   The editor can be independently extended to
;;;   understand other options without changing this function.

(defun establish-region (bp1 bp2 &optional (region-style t))
  (when (and bp1 bp2)
    (move-point bp1)
    (move-mark  bp2)
    (setf (zwei:window-mark-p zwei:*window*) region-style)
    (setq zwei:*mark-stays* t)
    (zwei:must-redisplay zwei:*window* zwei:dis-bps)))


;;;; Virtual movement

;;; These commands move without side-effecting. A new BP is returned. You
;;; have to do (MOVE-POINT ...) or (MOVE-MARK ...) to get the cursor
;;; there.

;;; We currently use the ZWEI up/down-list type commands. Note that these
;;; will not work correctly to go into readmacrofied regions. eg, down
;;; from before the ' of '(A B C) goes inside the paren by the A when
;;; conceptually it should go to before the Q in (QUOTE (A B C)), which
;;; is not really a place in the buffer at all. FORTUNATELY, the only
;;; interesting readmacros are #, which mostly introduces atoms and
;;; which we won't want to go inside of or ' which introduces quoted
;;; structure, which the coder never looks inside of. At some point, we
;;; may have to deal with #', but currently we don't, so we'll correct
;;; our up/down-list stuff to be more sensitive to this case when need
;;; presents itself...

;;; (BEGINNING-OF-LINE bp)

(defun beginning-of-line (&optional (pt (zwei:point)))
  (zwei:beg-line pt))

;;; (END-OF-LINE bp)

(defun end-of-line (&optional (pt (zwei:point)))
  (zwei:end-line pt))

;;; (FORWARD-DOWN-LIST bp)

(defun forward-down-list (bp) ;like zwei:com-down-list
  (zwei:forward-list bp 1 nil -1 t))

;;; (BACKWARD-UP-LIST bp)

(defun backward-up-list (bp) ;like zwei:com-backward-up-list
  (zwei:forward-up-list-or-string bp -1))

;;; (FORWARD-DEFUN bp)

(defun forward-defun (bp &optional (count 1))
  (zwei:forward-definition bp count))

;;; (BACKWARD-DEFUN bp)

(defun backward-defun (bp &optional (count 1))
  (zwei:forward-definition bp (- count)))

;;; (FORWARD-WORD bp [count])

(defun forward-word (bp &optional (count 1))
  (zwei:forward-word bp count))

;;; (BACKWARD-WORD bp [count])

(defun backward-word (bp &optional (count 1))
  (zwei:forward-word bp (- count)))

;;; (FORWARD-SEXP bp [count])
;;;
;;;  Goes forward count (default 1) s-expressions.

(defun forward-sexp (bp &optional (count 1))
  (zwei:forward-sexp bp count))

;;; (BACKWARD-SEXP bp [count])
;;;
;;;  Goes backward count (default 1) s-expressions.

(defun backward-sexp (bp &optional (count 1))
  (zwei:forward-sexp bp (- count)))

;;; (FORWARD-CHAR bp [count])
;;;
;;;  Goes forward (default 1) char(s).

(defun forward-char (bp &optional (count 1))
  (zwei:forward-char bp count))

;;; (BACKWARD-CHAR bp [count])
;;;
;;;  Goes backward (default 1) char(s).

(defun backward-char (bp &optional (count 1))
  (zwei:forward-char bp (- count)))

;;; (FORWARD-LINE pt count)

(defun forward-line (&optional (pt (zwei:point)) (count 1))
  (zwei:beg-line pt count))

;;; (BACKWARD-LINE pt count)

(defun backward-line (&optional (pt (zwei:point)) (count 1))
  (forward-line pt (- count)))

(defun end-of-definition-comments (&optional
                                     (name (current-editor-definition-name))
                                     (make-room? nil))
  (if (not name) (editor-barf "Not inside a definition."))
  (multiple-value-bind (best-guess string)
      (find-single-definition name)
    (if (not best-guess) (editor-barf string))
    (do ((bp (zwei:beg-line best-guess -1)
             (zwei:beg-line bp -1))
         (flag 0 (1+ flag)))
        (nil)
      (when (or (not bp)
                (neq (zwei:line-type (zwei:bp-line bp)) ':blank))
        (when (and make-room? (< flag 2))
          (insert-into-buffer-before best-guess (text "~V%" (- 3 flag)))
          (return (end-of-definition-comments name)))
        (return best-guess))
      (if (< flag 2) (setq best-guess bp)))))

(defun insert-comment-definition (name explanation &optional syntax)
  (when explanation
    (move-point
     (forward-char
      (insert-into-buffer-before
       (end-of-definition-comments name t)
       (format-explanation-for-comment explanation syntax))))))

;;;; Ordering

;;; (POINT-PRECEDES? bp1 bp2)

(defun point-precedes? (bp1 bp2)
  (zwei:bp-< bp1 bp2))

;;;; Search

;;; (SEARCH-BUFFER string [bp])

(defun search-buffer (string &optional (bp (zwei:point)) (times 1))
  (let ((pt bp) (rev? (minusp times)))
    (dotimes (i (abs times))
      (if pt (setq pt (zwei:search pt string rev?))))
    pt))

;;; (SEARCH-BUFFER-FORWARD string [bp])
;;;
;;;  Searches buffer forward for a given string. By default, searches
;;;  from the current point, but an initial bp can be specified.

(defun search-buffer-forward (string &optional (bp (zwei:point)))
  (zwei:search bp string))

;;; (SEARCH-BUFFER-BACKWARD string [bp])
;;;
;;;  Searches buffer backward for a given string. By default, searches
;;;  from the current point, but an initial bp can be specified.

(defun search-buffer-backward (string &optional (bp (zwei:point)))
  (zwei:search bp string t))

;;; (SEARCH-BUFFER-FORWARD-DELIMITED string [pt])
;;;
;;;  Searches the buffer forward for a given string bounded by
;;;  atom delimiters.

(defun search-buffer-forward-delimited (string
                                        &optional (bp (editor-point)))
  (do ((pt (search-buffer string bp) (search-buffer string pt))
       (len (string-length string)))
      ((or (not pt)
           (and (breakchar-p (char-after pt))
                (breakchar-p (char-before (backward-char pt len)))))
       pt)))

;;; (SEARCH-BUFFER-BACKWARD-DELIMITED string [pt])
;;;
;;;  Like SEARCH-BUFFER-FORWARD-DELIMITED, but works backward.

(defun search-buffer-backward-delimited (string
                                         &optional (bp (editor-point)))
  (do ((pt (search-buffer-backward string bp)
           (search-buffer-backward string pt))
       (len (string-length string)))
      ((or (not pt)
           (and (breakchar-p (char-before pt))
                (breakchar-p (char-after (forward-char pt len)))))
       pt)))

;;; (SEARCH-BUFFER-BETWEEN bp1 bp2 string [reverse?])

(defun search-buffer-between (bp1 bp2 string &optional (reverse? nil))
  (zwei:search bp1 string reverse? nil nil bp2))

;;;; Redisplay

;;;  ZWEI's redisplay level objects are constants bound to
;;;  the following global variables in the ZWEI package:
;;;
;;;     DIS-NONE          No redisplay needed.
;;;
;;;     DIS-MARK-GOES     No redisplay needed except maybe
;;;
;;;
;;;     DIS-BPS           Point and mark may have moved, but text is
;;;
;;;
;;;     DIS-LINE          Text in one line may have changed.
;;;
;;;
;;;
;;;
;;;     DIS-TEXT          Any text might have changed.
;;;
;;;     DIS-ALL           Global parameters of the window have changed.
;;;
;;;
;;;
;;;  This information was obtained by using M-. on ZWEI:DIS-NONE.
;;;  It should be periodically checked for correctness until a manual
;;;  is published declaring this sort of thing to be stable.

(defun editor-window-redisplay ()
  (zwei:must-redisplay zwei:*window* zwei:dis-text))

(defun editor-window-must-redisplay-rest-of-line (&optional
                                                    (bp (zwei:point)))
  (zwei:must-redisplay zwei:*window*
                       zwei:dis-line
                       (zwei:bp-line bp)
                       (zwei:bp-index bp)))

(defun redisplay-all-windows () (zwei:redisplay-all-windows t nil))

;;;; Mode line

(defun set-mode-line (data)
  (setq zwei:*mode-line-list*
        (cond ((atom data) (cons data (cdr zwei:*mode-line-list*)))
              (t data))))

(defmethod (zwei:top-level-editor :change-mode-line-list) (fn)
  (setq zwei:*mode-line-list* (funcall fn zwei:*mode-line-list*)))

(define-internal-option *zmacs-default-mode-line-name*
    "Editor name for UPDATE-ZMACS-MODE-LINE"
  "KBEmacs" :string)
                                        ;  "KBE//ZMACS" :string)

(defun update-zmacs-mode-line ()
  (send zwei:*zmacs-command-loop* ':change-mode-line-list
        #'(lambda (l)
            (let ((new (copylist l)))
              (if (and (stringp (car new))
                       (string-search "MACS" (car new)))
                  (setf (car new)
                        (text "~A " *zmacs-default-mode-line-name*)))
              (setq new (delq 'zwei:*buffer-modified-p* new))
              (let ((tail (or (member ") " new)
                              (member ")" new))))
                (when tail
                  (when (equal (car tail) ") ")
                    (push " " (cdr tail))
                    (push 'zwei:*buffer-modified-p* (cdr tail)))
                  (setf (car tail) ")")))
              new))) )

(define-system-initialization zmacs-mode-line
  (update-zmacs-mode-line))

;;;; In the name of abstraction

(defflavor vanilla-editor-frame () (zwei:zmacs-frame))

(defmethod (zwei:zmacs-frame :delegate) ()
  (or (send-any-superior-if-handles (send self ':superior) ':delegate)
      (editor-delegate)))

(defun editor-state ()
  (send zwei:*zmacs-window-process* ':whostate))

;;;; Comment stuff

(defun editor-comment-begin () zwei:*comment-begin*)

(defun editor-comment-end () zwei:*comment-end*)

;;;; Line validation

;; DWIM stuff for debugging. This should go away some day. --KMP

(define-KBE-option *Automatic-Lisp-Mode*
    "Automatically enter Lisp Mode for code buffers"
  t :boolean)

(define-KBE-option *Automatic-KBE-Package*
    "Automatically put code buffers in KBE package"
  t :boolean)

(defvar *known-modes* '(zwei:lisp-mode zwei:ada-mode))

(defun set-buffer-attribute (attribute value
                             &optional (attribute-string
                                        (string-capitalize-words
                                         attribute))
                               (value-string (string value)))
  (let ((zwei:*set-attribute-updates-list* t))
    (zwei:set-attribute-internal attribute
                                 attribute-string
                                 value-string
                                 value)
    (zwei:com-reparse-attribute-list)))

(defun set-buffer-mode (mode-keyword)
  (selectq mode-keyword
           (:ada (com-ada-mode))
           (otherwise (funcall (intern (text "COM-~A-MODE" mode-keyword) 'ZWEI))))
  (set-buffer-attribute ':major-mode (intern (text "~A-MODE" mode-keyword)
                                             'ZWEI)
                        "Mode" (string mode-keyword)))

(defun validate-buffer-mode (language &optional for-function)
  for-function ;ignored for now
  (set-buffer-mode language))

(defun validate-buffer-package ()
  (when (and *automatic-kbe-package* (not (eq package *kbe-package*)))
    (set-buffer-attribute ':package *kbe-package* "Package" "KBE")
    (zwei:sectionize-buffer zwei:*interval*)))

(defun enter-lisp-mode ()
  (zwei:com-lisp-mode)
  (zwei:com-update-attribute-list))

(defun check-sectionization (&optional (point (editor-point)))
  (when point
    (zwei:check-interval-sections
     (zwei:line-node (zwei:bp-line point)))))

(defun definition-last-modified-time (fn &optional type)
  (let ((node (find-single-definition-node fn type)))
    (if node (zwei:node-tick node))))

(defun editor-current-time () zwei:*tick*)

(defmacro define-editor-definition-type (name type)
  `(defprop ,name ,type zwei:definition-function-spec-type))

(defun real-editor-buffer? (x) (typep x 'zwei:buffer))

(defun assure-real-editor-buffer ()
  (if (not (real-editor-buffer? (current-editor-buffer)))
      (editor-barf "Not in correct buffer context.")))

(comment ;old definition

 (defun place-to-create-dummy-definition (name &optional type)
   type ;ignored
   (let ((data (suggested-code-home name)))
     (let ((buffer-name (get (locf data) ':buffer))
           (near        (get (locf data) ':near)))
       (let ((buffer  (if buffer-name (find-buffer buffer-name)))
             (near-bp (if near (find-single-definition near))))
         (cond ((and near-bp
                     buffer
                     (neq buffer (containing-buffer near-bp)))
                (buffer-end buffer))
               (near-bp
                (zwei:interval-last-bp
                 (zwei:bp-node near-bp)))
               (buffer
                (buffer-end buffer))
               (t
                (buffer-end (current-editor-buffer))))))))

 )

(defun place-to-create-dummy-definition (name &optional type)
  type ;ignored
  (let ((data (suggested-code-home name)))
    (let ((buffer-name (get (locf data) ':buffer))
          (near        (get (locf data) ':near)))
      (let ((buffer  (if buffer-name (find-buffer buffer-name))))
        (multiple-value-bind (start-pt end-pt)
            (if near (bounds-of-definition near))
          (copy-buffer-pointer ;be safe...
           (cond ((and start-pt
                       buffer
                       (neq buffer (containing-buffer start-pt)))
                  (buffer-end buffer))
                 ((and end-pt (forward-line end-pt)))
                 (buffer
                  (buffer-end buffer))
                 (t
                  (buffer-end (current-editor-buffer))))))))))

(defun buffer-editing-type (&optional (buffer zwei:*interval*))
  (or (get (send buffer ':get ':major-mode) 'zwei:editing-type)
      ':lisp))

(defun create-dummy-definition (name &optional type)


  (assure-real-editor-buffer)
  (if (not type) (setq type 'DEFUN))
  (let ((where (place-to-create-dummy-definition name type)))
    (zwei:check-interval-sections
     where
     (insert-into-buffer-before
      where
      (make-dummy-definition
       (buffer-editing-type (containing-buffer where)) name type)))
    (when (not (find-single-definition name) )
      (catch-error-restart (sys:error "Continue without sectionization.")
                           (cbarf "Try CHECK-INTERVAL-SECTIONS instead."
                                  "CHECK-INTERVAL-SECTIONS failed.")
                           (zwei:sectionize-buffer (containing-buffer where))))))
(defun make-dummy-definition (editing-type name type)
  (let ((fn (get 'make-dummy-definition editing-type)))
    (cond ((not fn)
           (editor-barf "Can't hack ~A definitions." editing-type))
          (t (funcall fn name type)))))
(defun (make-dummy-definition :lisp) (name type)
  (text "~%(~A ~S (...)  ...dummy definition...)~%"
        (string-downcase type)
        name) )
(defun (make-dummy-definition :ada) (name type)
  type  ; ignored
  (text "~%entry ~A;~%" name))

(defun validate-zmacs-buffers-datum (spec buffer-datum)
  (let (((type node) buffer-datum) )
    (let ((definition-line (send node ':definition-line)))
      (when (or (not (stringp definition-line))
                (eq (zwei:line-tick definition-line) 'zwei:deleted))
        (zwei:check-interval-sections node)
        (setq definition-line (send node ':definition-line)))
      (multiple-value-bind (fspec ftype)
          (with-local-buffer-pointer (bp
                                      (zwei:create-bp definition-line 0))
            (zwei:get-definition-function-spec bp))
        (unless (and (equal spec fspec)
                     (equal type ftype))
          (zwei:check-interval-sections (send node ':superior)))))))
(defun validate-zmacs-buffers-data (spec)
  (mapc #'(lambda (datum)
            (validate-zmacs-buffers-datum spec datum) )
        (si:function-spec-get spec 'zwei:zmacs-buffers)))
(defun find-single-definition-node (spec &optional type)
  (setq type (or type 'DEFUN)) ;allow NIL to mean DEFUN
  (validate-zmacs-buffers-data spec)
  (let ((result (mem #'(lambda (x y) (eq x (car y)))
                     type
                     (si:function-spec-get spec 'zwei:zmacs-buffers))))
    (setq result (del #'(lambda (ignore y)
                          (eq (zwei:line-tick
                               (send (cadr y) ':definition-line))
                              'zwei:deleted))
                      nil result))
    (cond ((or (not result)
               (assq type (cdr result)))
           (values nil (text "~:[No~;Too many~] ~A ~S's found."
                             result
                             (si:definition-type-name type)
                             spec)))
          (t
           (values (cadr (car result)) nil)))))
(defun find-single-definition (spec &optional type)
  (multiple-value-bind (node error-string)
      (find-single-definition-node spec type)
    (values (if node (zwei:create-bp (send node ':definition-line) 0))
            error-string
            node)))
(defun get-definition (&optional (fn (current-editor-definition-name) )
                         (type nil))
  (let ((bp (find-single-definition fn type) ))
    (cond ((not bp) nil)
          (t
           (let ((result (get-possibly-cached-definition fn type bp)))
             (if (atom result) nil result))))))  ;hack to bypass fact
                                        ; this loses on ADA for now
(defun get-definition-text (&optional (fn (current-editor-definition-name) )
                              (type nil))
  type  ; ignored
  (multiple-value-bind (start-bp end-bp)
      (bounds-of-definition fn)
    (if (null start-bp) ""
        (string-buffer-between start-bp end-bp))))
(defun get-possibly-cached-definition (fn type bp)
  (when (not type)
    (setq type 'defun))

  (let ((db (get fn 'cached-definitions)))
    (when (not db)
      (setq db (ncons nil))
      (setf (get fn 'cached-definitions) db))
    (let ((data (get db type)))
      (when (not data)
        (setq data (ncons nil))
        (setf (get db type) data))
      (let ((last-changed (zwei:node-tick (zwei:bp-node bp))))
        (when (or (not (cdr data))
                  (not (eq (car data) last-changed)))
          (setf (cdr data) (really-get-definition fn type bp))
          (setf (car data) last-changed) )
        (cdr data)))))
(define-internal-option *debug-parser* "Debug parser problems"
  nil :boolean)
(defun really-get-definition (fn type bp)
  type ; ignored
  (when-error (let ((zwei:*interval* (containing-buffer bp)))
                (let ((package
                       (buffer-package zwei:*interval*)) )
                  (multiple-value-bind (start-bp end-bp)
                      (bounds-of-definition fn bp)
                    (let ((fn (get 'get-definition (buffer-editing-type))))
                      (cond ((not fn) (ferror "Bad editing mode."))
                            (t
                             (condition-bind-if
                              *debug-parser*
                              ((sys:error #'(lambda (cond)
                                              (format debug-io "~&~A"
                                                      cond)
                                              (funcall #'dbg)
                                              nil)))
                              (funcall fn start-bp end-bp))))))))
              (barf "Unable to parse definition of ~S" fn)))
(defun (get-definition :lisp) (start-bp end-bp)
  (read-hacking-/{ (zwei:interval-stream start-bp end-bp t t)))
(defun (get-definition :ada) (start-bp end-bp)
  (ada-parse (zwei:string-interval start-bp end-bp)))
;; We might want to make this use *FILL-COLUMN* sometime, but
 ;;; that's too hard to access since the *FILL-COLUMN* for a
  ;;; different buffer is only accessible by making that buffer
  ;;; current... Also, this mechanism may be useful in addition
;; to (not necessarily "instead of") checking fill column,
(define-KBE-option *max-buffer-line-length*
    "Max line length for grinding code in editor"
  79.  :number)
(define-kbe-option *kbe-user-role-marker* "KBE role marker (visible)"
  "#{" :assoc '(("{" . "{") ("#{" . "#{")))
(defun grind-code-for-buffer (code buffer)
  (let ((pp:*print-right-margin*
         (or pp:*print-right-margin*
             (min (1-  ;guard against fenceposts
                   (// (send terminal-io  ':size)
                       (tv:font-char-width
                        (buffer-font buffer))))
                  *max-buffer-line-length*) ))
        (package (buffer-package buffer))
        (radix (buffer-radix  buffer)))
    (let ((base     (or radix 10.))
          (*nopoint (if radix t))

          (*kbe-role-marker* *kbe-user-role-marker*) )
      (declare (special *kbe-role-marker*) )
      (with-output-to-string (out)
        (print-code code out)))))
(putprop 'define-cliche 'defun 'zwei:definition-function-spec-type)
(defun infer-definition-type (code)
  (if (not (atom code))
      (or (get (car code)
               'zwei:definition-function-spec-type)
          (car code))))
(defun replace-definition (fn code &optional type)
  (if (not type) (setq type (infer-definition-type code)))
  (multiple-value-bind (pt error-message)
      (find-single-definition fn type)
    (cond ((not pt)
           (values nil
                   (text "Can't update ~S's definition: ~A"
                         fn error-message)))
          ((equal (get-definition fn) code)
           (values ':ignored
                   (text "~S's definition didn't need to be updated." fn)))
          (t
           (let ((zwei:*interval* (containing-buffer pt)))
             (let ((text (grind-code-for-buffer code zwei:*interval*)))
               (multiple-value-bind (start-pt end-pt)
                   (bounds-of-definition fn pt)
                 (with-buffer-pointer (saved-start-pt start-pt ':moves)
                   (with-buffer-pointer (saved-end-pt end-pt ':moves)
                     (with-buffer-pointer (temp-pt
                                           (insert-into-buffer-before
                                            start-pt
                                            text)
                                           ':moves)
                       (if (zwei:bp-= (delete-from-buffer-between
                                       temp-pt
                                       saved-end-pt)
                                      (zwei:point))
                           (zwei:move-bp (zwei:point) saved-start-pt))
                       (check-sectionization temp-pt)
                       (check-sectionization saved-start-pt)
                       (check-sectionization saved-end-pt)
                       (when  (not (find-single-definition fn))
                         ;; Should never get here (ha,ha) --kmp
                         (when *debug*
                           (editor-echo-output
                            "Sectionizing buffer to find ~S." fn))
                         (zwei:sectionize-buffer zwei:*interval*))))))
               (editor-window-redisplay)
               (values ':replaced
                       (text "Definition of ~S~@[ (in ~A)~] updated."
                             fn
                             (buffer-name-if-relevant
                              zwei:*interval*)))))))))
(defun bounds-of-definition (&optional
                               (fn (current-editor-definition-name) )
                               (pt (find-single-definition fn)))
  (if (null pt)
      (values nil nil)
      (let ((type (buffer-editing-type (containing-buffer pt))))
        (let ((handler (get 'bounds-of-definition type)))
          (cond ((not handler)
                 (editor-barf "Can't hack ~A editing." type))
                (t (funcall handler fn pt)))))))
(defun (bounds-of-definition :lisp) (fn pt)

  fn  ; ignored
  (values pt (forward-sexp pt)))
(defun (bounds-of-definition :ada) (fn pt)
  (let ((prev-end (or (search-buffer-backward '#.(text "~%end") pt)
                      (buffer-start (containing-buffer pt)))))
    (let ((start-pt (search-buffer-between
                     prev-end pt '#.(text "~%with"))))
      (unless start-pt
        (setq start-pt (search-buffer-between
                        prev-end pt '#.(text "~%use"))))
      (setq start-pt (if start-pt (beginning-of-line start-pt) pt))
      (let ((end-pt (if (and (eq pt start-pt)
                             (ignore-errors
                               (let ((pt (zwei:forward-over
                                          '(#\sp #\tab)
                                          (forward-sexp pt 2))))
                                 (or (= (char-after pt) #/;)
                                     (= (char-after (forward-sexp pt))
                                        #/;)))))
                        (end-of-line start-pt)
                        (or (search-buffer-forward (text "end ~A;" fn)
                                                   start-pt)
                            (editor-barf "Missing /"end ~A;/"" fn)))))
        (values start-pt end-pt)))))
(define-editor-command com-mark-this-definition (ignore) ()
  "Mark the definition at the cursor."
  (multiple-value-bind (start-pt end-pt)
      (bounds-of-definition (current-editor-definition-name) )
    (establish-region start-pt end-pt))
  nil) ;hopefully no change to buffer

=


;;; Misc

;;; Terminal interaction
(defun editor-last-command-char ()
  zwei:*last-command-char*)
;;; Self-insertion
(defun editor-insert-last-command-char ()
  (zwei:com-standard))
;;; Enters a mail buffer to a given recipient beginning with given text.
(defun send-mail (rcpt initial-text)
  (zwei:com-mail-internal t rcpt initial-text))

;;; Utilities supporting completion
;;; Utility macros
;;; (BREAKCHAR-P char)
;;; Returns true if char is an atom break character according to
;;; the active syntax table.
(defun breakchar-p (x)
  (zwei:fsm-atom-delimiter-char-p x))
(defun uppercase-p (x)
  (and (^] x #/A) (^\ x #/Z)))
(defun char-after (bp) (ldb %%ch-char (zwei:bp-char bp)))
(defun char-before (bp) (ldb %%ch-char (zwei:bp-char-before bp)))
;;; (BOUND-TOKEN)
;;; Returns NIL if there is no obvious token which completion should
;;; apply to. Otherwise, returns three values:
;;; 1 - A string indicating what is probably right to complete
;;; 2 - The starting bp of that string.
;;; 3 - The ending bp of that string.
;;; The actual token chosen is not quite a lisp symbol. It is a lisp
;;; symbol minus any funny chars on one or the other end. Hence,
;;; being near "*FOO*" will return info just about the "FOO". This
;;; heuristic can be toyed with, if need be, by changing the funny
 ;;; sequence of. forward/backward-sexp/word in this definition
(defun bound-token ()
  (let ((pt (zwei:point)))
    (if (not (breakchar-p (zwei:bp-char pt)))
        (setq pt (zwei:forward-sexp pt)))
    (cond ((not pt) nil)
          ((breakchar-p (zwei:bp-char-before pt)) nil)
          (t (let ((start-bp (when-error
                              (zwei:forward-sexp
                               (forward-word
                                (zwei:forward-sexp pt -1 nil 0 nil nil t))
                               -1 nil 0 nil nil t)
                              nil)))
               (let ((end-bp (when-error
                              (forward-sexp
                               (backward-word (forward-sexp start-bp)))
                              nil)))
                 (if end-bp
                     (values (zwei:string-interval start-bp end-bp)
                             start-bp end-bp))))))))

;;; Comtabs
(defmacro define-major-mode (mode-name
                             mode-name-string
                             documentation
                             &body  body)
  (let ((mode-function-name (intern (text "COM-~A" mode-name)
                                    (symbol-package mode-name) ))
        (zwei-var (intern (get-pname mode-name)
                          "ZWEI"))) ;MUST be on ZWEI packagel
    `(progn 'compile
      (zwei:defmajor ,mode-function-name ,zwei-var ,mode-name-string
       ,documentation ()
       ,@body)
      (zwei:set-comtab zwei:*standard-comtab*
       '()
       (zwei:make-command-alist
        `(,mode-function-name) )))))
(defvar *kbe-mode-key-bindings*
  '(#\alt     com-complete-KBE-word
    #\help    com-document-KBE-completions
    #\space   com-auto-complete-KBE-word
    #/,       com-auto-complete-KBE-word
    #/.       com-auto-complete-KBE-word
    #/!       com-auto-complete-KBE-word
    #/?       com-auto-complete-KBE-word) )
;;; %%% Release 5 %%%
;;; This copies functionality from the Lisp comtab.
;;; When Rel 5 becomes available, there is a much cleaner way
;;; to do this since modes are finally flavorized.
(define-major-mode kbe-mode "KBE"
  "Sets things up for talking to the Knowledge-Based Editor."
  (setq zwei:*space-indent-flag* t)
  (setq zwei:*paragraph-delimiter-list* nil)
  (setq zwei:*comment-start* 'zwei:lisp-find-comment-start-and-end)
  (zwei:set-comtab zwei:*mode-comtab* *kbe-mode-key-bindings*)
  (zwei:set-comtab
   zwei:*mode-comtab* '(#\tab            zwei:com-indent-for-lisp
                        #\rubout         zwei:com-tab-hacking-rubout
                        #\control-rubout zwei:com-rubout
                        #\meta-Z         zwei:com-compile-and-exit
                        #\control-meta-Z zwei:com-evaluate-and-exit)))
(define-zwei-command com-not-implemented-for-this-mode (ignore) ()
  "Complain that this key is not defined here."
  (editor-barf "~@:C is not yet implemented for ~A mode."
               (editor-last-command-char)
               (symeval zwei:*major-mode*)))
(define-editor-command com-evaluate-this-definition (ignore) ()
  "Evaluate the definition at the cursor."
  (let ((name (current-editor-definition-name) ))
    (let ((form (get-definition name) ))
      (cond ((not form) (beep))
            (t (editor-echo-output "Evaluating definition of ~S  ..."
                                   name)
               (eval form)
               (editor-echo-output "Done.")))))
  nil)  ;hopefully no change to buffer
(define-editor-command com-eval-and-compile-this-definition (ignore) ()
  "Eval the definition at the cursor.
 If the definition name is fbound when done, compile it as well."
  (let ((name (current-editor-definition-name) ))
    (let ((form (get-definition name)))

      (cond ((not form) (beep))
            (t (editor-echo-output "Compiling definition of ~S ..."
                                   name)
               (let ((result (eval form)))
                 (cond ((symbolp result)
                        (cond ((and (eq result (cadr form) )
                                    (fboundp result))
                               (compile result)))
                        (editor-echo-output "Done."))
                       (t
                        (editor-echo-output "Failed."))))))))
  nil)  ;hopefully no change to buffer
(defun (com-mode-break :lisp) ()
  (break "in Lisp"))
(comment ;why doesn't this work? --kmp 6/4/84
 (define-editor-command com-mode-break (ignore) ()
   "Enter a debugging breakpoint in this buffer's language."
   (unwind-protect (let ((zwei:*inside-break* t))
                     (let ((terminal-io zwei:*typeout-window*))
                       (funcall (or (get 'com-mode-break (buffer-editing-type))
                                    (get 'com-mode-break ':lisp)))))
     (send zwei:*zmacs-command-loop*
           ':expose-mode-line-window))
   (send zwei:*typeout-window* ':make-complete)
   nil) ;No change to buffer (probably)
 );tnemmoc

(define-major-mode ada-mode "ADA"
  "Sets things up for editing ADA code."
  (zwei:set-comtab zwei:*mode-comtab*
                   '(#\tab zwei:com-indent-relative
                     #\c-m-H com-mark-this-definition
                     #\c-sh-E com-evaluate-this-definition
                     #\c-sh-C com-eval-and-compile-this-definition
                     #\m-sh-E com-not-implemented-for-this-mode
                     #\m-sh-C com-not-implemented-for-this-mode))
  (setq zwei:*space-indent-flag* t)
  (setq zwei:*paragraph-delimiter-list* nil)
  (setq zwei:*comment-start* "--")
  (setq zwei:*comment-begin* "-- ")
  (setq zwei:*comment-end* "")
  (setq zwei:*comment-column* (* 60. 6))
  (zwei:set-char-syntax (zwei:char-syntax #\space zwei:*list-syntax-table*)
                        zwei:*list-syntax-table* #/;))

;;; Set up a canonical file type called KBE for KBE command files
(fs:define-canonical-type :KBE "KBE"
  ((:tops-20 :tenex :unix :vms) "KBE"))
 ;;; Set up to have KBE Mode always entered when a .KBE file is read in.
(push '(:KBE . :KBE) fs:*file-type-mode-alist*)
(defvar *kbe-input-window-comtab*
  (zwei:copy-comtab zwei:*standalone-comtab*) )
(zwei:set-comtab *kbe-input-window-comtab*
                 *kbe-mode-key-bindings*)
(zwei:set-comtab *kbe-input-window-comtab*
                 '(#\end           com-submit-and-kill-KBE-requests
                   #\abort          com-kill-KBE-requests
                   #\control-meta-Y com-yank-pop-KBE-request

                   #\control-C      com-yank-pop-KBE-request) )
(defvar *kbe-mini-buffer-multi-line-comtab*
  (zwei:copy-comtab zwei:*mini-buffer-multi-line-comtab*) )
(zwei:set-comtab *kbe-mini-buffer-multi-line-comtab*
                 *kbe-mode-key-bindings*)
(defvar *kbe-mini-buffer-comtab*
  (zwei:copy-comtab zwei:*mini-buffer-comtab*) )
(zwei:set-comtab *kbe-mini-buffer-comtab*
                 *kbe-mode-key-bindings*)

;;; Streams
;;; (MINI-BUFFER-STREAM prompt multi-line? initial-contents initial-pos)
;;; Prompts for input from some sort of mini-buffer with the given
;;; string as a prompt. If MULTI-LINE? is non-nil, which it defaultly
;;; is, the input prompted for may extend over more than one line. The
;;; initial contents of the input buffer, INITIAL-CONTENTS, and location
;;; of the cursor in that buffer, INITIAL-POS, may also be specified.
;;; The return value is a stream to the user's input.
(defvar *multi-mode-info*
  (list ':right-flush
        (text " (~:C completes, ~:C ends)" #\alt #\end)))
(defvar *single-mode-info*
  (list ':right-flush
        (text " (~:C completes, ~:C ends)" #\alt #\return)))
(defun mini-buffer-stream (prompt
                           &optional
                             (multi-line? t)
                             (initial-contents nil)
                             (initial-pos
                              (if initial-contents
                                  (string-length initial-contents))))
  (multiple-value-bind (nil nil node)
      (zwei:edit-in-mini-buffer
       (cond (multi-line? *kbe-mini-buffer-multi-line-comtab*)
             (t           *kbe-mini-buffer-comtab*) )
       initial-contents
       initial-pos
       (list prompt (cond (multi-line? *multi-mode-info*)
                          (t          *single-mode-info*))))
    (zwei:interval-stream node) ))
                                        ;(rest-of-interval-stream (zwei:interval-first-bp node))?

 ;;; (BUFFER-STREAM buffer-name prompt create?)
 ;;; Returns  a stream to some editor buffer given by a string,
 ;;; BUFFER-NAME. See FIND-BUFFER for arg convention info.
(defun buffer-stream (&optional (buffer-name zwei:*interval*)
                        (prompt nil)
                        (create? t))
  (zwei:rest-of-interval-stream
   (zwei:interval-first-bp (find-buffer buffer-name prompt create?))))
 ;;; (REST-OF-BUFFER-STREAM pt)
 ;;; Returns a stream to the rest of some editor buffer after some
 ;;; given point.
(defun rest-of-buffer-stream (&optional (pt (zwei:point)))
  (zwei:rest-of-interval-stream pt))

;;; (GET-CURRENT-TOKEN-COMPLETION table)
;;; The table should be something created by MAKE-COMPLETION-TABLE.
;;; If the first value returned is NIL, then the second value will
;;; be an error message saying why no completion was found.
;;; If the first value returned is non-NIL, then it will be a bp
;;; where the second value, a string, should be inserted. The third
;;; will be the token originally completed.
(defun get-current-token-completion (table)
  (declare (values bp-or-NIL completion-or-diagnostic initial-string-or-n))
  (multiple-value-bind (token start-bp end-bp)
      (bound-token)
    start-bp ; ignored
    (cond ((not token)
           (values nil
                   "Can't  do completion at this buffer position."
                   0))
          (t
           (let ((completion (get-completion-from-table token table)))
             (cond ((> (string-length completion) 0)
                    (values end-bp completion token))
                   (t
                    (let ((options (possible-completions token table)))
                      (cond ((not options)
                             (values
                              nil
                              (text "No valid completions for /"~A/""
                                    token)
                              0))
                            ((not (cdr options))
                             (values
                              nil
                              (text "Unique match: /"~A/"" (car options) )
                              1))
                            ((member token options)
                             (values
                              nil
                              (text "Exact match of /"~A/" ~
                                        shadows ~{~A~^, ~}."
                                    token
                                    (remove token options))
                              (- (length options))))
                            (t
                             (values
                              nil
                              (text "Completion ambiguity: ~A => ~
                                        ~{~A~^, ~}."
                                    token options)
                              (length options))))))))))))
 ;;; (COMPLETE-CURRENT-TOKEN table)
 ;;; Causes modifications to the buffer at the active point inserting
 ;;; any completion characters possible. If no completion characters
 ;;; can be inserted, an error will result unless an error-p arg of NIL
 ;;; is specified. Return values are the number of possible matches and
 ;;; an error string (or NIL if no error).
(defun complete-current-token (table &optional (error-p t))
  (declare (values n-matches lose-msg) )
  (multiple-value-bind (x y z)
      (get-current-token-completion table)
    (cond (x (insert-into-buffer-before x y)
             (values 1 nil))

          (error-p (editor-barf y)
                   (values z y)) ;should never get to here, but...
          (t (values z y)))))
;;; (DOCUMENT-COMPLETIONS table)
;;; Types out in the editor echo area what effect completion would
;;; have at the current buffer position.
(defun document-completions (table)
  (multiple-value-bind (x y z)
      (get-current-token-completion table)
    (cond (x (editor-echo-output "Completion here will change ~A => ~A~A"
                                 z z y))
          (t (editor-echo-output y)))))

;;; Input window -- a kind of standalone editor
(defflavor edit-window () (margin-scroll-bar-mixin
                           zwei:standalone-editor-window) )
(defflavor edit-window-input-commands-mixin () ()
           (:required-flavors zwei:editor)
           (:default-init-plist :*comtab* *kbe-input-window-comtab*) )

;;; Stream into buffer's region if any
(defun region-stream ()
  (cond ((zwei:window-mark-p zwei:*window*)
         (zwei:interval-stream (zwei:point)
                               (zwei:mark)
                               (zwei:bp-< (zwei:point) (zwei:mark))))
        (t nil)))
(defun interval-stream (bp1 bp2)
  (zwei:interval-stream bp1 bp2))

;;; Recentering Windows
(defun move-point-to-top-of-window (bp)
  (zwei:recenter-window zwei:*window* ':start bp))

;;; Command Type
;;; ZWEI keeps track of our current and previous command type so that
;;; we can find out what kind of command was done last. This is good
;;; for things like finding out when doing a m-Y that we had just done
;;; a c-Y before, or for finding out when doing c-m-Y if we have just
;;; done a c-m-Y already before that.
(defun set-editor-command-type (type)
  (setq zwei:*current-command-type* type))
(defun last-editor-command-type ()
  zwei:*last-command-type*)

;;; Support Buffers
(defun push-possibilities-buffer (action type &rest set-options)
  (assure-real-editor-buffer)
  (lexpr-funcall-in-editor #'zwei:push-possibilities-buffer
                           action type set-options))

(defun initialize-editor-window (window)
  ;; The editor needs to get selected so it can create a buffer
  ;; for the window, etc. It is important in all this to give
  ;; windows enough time to switch so we don't deadlock...
  (send window ':select) ;Allow initialization
  (process-wait "Select Editor" ;Await editor selection
                #'(lambda (window)
                    (do ((w tv:selected-window (send w ':superior)))
                        ((not w) nil)
                      (if (eq w window) (return t))))
                window)
  (funcall-in-editor #'redisplay-all-windows) ;Maybe wait some more
  (let ((tw (send tv:selected-window ':send-if-handles ':typeout-window)))
    (if tw (send tw ':set-font-map (list tv:*default-font*))))
  (let* ((buffer (funcall-in-editor #'current-editor-buffer))
         (buffer-name (send buffer ':name)))
    (when (and (string-equal buffer-name "*Buffer-" 0 0 8)
               (not (string-equal buffer-name "*Buffer-1*")))
      (funcall-in-editor #'make-buffer-current
                         (funcall-in-editor #'find-buffer "*Buffer-1*"))
      (funcall-in-editor #'kill-buffer buffer)
      (funcall-in-editor #'redisplay-all-windows)))
  (send terminal-io ':select)) ;Take back the terminal

;;; ADA sectionization support
;;; %%% Release 5 %%%
;;; The following magic works with <KMP,LISPM, BUGS, FIXES>GET-SECTION-NAME.LISP
;;; in order to define a minimal sectionization strategy for ADA mode
;;; buffers.
(defprop zwei:ada-mode :ada zwei:editing-type)
(defun (zwei:get-section-name :ada) (line bp)
  (declare (return-list fspec type str error-p))
  (let ((type nil))
    (if (or (%string-equal line 0 "procedure" 0 9) ; ADA procedure
            (%string-equal line 0 "function" 0 8)   ; ADA function
            (%string-equal line 0 "package" 0 7)    ; ADA package           :
            (%string-equal line 0 "cliche" 0 6)     ; Cliche
            (%string-equal line 0 "entry" 0 6))     ; Dummy definition
        (get-ada-function-spec (zwei:move-bp bp line 0) type)
        (values nil nil nil t))))
(defun get-ada-function-spec (bp &optional type &aux bp1)
  (zwei:atom-word-syntax-bind
    ;;; Now get the second word after BP.
   (if (and (setq bp (zwei:forward-word bp))
            (setq bp (zwei:forward-over zwei:*blanks*  bp))
            (setq bp1 (zwei:forward-word bp)))
       (let ((string (zwei:string-interval bp bp1)))
         (values (let ((package (buffer-package  (containing-buffer bp))))
                   (read-from-string  string) )
                 (or type  'DEFUN)
                 string))
       (values nil nil nil t))))

;;; Local Modes:;
;;; Mode:LISP;
;;; Lisp WHEN Indent:1;
;;; Lisp UNLESS Indent:1;
;;; Ends;
