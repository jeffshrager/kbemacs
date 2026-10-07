;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>EDIT-ACTOR.LISP.1130, 13 December 1984 14:15:26
;;; Knowledge-Based_Editor_Jan-1985_volume_4.pdf, PDF pages 147-158.
;;;
;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.
;;;
;;; EDIT-ACTOR: The "Edit" Frame's associated Actor.

;;;; Option Variables

(define-internal-option *pending-code-notification*
    "Notification of available code"
  #+3600 "the screen flashes"
  #-3600 "the tone sounds"
  :sexp)

(define-internal-option *pending-typeout-notification*
    "Notification of pending typeout"
  #+3600 "the screen flashes"
  #-3600 "the tone sounds"
  :sexp)

;;; EDIT-MIXIN

(defflavor edit-mixin () ())

(define-behavior (edit-mixin answer) (the-answer)
  (let ((requests (send self ':pending-requests)))
    (with-lock ((car requests) :whostate "Await Queue")
      (cond ((stringp the-answer)
             (push `(:text :text ,the-answer) (cdr requests)))
            ((not (atom the-answer))
             (push the-answer (cdr requests))))
      (when (and (get (cadr requests) ':text)
                 *pending-typeout-notification*
                 (not (editor-awaiting? self)))
        (make-noise-about 'display-ready)))))

;;;; EDITING-ACTOR
;;;
;;;  This guy is responsible for managing queues going to/from a normal
;;;  full-editor window such as Zwei.
;;;
;;;  Notes:
;;;
;;;   There is some question about whether the same actor ought to serve
;;;   all edit windows. Do people think of different editor windows as
;;;   different entities? Would they if they could? For now we punt this
;;;   issue and make actors and windows be one-to-one since the simple
;;;   case is of just one edit window. Changing things later probably
;;;   only involves very local changes to this section where messages
;;;   are received and dispatched.
;;;
;;;   Also, PENDING-REQUESTS may get hacked both from the background
;;;   window and the foreground window. Really the foreground
;;;   window should have its own delegate and should interface
;;;   via real message passing also. When this is done, the
;;;   cheap locking strategy used with having PENDING-REQUESTS
;;;   be (lock . queue) can be changed into a simpler lock-free
;;;   strategy, allowing locking to be a side-effect of mailbox
;;;   communication.

(defflavor editing-actor ((suggested-focus nil)
                          (pending-requests (ncons nil)))
  (edit-mixin employer-mixin standard-actor)
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

;;; Possibilities stuff

(defvar *awaiting* (list nil nil))

(defun editor-awaiting? (me)
  me ;ignored
  (car *awaiting*))

(comment
 (let ((state (editor-state)))
   (and (string-equal state "Await " 0 0 6)
        (send me ':name? (substring state 6))))
 );ntemmoc

;;; >Report status.

(define-interpretation (editing-actor report) ()
  (let ((base 10.) (*nopoint t))
    (with-output-to-string (str)
      (format str "I have ~A pending edit request~:P to perform.~
                  ~@[~%~A~]"
              (let ((n (length (cdr (send self ':pending-requests)))))
                (cond ((zerop n) "no")
                      (t n)))
              (let ((apprentice (send self ':associated-apprentice)))
                (when apprentice
                  (text "~A: ~A"
                        apprentice
                        (ask apprentice task "Report status."))))))))


;;; CODER-CONFIRMATION name new-code
;;;
;;;  This says that coding has occurred and some new piece of code is now
;;;  available for use.

(define-noise code-ready function-name)

(define-behavior (editing-actor coder-confirmation) (fn-name code)
  (let ((old-belief (send self ':belief ':code fn-name)))
    (send self ':believe ':code fn-name code)
    (let ((new-definition? (not (equal old-belief code))))
      (when new-definition?
        (send self ':update-definition-info fn-name ':code code)))))

;;; DOCUMENT-CODE name documentation
;;;
;;;  This provides documentation for a given piece of code.

(define-behavior (editing-actor document-code) (fn-name doc)
  (send self ':update-definition-info fn-name ':documentation doc))

(defmethod (editing-actor :update-definition-info) (fn-name marker value)
  (let ((requests (send self ':pending-requests)))
    (with-lock ((car requests) :whostate "Await Queue")
      (let ((entry (dolist (item (cdr requests))
                     (if (eq (get item ':name) fn-name) (return item)))))
        (cond ((not entry) (push (list ':definition
                                       ':name fn-name
                                       marker value)
                                 (cdr requests)))
              (t
               (putprop entry value marker))))))
  (when (and *pending-code-notification*
             (not (editor-awaiting? self)))
    (make-noise-about 'code-ready fn-name)))

;;; >Typeout "...string...".
;;;
;;;  This requests that an actor do literal typeout of the text in
;;;  a given string.

(define-noise display-ready)

(define-interpretation (editing-actor typeout) (string)
  (let ((requests (send self ':pending-requests)))
    (with-lock ((car requests) :whostate "Await Queue")
      (push `(:text :text ,string) (cdr requests))))
  (when (and *pending-typeout-notification*
             (not (editor-awaiting? self)))
    (make-noise-about 'display-ready)))

(comment ;think about this. hint: it doesn't work. -kmp

 (define-interpretation (editing-actor analyze) (reference)
   (let ((cmd (cond ((eq (car reference) 'code)
                     (unparse-command `(analyze ,reference)))
                    (t
                     (text "Analyze $~S"
                           (let ((ref-string (unparse-command reference)))
                             (or (get (task (text "How is ~A defined?"
                                                  ref-string))
                                      ':definition)
                                 (complain "I don't know about ~A"
                                           ref-string))))))))
     (send self ':defer-task cmd)))

 )

(define-interpretation (editing-actor how) (toplevel-reference)
  (let ((result (send self ':belief ':code
                      (name-symbol (caddr toplevel-reference)))))
    `(:code
      :text ,(cond (result (let (prinlevel prinlength) (text "~S" result)))
                   (t (text "I don't know the definition of ~A"
                            (unparse-command toplevel-reference))))
      :definition ,result)))

;;; SUGGEST-FOCUS reference-list path-list
;;;
;;;  This accepts a list of references to focus on. We should define what
;;;  it means to focus on things. Maybe it should just change our local
;;;  version of IT or something. Who knows. Anyway, ignore this for now.

(define-behavior (editing-actor suggest-focus) (data)
  (send self ':set-suggested-focus data))

(define-behavior (editing-actor suggested-focus) ()
  (return (send self ':suggested-focus)))

(defvar *initial-editing-actor* nil)

(define-system-initialization editing-actor
  (setq *initial-editing-actor* (make-instance 'editing-actor)))

(defflavor editing-actor-secretary ()
  (edit-mixin employer-mixin standard-actor)
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:default-init-plist
      :associated-apprentice (send *initial-editing-actor* ':name)
    :window-flavor  'edit-frame))

(defwhopper (editing-actor-secretary :submit-by-proxy) (x &optional await?)
  (cond (await?
         (with-lock ((car *awaiting*) :whostate "Await editor")
           (continue-whopper x await?)))
        (t
         (continue-whopper x await?))))

(defmethod (editing-actor-secretary :next-pending-request) ()
  (let ((requests (send self ':pending-requests)))
    (with-lock ((car requests) :whostate "Await Queue")
      (pop (cdr requests)))))

(define-KBE-option *focus-style* "Focus style"
  ':temporary
  :assoc '(("Invisible" . nil)
           ("Temporary" . :temporary)
           ("Permanent" . t)))

(defvar *old-definition* nil)

                                        ;these two hook fns assume they run without the user changing
                                        ;anything relevant in between.

(defun default-mark-pre-hook (data)
  (let ((fn (get (locf data) ':name)))
    (setq *old-definition* (get-appropriate-definition fn))))

(defun default-mark-post-hook (data)
  (declare (special *input-language*))
  (caseq *input-language*
         (ada (set-cursor-for-ada data))
         (T (set-cursor-for-lisp data))))

(defun get-appropriate-definition (fn)
  (declare (special *input-language*))
  (when fn
    (caseq *input-language*
           (ada (get-definition-text fn))
           (T (get-definition fn)))))

(defun set-cursor-for-lisp (data)
  (let* ((fn (get (locf data) ':name))
         (path (get (locf data) ':path))
         (highlight? (get (locf data) ':force-highlight))
         (current-definition (get-appropriate-definition fn)))
    (when fn
      (if highlight?
          (if path (mark-sexp-from-path fn path *focus-style*))
          (if (not (equal current-definition *old-definition*))
              (multiple-value-bind (path at-end?)
                  (compare-sexps-for-mark
                   *old-definition* current-definition)
                (mark-sexp-from-path fn path nil at-end?)))))))

                                        ;here the traditional path information never makes any sense.
                                        ;also we cannot actually highlight a region at this time.
                                        ;or do anything based on the underlying sexprs.

(defun set-cursor-for-ada (data)
  (let* ((fn (get (locf data) ':name))
         (current-definition (get-appropriate-definition fn)))
    (when fn
      (if (not (equal current-definition *old-definition*))
          (let (((x y) (compare-texts-for-mark
                        *old-definition* current-definition)))
            (mark-sexp-from-offsets fn x y nil))))))

                                        ;the following are minimal comparitors which will be improved soon

(defun compare-sexps-for-mark (old new)
  (first-sexp-difference old new))

(defun compare-texts-for-mark (old new)
  (first-text-difference old new))

                                        ;assumes no dotted cdrs says that two whole lists are
                                        ;different if either one has an atomic car and the cars
                                        ;are different.

(defun first-sexp-difference (old new)
  (declare (special at-end?))
  (let (at-end?)
    (values (delq T (first-sexp-difference1 old new)) at-end?)))

                                        ;Note it will be buggy if we ever have to
                                        ;go inside of a {} thing.

(defun first-sexp-difference1 (old new)
  (declare (special at-end?))
  (cond ((equal old new) nil)
        ((and (listp old) (eq (car old) 'kbe-/#-comment-macro)
              (equal new (caadr old)) new)
         nil) ;ignore simple removal of roles
        ((not (and (listp old) (listp new))) (ncons T))
        ((and (not (eq (car old) (car new)))
              (or (symbolp (car old)) (symbolp (car new)))) (ncons T))
        ((and (eq (car old) 'kbe-/#-comment-macro)
              (not (equal (caddr old) (caddr new)))
              (ncons T)))
        (T (let* ((old-len (length old))
                  (new-len (length new))
                  (end (1- (min old-len new-len)))
                  (is-let* (eq (car new) 'let*))
                  (let*-vars-dif))
             (do ((i 0 (1+ i)))
                 ((> i end)
                  (cond (let*-vars-dif)
                        ((> new-len old-len) (ncons i))
                        ((> old-len new-len)
                         (setq at-end? t) (ncons (1- i)))
                        (T (ncons T))))
               (let ((inner-dif
                      (first-sexp-difference1 (nth i old) (nth i new))))
                 (when inner-dif
                   (if (and is-let* (= i 1))
                       (setq let*-vars-dif (cons i inner-dif))
                       (return (cons i inner-dif))))))))))

                                        ;this assumes it is being passed to top level ada functions
                                        ;it breaks them up into regions and returns a difference between
                                        ;them, prefering the body to the earlier stuff.

(defun first-text-difference (old new)
  (let* (((old-with old-dcls old-body) (ada-text-breakup old))
         ((new-with new-dcls new-body) (ada-text-breakup new)))
    (cond ((string-equal old "") (list 0 0))
          ((first-text-difference1 old-body new-body
                                   (+ (num-lines new-with) (num-lines new-dcls))))
          ((first-text-difference1 old-dcls new-dcls (num-lines new-with)))
          ((first-text-difference1 old-with new-with))
          (T (list 0 0)))))

(defun num-lines (string)
  (do ((spot -1 (string-search-char #\return string (1+ spot)))
       (lines -1 (1+ lines)))
      ((null spot) (return lines))))

                                        ;we assume that there must be a dcls start if anything.
(defun ada-text-breakup (string)
  (let* ((dcls-start
          (1+ (or (string-search #.(format nil "~%cliche") string)
                  (string-search #.(format nil "~%package") string)
                  (string-search #.(format nil "~%procedure") string)
                  (string-search #.(format nil "~%function") string)
                  -1)))
         (body-start
          (1+ (or (string-search #.(format nil "~%begin") string)
                  (1- dcls-start))))
         (last-open (max (or (string-reverse-search
                              "open(" string
                              (string-length string) body-start)
                             0)
                         (or (string-reverse-search
                              "create(" string
                              (string-length string) body-start)
                             0))))
    (if (not (zerop last-open))
        (setq body-start (1+ (or (string-search-char
                                  #\return string last-open)
                                 (1- body-start)))))
    (list (substring string 0 dcls-start)
          (substring string dcls-start body-start)
          (substring string body-start))))

(defun first-text-difference1 (old new &optional initial-y)
  (let ((x 0) (y (or initial-y 0))
        (old-end (1- (string-length old)))
        (new-end (1- (string-length new)))
        (old-i 0) (new-i 0)
        old-c new-c
        (difference? T))
    (do () (nil)
     L (when (<= old-i old-end)
         (setq old-c (aref old old-i))
         (when (blank-stuff old-c)
           (incf old-i)
           (go L)))
     M (when (<= new-i new-end)
         (setq new-c (aref new new-i))
         (caseq new-c
                (#\return (incf y) (setq x 0))
                (T (incf x)))
         (when (blank-stuff new-c)
           (incf new-i)
           (go M)))
      (when (or (> old-i old-end) (> new-i new-end))
        (if (and (> old-i old-end) (> new-i new-end))
            (setq difference? nil)
            (decf x))
        (return))
      (cond ((= new-c old-c) (incf old-i) (incf new-i))
            (T (do ((j new-i (1- j))) ((minusp j))
                 (if (not (alpha-numeric (aref new j))) (return))
                 (decf x))
               (return))))
    (if difference? (list x y))))

(defun alpha-numeric (c)
  (or (= c #/_)
      (and (<= #/0 c) (>= #/9 c))
      (and (<= #/A c) (>= #/Z c))
      (and (<= #/a c) (>= #/z c))))

(defun blank-stuff (c)
  (member c '#.(list #\space #\tab #\return)))

(defvar *mark-pre-finish-hook* 'default-mark-pre-hook)

(defvar *mark-post-finish-hook* 'default-mark-post-hook)

(defmethod (editing-actor-secretary :finish-requests) (&optional confirm?)
  (funcall *mark-pre-finish-hook* (send self ':suggested-focus))
  (do ((entry (send self ':next-pending-request)
              (send self ':next-pending-request)))
      ((not entry)
       (funcall *mark-post-finish-hook* (send self ':suggested-focus)))
    (send self ':finish-request entry confirm?)))

(defmethod (editing-actor-secretary :finish-request)
    (data &optional (confirm? nil))
  (when (or (not confirm?)
            (not (eq (car data) ':definition))
            (fquery '() "Update definition of ~A?" (get data ':name)))
    (selectq (car data)
             ((:text)
              (format t "~@[~&~A~&~]" (get data ':text)))
             ((:possibilities)
              (push-possibilities-buffer
               ':list-possibilities     (get data ':type)
               ':set-context            (get data ':context)
               ':set-tagged-string-list (get data ':data)))
             ((:definition)
              (let ((val nil)
                    (fn-name (get data ':name)))
                (cond-every
                 ((setq val (get data ':code))     ;must precede :document
                  (replace-or-create-definition fn-name val))
                 ((setq val (get data ':documentation))
                  (comment-definition fn-name val)))))
             (otherwise
              (cbarf "Proceed, ignoring this item."
                     "Ill-formed entry in KBE's pending-edit queue.")))))

;;; PRESENTATIONAL-FOCUS
;;;
;;;  Eventually we should probably read the focus info in the buffer,
;;;  but for now let's just make it be the whole definition we're in.

(define-behavior (editing-actor-secretary presentational-focus) ()
  (multiple-value-bind (definition-name definition-type)
      (current-editor-definition-name)
    (let ((type-name (selectq definition-type
                              (defun "program")
                              (define-cliche "cliche")
                              (otherwise nil))))
      (return
        (if definition-type
            `(:reference
              ,(if (and type-name
                        (symbolp definition-name))
                   (text "the ~A ~S" type-name definition-name))
              :type ,definition-type
              :name ,definition-name
              :path ,(current-editor-path)))))))

  ;;; KLUDGE definitions for now...

(define-behavior (editing-actor-secretary suggest-focus) (data)
  (send self ':suggest-focus data))

(define-behavior (editing-actor-secretary suggested-focus) ()
  (return (send self ':suggested-focus)))

(defmethod (editing-actor-secretary :pending-requests) ()
  (send (parse-actor (send self ':associated-apprentice))
        ':pending-requests))

(defmethod (editing-actor-secretary :suggest-focus) (data)
  (send (parse-actor (send self ':associated-apprentice))
        ':set-suggested-focus data))

(defmethod (editing-actor-secretary :suggested-focus) ()
  (send (parse-actor (send self ':associated-apprentice))
        ':suggested-focus))

(defvar *initial-editing-actor-secretary* nil)

(define-system-initialization setup-editor
  (setq *initial-editing-actor-secretary*
        (make-instance 'editing-actor-secretary))
  (initialize-editor-window
   (send *initial-editing-actor-secretary* ':window)))

(defun editor-delegate () *initial-editing-actor-secretary*)

;;;; Initialization

;;; Request compile-time compilation of flavors. By default, the
;;; methods would not get compiled until fasload-time otherwise.

(compile-flavor-methods editing-actor editing-actor-secretary)

;;; Local Modes:
;;; Mode:LISP
;;; Lisp WHEN Indent:1
;;; Lisp UNLESS Indent:1
;;; Lisp WITH-OUTPUT-TO-STRING Indent:1
;;; Lisp WITH-INPUT-FROM-STRING Indent:1
;;; End:
