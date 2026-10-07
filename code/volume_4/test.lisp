;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>TEST.LISP.91, 14 November 1984 12:53:48
;;; PDF pages 199-204 (printed source pages 1-6).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; TEST: Self-testing facilities for the KBE.
;;; Note: The name "sequence" is overloaded in this file. Sometimes
;;;   it means a sequence of commands (which should rightly be called
;;; a "scenario") and sometimes it means a sequence of scenarios.
;;;   It should usually be obvious by context which was meant. Sometime
;;; I'll regularize the naming, I hope. --kmp
;;; Declarations
(defvar *demo-bindings*
  `((PRINLEVEL   NIL)
    (PRINLENGTH  NIL)
    (PRINENDLINE NIL)
    (IBASE       10.)
    (BASE        10.)
    (*NOPOINT    T)
;;; this is a nice idea but it doesn't work well enough, because
;;; even if things get read in right, the get written out again and
;;; then get read in again in the wrong package later.  We
 ;;; must do a real pkg-goto. --~ Dick
    (PACKAGE     ,*KBE-PACKAGE*))
  "Bindings that should be in effect during KBE demo testing.")
(defmacro defun-in-demo-environment (name bvl &body body)
  `(defun ,name ,bvl
    (progw *demo-bindings*  ,@body)))
(fs:define-canonical-type  :KBE-TEST-SEQUENCE "KBE-TEST-SEQUENCE"
  ((:tops-20) "KBE-TEST-SEQUENCE" )
  ((:tenex :unix :vms) "KSQ"))

 ;;; File System Utilities
;;; (MATCH-PATHNAMES wildcard-pathname)       :
;;; Returns a list of all the pathnames that match a given pathname.
(defun match-pathnames (wildcard-pathname)
  (let ((result (fs:directory-list wildcard-pathname
                                   ':fast ':noerror)))
    (cond ((typep result 'sys:error) nil)
          (t (mapcar #'car (cdr result))))))

;;; Test and Demo Files
;;; The DEMO directory is allocated for holding onto standard test
;;; sequences and their correct results.
(define-internal-option *demo-file-pathname-defaults*
    "Self-test input file pathname default"
  "KBE: DEMO; * * >" :sexp)
(defun demo-file-pathname (type &optional (name ':wild))
  (send (fs:parse-pathname *demo-file-pathname-defaults*) ':new-pathname
        ':name name ':type type))
;;; The TEST directory is allocated for holding onto the results
;;; of KBE automatic testing.
(define-internal-option *test-file-pathname-defaults*
    "Self-test output file pathname default"
  "KBE: TEST; * LISP >" :sexp)
(defun test-file-pathname ()
  (send (fs:parse-pathname *test-file-pathname-defaults*) ':new-pathname
        ':name (kbe-version-string-for-pathname) ))
(defun all-demo-names ()
  (Rlist (send (Elist (match-pathnames (demo-file-pathname ':KBE)))
               ':name)))
(defun all-demo-sequence-names ()
  (Rlist
   (let ((pathname (Elist (match-pathnames
                           (demo-file-pathname ':KBE-TEST-SEQUENCE) ))))
     (let ((sequence (send pathname  ':name)))
       (cons (text  "~A sequence"
                    (string-capitalize (string-downcase sequence) ))
             (with-open-file (stream pathname)
               (read-forms-from-stream stream)))))))
(defun-in-demo-environment demo-sequence (name)
  (error-restart (sys:error "Retry obtaining info for ~A demo sequence."
                            name)
                 (values (with-open-file (instream (demo-file-pathname ':KBE name) )
                           (read-forms-from-stream instream #'maybe-parse-command) )
                         (with-open-file (instream (demo-file-pathname ':LISP name) )
                           (read-forms-from-stream instream) ))))
(defun maybe-parse-command (stream eof-val)
  (let ((command (read-command stream) ))
    (cond ((not command) eof-val)
          (t
           (unparse-command command)))))
(defun read-forms-from-stream (stream &optional (reader #'read))
  (do ((form (funcall reader stream nil) (funcall reader stream nil))
       (l nil (cons form l)))
      ((null form) (nreverse l))))

 ;;;;
(defun format-test-file-header (outstream)
  (let ((oddities (odd-KBE-options)))
    (format outstream
            ";;; -*- Mode: LISP; Package: KBE; Base: 10; -*-~
           ~%;;;~
           ~%;;; Knowledge Based Editor.~40TAutomatic self-test results.~
           ~%;;;~
           ~%;;; For ~A, ~Q.~
           ~%;;;~
           ~%;;; In ~A on ~A~
           ~%;;; using ~A in ~A~@[, ~A~].~:[~*~;~
           ~%;;;~
           ~%;;; Special option~P:~2:*~{~
           ~%;;;   * ~A~}~*~]~%"
            user-id #'time:print-current-date
            (loaded-band-string) (site-string)
            (system-version-string "KBE")
            (system-version-string ':RELEASE)
            (cond ((not (released-system-version-active? "System"))
                   (system-version-string "System")))
            oddities (length oddities))))

;;; Returns
 ;;; 1: a string describing the results
 ;;; 2: T if all tests were passed, NIL otherwise.
(defun test-KBE (&optional sequence)
  (pkg-goto 'kbe) ;so that tests will work --- Dick
  (declare (values string success?))
  (let ((names (cond ((null sequence) (all-demo-names) )
                     ((stringp sequence) (ncons (string sequence)))
                     ((symbolp sequence)
                      (or
                       (mapcar #'string
                               (cdr
                                (ass #'(lambda (x y)
                                         (%string-equal x 0 y 0
                                                        (string-length x)))
                                     (string sequence)
                                     (all-demo-sequence-names) )))
                       (ferror "No such sequence: ~S" sequence) ))
                     (t (mapcar #'string sequence))))
                                        ;(*db-check-fact-ports* t) ;Nothing works with this set (yet).
        )
    (let ((results (mapcar #' (lambda (name)
                                (cons name
                                      (multiple-value-list
                                       (run-KBE-test name))))
                              names) )
          (string nil)
          (success? nil))
      (with-open-file (outstream (test-file-pathname) ':direction ':out)
        (unwind-protect (format-KBE-test outstream results)
          (close outstream))
        (setq string
              (format nil "KBE ~:[failed~;passed~] the~@[ ~A~] test!~
                            ~@[ Details in ~A~]"
                      (setq success? (every results #'cadr))
                      (if (null (cdr names)) (car names))
                      (send outstream ':send-if-handles ':pathname))))
      (values string success?))))

(defun format-KBE-test (outstream results)
  (format-test-file-header outstream)
  (format-test-summary outstream results)
  (dolist (result results)
    (lexpr-funcall #'format-test-result outstream result)))
(defun test-KBE-1 (name outstream)
  (lexpr-funcall #'format-test-result
                 outstream name
                 (multiple-value-list (run-KBE-test name) )))
(defun format-test-summary (outstream results)
  (format outstream "~%;;;; Summary~%")
  (cond ((and (cdr results) (every results #'cadr))
         (format outstream ";;; All test sequences were successful.~%"))
        (t
         (dolist (result results)
           (format outstream
                   (cond ((cadr result)
                          ";;; The ~A sequence was successful.~%")
                         (t ";;; The ~A sequence failed:~
                            ~%;;;   ~A~%"))
                   (car result) (caddr result))))))

(defun-in-demo-environment format-test-result
    (outstream name win? errmsg input-list n alist)
  (let ((result nil))
    (format outstream "~/|~%;;;; The ~A Sequence~%~{~
                          ~%;;; >~A~}~%"
            name input-list)
    (cond (win?
           (format outstream "~%;;; No coding errors in this sequence.~%"))
          (t
           (format outstream "~%;;; Test failed~@[ after ~D lines~]!~
                              ~%;;; ~A~%" n errmsg)))
    (dolist (entry alist)
      (let (((fn code good-code explanation) entry))
        (format outstream "~%;;; ~S~%~A~%"
                (cons fn (parse-argument-list (caddr code)))
                (cond (explanation (format-explanation-for-comment explanation))
                      (t ";;; Note: No explanation generated by KBE.")))
        (cond ((equal code good-code)
               (format outstream "~%~:=~%" code))
              ((null code)
               (push fn result)
               (format outstream "~% ;; No code produced for ~S.~%" fn))
              (t
               (push fn result)
               (format outstream
                       "~% ;; Faulty code produced for ~S...~%~
                        ~%~:=~%~
                        ~% ;; Correct code would have been...~%~
                        ~%~:=~%"
                       fn code good-code)))))
    result))

;;; Returns
 ;;; 1: T if successful, NIL if not.
 ;;; 2: If not successful, an explanation of why not.
 ;;; 3: Input list
 ;;; 4: If not all input lines executed, number that were. Otherwise NIL.
 ;;; 5: An alist of the form ((fn code correct-code explanation) ...)
(defun run-KBE-test (name)
  (declare (values win errmsg input-list n results-alist))
  (multiple-value-bind (input-list good-code-list)

      (demo-sequence name)
    (let ((fail nil) (codings nil) (n 0))
      (catch-error-restart ((sys:error sys:abort) "Abort test ~S" name)
                           (dolist (request input-list)
                             (when-abnormal-exit (condition-case-if  (not *debug*) (cond)
                                                                     (with-interface-disabled
                                                                         (eval-command  (parse-command request)))
                                                                     (sys:abort  ;Typed c-Abort while running
                                                                      (setq fail (format nil
                                                                                         "Aborted during ~S"
                                                                                         request))
                                                                      (return))
                                                                     (sys:error
                                                                      cond ;ignored for now...
                                                                      (setq fail (format nil "Error during ~S"
                                                                                         request) )
                                                                      (return)))
             ;;; This can happen, for example, when the user
                                                 ;; types Abort from the debugger to find our SYS:ERROR trap
                                        ; just outside this do-loop.
                                                 (if (not fail)
                                                     (setq fail
                                                           (format nil "Aborted during ~S" request))))
                             (incf n)))
      (if (not fail) (setq n nil))
      (dolist (good-code good-code-list)
        (let ((fn    (cadr good-code)))
          (let ((code        (copytree (code-for-defun fn)))
                (explanation (explanation fn)))
            (if (or (not code) (not (equal code good-code)))
                (setq fail (or fail "Bad code generated.")))
            (push (list fn code good-code explanation) codings))))
      (values (not fail) fail input-list n (nreverse codings)))))
(defun choose-KBE-test ()
  (let ((individuals (all-demo-names))
        (sequences   (all-demo-sequence-names) ))
    (let ((the-group `("All of the above" :value , individuals) ))
      (tv:menu-choose `(,@individuals
                        , the-group
                        ,@(mapcar #'(lambda (x)
                                      (list (car x) ':value (cdr x)))
                                  sequences) )
                      "Choose a demo sequence:"))))
(define-db-action test (yourself)
  "Requests that the apprentice test itself."
  (multiple-value-bind (text success?)
      (with-interface-disabled (test-kbe))
    `(:test-results :success ,success? :text ,text)))
