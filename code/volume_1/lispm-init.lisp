;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 74-77; listing lispm-init.

;;; -*-Mode:LISP; Fonts:MEDFNB; -*-

;;; To use this init file, just do (LOGIN 'KBE 'OZ).
;;; You will be prompted with questions about how much of the system
;;; to load, whether to compile things, whether to ask for confirmations,
;;; etc. If you don't select confirmations, reasonable things will
;;; happen based on the answers to the other questions.

;;; Disable incoming net traffic

(si:disable-services)
(format t "~&(Chaos servers disabled)~%")

;;; Load definitions of component packages/systems.
;;;
;;; Defines the KBE package, KBE logical device, and KBE system.
;;; This must be done before the next s-expression can be read,
;;; since it can contain references to KBE: ...

(load "PS:<KBE>SYSDEF")

;;; Support files

(let-globally ((si:gc-report-stream nil))
 (let ((old-more-state (send terminal-io ':more-p)))
  (unwind-protect
   (let* ((full? (fquery '(:choices
                           (((T "Yes.") #/Y #\Space)
                            ((NIL "No.") #/N #\Rubout)
                            ((:COMPILE "Compile and load system.") #/C)
                            ((:LOAD "Load system.") #/L)
                            ((:DUMP "Set up to dump system.") #/D))
                           :help-function
                           (lambda (stream choices ignore)
                             (format stream
                                     "~{~& ~1{~1G~C: ~0G~1{~1G~A~}~}~}"
                                     choices)))
                         "~&Set up full KBE environment? "))
          (confirm?      (selectq full?
                           ((:COMPILE :LOAD :DUMP) nil)
                           (otherwise
                            (y-or-n-p "Ask for confirmations? "))))
          (compile?      (selectq full?
                           ((:COMPILE) t)
                           ((:LOAD :DUMP) nil)
                           (otherwise
                            (y-or-n-p "Compile recent changes? "))))
          (plan-to-dump? (selectq full?
                           ((:COMPILE) nil)
                           ((:DUMP) t)
                           (otherwise
                            (y-or-n-p "Plan to dump? "))))
          (load-patches? (selectq full?
                           ((:COMPILE :LOAD :DUMP) t)
                           (otherwise
                            (y-or-n-p "Load patches? "))))
          (increment?    (and compile?
                              (or (not (status feature 3600.))
                                  (y-or-n-p "Increment patch version? ")))))

     (when (si:get-system-version "KBE")
       (setq confirm? t) ;Paranoia
       (when plan-to-dump?
         (ferror "You can't be serious about wanting to dump this out."))
       (setq plan-to-dump? nil))

     (when (not plan-to-dump?)
       (si:enable-services)
       (format t "~&(Chaos servers enabled)~%"))

     (send terminal-io ':set-more-p nil)

     ;; Maybe get latest system stuff loaded

     (catch-error ;eg, if site is down...
      (if load-patches?
          (cond (confirm? (load-patches))
                (t (load-patches ':noselective)))))

     (let ((base 10.)       ;Must output base ten with decimal point
           (*nopoint nil)) ;or MAKE-SYSTEM may sometimes lose. -kmp

       (if (or (not confirm?) (y-or-n-p "Make-System KBE? "))
           (make-system 'KBE
                        (if confirm?   ':noop    ':noconfirm)
                        (if compile?   ':compile ':noop     )
                        (if (and compile? (not increment?))
                            ':no-increment-patch
                            ':noop)
                        ':no-reload-system-declaration))

       (if (and compile? (not increment?))
           (make-system 'KBE ':noconfirm ':no-reload-system-declaration)))

     (cond ((or (and (not confirm?) full?)
                (y-or-n-p "Set up default environment? "))
            (cond-every ((or (not confirm?)
                             (y-or-n-p "Default font CPTFONTB? "))
                         (tv:set-default-font fonts:cptfontb)
                         ;; We keep switching around what the above default
                         ;; font is, but it seems generally agreed that in
                         ;; any case, the initial lisp listener probably
                         ;; wants to remain invariant of that.
                         )
                        ((or (not confirm?)
                             (y-or-n-p "Initial Lisp Listener in font MEDFNB? "))
                         (send terminal-io ':set-font-map '(fonts:medfnb))
                         (send (car (send terminal-io ':blinker-list))
                               ':set-size
                               (send terminal-io ':char-width)
                               (send terminal-io ':line-height)))
                        ((or (not confirm?)
                             (y-or-n-p "BASE,IBASE 10, *NOPOINT T? "))
                         (add-initialization "Base Ten Default"
                           '(setq base 10. ibase 10. *nopoint t)
                           '(:warm :now))) ; Saying :WARM implies :COLD
                        ((and (status feature 3600.)
                              (= si:sequence-break-interval 1000000.)
                              (or
                               (not confirm?)
                               (y-or-n-p "Reduce sequence break interval? ")))
                         (setq si:sequence-break-interval 10000.))
                        ((or (not confirm?)
                             (y-or-n-p "Jazzy PA herald on cold boots?"))
                         (add-initialization "PA Herald"
                           '(kbe:print-PA-herald)
                           '(:warm)))))) ;Saying :WARM implies :COLD

     (kbe:initialize-system) ;Want to do this after setting up fonts, etc.

     (catch-error
      (cond ((or (and (not confirm?) full?)
                 (y-or-n-p "Load EMouse? "))
             (load "KBE: EMACS; EMOUSE LISP"))))

     (catch-error
      (cond ((and (not (status feature 3600.))
                  (or (and (not confirm?) full?)
                      (y-or-n-p "Load KMouse? ")))
             (load "KBE: LMLIB; KMOUSE"))))

     (catch-error
      (cond ((or (and (not confirm?) full?)
                 (y-or-n-p "Load DEMO init file? "))
             (load "KBE: KBE; DEMO-INIT"))))

     (catch-error
      (cond ((or (and (not confirm?) full?)
                 (y-or-n-p "Load LOGIN advice? "))
             (load "KBE: PA; LOGIN"))))

     (catch-error ;eg, in case AI is down...
      (if (and load-patches? (not compile?))
          (cond ((not confirm?)
                 (load-patches 'KBE ':noselective))
                ((or full? (y-or-n-p "Load patches? "))
                 (load-patches 'KBE)))))

     (catch-error
      (cond ((and compile?
                  increment?
                  (not plan-to-dump?)
                  (or (not confirm?)
                      (y-or-n-p "Create documentation file? ")))
             (let ((version-info (kbe:system-version-string "KBE")))
               (format t "~&Producing documentation for ~A..."
                       version-info)
               (format t "~&~A documentation written to ~A.~%"
                       version-info
                       (kbe:document-KBE-to-file))))))

     (when (and compile?
                (not plan-to-dump?)
                (or (not confirm?)
                    (y-or-n-p "Run standard self-test? ")))
       (format t "~&Running self-test sequence ...~%")
       (let ((data kbe:(with-delegate (parse-actor "Apprentice 1")
                         (eval-command (parse-command "Test yourself.")))))
         (let ((text    (get data ':text))
               (passed? (get data ':success)))
           (format t "~&~A~%" text)
           (when (and passed?
                      increment?
                      (or (not confirm?)
                          (y-or-n-p "Release this KBE version? ")))
             (set-system-status 'KBE ':released)
             (format t "~A is now released."
                     (kbe:system-version-string "KBE")))
           (when (and (not passed?)
                      (eq (multiple-value-bind (ignore ignore status)
                              (si:get-system-version "KBE"))
                          ':released)
                      (or (not confirm?)
                          (y-or-n-p "De-Release this KBE version? ")))
             (set-system-status 'KBE ':broken)
             (format t "~A is no longer released."
                     (kbe:system-version-string "KBE"))))))

     (multiple-value-bind (major minor)
         (si:get-release-version)
       ;; People like RWK at Symbolics keep putting non-numeric
       ;; data into the release info, so we need to be cautious here...
       (if (not (numberp major)) (setq major -1))
       (let ((base 10.) (*nopoint t))
         (setq minor (or (parse-number (format nil "~A" minor)) -1)))
       (when (= major 4)
         (cond ((not (or (= minor 3) (= minor 4) (= minor 5)))
                (format t
                        "~&Bug fixes NOT being loaded into Release ~D.~D~%"
                        major minor))
               ((mapcar #'(lambda (x) (load (car x)))
                        (ignore-errors
                          (cdr (fs:directory-list
                                 #-3600 "KBE: BUGFIX; * QBIN >"
                                 #+3600 "KBE: BUGFIX; * BIN >"
                                 ':fast ':noerror))))
                ;If any files were loaded, note the fact...
                ;(setq si:system-additional-info "Hacked Magic")
                (format t "~&KBE private patches loaded.~%")))))

     ;; Acknowledge successful termination.

     (format t "~&~A loaded.~%"
             (kbe:system-version-string "Knowledge-Based-Editor"))

     (when plan-to-dump?
       (let ((ok? t))
         (cond-every (compile?
                      (setq ok? nil)
                      (format t "~&>>> Warning: System freshly compiled."))
                     (chaos:chaos-servers-enabled
                      (setq ok? nil)
                      (format t "~&>>> Warning: Chaos servers enabled."))
                     ((not load-patches?)
                      (setq ok? nil)
                      (format t "~&>>> Warning: Patches not loaded.")))
         (when ok?
           (multiple-value-bind (ignore ignore keyword)
               (si:get-system-version 'system)
             (if (or (null si:system-additional-info)
                     (equal si:system-additional-info "")
                     (equal si:system-additional-info "Vanilla"))
                 (setq si:system-additional-info
                       (caseq keyword
                         ((:EXPERIMENTAL) "Black Magic")
                         ((:RELEASED)     "Sorcerer's Apprentice")
                         ((:BROKEN)       "Faulty Magic")
                         ((:OBSOLETE)     "Worn Out Magic")
                         (T               si:system-additional-info)))))
           (format t "~&Ready for dump.~%"))))

     ;; Get user's attention.

     (kbe:pleasant-noise-2)

     ) ;TEL
   (send terminal-io ':set-more-p old-more-state)
   ;TCETORP-DNIWNU
   )
  ;TEL
  )
 ;YLLABOLG-TEL
 )
