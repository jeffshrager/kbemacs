;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 228-248; listing dmac.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.


;;; DMAC: Macros supporting DB
;;; This file uses ordinary Lisp control primitives only.
;;; DB memory management: live data is clustered in *DB-AREA*.
;;; DB-GC must know every variable and property pointing into that area.
(defvar *db-area* default-cons-area "cons-area used by db")
(defvar *db-size* nil "last known size of db")
(defvar *db-area-count* 0 "# of db areas created")
(defvar *db-areas* nil "list of all db areas")
(defvar *db-free-areas* nil "list of free db areas")
(defvar *db-vars* nil "who points at db area")

;;; PDF page 229
(defmacro db-cons (x y) `(cons-in-area ,x ,y *db-area*))
(defmacro db-xcons (x y) `(xcons-in-area ,x ,y *db-area*))
(defmacro db-ncons (x) `(ncons-in-area ,x *db-area*))
(defmacro db-list* (&rest args) `(list*-in-area *db-area* ,@args))
(defmacro db-list (&rest args) `(list-in-area *db-area* ,@args))
(defmacro db-make-list (n) `(make-list ,n ':area *db-area*))
(defmacro db-copylist (l) `(copylist ,l *db-area*))
;;; DB-PUSH allows double evaluation of some subforms.
(defmacro db-push (x y) `(setf ,y (db-cons ,x ,y)))

;;; PDF page 230
(defmacro def-db-var (sym &rest maybe-value-and-doc)
  `(progn 'compile
     (if (not (memq ',sym *db-vars*)) (push ',sym *db-vars*))
     (defvar ,sym ,@maybe-value-and-doc)))
(defmacro def-db-const (sym &rest maybe-value-and-doc)
  `(progn 'compile
     (if (not (memq ',sym *db-vars*)) (push ',sym *db-vars*))
     (defconst ,sym ,@maybe-value-and-doc)))
;;; A super-simple defstruct. Allocates its storage in the DB area.
(defmacro def-db-struct (name &rest slots)
  (let ((pred (symbolconc name "?"))
        (make (symbolconc "MAKE-" name))
        (tag (symbolconc "^^" name)))
    `(progn 'compile
       (defmacro ,make ,slots
         `(db-list ',',tag ,,@slots))
       (defun ,pred (obj)
         (and ;this test may lose if asynchronous GC occurs midway
           (= (%area-number obj) *db-area*)
           (eq (car obj) ',tag)))
       ,@(do ((i 1 (1+ i)) (s slots (cdr s)) (d nil))
             ((null s) d)
           (let ((slot (car s)))
             (push `(defmacro ,slot (,name)
                      `(nth ,,i ,,name))
                   d))))))

;;; PDF page 231
;;; A DB property must only be put on an atom in *DB-SEGS*.
;;; Call DB-INIT-SYM before taking an atom off that list.
(defvar *db-properties* '()
  "List of symbol properties dynamically hacked by DB")
(defmacro db-property? (tag) `(memq ,tag *db-properties*))
(defun db-init-sym (sym)
  (dolist (prop *db-properties*) (remprop sym prop)))
(defmacro def-db-property (name &optional (comment ""))
  `(progn 'compile
     (push ',name *db-properties*)
     (defmacro ,name (x) `(get ,x ',',name))
     (defprop ,name ,comment db-property)))
;these two properties are essential for speed, there shouldn't
;be any others.
(def-db-property DB-INDEX)
(def-db-property DB-NAMES)

;;; PDF page 232
;;; A layer is a list of contexts in which a fact is known to be in.
(defmacro build-layers-list (&rest args) `(db-list ,@args))
(defun add-layer (layer list)
  (cond ((not (memq layer list)) (db-cons layer list)) (t list)))
;;; Conses cdr-coded lists in the database's special area.
(defun del-layer (layer list)
  (let ((size 0))
    (dolist (element list) (if (not (= element layer)) (incf size)))
    (let ((result (db-make-list size)))
      (do ((l result (cdr l)))
          ((null l) result)
        (do () ((not (= (car list) layer))) (pop list))
        (setf (car l) (car list))
        (pop list)))))

;;; PDF page 233
;;; CMARKER: a context marker for a fact.
(defmacro build-cmarker (? i o n) ;note this defines evaluation order
  `(db-list '*cmarker* ,? ,i ,o ,n))
(defmacro cmarker-is-in? (f) `(cadr ,f))
(defmacro in-layers (f) `(caddr ,f))
(defmacro out-layers (f) `(cadddr ,f))
(defmacro new-cmarker (f) `(car (cddddr ,f)))
(pp:define-list-print-function *cmarker* (cmarker s)
  (format s "cmarker{~@!~@{~=~^ ~:_~}~.}"
          (cmarker-is-in? cmarker) (in-layers cmarker) (out-layers cmarker)
          (if (new-cmarker cmarker) '=-new-)))

;;; PDF page 234
;;; A fact expresses a proposition true in contexts given by its cmarkers.
(defmacro build-fact (cmarker . fields) `(db-list ,cmarker ,@fields))
(defmacro cmarker (f) `(car (cr ,f)))
(defmacro field0 (f) `(car (cdr ,f)))
(defmacro fieldk (f) `(car (cddr ,f)))
(defmacro field1 (f) `(car (cdddr ,f)))
(defmacro field2 (f) `(car (cddddr ,f)))
(defmacro field3 (f) `(car (cdddddr ,f)))
(defmacro cmarker? (obj)
  (once-only (obj) `(and (listp ,obj) (eq (car ,obj) '*cmarker*))))
;;; FACT? must ensure that field selectors are safe and distinguish segcases.
(defmacro fact? (thing)
  (once-only (thing) `(and (listp ,thing) (cmarker? (car ,thing)))))
(pp:define-generalized-print-function fact (fact s)
    (fact? fact)
  (local-declare ((*expr audit-fact compress-fact))
    (cond ((fact? fact)
           (cond ((audit-fact fact t)
                  (format s "{~@l~{~=~^ ~:_~}~.}" (compress-fact fact)))
                 (T (format s "fact{~@l~{~=~^ ~:_~}~.}" fact)))))))
(defmacro fact-is-in? (fact)
  `(let ((cmarker (cmarker ,fact)))
     (and cmarker (cmarker-is-in? cmarker))))
;;; PDF page 235
(defmacro over-in-facts ((var list) &body body)
  `(dolist (,var ,list) (cond ((fact-is-in? ,var) ,@body))))

;;; PDF page 236
(defsubst keyword-name (keyword) keyword)
(defmacro def-fact-predicate (name list)
  (cond ((cdr list)
         `(defmacro ,name (fact)
            `(memq (~keyword ,fact) ',',list)))
        (t `(defmacro ,name (fact)
              `(eq (~keyword ,fact) ',',(car list))))))
(def-fact-predicate input-fact? (input))
(def-fact-predicate output-fact? (output))
(def-fact-predicate tinput-fact? (tinput))
(def-fact-predicate toutput-fact? (toutput))
(def-fact-predicate input-port-fact? (input tinput))
(def-fact-predicate output-port-fact? (output toutput))
(def-fact-predicate port-fact? (input output tinput toutput))
(def-fact-predicate io-fact? (input output))
(def-fact-predicate tio-fact? (tinput toutput))
(def-fact-predicate cflow-fact? (cflow))
(def-fact-predicate dflow-fact? (dflow))
(def-fact-predicate flow-fact? (cflow dflow))
(def-fact-predicate flow-or-port-fact? (input output cflow dflow tinput toutput))
(def-fact-predicate @seg-fact? (@seg))
(def-fact-predicate @io-fact? (@io))
(def-fact-predicate @createvar-fact? (@createvar))
(def-fact-predicate @-fact? (@seg @io @createvar))
(def-fact-predicate io-or-@-fact? (input output @seg @io @createvar))
(def-fact-predicate plan-fact? (plantype recursive role subseg cflow dflow))
(def-fact-predicate slot-fact? (slot))
(def-fact-predicate wildcard-fact? (~*))

;;; PDF page 237
(defmacro fact-key-dispatch (val &body clauses)
  `(caseq (~keyword ,val) ,@clauses))
(defmacro same-fact-key? (fact1 fact2)
  `(eq (~keyword ,fact1) (~keyword ,fact2)))

;;; PDF page 238
;;; FACTDEF specifiers are (SELECTOR-NAME TYPE IDX . REST).
;;; Types: S segcase; P port; SP either; C code; K keyword; NP port name;
;;; L Lisp sexpr. Each fact must be indexed exactly once in buckets 0-10,
;;; and never twice in the same bucket. Shared field names share positions.
(eval-when (eval compile load)
  (defconst *db-keywords* nil
    "List of possible fillers for the ~keyword slot of a fact")
  (defconst *db-fields* nil
    "List of all of the possible field selector names in facts"))

;;; PDF page 239
(defmacro factdef (&body specs)
  (let* ((default-cons-area working-storage-area)
         (selectors (mapcar #'car specs))
         (types (mapcar #'cadr specs))
         (idxs (mapcar #'caddr specs))
         (rests (mapcar #'cdddr specs))
         (keyword (caadr rests)))
    (cond ((memq keyword *db-keywords*)
           (grumble "Redefining fact keyword: ~A" keyword)))
    `(eval-when (eval compile load)
       (push ',keyword *db-keywords*)
       (defprop ,keyword ,types DB-KEYWORD-TYPES)
       (defprop ,keyword ,idxs DB-KEYWORD-IDXS)
       (defprop ,keyword ,rests DB-KEYWORD-RESTS)
       ,@(mapcan
           #'(lambda (fieldsym get-n put-n)
               (let* ((old-def (cdr (assq fieldsym *db-fields*))))
                 (cond ((or (null old-def) (eq get-n old-def))
                        ;always redefining is robust when patching!
                        (let* ((field (get-pname fieldsym))
                               (get-fn (intern (string-append "~" field)
                                               (symbol-package fieldsym)))
                               (put-fn (intern (string-append "&~" field)
                                               (symbol-package fieldsym))))
                          `((push ',(cons fieldsym get-n) *db-fields*)
                            (defmacro ,get-fn (s) `(,',get-n ,s))
                            (defmacro ,put-fn (s x) `(,',put-n ,s ,x)))))
                       (T (barf "Redefining field in different location~
                                ~%~A" fieldsym)))))
           selectors
           '(field0 fieldk field1 field2 field3)
           '(&field0 &fieldk &field1 &field2 &field3)))))

(defmacro &field0 (s x)
  `(let ((h (db-copylist ,s)))
     (rplaca (cdr h) ,x)
     (rplaca h *null-cmarker*)))

(defmacro &fieldk (s x)
  `(let ((h (db-copylist ,s)))
     (rplaca (cddr h) ,x)
     (rplaca h *null-cmarker*)))

(defmacro &field1 (s x)
  `(let ((h (db-copylist ,s)))
     (rplaca (cdddr h) ,x)
     (rplaca h *null-cmarker*)))

(defmacro &field2 (s x)
  `(let ((h (db-copylist ,s)))
     (rplaca (cddddr h) ,x)
     (rplaca h *null-cmarker*)))

(defmacro &field3 (s x)
  `(let ((h (db-copylist ,s)))
     (rplaca (cdr (cddddr h)) ,x)
     (rplaca h *null-cmarker*)))

;;; PDF page 240
(factdef (segcase S 8.) (keyword K nil INPUT)
  (kind L nil NIL ID) (obj NP nil) (id NP nil))
(factdef (segcase S 9.) (keyword K nil OUTPUT)
  (kind L nil NIL ID PART) (obj NP nil) (id NP nil))
(factdef (segcase S 8.) (keyword K nil TINPUT)
  (lower-env S 14.) (obj NP nil) (lower-io P 15.))
(factdef (segcase S 9.) (keyword K nil TOUTPUT)
  (lower-env S 15.) (obj NP nil) (lower-io P 14.))
(factdef (segcase S 10.) (keyword K nil EXPECT) (wff L nil))
(factdef (segcase S 10.) (keyword K nil ASSERT) (wff L nil))
(factdef (segcase S 10.) (keyword K nil WHEN) (wff L nil))
(factdef (segcase S 2.) (keyword K nil SEGTYPE) (type K nil SPLIT JOIN SIMPLE))
(factdef (segcase S 0.) (keyword K nil SUBSEG) (subseg S 11.))
(factdef (segcase S 6.) (keyword K nil DFLOW) (from P 12.) (to P 13.))
(factdef (segcase S 6.) (keyword K nil CFLOW) (from S 14.) (to S 15.))
(factdef (segcase S 1.) (keyword K nil PLANTYPE)
  (type K nil NEW-VIEW EXPR AND COMP XOR PRED JOIN SSR TEMPCOMP
        SSR-BODY CONSTANT BUILTIN))
(factdef (segcase S 7.) (keyword K nil ROLE) (roleseg S 11.)
  (rolename K nil ACTION INIT PRED JOIN OP BODY REC DFJOIN
            BSSR GEN TRN ACC TERM FILTER))
(factdef (segcase S 7.) (keyword K nil SLOT) (slotseg S 11.)
  (slotname L nil) (slottype L nil INPUT OUTPUT SEGMENT))
(factdef (segcase S 10.) (keyword K nil CLICHE) (clichename L nil))
(factdef (segcase S 10.) (keyword K nil CLICHE-DEF)
  (cliche-def-name L nil) (cliche-def-comments L nil))
(factdef (segcase S 7.) (keyword K nil RECURSIVE) (subseg S 11.))
(factdef (segcase S 3.) (keyword K nil @SEG)
  (kind K nil JOIN FUNCALL CONSTANT LAMBDA-EXP OPENCODE RECURSION)
  (name L nil) (code C nil))
(factdef (deep P 4.) (keyword K nil @IO)
  (kind K nil LAMBDA-ARG ACTUAL-ARG FREE-VAR RETURNVAL SIDE-EFFECT DUMMY)
  (name L nil) (argno L nil))
(factdef (deep P 5.) (keyword K nil @CREATEVAR) (varname L nil))

;;; PDF page 241
(defmacro ~seg (s) `(car ,s))
(defmacro ~case (s) `(cdr ,s))
(defmacro &~seg (s x) `(db-xcons (cdr ,s) ,x))
(defmacro &~case (s x) `(db-cons (car ,s) ,x))
(defmacro ~seg~segcase (s) `(~seg (~segcase ,s)))
(defmacro ~seg~deep (s) `(~seg (~deep ,s)))
(defmacro ~seg~subseg (s) `(~seg (~subseg ,s)))
(defmacro ~seg~roleseg (s) `(~seg (~roleseg ,s)))
(defmacro &~seg~segcase (s x)
  (once-only (s) `(&~segcase ,s (&~seg (~segcase ,s) ,x))))
(defmacro &~seg~deep (s x)
  (once-only (s) `(&~deep ,s (&~seg (~deep ,s) ,x))))
(defmacro &~seg~subseg (s x)
  (once-only (s) `(&~subseg ,s (&~seg (~subseg ,s) ,x))))
(defmacro &~seg~roleseg (s x)
  (once-only (s) `(&~roleseg ,s (&~seg (~roleseg ,s) ,x))))

;;; PDF pages 242-243
(defmacro ~set (&rest args)
  (let* ((reversed-args (reverse args))
         ((facts . reversed-fields) reversed-args)
         (selector (do ((fl reversed-fields (cdr fl))
                        (s 's (list (car fl) s)))
                       ((null fl) s))))
    `(mapcar #'(lambda (s) ,selector) ,facts)))
(defmacro ~nset (&rest args)
  (let* ((reversed-args (reverse args))
         ((facts . reversed-fields) reversed-args)
         (selector (do ((fl reversed-fields (cdr fl))
                        (s '(car s) (list (car fl) s)))
                       ((null fl) s))))
    (cond ((symbolp facts)
           `(progn (map #'(lambda (s) (setf (car s) ,selector)) ,facts)
                   ,facts))
          (t `(let ((temp ,facts))
                (map #'(lambda (s) (setf (car s) ,selector)) temp)
                temp)))))
(defmacro ~1st (&rest args)
  (let* ((reversed-args (reverse args))
         ((facts . reversed-fields) reversed-args)
         (selector (do ((fl reversed-fields (cdr fl))
                        (s 's (list (car fl) s)))
                       ((null fl) s))))
    (cond (reversed-fields
           `(let ((s (car ,facts))) (cond (s ,selector) (t nil))))
          (t `(car ,facts)))))
(defmacro ~01 (&rest args)
  (let* ((reversed-args (reverse args))
         ((facts . reversed-fields) reversed-args)
         (selector (do ((fl reversed-fields (cdr fl))
                        (s 's (list (car fl) s)))
                       ((null fl) s))))
    (cond (reversed-fields
           `(let ((s (sel01 ,facts))) (cond (s ,selector) (t nil))))
          (t `(sel01 ,facts)))))

;;; PDF page 244
;;; (& seg [case]) creates a segcase; (& field0 keyword fields...) a fact.
;;; Standard fact keywords are implicitly quoted.
(defmacro & (&rest body)
  (if (memq '* body) (ferror "Cannot use * in an (& ...) expression."))
  (let* (((a . as) body) ((b . bs) as))
    (cond ((null as) `(db-ncons ,a)) ;Redundant clause - just for clarity
          ((null bs) `(db-cons ,a ,b))
          (t (cond ((atom b)
                    (let ((len (length (get b 'db-keyword-types))))
                      (cond (len
                             (if (not (= len (+ 2 (length bs))))
                                 (ferror "Wrong number of fields.~% ~S"
                                         `(& ,@body)))
                             (setq b `',b))))))
             `(build-fact *null-cmarker* ,a ,b ,@bs)))))

;;; PDF page 245
;;; Pattern builder: converts * to '~* and fills omitted fields.
;;; PORT is a pseudo-keyword for INPUT/OUTPUT/TINPUT/TOUTPUT patterns.
(defmacro &* (&rest body)
  (map #'(lambda (l) (if (eq (car l) '*) (setf (car l) ''~*))) body)
  (cond ((null (cdr body)) `(& ,(car body) '~*))
        (t (let ((keyword (cadr body)) (slot1 (car body))
                 (slot2 (cadr body)) (more-slots (cddr body)))
             (cond ((eq keyword 'PORT)
                    (setq slot2 '~*) (setq keyword 'INPUT)))
             (let ((len (length (get keyword 'db-keyword-types))))
               (if (< len (length body))
                   (ferror "Too many args to &*~% Source: ~S" `(&* . ,body)))
               `(& ,slot1 ',slot2 ,@more-slots
                   ,@(nthcdr (- 3 (- len (length body)))
                             '('~* '~* '~*))))))))
(defmacro +& (&rest body) `(add-fact (& . ,body)))
(defmacro >& (&rest body) `(assert-fact (& . ,body)))
(defmacro ??& (&rest body) `(find-facts (&* . ,body)))
(defmacro --& (&rest body) `(erase-facts (&* . ,body)))
(defmacro ?& (&rest body) `(sel1 (find-facts (&* . ,body))))
(defmacro a?& (&rest body) `(sel1+ (find-facts (&* . ,body))))
(defmacro -& (&rest body) `(sel1 (erase-facts (&* . ,body))))
(defmacro a-& (&rest body) `(sel1+ (erase-facts (&* . ,body))))

;;; PDF page 246
(defmacro db-context-is (context)
  `(local-declare ((special ,context *db-context* *db-context-names*))
     (cond ((not (memq ',context *db-context-names*))
            (push ',context *db-context-names*)))
     (setq ,context *db-context*)))
(defmacro db-context-was (context)
  `(local-declare ((special ,context *db-context* *db-context-names*))
     (cond ((not (memq ',context *db-context-names*))
            (push ',context *db-context-names*)))
     (setq ,context (cdr *db-context*))))
(defmacro remember-db-context (old-name new-name)
  `(local-declare ((special ,new-name *db-context-names*))
     (cond ((not (memq ',new-name *db-context-names*))
            (push ',new-name *db-context-names*)))
     (setq ,new-name ,old-name)))

;;; PDF page 247
;;; DBHUNK structures must retain their cdr-coding, including during GC.
(defmacro make-dbhunk () `(rplaca (db-make-list 17.) '*DBHUNK*))
;;; DBX evaluates its arguments in the reverse of the order written.
(defmacro dbx (num hunk)
  `(car (%make-pointer-offset dtp-list ,hunk (1+ ,num))))
(defsubst dbhunk-name (hunk)
  (local-declare ((special *db-segs*))
    (dolist (seg *db-segs*)
      (if (eq (db-index seg) hunk) (return seg)))))
(pp:define-list-print-function *dbhunk* (hunk s)
  (let ((seg (dbhunk-name hunk)))
    (cond (seg (format s "dbH-~A" seg))
          (T (format s "~\block\" hunk)))))

;;; PDF page 248
;;; Toplevel DB access must assure a consistent view of the database.
(defmacro with-db-locked (&body forms)
  (let ((lock? (gensym)))
    `(let ((,lock? (neq *db-lock* current-process)))
       (unwind-protect
           (progn (if ,lock? (process-lock (locf *db-lock*) nil "Await DB"))
                  ,@forms)
         (if ,lock? (process-unlock (locf *db-lock*) nil nil))))))
