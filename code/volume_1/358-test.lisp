;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 23-30; listing 358-test.

;;; -*- Mode: LISP; Package: KBE; Base: 10; -*-
;;;
;;; Knowledge Based Editor.   Automatic self-test results.
;;;
;;; For KBE, Friday the fourteenth of December, 1984; 2:25:23 pm.
;;;
;;; In >World1.load on PA Lisp Machine Merlin
;;; using KBE 358.0x in Release 4.5.
;;;
;;; Special options:
;;;  * KBE debug mode (local): No.
;;;  * Garbage collector enabled: No.

;;;; Summary
;;;  The BUCKET sequence failed:
;;;   Bad code generated.
;;;  The DELETE sequence failed:
;;;   Bad code generated.
;;;  The HASH sequence was successful.
;;;  The MEMQ sequence failed:
;;;   Bad code generated.
;;;  The SQRT sequence was successful.
;;;  The SUMLIST sequence was successful.

;;;; The BUCKET Sequence

;;; >Reinitialize database.
;;; >Define a<n> program BUCKET with parameters OBJ and L.
;;; >Implement the program as a linear-search of a sublist-enumeration of [L] .
;;; >Implement the search-test as [(EQ (CAR LIST) OBJ)] .
;;; >Implement the action as [LIST] .
;;; >Reimplement the search-test as an equality-within-epsilon.
;;; >Implement the first-number as [(CAR LIST)] .
;;; >Implement the second-number as [OBJ] .
;;; >Append a parameter EPS.
;;; >Implement the epsilon as [EPS] .

;;; Test failed!
;;; Bad code generated.

;;; (BUCKET OBJ L EPS)
;;; The program BUCKET is a linear-search.
;;;   The enumerator is a sublist-enumeration.
;;;      It enumerates the sublists of L.
;;;   The search-test is an equality-within-epsilon.
;;;      It determines whether (CAR LIST) and OBJ differ by less than EPS.
;;;   The action is LIST.

 ;; Faulty code produced for BUCKET...

(DEFUN BUCKET (OBJ L EPS)
  (LET* ((LIST L))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (IF (< (ABS (- (CAR LIST) OBJ)) EPS) (RETURN LIST))
      #{LIST, the output sublist}
      (SETQ LIST (CDR LIST)))))

 ;; Correct code would have been...

(DEFUN BUCKET (OBJ L EPS)
  (LET* ((LIST L))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (IF (< (ABS (- (CAR #{LIST, the output item}) OBJ)) EPS) (RETURN LIST))
      (SETQ LIST (CDR LIST)))))

;;;; The DELETE Sequence

;;; >Reinitialize database.
;;; >Define a<n> program MYDELETE with a parameter SYMBOL.
;;; >Implement it as a linear-search.
;;; >Implement the enumerator as a trailing-pointer-list-enumeration of [(MYHASH SYMBOL)] .
;;; >Implement the search-test as [(EQ {a use of the element} SYMBOL)] .
;;; >Implement the action as a splice-out of [PREVIOUS-SUBLIST] .
;;; >Define a<n> program MYOTHERDELETE with a parameter SYMBOL.
;;; >Implement it as a linear-search.
;;; >Implement the enumerator as a trailing-pointer-sublist-enumeration of [(MYHASH SYMBOL)] .
;;; >Implement the search-test as [(EQ (CAR LIST) SYMBOL)] .
;;; >Implement the action as a splice-out of [PREVIOUS-SUBLIST] .

;;; Test failed!
;;; Bad code generated.

;;; (MYDELETE SYMBOL)
;;; The program MYDELETE is a linear-search.
;;;   The enumerator is a trailing-pointer-list-enumeration.
;;;      It enumerates the elements of the tail of (MYHASH SYMBOL)
;;;      maintaining a trailing pointer.
;;;   The search-test is (EQ #{(CAR LIST), the output element} SYMBOL).
;;;   The action is a splice-out.
;;;      It splices an element out of a list after PREVIOUS-SUBLIST.

(DEFUN MYDELETE (SYMBOL)
  (LET* ((PREVIOUS-SUBLIST (MYHASH SYMBOL))
         (LIST (CDR PREVIOUS-SUBLIST)))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (WHEN (EQ #{(CAR LIST), the output element} SYMBOL)
        (RPLACD PREVIOUS-SUBLIST (CDDR PREVIOUS-SUBLIST))
        (RETURN NIL))
      (SETQ PREVIOUS-SUBLIST LIST)
      (SETQ LIST (CDR LIST)))))

;;; (MYOTHERDELETE SYMBOL)
;;; The program MYOTHERDELETE is a linear-search.
;;;   The enumerator is a trailing-pointer-sublist-enumeration.
;;;      It enumerates the sublists of the tail of (MYHASH SYMBOL)
;;;      maintaining a trailing pointer.
;;;   The search-test is (EQ (CAR LIST) SYMBOL).
;;;   The action is a splice-out.
;;;      It splices an element out of a list after PREVIOUS-SUBLIST.

 ;; Faulty code produced for MYOTHERDELETE...

(DEFUN MYOTHERDELETE (SYMBOL)
  (LET* ((PREVIOUS-SUBLIST (MYHASH SYMBOL))
         (LIST (CDR PREVIOUS-SUBLIST)))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (WHEN (EQ (CAR LIST) SYMBOL)
        (RPLACD PREVIOUS-SUBLIST (CDDR PREVIOUS-SUBLIST))
        (RETURN NIL))
      #{LIST, the output sublist}
      (SETQ PREVIOUS-SUBLIST LIST)
      (SETQ LIST (CDR LIST)))))

 ;; Correct code would have been...

(DEFUN MYOTHERDELETE (SYMBOL)
  (LET* ((PREVIOUS-SUBLIST (MYHASH SYMBOL))
         (LIST (CDR PREVIOUS-SUBLIST)))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (WHEN (EQ (CAR #{LIST, the output sublist}) SYMBOL)
        (RPLACD PREVIOUS-SUBLIST (CDDR PREVIOUS-SUBLIST))
        (RETURN NIL))
      (SETQ PREVIOUS-SUBLIST LIST)
      (SETQ LIST (CDR LIST)))))

;;;; The HASH Sequence

;;; >Reinitialize database.
;;; >Define a<n> program MYHASH with a parameter SYMBOL.
;;; >Implement it as [(ARRAYFETCH TBL (ABS (REMAINDER (MAKNUM SYMBOL) TBLSIZE)))] .

;;; No coding errors in this sequence.

;;; (MYHASH SYMBOL)
;;;  Note: No explanation generated by KBE.

(DEFUN MYHASH (SYMBOL) (ARRAYFETCH TBL (ABS (REMAINDER (MAKNUM SYMBOL) TBLSIZE))))

;;;; The MEMQ Sequence

;;; >Reinitialize database.
;;; >Define a<n> program MYMEMQ with parameters OBJ and SEQUENCE.
;;; >Implement it as a linear-search.
;;; >Implement the enumerator as a list-enumeration of [SEQUENCE] .
;;; >Implement the search-test as an EQ of the element and [OBJ] .
;;; >Implement the action as [LIST] .
;;; >Define a<n> program MYOTHERMEMQ with parameters OBJ and SEQUENCE.
;;; >Implement it as a linear-search.
;;; >Implement the enumerator as a sublist-enumeration of [SEQUENCE] .
;;; >Implement the search-test as [(EQ (CAR LIST) OBJ)] .
;;; >Implement the action as [LIST] .

;;; Test failed!
;;; Bad code generated.

;;; (MYMEMQ OBJ SEQUENCE)
;;; The program MYMEMQ is a linear-search.
;;;   The enumerator is a list-enumeration.
;;;      It enumerates the elements of SEQUENCE.
;;;   The search-test is a binary-function.  It nil.
;;;   The action is LIST.

(DEFUN MYMEMQ (OBJ SEQUENCE)
  (LET* ((LIST SEQUENCE))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (IF (EQ #{(CAR LIST), the output element} OBJ) (RETURN LIST))
      (SETQ LIST (CDR LIST)))))

;;; (MYOTHERMEMQ OBJ SEQUENCE)
;;; The program MYOTHERMEMQ is a linear-search.
;;;   The enumerator is a sublist-enumeration.
;;;      It enumerates the sublists of SEQUENCE.
;;;   The search-test is (EQ (CAR LIST) OBJ).
;;;   The action is LIST.

 ;; Faulty code produced for MYOTHERMEMQ...

(DEFUN MYOTHERMEMQ (OBJ SEQUENCE)
  (LET* ((LIST SEQUENCE))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (IF (EQ (CAR LIST) OBJ) (RETURN LIST))
      #{LIST, the output sublist}
      (SETQ LIST (CDR LIST)))))

 ;; Correct code would have been...

(DEFUN MYOTHERMEMQ (OBJ SEQUENCE)
  (LET* ((LIST SEQUENCE))
    (LOOP DO
      (IF (NULL LIST) (RETURN NIL))
      (IF (EQ (CAR #{LIST, the output sublist}) OBJ) (RETURN LIST))
      (SETQ LIST (CDR LIST)))))

;;;; The SQRT Sequence

;;; >Reinitialize database.
;;; >Define a<n> program MYSQRT with a parameter NUM.
;;; >Implement it as a successive-approximation.
;;; >Implement the initial-approximation as [1.] .
;;; >Implement the improvement-step as an average of [APPROXIMATION] and [(// NUM APPROXIMATION)] .
;;; >Implement the criterion as an equality-within-epsilon of [(* APPROXIMATION APPROXIMATION)] and [NUM] .
;;; >Implement the epsilon as [0.0001] .
;;; >Define a<n> program MYOTHERSQRT with a parameter NUM.
;;; >Implement it as a successive-approximation.
;;; >Implement the initial-approximation as [1.] .
;;; >Implement the improvement-step as an average of [APPROXIMATION] and [(// NUM APPROXIMATION)] .
;;; >Implement the criterion as an equality-within-epsilon of [(* APPROXIMATION APPROXIMATION)] and [NUM] .
;;; >Append a parameter CRITERION.
;;; >Implement the epsilon as [CRITERION] .

;;; No coding errors in this sequence.

;;; (MYSQRT NUM)
;;; The program MYSQRT is a successive-approximation.
;;;   The initial-approximation is 1.
;;;   The criterion is an equality-within-epsilon.
;;;      It determines whether a first-number and NUM differ by less than
;;;      0.0001.
;;;   The improvement-step is an average.
;;;      It computes the average of APPROXIMATION and a second-number.

(DEFUN MYSQRT (NUM)
  (LET* ((APPROXIMATION 1))
    (LOOP DO
      (IF (< (ABS (- (* APPROXIMATION APPROXIMATION) NUM)) 0.0001) (RETURN APPROXIMATION))
      (SETQ APPROXIMATION (// (+ APPROXIMATION (// NUM APPROXIMATION)) 2.0)))))

;;; (MYOTHERSQRT NUM CRITERION)
;;; The program MYOTHERSQRT is a successive-approximation.
;;;   The initial-approximation is 1.
;;;   The criterion is an equality-within-epsilon.
;;;      It determines whether a first-number and NUM differ by less than
;;;      CRITERION.
;;;   The improvement-step is an average.
;;;      It computes the average of APPROXIMATION and a second-number.

(DEFUN MYOTHERSQRT (NUM CRITERION)
  (LET* ((APPROXIMATION 1))
    (LOOP DO
      (IF (< (ABS (- (* APPROXIMATION APPROXIMATION) NUM)) CRITERION) (RETURN APPROXIMATION))
      (SETQ APPROXIMATION (// (+ APPROXIMATION (// NUM APPROXIMATION)) 2.0)))))

;;;; The SUMLIST Sequence

;;; >Reinitialize database.
;;; >Define a<n> program SUMLIST with a parameter LIST.
;;; >Implement it as a sum of a list-enumeration of [LIST] .

;;; No coding errors in this sequence.

;;; (SUMLIST LIST)
;;; The program SUMLIST is a sum.
;;;   The number is a list-enumeration.
;;;      It enumerates the elements of LIST.

(DEFUN SUMLIST (LIST)
  (LET* ((SUM 0))
    (LOOP DO
      (IF (NULL LIST) (RETURN SUM))
      (SETQ SUM (+ SUM #{(CAR LIST), the output element}))
      (SETQ LIST (CDR LIST)))))
