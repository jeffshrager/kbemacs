;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>KBELIB.LISP
;;; PDF pages 235-242 (printed source pages 1-8).

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;the following is a dumb special little cliche so that you
;can say "a use of" something.  The only time you might
 ;want to say this is when you embed it in code
;eg '(eq (car {a use of the item}) 'foo)
(define-cliche use
  (primary-roles (thing))
  {thing})
;NOTE EACH CLICHE IS CODED IN THE EXACT STYLE THAT THE
;CODER PRODUCES IN ORDER TO FACILITATE COMPARISON.
;first we have the prototypes needed for the SQRT example
(DEFINE-CLICHE SUCCESSIVE-APPROXIMATION
   (PRIMARY-ROLES (INITIAL-APPROXIMATION)
    DESCRIBED-ROLES (INITIAL-APPROXIMATION CRITERION IMPROVEMENT-STEP)
    COMMENT "iteratively improves an approximation until {the criterion} is
             satisfied"
    main "Successive approximation"
    detail ("Successive approximation.  Beginning with an initial"
            "approximation of" initial-approximation ", tries to find"
            "a value for which" criterion "succeeds."
            "If that test fails, a new approximation is generated"
            "from the old by" improvement-step
            "This process is repeated until the test succeeds.")
    criterion "predicate expression")
  (LET* ((APPROXIMATION {the input initial-approximation}) )
    (LOOP DO
      (IF ({the criterion} APPROXIMATION) (RETURN APPROXIMATION) )
      (SETQ APPROXIMATION ({the improvement-step} APPROXIMATION) ))))
(DEFINE-CLICHE AVERAGE
    (PRIMARY-ROLES (FIRST-NUMBER SECOND-NUMBER)
     DESCRIBED-ROLES (FIRST-NUMBER SECOND-NUMBER)
     COMMENT "computes the average of {the first-number}
              and {the second-number}"
     main ("average of" first-number "and" second-number)
     detail ("Averages" first-number "and" second-number
             "by summing them and dividing by two.")
     first-number "thing to be averaged"
     second-number "thing to be averaged")
  (// (+ {the input first-number} {the input second-number}) 2.0))
(DEFINE-CLICHE SQUARE
    (PRIMARY-ROLES (NUMBER)
     DESCRIBED-ROLES (NUMBER)
     COMMENT "computes the square of {the number}")
  (* {the input number} 2))
(DEFINE-CLICHE EQUALITY-WITHIN-EPSILON
    (PRIMARY-ROLES (FIRST-NUMBER SECOND-NUMBER)
     DESCRIBED-ROLES (FIRST-NUMBER SECOND-NUMBER EPSILON)
     COMMENT "determines whether {the first-number} and
              {the second-number} differ by less than {the epsilon}"
;;; PDF page 236

     main ("equality within" epsilon "test")
     detail ("Succeeds when" first-number "and" second-number
             "are within" epsilon "of each other.")
     first-number "value being tested"
     second-number "value being tested"
     epsilon "epsilon")
  (< (ABS (- {the input first-number} {the input second-number}))
     {the epsilon}))
;;; PDF page 237

;here are the prototypes needed for the DELETE example.
;note we should have an element-accessor, but we cannot
;because it gets grouped up with the search-test wrecking the       :
;plan.  note the gastly kludge to get the search-test
;pulled separated out by the analyzer.  We could do more
;work in the analyzer to figure out that this should come out.
(compiler-let ((*separate-gens-and-terms* t))
    (DEFINE-CLICHE LINEAR-SEARCH
    (PRIMARY-ROLES (ENUMERATOR)
     DESCRIBED-ROLES (ENUMERATOR SEARCH-TEST ACTION)
     COMMENT "searches (in linear time)
              {the input structure of the enumerator}
              for an element satisfying {the search-test}"
     main "linear time search"
     detail ("Searches" enumerator "for an element which"
             "satisfies" search-test ". If found, returns"
             action ". Otherwise, returns NIL.")
     structure-of-enumerator "thing being searched")
  (LET* ((DATA {the input structure of the enumerator}))
    (LOOP DO
      (IF ({the empty-test of the enumerator} DATA) (RETURN NIL))
      (IF ({the search-test} DATA) (RETURN {the action}))
      (SETQ DATA ({the step of the enumerator} DATA))))) )
;this has a lot of problems, but it is the only thing that I can
;make work.  It cannot have a real unitary output prev, because
;that would make it not be an ssr.  It cannot have a temporal output
;prev becuase then there is no way to get the last value of it out
;when you hook data flow up to it.  This causes the variable PREV
;to exist when you add it into a plan so that you can use it later.
;Unfortunately, if you analyze and recode then this variable will
;get deleted since it isn't really used for anything.  That is
;to bad, we will just have to avoid doing that for now.
(DEFINE-CLICHE TRAILING-POINTER-LIST-ENUMERATION
    (PRIMARY-ROLES (LIST)
     DESCRIBED-ROLES (LIST)
     COMMENT "enumerates the elements of the tail of {the list}
              maintaining a trailing pointer"
     main ("trailing pointer list enumeration of" list)
     detail ("Enumerates the elements in the tail of the list returned by"
             list "maintaining a backpointer to successive values of"
             current.)
     list    "initial value computation"
     element    "element under examination")
  (LET* ((PREVIOUS-SUBLIST {the input list})        ;
         (LIST (CDR PREVIOUS-SUBLIST)))
    (LOOP DO
      (IF (NULL LIST) (RETURN PREVIOUS-SUBLIST) )
      {(CAR LIST), the output element}
      (SETQ PREVIOUS-SUBLIST LIST)
      (SETQ LIST (CDR LIST)))))
(DEFINE-CLICHE SPLICE-OUT
    (PRIMARY-ROLES (POSITION)
     DESCRIBED-ROLES (POSITION)
     COMMENT "splices an element out of a list after {the position}"
     main ("splice out of the successor to" position)
     detail ("Splices an element out of a list;"
             position "has its tail modified to be its tail's tail.")
     position "previous sublist")
  (LET* ((PREVIOUS-SUBLIST {the input position}))
;;; PDF page 238

    (RPLACD PREVIOUS-SUBLIST (CDDR PREVIOUS-SUBLIST) )
    NIL))
;;; PDF page 239

;some related cliches
(DEFINE-CLICHE TRAILING-POINTER-SUBLIST-ENUMERATION
    (PRIMARY-ROLES (LIST)
     DESCRIBED-ROLES (LIST)
     COMMENT "enumerates the sublists of the tail of {the list}
              maintaining a trailing pointer"
     main ("trailing pointer sublist enumeration of" list)
     detail ("Enumerates the sublsits in the tail of the list returned by"
             list "maintaining a backpointer to successive values of"
             sublist ".")
     list    "initial value computation"
     sublist "sublist under examination")
  (LET* ((PREVIOUS-SUBLIST {the input list})
         (LIST (CDR PREVIOUS-SUBLIST) ))
    (LOOP DO
      (IF (NULL LIST) (RETURN PREVIOUS-SUBLIST) )
      {LIST, the output sublist}
      (SETQ PREVIOUS-SUBLIST LIST)
      (SETQ LIST (CDR LIST)))))
(DEFINE-CLICHE SPLICE-IN
    (PRIMARY-ROLES (POSITION NEW-ELEMENT)
     DESCRIBED-ROLES (POSITION NEW-ELEMENT)
     COMMENT "splices {the new-element} into a list after {the position}"
     main ("splice in of" new-element "after" position)
     detail ("Splices" new-element
             "into a list.  A new cell is created with head"
             new-element "and tail the same as the tail of" position ";"
             position "has its tail modified to be" new-element ".")
     position "previous sublist"
     new-element "new element")
  (LET* ((PREVIOUS-SUBLIST {the input position}))
    (RPLACD PREVIOUS-SUBLIST
            (CONS {the input new-element} (CDR PREVIOUS-SUBLIST)))
    NIL))
;;; PDF page 240

;Here are the basic kinds of loop fragments.
;Note that since adding new fragments into toops doesn't work, and
;just combining the fragmants together into a loop expression
;before filling them in doesn't work much better, these are of      =
;rather limited utility.
(DEFINE-CLICHE ENUMERATION
    (PRIMARY-ROLES (STRUCTURE)
     DESCRIBED-ROLES (STRUCTURE ELEMENT-ACCESSOR EMPTY-TEST STEP)
     COMMENT "enumerates the elements of {the structure}")
  (LET* ((DATA {the input structure}))
    (LOOP DO
      (IF ({the empty-test} DATA) (RETURN))
      {({the element-accessor} DATA), the output element}
      (SETQ DATA ({the step} DATA)))))
(DEFINE-CLICHE GENERATION
    (PRIMARY-ROLES (STRUCTURE)
     DESCRIBED-ROLES (STRUCTURE ELEMENT-ACCESSOR STEP)
     COMMENT "generates the elements of  {the structure}")
  (LET* ((DATA {the input structure}))
    (LOOP DO
      {({the element-accessor} DATA),  the output element}
      (SETQ DATA  ({the step} DATA)))))
;note that a cliche map is defined in syslib.
(DEFINE-CLICHE ACCUMULATION
     (PRIMARY-ROLES (ITEM)
     DESCRIBED-ROLES  (ITEM OPERATOR ZERO-FOR-OPERATOR)
     COMMENT  "accumulates {the item}")
   (LET* ((RESULT {the input zero-for-operator}))
     (LooP DO
       (SETQ RESULT ({the operator} RESULT {the input item})))
     RESULT) )
(DEFINE-CLICHE SELECT
     (PRIMARY-ROLES (ITEM SELECTION-TEST)
      DESCRIBED-ROLES (ITEM SELECTION-TEST)
      COMMENT "selects a subseries of {the item}")
   (LET* (DATUM)
     (LOOP DO
       (SETQ DATUM {the input item})
       (IF ({the selection-test} DATUM)
           {DATUM, the output selected-item}))))
;;; PDF page 241

;Here are a few other example cliches just to test things.
(DEFINE-CLICHE LIST-ENUMERATION
    (PRIMARY-ROLES (LIST)
     DESCRIBED-ROLES (LIST)
     COMMENT "enumerates the elements of {the list}"
     main ("list enumeration of" list)
     detail ("Enumerates the successive elements of" list ".")
     element "element")
  (LET* ((LIST {the input list}))
    (LOOP DO
      (IF (NULL LIST) (RETURN))
      {(CAR LIST), the output element}
      (SETQ LIST (CDR LIST)))))
(DEFINE-CLICHE SUBLIST-ENUMERATION
    (PRIMARY-ROLES (LIST)
     DESCRIBED-ROLES (LIST)
     COMMENT "enumerates the sublists of {the list}")
  (LET* ((LIST {the input list}))
    (LOOP DO
      (IF (NULL LIST) (RETURN))
      {LIST, the output sublist}
      (SETQ LIST (CDR LIST)))))
(DEFINE-CLICHE VECTOR-ENUMERATION
    (PRIMARY-ROLES (VECTOR)
     DESCRIBED-ROLES (VECTOR)
     COMMENT "enumerates the elements of  {the vector}")
  (LET* ((I 0)
         (VECTOR {the  input vector})
         (SIZE (ARRAY-ACTIVE-LENGTH VECTOR)))
    (LOOP DO
      (IF (NOT  (< I SIZE)) (RETURN))
      {(AREF VECTOR  I), the output element}
      (SETQ  I (1+ I)))))
(DEFINE-CLICHE LIST-GENERATION
    (PRIMARY-ROLES  (LIST)
     DESCRIBED-ROLES  (LIST)
     COMMENT  "generates the elements of {the list}")
   (LET* ((LIST {the input list}))
     (LOOP DO
      {(CAR  LIST), the output element}
       (SETQ LIST (CDR LIST)))))

;;; PDF page 242
(DEFINE-CLICHE SUM
    (PRIMARY-ROLES (NUMBER)
     DESCRIBED-ROLES (NUMBER)
     COMMENT "accumulates the sum of {the number}")
  (LET* ((SUM 0))
    (LOOP DO
      (SETQ SUM (+ SUM {the input number})))
    SUM))
(DEFINE-CLICHE PRODUCT
    (PRIMARY-ROLES (NUMBER)
     DESCRIBED-ROLES (NUMBER)
     COMMENT "accumulates the product of {the number}")
  (LET* ((PRODUCT 1))
    (LOOP DO
      (SETQ PRODUCT (* PRODUCT {the input number})))
    PRODUCT))
