;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 98-104; listing sets.

;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.

;;; SETS: Useful set operations

;;;; Sets

;;; The following fns define the basic operations on sets.  They come in
;;; 4 versions: sets with membership determined by equal (set), sets
;;; with membership determined by eq (qset), sets with membership
;;; determined by = that are sorted smallest number first (nset), and
;;; sets with membership determined by eq where the elements are ordered
;;; by MAKNUM smallest first (fset).
;;;
;;; Note: In principle, fsets shouldn't work on the LispM because
;;;       the gc might move stuff and pointer values might change.
;;;       Since the gc is never really used, we ignore that problem
;;;       and let things be fast.

;;; The operations are:
;;;
;;;  1. make(n/f/q/ )set -- take a list and DESTRUCTIVELY convert it
;;;                          to a set -- in particular delete duplicates.
;;;
;;;  2. (n/f/q/ )set=    -- determine if two sets are the same
;;;  3. (n/f/q/ )setU    -- form the union of two sets
;;;  4. (n/f/q/ )set^    -- form the intersection of two sets
;;;  5. (n/f/q/ )set-    -- the result is the elements of the first
;;;                          set not in the second set
;;;  6. (n/f/q/ )set<    -- tests whether the first set is a subset of
;;;                          the second
;;;  7. (n/f)setmem      -- tests whether an element is in a set (member
;;;                          and memq are used for sets and qsets)
;;;

;;; The fsets and nsets (fast sets) are the most interesting. The others
;;;  just slug along slowly.  They preserve the order of the sets for
;;;  convenience.  It is intended that fsets be used whenever you care the
;;;  least bit about speed.

;;;; EQUAL sets

;;; SETs: Sets based on Lisp equal
;;;
;;; Note: member is one of these operations.

(defun makeset (list)
  (do ((l list (cdr l)))
      ((null (cdr l)) list)
    (setf (cdr l) (delete (car l) (cdr l)))))

(defun set= (setx sety)
  (and (set< setx sety) (set< sety setx)))

(defun set< (setx sety)
  (do ((subsetx setx (cdr subsetx)))
      ((null subsetx) t)
    (cond ((not (member (car subsetx) sety))
           (return nil)))))

(defun setU (setx sety)
  (do ((subsetx setx (cdr subsetx))
       (newset  sety))
      ((null subsetx) newset)
    (cond ((not (member (car subsetx) sety))
           (push (car subsetx) newset)))))

(defun set^ (setx sety)
  (do ((subsetx setx (cdr subsetx))
       (newset nil))
      ((null subsetx) (nreverse newset))
    (cond ((member (car subsetx) sety)
           (push (car subsetx) newset)))))

(defun set- (setx sety)
  (do ((subsetx setx (cdr subsetx))
       (newset nil))
      ((null subsetx) (nreverse newset))
    (cond ((not (member (car subsetx) sety))
           (push (car subsetx) newset)))))

;;;; Sorted fixnum sets

;;; NSETs: Sorted fixnum sets

(defun makenset (list)
  (setq list (sort list #'<))
  (do ((l list))
      ((null (cdr l)) list)
    (cond ((= (car l) (cadr l)) (rplacd l (cddr l)))
          (t (setq l (cdr l))))))

(defun nset= (nsetx nsety)
  (do ((fx nsetx (cdr fx))
       (fy nsety (cdr fy)))
      ((or (null fx) (null fy))
       (and (null fx) (null fy))) ;return T only if sets are same size
    (cond ((not (= (car fx) (car fy)))
           (return nil)))))

(defun nset< (nsetx nsety)
  (do ((x (car nsetx))
       (y (car nsety)))
      (nil)
    (cond ((null nsetx) (return t))
          ((null nsety) (return nil))
          ((< x y) (return nil))
          ((< y x) (setq nsety (cdr nsety) y (car nsety)))
          (t (setq nsetx (cdr nsetx) x (car nsetx))
             (setq nsety (cdr nsety) y (car nsety))))))

(defun nsetmem (x nsety)
  (do ((yl nsety (cdr yl)))
      ((null yl) nil)
    (cond ((= x (car yl)) (return t))
          ((< x (car yl)) (return nil)))))

(defun nsetU (nsetx nsety)
  (do ((res)
       (x (car nsetx))
       (y (car nsety)))
      (nil)
    (cond ((null nsetx)
           (return (nreconc res (append nsety nil))))
          ((null nsety)
           (return (nreconc res (append nsetx nil))))
          ((< x y)
           (push x res)
           (setq nsetx (cdr nsetx) x (car nsetx)))
          ((< y x)
           (push y res)
           (setq nsety (cdr nsety) y (car nsety)))
          (t
           (push x res)
           (setq nsetx (cdr nsetx) x (car nsetx))
           (setq nsety (cdr nsety) y (car nsety))))))

(defun nset^ (nsetx nsety)
  (do ((res)
       (x (car nsetx))
       (y (car nsety)))
      (nil)
    (cond ((or (null nsetx) (null nsety)) (return (nreverse res)))
          ((< x y) (setq nsetx (cdr nsetx) x (car nsetx)))
          ((< y x) (setq nsety (cdr nsety) y (car nsety)))
          (t (push x res)
             (setq nsetx (cdr nsetx) x (car nsetx))
             (setq nsety (cdr nsety) y (car nsety))))))

(defun nset- (nsetx nsety)
  (do ((res)
       (x (car nsetx))
       (y (car nsety)))
      (nil)
    (cond ((or (null nsetx) (null nsety))
           (return (nreconc res (append nsetx nil))))
          ((< x y)
           (push x res)
           (setq nsetx (cdr nsetx) x (car nsetx)))
          ((< y x)
           (setq nsety (cdr nsety) y (car nsety)))
          (t
           (setq nsetx (cdr nsetx) x (car nsetx))
           (setq nsety (cdr nsety) y (car nsety))))))

;;;; Fast sets

;;; FSETs: Fast sets based on exploiting pointer equality

;;; If you know that none of the addresses of things are going to
;;; change then you can still use the sorted sets on the LispM,
;;; but you better be right!
;;;
;;; Note: If the GC gets turned back on, the fset definitions should
;;;       be made back into QSET operations. FSETMEM is MEMQ. There
;;;       is no QSETMEM.

(defun makefset (list)
  (setq list (sort list #'(lambda (x y) (< (%pointer x) (%pointer y)))))
  (do ((l list))
      (nil)
    (cond ((null (cdr l)) (return list))
          ((eq (car l) (cadr l)) (rplacd l (cddr l)))
          (t (setq l (cdr l))))))

(defun fset= (fsetx fsety)
  (do ((fx fsetx (cdr fx))
       (fy fsety (cdr fy)))
      ((or (null fx) (null fy))
       (and (null fx) (null fy))) ;return T only if sets are same size
    (cond ((not (eq (car fx) (car fy)))
           (return nil)))))

(defun fset< (fsetx fsety)
  (do ((x (car fsetx))
       (y (car fsety)))
      (nil)
    (cond ((null fsetx)
           (return t))
          ((null fsety)
           (return nil))
          ((< (%pointer x) (%pointer y))
           (return nil))
          ((< (%pointer y) (%pointer x))
           (setq fsety (cdr fsety) y (car fsety)))
          (t
           (setq fsetx (cdr fsetx) x (car fsetx))
           (setq fsety (cdr fsety) y (car fsety))))))

(defun fsetmem (x fsety)
  (do ((yl fsety (cdr yl)))
      ((null yl) nil)
    (cond ((eq x (car yl)) (return t))
          ((< (%pointer x) (%pointer (car yl))) (return nil)))))

(defun fsetU (fsetx fsety)
  (do ((res)
       (x (car fsetx))
       (y (car fsety)))
      (nil)
    (cond ((null fsetx) (return (nreconc res (append fsety nil))))
          ((null fsety) (return (nreconc res (append fsetx nil))))
          ((< (%pointer x) (%pointer y))
           (push x res)
           (setq fsetx (cdr fsetx) x (car fsetx)))
          ((< (%pointer y) (%pointer x))
           (push y res)
           (setq fsety (cdr fsety) y (car fsety)))
          (t
           (push x res)
           (setq fsetx (cdr fsetx) x (car fsetx))
           (setq fsety (cdr fsety) y (car fsety))))))

(defun fset^ (fsetx fsety)
  (do ((res)
       (x (car fsetx))
       (y (car fsety)))
      (nil)
    (cond ((or (null fsetx) (null fsety))
           (return (nreverse res)))
          ((< (%pointer x) (%pointer y))
           (setq fsetx (cdr fsetx) x (car fsetx)))
          ((< (%pointer y) (%pointer x))
           (setq fsety (cdr fsety) y (car fsety)))
          (t
           (push x res)
           (setq fsetx (cdr fsetx) x (car fsetx))
           (setq fsety (cdr fsety) y (car fsety))))))

(defun fset- (fsetx fsety)
  (do ((res)
       (x (car fsetx))
       (y (car fsety)))
      (nil)
    (cond ((or (null fsetx) (null fsety))
           (return (nreconc res (append fsetx nil))))
          ((< (%pointer x) (%pointer y))
           (push x res)
           (setq fsetx (cdr fsetx) x (car fsetx)))
          ((< (%pointer y) (%pointer x))
           (setq fsety (cdr fsety) y (car fsety)))
          (t
           (setq fsetx (cdr fsetx) x (car fsetx))
           (setq fsety (cdr fsety) y (car fsety))))))

;;;; EQ sets

;;; QSETs: Sets based on EQ-ness

;;; The following implement set operations on eq sets
;;;  (Note: MEMQ is one of these operations)

(defun makeqset (list)
  (do ((l list (cdr l)) (set nil))
      ((null l) set)
    (cond ((not (memq (car l) (cdr l)))
           (push (car l) set)))))

(defun qset= (qsetx qsety)
  (and (qset< qsetx qsety) (qset< qsety qsetx)))

(defun qset< (qsetx qsety)
  (do ((ql qsetx (cdr ql)))
      ((null ql) t)
    (cond ((not (memq (car ql) qsety))
           (return nil)))))

(defun qsetU (qsetx qsety)
  (let ((newset qsety))
    (dolist (x qsetx)
      (cond ((not (memq x qsety))
             (push x newset))))
    newset))

(defun qset^ (qsetx qsety)
  (let ((newset nil))
    (dolist (x qsetx)
      (cond ((memq x qsety)
             (push x newset))))
    (nreverse newset))) ; This NREVERSE shouldn't be necessary, should it?

(defun qset- (qsetx qsety)
  (let ((newset nil))
    (dolist (x qsetx)
      (cond ((not (memq x qsety))
             (push x newset))))
    (nreverse newset))) ; Someone seems to be relying on this NREVERSE!
                        ; The following piece of code would not ANALYZE
                        ; (gets a BARF in TD-COPY-OUTPUT-DFLOW):
                        ;   (DEFINE TEST-DO2 (I)
                        ;     (DO ((X 1 (1+ X)))
                        ;         ((= X I))
                        ;       (SETQ X (+ X 2))
                        ;       (PRINT 'FOO)))
                        ; At some point, we should find and fix the
                        ; tab that relies on sets to have order! -kmp
