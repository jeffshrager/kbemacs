;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 291-298; listing gc.

;;; (c) Copyright 1983, Massachusetts Institute of Technology
;;;     All Rights Reserved.
;;;     Permission to copy all or part of this material is granted,
;;;     provided that the copies are not made or distributed for resale,
;;;     the MIT copyright notice and reference to the source file and
;;;     the software distribution version appear, and that notice is
;;;     given that copying is by permission of Massachusetts Institute
;;;     of Technology.


;;;; Sanitation Engineering (GC)
(defvar *gc-target-area* nil "The area which is the target for GC moves")
(defvar *gc-next-address* nil "current-insertion point in area (fixnum)")
(defvar *gc-last-address* nil "last free cell in area (fixnum)")
;;; DB-REAP deletes every fact not IN in the current context.
;;; Backing up becomes impossible: facts about pushed contexts are destroyed.
(def-db-var *db-context-after-reap*
  (db-list (+ 1 *initial-db-context-number*) *initial-db-context-number*)
  "Context to be instantiated after a reap has been done.")
(defvar *db-context-count-after-reap* (+ 1 *initial-db-context-number*))
(defun db-reap ()
  (let ((context *db-context*))
    (setq *db-context-count* *db-context-count-after-reap*)
    (setq *db-context* *db-context-after-reap*)
    (dolist (name *db-context-names*)
      (let ((con (symeval name)))
        (cond ((equal con context) (set con (cdr *db-context*)))
              (t (set name nil)))))
    (dolist (cmarker *cmarkers*)
      (setf (in-layers cmarker)
            (cond ((cmarker-is-in? cmarker) (cdr *db-context*))
                  (t nil))) ;facts with this marker will be expunged below
      (setf (out-layers cmarker) nil))
    (db-expunge)))

;;; PDF page 292
;;; DB-EXPUNGE removes facts with no IN-LAYERS and unreferenced seg hunks.
;;; A fact may be visited more than once; set its cmarker before discarding it.
(defun db-expunge ()
  (let ((segs (cons nil *db-segs*)) (header (ncons nil)) (count 0))
    (repeat ((hsegs segs) hunk)
      (cond ((null (cdr hsegs)) (setq *db-segs* (cdr segs)) (return)))
      (setq hunk (db-index (cadr hsegs)))
      (cond ((over num <- 15. by (1- num) until (minusp num)
              and (repeat ((list (rplacd header (dbx num hunk))))
                    (cond ((null (cdr list))
                           (setf (dbx num hunk) (cdr header))
                           (return (null (cdr header)))))
                    (cond ((cond ((eq (cmarker (cadr list)) *null-cmarker*))
                                 ((null (in-layers (cmarker (cadr list))))
                                  (setf (cmarker (cadr list)) *null-cmarker*)
                                  (setq count (1+ count))))
                           (setf (cdr list) (cddr list)))
                          (t (pop list)))))
             (db-init-sym (cadr hsegs)) ;flush KBE properties...
             (setf (cdr hsegs) (cddr hsegs))) ;splice
            (t (pop hsegs))))
    (setq *cmarkers*
          (db-copylist (collect (cmarker *cmarkers*) (in-layers cmarker))))
    count))

;;; PDF page 293
;;; GET-DB-AREA returns an area and fixnum addresses of its next/last free cells.
;;; DB areas must fit within a single region.
(define-internal-option *recycle-old-db-areas*
  "GC may recycle used DB areas" t :boolean)
(defun get-db-area ()
  (let* ((area
           (or (if *recycle-old-db-areas* (pop *db-free-areas*))
               (car (push (make-area
                            ':name (intern-KBE
                                     (format nil "*DB-AREA-~D*"
                                             (incf *db-area-count*)))
                            ':region-size #o400000
                            ':representation ':list
                            ':gc #+symbolics ':temporary
                                 #-symbolics ':static)
                          *db-areas*))))
         (next (%pointer (make-list #o2000 ':area area)))
         (last (+ next #.(- #o2000 2))))
    (values area next last)))
(defun free-db-area (area)
  (cond ((memq area *db-areas*)
         (sys:reset-temporary-area area)
         (push area *db-free-areas*))
        ((eq area default-cons-area) ;we cheat and use this area initially
         ;(format t "~&;FREE-DB-AREA: Ignoring arg of DEFAULT-CONS-AREA.")
         nil) ;This seems debugged, so the warning is disabled for now...
        (t (barf "Area ~D., ~S, is not a DB area." area (area-name area)))))
(defmacro forward-db-value (sym)
  (once-only (sym) `(set ,sym (move-to-new-db-area (symeval ,sym)))))

;;; PDF page 294
;;; Assume one property is copied iff all properties have been copied.
(defmacro forward-db-properties (sym)
  (let ((p (gensym)) (f (gensym)))
    `(do ((,p (plist ,sym) (cddr ,p)) (,f nil))
         ((null ,p))
       (when (db-property? (car ,p))
         (unless ,f
           (setq ,f t)
           (cond ((= (%area-number (cadr ,p)) *gc-target-area*)
                  (return nil))))
         (cond ((eq (car ,p) 'DB-INDEX) ;treat cdr-coding specially...
                (let ((new (copylist (cadr ,p) *gc-target-area*)))
                  (do ((l new (cdr l)))
                      ((null l))
                    (setf (car l) (move-to-new-db-area (car l))))
                  (setf (cadr ,p) new)))
               (t (setf (cadr ,p) (move-to-new-db-area (cadr ,p)))))))))

;;; PDF page 295
;;; MOVE-TO-NEW-DB-AREA preserves sharing, circularity, and cdr-coding.
;;; Only lists are copied. DTP-FREE marks forwarded cells. The input must
;;; not already point into the target region. Copied originals are destroyed.
;;; GC-NEXT-ADDRESS keeps three free cells for car, cdr, and block pointer.
(defmacro compiler-add (&rest args) (eval `(+ ,@args)))
(defun move-to-new-db-area (cell) #+3600 cell #-3600
  (prog (copy do-cars)
    (cond ((not (listp cell)) (return cell))
          ((= (%p-data-type cell) dtp-free)
           (return (%make-pointer dtp-list (%p-pointer cell)))))
    (setq copy (%make-pointer dtp-list *gc-next-address*)
          do-cars *gc-next-address*)
   CDRS
    (%p-store-contents *gc-next-address* (car cell))
    (let ((cdr (cdr cell)))
      (%p-store-tag-and-pointer cell dtp-free *gc-next-address*)
      (setq cell cdr))
    (cond ((null cell) (%p-store-cdr-code *gc-next-address* cdr-nil))
          ((not (listp cell))
           ;;; PDF page 296
           (%p-store-cdr-code *gc-next-address* cdr-normal)
           (incf *gc-next-address*)
           (%p-store-contents *gc-next-address* cell)
           (%p-store-cdr-code *gc-next-address* cdr-error)
           (setq cell nil))
          ((= (%p-data-type cell) dtp-free)
           (%p-store-cdr-code *gc-next-address* cdr-normal)
           (incf *gc-next-address*)
           (%p-store-tag-and-pointer *gc-next-address*
                                    (compiler-add (lsh cdr-error 6) dtp-list)
                                    (%p-pointer cell))
           (setq cell nil)))
    (incf *gc-next-address*)
    (cond ((not (< *gc-next-address* *gc-last-address*))
           (let ((b (%pointer (make-list #o400 ':area *gc-target-area*))))
             (cond (cell
                    (%p-store-cdr-code (1- *gc-next-address*) cdr-normal)
                    (%p-store-tag-and-pointer *gc-next-address*
                                             (compiler-add
                                               (lsh cdr-error 6) dtp-list)
                                             b)
                    (incf *gc-next-address*)))
             (%p-store-tag-and-pointer *gc-next-address* dtp-free b)
             (setq *gc-next-address* b
                   *gc-last-address* (+ *gc-next-address* #.(- #o400 2))))))
    (cond (cell (go CDRS)))
   CARS
    (cond ((not (< do-cars *gc-next-address*)) (return copy))
          ((= (%p-data-type do-cars) dtp-free)
           (setq do-cars (%p-pointer do-cars)) (go CARS))
          ((or (= (%p-cdr-code do-cars) cdr-error)
               (not (= (%p-data-type do-cars) dtp-list)))
           (incf do-cars) (go CARS)))
    (let ((ptr (%p-pointer do-cars)))
      (cond ((= (%p-data-type ptr) dtp-free)
             (%p-store-pointer do-cars (%p-pointer ptr))
             (incf do-cars) (go CARS)))
      (setq cell (%make-pointer dtp-list ptr))
      (%p-store-pointer do-cars *gc-next-address*)
      (incf do-cars)
      (go CDRS))))

;;; PDF page 297
;;; DB-GC compacts the database and returns its storage use in words.
;;; Every variable and property pointing into *DB-AREA* must be known here.
(define-internal-option *gc-enabled* "Garbage collector enabled" t :boolean)
#+3600 ;GC isn't going to work on the 3600
(setq *gc-enabled* nil)
(defun db-gc ()
  (if *gc-enabled*
      (multiple-value-bind (*gc-target-area* *gc-next-address* *gc-last-address*)
          (get-db-area)
        (let ((start-p *gc-next-address*))
          (dolist (sym *db-vars*) (forward-db-value sym))
          (dolist (sym *db-context-names*) (forward-db-value sym))
          (dolist (sym *db-segs*) (forward-db-properties sym))
          (free-db-area (prog1 *db-area* (setq *db-area* *gc-target-area*)))
          (setq *db-size* (- *gc-next-address* start-p))))))

;;; PDF page 298
;;;; GC debugging tools
;;; Print forwarding pointers as #<DTP-Free address> = ...obj...
(comment
  (pp:define-generalized-print-function dtp-free (obj s)
      (and (listp obj) (= (%p-data-type obj) dtp-free))
    (format s "#<~@!DTP-Free ~S~.> = ~="
            (%pointer obj) (%make-pointer dtp-list (%p-pointer obj)))))
