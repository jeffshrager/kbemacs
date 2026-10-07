;;; -*- Mode:LISP; Package:ZWEI; Base:10; Fonts:MEDFNB; -*-

;;; Source: OZ:<KBE.SOURCE>POSSIBILITIES.LISP.6, 26 July 1984 14:08:40
;;; PDF pages 132-136 (printed source pages 1-5).

;;; Unfinished-Edits Subnodes
(defflavor unfinished-edits-subnode (strings) (function-spec-subnode)
           :initable-instance-variables
           :gettable-instance-variables)
(defmethod (unfinished-edits-subnode :description) (stream)
  (format stream "Edit ~A ~S~%~{ ~A~%~}"
          (si:definition-type-name (send self ':definition-type))
          (send self ':function-spec)
          strings))

                                        ;333 Unfinished-edits Nodes
(define-special-buffer-type
    unfinished-edits-buffer
    ((context nil)
     (tagged-string-list nil))
  (header-description-1-subnode-special-purpose-buffer-mixin
   possibilities-special-purpose-buffer)
  :unfinished-edits "Unfinished-Edits"
  (:buffer-init-options :set-context :set-tagged-string-list)
  :settable-instance-variables)

(defmethod (unfinished-edits-buffer :header-description-1) (stream plural-p)
  (if context
      (format stream "Unfinished edit~P~@[ in ~A~]"
              plural-p context)))
(defmethod (unfinished-edits-buffer :document) (ignore)
  (format t "Complete unfinished edits~@[ in ~A~]." context))
(defmethod (unfinished-edits-buffer :reusable) (ignore) t)
(defmethod (unfinished-edits-buffer :revert) ()
  (do ((l tagged-string-list (cdr l)))
      ((null l))
    (send self ':add-node 'unfinished-edits-subnode
          ':function-spec   (caar l)
          ':definition-type 'defun
          ':strings         (cdar l))))
(defmethod (unfinished-edits-buffer :description) ()
  (or context ""))
(defmethod (unfinished-edits-buffer :list-subnodes) (first-node)
  (do ((node first-node (node-next node)))
      ((not node) )
    (format t "~&For the ~A "
            (si:definition-type-name (send node  ':definition-type)))
    (send *typeout-window*  ':item 'function-name
          (send node  ':function-spec))
    (format t "~%~{ ~A~%~}~%" (send node  ':strings))))

;;; This fixes a bug in SUBNODE-SPECIAL-PURPOSE-BUFFER.
;;; It is claimed to be fixed in Release 5, at which time this code
;;; should be discarded if possible.
(defmethod (unfinished-edits-buffer :add-node) (flavor &rest options)
  (let ((node (lexpr-funcall #'make-instance flavor options)))
    (create-interval nil nil node)
    (send interval-stream ':set-bp (interval-last-bp node))
    (send node ':description interval-stream)
    (let* ((infs (last inferiors) )
           (inf (car infs)))
      (setf (node-next inf) node)
      (setf (node-previous node) inf)
      (rplacd infs (ncons node))
      (setf (node-superior node) self)
      (let ((inf-last-line   (bp-line  (interval-last-bp  inf)))   ;changed
            (node-first-line (bp-line  (interval-first-bp node)))) ;changed
        (setf (line-next inf-last-line) node-first-line)           ; changed
        (setf (line-previous node-first-line)  inf-last-line))     ;changed
      (move-bp last-bp (interval-last-bp  node))                   ; changed
      (move-bp (interval-last-bp  inf) (interval-first-bp node)))
                                        ;   (setf (node-tick node)  ':read-only) ;commented out in the original
    node))

(compile-flavor-methods unfinished-edits-subnode
                        unfinished-edits-buffer)
