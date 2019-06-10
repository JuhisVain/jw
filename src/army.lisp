(in-package :war)

;;; Currently at data.lisp:
;;(defstruct army
;;  (id)
;;  (x) (y)
;;  (troops)
;;  (movement)
;;  (counter))

(defvar *unit-types* (make-hash-table :test 'eq))
(defvar *unit-type-movecosts* (make-hash-table :test 'eq))

(defun setup-unit-type-movecosts (type-name tile-cost-alist)
  (gethash type-name *unit-type-movecosts*))

(defun setup-unit-type (unit-name move-type)
  )

(defun army-movecost (xy dir &optional (world *world*))
  ())
