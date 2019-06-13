(in-package :war)

;;; Currently at data.lisp:
;;(defstruct army
;;  (id)
;;  (x) (y)
;;  (troops)
;;  (movement)
;;  (counter))

;; Army-troops should hold alist ((unit-name . amount) etc...)

;; Using a struct for *unit-type-movecosts* might be smarter,
;; but will have to be defined procedurally
(defvar *unit-types* (make-hash-table :test 'eq)) ; dragoon # cavalry
(defvar *unit-type-movecosts* (make-hash-table :test 'eq)) ; cavalry # ((grass 2) (hill 3) ...)

(defun setup-unit-type-movecosts (type-name tile-cost-alist)
  (gethash type-name *unit-type-movecosts*))

(defun setup-unit-type (unit-name move-type)
  )


(defun move-area (army &optional (world *world*))
  (let (;(frontier (make-heap))
	;(came-from (make-hash-table :test 'equal))
	(start-x (army-x army))
	(start-y (army-y army))
	(slow-moves (slowest-movecosts (army-troops army)))
	(move-range (army-movement army)))

    (setf *current-move-area*  ;; aww shit
	  (breadth-first-fill
	   start-x start-y :range move-range
	   :costfunc #'(lambda (x y world)
			 (let ((cost 1)) 
			   (dolist (tile-type (tile-type (tile-at x y world)))
			     (let ((current-cost (cadr (assoc tile-type slow-moves))))
			       (when (>= current-cost cost)
				 (setf cost current-cost))))
			   cost))))
    )
  )


(defun test-slowest-movecosts ()
  (setf (gethash 'commando *unit-types*) 'infantry)
  (setf (gethash 'dragoon *unit-types*) 'cavalry)
  (setf (gethash 'jeep *unit-types*) 'wheeled)
  (setf (gethash 'flak88 *unit-types*) 'towed)
  (setf (gethash 'infantry *unit-type-movecosts*)
	'((grass 3) (hill 5) (mountain 10) (forest 4) (sea 10000) (city 3)))
  (setf (gethash 'cavalry *unit-type-movecosts*)
	'((grass 2) (hill 5) (mountain 12) (forest 5)(sea 10000) (city 3)))
  (setf (gethash 'wheeled *unit-type-movecosts*)
	'((grass 2) (hill 5) (mountain 20) (forest 10)(sea 10000) (city 2)))
  (setf (gethash 'towed *unit-type-movecosts*)
	'((grass 4) (hill 10) (mountain 14) (forest 7)(sea 10000) (city 3)))
  
  (slowest-movecosts '((commando . 10) (dragoon . 50) (jeep . 10) (flak88 . 10)))
  )

(defun slowest-movecosts (unit-list)
  "Returns list containing highest move costs on different tiles for units in unit-list.
In form: ( (tile-type move-cost ..rest-slowest-units..) ...)"
  (let ((unit-type-list)
	(slowest))
    (dolist (unit unit-list)
      (pushnew (gethash (car unit) *unit-types*) unit-type-list))

    (dolist (type-costs
	      (mapcar #'(lambda (unit-type) ; ( (unit-type ((tile-type move-cost) ...)) ...)
			  (cons unit-type
				(gethash unit-type *unit-type-movecosts*)))
		      unit-type-list))
      (let ((unit-type (car type-costs))) ; cavalry
	(dolist (costs (cdr type-costs)) ; ( (tile-type move-cost) ...)
	  (let* ((type (car costs)) ; grass
		 (cost (cadr costs)) ; integer
		 (old (assoc type slowest))) ; (grass integer unit-types..)
	    (cond ((null old)
		   (push (append costs (list unit-type)) slowest))
		  ((> cost (cadr old))
		   (rplacd old (list cost unit-type)))
		  ((= cost (cadr old))
		   (rplacd old (append (cdr old) (list unit-type)))))
	    ))))
    slowest))
