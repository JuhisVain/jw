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
  (let ((start-x (army-x army))
	(start-y (army-y army))
	(slow-moves (slowest-movecosts (army-troops army)))
	(move-range (army-movement army)))

    (setf *current-move-area*  ;; aww shit
	  (breadth-first-fill
	   start-x start-y :range move-range
	   :costfunc #'(lambda (x y dir world)
			 (let* ((cost 0)
				(final-cost 0)
				(tile-move-types (list-tile-move-types x y dir world))
				(road-river (car tile-move-types)))

			   (if (car road-river) ; if road
			       (progn ; road override
				 (setf final-cost 10000)
				 ;; TODO: think this through later.. I'm not sure if this logical
				 (dolist (road-type (car road-river)) ; find lowest road moves
				   (let ((current-cost (cadr (assoc road-type slow-moves))))
				     (when (< current-cost final-cost)
				       (setf final-cost current-cost)))
				   ))
			       
			       (progn ; if river
				 (when (cdr road-river) ; Add river crossing cost
				   (incf final-cost (cadr (assoc (cdr road-river) slow-moves))))
				 
				 (dolist (tile-type (cdr tile-move-types)) ; find highest type entry cost
				   (let ((current-cost (cadr (assoc tile-type slow-moves))))
				     (when (>= current-cost cost)
				       (setf cost current-cost))))

				 (incf final-cost cost) ; add highest type entry
				 ))
			   final-cost)))) ; return total
    ))

(defun list-tile-move-types (x y entry-direction &optional (world *world*))
  "Lists all symbols that affect armies' movement to tile X Y from DIRECTION.
First element will be cons of roadtypeslist and river."
  (let* ((to (tile-at x y world))
	 (river (cdr (assoc entry-direction (tile-river-borders to))))
	 (road (cdr (assoc entry-direction (tile-road-links to)))))
    (list*
     (cons road river)
     (tile-type to))))

(defun test-slowest-movecosts ()
  (setf (gethash 'commando *unit-types*) 'infantry)
  (setf (gethash 'dragoon *unit-types*) 'cavalry)
  (setf (gethash 'jeep *unit-types*) 'wheeled)
  (setf (gethash 'flak88 *unit-types*) 'towed)
  (setf (gethash 'infantry *unit-type-movecosts*)
	'((grass 3) (hill 5) (mountain 10) (forest 4) (sea 10000) (city 3) (stream 3) (rail 3)))
  (setf (gethash 'cavalry *unit-type-movecosts*)
	'((grass 2) (hill 5) (mountain 12) (forest 5)(sea 10000) (city 3)(stream 3) (rail 2)))
  (setf (gethash 'wheeled *unit-type-movecosts*)
	'((grass 2) (hill 5) (mountain 20) (forest 10)(sea 10000) (city 2)(stream 10) (rail 2)))
  (setf (gethash 'towed *unit-type-movecosts*)
	'((grass 4) (hill 10) (mountain 14) (forest 7)(sea 10000) (city 3)(stream 10) (rail 4)))
  
  (slowest-movecosts '((commando . 10) (dragoon . 50) (jeep . 10) (flak88 . 10)))
  )

(defun test-movecosts ()
  (test-slowest-movecosts) ; setup dummy movetype data
  ;; can't set *testunit*s here since sdl needs to be initalized -> do it manually
  (when (>= (length *testunit*) 2)
    (setf (army-troops (car *testunit*)) '((jeep . 10) (flak88 . 2)))
    (setf (army-troops (cadr *testunit*)) '((dragoon . 20) (commando . 20)))
  ))

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
