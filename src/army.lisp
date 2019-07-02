(in-package :war)

;;; Currently at data.lisp:
;;(defstruct army
;;  (id)
;;  (x) (y)
;;  (troops)
;;  (movement)
;;  (counter))

;; Army-troops should hold alist ((unit-name . amount) etc...)

;; Using a struct or array for *unit-type-(road-)movecosts* might be smarter,
;; but will have to be defined procedurally
(defvar *unit-types* (make-hash-table :test 'eq)) ; dragoon # cavalry
(defvar *unit-type-movecosts* (make-hash-table :test 'eq)) ; cavalry # ((grass 2) (hill 3) ...)
(defvar *unit-type-road-movecosts* (make-hash-table :test 'eq))

(defvar *road-types* nil) ; (road rail maglev teslavacuumtube footpath etc..)

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

			   ;; TODO: Tile-location should also most likely override any tile type

			   (if (car road-river) ; if road
			       (progn ; road override

				 ;; truck's roadmove is 1 and railmove is 3
				 ;; train's roadmove is 4 and railmove is 2
				 ;; If (truck train) moves on (road rail) as one unit
				 ;; -> movecost should be 2
				 ;; if only truck moves there -> movecost is 1
				 ;; if only train -> movecost is 2
				 ;;; meaning we want the lowest movecost of the unittype whose lowest cost for roadtypes
				 ;; is highest of all unittypes' lowest movecosts on roadtypes... ffs



				 ;; TODO: does not work. multiroad movement cannot be calculated with (slowest-movecosts)
				 ;; the types of roads available will influence the minimal costs required.
				 ;; write new func
				 '(dolist (road-type (car road-river)) ; find highest move cost by road
				   (let ((current-cost (cadr (assoc road-type slow-moves))))
				     (when (>= current-cost cost)
				       (setf cost current-cost))))
				 (setf final-cost 2))
			       
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

(defmacro defmovecosts (movement-type &rest terrain-costs)
  "*Road-types* needs to be set up before using this. Defines terrain specific entry costs
for movement-type."
  (let ((movecosts nil)
	(roadcosts nil))
    (dolist (tc terrain-costs)
      (if (member (car tc) *road-types*)
	  (push tc roadcosts)
	  (push tc movecosts)))
    `(setf (gethash ',movement-type *unit-type-movecosts*) ',movecosts
	   (gethash ',movement-type *unit-type-road-movecosts*) ',roadcosts)))

(defun test-slowest-movecosts ()
  ;; This is dumb dumb dumb dummy data only for testing
  (setf *road-types* '(rail road))
  (setf (gethash 'commando *unit-types*) 'infantry)
  (setf (gethash 'dragoon *unit-types*) 'cavalry)
  (setf (gethash 'jeep *unit-types*) 'wheeled)
  (setf (gethash 'flak88 *unit-types*) 'towed)
  (setf (gethash 'pendolino *unit-types*) 'rail)
  (setf (gethash 'infantry *unit-type-movecosts*)
	'((grass 3) (hill 5) (mountain 10) (forest 4) (sea 10000) (city 3) (stream 3) (rail 3) (road 3)))
  (setf (gethash 'cavalry *unit-type-movecosts*)
	'((grass 2) (hill 5) (mountain 12) (forest 5)(sea 10000) (city 3)(stream 3) (rail 2) (road 2)))
  (setf (gethash 'wheeled *unit-type-movecosts*)
	'((grass 2) (hill 5) (mountain 20) (forest 10)(sea 10000) (city 2)(stream 10) (rail 3) (road 1)))
  (setf (gethash 'towed *unit-type-movecosts*)
	'((grass 4) (hill 10) (mountain 14) (forest 7)(sea 10000) (city 3)(stream 10) (rail 5) (road 3)))
  (setf (gethash 'rail *unit-type-movecosts*)
	'((grass 4) (hill 5) (mountain 20) (forest 10)(sea 10000) (city 2)(stream 10) (rail 3) (road 4)))
  
  (slowest-movecosts '((commando . 10) (dragoon . 50) (jeep . 10) (flak88 . 10)))
  )

(defun test-movecosts ()
  (test-slowest-movecosts) ; setup dummy movetype data
  ;; can't set *testunit*s here since sdl needs to be initalized -> do it manually
  (when (>= (length *testunit*) 2)
    (setf (army-troops (car *testunit*)) '((jeep . 10) (pendolino . 2)))
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
      (let ((unit-type (car type-costs)) ; cavalry
	    (unit-road-costs))
	
	(dolist (costs (cdr type-costs)) ; ( (tile-type move-cost) ...)
	  (let* ((type (car costs)) ; grass
		 (is-road (member type *road-types*))
		 (cost (cadr costs)) ; integer
		 (old (assoc type slowest))) ; (grass integer unit-types..)
	    (cond (is-road
		   nil) ; can't be done here
		  ((null old)
		   (push (append costs (list unit-type)) slowest))
		  ((> cost (cadr old))
		   (rplacd old (list cost unit-type)))
		  ((= cost (cadr old))
		   (rplacd old (append (cdr old) (list unit-type)))))
	    ))
	))
    slowest))
