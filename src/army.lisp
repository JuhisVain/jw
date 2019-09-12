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

(defvar *road-types* '(rail road)) ; (road rail maglev teslavacuumtube footpath etc..)


;;; !!!!!!!!!!!!!!!!!!!!!!! redo all
;;; All visibles need to be placed in the same hashtable, as a unit's vision will affect
;; the results of other units' visions

;;; Notes:
;; units should have a stat to determine how well they can see through various terrain types.
;; Infantry on grass won't see "through" a city, but a hot air balloon might etc...
;; Multiple units with vision -> all seen tiles must be placed in same hash-table
;; Moving units ???

(defun list-ring (xy radius)
  "Returns list of coordinate conses around XY at distance radius."
  (let ((current xy)
	(ring nil))
    ;; Might Just wanto to use a custom list instead of +std-short-dirs+
    ;; -> coordinate could be set with + or -
    (dotimes (count radius) ;; Move current pointer from center to SW corner of ring
      (setf current (neighbour-tile-coords (car current) (cdr current) 'sw)))
    (dolist (dir +std-short-dirs+) ;; Travel the edge of the ring and store coords
      (dotimes (count radius)
	(push current ring)
	(setf current (neighbour-tile-coords (car current) (cdr current) dir))))
    ring))

(defun seen (coord vision-hash-table)
  (gethash coord vision-hash-table 0))

;; Not a good visibilty func but does it's job for now
'(progn (defparameter xxx
	  (visible-area (car *testunit*)
			5
			#'(lambda (target parent-1 p1-weight parent-2 p2-weight visibles)
			    (let ((total-weight (+ p1-weight p2-weight))
				  (grass 0.95)
				  (hill 0.75)
				  (mountain 0.5)
				  (sea 1))
			      (declare (special grass hill mountain sea))
			      (*
			       (apply #'min
				      (mapcar #'symbol-value
					      (tile-type (tile-at (car target) (cdr target)))))
			       (+ (* (or (gethash parent-1 visibles) 0) (/ p1-weight total-weight))
				  (* (gethash parent-2 visibles) (/ p2-weight total-weight)))
			       )))))
  (maphash #'(lambda (x y) (format t "~&~a : ~a ~%" x y)) xxx))


(defun visible-area (army max-range vis-cost-func &optional (world *world*))
  "VIS-COST-FUNC should take target coord, parent coord 1, parcoord1's weight,
parcoord2 ,parcoord2's weight and returns float between 0 and 1."
  (let ((visibles (make-hash-table :test 'equal))
	(army-xy (cons (army-x army) (army-y army))))
    (setf (gethash army-xy visibles) 1)
    ;; Populate hashtable with cardinal columns:
    (dolist (dir +std-short-dirs+)
      (do ((distance 1 (1+ distance))
	   (current (neighbour-tile-coords (car army-xy) (cdr army-xy) dir)
		    (neighbour-tile-coords (car current) (cdr current) dir)))
	  ((or (> distance max-range) (null current)))
	(setf (gethash current visibles)
	      (funcall vis-cost-func current
		       nil 0
		       (neighbour-tile-coords (car current) (cdr current) (oppdir dir))
		       1
		       visibles))
	))
    
    (dolist (sector '((n . ne) (ne . se) (se . s)
		      (s . sw) (sw . nw) (nw . n)))
      (let* ((dir1 (car sector))
	     (odir1 (oppdir dir1))
	     (dir2 (cdr sector))
	     (odir2 (oppdir dir2))
	     (sector-head (neighbour-tile-coords (car army-xy) (cdr army-xy) (car sector))))
	(when sector-head
	  (do ((column-head (neighbour-tile-coords (car sector-head) (cdr sector-head) dir2)
			    (neighbour-tile-coords (car column-head) (cdr column-head) dir2))
	       (column-index 1 (1+ column-index)))
	      ((or (= column-index max-range) (null column-head)))
	    (do ((current column-head
			  (neighbour-tile-coords (car current) (cdr current) dir1))
		 (count 1 (1+ count)))
		((or (> (+ column-index count) max-range) (null current)))
	      (setf (gethash current visibles)
		    (funcall vis-cost-func current
			     (neighbour-tile-coords (car current) (cdr current) odir2)
			     column-index
			     (neighbour-tile-coords (car current) (cdr current) odir1)
			     count
			     visibles)
		    ))))
	)
      )
    visibles))


(defun move-area (army &optional (world *world*))
  (let ((start-x (army-x army))
	(start-y (army-y army))
	(slow-moves (slowest-movecosts (army-troops army)))
	(move-range (army-movement army)))

    (setf *current-move-area*  ;; aww shit
	  (breadth-first-fill
	   start-x start-y :range move-range
	   :costfunc #'(lambda (x y dir world)
			 (let* ((roads (coord-border-roads x y dir world))
				(river (coord-border-rivers x y dir world))
				(terrain (coord-types x y world))
				(locations (coord-locations x y world)))

			   (cond ((enemy-army-at (army-owner army) x y) ; This makes a tile occupied by
				  most-positive-fixnum)                 ;  an enemy impossible to enter
				 (roads
				  (apply #'max
					 (mapcar
					  #'(lambda (movetype) ; for all movetypes in army
					      (apply #'min ; choose smallest
						     (mapcar
						      #'cadr ; from the costs
						      (intersection ; out of list of road-costs for current movetype
						       ;; This works ONLY IF result is picked from LIST1 argument
						       (gethash movetype *unit-type-road-movecosts*)
						       roads
						       :test #'(lambda (road-cost road)
								 (eq road (car road-cost)))))))
					  (troops-to-movetypes (army-troops army)))))
				 
				 (locations
				  (+ (if river
					 (cadr (assoc river slow-moves))
					 0)
				     (apply #'max (mapcar #'(lambda (location)
							      (cadr (assoc location slow-moves)))
							  (mapcar #'car locations)))))
				 
				 (t
				  (+ (if river
					 (cadr (assoc river slow-moves))
					 0)
				     (apply #'max (mapcar #'(lambda (terrain-type)
							      (cadr (assoc terrain-type slow-moves)))
							  terrain)))))))))))

;; These would probably work better as macros so they could be set.
(defun coord-types (x y &optional (world *world*))
  "Returns tile-type field f tile at X Y."
  (tile-type (tile-at x y world)))

(defun coord-border-rivers (x y dir &optional (world *world*))
  "Returns symbol representing type of river at DIR border of tile at X Y."
  (cdr (assoc dir (tile-river-borders (tile-at x y world)))))

(defun coord-border-roads (x y dir &optional (world *world*))
  "Returns list of roadtype symbols traversing border DIR of tile at X Y."
  (cdr (assoc dir (tile-road-links (tile-at x y world)))))

(defun coord-locations (x y &optional (world *world*))
  "Returns list of locations for tile at X Y."
  (tile-location (tile-at x y world)))

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

;; Maybe rename:
(defmacro defmovetypeunits (move-type &rest unit-names)
  "Associate each UNIT-NAME with MOVE-TYPE."
  `(progn
     ,@(mapcar
	#'(lambda (unit)
	    `(setf (gethash ',unit *unit-types*) ',move-type))
       unit-names)))


(defun test-slowest-movecosts ()
  ;; This is dumb dumb dumb dummy data only for testing
  ;;(setf *road-types* '(rail road)) ; Too late to set here
  (defmovetypeunits infantry commando ranger light-machinegun)
  (defmovetypeunits cavalry dragoon haccapelite knight)
  (defmovetypeunits wheeled jeep truck formula1)
  (defmovetypeunits towed flak88 8-pounder)
  (defmovetypeunits rail pendolino steam-locomotive)
  (defmovecosts infantry (grass 3) (hill 5) (mountain 10) (forest 4) (sea 10000) (city 3) (stream 3) (rail 3) (road 3))
  (defmovecosts cavalry (grass 2) (hill 5) (mountain 12) (forest 5) (sea 10000) (city 3) (stream 3) (rail 2) (road 2))
  (defmovecosts wheeled (grass 2) (hill 5) (mountain 20) (forest 10)(sea 10000) (city 2)(stream 10) (rail 3) (road 1))
  (defmovecosts towed (grass 4) (hill 10) (mountain 14) (forest 7)(sea 10000) (city 3)(stream 10) (rail 5) (road 3))
  (defmovecosts rail (grass 4) (hill 5) (mountain 20) (forest 10)(sea 10000) (city 2)(stream 10) (rail 1) (road 5))

  ;;this is a test and may be removed:
  (slowest-movecosts '((commando . 10) (dragoon . 50) (jeep . 10) (flak88 . 10)))
  )

(defun test-movecosts ()
  (test-slowest-movecosts) ; setup dummy movetype data
  ;; can't set *testunit*s here since sdl needs to be initalized -> do it manually
  (when (>= (length *testunit*) 2)
    (setf (army-troops (car *testunit*)) '((jeep . 10) (pendolino . 2)))
    (setf (army-troops (cadr *testunit*)) '((dragoon . 20) (commando . 20)))
    ))

(defun dumb-tiles-within (range)
  "How many hexes within RANGE from a hex."
  (+ 1 ; Center
     (* 6 ; Sectors
	(/ (* range (+ range 1)) ; triangular number formula
	   2))))

(defun troops-to-movetypes (unit-list)
  "Convert list of units to their movement-types."
  (let ((move-types))
    (dolist (unit unit-list)
      (pushnew (gethash (car unit) *unit-types*) move-types))
    move-types))

(defun slowest-movecosts (unit-list)
  "Returns list containing highest move costs on different tiles for units in unit-list.
In form: ( (tile-type move-cost ..rest-slowest-units..) ...)"
  (let ((unit-type-list (troops-to-movetypes unit-list))
	(slowest))
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

;;TODO: Should be combined with (hash-path) from worldgen.lisp
(defun path-move-table (target move-area)
  "Return list of (x . y) from TARGET to wherever MOVE-AREA starts."
  (cons target
	(let ((current (cdr (gethash target move-area))))
	  (when (car current)
	    (path-move-table current move-area)))))
