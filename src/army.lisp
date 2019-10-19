(in-package :war)

;; Using a struct or array for *unit-type-(road-)movecosts* might be smarter,
;; but will have to be defined procedurally
;; I think these should be the same for all factions.
;; Factions' techs should affect only only move points as far as these are concerned.

(defvar *unit-types* (make-hash-table :test 'equal)) ; "dragoon" # cavalry
(defvar *unit-type-movecosts* (make-hash-table :test 'eq)) ; cavalry # ((grass 2) (hill 3) ...)
(defvar *unit-type-road-movecosts* (make-hash-table :test 'eq))

(defvar *road-types* '(rail road)) ; (road rail maglev teslavacuumtube footpath etc..)

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

(defun army-action-points (army)
  "Returns current action points of ARMY."
  (reduce #'min (army-troops army) :key #'unit-stack-action-points))

(defun army-attack (army target &key (advance nil))
  "ARMY attacks TARGET, which must be in a tile adjacent to ARMY's.
If ADVANCE is true ARMY will move to TARGET's position, if possible."
  (declare (army army target))

  (unless (neighbourp (cons (army-x army)
			    (army-y army))
		      (cons (army-x target)
			    (army-y target)))
    (format t "~&ERROR: Army at ~a,~a tried to attack army at ~a,~a~%"
	    (army-x army) (army-y army) (army-x target) (army-y target))
    (return-from army-attack nil))
  
  (let ((winner (if (>= (reduce #'+ (army-troops army) :key #'unit-stack-count)
			(reduce #'+ (army-troops target) :key #'unit-stack-count))
		    army target)))

    (if (eq winner army)
	(progn
	  (destroy-army target)
	  (unless (enemy-army-at (army-owner army) (army-x target) (army-y target))
	    (place-unit army (army-x target) (army-y target))))
	(destroy-army army))
    
    (datalog (army-owner army) 'attack
	     (list (list army ; It will be smarter to just store troops involved in the final log
			 (army-x army)
			 (army-y army))
		   (list target
			 (army-x target)
			 (army-y target))
		   winner))))

(defun step-unit-to (army x y &optional (world *world*))
  "Places ARMY to coordinates (X,Y), which must be ARMY's current location's neighbour."
  (let ((direction (neighbourp (cons x y)
			       (cons (army-x army) (army-y army)))))
    (when direction
      (dolist (stack (army-troops army))
	;; step-cost was not intended to be used this way
	;; only reduce specific unit-type's movecost from stack of its type
	(let ((cost (step-cost army x y direction world 
			       (list (faction-unit-movement (unit-stack-type stack))))))
	  (decf (unit-stack-action-points stack) cost)
	  ;; There was a check here if AP is now less than zero but AP type is (MOD 101)
	  ;; Maybe that will take care of error checking
	  ))
      (place-unit army x y world))))

(defun destroy-army (army)
  "Removes ARMY from map."
  (let ((tile (tile-at (army-x army) (army-y army))))
    (setf (tile-units tile)
	  (delete army (tile-units tile))
	  (faction-armies (army-owner army))
	  (delete army (faction-armies (army-owner army)))
	  )
    ;;; TODO:
    ;; increase straglers
    ;; do logging
    ;; etc..
    ))


(defun step-cost (army x y dir world
		  &optional
		    (movetypes (troops-to-movetypes (army-troops army)))
		    (slow-moves (slowest-movecosts movetypes)))
  "Returns the cost of stepping ARMY ***from*** DIR ***to*** (X,Y)."
  (let* ((roads (coord-border-roads x y dir world))
	 (river (coord-border-rivers x y dir world))
	 (terrain (coord-types x y world))
	 (locations (coord-locations x y world)))

    (cond
      ;; 1st check: If there's an unknown enemy at (x y) make tile appear movable
      ;; if it's known make tile unmoveable
      ;; This seems like a human interface thing, but the AI should have the same disadvantage in theory...
      ((let ((enemy (enemy-army-at (army-owner army) x y))) 
	 (when enemy                                        
	   (let ((known (gethash enemy (faction-enemy-unit-info (army-owner army)))))
	     (when known
	       (unit-info-has-been-seen known)))))
       most-positive-fixnum)
      ;; No known enemies: do the cost computing
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
	       movetypes)))
      
      (locations
       (+ (if river
	      (cadr (assoc river slow-moves))
	      0)
	  (apply #'max (mapcar #'(lambda (location)
				   (cadr (assoc location slow-moves)))
			       (mapcar #'type-of locations)))))
      
      (t
       (+ (if river
	      (cadr (assoc river slow-moves))
	      0)
	  (apply #'max (mapcar #'(lambda (terrain-type)
				   (cadr (assoc terrain-type slow-moves)))
			       terrain)))))))


(defun move-area (army &optional (world *world*))
  "Sets *current-move-area* to hold hashtable of tiles in range of ARMY."
  (let* ((start-x (army-x army))
	 (start-y (army-y army))
	 (movetypes (troops-to-movetypes (army-troops army)))
	 (slow-moves (slowest-movecosts movetypes))
	 (move-range (army-action-points army)))

    (breadth-first-fill
     start-x start-y :range move-range
     :costfunc #'(lambda (x y dir world)
		   (step-cost army x y dir world movetypes slow-moves)))))

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

;;; NOTE: A movecost value of any less than 10 is risky while heap size is 6139
;; if movecosts need to be lower might need to investigate rewriting heaps with lists
;; or some other algorithm altogether
(defun test-slowest-movecosts ()
  ;; This is dumb dumb dumb dummy data only for testing
  ;;(setf *road-types* '(rail road)) ; Too late to set here
  (defmovetypeunits infantry "Commando" "ranger" "light-machinegun")
  (defmovetypeunits cavalry "Dragoon" "haccapelite" "knight")
  (defmovetypeunits wheeled "Jeep" "truck" "formula1")
  (defmovetypeunits towed "8.8cm Flak" "8-pounder")
  (defmovetypeunits rail "Pendolino" "steam-locomotive")
  (defmovecosts infantry (grass 10) (hill 25) (mountain 90) (nforest 15)
		(sea 10000) (city 7) (stream 20) (river 50) (rail 5) (road 5))
  (defmovecosts cavalry (grass 5) (hill 15) (mountain 90) (nforest 15)
		(sea 10000) (city 7) (stream 20) (river 50) (rail 4) (road 4))
  (defmovecosts wheeled (grass 5) (hill 30) (mountain 100) (nforest 50)
		(sea 10000) (city 5)(stream 50) (river 75) (rail 3) (road 3))
  (defmovecosts towed (grass 15) (hill 30) (mountain 90) (nforest 20)
		(sea 10000) (city 10)(stream 50) (river 75) (rail 7) (road 7))
  (defmovecosts rail (grass 1000) (hill 1000) (mountain 1000) (nforest 1000)
		(sea 10000) (city 1000)(stream 1000) (river 1000) (rail 2) (road 1000))

  (mapcar
   #'(lambda (faction)
       (setf (faction-unit-types faction)
	     (list
	      (make-faction-unit :movement 'infantry :name "Commando")
	      (make-faction-unit :movement 'cavalry :name "Dragoon")
	      (make-faction-unit :movement 'wheeled :name "Jeep")
	      (make-faction-unit :movement 'towed :name "8.8cm Flak")
	      (make-faction-unit :movement 'rail :name "Pendolino")))) ; etc..
   (world-factions *world*))
  
  nil ; No need to print previous
  )

(defun test-movecosts ()
  (test-slowest-movecosts) ; setup dummy movetype data
  ;; can't set *testunit*s here since sdl needs to be initalized -> do it manually
  (when (>= (length *testunit*) 2)
    (setf (army-troops (car *testunit*))
	  (list (make-unit-stack :type (unit-type-by-name "Jeep" (army-owner (car *testunit*)))
				 :count 10)
		(make-unit-stack :type (unit-type-by-name "8.8cm Flak" (army-owner (car *testunit*)))
				 :count 1)))
    (setf (army-troops (cadr *testunit*))
	  (list (make-unit-stack :type (unit-type-by-name "Dragoon" (army-owner (car *testunit*)))
				 :count 20)
		(make-unit-stack :type (unit-type-by-name "Commando" (army-owner (car *testunit*)))
				 :count 20)))
    )

  ;; Could just iterate over armies for every faction and set random unit-stacks
  
  )

(defun unit-type-by-name (name faction)
  "Returns basic unit-type named NAME from FACTION's unit-types"
  (find-if #'(lambda (unit-type)
	       (string= (faction-unit-name unit-type) name))
	   (faction-unit-types faction)))

(defun dumb-tiles-within (range)
  "How many hexes within RANGE from a hex."
  (+ 1 ; Center
     (* 6 ; Sectors
	(/ (* range (+ range 1)) ; triangular number formula
	   2))))

(defun troops-to-movetypes (unit-list)
  "Convert list of unit-stacks to their movement-types."
  (let ((move-types))
    (dolist (unit unit-list)
      (pushnew (faction-unit-movement (unit-stack-type unit)) move-types))
    move-types))

(defun slowest-movecosts (unit-type-list)
  "Returns list containing highest move costs on different tiles for unit-types in unit-type-list.
In form: ( (tile-type move-cost ..rest-slowest-units..) ...)"
  (let ((slowest))
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
