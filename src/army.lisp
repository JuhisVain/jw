(in-package :war)

;; Using a struct or array for *unit-type-(road-)movecosts* might be smarter,
;; but will have to be defined procedurally
;; I think these should be the same for all factions.
;; Factions' techs should affect only only move points as far as these are concerned.

(defvar *unit-types* (make-hash-table :test 'equal)) ; "dragoon" # cavalry
(defvar *unit-type-movecosts* (make-hash-table :test 'eq)) ; cavalry # ((grass 2) (hill 3) ...)

(defvar *road-types* '(rail road)) ; (road rail maglev teslavacuumtube footpath etc..)

(defun army-xy (army)
  "Returns the values from the x and y slots in ARMY as a single cons."
  (declare (army army))
  (the coordinates
       (cons (army-x army)
	     (army-y army))))

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


(defvar *turn-readiness-replenishment* 25)

(defun readiness-replenish-mod (readiness)
  "Returns multiplier to be used with unit's standard supply-use."
  ;; Currently grows linearly from 1 up to 2 at *turn-readiness-replenishment*
  (+ 1
     (/ (min (- 100 readiness)
	     *turn-readiness-replenishment*)
	*turn-readiness-replenishment*)))

(defun army-supply-use (army)
  "Returns amount of supply ARMY consumes in a round."
  (declare (army army))
  (apply #'+
	 (mapcar #'(lambda (troop)
		     (*
		      (faction-unit-supply-use (unit-stack-type troop))
		      (readiness-replenish-mod (unit-stack-readiness troop))
		      (unit-stack-count troop)))
		 (army-troops army))))

(defun army-supply-space (army)
  "Returns total amount of supplies ARMY can carry."
  (declare (army army))
  (apply #'+
	 (mapcar #'(lambda (troop)
		     (*
		      (faction-unit-supply-space (unit-stack-type troop))
		      (unit-stack-count troop)))
		 (army-troops army))))

(defun army-supply-request (army)
  "Returns how many units of supply to request from HQ."
  (* (/ (army-supply-req army)
	100)
     (- (army-supply-space army) ; Replenish army stocks
	(army-supplies army))))

(defun army-supply-stockpiles (army)
  "Returns ARMY's supply stockpile unit-stack, or nil if not found."
  ;;; TODO: Supply ought to be the same everywhere
  ;; -> define a constant faction unit
  (declare (army army))
  (the (or null unit-stack)
       (find-if
	#'(lambda (troop)
	    (string= "Supply"
		     (faction-unit-name
		      (unit-stack-type troop))))
	(army-troops army))))

(defun army-supply-stockpiles-count (army)
  "Returns amount of ARMY's supply stockpile"
  (declare (army army))
  (unit-stack-count
   (or (army-supply-stockpiles army)
       (return-from army-supply-stockpiles-count 0))))

(defun increase-supply-stockpiles (army amount)
  "Increases ARMY's supply stockpiles by AMOUNT and returns total."
  (declare (army army) ((integer 0 *) amount))
  (incf (unit-stack-count
	 (or (army-supply-stockpiles army)
	     (make-unit-stack :type (unit-type-by-name
				     "Supply" (army-owner army))
			      :count 0)))
	amount))

(defun decrease-supply-stockpiles (army amount)
  "Decreases ARMY's supply stockpiles by AMOUNT and returns total."
  (declare (army army) ((integer 0 *) amount))
  ;; Should puke out errors if result is < 0 or no stockpiles at all.
  (decf (unit-stack-count (army-supply-stockpiles army))
	amount))
  

(defun army-validate-supply (army)
  "Moves ARMY's excess supplies to a unit-stack and required supplies from
unit-stack to army-supplies."
  (let ((stockpile (army-supply-stockpiles army))
	(deficit (- (army-supply-space army)
		    (army-supplies army))))
    (cond ((and stockpile ; not enough supply carried
		(> deficit 0)
		(> (unit-stack-count stockpile)
		   deficit))
	   (decf (unit-stack-count stockpile) deficit)
	   (incf (army-supplies army) deficit))
	  ((and stockpile ; not enough supply carried & not enough stockpile
		(> deficit 0)
		(<= (unit-stack-count stockpile)
		    deficit))
	   (incf (army-supplies army) (unit-stack-count stockpile))
	   (setf (army-troops army)
		 (delete stockpile (army-troops army) :test #'eq)))
	  ((and stockpile ; Too much supply carried
		(< deficit 0))
	   (incf (army-supplies army) deficit)
	   (decf (unit-stack-count stockpile) deficit))
	  ((and (null stockpile) ; Too much supply carried
		(< deficit 0))
	   (incf (army-supplies army) deficit)
	   (push (make-unit-stack :type (unit-type-by-name
					 "Supply" (army-owner army))
				  :count (- deficit))
		 (army-troops army))))))

(defun step-unit-to (army x y &optional (world *world*))
  "Places ARMY to coordinates (X,Y), which must be ARMY's current location's neighbour."
  (let ((direction (neighbourp (cons x y)
			       (cons (army-x army) (army-y army)))))
    (when direction
      (dolist (stack (army-troops army))
	;; The pace of the slowest unit is the pace of everything:
	(let ((cost (step-cost army x y direction world)))

	  ;; Currently unit's first move may produce costs greater than actionpoints
	  (when (< (- (unit-stack-action-points stack)
		      cost)
		   0)
	    (setf cost (unit-stack-action-points stack)))
	  
	  (decf (unit-stack-action-points stack) cost)
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
		    (faction (army-owner army))
		    (movecarries (troop-movecarry-data (army-troops army)))
		    (slow-moves (slowest-movecosts movecarries)))
  "Returns the cost of stepping ARMY ***from*** DIR ***to*** (X,Y).
If MOVECARRIES set to NIL, ARMY's actual movetypes are ignored and ARMY will
only be used to determine owning faction. SLOW-MOVES then must be supplied.
If ARMY set to nil FACTION must be supplied.

SLOW-MOVES should be an alist of (tiletype . (cost))
most likely source is (gethash movement-type *unit-type-movecosts*)."
  (let* ((roads (coord-border-roads x y dir world))
	 (river (coord-border-rivers x y dir world))
	 (terrain (coord-types x y world))
	 (locations (coord-locations x y world))
	 )

    (cond
      ;; 1st check: If there's an unknown enemy at (x y) make tile appear movable
      ;; if it's known make tile unmoveable
      ;; This seems like a human interface thing, but the AI should have the same disadvantage in theory...
      ((let ((enemy (enemy-army-at faction x y))) 
	 (when enemy                                        
	   (let ((known (gethash enemy (faction-enemy-unit-info faction))))
	     (when known
	       (unit-info-has-been-seen known)))))
       most-positive-fixnum)
      ;; No known enemies: do the cost computing
      (roads ;; Road movement ignores terrain (even mountains??)
       (apply #'max (mapcar #'(lambda (road-type)
				(cadr (assoc road-type slow-moves)))
			    roads)))
      (locations
       (+ (if river
	      (cadr (assoc river slow-moves))
	      0)
	  (apply #'max (mapcar #'(lambda (location)
				   (cadr (assoc location slow-moves)))
			       (mapcar #'type-of locations)))))
      
      (t ;; "Normal movement"
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
	 (movetypes (troop-movecarry-data (army-troops army)))
	 (slow-moves (slowest-movecosts movetypes))
	 (move-range (army-action-points army)))

    (breadth-first-fill
     start-x start-y :range move-range
     :costfunc #'(lambda (x y dir world)
		   (step-cost army x y dir world (army-owner army) movetypes slow-moves)))))

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
  "Defines terrain specific entry costs for MOVEMENT-TYPE."
  `(setf (gethash ',movement-type *unit-type-movecosts*)
	 ',(mapcar #'(lambda (type-cost)
		       (if (symbolp (cadr type-cost))
			   (list (car type-cost) (symbol-value (cadr type-cost)))
			   type-cost))
		   terrain-costs)))

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
  (defmovetypeunits wheeled "Jeep" "Truck" "formula1")
  (defmovetypeunits towed "8.8cm Flak" "8-pounder")
  (defmovetypeunits rail "Pendolino" "steam-locomotive")
  (defmovetypeunits moveable "Supply")
  (defmovecosts infantry (grass 40) (hill 50) (mountain 80) (nforest 50)
		(sea 10000) (city 20) (stream 20) (river 50) (rail 20) (road 20))
  (defmovecosts cavalry (grass 30) (hill 40) (mountain 80) (nforest 50)
		(sea 10000) (city 20) (stream 20) (river 50) (rail 15) (road 15))
  (defmovecosts wheeled (grass 25) (hill 30) (mountain 100) (nforest 70)
		(sea 10000) (city 10) (stream 70) (river 90) (rail 10) (road 10))
  (defmovecosts towed (grass 60) (hill 80) (mountain 100) (nforest 70)
		(sea 10000) (city 30) (stream 70) (river 90) (rail 30) (road 30))
  (defmovecosts rail (grass +inf+) (hill +inf+) (mountain +inf+) (nforest +inf+)
		(sea +inf+) (city +inf+) (stream +inf+) (river +inf+) (rail 10) (road +inf+))
  (defmovecosts moveable (grass +inf+) (hill +inf+) (mountain +inf+) (nforest +inf+)
		(sea +inf+) (city +inf+) (stream +inf+) (river +inf+) (rail +inf+) (road +inf+))
  ;;; Should be possible to set priorityqueue default size based on lowest movecost of defmovecosts

  (mapcar
   #'(lambda (faction)
       (setf (faction-unit-types faction)
	     (list
	      (make-faction-unit :movement 'infantry :name "Commando")
	      (make-faction-unit :movement 'cavalry :name "Dragoon")
	      (make-faction-unit :movement 'wheeled :name "Jeep"
				 :carry-space 2 :size 10)
	      (make-faction-unit :movement 'wheeled :name "Truck"
				 :carry-space 20 :size 25)
	      (make-faction-unit :movement 'towed :name "8.8cm Flak")
	      (make-faction-unit :movement 'rail :name "Pendolino")
	      (make-faction-unit :movement 'moveable :name "Supply"
				 :supply-use 0 :supply-space 0)))) ; etc..
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
		(make-unit-stack :type (unit-type-by-name "Commando" (army-owner (car *testunit*)))
				 :count 10)))
    (setf (army-troops (cadr *testunit*))
	  (list (make-unit-stack :type (unit-type-by-name "Dragoon" (army-owner (car *testunit*)))
				 :count 20)
		(make-unit-stack :type (unit-type-by-name "8.8cm Flak" (army-owner (car *testunit*)))
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

(defstruct movecarry
  (movetype nil :type symbol)
  (totalsize nil :type (integer 0 *))
  (carryspace nil :type (integer 0 *)))

(defun troop-movecarry-data (unit-list)
  "Returns list of compiled movecarry structures, to be used in determining automatic
carrying of slower units during movement.
Duplicate movement types are compiled into one structure"
  (let ((data nil))
    (loop for unit in unit-list
	  do (let* ((unit-count (unit-stack-count unit))
		(unit-type (unit-stack-type unit))
		(move-type (faction-unit-movement unit-type))
		(dataset (find-if
			  #'(lambda (mc)
			      (eq (movecarry-movetype mc)
				  move-type))
			  data)))
	   (if dataset
	       (progn (incf (movecarry-totalsize dataset)
			    (* (faction-unit-size unit-type)
			       unit-count))
		      (incf (movecarry-carryspace dataset)
			    (* (faction-unit-carry-space unit-type)
			       unit-count)))
	       (push (make-movecarry :movetype move-type
				     :totalsize (* (faction-unit-size unit-type)
						   unit-count)
				     :carryspace (* (faction-unit-carry-space unit-type)
						    unit-count))
		     data)
	       )))
    data))
    

(defun slowest-movecosts (movecarry-list)
  "Returns list containing highest move costs on different tiles for unit-types in unit-type-list.
In form: ( (tile-type move-cost ..rest-slowest-units..) ...)"
  (let ((unit-type-list (mapcar #'movecarry-movetype movecarry-list))
	(slowest))
    (dolist (type-costs
	      (mapcar #'(lambda (unit-type) ; ( (unit-type ((tile-type move-cost) ...)) ...)
			  (cons unit-type
				(gethash unit-type *unit-type-movecosts*)))
		      unit-type-list))
      (let ((unit-type (car type-costs)) ; cavalry
	    (unit-road-costs))

	(dolist (costs (cdr type-costs))
	  (let* ((typesym (car costs))
		 (typecons (assoc typesym slowest))
		 (cost (cadr costs)))
	    (if typecons
	        (let ((same-cost (assoc cost (cdr typecons))))
		  (if same-cost
		      (rplacd same-cost (cons unit-type (cdr same-cost)))
		      (rplacd typecons
			      (merge 'list (list (list cost unit-type)) (cdr typecons)
				     #'> :key #'car))))
		(push (list typesym
			    (list cost unit-type))
		      slowest))))
	
	;; SLOWEST now holds ( (GRASS (60 TOWED) (40 INFANTRY) (25 WHEELED))
	;;                     (MOUNTAIN (100 WHEELED) (80 INFANTRY CAVALRY)) ...)

	))

    (labels
	((populate-carryspace (totals faster)
	   ;; Totals is now the total sizes of all previous units
	   (let* ((current-movetypes (cdar faster))
		  (current-movecarries
		   (mapcar #'(lambda (type)
			       (find type movecarry-list
				     :key #'movecarry-movetype))
			   current-movetypes)))

	     (when (cdr faster) ; If there are faster units
	       (let ((total-size
		      (apply
		       #'+
		       (mapcar #'movecarry-totalsize current-movecarries))))
	       (setf totals
		     (populate-carryspace
		      (+ totals ; totals larger by amount of unitsizes in current
			 total-size)
		      (cdr faster)))
	       ;; this types totals sizes are in totals -> take them them out
	       (when (not (listp totals))
		 (decf totals total-size))
	       ))
	     
	     ;; A list instead of a number is the movetype whos carryspace is exhausted
	     (when (listp totals)
	       (return-from populate-carryspace totals))
	     
	     (decf totals
		   (apply #'+
			  (mapcar #'movecarry-carryspace current-movecarries))))
	   (if (<= totals 0)
	       (car faster)
	       totals))
	 
	 )
      
      (mapcar #'(lambda (typecons)
		  (cons (car typecons)
			(or
			 (populate-carryspace 0 (cdr typecons))
			 (cadr typecons))))
	      slowest))))

;;TODO: Should be combined with (hash-path) from worldgen.lisp
(defun path-move-table (target move-area)
  "Return list of (x . y) from TARGET to wherever MOVE-AREA starts."
  (cons target
	(let ((current (cdr (gethash target move-area))))
	  (when (car current)
	    (path-move-table current move-area)))))
