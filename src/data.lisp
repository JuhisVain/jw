(in-package :war)

(defmacro count+list (&body list)
  "Push LIST's length to beginning of LIST."
  `'(,(length list) ,@list))

(defun conc-syms (&rest to-concs)
  "Produces symbol concatenation of stringified to-concs."
  (intern
   (string-upcase
    (apply #'concatenate
	   (cons 'string (mapcar #'princ-to-string to-concs))))))

(defun chance (percent)
  (declare (number percent))
  (if (< (random 100) percent) t))

(defstruct (log-round
	     (:print-object log-round-printer))
  (index nil :type integer)
  (turns nil))

(defstruct (log-turn
	     (:print-object log-turn-printer))
  (faction nil :type faction) ; Whose turn this is
  (events nil))

(defstruct (log-event
	     (:print-object log-event-printer))
  (index nil :type integer)
  (source nil :type faction)
  (data-type nil :type symbol)
  (data))

;; Just for testing, will break if given indexes too high
(defun print-log-event (round turn event)
  "Prints event number EVENT of turn number NUMBER of round number ROUND,
with data field in full."
  (let ((event
	 (nth event (log-turn-events
		     (nth turn (log-round-turns
				(nth round (world-log *world*))))))))
    (format t "~&~a~%~a~%" event (log-event-data event))))

(defun log-round-printer (this stream)
  (declare (log-round this))
  (format stream "ROUND ~a~%~{~a~}"
	  (log-round-index this)
	  (log-round-turns this)))

(defun log-turn-printer (this stream)
  (declare (log-turn this))
  (format stream "~2tTURN ~a :~%~{~4t~a~%~}"
	  (faction-name (log-turn-faction this))
	  (log-turn-events this)))

(defun log-event-printer (this stream)
  (declare (log-event this))
  (format stream "~a, ~a, ~a :: ~a"
	  (log-event-index this)
	  (faction-name (log-event-source this))
	  (log-event-data-type this)
	  (not (null (log-event-data this)))))


(defstruct world
  (width nil)     ;amount of columns
  (height nil)    ;hexes in a column
  ;;If wrapping required on x-axis, column count must be even 
  (map nil)       ;a 2d array
  (factions nil)  ;list of faction structs
  (cities)
  (locations)     ;production, victory locations, etc..
  (theme (list :city-names ; Random city names
	       (count+list "Paris" "Lyon" "Montpellier" "Toulouse"
			   "Marseille" "Nantes" "Nice" "Bordeaux"
			   "Le Havre" "Brest" "Caen"
			   "Sainte-Geneviève-des-Bois")))
  (current-round 0)
  (current-turn nil) ; a faction
  (log (list (make-log-round :index 0))) ;list of log-rounds
  )

(defstruct location
  (name)
  (owner)
  (x) (y)
  (production))

(defstruct (city (:include location)))

(defstruct tile
  (owner nil :type (or faction null))
  (type (list 'sea)) ;things within tile that affect unit movement
  (variant nil)  ;graphical data
  (location nil) ;city/resource/airfield etc.. -> things of importance
  (river-borders nil)
  (road-links nil)
  (units nil))

(defparameter *army-id-counter* 0) ; Would rather close on lexical var, but unclear if defstruct portable
(defstruct army
  (id (incf *army-id-counter*) :type (integer 0 *) :read-only t)
  (owner nil :type faction) ; owning faction
  (coc nil :type oob-element) ; position in chain of command
  (x 0 :type (integer 0 *)) ; might want to have some kind of out of play value?
  (y 0 :type (integer 0 *))
  (troops)
  (supplies 0 :type fixnum)
  (supply-req 100 :type (integer 0 *)) ; percentage, how much to request supply
  (counter))

;; (create-nest-readers troop
;;                      (defstruct unit-stack ...)
;;                      movement
;;                      (defstruct faction-unit ...))
;; -> (progn ...
;;      (defun troop-movement (troop)
;;        (declare (unit-stack troop))
;;        (faction-unit-movement (unit-stack-type troop))) ..)
(defmacro create-nested-readers (reader-name nest-slot nest-struct-def mother-struct-def)
  "Generates reader functions for NEST-STRUCT's slots using MOTHER-STRUCT instances as arguments,
in form: (READERNAME-SLOTNAME MOTHERSTRUCTNAME)."
  (declare (symbol reader-name nest-slot)
	   (list mother-struct-def nest-struct-def))
  (let* ((mother-name (if (listp (cadr mother-struct-def))
			  (caadr mother-struct-def)
			  (cadr mother-struct-def)))
	 (nest-name (if (listp (cadr nest-struct-def))
			(caadr nest-struct-def)
			(cadr nest-struct-def)))
	 (nest-accessor `(,(conc-syms mother-name '- nest-slot) troop))) ;; might want to rename "troop"
    `(progn ,mother-struct-def
	    ,nest-struct-def
	    ,@(mapcar #'(lambda (slot)
			  (let ((slot-name (if (listp slot)
					       (car slot)
					       slot)))
			    `(defun ,(conc-syms reader-name '- slot-name)
				 (troop)
			       (declare (,mother-name troop))
			       (,(conc-syms nest-name '- slot-name)
				 ,nest-accessor))))
		      (cddr nest-struct-def)))))

(create-nested-readers
 troop type

;;; This should be used in a list in faction struct to be used
 ;; for the basic values of a type of unit.
 ;; The values in this thing should only be changed by faction teching up ??maybe??
 ;; Real units on map should then have experience, morale, readiness, cohesion etc..
 ;; That will be taken into account when checking these during play.
 (defstruct faction-unit
   (movement nil :type symbol)
   (name nil :type string)
   (vision) ; TODO
   (carry-space 0 :type fixnum) ; How many sizes worth of units this thing can carry
   (size 1 :type fixnum)
   (supply-use 2 :type fixnum) ; Supplies used in a turn based on readiness ???
   (supply-space 4 :type fixnum) ; How many supplies this thing can carry
   (fuel-use 0 :type fixnum) ; Fuel used per 100 move-points
   (combat-values) ; TODO: a structure containing data on fighting on terrain against what?? 
   )

 (defstruct unit-stack
   (type nil :type faction-unit)
   (count 0 :type fixnum)
   (action-points 100 :type (integer 0 100))
   (experience 0 :type fixnum)
   (morale 0 :type fixnum)
   (readiness 0 :type (integer 0 100)))

)

(deftype coordinates ()
  '(cons fixnum fixnum))

(deftype direction ()
  '(member n nw sw s se ne))

(deftype border ()
  '(cons coordinates (cons direction null)))

;; LOG is the logarithm function
;; I think datalog is a programming language but whatcha gonna do
;; WIP
(defun datalog (faction type content &key (world *world*))

  (let* ((round
	  (progn
	    ;; If we're on a new turn, create new log-round
	    (when (> (world-current-round world)
		     (log-round-index (car (world-log world))))
	      (setf (world-log world)
		    (push (make-log-round :index (world-current-round world))
			  (world-log world))))
	    (car (world-log world))))
	 
	 ;; If currently playing faction is not the faction at head of this round's turn list
	 ;; -> means that the turn has changed
	 (turn
	  
	  (if (and
	       (log-round-turns round)
	       (eq (log-turn-faction (car (log-round-turns round)))
		   (world-current-turn world)))
	      (car (log-round-turns round))
	      (progn
		(setf (log-round-turns round)
		      (push (make-log-turn :faction (world-current-turn world))
			    (log-round-turns round)))
		(car (log-round-turns round))
		)))
	 

	 (event
	  (make-log-event :index (let ((previous
					(car (log-turn-events turn))))
				   (if previous
				       (1+ (log-event-index previous))
				       0))
			  :source faction
			  :data-type type
			  :data content
			  
	 
	 )))

    (setf (log-turn-events turn) (push event (log-turn-events turn)))

    ))


(defun place-unit (unit x y &optional (world *world*))
  "Removes UNIT from it's current location if within world bounds
and places it at (X,Y) in WORLD if within bounds. Giving out of bounds
X Y coordinates places unit outside of WORLD."
  (when (coord-in-bounds (cons (army-x unit) (army-y unit)) world)
    (let ((old-tile (tile-at (army-x unit) (army-y unit) world)))
      (setf (tile-units old-tile)
	    (delete unit (tile-units old-tile) :test #'eq))))
  
  (setf (army-x unit) x)
  (setf (army-y unit) y)
  
  (when (coord-in-bounds (cons x y) world)
    (change-tile-owner x y (army-owner unit))
    (pushnew unit (tile-units (tile-at x y world)))))

;; A stronger army moving to a tile might also take control of neighbourhood?
(defun change-tile-owner (x y faction)
  (let* ((tile (tile-at x y))
	 (locations (tile-location tile)))
    (setf (tile-owner tile) faction)
    (dolist (loc locations)
      (setf (location-owner loc) faction))))

(defun coord-in-bounds (coord-pair &optional (world *world*))
  (and (<= 0 (car coord-pair) (world-width world))
       (<= 0 (cdr coord-pair) (world-height world))))

;; Using a symbol rather than an integer for default range would be smart -> also should be usable in costfunc
(defun breadth-first-fill (x0 y0 &key (range most-positive-fixnum) (world *world*) costfunc (endfunc nil end-p))
  "Flood fills RANGE area starting at x0 y0 in world according to COSTFUNC.
Costfunc takes x y direction world arguments and returns a number.
Optional endfunc takes x y world, if returns true this function returns before fill is complete."
  (let ((frontier (make-heap))
	(came-from (make-hash-table :test 'equal))
	(xy0 (cons x0 y0)))
    (heap-insert frontier xy0 range)
    (setf (gethash xy0 came-from) (list range nil))

    (do ()
	((heap-empty frontier))

      (let* ((current (heap-remove-max frontier)) ; -> (range x . y)
	     (range-left (car current))
	     (current-x (cadr current))
	     (current-y (cddr current)))
      
	(when (> range-left 0)
	  (dolist
	      (neighbour-entry ; ((x . y) dir)
		(mapcar #'(lambda (dir)
			    (list
			     (neighbour-tile-coords
			      current-x current-y dir
			      (world-width world) (world-height world))
			     (oppdir dir))) ; This is the direction of entry to tile
			+std-short-dirs+))

	    (let ((neighbour (car neighbour-entry)) ; (x . y)
		  (entry (cadr neighbour-entry))) ; dir
	      (cond ((null neighbour) nil)
		    (t (let ((move-cost (- range-left ; move-cost is actually the potential range left if we move
					   (funcall costfunc
						    (car neighbour) (cdr neighbour)
						    entry world))))
			 (when
			     (and
			      (or (null (gethash neighbour came-from)) ; if this neighbouring tile is not already in came-from
				  (>= move-cost ; OR if this neighbour's move cost is better than the one's in came-from
				      (car (gethash neighbour came-from))))
			      (or (>= move-cost 0) ; AND IF we actually have range left for the move
				  (and (= range-left 100) (>= move-cost -100))) ; OR IF this is the first move
			      ;; using range-left to determine first move is risky.
			      ;; using move-cost to determine moveable tile is dumb. It's there mainly to ignore water
			      ;; Currently UI will show negative movepoints left
			      )

			   (heap-insert frontier neighbour move-cost)
			   (setf (gethash neighbour came-from)
				 (cons move-cost (cdr current))))))))))
	(and end-p
	     (funcall endfunc current-x current-y world)
	     (return-from breadth-first-fill came-from))
	))
    came-from))

(defun distance (x1 y1 x2 y2)
  "Returns distance in tiles from (x1 y1) to (x2 y2)"
  (let ((c1 (cube-crd x1 y1))
	(c2 (cube-crd x2 y2)))
    (max (abs (- (car c1)
		 (car c2)))
	 (abs (- (cadr c1)
		 (cadr c2)))
	 (abs (- (caddr c1)
		 (caddr c2))))))

(defun cube-crd (x y)
  (let ((z (cube-z-crd x y)))
    (list x (- (- x) z) z)))

(defun cube-z-crd (x y)
  (- y (/ (- x (rem x 2))
	  2)))

'(defun test-a* ()
  ;; Not guaranteed to produce sensible results
  (defparameter a*test
    (a* 1 12 11 8
	:costfunc
	#'(lambda (xy0 xy1)
	    (let ((xy1-terrain (tile-type (tile-at (car xy1) (cdr xy1)))))
	      (cond ((member 'sea xy1-terrain) 1000000)
		    ((member 'mountain xy1-terrain) 5)
		    ((member 'hill xy1-terrain) 3)
		    (t 1))))
	:heuristic ;; Notice condition (t 1) above
	#'(lambda (current end)
	    (distance (car current) (cdr current)
		      (car end) (cdr end))))))


(defun a* (x0 y0 x1 y1 &key (max-range 1000000) (world *world*) costfunc endfunc heuristic)
  "COSTFUNC takes takes two coordinate conses and returns the cost of moving from the first
to the second.

HEURISTIC should return ??best case?? estimate of total cost of moving from any
coordinate cons to (X1, Y1)."
  (let ((xy0 (cons x0 y0))
	(xy1 (cons x1 y1))
	(frontier (make-heap))
	(came-from (make-hash-table :test 'equal))
	;;(cost-so-far (make-hash-table :test 'equal))
	)
    (heap-insert frontier (list (cons x0 y0) nil) max-range)
    ;;                   location^            ^direction taken to get here
    
    (setf (gethash xy0 came-from) (list 0 nil))
    
    (do ((current))
	((heap-empty frontier))
      (setf current (heap-remove-max frontier)) ; (prio (x . y) dir)
      (let ((xyc (cadr current))
	    (dirc (caddr current)))
	
	(when (equal xyc xy1)
	  (return-from a* came-from))
	
	(dolist (neighbour (mapcar #'(lambda (direction) ; ((xn . yn) dir)
				       (list
					(neighbour-tile-coords
					 (caadr current)
					 (cdadr current)
					 direction
					 (world-width world)
					 (world-height world))
					direction))
				   '(n ne se s sw nw)))
	  (when (car neighbour)
	    (let* ((xyn (car neighbour))
		   (dirn (cadr neighbour))
		   (new-cost (+ (car (gethash xyc came-from))
				(funcall costfunc xyc xyn))))
	      (when (and (not (gethash xyn came-from))
			 (<= new-cost max-range))
		(heap-insert frontier (list xyn dirn) (- max-range (+ new-cost
								      (funcall heuristic xyn xy1))))
		(setf (gethash xyn came-from) (list new-cost xyc dirn))
		))))))))
  
