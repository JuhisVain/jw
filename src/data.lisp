(in-package :war)

(defmacro count+list (&body list)
  ;; Push list's length to beginning of list
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
			   "Sainte-Geneviève-des-Bois"))))

(defstruct city
  (name)
  (owner)
  (x) (y)
  (production))

(defstruct location
  (name)
  (type)
  (owner)
  (x) (y)
  (production))

(defstruct tile
  (type (list 'sea)) ;things within tile that affect unit movement
  (variant nil)  ;graphical data
  (location nil) ;city/resource/airfield etc.. -> things of importance
  (river-borders nil)
  (road-links nil)
  (units nil))

(defstruct army
  (id)
  (x) (y)
  (troops)
  (movement)
  (counter))

(defun place-unit (unit x y)
  (setf (tile-units (aref (world-map *world*) (army-x unit) (army-y unit)))
	(delete unit
		(tile-units (aref (world-map *world*) (army-x unit) (army-y unit)))
		:test #'eq))
  (setf (army-x unit) x)
  (setf (army-y unit) y)
  (pushnew unit (tile-units (aref (world-map *world*) x y))))

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
		    (t (let ((move-cost (- range-left
					   (funcall costfunc
						    (car neighbour) (cdr neighbour)
						    entry world))))
			 (when
			     (and
			      (or (null (gethash neighbour came-from)) ; if this neighbouring tile is not already in came-from
				  (>= move-cost ; OR if this neighbour's move cost is better than the one's in came-from
				      (car (gethash neighbour came-from))))
			      (>= move-cost 0)) ; AND we actually have range left for the move

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

(defun test-a* ()
  ;; TODO: with current values moves to hill from (1 12) instead of smarter grass tile???
  (defparameter a*test
    (a* 1 12 11 8
	:costfunc
	#'(lambda (xy0 xy1)
	    (let ((xy1-terrain (tile-type (tile-at (car xy1) (cdr xy1)))))
	      (cond ((member 'sea xy1-terrain) 1000000)
		    ((member 'mountain xy1-terrain) 3)
		    ((member 'hill xy1-terrain) 2)
		    (t 1))))
	:heuristic
	#'(lambda (current end)
	    (distance (car current) (cdr current)
		      (car end) (cdr end))))))


(defun a* (x0 y0 x1 y1 &key (max-range 1000000) (world *world*) costfunc endfunc heuristic)
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
  
