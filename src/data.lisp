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
  (rail-links nil)
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

(defun breadth-first-fill (x0 y0 &key (range most-positive-fixnum) (world *world*) costfunc)
  "Costfunc takes x y direction world arguments and returns a number"
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
	      (neighbour-entry
		(mapcar #'(lambda (dir)
			    (list
			     (neighbour-tile-coords
			      current-x current-y dir
			      (world-width world) (world-height world))
			     (oppdir dir))) ; This is the direction of entry to tile
			+std-short-dirs+))

	    (let ((neighbour (car neighbour-entry))
		  (entry (cadr neighbour-entry)))

	      ;; TODO: fix this spaghetti
	      (if (null neighbour)
		  nil
		  (let ((move-cost
			 (- range-left
			    (funcall costfunc
				     (car neighbour) (cdr neighbour)
				     entry world))))
		    
		    (if (or (null (gethash neighbour came-from))
			    (>= move-cost
			       (car (gethash neighbour came-from)))) ; move left when moving to neigh
			       
			
			(when (>= move-cost 0)
			  (heap-insert frontier neighbour move-cost)
			  (setf (gethash neighbour came-from)
				(cons move-cost (cdr current))))))
		  ))))))

    came-from))
  
