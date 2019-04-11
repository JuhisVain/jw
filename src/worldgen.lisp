(in-package :war)

(defvar *graphics-variants* nil) ; ((name1 name1-a name1-b) (name2 name2-a name2-b name2-c)) etc..

(defun init-world (width height
		   &key
		     (algo 'random) ; Algorithm to use for map generation
		     (faction-count 2) ; 
		     (mirror nil) ; Should the world be mirrored?
					; TODO: maybe use symbolnames for mirrors for choice...
					; such as 'NW-SE or 'SW-NE.
					; Diag and horiz mirrors will need 1 tile wide padding(?)
		     (islands 1)) ; Amount of init points from which to start filling land tiles
  (finalize-world
   (cond ((eq algo 'perlin)
	  nil);(make-perlin-world width height faction-count mirror))
	 ((eq algo 'random)
	  (make-random-world width height))
	 ((eq algo 'testing)
	  (make-testing-world width height faction-count mirror islands))
	 )))

(defun make-testing-world (width height faction-count
			   &optional
			     (mirror nil)
			     (islands 1)
			     (island-size 10))
  (let* ((world (make-world :width (- width 1)
			    :height (- height 1)
			    :map (make-array (list width height))
			    :factions nil))
	 (grid-res 1) ; ???
	 (tw-width (ceiling width grid-res))
	 (tw-height (ceiling height grid-res))
	 (template-world (make-world :width tw-width
				     :height tw-height
				     :map (make-array (list tw-width tw-height))
				     :factions nil)))

    ;; Initialize world with sea tiles:
    (dotimes (x width)
      (dotimes (y height)
	;;(setf (aref (world-map world) x y) (make-tile))
	(setf (tile-at x y world) (make-tile))
	))
    
    ;; Create some random noise to use for breadth-first-fill:
    (dotimes (x tw-width)
      (dotimes (y tw-height)
	(setf (aref (world-map template-world) x y)
	      (make-tile :type (list (random 10))) ; This is the WEIGHT of filling the tile
	      )))

    ;; Select a random coordinate and fire breadth-first-fill at it:
    ;; TODO: select more coordinates for other types of tiles and more land and such
    (let ((fill-these-tiles
	   (breadth-first-fill ;(cons (random width) (random height))
	    (mapcar #'cons (list-randoms islands width)
		    (list-randoms islands height))
					;(+ 30 (random 20))
	    island-size
	    template-world)))

      (maphash #'(lambda (key value)
		   (setf (tile-type (aref (world-map world) (car key) (cdr key)))
			 (list 'grass)))
	       fill-these-tiles)

      (if mirror
	  ;; This mirrors NW / SE
	  (maphash #'(lambda (key value)
		       (setf (tile-type (aref (world-map world)
					      (- (world-width world) (car key))
					      (- (world-height world) (cdr key))))
			     (list 'grass)))
		   fill-these-tiles))
      
      )
    world))

(defun list-randoms (size rand-num)
  (if (< size 1) nil
      (cons (random rand-num) (list-randoms (1- size) rand-num))))

(defun breadth-first-fill (start-list range world)
  (let ((frontier (make-heap))
	(came-from (make-hash-table :test 'equal)))

    (dolist (start start-list)
      (heap-insert frontier start range)
      (setf (gethash start came-from) (list range nil))

      (do ((current))
	  ((heap-empty frontier))

	(setf current (heap-remove-max frontier))

	(if (> (car current) 0)
	    (dolist (neighbour (mapcar #'(lambda (direction)
					   (neighbour-tile-coords
					    (cadr current)
					    (cddr current)
					    direction world))
				       '(n ne se s sw nw)))
	      
	      (cond ((null neighbour) nil)
		    ((null (gethash neighbour came-from))
		     (let ((move-cost (- (car current)
					 (eval
					  (car (last
						(tile-type (aref (world-map world)
								 (car neighbour)
								 (cdr neighbour)))))))))
		       (cond ((>= move-cost 0)
			      (heap-insert frontier neighbour move-cost)
			      (setf (gethash neighbour came-from)
				    (cons move-cost (cdr current))))))))))
	
	))
    came-from))


(defun finalize-world (world)
  ;; Adds outskirt graphics
  (do ((x 0)
       (y 0))
      ((>= x (array-dimension (world-map world) 0)))

    ;(format t "~&~a,~a" x y)

    (finalize-tile x y world)

    ;; TODO: Whatever this was written to do it doesn't do.
    ;; the tile-variant graphics will need to be ordered according to some smart priority 
    (setf (tile-variant (aref (world-map world) x y))
	  (nreverse (tile-variant (aref (world-map world) x y))))
    
    (incf y)
    (if (>= y (array-dimension (world-map world) 1))
	(progn (incf x)
	       (setf y 0))))
  world)

(defun finalize-tile (x y &optional (world *world*))
  "Pulls neighbouring tiles' types as outskirts to tile at (x,y)"
  (setf (tile-variant (tile-at x y world)) nil) ; reset variant list
  (if (not (member 'sea (tile-type (tile-at x y world)))) ; don't do for sea tiles (for now)
      (progn
	
	(dolist (direction (list 'N 'NE 'SE 'S 'SW 'NW))
	  (let ((neighbour-tile (neighbour-tile-coords x y direction world)))
	    (if neighbour-tile
		(progn
		  (if (member 'sea (tile-type (aref (world-map world) (car neighbour-tile) (cdr neighbour-tile))))
		      (push (intern (concatenate 'string "SEA-OUTSKIRTS-" (symbol-name direction)))
			    (tile-variant (aref (world-map world) x y))))
		  (if (member-if #'(lambda (x) (eq (car x) 'city))
				 (tile-location (aref (world-map world) (car neighbour-tile) (cdr neighbour-tile))))
		      (push (intern (concatenate 'string "CITY-OUTSKIRTS-" (symbol-name direction)))
			    (tile-variant (aref (world-map world) x y))))
		  (if (member 'field (tile-type (aref (world-map world) (car neighbour-tile) (cdr neighbour-tile))))
		      (unless (or (eq direction 'sw) (eq direction 'nw))
			(push (intern (concatenate 'string "FIELD-OUTSKIRTS-" (symbol-name direction)))
			      (tile-variant (aref (world-map world) x y)))))
		  (if (member 'forest (tile-type (aref (world-map world) (car neighbour-tile) (cdr neighbour-tile))))
		      (push (intern (concatenate 'string "FOREST-OUTSKIRTS-" (symbol-name direction)))
			    (tile-variant (aref (world-map world) x y))))
		  ))))
	(if (member-if #'(lambda (x) (eq (car x) 'city)) (tile-location (tile-at x y world)))
	    (push 'city-a (tile-variant (tile-at x y world))))
	(if (member 'field (tile-type (tile-at x y world)))
	    (push (has-variants 'field (random 10)) (tile-variant (tile-at x y world))))
	(if (member 'forest (tile-type (tile-at x y world)))
	    (push 'forest-a (tile-variant (tile-at x y world)))))
      )
  ;; Pushing tile's type as base element of graphics list:
  (push (random-variant (car (tile-type (tile-at x y world))))
	(tile-variant (tile-at x y world)))
  )

(defun has-variants (tile-graphics-symbol &optional seed)
  "If given no seed, returns list containing variant symbols.
With seed, which the caller must generate in some way, returns (nth (rem seed length)) symbol"
  (dolist (sym-list *graphics-variants*)
    (if (eq tile-graphics-symbol (car sym-list))
	(return-from has-variants
	  (if seed (nth (rem seed (length (cdr sym-list))) (cdr sym-list))
	      (cdr sym-list))))))

(defun random-variant (tile-type-symbol)
  "Returns randomly chosen variant graphics symbol from *graphics-variants*,
NIL on failure."
  (let ((variants (cdr (assoc tile-type-symbol *graphics-variants*))))
    (when variants (nth (random (length variants)) variants))))

(defun sort-tile-graphics (tile)
  "Sorts a tile's variant field according to set priorities in ascending order"
  (setf (tile-variant tile)
	(sort (tile-variant tile)
	      #'(lambda (a b)
		  (< (graphics-priority (symbol-value a))
		     (graphics-priority (symbol-value b)))))))

(defun finalize-tile-region (x y &optional (world *world*))
  "Finalizes tile at (x,y) and all it's neighbours"
  ;;; Should be used on a tile after it has been manually modified
  ;;    with "(setf (tile-type (tile-at x y)))"
  (finalize-tile x y world)
  (dolist (direction (list 'N 'NE 'SE 'S 'SW 'NW))
    (let ((neighbour-tile (neighbour-tile-coords x y direction world)))
      (if neighbour-tile (finalize-tile (car neighbour-tile) (cdr neighbour-tile) world)))))


(defun make-random-world (width height) ;; the original and worst
  (let ((world (make-world)))
    (setf (world-width world) (- width 1))
    (setf (world-height world) (- height 1))
    (setf (world-map world) (make-array `(,width ,height)))

    ;; Make totally random map:
    (do ((x 0)
	 (y 0))
	((>= x (array-dimension (world-map world) 0)))
      (setf (aref (world-map world) x y)
	    (make-tile :type (if (< (random 4) 1)
				 (cons 'sea nil) ; '(sea) results in all tile types pointing to EQ type
				 (cons 'grass nil))))
      (and (member 'grass (tile-type (aref (world-map world) x y)))
	   (< (random 10) 1)
	   (prog1 1 (format t "~&Gonna build me a city at ~a, ~a~%" x y))
	   (create-city x y world))
	  
      (incf y)
      (if (>= y (array-dimension (world-map world) 1))
	  (progn (incf x)
		 (setf y 0))))

    world))

(defun create-city (x y &key (world *world*) (owner nil) (name nil) (production nil))
  (let ((new-city
	 (make-city :name (or name (random-city-name owner *world*))
		    :owner owner
		    :x x :y y
		    :production (or production *standard-city-production-list*))))
    (if world
	(progn
	  (pushnew (list 'city new-city) (tile-location (tile-at x y world)))
	  (pushnew (random-variant 'city) (tile-variant (tile-at x y)))
	  (finalize-tile-region x y world)
	  (pushnew new-city (world-cities world))))
    new-city))

(defun random-city-name (&optional (owner nil) (world *world*))
  (let ((city-name-list (if (null owner) (getf (world-theme world) :city-names)
			    ;;TODO: else get name from owner faction's name list
			    )))
    (nth (random (1+ (car city-name-list))) (cdr city-name-list))))

