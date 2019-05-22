(in-package :war)

(defvar *graphics-variants* nil) ; ((name1 name1-a name1-b) (name2 name2-a name2-b name2-c)) etc..
(defconstant +std-short-dirs+ '(N NE SE S SW NW))
(defconstant +std-long-dirs+ '(NORTH NORTH-EAST SOUTH-EAST SOUTH SOUTH-WEST NORTH-WEST))

(defun oppdir (direction)
  "Return direction symbol opposite to direction."
  (declare (symbol direction))
  (case direction
    (N 'S)
    (NE 'SW)
    (NW 'SE)
    (S 'N)
    (SE 'NW)
    (SW 'NE)
    (NORTH 'SOUTH)
    (NORTH-EAST 'SOUTH-WEST)
    (SOUTH-EAST 'NORTH-WEST)
    (SOUTH 'NORTH)
    (SOUTH-WEST 'NORTH-EAST)
    (NORTH-WEST 'SOUTH-EAST)))

(defun short-dir (long-dir)
  "Translate a direction in long form to short form."
  (mapcar #'(lambda (short long)
	      (when (eq long-dir long) (return-from short-dir short)))
	  +std-short-dirs+
	  +std-long-dirs+)
  nil)

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
	 ((eq algo 'smooth)
	  (make-smoothed-height-world width height faction-count mirror))
	 )))

;; (init-test 40 40 :algo 'smooth :mirror t)
(defun make-smoothed-height-world (width height faction-count
				   &optional (mirror nil))
  (let* ((world (make-world :width (1- width) ; Why is this off by one?
			    :height (1- height)
			    :map (make-array (list width height))
			    :factions nil))
	 (number-world (make-array (list width height))))

    (when (null mirror)
      (dotimes (x width)
	(dotimes (y height)
	  (setf (aref number-world x y) (random 100)))))

    (when mirror
      (do ((x 0)
	   (y 0)
	   (rand-val (random 100) (random 100)))
	  ((= x height))
	(setf (aref number-world x y) rand-val)
	(setf (aref number-world (- width 1 x) (- height 1 y)) rand-val)
	(incf y)
	(when (= y height)
	  (incf x)
	  (setf y 0))))

    (setf number-world (blur-array number-world 1.1)) ; TODO: radius 1 is too sharp, 2 is too soft

    (docoords (x y world)
      (let ((gen-num (aref number-world x y)))
	(setf (tile-at x y world) (make-tile :type (if (> gen-num 50)
						       '(grass) '(sea))))
	(or (when (> gen-num 65) (push 'mountain (tile-type (tile-at x y world))))
	    (when (> gen-num 60) (push 'hill (tile-type (tile-at x y world)))))))

    world
    ))

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
	 (template-world (make-world :width (1- tw-width)
				     :height (1- tw-height)
				     :map (make-array (list tw-width tw-height))
				     :factions nil)))

    ;; Initialize world with sea tiles:
    (dotimes (x width)
      (dotimes (y height)
	(setf (tile-at x y world) (make-tile))
	))
    
    ;; Create some random noise to use for breadth-first-fill:
    (dotimes (x tw-width)
      (dotimes (y tw-height)
	(setf (tile-at x y template-world)
	      (make-tile :type (list (random 10))) ; This is the WEIGHT of filling the tile
	      )))

    ;;(setf template-world (blur-number-world template-world)) ; not good

    ;; Select a random coordinate and fire breadth-first-fill at it:
    (let ((fill-these-tiles
	   (breadth-first-fill ;(cons (random width) (random height))
	    (mapcar #'cons (list-randoms islands width)
		    (list-randoms islands height))
	    island-size
	    template-world           ; NOTE: These "tiles" have been hacked to have integers in type field
					; -> no magic dynamic vars required in this case
	    #'(lambda (x y world)
		(car (last (tile-type (tile-at x y world)))))
	    )))

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

(defun blur-array (array &optional (radius 1))
  "Smooths out a 2-dimensional array."
  (let* ((width (array-dimension array 0))
	 (height (array-dimension array 1))
	 (sig-rad (ceiling (* radius 2.57))) ; ???
	 (blurred-array (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
	(let ((value 0)
	      (weight-sum 0))
	  (do ((xb (- x sig-rad) (incf xb)))
	      ((>= xb (+ x sig-rad 1)))
	    (do ((yb (- y sig-rad) (incf yb)))
		((>= yb (+ y sig-rad 1)))
	      (when (and (< -1 xb width)
			 (< -1 yb height))
		(let ((weight
		       (/ (exp (/ (- (+ (expt (- xb x) 2)
					(expt (- yb y) 2)))
				  (* 2 radius radius)))
			  (* pi 2 radius radius))))
		  (incf value (* weight
				 (aref array xb yb)
				 

				 ))
		  (incf weight-sum weight))
		(setf (aref blurred-array x y)
		      (round (/ value weight-sum)))))))))
    blurred-array))

;; TODO: This gaussian blur is way too heavy, just need to smooth edges a little bit
(defun blur-number-world (world &optional (radius 1))
  "Smooths out a world with numbers in tile-type field."
  (let ((sig-rad (ceiling (* radius 2.57))) ; ???
	(blur-world (make-world :width (world-width world)
				:height (world-height world)
				:map (make-array
				      (list (1+ (world-width world))
					    (1+ (world-height world)))))))
    (docoords (x y world)
      (let ((value 0)
	    (weight-sum 0))
	(do ((xb (- x sig-rad) (incf xb))) ; Should prolly make a macro for this too
	    ((>= xb (+ x sig-rad 1)))
	  (do ((yb (- y sig-rad) (incf yb)))
	      ((>= yb (+ y sig-rad 1)))
	    (when (coord-in-bounds (cons xb yb) world)
	      (let ((weight
		     (/ (exp (/ (- (+ (expt (- xb x) 2)
				      (expt (- yb y) 2)))
				(* 2 radius radius)))
			(* pi 2 radius radius))))
		(incf value (* weight
			       (car (tile-type
				     (tile-at
				      (min (1- (world-width world))
					   (max 0 xb))
				      (min (1- (world-height world))
					   (max 0 yb))
				      world)))))
		(incf weight-sum weight))
	      (setf (aref (world-map blur-world) x y)
		    (make-tile :type (list (round (/ value weight-sum))))))))))
    blur-world))


(defun list-randoms (size rand-num)
  (if (< size 1) nil
      (cons (random rand-num) (list-randoms (1- size) rand-num))))


;;; To use this: various types of tile symbols need to be bound AND be declared special.
;; Unless apparently if you abuse tile structs and push numbers where they don't belong...
(defun breadth-first-fill (start-list range world move-cost-func)
  ;;move-cost-func takes x and y coord and world of tile to move to
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
					    direction
					    (world-width world)
					    (world-height world)))
				       '(n ne se s sw nw)))
	      
	      (cond ((null neighbour) nil)
		    ((null (gethash neighbour came-from))
		     (let ((move-cost (- (car current)
					 (funcall move-cost-func (car neighbour) (cdr neighbour) world)
					 )))
		       (cond ((>= move-cost 0)
			      (heap-insert frontier neighbour move-cost)
			      (setf (gethash neighbour came-from)
				    (cons move-cost (cdr current))))))))))))
    came-from))



;;test below with this
;; Currently duplicate borders from the other side appear in the hash map
'(maphash #'(lambda (key val) (format t "~&KEY: ~a~%~a" key val))
	   (breadth-first-fill-borders
  5 7 'n 2 *world*
  #'(lambda (xy dir world)
      (let ((grass 1)
	    (sea 1000)
	    (field 1)
	    (mountain 10)
	    (forest 1)
	    (swamp 1)
	    (type-lists (border-adjacent-tile-types xy dir world)))
	(declare (special grass sea field
			  mountain forest swamp))

	(min (symbol-value (car (last (car type-lists))))
	     (symbol-value (car (last (cadr type-lists)))))))))

(defun breadth-first-fill-borders
    (start-x start-y start-dir range world move-cost-func end-when-func)
  "Move-cost-func takes 3 arguments: (cons x y) dir world
End-when-func takes 2 arguments: (cons x y) dir"

  (let ((frontier (make-heap))
	(came-from (make-hash-table :test 'equal)))
    
    (heap-insert frontier (list (cons start-x start-y) start-dir) range)
    (setf (gethash (list (cons start-x start-y) start-dir) came-from) (list range nil))
    
    (do ((current))
	((heap-empty frontier))

      (setf current (heap-remove-max frontier))

	(if (> (car current) 0)
	    (dolist (neighbour (list-neighbour-borders (caadr current) ; x
						       (cdadr current) ; y
						       (caddr current))) ; dir
	      
	      (cond ((null neighbour) nil)
		    ((null (car neighbour)) nil)
		    ((null (gethash neighbour came-from))
		     (let ((move-cost
			    (- (car current)
			       (or (funcall move-cost-func
					    (car neighbour)
					    (cadr neighbour)
					    world)
				   (1+ range))) ; if move-cost-func returns nil, make move-cost var < 0
			     ))
		       (cond ((>= move-cost 0)
			      (heap-insert frontier neighbour move-cost)
			      (setf (gethash neighbour came-from)
				    (cons move-cost (cdr current))))))))
	      (if (funcall end-when-func (car neighbour) (cadr neighbour))
		  (return-from breadth-first-fill-borders came-from)))))
    came-from
    ))

(defun list-neighbour-borders (tile-x tile-y border &optional (world *world*))
  "Returns the inout's neighbouring borders as list of four instances of ((x . y) dir),
dir being one of (N NW SW)."
  (case border ; reorganize
    (s (setf border 'n)
       (let ((ntc (neighbour-tile-coords tile-x tile-y 's
					 (world-width world)
					 (world-height world))))
	 (setf tile-x (car ntc)
	       tile-y (cdr ntc))))
    (se (setf border 'nw)
	(let ((ntc (neighbour-tile-coords tile-x tile-y 'se
					  (world-width world)
					  (world-height world))))
	  (setf tile-x (car ntc)
		tile-y (cdr ntc))))
    (ne (setf border 'sw)
	(let ((ntc (neighbour-tile-coords tile-x tile-y 'ne
					  (world-width world)
					  (world-height world))))
	  (setf tile-x (car ntc)
		tile-y (cdr ntc)))))
  (case border
    (n (let ((ntc (neighbour-tile-coords tile-x tile-y 'ne
					 (world-width world)
					 (world-height world))))
	 (list (list (cons tile-x (1- tile-y)) 'sw)
	       (list ntc 'nw)
	       (list (cons tile-x tile-y) 'nw)
	       (list ntc 'sw))))
    (nw (let ((ntc (neighbour-tile-coords tile-x tile-y 'nw
					  (world-width world)
					  (world-height world))))
	  (list (list (cons tile-x (1- tile-y)) 'sw)
		(list (cons (car ntc) (1+ (cdr ntc))) 'n)
		(list (cons tile-x tile-y) 'n)
		(list (cons tile-x tile-y) 'sw))))
    (sw (let ((ntc (neighbour-tile-coords tile-x tile-y 'sw
					  (world-width world)
					  (world-height world))))
	  (list (list ntc 'n)
		(list (cons tile-x (1+ tile-y)) 'nw)
		(list (cons tile-x (1+ tile-y)) 'n)
		(list (cons tile-x tile-y) 'nw))))))

;;testing:
'(breadth-first-fill-borders
  5 14 'nw 10 *world*
  #'(lambda (xy dir world)
      (let ((grass 1)
	    (sea 1000)
	    (field 1)
	    (mountain 10)
	    (forest 1)
	    (swamp 1)
	    (type-lists (border-adjacent-tile-types xy dir world)))
	(declare (special grass sea field
			  mountain forest swamp))

	(min (symbol-value (car (last (car type-lists))))
	     (symbol-value (car (last (cadr type-lists))))))))
  

(defun vert-coord-dir (x1 y1 x2 y2)
  "The vertex direction of (x2,y2) from (x1,y1), nil on failure."
  (dolist (dir '(up right left))
    (if (equal (neighbour-vertex-coords x1 y1 dir)
	       (cons x2 y2))
	(return-from vert-coord-dir dir))))

(defun border-adjacent-tile-types (coord-pair dir &optional (world *world*))
  "Returns list with tile-type lists of tiles bordering DIR of tile (tile-x,tile-y)"
  (let* ((tile-x (car coord-pair))
	 (tile-y (cdr coord-pair))
	 (neighbour-tile (neighbour-tile tile-x tile-y dir world)))
    (when neighbour-tile
      (list (tile-type (if (coord-in-bounds coord-pair)
			   (tile-at tile-x tile-y world)
			   neighbour-tile)) ; if not in bounds, use neighbour's typelist
	    (tile-type neighbour-tile)))))

(defun heightmap-border-adjacent-value (coord-pair dir array)
  (let* ((x (car coord-pair))
	 (y (cdr coord-pair))
	 (max-x (1- (array-dimension array 0)))
	 (max-y (1- (array-dimension array 0)))
	 (neighbour-tile (neighbour-tile-coords x y dir max-x max-y)))
    (when neighbour-tile
      (list (tile-type (if (and (<= 0 x max-x) (<= 0 y max-y))
			   (aref array x y)
			   neighbour-tile)) ; if not in bounds, use neighbour's typelist
	    (tile-type neighbour-tile)))))


(defun neighbour-vertex-coords (vert-x vert-y direction &optional (world *world*))
  "Direction should be one of 'UP 'RIGHT or 'LEFT"
  ;; TODO: Check limits based on world-width & world-height
  (cond ((and (evenp vert-y) (evenp vert-x))
	 (case direction
	   (up (cons vert-x (1- vert-y)))
	   (right (cons (1+ vert-x) (1- vert-y)))
	   (left (cons vert-x (1+ vert-y)))))
	((and (evenp vert-y) (oddp vert-x))
	 (case direction
	   (up (cons vert-x (1- vert-y)))
	   (right (cons (1+ vert-x) (1+ vert-y)))
	   (left (cons vert-x (1+ vert-y)))))
	((and (oddp vert-y) (evenp vert-x))
	 (case direction
	   (up (cons vert-x (1- vert-y)))
	   (right (cons (1+ vert-x) vert-y))
	   (left (cons vert-x (1+ vert-y)))))
	(t ; both odd
	 (case direction
	   (up (cons vert-x (1- vert-y)))
	   (right (cons vert-x (1+ vert-y)))
	   (left (cons (1- vert-x) (1+ vert-y)))))
  ))

(defun finalize-world (&optional (world *world*))
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

(defun pointers-to-variants (sym-list &optional seed)
  "Transforms list of logical tiletype symbols into graphics pointers."
  (let ((variant-seed (if seed seed (random 100))))
    (remove nil (mapcar #'(lambda (sym)
			    (has-variants sym variant-seed))
			sym-list))))

(defun collect-graphics (tile)
  "Collect symbols from tile's logical fields."
  (when tile
    (remove nil
	    (append (tile-type tile)
		    (mapcar #'car (tile-location tile))
		    (tile-river-borders tile)
		    (tile-road-links tile)
		    (tile-rail-links tile)))))

(defun collect-overflowing-graphics (x y &optional (world *world*))
  "Collect symbols from (x,y)tile's fields containing graphics that might overflow."
  (let ((tile (tile-at x y world)))
    (when tile
      (pointers-to-variants
       (collect-graphics tile)
       (+ x y)))))

(defun collect-border-graphics (x y &optional (world *world*))
  "Collect symbols from (x,y)tile's fields containing graphics that are supposed to
be drawn 'between' tiles."
  (let ((tile (tile-at x y world)))
    (when tile
      (pointers-to-variants
       (append (tile-river-borders tile)
	       (tile-road-links tile)
	       (tile-rail-links tile))
       (+ x y)))))

(defun finalize-tile-variant-list (x y &optional (world *world*))
  "Initializes a tile's variant-list with graphics generated from it's own fields."
  (let ((tile (tile-at x y world))
	(variant-seed (+ x y)))
    (setf (tile-variant tile)
	  (pointers-to-variants (collect-graphics tile) variant-seed))))

(defun OBSOLETEpull-outskirts (x y &optional (world *world*))
  "Lists neighbouring tiles' types as outskirts."
  (let ((outskirts nil))
    (mapcar #'(lambda (ncrd dir)
		(when ncrd
		  (dolist (sym (collect-overflowing-graphics (car ncrd) (cdr ncrd)))
		    (let ((candidate-border
			   (intern (concatenate 'string
						(symbol-name sym)
						"-OUTSKIRTS-"
						(symbol-name dir)))))
		      (format t "~&---~a xxx ~a~%" (symbol-name sym) (symbol-name dir))
		      (when (and
			     (boundp candidate-border)
			     (or (symbol-value candidate-border)
				 (and (setf candidate-border (primary-outskirt-graphics candidate-border))
				      (symbol-value candidate-border))))
			(push candidate-border outskirts))
		      )
		    )
		  ))
	    (neighbour-tiles x y world)
	    +std-short-dirs+)
    outskirts))

(defun pull-outskirts (x y &optional (world *world*))
  "Lists neighbouring tiles' types as outskirts."
  (do ((outskirts)
       (neighbours (neighbour-tiles x y world) (cdr neighbours))
       (dir-head +std-short-dirs+ (cdr dir-head))
       (this-graphics (collect-graphics (tile-at x y world))))
      ((null neighbours) outskirts)
    (let* ((neigh-x (caar neighbours))
	   (neigh-y (cdar neighbours))
	   (dir (car dir-head))
	   (logic-overflows (collect-graphics (tile-at neigh-x neigh-y world)))
	   (variant-overflows (collect-overflowing-graphics neigh-x neigh-y world)))

      (setf outskirts
	    (nconc
	     (remove nil
		     (mapcar #'(lambda (log-ovs var-ovs)
				 ;; if this tile has same type as neighbour AND
				 ;; is not set to place outskirts everywhere -> ignore outskirts,
				 ;; else get some relevant outskirts reference:
				 (unless (and (member log-ovs this-graphics)
					      (not (member :outskirts-everywhere (get-overrides log-ovs))))
				   (get-outskirt var-ovs dir)))
			     logic-overflows
			     variant-overflows))
	     outskirts))
      )))

(defun get-overflown (primary-symbol direction &optional variant)
  "Returns most relevant reference to outskirt graphics for primary-symbol."
  (let ((var-in-primary (get-variant primary-symbol)))
    (intern (concatenate 'string
			 (cond (var-in-primary (symbol-name primary-symbol))
			       (t (concatenate 'string
					       (symbol-name primary-symbol)
					       "-"
					       (if variant
						   (symbol-name variant)
						   "A"))))
			 "-OUTSKIRTS-"
			 (symbol-name direction)))))

(defun get-variant (sym)
  "If hyphen found, return what's after it."
  (let ((rev-sym (reverse (symbol-name sym))))
    (when (find #\- rev-sym)
      (intern (reverse (subseq rev-sym 0 (position #\- rev-sym)))))))

(defun get-base (sym)
  "If hyphen found, return what's before it."
  (let ((sym-name (symbol-name sym)))
    (intern (subseq sym-name 0 (position #\- sym-name)))))

(defun get-outskirt (primary-symbol direction &optional seed)
  "Return a valid reference to graphics of primary-symbol's
outskirt towards direction."
  ;;; TODO: Store outskirts also in *graphics-variants* and pick them from there based on seed.
  (let* ((potential-symbol (get-overflown primary-symbol direction))
	 (symbol-main-variant (get-overflown (get-base primary-symbol) direction)))
    (cond ((and (boundp potential-symbol) (symbol-value potential-symbol)) potential-symbol)
	  ((and (boundp symbol-main-variant) (symbol-value symbol-main-variant)) symbol-main-variant))))

(defun primary-outskirt-graphics (sym)
  "Forms the 'xxx-A-outskirts-dir' variant out of the given outskirt symbol."
  (let* ((sym-name (symbol-name sym))
	 (rev-sym-name (reverse sym-name)))
    (intern (concatenate
	     'string
	     (subseq sym-name 0 (position #\- sym-name))
	     "-A-OUTSKIRTS-"
	     (reverse (subseq rev-sym-name 0 (position #\- rev-sym-name)))))))

(defun neighbour-tiles (x y &optional (world *world*))
  "Returns list of cons coordinates to (x.y)'s neighbours, or nils to off map."
  (mapcar #'(lambda (dir)
	      (neighbour-tile-coords x y dir
				     (world-width world)
				     (world-height world)))
	  +std-short-dirs+))

;;   TODO: might want to handle complete masks in some way (as in no field outskirts on sea tiles etc..)
(defun finalize-tile (x y &optional (world *world*))
  (setf (tile-variant (tile-at x y world))
	(remove-duplicates
	 (append
	  (collect-overflowing-graphics x y world)
	  (pull-outskirts x y world)
	  (collect-border-graphics x y world)))))

(defun has-variants (primary-symbol &optional seed)
  "If given no seed, returns list containing variant symbols, or NIL on failure.
With seed, which the caller must generate in some way, returns (nth (rem seed length)) symbol"
  (let ((variants (cdr (assoc primary-symbol *graphics-variants*))))
    (if (and variants seed)
	(nth (rem seed (length variants)) variants)
	variants)))

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

(defun sort-variant-list (variant-list)
  "Sorts a variant list from a tile according to set priorities in ascending order"
  (sort variant-list
	#'(lambda (a b)
	    (< (graphics-priority (symbol-value a))
	       (graphics-priority (symbol-value b))))))

(defun finalize-tile-region (x y &optional (world *world*))
  "Finalizes tile at (x,y) and all it's neighbours"
  ;;; Should be used on a tile after it has been manually modified
  ;;    with "(setf (tile-type (tile-at x y)))"
  (finalize-tile x y world)
  (dolist (direction (list 'N 'NE 'SE 'S 'SW 'NW))
    (let ((neighbour-tile (neighbour-tile-coords x y direction
						 (world-width world)
						 (world-height world))))
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
				 (cons 'sea nil) ; '(sea) results in all tile types pointing to same list
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
	  (pushnew (random-variant 'city) (tile-variant (tile-at x y world)))
	  (finalize-tile-region x y world)
	  (pushnew new-city (world-cities world))))
    new-city))

(defun random-city-name (&optional (owner nil) (world *world*))
  (let ((city-name-list (if (null owner) (getf (world-theme world) :city-names)
			    ;;TODO: else get name from owner faction's name list
			    )))
    (nth (random (1+ (car city-name-list))) (cdr city-name-list))))

;;This is a utility function for tile graphics editing
(defun dummy-outskirts (x y type-symbol-var)
  "Create all outskirts related to type-symbol in tile at x y
-> screenshot that + do some gimping to fix tile borders" ;or not
  (let ((old-variant-list (reverse (tile-variant (tile-at x y))))
	(type-outskirts (mapcar #'(lambda (dir)
				    (intern (concatenate 'string
							 (symbol-name type-symbol-var)
							 "-OUTSKIRTS-"
							 (symbol-name dir))))
				+std-short-dirs+)))
    (setf (tile-variant (tile-at x y))
	  (let ((newlist '(grass-a)))
	    (dolist (x type-outskirts)
	      (and (boundp x) (symbol-value x) (setf newlist (push x old-variant-list))))
	    (reverse old-variant-list)))))


(defun create-location-at (location-symbol x y &optional (world *world*))
  "Creates symbol location-symbol in type field of tile at x y, and sets up tile's graphics."
  (setf (tile-type (tile-at x y world))
	(reverse (pushnew location-symbol
			  (tile-type (aref (world-map world) x y)))))
  (finalize-tile-region x y world)
  (tile-type (tile-at x y world)))


(defun add-rail (x y direction &optional (world *world*))
  "Adds a single piece of rail running from tile X Y to tile towards DIRECTION."
  (when (member direction +std-long-dirs+) (setf direction (short-dir direction)))
  (let ((tile (tile-at x y world))
	(destination (neighbour-tile x y direction world)))
    (when (or (member 'sea (tile-type tile))
	      (member 'sea (tile-type destination)))
      (return-from add-rail nil))

    (pushnew (intern (concatenate 'string "RAIL-" (symbol-name direction)))
	     (tile-rail-links tile))
    (pushnew (intern (concatenate 'string "RAIL-" (symbol-name (oppdir direction))))
	     (tile-rail-links destination))
    (finalize-tile-region x y))) ; Not efficient to form variant lists here but way easier to test


(defun add-river (x y size location-on-tile &optional (world *world*))
  "Adds a single piece of river of type SIZE running on border LOCATION-ON-TILE of tile X Y."
  (when (member location-on-tile +std-long-dirs+) (setf location-on-tile (short-dir location-on-tile)))
  (let ((tile (tile-at x y world))
	(destination (neighbour-tile x y location-on-tile world)))
    (when (or (member 'sea (tile-type tile))
	      (member 'sea (tile-type destination)))
      (return-from add-river nil))
    
    (pushnew (intern (concatenate 'string (symbol-name size) "-" (symbol-name location-on-tile)))
	     (tile-rail-links tile))
    (pushnew (intern (concatenate 'string (symbol-name size) "-" (symbol-name (oppdir location-on-tile))))
	     (tile-rail-links destination))
    (finalize-tile-region x y)))


(defun heightmap-run-river-to (heightmap mouth-x mouth-y mouth-dir length)
  (dolist (border
	    (border-hash-path
	     (list (cons end-x end-y) end-dir)
	     (breadth-first-fill-borders
	      mouth-x mouth-y mouth-dir length *world*
	      #'(lambda (xy dir world)
		  (let ((grass 1) ; move costs for rivers
			(sea length)
			(field 1)
			(mountain 100)
			(forest 1)
			(swamp 1)
			(hill 25)
			(type-lists (border-adjacent-tile-types xy dir world)))
		    (declare (special grass sea field
				      mountain forest swamp hill))
		    (when type-lists
			(min (highest-move-cost (car type-lists))
			     (highest-move-cost (cadr type-lists))
			     ))))

	      #'(lambda (xy dir)
		  (if (and (equal xy (cons end-x end-y)) (eq end-dir dir)) t)))))

    (let ((x (caar border))
	  (y (cdar border))
	  (pos (cadr border)))
      (add-river x y 'stream pos)
  
  )))


(defun run-river-from-to (mouth-x mouth-y mouth-dir end-x end-y end-dir &optional (range 1000))
  "Runs a river from mouth xy to end xy. Mouth coordinates + dir should refer to a land tile
border on the coast, end coords to a tile border inland."
  (dolist (border
	    (border-hash-path
	     (list (cons end-x end-y) end-dir)
	     (breadth-first-fill-borders
	      mouth-x mouth-y mouth-dir range *world*
	      #'(lambda (xy dir world)
		  (let ((grass 1) ; move costs for rivers
			(sea range)
			(field 1)
			(mountain 100)
			(forest 1)
			(swamp 1)
			(hill 25)
			(type-lists (border-adjacent-tile-types xy dir world)))
		    (declare (special grass sea field
				      mountain forest swamp hill))
		    (when type-lists
			(min (highest-move-cost (car type-lists))
			     (highest-move-cost (cadr type-lists))
			     ))))

	      #'(lambda (xy dir)
		  (if (and (equal xy (cons end-x end-y)) (eq end-dir dir)) t)))))

    (let ((x (caar border))
	  (y (cdar border))
	  (pos (cadr border)))
      (add-river x y 'stream pos)
  
  )))

(defun highest-move-cost (type-list)
  (apply #'max (mapcar #'(lambda (sym)
			   (if (boundp sym) (symbol-value sym)
			       (progn
				 (format t "~&No move cost bound for ~a!~%" sym)
				 666)))
		       type-list)))

(defun border-hash-path (start-border hashmap)
  "Traverse a hashmap of borders shortest route.
Generates route as a list (((end-x . end-y) end-dir) ... ((mouth-x . mouth-y) mouth-dir))"
  (if (and (car start-border) start-border)
      (cons start-border
	    (let ((border (gethash start-border hashmap)))
	      (border-hash-path (cdr border) hashmap)))))
