
(in-package #:war)

(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-image)
(ql:quickload :lispbuilder-sdl-ttf)
(ql:quickload :lispbuilder-sdl-gfx)

(defvar *testunit* nil)
(defvar *current-move-area* nil)

(defun set-test-unit (oct-diam)
  (format t "~%Setting up testunit~&")
  (counter-gen:nato-dimension-init oct-diam)
  (cond (t ;;if t -> set to create new armies at (10,8) everytime (test) runs
	 ;;(null *testunit*) ;; no more units created
	 (setf *testunit*
	       (make-army :x 0 :y 0
			  :id 666
			  :movement 25
			  :counter
			  (make-graphics :surface
					 ;;(counter-gen:cns-fun
					 ;; oct-diam 'counter-gen:friendly 'counter-gen:land
					 ;; '(counter-gen:infantry counter-gen:mountain))
					 (counter-gen:generate
					  80 80 oct-diam 'counter-gen:neutral 'counter-gen:land
					  '(counter-gen:anti-tank counter-gen:mountain)
					  'team 'half-track)
					 :x-at 24 :y-at 7)))
	 (place-unit *testunit* 10 8))))

(defstruct graphics
  (surface nil)
  (x-at 0)  ; modifiers for drawing
  (y-at 0))

(defvar *world* nil)

(defun init-test (height width)
  (setf *world* (init-world height width))
  nil)


(defun cursor-coordinates-on-map (screen-x screen-y x-shift y-shift)
  ;; tile-x & tile-y are the (almost) actual
  ;;  coordinates of the hex the cursor is hovering over
  (let* ((tile-x (floor (/ (- screen-x x-shift) (car tile-size))))
	 (tile-y (floor (/ (if (evenp tile-x)
			       (- screen-y y-shift)
			       (- (- screen-y y-shift) (/ (cdr tile-size) 2)))
			   (cdr tile-size)))))
    (cons tile-x tile-y)))

(defun cursor-coordinates-on-screen (screen-x screen-y x-shift y-shift tile-x)
  "Returns as (x . y) the coordinates to draw a selection cursor to screen"
  (let* (
	 ;;There are a bunch of hexes on screen. Which one are we pointing at?
	 ;;used for multiplying pixel coordinates
	 (adj-tile-x (floor (/ (- screen-x (rem x-shift (car tile-size))) (car tile-size))))
	 (adj-tile-y (floor (/ (if (evenp tile-x)
				   (- screen-y (rem y-shift (cdr tile-size)))
				   (- (- screen-y (rem y-shift (cdr tile-size)))
				      (/ (cdr tile-size) 2)))
			       (cdr tile-size))))
	 ;;The x coordinate for graphics:
	 (adj-screen-x (+ (* adj-tile-x (car tile-size))
			  (rem x-shift (car tile-size))))
	 ;;y coordinate graphics need to be moved downwards by (cdr tile-size) / 2 if x is odd
	 (adj-screen-y (if (evenp tile-x)
			   (+ (* adj-tile-y (cdr tile-size))
			      (rem y-shift (cdr tile-size)))
			   (+ (* adj-tile-y (cdr tile-size))
			      (rem y-shift (cdr tile-size))
			      (floor (/ (cdr tile-size) 2)))))
	 )
    (cons adj-screen-x adj-screen-y)))


(defun test ()
  (sdl:with-init()
    (defparameter window (sdl:window 1500 900 :title-caption "a war game"))
    (setf (sdl:frame-rate) 60)
    
    (sdl:initialise-default-font)

    (load-tiles)

    (counter-gen:nato-color-init)
    (set-test-unit 34) ;; testing army graphics

    (let ((x-shift 0) (y-shift 0)
	  (selector-tile '(0 . 0)) (selector-graphics '(0 . 0))
	  (selected-tile nil) (selected-graphics nil)
	  (selected-unit nil))

	  (sdl:with-events ()
	    (:quit-event () t)
	    (:key-down-event (:key keyb :mod keyb-mod)
			     (format t "~&Key: ~a, mod: ~a~%" keyb keyb-mod)
			     (case keyb ((:sdl-key-escape) (setf selected-unit nil))))

	    (:mouse-motion-event (:x x :y y)
				 (setf selector-tile
				       (cursor-coordinates-on-map
					x y x-shift y-shift))
				 (setf selector-graphics 
				       (cursor-coordinates-on-screen
					x y x-shift y-shift (car selector-tile)))
				 
				 )
	    
	    (:mouse-button-down-event
	     (:button button :x x :y y)
	     (cond ((equal 'world (mouse-over-what x y))
		    (cond ((equal button sdl:sdl-button-right)
			   (sdl:clear-display sdl:*black*)
			   (setf x-shift (- x-shift (- x (floor (/ (sdl:width window) 2)))))
			   (setf y-shift (- y-shift (- y (floor (/ (sdl:height window) 2))))))
			  
			  ((equal button sdl:sdl-button-left)
			   (setf selected-tile selector-tile)
			   (setf selected-graphics selector-graphics)
			   (format t "~&Selected ~a~%" selected-tile)
			   (cond ((null selected-unit)
				  (setf selected-unit (car (tile-units ;; take the first unit from list
							    (aref (world-map *world*)
								  (car selected-tile)
								  (cdr selected-tile))))))
				 
				 ((gethash selected-tile *current-move-area*)
				  (place-unit selected-unit
					      (car selected-tile)
					      (cdr selected-tile)))))

			  ((equal button sdl:sdl-button-wheel-up)
			   (set-tile-size 'large))
			  ((equal button sdl:sdl-button-wheel-down)
			   (set-tile-size 'small))))
		   ((equal 'panel (mouse-over-what x y))
		    (setf selected-unit (select-from-panel y (tile-at
							      (car selected-tile)
							      (cdr selected-tile)))))))

	    (:idle ()

		   (draw-world x-shift y-shift
			       selector-graphics selector-tile
			       selected-tile selected-unit)
		   (draw-panel selected-tile selected-unit)
		   (sdl:update-display)
		   )))))

(defun mouse-over-what (mouse-x mouse-y)
  (if (>= mouse-x (- (sdl:width window) *panel-width*))
      'panel
      'world))

(defun tc-gc (tile-coords x-shift y-shift) ;; Tile Coordinate to Graphics Coordinate
  (if (null (car tile-coords)) (return-from tc-gc nil))
  (let* ((x (car tile-coords))
	 (y (cdr tile-coords))
	 (tile-width (car tile-size))
	 (tile-height (cdr tile-size))
	 (gx-location (* x tile-width))
	 (gy-location (* y tile-height)))
    (cons (+ gx-location
	     x-shift)
	  (+ (if (evenp x) gy-location
		 (+ gy-location (floor tile-height 2)))
	     y-shift))))


(defun draw-move-area (start move-range x-shift y-shift end)
  (let ((move-area (move-area start move-range)))
    (labels ((draw-path (current)
	       (let* ((next (cdr (gethash current move-area)))
		     (current-g (tc-gc current
				       x-shift y-shift))
		     (next-g (tc-gc next
				    x-shift y-shift)))
		 (cond ((car next)
			(sdl:draw-line-* (+ (car current-g) (floor (car tile-size) 2))
					 (+ (cdr current-g) (floor (cdr tile-size) 2))
					 (+ (car next-g) (floor (car tile-size) 2))
					 (+ (cdr next-g) (floor (cdr tile-size) 2))
					 :color sdl:*white*)
			(draw-path next))))))

      (maphash #'(lambda (key value)
		   (draw-string-at (car key) (cdr key)
				   x-shift y-shift
				   (write-to-string (car value))))
	       move-area)

      (if end
	  (draw-path end)))))



(defun move-area (start move-range)
  ;; return currently the came-from hash table with (cons x1 y1) as key
  ;;   value is (movement-left-after-this-move (cons x0 y0))
  
;;????? SHOULD BE FIXED NOW with declare specials
  ;;;;; engine must first be initialized by running test-init and test
  ;; then ->
  ;; ???Function must be compiled before tile-type shadowing will work
  
;;   check with this:
;;    (maphash #'(lambda (key value)
;;	 (format t "~&~a :: ~a~%" key value)) xxx)
  
  (let ((sea 100) ; temporary shadows for move costs
	(grass 2)

	(frontier (make-heap))
	(came-from (make-hash-table :test 'equal)))
    (declare (special sea) (special grass))
    (heap-insert frontier start move-range)
    (setf (gethash start came-from) `(,move-range nil))

    (do ((current))
	((heap-empty frontier))

      (setf current (heap-remove-max frontier))

      (if (> (car current) 0)
	  (dolist (neighbour (mapcar #'(lambda (direction)
					 (neighbour-tile-coords
					  (cadr current)
					  (cddr current)
					  direction *world*))
				     '(n ne se s sw nw)))
	    
	    (cond ((null neighbour) nil)
		  ((null (gethash neighbour came-from))
		   (let ((move-cost (- (car current)
				       (eval (tile-type (aref (world-map *world*)
							      (car neighbour)
							      (cdr neighbour)))))))
		     (cond ((>= move-cost 0)
			    (heap-insert frontier neighbour move-cost)
			    (setf (gethash neighbour came-from)
				  (cons move-cost (cdr current)))))))))))
    (setf *current-move-area* came-from)))

(defun draw-world (x-shift y-shift selector-graphics selector-tile selected-tile selected-unit)

  (let* ((draw-count 0)
	 (x-start-void (floor x-shift (car tile-size)))
	 (x-start (if (>= (+ x-shift (car tile-size)) 0) 0
		      (- (abs x-start-void) 2)))
	 (x-end (min
		 (+ (- x-start-void) (floor (sdl:width window) (car tile-size)))
		 (1- (array-dimension (world-map *world*) 0)))) ;; The last column
	 (y-start-void (floor y-shift (cdr tile-size)))
	 (y-start (min (if (>= (+ y-shift (cdr tile-size)) 0) 0
			   (- (abs y-start-void) 2))
		       (1- (array-dimension (world-map *world*) 1))))
	 (y-end (min
		 (+ (- y-start-void) (floor (sdl:height window) (cdr tile-size)))
		 (1- (array-dimension (world-map *world*) 1))))) ;; The last row

    (do ((x x-start)
	 (y y-start))
	(nil)

      (cond ((> y y-end)
	     (setf y y-start)
	     (incf x)))
      (if (> x x-end)
	  (return))

      (setf draw-count (+ draw-count 1))

      (draw-tile x y x-shift y-shift)

      (incf y)
      ))
  
  (sdl:draw-surface-at-*
   (graphics-surface (eval selector))
   (car selector-graphics) (cdr selector-graphics))
  (draw-coords (car selector-tile) (cdr selector-tile) x-shift y-shift)
  (if selected-unit (draw-move-area (cons (army-x selected-unit)
					  (army-y selected-unit))
				    (army-movement selected-unit)
				    x-shift y-shift
				    selector-tile))
  )

(defun draw-at (x y x-shift y-shift graphics)
  (let* ((tile-width (car tile-size))
	 (tile-height (cdr tile-size))
	 (gx-location (* x tile-width))
	 (gy-location (* y tile-height)))
  (sdl:draw-surface-at-* (graphics-surface graphics)
			 (+ gx-location
			    x-shift (graphics-x-at graphics))
			 (+ (if (evenp x) gy-location
				(+ gy-location (/ (cdr tile-size) 2)))
			    y-shift (graphics-y-at graphics)))))

(defun draw-string-at (x y x-shift y-shift string)
  (let* ((tile-width (car tile-size))
	 (tile-height (cdr tile-size))
	 (gx-location (* x tile-width))
	 (gy-location (* y tile-height)))
    (sdl:draw-string-solid-* string
			     (+ gx-location
				x-shift
				(floor (car tile-size) 2))
			     (+ (if (evenp x) gy-location
				    (+ gy-location (/ (cdr tile-size) 2)))
				y-shift
				(floor (cdr tile-size) 2)))))

(defun draw-tile (x y x-shift y-shift)
  ;; Draw tile's basic type
  (draw-at x y x-shift y-shift (eval (tile-type (aref (world-map *world*) x y))))

  ;; Draw tile's variant list
  (dolist (variant (tile-variant (aref (world-map *world*) x y)))
    (draw-at x y x-shift y-shift (eval variant)))

  (dolist (unit (tile-units (aref (world-map *world*) x y)))
    (draw-at x y x-shift y-shift counterbase)
    (draw-at x y x-shift y-shift (army-counter unit)))
  )

(defun draw-coords (x y x-shift y-shift)
  ;;Add this to end of draw-tile to write map coords as text on tiles
  (let ((left-top-x (+ (* x (car tile-size))
		       x-shift))
	(left-top-y (+ (if (evenp x) (* y (cdr tile-size))
			   (+ (* y (cdr tile-size)) (/ (cdr tile-size) 2)))
		       y-shift)))
    (sdl:draw-string-solid-* (concatenate 'string
					  (write-to-string x) "," (write-to-string y))
			     left-top-x
			     left-top-y
			     :color sdl:*white*)))

;;Should have x and y coordinate floating free to be used: TODO REWRITE
(defmacro dotiles ((tile map &optional result-form) &body body)
  `(do* ((mapxxx ,map)
	 (x-dimxxx (array-dimension ,map 0))
	 (y-dimxxx (array-dimension ,map 1))
	 (lengthxxx (* x-dimxxx y-dimxxx))
	 (ixxx 0 (1+ ixxx))
	 (,tile nil))
	((equal ixxx lengthxxx) ,result-form)
     (setf ,tile (aref mapxxx (floor (/ ixxx y-dimxxx)) (mod ixxx y-dimxxx)))
     ,@body))


(defmacro tile-graphics-setup (tile-symbol &optional (x-offset 0) (y-offset 0))
  (let* ((symbol-string (symbol-name tile-symbol))
	 (to-conc nil))
    (cond ((search "SOUTH-WEST" symbol-string)
	   (setf to-conc "SW"))
	  ((search "SOUTH-EAST" symbol-string)
	   (setf to-conc "SE"))
	  ((search "SOUTH" symbol-string)
	   (setf to-conc "S"))
	  ((search "NORTH-EAST" symbol-string)
	   (setf to-conc "NE"))
	  ((search "NORTH-WEST" symbol-string)
	   (setf to-conc "NW"))
	  ((search "NORTH" symbol-string)
	   (setf to-conc "N")))

    `(prog1
	 (defparameter ,tile-symbol
	   (make-graphics :surface
			  (sdl-image:load-image
			   (concatenate 'string "graphics/"
					(subseq (substitute #\_ #\- ,symbol-string) 0
						(or (search "SOUTH" ,symbol-string)
						    (search "NORTH" ,symbol-string)))
					,to-conc
					".png"))
			  :x-at ,x-offset :y-at ,y-offset))
       (setf (sdl:color-key-enabled-p (graphics-surface ,tile-symbol)) t)
       (setf (sdl:color-key (graphics-surface ,tile-symbol)) (sdl:color :r 255 :g 0 :b 255)))))

(defun load-tiles ()

  (let ((color-key (sdl:color :r 255 :g 0 :b 255)))
    
    "Initializes tile graphics"
    ;; 128,104 is tile size -> 102 is distance from right point to lower left point:
    (defparameter tile-large-size '(102 . 104))

    (tile-graphics-setup sea-large)
    (tile-graphics-setup grass-large)
    (tile-graphics-setup sea-large-border-south)
    (tile-graphics-setup sea-large-border-south-west)
    (tile-graphics-setup sea-large-border-south-east)
    (tile-graphics-setup sea-large-border-north)
    (tile-graphics-setup sea-large-border-north-west)
    (tile-graphics-setup sea-large-border-north-east)

    (tile-graphics-setup swamp-large)

    (tile-graphics-setup stream-large-north-west -2 -2)
    (tile-graphics-setup stream-large-south-west -2 50)
    (tile-graphics-setup stream-large-north 24 -8)

    (tile-graphics-setup counterbase 24 7)


    ;;TODO small tiles not working
    ;;(62,52) -> 49
    (defparameter tile-small-size '(49 . 52))
    (defparameter sea-small (sdl-image:load-image "graphics/SEA_SMALL.png"))
    (setf (sdl:alpha-enabled-p sea-small) t)
    (defparameter grass-small (sdl-image:load-image "graphics/GRASS_SMALL.png"))
    (setf (sdl:alpha-enabled-p grass-small) t)

    
    (tile-graphics-setup selector-large)
    
    (defparameter selector-small (sdl-image:load-image "graphics/SELECT_SMALL.png"))
    (setf (sdl:alpha-enabled-p selector-small) t)

    (defparameter current-tile-size 'large)
    (set-tile-size current-tile-size)
    ))

(defun set-tile-size (var)
  "Switches between tile sizes"
  (sdl:clear-display sdl:*black*)

  (cond ((equal var 'large)
	 (defparameter selector selector-large)
	 (defparameter tile-size tile-large-size)
	 (defparameter sea sea-large)
	 (defparameter grass grass-large)

	 (defparameter swamp swamp-large)

	 (defparameter coast-s sea-large-border-south)
	 (defparameter coast-se sea-large-border-south-east)
	 (defparameter coast-sw sea-large-border-south-west)
	 (defparameter coast-n sea-large-border-north)
	 (defparameter coast-ne sea-large-border-north-east)
	 (defparameter coast-nw sea-large-border-north-west)

	 (defparameter stream-nw stream-large-north-west)
	 (defparameter stream-sw stream-large-south-west)
	 (defparameter stream-n stream-large-north)
	 )
	((equal var 'small)
	 (defparameter selector selector-small)
	 (defparameter tile-size tile-small-size)
	 (defparameter sea sea-small)
	 (defparameter grass grass-small))))

(defun add-river (tile-x tile-y size direction &optional (recursion t))
  (let ((tile-neighbour (neighbour-tile tile-x tile-y direction)))
    (if (eq 'sea (tile-type tile-neighbour))
	(return-from add-river)) ;; No rivers in sea
    (let ((tile (tile-at tile-x tile-y))
	  (river-symbol
	   (intern
	    (concatenate 'string
			 (string-upcase (symbol-name size)) "-"
			 (string-upcase (symbol-name direction))))))
      ;; Logic rivers
      (pushnew river-symbol
	       (tile-river-borders tile))
      ;; Graphic rivers
      (if (member direction '(N SW NW))
	  (pushnew river-symbol
		   (tile-variant tile)))

      (if recursion
	  (let ((opposing
		 (neighbour-tile-coords tile-x tile-y direction *world*)))
	    (add-river (car opposing) (cdr opposing) size
		       (cond ((eq direction 'N) 'S)
			     ((eq direction 'NE) 'SW)
			     ((eq direction 'NW) 'SE)
			     ((eq direction 'S) 'N)
			     ((eq direction 'SE) 'NW)
			     ((eq direction 'SW) 'NE))
		       nil))))))


(defun create-rivers ()
  )

(defun init-world (width height)
  (let ((world (make-world)))
    (setf (world-width world) (- width 1))
    (setf (world-height world) (- height 1))
    (setf (world-map world) (make-array `(,width ,height)))

    ;; Make totally random map:
    (do ((x 0)
	 (y 0))
	((>= x (array-dimension (world-map world) 0)))
      (setf (aref (world-map world) x y) (make-tile :type (if (< (random 4) 1) 'sea 'grass)))
      (incf y)
      (if (>= y (array-dimension (world-map world) 1))
	  (progn (incf x)
		 (setf y 0))))

    ;; Add in sea coasts to land tiles:
    (do ((x 0)
	 (y 0))
	((>= x (array-dimension (world-map world) 0)))

      (format t "~&~a,~a" x y)

      (if (not (eq (tile-type (tile-at x y world)) 'sea)) ; don't do for sea tiles
	  (dolist (direction (list 'N 'NE 'SE 'S 'SW 'NW))
	    (format t "doing list~%")
	    (let ((neighbour-tile (neighbour-tile-coords x y direction world)))
	      (format t "~&neigbour: ~a,~a" (car neighbour-tile) (cdr neighbour-tile))
	      (if neighbour-tile
		  (if (equal (tile-type (aref (world-map world) (car neighbour-tile) (cdr neighbour-tile)))
			     'sea)
		      (push (intern (concatenate 'string "COAST-" (symbol-name direction)))
			    (tile-variant (aref (world-map world) x y))))))))
	  
      

      (incf y)
      (if (>= y (array-dimension (world-map world) 1))
	  (progn (incf x)
		 (setf y 0))))

    world))

(defun tile-at (x y &optional (world *world*))
  "Returns struct tile at (x,y) or nil on failure"
  (and (>= x 0) (>= y 0)
       (< x (array-dimension (world-map world) 0))
       (< y (array-dimension (world-map world) 1))
       (aref (world-map world) x y)))

(defun neighbour-tile (here-x here-y direction &optional (world *world*))
  (let ((neighbour-coords (neighbour-tile-coords here-x here-y direction world)))
    (aref (world-map world) (car neighbour-coords) (cdr neighbour-coords))))

(defun neighbour-tile-coords (here-x here-y direction world)
  (let* ((map-width (1- (array-dimension (world-map world) 0)))
	 (map-height (1- (array-dimension (world-map world) 1)))
	 (shift (if (evenp here-x) -1 0))
	 (neighbour-x (cond ((member direction '(SW NW)) (1- here-x))
			    ((member direction '(SE NE)) (1+ here-x))
			    (t here-x)))
	 (neighbour-y (cond ((equal direction 'N) (1- here-y))
			    ((equal direction 'S) (1+ here-y))
			    ((member direction '(NE NW)) (+ here-y shift))
			    ((member direction '(SE SW)) (+ 1 here-y shift)))))

    (if (or (< neighbour-x 0) ; Check if out of bounds
	    (> neighbour-x map-width)
	    (< neighbour-y 0)
	    (> neighbour-y map-height))
	(return-from neighbour-tile-coords nil))

    ;;(aref (world-map *world*) neighbour-x neighbour-y)
    (cons neighbour-x neighbour-y)
    ))
    
(defun make-map (width height faction-count)
  (init-world width height)
  )

(defun generate-gradient ()
  (let ((radians (* (/ (random 3600) (random 3600)) pi)))
    (cons (sin radians) (cos radians))))

(defun dot-product-shift (a b)
  (dot-product b (cons (- (car a) (car b))
		       (- (cdr a) (cdr b)))))

(defun dot-product (a b)
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))
