
(in-package #:war)

(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-image)
(ql:quickload :lispbuilder-sdl-ttf)
(ql:quickload :lispbuilder-sdl-gfx)

(defvar *current-pov-faction* nil) ;; needs world to exist
(defvar *cpf-vision* (make-hash-table :test 'equal))
;;; Should be initialized later with size eq to tiles in worldmap
;;; In case of enemy passing through a faction's vision
;; -> After initial computing of vision at turn beginning
;; check logs for movement datas, compare with *cpf-vision*
;;; Note: that would mean enemy movement can't be shown in real time
;; which would be easier for multiplayer if I'll ever do that

(defvar *testunit* nil)
(defvar *current-move-area* nil)
(defvar *war-color-key* (sdl:color :r 255 :g 0 :b 255))

(defvar selected-tile nil)

;; 128,104 is tile size -> 102 is distance from right point to lower left point:
;;(defparameter tile-large-size-full (cons 128 104))
(defparameter tile-large-size-full-x 128)
(defparameter tile-large-size-full-y 104)

(defparameter tile-large-size-full-hor-x 76) ; Length of the horizontal top & bottom lines
;; above also computed by :
;;(- (* 2 (car tile-large-size))
;;   tile-large-size-full-x)
(defparameter tile-large-size (cons 102 104))

(defparameter tile-small-size-full-x 62)
(defparameter tile-small-size-full-y 52)
(defparameter tile-small-size-full-hor-x 36)
(defparameter tile-small-size (cons 49 52))

(defparameter tile-size-x (car tile-large-size))
(defparameter tile-size-y (cdr tile-large-size))
(defparameter tile-size-hor-x tile-large-size-full-hor-x) ; these should be set in the init func

(defun set-test-enemy-unit ()
  (defvar *enemy-units* nil)
  (let ((x (random (1+ (world-width *world*))))
	(y (random (1+ (world-height *world*))))
	(faction (car (member-if-not
		       #'(lambda (faction) (string= (faction-name faction) "Free France"))
		       (world-factions *world*)))))
    (new-army faction x y (let ((seed (random 8)))
			    (list (prog1 (cond ((eq seed 0) 'air)
					       ((eq seed 1) 'space)
					       ((eq seed 2) 'land)
					       ((eq seed 3) 'surface)
					       ((eq seed 4) 'subsurface)
					       ((eq seed 5) 'equipment)
					       ((eq seed 6) 'installation)
					       ((eq seed 7) 'activity))
				    (setf seed (random 6)))
				  (prog1 (cond ((eq seed 0) 'air-assault-with-organic-lift)
					       ((eq seed 1) 'air-defense)
					       ((eq seed 2) 'amphibious)
					       ((eq seed 3) 'analysis)
					       ((eq seed 4) 'antitank)
					       ((eq seed 5) 'broadcast-transmitter-antenna))))))))

(defun set-test-unit ()
  (format t "~%Setting up testunit~&")
  (cond (t ;;if t -> set to create new armies at (10,8) everytime (test) runs
	 ;;(null *testunit*) ;; no more units created
	 (setf *testunit*
	       (cons (new-army *current-pov-faction* 10 8
			       (let ((seed (random 8)))
				 (list (prog1 (cond ((eq seed 0) 'air)
						    ((eq seed 1) 'space)
						    ((eq seed 2) 'land)
						    ((eq seed 3) 'surface)
						    ((eq seed 4) 'subsurface)
						    ((eq seed 5) 'equipment)
						    ((eq seed 6) 'installation)
						    ((eq seed 7) 'activity))
					 (setf seed (random 6)))
				       (prog1 (cond ((eq seed 0) 'air-assault-with-organic-lift)
						    ((eq seed 1) 'air-defense)
						    ((eq seed 2) 'amphibious)
						    ((eq seed 3) 'analysis)
						    ((eq seed 4) 'antitank)
						    ((eq seed 5) 'broadcast-transmitter-antenna))))))
		     *testunit*)))))

(defstruct graphics
  (surface nil)
  (x-at 0)  ; modifiers for drawing
  (y-at 0)
  (priority 0)  ; Higher priorities should be moved towards tile lists' ends
  )

(defvar *world* nil)

(defmacro do-world-tiles ((var &optional (world *world*)) &body body)
  (let ((x (gensym))
	(y (gensym)))
    `(dotimes (,x (+ 1 (world-width ,world)))
       (dotimes (,y (+ 1 (world-height ,world)))
	 (let ((,var (tile-at ,x ,y)))
	   ,@body)))))

(defmacro docoords ((x y &optional (world *world*)) &body body)
  `(dotimes (,x (1+ (world-width ,world)))
     (dotimes (,y (1+ (world-height ,world)))
       ,@body)))

(defun sort-world-graphics (&optional (world *world*))
  (do-world-tiles (tile world)
    (sort-tile-graphics tile)))

(defun init-test (height width &key (algo 'testing) (islands 1) (mirror nil))
  (setf *world* (init-world height width :algo algo :mirror mirror :islands islands))
  nil)

(defun cursor-coordinates-on-map (screen-x screen-y x-shift y-shift)
  "What tile is the user hovering mouse over?"
  (let* ((half-height (/ tile-size-y 2)) ; The y coordinate of imaginary left border lines
	 (absolute-x (- screen-x x-shift)) ; Cursor's absolute x coordinate from (0,?) in pixels
	 (in-tile-x (rem absolute-x tile-size-x)) ; Cursor's x coordinate within real cut tile in pix
	 (tile-x (floor (/ absolute-x tile-size-x))) ; Preliminary tile's x coordinate
	 (absolute-y (if (evenp tile-x) ; Cursor's absolute y coordinate from (tile-x,0) in pixels
			 (- screen-y y-shift)
			 (- (- screen-y y-shift) (/ tile-size-y 2))))
	 (in-tile-y (rem absolute-y tile-size-y)) ; Cursor's y within tile in pixels

	 ;;;   (v1.x - v0.x)*(v2.y - v0.y) - (v2.x - v0.x)*(v1.y - v0.y)
	 ;;      => Will return positive if v2 to the left of line (v0-v1)
	 ;; v0 = NW ; v1 = W :
	 (left-of-nw (- (* (- tile-size-x tile-size-hor-x)
			   (- in-tile-y half-height))
			(* in-tile-x
			   (- half-height))))
	 ;; v0 = W ; v1 = SW :
	 (left-of-sw (- (* (- (- tile-size-x tile-size-hor-x))
			   (- in-tile-y tile-size-y))
			(* (- in-tile-x (- tile-size-x tile-size-hor-x))
			   (- half-height tile-size-y))))
	 
	 (tile-y (floor (/ absolute-y tile-size-y)))) ; Preliminary y coordinate

    (cond ((<= left-of-nw 0) ; cursor actually over tile at NW
	   (if (evenp tile-x) (decf tile-y)) ; problems of hex stacking
	   (decf tile-x))
	  ((<= left-of-sw 0) ; cursor actually over tile at SW
	   (if (oddp tile-x) (incf tile-y))
	   (decf tile-x)))
    
    (cons tile-x tile-y)))

(defun cursor-coordinates-on-screen (screen-x screen-y x-shift y-shift tile-x)
  "Returns as (x . y) the coordinates to draw a selection cursor to screen"
  (let* (
	 ;;There are a bunch of hexes on screen. Which one are we pointing at?
	 ;;used for multiplying pixel coordinates
	 (adj-tile-x (floor (/ (- screen-x (rem x-shift tile-size-x)) (car tile-size))))
	 (adj-tile-y (floor (/ (if (evenp tile-x)
				   (- screen-y (rem y-shift tile-size-y))
				   (- (- screen-y (rem y-shift tile-size-y))
				      (/ tile-size-y 2)))
			       tile-size-y)))
	 ;;The x coordinate for graphics:
	 (adj-screen-x (+ (* adj-tile-x tile-size-x)
			  (rem x-shift tile-size-x)))
	 ;;y coordinate graphics need to be moved downwards by tile-size-y / 2 if x is odd
	 (adj-screen-y (if (evenp tile-x)
			   (+ (* adj-tile-y tile-size-y)
			      (rem y-shift tile-size-y))
			   (+ (* adj-tile-y tile-size-y)
			      (rem y-shift tile-size-y)
			      (floor (/ tile-size-y 2)))))
	 )
    (cons adj-screen-x adj-screen-y)))




(defun test ()
  (sdl:with-init()
    (defparameter window (sdl:window 1500 900 :title-caption "a war game"))
    (setf (sdl:frame-rate) 60)
    
    (sdl:initialise-default-font)

    (grand-unified-graphics-setup
     :full
     '((sea :large (100 -4 -9) :small (0 0 0))
       (grass :large (0 0 0) :small (0 0 0))
       (field :large (25 -4 -9) :small (25 0 0) :overrides (:outskirts-everywhere))
       (forest :large (75 -4 -17) :small (75 0 0))
       (city :large (50 -6 -6) :small (50 0 0))
       (suburb :large (50 0 0) :small (50 0 0))
       (swamp :large (1 0 0) :small (1 0 0))
       (mountain :large (80 -5 -8) :small (80 0 0) :overrides (:outskirts-everywhere))
       (hill :large (45 -9 -3) :small (80 0 0) :overrides (:outskirts-everywhere)))

     :border '((stream
		:large (:north (55 24 -8)
			:north-west (55 -2 -2)
			:south-west (55 -2 50))
		:small (:north (10 20 30) ;; DUMMY data
			:north-west (40 50 60)
			:south-west (70 80 90)))
	       (river
		:large (:north (55 26 -6)
			:north-west (55 -2 -2)
			:south-west (55 -2 50))
		:small (:north (10 20 30) ;; DUMMY data
			:north-west (40 50 60)
			:south-west (70 80 90)))
	       (rail
		:large (:north (60 44 -53)
			:north-west (60 -40 -1)
			:south-west (60 -39 50))
		:small (:north (60 1 1)
			:north-west (60 1 1)
			:south-west (60 1 1))))

     :misc
     '((selector :large (200 11 0) :small (200 5 0))
       (missing :large (300 0 0) :small (300 0 0)))
     )

    (set-tile-size 'small)
    (set-tile-size 'large)

    (unless *world*
      (init-test 40 40 :algo 'smooth :islands 20 :mirror t)
      (setf *current-pov-faction* (create-faction "Free France" :controller 'local :world *world*))
      (create-faction "Martians" :controller 'none :world *world*)
      )
    ;; init-test can't be executed before variant and outskirts have been generated in grand-unified-graphics-setup

    (init-cgen)

    (sort-world-graphics) ;; Put graphics in order to render correctly. NOTE: This is a pretty heavy operation
    (setup-panels) ;; Setup the chrome

    ;;(set-test-unit) ;; testing army graphics

    (let ((x-shift 0) (y-shift 0)
	  (selector-tile '(0 . 0)) (selector-graphics '(0 . 0))
	  ;;(selected-tile nil)
	  (selected-graphics nil)
	  (selected-unit nil)
	  (gui-state-changed t)) ;; Set to t when there is something new to draw

	  (sdl:with-events ()
	    (:quit-event () t)
	    (:key-down-event (:key keyb :mod keyb-mod)
			     (setf gui-state-changed t) ;; testing
			     (format t "~&Key: ~a, mod: ~a~%" keyb keyb-mod)
			     (case keyb ((:sdl-key-escape) (setf selected-unit nil))))

	    (:mouse-motion-event (:x x :y y)
				 (let ((old-selector-tile selector-tile))
				   (setf selector-tile
					 (cursor-coordinates-on-map
					  x y x-shift y-shift))
				   (unless (equal old-selector-tile
						  selector-tile)
				     (setf gui-state-changed t)
				     )
				   (setf selector-graphics 
					 (cursor-coordinates-on-screen
					  x y x-shift y-shift (car selector-tile)))
				   
				   ))
	    
	    (:mouse-button-down-event
	     (:button button :state state :x x :y y)
	     (setf gui-state-changed t)
	     (cond ((equal 'world (mouse-over-what x y))
		    (cond ((equal button sdl:sdl-button-right)
			   (sdl:clear-display sdl:*black*)
			   (setf x-shift (- x-shift (- x (floor (/ (sdl:width window) 2)))))
			   (setf y-shift (- y-shift (- y (floor (/ (sdl:height window) 2))))))
			  
			  ((and (equal button sdl:sdl-button-left)
			        (coord-in-bounds selector-tile)) ; Avoid invalid clicks
			   (setf selected-tile selector-tile)
			   (setf selected-graphics selector-graphics)
			   (format t "~&Selected ~a~%" selected-tile)
			   
			   (cond ((null selected-unit) ; Nothing selected -> try to select unit from tile:
				  (setf selected-unit (car (tile-units ;; take the first unit from list
							    (tile-at (car selected-tile)
								     (cdr selected-tile))))))
				 
				 ((member selected-unit ; Previously selected unit in tile -> select next (or first) unit
					  (tile-units (tile-at (car selected-tile)
							       (cdr selected-tile))))
				  (setf selected-unit
					(or (cadr
					     (member selected-unit ; Should be LETted but whatcha gonna do
						     (tile-units (tile-at (car selected-tile)
									  (cdr selected-tile)))))
					    (car (tile-units (tile-at (car selected-tile)
								      (cdr selected-tile)))))))
				 
				 ((gethash selected-tile *current-move-area*) ; Clicked tile inside current unit's move area
				  (place-unit selected-unit
					      (car selected-tile)
					      (cdr selected-tile))
				  ;; TODO: I'd say think more on this & make some kind of superfunction
				  ;; that takes everything into account
				  ;; clear pov-faction's vision hashtable and recompute:
				  (clrhash *cpf-vision*)
				  (dolist (army (faction-armies *current-pov-faction*))
				    ;; Currently just choose the highest vision percentage generated
				    (maphash #'(lambda (coord percentage)
						 (let ((old-vision (gethash coord *cpf-vision*)))
						   (cond ((and old-vision (< old-vision percentage))
							  (setf (gethash coord *cpf-vision*) percentage))
							 ((null old-vision)
							  (setf (gethash coord *cpf-vision*) percentage)))))
					     (visible-area ; Work in progress
					      army
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
							(* (or (gethash parent-2 visibles) 0) (/ p2-weight total-weight))))
						    ))))
				    ))))

			  ((equal button sdl:sdl-button-wheel-up)
			   (set-tile-size 'large)
			   (let ((new-focus (focus-on-tile selector-tile
							   (sdl:width window)
							   (sdl:height window))))
			     (setf x-shift (car new-focus)
				   y-shift (cdr new-focus))
			     )
			   
			   )
			  ((equal button sdl:sdl-button-wheel-down)
			   (set-tile-size 'small)
			   (let ((new-focus (focus-on-tile selector-tile
							   (sdl:width window)
							   (sdl:height window))))
			     (setf x-shift (car new-focus)
				   y-shift (cdr new-focus))
			     )
			   )))
		   ((equal 'panel (mouse-over-what x y))
		    (click-panel button state x y)
		    ;;(setf selected-unit (select-from-panel y (tile-at
			;;				      (car selected-tile)
			;;				      (cdr selected-tile))))
		    )))

	    (:idle ()

		   (when gui-state-changed
		     (draw-world x-shift y-shift
				 selector-graphics selector-tile
				 selected-tile selected-unit)
		     ;;(draw-panel selected-tile selected-unit)
		     (draw-panels)
		     (sdl:update-display))
		   (setf gui-state-changed nil)
		   )))))

(defun focus-on-tile (coord-pair view-width view-height)
  "Return coordinate pair to use as new graphics shifts"
  (cons
   (- 0 (* (car coord-pair) tile-size-x)
      (- (floor tile-size-full-x 2) (floor view-width 2))) ; center tile
   (- 0 (* (cdr coord-pair) tile-size-y)
      (if (oddp (car coord-pair)) ; shift a little if x is odd
	  (floor tile-size-y 2)
	  0)
      (- (floor tile-size-y 2) (floor view-height 2))))) ; center tile

(defun mouse-over-what (mouse-x mouse-y)
  (if (>= mouse-x (- (sdl:width window) *panel-width*))
      'panel
      'world))

(defun tc-gc (tile-coords x-shift y-shift) ;; Tile Coordinate to Graphics Coordinate
  (if (null (car tile-coords)) (return-from tc-gc nil))
  (let* ((x (car tile-coords))
	 (y (cdr tile-coords))
	 (tile-width tile-size-x)
	 (tile-height tile-size-y)
	 (gx-location (* x tile-width))
	 (gy-location (* y tile-height)))
    (cons (+ gx-location
	     x-shift)
	  (+ (if (evenp x) gy-location
		 (+ gy-location (floor tile-height 2)))
	     y-shift))))


(defun draw-move-area (army x-shift y-shift end)
  (let* ((start (cons (army-x army) (army-y army)))
	 (move-range (army-movement army))
	 (move-area (move-area army)))
    (labels ((draw-path (current)
	       (let* ((next (cdr (gethash current move-area)))
		     (current-g (tc-gc current
				       x-shift y-shift))
		     (next-g (tc-gc next
				    x-shift y-shift)))
		 (cond ((car next)
			(sdl:draw-line-* (+ (car current-g) (floor tile-size-x 2))
					 (+ (cdr current-g) (floor tile-size-y 2))
					 (+ (car next-g) (floor tile-size-x 2))
					 (+ (cdr next-g) (floor tile-size-y 2))
					 :color sdl:*white*)
			(draw-path next))))))

      (maphash #'(lambda (key value)
		   (draw-string-at (car key) (cdr key)
				   x-shift y-shift
				   (write-to-string (car value))))
	       move-area)

      (if end
	  (draw-path end)))))



(defun OBSOLETEmove-area (start move-range)
  ;; return currently the came-from hash table with (cons x1 y1) as key
  ;;   value is (movement-left-after-this-move (cons x0 y0))
  
;;????? SHOULD BE FIXED NOW with declare specials
  
;;   check with this:
;;    (maphash #'(lambda (key value)
;;	 (format t "~&~a :: ~a~%" key value)) xxx)
  
  (let ((sea 100) ; temporary shadows for move costs
	(grass 2)
	(city 10)
	(field 3)
	(hill 5)
	(mountain 10)

	(frontier (make-heap))
	(came-from (make-hash-table :test 'equal)))
    
    (declare (special sea) (special mountain) (special hill)
	     (special grass) (special city) (special field))
    
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
					  direction
					  (world-width *world*)
					  (world-height *world*)))
				     '(n ne se s sw nw)))
	    
	    (cond ((null neighbour) nil)
		  ((null (gethash neighbour came-from))
		   (let ((move-cost (- (car current)
				       (eval
					(or
					 (caar ; TODO: should probably check whose locs we pass over
					  (last
					   (tile-location (tile-at (car neighbour)
								   (cdr neighbour)))))
					 (car (last
					       (tile-type (aref (world-map *world*)
								(car neighbour)
								(cdr neighbour))))))))))
		     (cond ((>= move-cost 0)
			    (heap-insert frontier neighbour move-cost)
			    (setf (gethash neighbour came-from)
				  (cons move-cost (cdr current)))))))))))
    (setf *current-move-area* came-from)))

;;TODO: very WIP. Doesn't do aynthing useful yet.
;; Vision precentage writing should depend on some option somewhere, also change color?
(defun draw-vision (x0 y0 xn yn x-shift y-shift)
  (maphash
   #'(lambda (coord vision)
       (let ((cx (car coord))
	     (cy (cdr coord)))
	 (when (and (<= x0 cx xn) ; Currently only draw vision percentage
		    (<= y0 cy yn))
	   (draw-string-at cx cy x-shift (+ y-shift 10) (write-to-string (round (* vision 100)))
			   :color sdl:*blue*))))
   *cpf-vision*))

(defun draw-world (x-shift y-shift selector-graphics selector-tile selected-tile selected-unit)

  (let* (;;(draw-count 0)
	 (x-start-void (floor x-shift tile-size-x))
	 (x-start (if (>= (+ x-shift tile-size-x) 0) 0
		      (- (abs x-start-void) 2)))
	 (x-end (min
		 (+ (- x-start-void) (floor (sdl:width window) tile-size-x))
		 (1- (array-dimension (world-map *world*) 0)))) ;; The last column
	 (y-start-void (floor y-shift tile-size-y))
	 (y-start (min (if (>= (+ y-shift tile-size-y) 0) 0
			   (- (abs y-start-void) 2))
		       (1- (array-dimension (world-map *world*) 1))))
	 (y-end (min
		 (+ (- y-start-void) (floor (sdl:height window) tile-size-y))
		 (1- (array-dimension (world-map *world*) 1))))) ;; The last row

    (macrolet ((draw-tiles-by-slot (accessor &optional (sub-accessor nil))
		 (let ((x (gensym))
		       (y (gensym))
		       (slot (gensym)))
		   `(do ((,x x-start)
			 (,y y-start))
			(nil)

		      (cond ((> ,y y-end)
			     (setf ,y y-start)
			     (incf ,x)))
		      (if (> ,x x-end)
			  (return))

		      (dolist (,slot (,accessor (aref (world-map *world*) ,x ,y)))


		      
			(draw-at ,x ,y x-shift y-shift
				 ,(if sub-accessor
				      `(,sub-accessor ,slot)
				      `(symbol-value ,slot))))
		      (incf ,y)))))

      ;;(draw-tiles-by-slot tile-type) ; types moved to variants list
      (draw-tiles-by-slot tile-variant)
      (draw-tiles-by-slot tile-units army-counter) ; TODO: this macro won't cut it anymore. use fun SEEN to pick
      (draw-vision x-start y-start x-end y-end x-shift y-shift)
      ))

  (if (coord-in-bounds selector-tile) ; Avoid drawing selector & coords if mouse pointer outside world
      (progn
	(draw-at (car selector-tile) (cdr selector-tile)
		 x-shift y-shift selector)
	(draw-coords (car selector-tile) (cdr selector-tile) x-shift y-shift)))
  
  (if selected-unit (draw-move-area
		     ;;(cons (army-x selected-unit)(army-y selected-unit))(army-movement selected-unit)
		     selected-unit
		     x-shift y-shift
		     selector-tile)))


(defun draw-at (x y x-shift y-shift graphics &optional (destination sdl:*default-surface*))
  (when (null graphics)
    (return-from draw-at))
  (let* ((tile-width tile-size-x)
	 (tile-height tile-size-y)
	 (gx-location (* x tile-width))
	 (gy-location (* y tile-height)))
  (sdl:draw-surface-at-* (graphics-surface graphics)
			 (+ gx-location
			    x-shift (graphics-x-at graphics))
			 (+ (if (evenp x) gy-location
				(+ gy-location (/ tile-size-y 2)))
			    y-shift (graphics-y-at graphics))
			 :surface destination)))

(defun draw-string-at (x y x-shift y-shift string &key (color sdl:*black*))
  (let* ((tile-width tile-size-x)
	 (tile-height tile-size-y)
	 (gx-location (* x tile-width))
	 (gy-location (* y tile-height)))
    (sdl:draw-string-solid-* string
			     (+ gx-location
				x-shift
				(floor tile-size-x 2))
			     (+ (if (evenp x) gy-location
				    (+ gy-location (/ tile-size-y 2)))
				y-shift
				(floor tile-size-y 2))
			     :color color)))

(defun draw-coords (x y x-shift y-shift)
  ;;Add this to end of draw-tile to write map coords as text on tiles
  (let ((left-top-x (+ (* x tile-size-x)
		       x-shift))
	(left-top-y (+ (if (evenp x) (* y tile-size-y)
			   (+ (* y tile-size-y) (/ (cdr tile-size) 2)))
		       y-shift)))
    (sdl:draw-string-solid-* (concatenate 'string
					  (write-to-string x) "," (write-to-string y))
			     left-top-x
			     left-top-y
			     :color sdl:*white*)))

;; TODO: Rewrite tile-graphics-setup as func below, move to graphicssetup.lisp
(defun tile-graphics-setup (tile-symbol priority &optional (x-offset 0) (y-offset 0))
  (let* ((size-of-tile (cond ((search "LARGE" (symbol-name tile-symbol)) 'large)
			     ((search "SMALL" (symbol-name tile-symbol)) 'small)))
	 (tile-dims (case size-of-tile
		      ((large) (list tile-large-size-full-hor-x tile-large-size-full-x tile-large-size-full-y))
		      ((small) (list tile-small-size-full-hor-x tile-small-size-full-x tile-small-size-full-y))))
	 (symbol-string (substitute #\_ #\- (symbol-name tile-symbol)))
	 (graphics-path (concatenate 'string "./graphics/" symbol-string ".png")))
    (if (null (probe-file graphics-path))
	(case size-of-tile
	  ;; Can't be helped:
	  ((large) (eval `(defparameter ,tile-symbol missing-large)))
	  ((small) (eval `(defparameter ,tile-symbol missing-small))))
	;;else
	(let ((graphics-list
	       (mapcar #'(lambda (direction graphics)
			   (let ((current-symbol (if direction
						     (intern (concatenate 'string (string tile-symbol) direction))
						     tile-symbol))) ; direction is nil: this is the central tile
			     ;; Free surface if we have already bound the symbol on a previous run:
			     ;; TODO: Ideally use a checksum on surfaces or preferably the whole unchopped image
			     (when (boundp current-symbol)
			       (sdl:free (graphics-surface (symbol-value current-symbol))))
			     ;; Did chop-tile actually produce anything?
			     (when graphics 
			       (eval `(defparameter ,current-symbol
					,graphics)))
			     ))
		       
		       '(nil "-BORDER-NORTH" "-BORDER-NORTH-EAST" "-BORDER-SOUTH-EAST"
			 "-BORDER-SOUTH" "-BORDER-SOUTH-WEST" "-BORDER-NORTH-WEST")
		       (chop-tile graphics-path x-offset y-offset
				  (car tile-dims) (cadr tile-dims) (caddr tile-dims)))))
	  (dolist (graphics graphics-list)
	    (when graphics (setf (graphics-priority (symbol-value graphics)) priority)))
	  
	  ))))


(defmacro create-tile (base-name &key (large nil) (small nil)) ; <- large & small in form (priority x-ofs y-ofs)
  `(progn
     ;;(format t "~&Creating ~a ~a~%" ',large ',small)
     ,(if large
	 `(tile-graphics-setup ,(intern (concatenate 'string (symbol-name base-name) "-LARGE"))
			       ,(car large) ,(cadr large) ,(caddr large)))
     ,(if small
	 `(tile-graphics-setup ,(intern (concatenate 'string (symbol-name base-name) "-SMALL"))
			       ,(car small) ,(cadr small) ,(caddr small)))))



(defun cross-border-graphics-setup (symbol size priority x-offset y-offset)
  "Setup for unchopped graphics that are to be set over tile-borders. Roads and rivers etc..
Creates symbol with name like STREAM-NW-A-LARGE if appropriate file is found."
  (let* ((symbol-name (concatenate 'string
				   (symbol-name symbol) "-"
				   (symbol-name size)))
	 (final-symbol (intern symbol-name))
	 (graphics-file
	  (concatenate 'string "./graphics/" (substitute #\_ #\- symbol-name) ".png")))

    (when (probe-file graphics-file)
      (eval
       `(defvar ,final-symbol
	  (make-graphics
	   :surface (sdl-image:load-image ,graphics-file)
	   :x-at ,x-offset :y-at ,y-offset :priority ,priority)))

      (setf (sdl:color-key-enabled-p (graphics-surface (symbol-value final-symbol))) t)
      (setf (sdl:color-key (graphics-surface (symbol-value final-symbol))) *war-color-key*))))


'(defun OBSOLETEload-tiles ()
  ;;(tile-graphics-setup sea-large 100 -4 -9)
  (create-tile sea :large (100 -4 -9) :small (0 0 0))
  (create-tile grass :large (0 0 0) :small (0 0 0))
  
  ;(tile-graphics-setup grass-large 0)

  (tile-graphics-setup swamp-large 1)
  (tile-graphics-setup city-a-large 50 -6 -5)
  (tile-graphics-setup suburb-a-large 50)

  (tile-graphics-setup field-a-large 25 -4 -9)
  (tile-graphics-setup field-b-large 25 0 0)
  (tile-graphics-setup field-c-large 25 0 0)
  (pushnew '(field field-a field-b field-c) *graphics-variants*)

  (tile-graphics-setup forest-a-large 75 -4 -17)

  (tile-graphics-setup selector-large 200 11 0)

  (tile-river-setup stream-large-north-west -2 -2)
  (tile-river-setup stream-large-south-west -2 50)
  (tile-river-setup stream-large-north 24 -8)

  

  ;;(tile-graphics-setup sea-small 0)
  ;(tile-graphics-setup grass-small 0)
  (tile-graphics-setup selector-small 200 5 0)

  (set-tile-size 'large)
  )

'(defun OBSOLETEset-tile-size (var)
  "Switches between tile sizes"
  (sdl:clear-display sdl:*black*)

  (cond ((equal var 'large)

	 (defparameter tile-size-x (car tile-large-size))
	 (defparameter tile-size-y (cdr tile-large-size))
	 (defparameter tile-size-full-x tile-large-size-full-x)
	 (defparameter tile-size-hor-x tile-large-size-full-hor-x)
	 
	 (defparameter selector selector-large)
	 (defparameter tile-size tile-large-size)
	 (defparameter sea sea-large)
	 (defparameter grass grass-large)

	 (defparameter swamp swamp-large)
	 (defparameter city-a city-a-large)
	 (defparameter suburb-a suburb-a-large)

	 (defparameter field-a field-a-large)
	 (defparameter field-b field-b-large)
	 (defparameter field-c field-c-large)

	 (defparameter forest-a forest-a-large)

	 (defparameter coast-s sea-large-border-south)
	 (defparameter coast-se sea-large-border-south-east)
	 (defparameter coast-sw sea-large-border-south-west)
	 (defparameter coast-n sea-large-border-north)
	 (defparameter coast-ne sea-large-border-north-east)
	 (defparameter coast-nw sea-large-border-north-west)

	 (defparameter city-outskirts-s city-a-large-border-south)
	 (defparameter city-outskirts-se city-a-large-border-south-east)
	 (defparameter city-outskirts-sw city-a-large-border-south-west)
	 (defparameter city-outskirts-n city-a-large-border-north)
	 (defparameter city-outskirts-ne city-a-large-border-north-east)
	 (defparameter city-outskirts-nw city-a-large-border-north-west)

	 (defparameter field-outskirts-s field-a-large-border-south)
	 (defparameter field-outskirts-se field-a-large-border-south-east)
	 ;;(defparameter field-outskirts-sw field-large-border-south-west)
	 (defparameter field-outskirts-n field-a-large-border-north)
	 (defparameter field-outskirts-ne field-a-large-border-north-east)
	 ;;(defparameter field-outskirts-nw field-a-large-border-north-west)

	 (defparameter forest-outskirts-s forest-a-large-border-south)
	 (defparameter forest-outskirts-se forest-a-large-border-south-east)
	 (defparameter forest-outskirts-sw forest-a-large-border-south-west)
	 (defparameter forest-outskirts-n forest-a-large-border-north)
	 (defparameter forest-outskirts-ne forest-a-large-border-north-east)
	 (defparameter forest-outskirts-nw forest-a-large-border-north-west)

	 (defparameter stream-nw stream-large-north-west)
	 (defparameter stream-sw stream-large-south-west)
	 (defparameter stream-n stream-large-north)
	 )
	((equal var 'small)

	 (defparameter tile-size-x (car tile-small-size))
	 (defparameter tile-size-y (cdr tile-small-size))
	 (defparameter tile-size-hor-x tile-small-size-full-hor-x)
	 (defparameter tile-size-full-x tile-small-size-full-x)
	 
	 (defparameter selector selector-small)
	 (defparameter tile-size tile-small-size)
	 (defparameter sea sea-small)
	 (defparameter grass grass-small)

	 ;; TODO: this should be done in the setup-tile-graphics macro
	 ;; -creates empty sdlsurface for nonexistent graphics
	 (defvar placeholder (make-graphics :surface (sdl:create-surface 0 0)))

	 (defparameter coast-s placeholder)
	 (defparameter coast-se placeholder)
	 (defparameter coast-sw placeholder)
	 (defparameter coast-n placeholder)
	 (defparameter coast-ne placeholder)
	 (defparameter coast-nw placeholder)
	 )))

(defun tile-at (x y &optional (world *world*))
  "Returns struct tile at (x,y) or nil on failure"
  (and x y (>= x 0) (>= y 0)
       (< x (array-dimension (world-map world) 0))
       (< y (array-dimension (world-map world) 1))
       (aref (world-map world) x y)))

(defun set-tile-at (x y world new-tile)
  (and x y (>= x 0) (>= y 0)
       (< x (array-dimension (world-map world) 0))
       (< y (array-dimension (world-map world) 1))
       (setf (aref (world-map world) x y) new-tile)
       (aref (world-map world) x y)))

(defsetf tile-at (x y &optional (world *world*)) (new-tile)
  `(set-tile-at ,x ,y ,(if world world *world*) ,new-tile))

(defun neighbour-tile (here-x here-y direction &optional (world *world*))
  (let ((neighbour-coords (neighbour-tile-coords here-x here-y direction
						 (world-width world)
						 (world-height world))))
    (when neighbour-coords (tile-at (car neighbour-coords) (cdr neighbour-coords) world))))

(defun neighbour-tile-coords (here-x here-y direction
			      &optional (max-x (world-width *world*)) (max-y (world-height *world*))
				(min-x 0) (min-y 0))
  "Returns coordinate cons of here's neighbour towards direction, maxes refer to index bounds."
  (let* ((shift (if (evenp here-x) -1 0))
	 (neighbour-x (cond ((member direction '(SW NW)) (1- here-x))
			    ((member direction '(SE NE)) (1+ here-x))
			    (t here-x)))
	 (neighbour-y (cond ((equal direction 'N) (1- here-y))
			    ((equal direction 'S) (1+ here-y))
			    ((member direction '(NE NW)) (+ here-y shift))
			    ((member direction '(SE SW)) (+ 1 here-y shift)))))

    (if (or (< neighbour-x min-x) ; Check if out of bounds
	    (> neighbour-x max-x)
	    (< neighbour-y min-y)
	    (> neighbour-y max-y))
	(return-from neighbour-tile-coords nil))

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
