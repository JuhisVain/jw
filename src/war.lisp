;;;; war.lisp

;;(in-package #:war)

(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-image)
(ql:quickload :lispbuilder-sdl-ttf)
(ql:quickload :lispbuilder-sdl-gfx)

(defstruct world
  (width nil)     ;amount of columns
  (height nil)    ;hexes in a column
  ;;If wrapping required on x-axis, column count must be even 
  (map nil)       ;a 2d array
  (factions nil)  ;list of faction structs
  )

(defstruct faction
  (units nil)
  (techs nil)
  )

(defstruct tile
  (type 'sea)
  (variant nil)  ;to be used as variant graphics for coast lines etc.
  (location nil) ;city/resource/airfield etc.
  (river-borders nil)
  (road-links nil)
  (rail-links nil)
  (units nil)
  (temp nil))

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

    (let ((x-shift 0) (y-shift 0)
	  (selector-tile '(0 . 0)) (selector-graphics '(0 . 0))
	  (selected-tile nil) (selected-graphics nil))

	  (sdl:with-events ()
	    (:quit-event () t)
	    (:key-down-event ()
			     (sdl:push-quit-event))

	    (:mouse-motion-event (:x x :y y)
				 (setf selector-tile
				       (cursor-coordinates-on-map
					x y x-shift y-shift))
				 (setf selector-graphics 
				       (cursor-coordinates-on-screen
					x y x-shift y-shift (car selector-tile))))
	    
	    (:mouse-button-down-event
	     (:button button :x x :y y)
	     (cond ((equal button sdl:sdl-button-right)
		    (sdl:clear-display sdl:*black*)
		    (setf x-shift (- x-shift (- x (floor (/ (sdl:width window) 2)))))
		    (setf y-shift (- y-shift (- y (floor (/ (sdl:height window) 2)))))

		    (setf selector-tile
			  (cursor-coordinates-on-map
			   x y x-shift y-shift))
		    (setf selector-graphics 
			  (cursor-coordinates-on-screen
			   x y x-shift y-shift (car selector-tile))))
		   
		   ((equal button sdl:sdl-button-left)
		    (setf selected-tile selector-tile)
		    (setf selected-graphics selector-graphics)
		    (format t "~&Selected ~a~%" selected-tile))

		   ((equal button sdl:sdl-button-wheel-up)
		    (set-tile-size 'large))
		   ((equal button sdl:sdl-button-wheel-down)
		    (set-tile-size 'small)))
	     )


	    
	    (:idle ()

		   (draw-world x-shift y-shift selector-graphics selector-tile)
		   (sdl:update-display)
		   )))))


(defun compute-path-2 (start end)
  ;; neighbour-tile-coords (here-x here-y direction world)
  (do ((shortest-path (list (cons start 0)))  ;( ((x . y) . 0) )
       (best-end nil)
       (end-counter 0))
      ((> end-counter 50) shortest-path)

    (dolist (tile-distance shortest-path)     ;  ((x . y) . w) iterate on elements in list
      (dolist (coordinate                     ;  (x . y) * 6  iterate on generated neighbours
		
		(mapcar #'(lambda (x)
			    (neighbour-tile-coords (caar tile-distance)
						   (cdar tile-distance)
						   x *world*))
			'(n ne se s sw nw)))

	(and coordinate ; if coordinate not nil, aka. outside map
	     (not best-end) ; this function overall is not best

	     (let* ((tile-struct (aref (world-map *world*)
				       (car coordinate)
				       (cdr coordinate)))

		    ;; This is most likely a problem:
		    (old-tile (car (member coordinate shortest-path
					   :test #'equal :key #'car)))

		    (move-cost (cond ((eq (tile-type tile-struct) 'sea) 50)
				     ((eq (tile-type tile-struct) 'grass) 3))))


	       (cond (old-tile
		      (if (equal (car old-tile) end)
			  (progn 
			    (format t "~&Found end with pot distance: ~a, old was ~a~%"
				    (+ (cdr tile-distance) move-cost) (cdr old-tile))
			    (incf end-counter)))
		      (setf (cdar (member coordinate shortest-path
					  :test #'equal :key #'car))
			    (min (cdr old-tile) (+ (cdr tile-distance) move-cost))))
		     (t
		      (nconc shortest-path (list (cons coordinate (+ (cdr tile-distance) move-cost))))
		      (if (and (equal coordinate end) (zerop end-counter)) (setf end-counter 1))
		      ))))))
   shortest-path))

(defun compute-path (start-x start-y end-x end-y)
  ;; tile-temp to hold  (:pathfinder (distance (start-x start-y end-x end-y)))
  ;;                      ^what for    ^small is good   ^id

  (let ((old-tile-temp
	 (getf (tile-temp (aref (world-map *world*) x y)) :pathfinder)))
    (if (equal (cdar old-tile-temp)
	       (list start-x start-y end-x end-y))
	(setf (car (getf (tile-temp (aref (world-map *world*) x y)) :pathfinder))
	      (min (car old-tile-temp)
		   (   ))))))


(defun min-map (hashmap)
  (format t "~&minmap called~%")
  (let ((lowest-key nil)
	(lowest-value 999999))
    (format t "~&maphashing~%")
    (maphash #'(lambda (key value)
		 (format t "~&Checking key:~a value:~a~%" key value)
		 (cond ((< (cdr value) lowest-value)
			(setf lowest-key key)
			(setf lowest-value (cdr value)))))
	     hashmap)
    (format t "~&returning: ~a~%" lowest-key)
    lowest-key))


(defun make-path (came-from current)
  (let ((path (list current)))
    (maphash #'(lambda (key value)
		 (push key path))
	     came-from)
    path))

;;not good
(defun cp (start-x start-y end-x end-y)
  (let ((start (cons start-x start-y))
	(end (cons end-x end-y))
	(closed-set (make-hash-table :test 'equal))
	(open-set (make-hash-table :test 'equal))
	(came-from (make-hash-table :test 'equal)))

    (format t "~&first letting done~%")

    (setf (gethash start open-set)
	  (cons 0 (* (min (abs (- start-x end-x))
			  (abs (- start-y end-y)))
		     5)))

    (format t "~&first setting done~%")

    (do ((lowest-total start (min-map open-set)))
	((or (zerop (hash-table-count open-set))
	     (equal lowest-total end))
	 (make-path came-from lowest-total))

      (format t "~&do loop start~%")

      (setf (gethash lowest-total closed-set)
	    (gethash lowest-total open-set))
      (remhash lowest-total open-set)

      (format t "~&moved data from open to closed set~%")

      (dolist (neighbour (mapcar
			  #'(lambda (x)
			      (neighbour-tile-coords
			       (car lowest-total)
			       (cdr lowest-total)
			       x *world*))
			  '(n ne se s sw nw)))

	(format t "~&doing mapcarred list: ~a~%" neighbour)
	
	(if (and neighbour (not (gethash neighbour closed-set)))
	    (let* ((neighbour-tile (aref (world-map *world*)
					 (car neighbour)
					 (cdr neighbour)))
		   (move-cost (eval (cond ((equal (tile-type neighbour-tile) 'sea)
					   50)
					  ((equal (tile-type neighbour-tile) 'grass)
					   3))))
		   (pot-g-score
		    (+ (car (gethash lowest-total closed-set))
		       move-cost)))

	      (format t "~&there was no neighbour in closed set~%")
	      
	      (if (null (gethash neighbour open-set))
		  (progn (format t "~&none in open set, potgscore: ~a car neigh:~a cdr neigh:~a~%"
				 pot-g-score (car neighbour) (cdr neighbour))
			 (setf (gethash neighbour open-set)
			       (cons pot-g-score (+ pot-g-score
						    (* (max (abs (- (car neighbour) end-x))
							    (abs (- (cdr neighbour) end-y)))
						       5))))
			 )
		  (progn (format t "~&there was one in open set~%")
			 (format t "~&pot-g-score:~a other-score:~a~%" pot-g-score
				 (cdr (gethash neighbour closed-set)))
			 (unless (>= pot-g-score (cdr (gethash neighbour open-set)))
			   (progn
			     (format t "got here~%")
			     (setf (gethash neighbour came-from) lowest-total)
			     (setf (gethash neighbour closed-set)
				   (cons pot-g-score (+ pot-g-score
							(* (max (abs (- (car neighbour) end-x))
								(abs (- (cdr neighbour) end-y)))
							   5))))))))))))))


	    
		  
			  
	    

	
      
      

      
  
  
  

(defun draw-path (start end)
  (sdl:draw-aa-line-* (car start) (cdr start)
		      (car end) (cdr end)
		      :color sdl:*white*))

(defun draw-world (x-shift y-shift selector-graphics selector-tile)

  (let* ((draw-count 0)
	 (x-start-void (floor x-shift (car tile-size)))
	 (x-start (if (> (+ x-shift (car tile-size)) 0) 0
		      (- (abs x-start-void) 2)))
	 (x-end (min
		 (+ (- x-start-void) (floor (sdl:width window) (car tile-size)))
		 (1- (array-dimension (world-map *world*) 0)))) ;; The last column
	 (y-start-void (floor y-shift (cdr tile-size)))
	 (y-start (min (if (> (+ y-shift (cdr tile-size)) 0) 0
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
  )

(defun draw-tile (x y x-shift y-shift)
  
  ;; Draw tile's basic type
  (let ((graphics-struct (eval (tile-type (aref (world-map *world*) x y)))))
    (sdl:draw-surface-at-* (graphics-surface graphics-struct)
			   (+ (* x (car tile-size))
			      x-shift (graphics-x-at graphics-struct))
			   (+ (if (evenp x) (* y (cdr tile-size))
				  (+ (* y (cdr tile-size)) (/ (cdr tile-size) 2)))
			      y-shift (graphics-y-at graphics-struct))))

  ;; Draw tile's variant list
  (dolist (variant (tile-variant (aref (world-map *world*) x y)))
    (let ((variant (eval variant)))
      (sdl:draw-surface-at-* (graphics-surface variant)
			     (+ (* x (car tile-size))
				x-shift (graphics-x-at variant))
			     (+ (if (evenp x) (* y (cdr tile-size))
				    (+ (* y (cdr tile-size)) (/ (cdr tile-size) 2)))
				y-shift (graphics-y-at variant))))))

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

    (tile-graphics-setup stream-large-north-west -2 -2)
    (tile-graphics-setup stream-large-south-west -2 50)
    (tile-graphics-setup stream-large-north 24 -8)


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
  (let ((river-symbol
	 (intern
	  (concatenate 'string
		       (string-upcase (symbol-name size)) "-"
		       (string-upcase (symbol-name direction))))))
    ;; Logic rivers
    (pushnew river-symbol
	     (tile-river-borders (aref (world-map *world*) tile-x tile-y)))
    ;; Graphic rivers
    (if (member direction '(N SW NW))
	(pushnew river-symbol
		 (tile-variant (aref (world-map *world*) tile-x tile-y))))

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
		     nil)))))
  

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

      (if (not (equal (aref (world-map world) x y) 'sea)) ; don't do for sea tiles
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



(defun make-perlin-map (world node-resolution)
  (let* ((width (world-width world))
	(height (world-height world))
	(nodes (make-array
		`(,(+ 1 (ceiling (/ width node-resolution)))
		   ,(+ 1 (ceiling (/ height node-resolution)))))))

    (dotimes (x (array-dimension nodes 0))
      (dotimes (y (array-dimension nodes 1))
	(setf (aref nodes x y) (generate-gradient))))
	
    (dotimes (x width)
      (dotimes (y height)

	(setf (aref (world-map world) x y)
	      (/ (+ (dot-product-shift (cons x y)
				       (aref nodes
					     (floor (/ x node-resolution))
					     (floor (/ y node-resolution)))))
		 (+ (dot-product-shift (cons x y)
				       (aref nodes
					     (ceiling (/ x node-resolution))
					     (floor (/ y node-resolution)))))
		 (+ (dot-product-shift (cons x y)
				       (aref nodes
					     (ceiling (/ x node-resolution))
					     (ceiling (/ y node-resolution)))))
		 (+ (dot-product-shift (cons x y)
				       (aref nodes
					     (floor (/ x node-resolution))
					     (ceiling (/ y node-resolution)))))
		 4))))))
        

    
    

(defun generate-gradient ()
  (let ((radians (* (/ (random 3600) (random 3600)) pi)))
    (cons (sin radians) (cos radians))))

(defun dot-product-shift (a b)
  (dot-product b (cons (- (car a) (car b))
		       (- (cdr a) (cdr b)))))

(defun dot-product (a b)
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))
