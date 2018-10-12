;;;; war.lisp

;;(in-package #:war)

(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-image)

(defstruct world
  (width nil)     ;amount of columns
  (height nil)    ;hexes in a column
  ;;If wrapping required on x-axis, column count must be even 
  (map nil)       ;an array (of what?)
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
  (units nil))

(defvar *world* nil)

(defun init-test ()
  (setf *world* (init-world 50 25))
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
    (defparameter window (sdl:window 800 600 :title-caption "a test window"))
    (setf (sdl:frame-rate) 60)

    (load-tiles)

    (let ((x-shift 0) (y-shift 0)
	  (selector-tile '(0 . 0)) (selector-graphics '(0 . 0))
	  (selected-tile nil))

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
					x y x-shift y-shift (car selector-tile)))
				 )

	    
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
		    (setf selected-tile selector-tile))

		   ((equal button sdl:sdl-button-wheel-up)
		    (set-tile-size 'large))
		   ((equal button sdl:sdl-button-wheel-down)
		    (set-tile-size 'small)))

	     (draw-world x-shift y-shift selector-graphics))

	    
	    (:idle ()

		   (draw-world x-shift y-shift selector-graphics)
		   
		   (sdl:update-display))))))

(defun draw-world (x-shift y-shift selector-graphics)
  (do ((x 0)
       (y 0))
      ((>= x (array-dimension (world-map *world*) 0)))
    (sdl:draw-surface-at-* (eval (tile-type (aref (world-map *world*) x y)))
			   (+ (* x (car tile-size))
			      x-shift)
			   (+ (if (evenp x) (* y (cdr tile-size))
				  (+ (* y (cdr tile-size)) (/ (cdr tile-size) 2)))
			      y-shift))
    (incf y)
    (if (>= y (array-dimension (world-map *world*) 1))
	(progn (incf x)
	       (setf y 0))))

  (sdl:draw-surface-at-* (eval selector)
			 (car selector-graphics) (cdr selector-graphics)))

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


(defun load-tiles ()
  "Initializes tile graphics"
  ;;98,80 is tile size -> 78 is distance from right point to lower left point:
  ;;(defparameter tile-large-size '(78 . 80)) ;these are for the medium ones
  (defparameter tile-large-size '(102 . 104))
  (defparameter sea-large (sdl-image:load-image "graphics/SEA_LARGE.png"))
  (setf (sdl:alpha-enabled-p sea-large) t)
  (defparameter grass-large (sdl-image:load-image "graphics/GRASS_LARGE.png"))
  (setf (sdl:alpha-enabled-p grass-large) t)

  (defparameter sea-large-border-south (sdl-image:load-image "graphics/SEA_LARGE_BORDER_S.png"))
  (setf (sdl:alpha-enabled-p sea-large-border-south) t)
  (defparameter sea-large-border-north (sdl-image:load-image "graphics/SEA_LARGE_BORDER_N.png"))
  (setf (sdl:alpha-enabled-p sea-large-border-north) t)
  (defparameter sea-large-border-south-east (sdl-image:load-image "graphics/SEA_LARGE_BORDER_SE.png"))
  (setf (sdl:alpha-enabled-p sea-large-border-south-east) t)
  (defparameter sea-large-border-south-west (sdl-image:load-image "graphics/SEA_LARGE_BORDER_SW.png"))
  (setf (sdl:alpha-enabled-p sea-large-border-south-west) t)
  (defparameter sea-large-border-north-east (sdl-image:load-image "graphics/SEA_LARGE_BORDER_NE.png"))
  (setf (sdl:alpha-enabled-p sea-large-border-north-east) t)
  (defparameter sea-large-border-north-west (sdl-image:load-image "graphics/SEA_LARGE_BORDER_NW.png"))
  (setf (sdl:alpha-enabled-p sea-large-border-north-west) t)

  ;;(62,52) -> 49
  (defparameter tile-small-size '(49 . 52))
  (defparameter sea-small (sdl-image:load-image "graphics/SEA_SMALL.png"))
  (setf (sdl:alpha-enabled-p sea-small) t)
  (defparameter grass-small (sdl-image:load-image "graphics/GRASS_SMALL.png"))
  (setf (sdl:alpha-enabled-p grass-small) t)

  (defparameter selector-large (sdl-image:load-image "graphics/SELECT.png"))
  (setf (sdl:alpha-enabled-p selector-large) t)
  (defparameter selector-small (sdl-image:load-image "graphics/SELECT_SMALL.png"))
  (setf (sdl:alpha-enabled-p selector-small) t)

  (defparameter current-tile-size 'small)
  (set-tile-size current-tile-size)
  )

(defun set-tile-size (var)
  "Switches between tile sizes"
  (sdl:clear-display sdl:*black*)

  (cond ((equal var 'large)
	 (defparameter selector selector-large)
	 (defparameter tile-size tile-large-size)
	 (defparameter sea sea-large)
	 (defparameter grass grass-large))
	((equal var 'small)
	 (defparameter selector selector-small)
	 (defparameter tile-size tile-small-size)
	 (defparameter sea sea-small)
	 (defparameter grass grass-small))))

(defun init-world (width height)
  (let ((world (make-world)))
    (setf (world-width world) (- width 1))
    (setf (world-height world) (- height 1))
    (setf (world-map world) (make-array `(,width ,height)))

    ;; Make totally random map:
    (do ((x 0)
	 (y 0))
	((>= x (array-dimension (world-map world) 0)))
      (setf (aref (world-map world) x y) (make-tile :type (if (= (random 2) 1) 'sea 'grass)))
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
	    (let ((neighbour-tile (neighbour-tile-coords x y direction)))
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

(defun neighbour-tile-coords (here-x here-y direction)
  (let* ((map-width (1- (array-dimension (world-map *world*) 0)))
	 (map-height (1- (array-dimension (world-map *world*) 1)))
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
