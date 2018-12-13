(in-package :war)

;; (chop-test "graphics/CITY_A_LARGE_BIG.png" -6 -5)

;; Since we are creating a hexagon sitting on it's side
;; (sitting side's length: hor-line-length), intending to
;; stack these in columns, the tiling will happen on x-axis
;; at multiples of
;; ((tile-width - hor-line-length) / 2) + hor-line-length
;; and on y-axis at tile-height
;; possibly recommended to keep args as even numbers
(defun chop-tile (raw-tile-location x-offset y-offset
		  &optional (hor-line-length 76) (tile-width 128) (tile-height 104))
  "Returns a list containing tile and it's outskirt graphics"
  
    (let* ((left-border-tri-x (floor (- tile-width hor-line-length) 2))
	   (right-border-tri-x (+ left-border-tri-x hor-line-length -1)) ;; More like right-border minus tri-x
	   (middle-height-lower (floor tile-height 2))
	   (middle-height-upper (- middle-height-lower 1))

	   (template (sdl:create-surface
		      tile-width tile-height
		      :color-key sdl:*white*))
	   (final-color-key *war-color-key*)

	   (raw-tile-image (make-graphics :surface (sdl-image:load-image raw-tile-location)
					  :x-at x-offset :y-at y-offset))
	   )
      
      ;; Fill surface with game's transparency key:
      (sdl:fill-surface final-color-key :surface template)

      ;; Draw the tile template:
      ;; notes: the pixel that gets drawn is at +1 of argument (if first pixel considered 1,1)
      (sdl:draw-filled-polygon (list (sdl:point :x left-border-tri-x
						:y 0)
				     (sdl:point :x right-border-tri-x
						:y 0)
				     (sdl:point :x (- tile-width 1)
						:y middle-height-upper)
				     (sdl:point :x (- tile-width 1)
						:y middle-height-lower)
				     (sdl:point :x right-border-tri-x
						:y (- tile-height 1))
				     (sdl:point :x left-border-tri-x
						:y (- tile-height 1))
				     (sdl:point :x 0
						:y middle-height-lower)
				     (sdl:point :x 0
						:y middle-height-upper))
			       :surface template :color sdl:*white*)

      (let ((chopping-block
	     (sdl:create-surface tile-width tile-height)))

	;; Behold!
	(mapcar #'(lambda (coord)
		    
		    (sdl:fill-surface *war-color-key* :surface chopping-block)
		    (draw-at (car coord) (cdr coord) 0 0 raw-tile-image chopping-block)
		    (sdl:draw-surface template :surface chopping-block)

		    ;; Determine bounds of actual graphics:
		    (let ((upper-bound)
			  (lower-bound)
			  (left-bound)
			  (right-bound))

		      (and (setf upper-bound
				 (do ((x 0) (y 0))
				     ((>= y tile-height) nil) ; return nil if no graphics found -> ends (and)
				   (if (not (sdl:color= (sdl:read-pixel-* x y :surface chopping-block)
							*war-color-key*))
				       (return y))
				   (and (incf x)
					(>= x tile-width)
					(setf x 0)
					(incf y))))
			   (setf lower-bound
				 (do ((x 0) (y (1- tile-height)))
				     ((< y 0) nil)
				   (if (not (sdl:color= (sdl:read-pixel-* x y :surface chopping-block)
							*war-color-key*))
				       (return y))
				   (and (incf x)
					(>= x tile-width)
					(setf x 0)
					(decf y))))
			   (setf left-bound
				 (do ((x 0) (y 0))
				     ((>= x tile-width) nil)
				   (if (not (sdl:color= (sdl:read-pixel-* x y :surface chopping-block)
							*war-color-key*))
				       (return x))
				   (and (incf y)
					(>= y tile-height)
					(setf y 0)
					(incf x))))
			   (setf right-bound
				 (do ((x (1- tile-width)) (y 0))
				     ((< x 0) nil)
				   (if (not (sdl:color= (sdl:read-pixel-* x y :surface chopping-block)
							*war-color-key*))
				       (return x))
				   (and (incf y)
					(>= y tile-height)
					(setf y 0)
					(decf x)))))
		      ;; Check'em:
		      (if (not (and upper-bound lower-bound left-bound right-bound))
			  nil ; return nil to resulting list in case of 'empty' graphics

			  ;; Create image of proper size to hold the chopped graphics
			  (let* ((final-width (1+ (- right-bound left-bound)))
				 (final-height (1+ (- lower-bound upper-bound)))
				 (final-image
				  (sdl:create-surface final-width
						      final-height)))

			    (sdl:draw-surface-at-* chopping-block (- left-bound) (- upper-bound) :surface final-image)
			    (setf (sdl:color-key-enabled-p final-image) t)
			    (setf (sdl:color-key final-image) *war-color-key*)

			    (make-graphics :surface final-image :x-at left-bound :y-at upper-bound)))))

		;; mapcar's list:
		(list (cons 0 0)       ;center
		      (cons 0 -1)      ;north from center -> south of it's own tile
		      (cons 1 -1)      ;north-east           south-west
		      (cons 1 0)       ;south-east           north-west
		      (cons 0 1)       ;south                north
		      (cons -1 0)      ;south-west           north-east
		      (cons -1 -1))))));north-west           south-east
	  
