(in-package :war)

(ql:quickload :vecto)

(defvar *nato-symbol-lib* (make-hash-table :test 'equal))

(defmacro find-first (predicate item-list list)
  (let ((element (gensym)))
    `(dolist (,element ,list)
       (if (or
	    ,@(mapcar #'(lambda (x) `(,predicate ,element ',x)) item-list))
	   (return ,element)))))


(defmacro origins-shape-rel (origin-list &rest coord-list)
  "Uses origins to draw several vector paths using coordinates relative to origin."
  `(progn
     ,@(mapcar #'(lambda (origin)
		   `(progn
		      (vecto:move-to ,(car origin) ,(cadr origin))
		      ,@(mapcar #'(lambda (coord)
				    `(vecto:line-to (+ ,(car origin) ,(car coord))
						    (+ ,(cadr origin) ,(cadr coord))))
				coord-list)))
	       origin-list)))


(defmacro origins-shape-abs (origin-list &rest coord-list)
  `(progn
     ,@(mapcar #'(lambda (origin)
		   `(progn
		      (vecto:move-to ,(car origin) ,(cadr origin))
		      ,@(mapcar #'(lambda (coord)
				    `(vecto:line-to ,(car coord) ,(cadr coord)))
				coord-list)))
	       origin-list)))

;; Return element from symbol lib if found, otherwise generate it first.
(defun description-to-surface (width height description)
  (setf description (sort description #'string< :key #'symbol-name))
  (or (gethash (cons (cons width height) description) *nato-symbol-lib*)
      (generate-natosymbol-from width height description)))

(defun generate-natosymbol-from (width height description)
  (let* ((octagon-dia 100)
	 (octagon-rad (floor octagon-dia 2))
	 (field-width (floor (* 1.5 octagon-dia)))
	 (field-height (floor (* 1.74 octagon-dia)))
	 (centre-x (floor field-width 2))
	 (centre-y (floor field-height 2))
	 (sin45 (* octagon-rad (sin (/ pi 4))))
	 (affiliation
	  (find-first eq (friendly hostile neutral unknown) description))
	 (dimension
	  (find-first eq (air space land surface subsurface
			      equipment installation activity)
		      description)))
    (vecto:with-canvas (:width field-width :height field-height)
      (vecto:set-rgb-fill 1.0 0.0 1.0) ; color key for sdl
      (vecto:clear-canvas) ; fill all with color key
      (vecto:set-line-width 1)
      
      (let ((oct-nw (cons (- centre-x sin45)
			  (+ centre-y sin45)))
	    (oct-ne (cons (+ centre-x sin45)
			  (+ centre-y sin45)))
	    (oct-se (cons (+ centre-x sin45)
			  (- centre-y sin45)))
	    (oct-sw (cons (- centre-x sin45)
			  (- centre-y sin45)))
	    (oct-w-x (- centre-x octagon-rad)) ; y is centre-y
	    (oct-e-x (+ centre-x octagon-rad))

	    ;; This is the x-distance between octagon's (circle's) NW
	    ;; and 1.1 times larger (on x axis) ellipse's x-coord
	    ;; when y = oct's N
	    (ellipse-intcard-x
	     (floor (sqrt (/ (expt (* octagon-rad 0.1) 2) 2))))
	    
	    (n-y)(nw-x)(nw-y)(ne-x)(ne-y)(s-y)(sw-x)
	    (sw-y)(se-x)(se-y)(w-x)(w-y)(e-x)(e-y))
	
	(cond ((eq affiliation 'friendly)
	       (vecto:set-rgb-fill (/ 128 255) (/ 224 255) (/ 255 255)) ; 'crystal blue'
	       (vecto:set-rgb-stroke 0 0 0) ; black
	       (cond ((or (eq dimension 'land);friendly land: rectangle
			  (eq dimension 'installation)
			  (eq dimension 'activity))
		      (setf
		       n-y (+ octagon-rad centre-y)
		       w-x 0
		       w-y centre-y
		       e-x field-width
		       e-y centre-y
		       nw-x w-x
		       nw-y n-y
		       s-y (- centre-y octagon-rad)
		       sw-x w-x
		       sw-y s-y
		       ne-x e-x
		       ne-y n-y
		       se-x e-x
		       se-y s-y)

		      ;; Handle installation:
		      (cond ((eq dimension 'installation)
			     (vecto:set-rgb-fill 0 0 0) ; black
			     (vecto:rectangle (- centre-x (/ octagon-rad 2))
					      n-y octagon-rad (/ octagon-dia 15))
			     (vecto:fill-and-stroke)
			     (vecto:set-rgb-fill (/ 128 255) (/ 224 255) 1.0))) ; blue

		      ;; Handle standard rectangle
		      (vecto:rectangle sw-x sw-y field-width octagon-dia)
		      (vecto:fill-and-stroke)

		      ;; Handle activity
		      (cond ((eq dimension 'activity)
			     (let ((corner (/ field-width 10)))
			       (vecto:set-rgb-fill 0 0 0) ; black
			       (vecto:rectangle nw-x (- nw-y corner) corner corner)
			       (vecto:rectangle (- ne-x corner) (- ne-y corner) corner corner)
			       (vecto:rectangle sw-x sw-y corner corner)
			       (vecto:rectangle (- se-x corner) se-y corner corner)
			       (vecto:fill-and-stroke)
			       (vecto:set-rgb-fill (/ 128 255) (/ 224 255) 1.0))) ; blue
			    ))
		     ((or (eq dimension 'air) ; friendly air: half ellipse, open bottom
			  (eq dimension 'space)) ; friendly space: as air, but black top bar
		      (setf
		       n-y field-height
		       w-x oct-w-x
		       w-y centre-y
		       e-x oct-e-x
		       e-y centre-y
		       nw-x (- (car oct-nw) ellipse-intcard-x)
		       nw-y (+ octagon-rad centre-y)
		       s-y (- centre-y octagon-rad)
		       sw-x (- w-x (* 0.1 octagon-rad))
		       sw-y s-y
		       ne-x (+ (car oct-ne) ellipse-intcard-x)
		       ne-y nw-y
		       se-x (+ e-x (* 0.1 octagon-rad))
		       se-y s-y)

		      ;; Full ellipse, to be cut later in this cond
		      (vecto:ellipse-arc centre-x s-y
					 (* 1.1 octagon-rad)
					 (* 1.37 octagon-dia)
					 0 0 (* 2 pi))
		      (vecto:fill-and-stroke)

		      ;; Do space specific black bar:
		      (if (eq dimension 'space)
			  (progn
			    (vecto:set-rgb-fill 0 0 0) ; black
			    (vecto:ellipse-arc centre-x s-y
					       (* 1.1 octagon-rad)
					       (* 1.37 octagon-dia)
					       0
					       (* 65/180 pi)
					       (* 115/180 pi))
			    (vecto:fill-and-stroke)
			    (vecto:set-rgb-fill (/ 128 255) (/ 224 255) 1.0))) ; blue

		      ;; Can't figure out clipping or making open half-ellipses: just draw a bottom
		      (vecto:set-rgb-fill 1.0 0.0 1.0) ; color-key
		      (vecto:move-to 0 s-y)
		      (vecto:line-to field-width s-y )
		      (vecto:line-to field-width 0)
		      (vecto:line-to 0 0)
		      (vecto:close-subpath)
		      (vecto:fill-path)
		      
		      (vecto:set-rgb-fill (/ 128 255) (/ 224 255) 1.0) ; blue
		      )
		     ((or (eq dimension 'surface) ; friendly (sea) surface: circle
			  (eq dimension 'equipment)) ; friendly equip, same as surface
		      (setf
		       n-y (+ octagon-rad centre-y)
		       w-x oct-w-x
		       w-y centre-y
		       e-x oct-e-x
		       e-y centre-y
		       nw-x (car oct-nw)
		       nw-y (cdr oct-nw)
		       s-y (- centre-y octagon-rad)
		       sw-x (car oct-sw)
		       sw-y (cdr oct-sw)
		       ne-x (car oct-ne)
		       ne-y (cdr oct-ne)
		       se-x (car oct-se)
		       se-y (cdr oct-se))

		      (vecto:centered-circle-path centre-x centre-y octagon-rad)
		      (vecto:fill-and-stroke)
		      )
		     ((eq dimension 'subsurface)
		      (setf
		       n-y (+ centre-y octagon-rad)
		       w-x oct-w-x
		       w-y centre-y
		       e-x oct-e-x
		       e-y centre-y
		       nw-x (- w-x (* 0.1 octagon-rad))
		       nw-y n-y
		       s-y (- centre-y octagon-rad)
		       sw-x (- (car oct-sw) ellipse-intcard-x)
		       sw-y (- centre-y octagon-rad)
		       ne-x (+ e-x (* 0.1 octagon-rad))
		       ne-y n-y
		       se-x (+ (car oct-se) ellipse-intcard-x)
		       se-y (- centre-y octagon-rad))

		      (vecto:ellipse-arc centre-x n-y
					 (* 1.1 octagon-rad)
					 (* 1.37 octagon-dia)
					 0 0 (* 2 pi))
		      (vecto:fill-and-stroke)

		      (vecto:set-rgb-fill 1.0 0.0 1.0) ; color-key
		      (vecto:move-to 0 n-y)
		      (vecto:line-to 0 field-height)
		      (vecto:line-to field-width field-height)
		      (vecto:line-to field-width n-y)
		      (vecto:close-subpath)
		      (vecto:fill-path)
		      
		      (vecto:set-rgb-fill (/ 128 255) (/ 224 255) 1.0) ; blue
		      
		      )))
	      ((eq affiliation 'neutral) ; this ought to be easy
	       (vecto:set-rgb-fill (/ 170 255) (/ 255 255) (/ 170 255)) ; 'bamboo green'
	       (vecto:set-rgb-stroke 0 0 0) ; black

	       (setf ; Anchor points same for all dimensions
		n-y (+ centre-y octagon-rad)
		w-x (- centre-x octagon-rad)
		w-y centre-y
		e-x (+ centre-x octagon-rad)
		e-y centre-y
		nw-x w-x
		nw-y n-y
		s-y (- centre-y octagon-rad)
		sw-x w-x
		sw-y s-y
		ne-x e-x
		ne-y n-y
		se-x e-x
		se-y s-y)

	       (cond ((or (eq dimension 'land) ; closed square
			  (eq dimension 'surface)
			  (eq dimension 'equipment)
			  (eq dimension 'installation) ; black bar on top
			  (eq dimension 'activity)) ; black squares at corners

		      (if (eq dimension 'installation)
			  (progn (vecto:set-rgb-fill 0 0 0)
				 (vecto:rectangle
				  (- centre-x (/ octagon-rad 2))
				  n-y
				  octagon-rad ; width
				  (/ octagon-dia 15)) ; height
				 (vecto:fill-and-stroke)
				 (vecto:set-rgb-fill (/ 170 255) (/ 255 255) (/ 170 255))))
		      
		      (vecto:rectangle sw-x sw-y octagon-dia octagon-dia)
		      (vecto:fill-and-stroke)

		      (if (eq dimension 'activity)
			  (let ((corner (/ field-width 10)))
			    (vecto:set-rgb-fill 0 0 0)
			    (vecto:rectangle nw-x (- nw-y corner) corner corner)
			    (vecto:rectangle (- ne-x corner) (- ne-y corner) corner corner)
			    (vecto:rectangle sw-x sw-y corner corner)
			    (vecto:rectangle (- se-x corner) se-y corner corner)
			    (vecto:fill-and-stroke)
			    (vecto:set-rgb-fill (/ 170 255) (/ 255 255) (/ 170 255)))))

		     ((or (eq dimension 'air) ; square with open bottom
			  (eq dimension 'space)) ; black full length bar on top

		      (if (eq dimension 'space)
			  (progn (vecto:set-rgb-fill 0 0 0)
				 (vecto:rectangle nw-x nw-y
						  octagon-dia (/ octagon-rad 5))
				 (vecto:fill-and-stroke)
				 (vecto:set-rgb-fill (/ 170 255) (/ 255 255) (/ 170 255))))
		      
		      (vecto:rectangle sw-x 0 octagon-dia (+ octagon-dia sw-y))
		      (vecto:fill-and-stroke)

		      ;; Clear bottom:
		      (vecto:set-rgb-fill 1.0 0.0 1.0) ; color-key
		      (vecto:move-to 0 s-y)
		      (vecto:line-to field-width s-y )
		      (vecto:line-to field-width 0)
		      (vecto:line-to 0 0)
		      (vecto:close-subpath)
		      (vecto:fill-path)
		      (vecto:set-rgb-fill (/ 170 255) (/ 255 255) (/ 170 255)) ; back to green
		      )))
	      
	      ((eq affiliation 'hostile)
	       (vecto:set-rgb-fill (/ 255 255) (/ 128 255) (/ 128 255)) ; 'salmon'
	       (vecto:set-rgb-stroke 0 0 0) ; black

	       (let ((vert-ofs (* octagon-dia (sqrt 1/2)))) ; vertex offset from centre
		 (cond ((or (eq dimension 'air)
			    (eq dimension 'space))
			(setf n-y (+ centre-y vert-ofs)
			      w-x (- centre-x octagon-rad)
			      w-y centre-y
			      e-x (+ centre-x octagon-rad)
			      e-y centre-y
			      nw-x w-x
			      nw-y (+ centre-y (- vert-ofs octagon-rad))
			      s-y (- centre-y octagon-rad)
			      sw-x w-x
			      sw-y s-y
			      ne-x e-x
			      ne-y nw-y
			      se-x e-x
			      se-y s-y)
			(vecto:move-to sw-x 0) ; open bottom
			(vecto:line-to nw-x nw-y)
			(vecto:line-to centre-x n-y)
			(vecto:line-to ne-x ne-y)
			(vecto:line-to se-x 0)
			(vecto:close-subpath)
			(vecto:fill-and-stroke)

			;; Clear bottom:
			(vecto:set-rgb-fill 1.0 0.0 1.0)
			(vecto:rectangle 0 0 field-width s-y)
			(vecto:fill-path)

			(if (eq dimension 'space)
			    (progn
			      (vecto:set-rgb-fill 0 0 0)
			      (vecto:move-to (- centre-x (- vert-ofs octagon-rad))
					     (+ centre-y octagon-rad))
			      (vecto:line-to centre-x n-y)
			      (vecto:line-to (+ centre-x (- vert-ofs octagon-rad))
					     (+ centre-y octagon-rad))
			      (vecto:fill-path)))
			
			(vecto:set-rgb-fill (/ 255 255) (/ 128 255) (/ 128 255)) ; back to red
			)
		       ((or (eq dimension 'land) ; square tilted 45 degrees
			    (eq dimension 'surface)
			    (eq dimension 'equipment)
			    (eq dimension 'installation)
			    (eq dimension 'activity))
			
			(setf n-y (+ centre-y vert-ofs)
			      w-x (- centre-x vert-ofs)
			      w-y centre-y
			      e-x (+ centre-x vert-ofs)
			      e-y centre-y
			      nw-x (car oct-nw)
			      nw-y (cdr oct-nw)
			      s-y (- centre-y vert-ofs)
			      sw-x (car oct-sw)
			      sw-y (cdr oct-sw)
			      ne-x (car oct-ne)
			      ne-y (cdr oct-ne)
			      se-x (car oct-se)
			      se-y (cdr oct-se))

			(if (eq dimension 'installation)
			    (progn
			      (vecto:set-rgb-fill 0 0 0)
			      (vecto:rectangle
			       (- centre-x (/ octagon-rad 2))
			       n-y
			       octagon-rad ; width
			       (/ octagon-dia 15)) ; height
			      (vecto:fill-and-stroke)
			      (vecto:set-rgb-fill (/ 255 255) (/ 128 255) (/ 128 255))))

			;; Tilted square:
			(vecto:move-to centre-x n-y)
			(vecto:line-to e-x e-y)
			(vecto:line-to centre-x s-y)
			(vecto:line-to w-x w-y)
			(vecto:close-subpath)
			(vecto:fill-and-stroke)

			(if (eq dimension 'activity)
			    (let ((mini-square-rad (/ (- vert-ofs octagon-rad)
						      2))) ; centre to corner

			      (vecto:set-rgb-fill 0 0 0)

			      (origins-shape-rel ((centre-x n-y) ; top
						  (centre-x (- centre-y octagon-rad)) ; bottom
						  ((+ centre-x octagon-rad mini-square-rad)
						   (+ centre-y mini-square-rad)) ; right
						  ((- centre-x octagon-rad mini-square-rad)
						   (+ centre-y mini-square-rad))) ; left

						 (mini-square-rad (- mini-square-rad))
						 (0 (* -2 mini-square-rad))
						 ((- mini-square-rad) (- mini-square-rad)))

			      (vecto:close-subpath)
			      (vecto:fill-path)
			      (vecto:set-rgb-fill (/ 255 255) (/ 128 255) (/ 128 255))))
			)
		       ((eq dimension 'subsurface)
			(setf n-y (+ centre-y octagon-rad)
			      w-x (- centre-x octagon-rad)
			      w-y centre-y
			      e-x (+ centre-x octagon-rad)
			      e-y centre-y
			      nw-x w-x
			      nw-y n-y 
			      s-y (- centre-y vert-ofs)
			      sw-x w-x
			      sw-y (- centre-y (- vert-ofs octagon-rad))
			      ne-x e-x
			      ne-y nw-y
			      se-x e-x
			      se-y sw-y)

			(origins-shape-abs ((centre-x s-y))
					   (sw-x sw-y) (w-x field-height)
					   (e-x field-height) (se-x se-y))

			(vecto:close-subpath)
			(vecto:fill-and-stroke)

			;; Clear top:
			(vecto:set-rgb-fill 1.0 0.0 1.0)
			(vecto:rectangle 0 n-y field-width (- field-height n-y))
			(vecto:fill-path)
			(vecto:set-rgb-fill (/ 255 255) (/ 128 255) (/ 128 255))
			))))
	      ((eq affiliation 'unknown)
	       (vecto:set-rgb-fill (/ 255 255) (/ 255 255) (/ 128 255)) ; 'light yellow'
	       (vecto:set-rgb-stroke 0 0 0) ; black

	       (cond ((or (eq dimension 'air) ; three petal flower, open ?straight? bottom
			  (eq dimension 'space)) ; top petal black tip

		      (setf n-y (+ centre-y (* sin45 2))
			    w-x (- centre-x (* sin45 2))
			    w-y centre-y
			    e-x (+ centre-x (* sin45 2))
			    e-y centre-y
			    nw-x (car oct-nw)
			    nw-y (cdr oct-nw)
			    s-y (- centre-y sin45) ; let's go with straight bottom
			    sw-x (car oct-sw)
			    sw-y (cdr oct-sw)
			    ne-x (car oct-ne)
			    ne-y (cdr oct-ne)
			    se-x (car oct-se)
			    se-y (cdr oct-se))

		      (vecto:move-to sw-x sw-y)
		      (vecto:arc (+ centre-x sin45) centre-y
				 sin45 (/ pi -2) (/ pi 2))
		      (vecto:arc centre-x (+ centre-y sin45)
				 sin45 0 pi)
		      (vecto:arc (- centre-x sin45) centre-y
				 sin45 (/ pi 2) (+ pi (/ pi 2)))

		      (vecto:fill-and-stroke)
		      
		      ;; clear bottom
		      (vecto:rectangle sw-x 0 (* 2 sin45) centre-y)
		      (vecto:fill-path)
		      (vecto:set-rgb-fill 1.0 0.0 1.0)
		      (vecto:rectangle 0 0 field-width sw-y)
		      (vecto:fill-path)

		      (if (eq dimension 'space)
			  (progn
			    (vecto:set-rgb-fill 0 0 0) ; black
			    (vecto:arc centre-x (+ centre-y sin45)
				       sin45 (* pi (/ 30 180)) (* pi (/ 150 180)))
			    (vecto:fill-path)))

		      (vecto:set-rgb-fill (/ 255 255) (/ 255 255) (/ 128 255)) ; back to yellow
		      
		      ))
	       
	       )
	      )

	
	;; test
	(vecto:move-to nw-x nw-y)
	(vecto:line-to se-x se-y)
	(vecto:stroke)

	(vecto:move-to ne-x ne-y)
	(vecto:line-to sw-x sw-y)
	(vecto:stroke)

	;;let's draw the octagon:
	(vecto:move-to centre-x (+ centre-y octagon-rad))
	(vecto:line-to (car oct-ne) (cdr oct-nw))
	(vecto:line-to (+ octagon-rad centre-x) centre-y)
	(vecto:line-to (car oct-se) (cdr oct-se))
	(vecto:line-to centre-x (- centre-y octagon-rad))
	(vecto:line-to (car oct-sw) (cdr oct-sw))
	(vecto:line-to (- centre-x octagon-rad) centre-y)
	(vecto:line-to (car oct-nw) (cdr oct-nw))
	(vecto:close-subpath)
	(vecto:stroke)

	(vecto:save-png "vectosave"))
      )))



