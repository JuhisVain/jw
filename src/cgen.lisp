(in-package :war)

(ql:quickload :vecto)
(ql:quickload :lispbuilder-sdl-vecto)

(defvar *nato-symbol-lib* (make-hash-table :test 'equal))

(defmacro find-first (predicate item-list list)
  "Searches list for items in item-list"
  (let ((element (gensym)))
    `(dolist (,element ,list)
       (if (or
	    ,@(mapcar #'(lambda (x) `(,predicate ,element ',x)) item-list))
	   (return ,element)))))

(defmacro colorset (what color)
  (append (cond ((eq what 'fill) '(vecto:set-rgb-fill))
		((eq what 'stroke) '(vecto:set-rgb-stroke)))
	  (cond ((or (eq color 'red) (eq color 'salmon))
		 '((/ 255 255) (/ 128 255) (/ 128 255)))
		((or (eq color 'blue) (eq color 'crystal))
		 '((/ 128 255) (/ 224 255) (/ 255 255)))
		((or (eq color 'green) (eq color 'bamboo))
		 '((/ 170 255) (/ 255 255) (/ 170 255)))
		((or (eq color 'yellow))
		 '((/ 255 255) (/ 255 255) (/ 128 255)))
		((or (eq color 'black))
		 '(0 0 0))
		((or (eq color 'color-key) (eq color 'key))
		 '(1 0 1)))))

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

(defun reset-cgen ()
  (and (boundp '*nato-symbol-lib*)
       (progn (maphash #'(lambda (key val)
			   (if (eq key 'default-counter-base)
			       (sdl:free (graphics-surface val))
			       (sdl:free val)))
		       *nato-symbol-lib*)
	      (setf *nato-symbol-lib* (clrhash *nato-symbol-lib*)))))

(defun init-cgen (&optional (push-to-archive nil))
  (and (null push-to-archive)
       (zerop (hash-table-count *nato-symbol-lib*))
       (setf (gethash 'default-counter-base *nato-symbol-lib*)
	     (make-graphics
	      :surface (sdl-image:load-image "graphics/COUNTERBASE.png"
					     :color-key *war-color-key*)
	      :x-at 24 :y-at 12))
       ))


;; Debugging:
(defun dump-natosymbols ()
  (defparameter testlist nil)
  (let ((xxx 0))
    (maphash #'(lambda (key val)
		 (push key testlist)

		 (format t "~&name: ~a~%KEY:   ~a~%VAL:   ~a~%~%" xxx key val)
		 
                 (sdl:save-image
		  (if (graphics-p val) (graphics-surface val) val)
		  (write-to-string xxx))
                 (setf xxx (+ 1 xxx)))
             *nato-symbol-lib*)))

(defun description-to-counter (faction width description
			       &optional (current-pov (world-current-turn *world*)))
  (let* ((aff-desc
	  (sort (cons (faction-relationship-with current-pov faction)
		      description)
		#'string< :key #'symbol-name))
	 (full-desc
	  (list faction width aff-desc)))
    
    (or (gethash full-desc *nato-symbol-lib*)
	(progn

	  (format t "~&~%DTC: aff: ~a ~%ful: ~a~%~%" aff-desc full-desc)
	  
	  (setf (gethash full-desc *nato-symbol-lib*)

		(let* ((counter-base
			;; blabla if not faction specific counterbase use default:..
			(gethash 'default-counter-base *nato-symbol-lib*))
		       (base
			(sdl:copy-surface :surface (graphics-surface counter-base))
			;;(sdl:create-surface (sdl:width (graphics-surface counter-base))
			;;		    (sdl:height (graphics-surface counter-base))
			;;		    :color-key *war-color-key*)
			 ))

		  (format t "~&base ::::  ~a~%" base)
		  (sdl:draw-surface-at-* ;; THSES BLITS RETURNF SOURCE NOT RESULT fgjfgjfgj
		   (description-to-surface width aff-desc)
		   10 10
		   :surface base
		   )
		  base))))

    (format t "~&Returning after creation: ~a~%" full-desc)
    (gethash full-desc *nato-symbol-lib*)))


(defun description-to-surface (width description)
  "Return sdl:surface from natosymbol archive if found, otherwise generate, store and return."
  ;;(setf description (sort description #'string< :key #'symbol-name))
  (format t "~&~a~%" description)
  (or (gethash (cons width description) *nato-symbol-lib*)
      (setf (gethash (cons width description) *nato-symbol-lib*)
	    (generate-natosymbol-from width description))))

(defun generate-natosymbol-from (width description)
  "Generates a symbol based on the Nato joint unit symbology specification."
  (let* ((slack 2)
	 (doubleslack (* slack 2))
	 (field-width width)
	 (octagon-dia (* 2/3 width))
	 (octagon-rad (floor octagon-dia 2))
	 (field-height (floor (* width 29/25))) ;; 174/150
	 (centre-x (+ (floor field-width 2) slack))
	 (centre-y (+ (floor field-height 2) slack))
	 (sin45 (* octagon-rad (sin (/ pi 4))))
	 (affiliation
	  (find-first eq (friendly hostile neutral unknown) description))
	 (dimension
	  (find-first eq (air space land surface subsurface
			      equipment installation activity)
		      description)))
    (vecto:with-canvas (:width (+ field-width doubleslack)
			:height (+ field-height doubleslack))
      (colorset fill key) ; color key for sdl
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
	    
	    (n-y)(nw-x)(nw-y)(ne-x)(ne-y)(s-y)(sw-x)
	    (sw-y)(se-x)(se-y)(w-x)(w-y)(e-x)(e-y))

	  (vecto:with-graphics-state
	  
	  (cond ((eq affiliation 'friendly)
		 (colorset fill blue)
		 (colorset stroke black)
		 (cond ((or (eq dimension 'land);friendly land: rectangle
			    (eq dimension 'installation)
			    (eq dimension 'activity))
			(setf
			 n-y (+ octagon-rad centre-y)
			 w-x slack
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
			       (colorset fill black)
			       (vecto:rectangle (- centre-x (/ octagon-rad 2))
						n-y octagon-rad (/ octagon-dia 15))
			       (vecto:fill-and-stroke)
			       (colorset fill blue)))

			;; Handle standard rectangle
			(vecto:rectangle sw-x sw-y field-width octagon-dia)
			(vecto:clip-path)
			(vecto:fill-and-stroke)

			;; Handle activity
			(cond ((eq dimension 'activity)
			       (let ((corner (/ field-width 10)))
				 (colorset fill black)
				 (origins-shape-rel ((nw-x (- nw-y corner -1)) ; Gotta shift up by 1
						     ((- ne-x corner (- 1)) (- ne-y corner -1))
						     (sw-x sw-y)
						     ((- se-x corner (- 1)) se-y))
						    (corner 0) (corner corner) (0 corner))
				 (vecto:fill-and-stroke)
				 (colorset fill blue)
				 ))))
		       ((or (eq dimension 'air) ; friendly air: half ellipse, open bottom
			    (eq dimension 'space)) ; friendly space: as air, but black top bar
			(setf
			 n-y field-height
			 w-x (- oct-w-x (* width 0.02)); Shifted large ellipsoids are too hard
			 w-y centre-y
			 e-x (+ oct-e-x (* width 0.02)); Clipping will handle this business
			 e-y centre-y
			 nw-x (- oct-w-x (* 0.1 octagon-rad))
			 nw-y (+ centre-y octagon-rad)
			 s-y (- centre-y octagon-rad)
			 sw-x (- oct-w-x (* 0.1 octagon-rad))
			 sw-y s-y
			 ne-x (+ oct-e-x (* 0.1 octagon-rad))
			 ne-y nw-y
			 se-x (+ oct-e-x (* 0.1 octagon-rad))
			 se-y s-y)

			;; Clip upper part of field
			(vecto:rectangle 0 s-y (+ field-width doubleslack)
					 (+ field-height doubleslack (- s-y)))
			(vecto:clip-path)
			(vecto:end-path-no-op)
			  
			;; Full ellipse drawn and clipped for open bottom
			(vecto:ellipse-arc centre-x s-y
					   (* 1.1 octagon-rad)
					   (* 1.37 octagon-dia)
					   0 0 (* 2 pi))

			(vecto:clip-path)
			(vecto:fill-and-stroke)

			;; Do space specific black bar:
			(if (eq dimension 'space) ; Could just draw black rect on upper field
			    (progn
			      (colorset fill black)
			      (vecto:ellipse-arc centre-x s-y
						 (* 1.1 octagon-rad)
						 (* 1.37 octagon-dia)
						 0
						 (* 65/180 pi)
						 (* 115/180 pi))
			      (vecto:fill-and-stroke)
			      (colorset fill blue)))
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
			(vecto:clip-path)
			(vecto:fill-and-stroke)
			)
		       ((eq dimension 'subsurface)
			(setf
			 n-y (+ centre-y octagon-rad)
			 w-x (- oct-w-x (* width 0.02))
			 w-y centre-y
			 e-x (+ oct-e-x (* width 0.02))
			 e-y centre-y
			 nw-x (- oct-w-x (* 0.1 octagon-rad))
			 nw-y n-y
			 s-y 0
			 sw-x nw-x
			 sw-y (- centre-y octagon-rad)
			 ne-x (+ oct-e-x (* 0.1 octagon-rad))
			 ne-y n-y
			 se-x ne-x
			 se-y (- centre-y octagon-rad))
			
			;; Clip upper part of field
			(vecto:rectangle 0 0 field-width n-y)
			(vecto:clip-path)
			(vecto:end-path-no-op)

			(vecto:ellipse-arc centre-x n-y
					   (* 1.1 octagon-rad)
					   (* 1.37 octagon-dia)
					   0 0 (* 2 pi))
			(vecto:clip-path)
			(vecto:fill-and-stroke)

			(colorset fill blue))))
		((eq affiliation 'neutral)
		 (colorset fill bamboo)
		 (colorset stroke black)

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
			    (progn (colorset fill black)
				   (vecto:rectangle
				    (- centre-x (/ octagon-rad 2))
				    n-y
				    octagon-rad ; width
				    (/ octagon-dia 15)) ; height
				   (vecto:fill-and-stroke)
				   (colorset fill green)))
			
			(vecto:rectangle sw-x sw-y octagon-dia octagon-dia)
			(vecto:clip-path)
			(vecto:fill-and-stroke)

			(if (eq dimension 'activity)
			    (let ((corner (/ field-width 10)))
			      (colorset fill black)

			      (origins-shape-rel ((nw-x (- nw-y corner -1)) ; Gotta shift up by 1
						     ((- ne-x corner -1) (- ne-y corner -1))
						     (sw-x sw-y)
						     ((- se-x corner -1) se-y))
						    (corner 0) (corner corner) (0 corner))
			      
			      (vecto:fill-and-stroke)
			      (colorset fill green))))

		       ((or (eq dimension 'air) ; square with open bottom
			    (eq dimension 'space)) ; black full length bar on top

			(if (eq dimension 'space)
			    (progn (colorset fill black)
				   (vecto:rectangle nw-x nw-y
						    octagon-dia (/ octagon-rad 5))
				   (vecto:fill-and-stroke)
				   (colorset fill bamboo)))

			;; Clipper
			(vecto:rectangle 0 s-y field-width (- field-height s-y))
			(vecto:clip-path)
			(vecto:end-path-no-op)
			;; Graphics
			(vecto:rectangle sw-x 0 octagon-dia (+ octagon-dia sw-y))
			(vecto:clip-path)
			(vecto:fill-and-stroke)

			)
		       ((eq dimension 'subsurface) ; square with open top

			(vecto:rectangle 0 0 field-width nw-y)
			(vecto:clip-path)
			(vecto:end-path-no-op)

			(vecto:rectangle sw-x sw-y octagon-dia (- field-height sw-y))
			(vecto:clip-path)
			(vecto:fill-and-stroke)
			
			
			)
		       ))
		
		((eq affiliation 'hostile)
		 (colorset fill salmon)
		 (colorset stroke black)

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

			  ;; Clip upper part of field
			  (vecto:rectangle 0 s-y field-width (- field-height s-y))
			  (vecto:clip-path)
			  (vecto:end-path-no-op)

			  ;; Draw house:
			  (vecto:move-to sw-x 0)
			  (vecto:line-to nw-x nw-y)
			  (vecto:line-to centre-x n-y)
			  (vecto:line-to ne-x ne-y)
			  (vecto:line-to se-x 0)
			  (vecto:clip-path)
			  (vecto:close-subpath)
			  (vecto:fill-and-stroke)

			  ;; Set anchors to force diagonals to converge at octagon centre
			  (setf nw-x (car oct-nw)
				nw-y (cdr oct-nw)
				ne-x (car oct-ne)
				ne-y (cdr oct-ne))

			  (if (eq dimension 'space)
			      (progn
				(colorset fill black)
				(vecto:move-to (- centre-x (- vert-ofs octagon-rad))
					       (+ centre-y octagon-rad))
				(vecto:line-to centre-x n-y)
				(vecto:line-to (+ centre-x (- vert-ofs octagon-rad))
					       (+ centre-y octagon-rad))
				(vecto:fill-path)
				(colorset fill red)))
			  
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
				(colorset fill black)
				(vecto:rectangle
				 (- centre-x (/ octagon-rad 2))
				 n-y
				 octagon-rad ; width
				 (/ octagon-dia 15)) ; height
				(vecto:fill-and-stroke)
				(colorset fill red)))

			  ;; Tilted square:
			  (vecto:move-to centre-x n-y)
			  (vecto:line-to e-x e-y)
			  (vecto:line-to centre-x s-y)
			  (vecto:line-to w-x w-y)
			  (vecto:clip-path)
			  (vecto:close-subpath)
			  (vecto:fill-and-stroke)

			  (if (eq dimension 'activity)
			      (let ((mini-square-rad (/ (- vert-ofs octagon-rad)
							2))) ; centre to corner

				(colorset fill black)

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
				(colorset fill red)))
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

			  (vecto:rectangle 0 0 field-width n-y)
			  (vecto:clip-path)
			  (vecto:end-path-no-op)

			  (vecto:move-to centre-x s-y)
			  (vecto:line-to sw-x sw-y)
			  (vecto:line-to w-x field-height)
			  (vecto:line-to e-x field-height)
			  (vecto:line-to se-x se-y)
			  (vecto:clip-path)

			  (vecto:close-subpath)
			  (vecto:fill-and-stroke)

			  (setf sw-x (car oct-sw) ;; Diagonals meet at oct centre
				sw-y (cdr oct-sw)
				se-x (car oct-se)
				se-y (cdr oct-se))

			  ))))
		((eq affiliation 'unknown)
		 (colorset fill yellow)
		 (colorset stroke black)

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

			;; Clipper
			(vecto:rectangle 0 s-y field-width (- field-height s-y))
			(vecto:clip-path)
			(vecto:end-path-no-op)

			;; Begin frame
			(vecto:move-to sw-x sw-y)
			(vecto:line-to sw-x 0) ;; Take stroke outside of drawn area
			(vecto:line-to se-x 0)
			;; Petals
			(vecto:arc (+ centre-x sin45) centre-y ; east
				   sin45 (/ pi -2) (/ pi 2))
			(vecto:arc centre-x (+ centre-y sin45) ; north
				   sin45 0 pi)
			(vecto:arc (- centre-x sin45) centre-y ; west
				   sin45 (/ pi 2) (+ pi (/ pi 2)))
			(vecto:clip-path)
			(vecto:fill-and-stroke)

			(if (eq dimension 'space)
			    (progn
			      (colorset fill black)
			      (vecto:arc centre-x (+ centre-y sin45)
					 sin45 (* pi (/ 30 180)) (* pi (/ 150 180)))
			      (vecto:fill-path)))

			(colorset fill yellow)
			)
		       ((eq dimension 'subsurface) ; three petals, open top
			(setf n-y (+ centre-y sin45)
			      w-x (- centre-x (* sin45 2))
			      w-y centre-y
			      e-x (+ centre-x (* sin45 2))
			      e-y centre-y
			      nw-x (car oct-nw)
			      nw-y (cdr oct-nw)
			      s-y (- centre-y (* sin45 2))
			      sw-x (car oct-sw)
			      sw-y (cdr oct-sw)
			      ne-x (car oct-ne)
			      ne-y (cdr oct-ne)
			      se-x (car oct-se)
			      se-y (cdr oct-se))

			;; Clipper:
			(vecto:rectangle 0 0 field-width n-y)
			(vecto:clip-path)
			(vecto:end-path-no-op)
			;; Begin:
			(vecto:move-to ne-x ne-y)
			(vecto:line-to ne-x field-height) ; Stroke outside clipper
			(vecto:line-to nw-x field-height)
			(vecto:arc (- centre-x sin45) centre-y
				   sin45 (/ pi 2) (* pi 3/2))
			(vecto:arc centre-x (- centre-y sin45)
				   sin45 pi (* 2 pi))
			(vecto:arc (+ centre-x sin45) centre-y
				   sin45 (- (/ pi 2)) (/ pi 2))
			(vecto:clip-path)
			(vecto:fill-and-stroke)
			)
		       ((or (eq dimension 'land)
			    (eq dimension 'surface)
			    (eq dimension 'equipment)
			    (eq dimension 'installation)
			    (eq dimension 'activity))
			
			(setf n-y (+ centre-y (* sin45 2))
			      w-x (- centre-x (* sin45 2))
			      w-y centre-y
			      e-x (+ centre-x (* sin45 2))
			      e-y centre-y
			      nw-x (car oct-nw)
			      nw-y (cdr oct-nw)
			      s-y (- centre-y (* sin45 2))
			      sw-x (car oct-sw)
			      sw-y (cdr oct-sw)
			      ne-x (car oct-ne)
			      ne-y (cdr oct-ne)
			      se-x (car oct-se)
			      se-y (cdr oct-se))

			(if (eq dimension 'installation)
			    (progn
			      (colorset fill black)
			      (vecto:rectangle
			       (- centre-x (/ octagon-rad 2))
			       n-y
			       octagon-rad ; width
			       (/ octagon-dia 15)) ; height
			      (vecto:fill-path)
			      (colorset fill yellow)))

			(vecto:move-to nw-x nw-y)
			(vecto:arc (- centre-x sin45) centre-y
				   sin45 (/ pi 2) (* pi 3/2))
			(vecto:arc centre-x (- centre-y sin45)
				   sin45 pi (* 2 pi))
			(vecto:arc (+ centre-x sin45) centre-y
				   sin45 (- (/ pi 2)) (/ pi 2))
			(vecto:arc centre-x (+ centre-y sin45)
				   sin45 0 pi)
			(vecto:clip-path) ; ok!
			(vecto:fill-and-stroke)

			(if (eq dimension 'activity)
			    (let ((msr (/ (- (* octagon-dia (sqrt 1/2)) octagon-rad) 2)))
			      (colorset fill black)
			      (origins-shape-rel (((- centre-x (/ msr 2)) n-y)
						  ((- e-x msr) (+ e-y (/ msr 2)))
						  ((- centre-x (/ msr 2)) (+ s-y msr))
						  (w-x (+ w-y (/ msr 2))))

						 (msr 0) (msr (- msr)) (0 (- msr))
						 )
			      (vecto:fill-path)
			      
			      (colorset fill yellow)
			      ))

			))
		 
		 )
		)

	  
	  ;; test
;;	  (vecto:set-rgb-stroke 1 0 0)
;;	  (vecto:move-to nw-x nw-y)
;;	  (vecto:line-to se-x se-y)
;;	  (vecto:stroke)

;;	  (vecto:move-to ne-x ne-y)
;;	  (vecto:line-to sw-x sw-y)
;;	  (vecto:stroke)

;;	  (vecto:set-rgb-stroke 0 0 1)
;;	  (vecto:move-to w-x w-y)
;;	  (vecto:line-to e-x e-y)
;;	  (vecto:stroke)

;;	  (vecto:move-to centre-x n-y)
;;	  (vecto:line-to centre-x s-y)
;;	  (vecto:stroke)

;;	  (vecto:set-rgb-stroke 0 0 0) ; black

	  ;;let's draw the octagon:
;;	  (vecto:move-to centre-x (+ centre-y octagon-rad))
;;	  (vecto:line-to (car oct-ne) (cdr oct-nw))
;;	  (vecto:line-to (+ octagon-rad centre-x) centre-y)
;;	  (vecto:line-to (car oct-se) (cdr oct-se))
;;	  (vecto:line-to centre-x (- centre-y octagon-rad))
;;	  (vecto:line-to (car oct-sw) (cdr oct-sw))
;;	  (vecto:line-to (- centre-x octagon-rad) centre-y)
;;	  (vecto:line-to (car oct-nw) (cdr oct-nw))
;;	  (vecto:close-subpath)
;;	  (vecto:stroke)


	  ;;; Full frame icons should be drawn here to make use of frame clipping
	  ;; Modifier sections on octagon are top and bottom 30% of oct height
	  ;; that is 20% up / down from centre
	  (let* ((modsec-height (* 0.3 octagon-dia))
		 (upsec-bottom (+ centre-y (* 0.2 octagon-dia))) ; maybe move to master let
		 (upsec-middle (+ upsec-bottom (/ modsec-height 2)))
		 (downsec-top (- centre-y (* 0.2 octagon-dia)))
		 (downsec-middle (- downsec-top (/ modsec-height 2))))
	    ;;(dolist (icon description)
	    (do* ((rest-list description (cdr rest-list))
		  (icon (car rest-list) (car rest-list)))
		 ((null rest-list))
	      (cond ((eq icon 'air-assault-with-organic-lift) ;; Quack quack, motherfucker!
		     (vecto:move-to 0 downsec-top)
		     (vecto:line-to (- centre-x (* 1/3 modsec-height)) downsec-top)
		     (vecto:line-to centre-x (- downsec-top (* 1/3 modsec-height)))
		     (vecto:line-to (+ centre-x (* 1/3 modsec-height)) downsec-top)
		     (vecto:line-to field-width downsec-top)
		     (vecto:stroke))
		    ((eq icon 'air-defense)

		     (vecto:ellipse-arc centre-x sw-y
					(/ (- se-x sw-x) 2)
					(- downsec-top sw-y)
					0 0 pi)
		     (vecto:stroke))
		    ((or (eq icon 'air-and-naval-gunfire-liaison-company)
			 (eq icon 'anglico))
		     (setf description
			   ;; The anglico icon is composed of these:
			   (nconc description '(reconnaissance field-artillery
						rotary-wing-aviation naval)))
		     )
		    ((eq icon 'amphibious)
		     (let* ((tip-width (/ octagon-rad 6))
			    (wave-width (* tip-width 2)))
		       (vecto:arc (+ centre-x (* wave-width 4)) centre-y tip-width 0 pi)
		       (vecto:arcn (+ centre-x (* wave-width 3))  centre-y tip-width 0 pi)
		       (vecto:arc (+ centre-x (* wave-width 2)) centre-y tip-width 0 pi)
		       (vecto:arcn (+ centre-x wave-width) centre-y tip-width 0 pi)
		       (vecto:arc centre-x centre-y tip-width 0 pi)
		       (vecto:arcn (- centre-x wave-width) centre-y tip-width 0 pi)
		       (vecto:arc (- centre-x (* wave-width 2)) centre-y tip-width 0 pi)
		       (vecto:arcn (- centre-x (* wave-width 3)) centre-y tip-width 0 pi)
		       (vecto:arc (- centre-x (* wave-width 4)) centre-y tip-width 0 pi)
		       (vecto:stroke))
		     )
		    ((eq icon 'analysis)
		     (vecto:move-to centre-x upsec-middle)
		     (vecto:line-to centre-x downsec-top)
		     (vecto:line-to (car oct-se) downsec-top)
		     (vecto:line-to centre-x (- centre-y (* octagon-rad 5/6)))
		     (vecto:line-to (car oct-sw) downsec-top)
		     (vecto:line-to centre-x downsec-top)
		     (vecto:stroke)
		     )
		    ((or (eq icon 'antitank)
			 (eq icon 'antiarmour))
		     (vecto:move-to sw-x sw-y)
		     (vecto:line-to centre-x n-y)
		     (vecto:line-to se-x se-y)
		     (vecto:stroke)
		     )
		    ((eq icon 'broadcast-transmitter-antenna)
		     (vecto:move-to (- centre-x (* sin45 3/4)) upsec-middle)
		     (vecto:line-to centre-x upsec-bottom)
		     (vecto:line-to (+ centre-x (* sin45 3/4)) upsec-middle)
		     (vecto:stroke)
		     (vecto:move-to centre-x upsec-middle)
		     (vecto:line-to centre-x downsec-middle)
		     (vecto:stroke)
		     )
		    ((eq icon 'corps-support) ;; Not good, will have to figure out something
		     (if (or (eq affiliation 'friendly)
			     (eq affiliation 'hostile)
			     (eq affiliation 'unknown))
			 (progn (vecto:move-to field-width (+ centre-y octagon-rad))
				(vecto:line-to oct-e-x centre-y)
				(vecto:line-to field-width (- centre-y octagon-rad))))
		     (if (eq affiliation 'neutral)
			 (progn (vecto:move-to ne-x ne-y)
				(vecto:line-to (/ (+ centre-x ne-x) 2)
					       centre-y)
				(vecto:line-to se-x se-y)))
		     (vecto:stroke)
		     ))
	      ))

	  ;;(vecto:save-png "vectosave")
	  
	  ) ;; with graphics state ends

	  ;; Lesser icons to be drawn here to escape clipping
	  
	  ) ;; all the lets

      (let ((ret-surf (sdl:surface-from-vecto :pixel-alpha nil)))
	(setf (sdl:color-key-enabled-p ret-surf) t)
	(setf (sdl:color-key ret-surf) *war-color-key*)
	ret-surf
	)
      
      )))

