(in-package :counter-gen)

(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-gfx)

;; Supposed to work something like:
;; (CREATE friendly land mountain infantry) -> pukes out sdlsurface

(defstruct frames
  (air)
  (space)
  (land)
  (equipment)
  (sea-surface)
  (sea-sub)
  (installation)
  (activity))

(defstruct unit-data
  (affiliation) ;; Friendly, neutral, hostile, unknown
  (dimension)   ;; Air, land-unit, land-equipment, sea surface, sub
  (icon)        ;; Infantry, cavalry, submarine etc..
  (size)        ;; Squad, corps, army etc..
  (mobility)    ;; Wheeled, halftrack etc..
  (hq))         ;;

(defvar *generation-list*
  (list
   :field-surface nil
   :final-mask nil
   :affiliation nil
   :dimension nil
   :icon-list nil
   :line-color nil
   :line-width nil
   :fill-color nil
   :n nil
   :ne nil
   :e nil
   :se nil
   :s nil
   :sw nil
   :w nil
   :nw nil))

(defun cns-fun (octagon-diameter affiliation dimension icon-list)
  (let ((line-color) (line-width) (fill-color)
	(field-surface (sdl:create-surface (compute-field-width octagon-diameter)
					   (compute-field-height octagon-diameter)
					   :color-key color-key))
	(affiliation affiliation)
	(n) (ne) (e) (se) (s) (sw) (w) (nw)
	(final-mask (sdl:create-surface (compute-field-width octagon-diameter)
					(compute-field-height octagon-diameter)
					:color-key sdl:*red*))
	;;(amplifier-surface)
	)

    (setf (getf *generation-list* :field-surface) (sdl:create-surface (compute-field-width octagon-diameter)
								      (compute-field-height octagon-diameter)
								      :color-key color-key))

    (setf (getf *generation-list* :final-mask) (sdl:create-surface (compute-field-width octagon-diameter)
								   (compute-field-height octagon-diameter)
								   :color-key sdl:*red*))
    
    (sdl:fill-surface color-key :surface (getf *generation-list* :final-mask))
    (sdl:clear-display color-key :surface (getf *generation-list* :field-surface))

    ;;(apply (lines-and-colors affiliation)
    
    (eval (eval affiliation))
    (eval (eval dimension))

    (format t "~&n:~a~%" n)

    (mapcar #'eval (mapcar #'eval icon-list))
		    
    ;; Draw field here:
    (sdl:blit-surface final-mask field-surface) ;; <- modifies second by first arg
    (sdl:free final-mask)

    field-surface

    ))


(defmacro create-nato-symbol (octagon-diameter affiliation dimension icon-list)
  `(let ((line-color) (line-width) (fill-color)
	 (field-surface (sdl:create-surface (compute-field-width ,octagon-diameter)
					    (compute-field-height ,octagon-diameter)
					    :color-key color-key))
	 (affiliation ,affiliation)
	 (n) (ne) (e) (se) (s) (sw) (w) (nw)
	 (final-mask (sdl:create-surface (compute-field-width ,octagon-diameter)
					    (compute-field-height ,octagon-diameter)
					    :color-key sdl:*red*))
	 ;;(amplifier-surface)
	 )
     (sdl:fill-surface color-key :surface final-mask)
     (sdl:clear-display color-key :surface field-surface)
     (format t "~&Creating nato symbol with: ~a~%" octagon-diameter)
     
     ,(eval affiliation)
     ,(eval dimension)

     (progn ,@(mapcar #'eval icon-list)) ;; ooga booga

     ;; Draw field here:
     (sdl:blit-surface final-mask field-surface) ;; <- modifies second by first arg
     (sdl:free final-mask)
     ;;(list ,octagon-diameter (list n s w e) field-surface)

     field-surface

     ))

(defmacro create-nato-amplifiers (echelon mobility octagon-diameter limits field)
  (setf limits (eval limits))
  `(let* ((actual-field-width ,(- (aref (cadddr limits) 0) (aref (caddr limits) 0)))
	  (actual-field-height ,(- (aref (cadr limits) 1) (aref (car limits) 1)))
	  (echelon-diameter ,(floor octagon-diameter 6)) ;; this is for a single symbol
	  (echelon-radius (floor echelon-diameter 2))
	  (echelon-width)
	  (echelon-x-position) ;; upper left x of echelon
	  (echelon-y-position 0) ;; upper left y of echelon
	  (amplifier-surface)
	  (n (car limits))
	  (s (cadr limits))
	  (w (caddr limits))
	  (e (cadddr limits)))

     (setf amplifier-surface (sdl:create-surface actual-field-width
						 (+ actual-field-height
						    echelon-diameter
						    ) ;; mobility height here
						 :color-key color-key))
     
     (sdl:fill-surface color-key :surface amplifier-surface)
     
     (sdl:blit-surface field ;; Copy the field to amplifier-surface at correct position
		       amplifier-surface
		       (sdl:set-cell-* ,(aref (caddr limits) 0)
				       ,(+ (aref (car limits) 1) (floor octagon-diameter 6))
				       ,(- (aref (cadddr limits) 0) (aref (caddr limits) 0))
				       ,(+ (- (aref (cadr limits) 1) (aref (car limits) 1))
					   (floor octagon-diameter 6))
				       :surface field))

     ,(eval echelon)
     amplifier-surface
     
     ))


;;(defparameter squad)

(defparameter circle
  '(let*
    ((diameter (floor octagon-diameter 8))
     (radius (floor diameter 2))
     )))

(defparameter team ;;todo will break on friendly air. Need to shift symbol down before doing ech
  '(let*
    (
     
     (x-position (floor (- (aref e 0) (aref w 0)) 2))
     (y-position 0);;(- (aref n 1) radius))
     (line-width (ceiling line-width 2))
     (temp-mask (sdl:create-surface field-width field-height :color-key sdl:*red*)))
    
    (sdl:fill-surface sdl:*red* :surface temp-mask)
    (sdl:draw-filled-circle (sdl:point
			     :x x-position
			     :y (+ y-position echelon-radius))
     echelon-radius
     :surface temp-mask :color line-color)
    (sdl:draw-filled-circle (sdl:point
			     :x x-position
			     :y (+ y-position echelon-radius))
     (- echelon-radius (* 2 line-width))
     :surface temp-mask :color color-key)
    (sdl:draw-filled-polygon (list
			      (sdl:point :x (- x-position echelon-radius)
					 :y (+ y-position echelon-diameter))
			      (sdl:point :x (- (+ x-position echelon-radius) line-width)
					 :y y-position)
			      (sdl:point :x (+ x-position echelon-radius)
					 :y y-position)
			      (sdl:point :x (+ (- x-position echelon-radius) line-width)
					 :y (+ y-position echelon-diameter)))
     :surface temp-mask :color line-color)
    (sdl:blit-surface temp-mask amplifier-surface)
    (sdl:free temp-mask)
    ))


(defun nato-gen (octagon-diameter affiliation dimension icon-list)
  (let ((line-color) (line-width) (fill-color)
	(field-surface (sdl:create-surface (compute-field-width octagon-diameter)
					   (compute-field-height octagon-diameter)
					   :color-key color-key))
	;;(n) (ne) (e) (se) (s) (sw) (w) (nw)
	(final-mask (sdl:create-surface (compute-field-width octagon-diameter)
					(compute-field-height octagon-diameter)
					:color-key sdl:*red*))
	)
    ;;(declare (special n ne e se s sw w nw))
    (sdl:fill-surface color-key :surface final-mask)
    (sdl:clear-display color-key :surface field-surface)

    (format t "~&Affiliation:~%")
    affiliation
    (format t "~&Dimension:~%")
    dimension

    (format t "~&~a ~a ~aVoodoo:~%" n ne e)
    `(progn ,@(mapcar #'eval icon-list)) ;; ooga booga

    (format t "~&Drawing:~%")
    ;; Draw field here:
    (sdl:blit-surface final-mask field-surface) ;; <- modifies second by first arg
    (sdl:free final-mask)

    (format t "~&Returning:~%")
    field-surface))


(defun test-nato-gen (octagon-diameter)
  (nato-dimension-init octagon-diameter)
  (nato-color-init)
  (let ((width 500)
	(height 500))
    (sdl:with-init ()
      (let ((main-win (sdl:window width height :title-caption "counter generator test"))
	    (field-symbol))
	(setf (sdl:frame-rate) 1)
	(sdl:clear-display sdl:*white*)
	
	(sdl:with-events ()
	  (:quit-event () t)

	  (:idle ()
		 ;;(sdl:save-image (nato-gen octagon-diameter 'friendly 'land '(infantry mountain))
		;;		 "test.bmp")
		 (sdl:draw-rectangle-* 0 0 field-width field-height
				       :surface main-win :color sdl:*red*)

		 (setf field-symbol
		       (create-nato-symbol 100 friendly land (infantry mountain)))
		       ;;(nato-gen octagon-diameter 'friendly 'land '(infantry mountain)))

		 
		 (sdl:draw-surface field-symbol :surface main-win)
		 (sdl:free field-symbol)
		 
		 (sdl:update-display)))))))

(defun lines-and-colors (affiliation)
  (cond ((equal affiliation 'friendly)
	 '(black 2 blue))
	((equal affiliation 'neutral)
	 '(black 2 green))
	((equal affiliation 'unknown)
	 '(black 2 yellow))
	((equal affiliation 'hostile)
	 '(black 2 red))))

(defparameter friendly '(progn
			 (format t "~&'friendly getting evaled~%")
			 (format t "~&currentl in package: ~a~%" *package*)
			 (setf line-color black)
			 (setf line-width 2)
			 (setf fill-color blue)))

(defparameter neutral '(progn
			(setf line-color black)
			(setf line-width 2)
			(setf fill-color green)))

(defparameter unknown '(progn
			(setf line-color black)
			(setf line-width 2)
			(setf fill-color yellow)))

(defparameter hostile '(progn
			(setf line-color black)
			(setf line-width 2)
			(setf fill-color red)))

(defparameter land '(progn
		     (cond ;; not the most elegant solution
		     ((equal affiliation friendly) ;; The final mask will need to be set up in these
		      (setf ne p-ne-rect) (setf se p-se-rect)
		      (setf sw p-sw-rect) (setf nw p-nw-rect)
		      (setf s p-s-octagon) (setf n p-n-octagon)
		      (setf w (shift-coord nw 0 octagon-radius))
		      (setf e (shift-coord ne 0 octagon-radius))
		      (sdl:draw-filled-polygon ;; Black background as border line
		       (list nw ne se sw)
		       :surface field-surface :color line-color)
		      (sdl:draw-filled-polygon ;; the fill
		       (list (shift-coord nw line-width line-width)
			     (shift-coord ne (- (+ 1 line-width)) line-width)
			     (shift-coord se (- (+ 1 line-width)) (- line-width))
			     (shift-coord sw line-width (- line-width)))
		       :surface field-surface :color fill-color)
		      (sdl:draw-filled-polygon ;; Final mask
		       (list nw ne se sw)
		       :surface final-mask :color sdl:*red*))
		     ((equal affiliation neutral)
		      (setf ne p-ne-box) (setf se p-se-box)
		      (setf sw p-sw-box) (setf nw p-nw-box)
		      (setf n p-n-octagon) (setf s p-s-octagon)
		      (setf w p-w-octagon) (setf e p-e-octagon)
		      (sdl:draw-filled-polygon
		       (list nw ne se sw)
		       :surface field-surface :color line-color)
		      (sdl:draw-filled-polygon
		       (list nw ne se sw)
		       :surface final-mask :color sdl:*red*)
		      (sdl:draw-filled-polygon
		       (list
			(shift-coord nw line-width line-width)
			(shift-coord ne (- line-width) line-width)
			(shift-coord se (- line-width) (- line-width))
			(shift-coord sw line-width (- line-width)))
		       :surface field-surface :color fill-color))
		     ((equal affiliation unknown)
		      (setf ne p-ne-octagon) (setf se p-se-octagon)
		      (setf sw p-sw-octagon) (setf nw p-nw-octagon)
		      (setf n p-n-flower) (setf s p-s-flower)
		      (setf w p-w-flower) (setf e p-e-flower)
		      
		      (sdl:draw-filled-circle-* ;; top circle
		       (aref p-n-octagon 0) ;; x at centre
		       (aref ne 1) ;; y at octagon ne/nw
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface field-surface :color line-color)
		      (sdl:draw-filled-circle-* ;; bottom
		       (aref p-n-octagon 0)
		       (aref se 1)
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface field-surface :color line-color)
		      (sdl:draw-filled-circle-* ;; left
		       (aref nw 0)
		       (aref e 1)
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface field-surface :color line-color)
		      (sdl:draw-filled-circle-* ;; right
		       (aref ne 0)
		       (aref e 1)
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface field-surface :color line-color)
		      (sdl:draw-filled-polygon
		       (list nw ne se sw)
		       :surface field-surface :color line-color)

		      (sdl:draw-filled-circle-* ;; top circle
		       (aref p-n-octagon 0) ;; x at centre
		       (aref ne 1) ;; y at octagon ne/nw
		       (- (floor (- (aref ne 0) (aref nw 0)) 2) line-width)
		       :surface field-surface :color fill-color)
		      (sdl:draw-filled-circle-* ;; bottom
		       (aref p-n-octagon 0)
		       (aref se 1)
		       (- (floor (- (aref ne 0) (aref nw 0)) 2) line-width)
		       :surface field-surface :color fill-color)
		      (sdl:draw-filled-circle-* ;; left
		       (aref nw 0)
		       (aref e 1)
		       (- (floor (- (aref ne 0) (aref nw 0)) 2) line-width)
		       :surface field-surface :color fill-color)
		      (sdl:draw-filled-circle-* ;; right
		       (aref ne 0)
		       (aref e 1)
		       (- (floor (- (aref ne 0) (aref nw 0)) 2) line-width)
		       :surface field-surface :color fill-color)
		      (sdl:draw-filled-polygon
		       (list (shift-coord nw line-width line-width)
			     (shift-coord ne (- line-width) line-width)
			     (shift-coord se (- line-width) (- line-width))
			     (shift-coord sw line-width (- line-width)))
		       :surface field-surface :color fill-color)

		      (sdl:draw-filled-circle-* ;; top circle
		       (aref p-n-octagon 0) ;; x at centre
		       (aref ne 1) ;; y at octagon ne/nw
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface final-mask :color sdl:*red*)
		      (sdl:draw-filled-circle-* ;; bottom
		       (aref p-n-octagon 0)
		       (aref se 1)
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface final-mask :color sdl:*red*)
		      (sdl:draw-filled-circle-* ;; left
		       (aref nw 0)
		       (aref e 1)
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface final-mask :color sdl:*red*)
		      (sdl:draw-filled-circle-* ;; right
		       (aref ne 0)
		       (aref e 1)
		       (floor (- (aref ne 0) (aref nw 0)) 2)
		       :surface final-mask :color sdl:*red*)
		      (sdl:draw-filled-polygon
		       (list nw ne se sw)
		       :surface final-mask :color sdl:*red*)
		      )
		     ((equal affiliation hostile)
		      (setf n p-n-diamond) (setf s p-s-diamond)
		      (setf w p-w-diamond) (setf e p-e-diamond)
		      (setf nw p-nw-octagon) (setf sw p-sw-octagon)
		      (setf ne p-ne-octagon) (setf se p-se-octagon)
		      (sdl:draw-filled-polygon
		       (list n e s w)
		       :surface field-surface :color line-color)
		      (sdl:draw-filled-polygon
		       (list n e s w)
		       :surface final-mask :color sdl:*red*)
		      (sdl:draw-filled-polygon
		       (list
			(shift-coord n 0 line-width)
			(shift-coord e (- line-width) 0)
			(shift-coord s 0 (- line-width))
			(shift-coord w line-width 0))
		       :surface field-surface :color fill-color)))))

(defparameter air '(cond ((equal affiliation friendly)
			  (setf sw p-sw-posarc) (setf se p-se-posarc)
			  (setf s p-s-octagon) (setf n p-n-posarc)
			  (setf nw p-nw-posarc) (setf ne p-ne-posarc)
			  (setf e p-se-posarc) (setf w p-sw-posarc)
			  
			  (sdl:draw-filled-ellipse
			   s ;; centre of ellipse
			   (floor (- (aref se 0) (aref sw 0)) 2) ;; x radius aka. semi-major axis
			   (aref p-s-octagon 1) ;; y, semi-minor
			   :surface field-surface :color line-color)
			  
			  (sdl:draw-filled-ellipse
			   s
			   (- (floor (- (aref se 0) (aref sw 0)) 2) line-width)
			   (- (aref p-s-octagon 1) line-width)
			   :surface field-surface :color fill-color)
			  
			  (sdl:draw-filled-ellipse
			   s
			   (floor (- (aref se 0) (aref sw 0)) 2)
			   (aref p-s-octagon 1)
			   :surface final-mask :color sdl:*red*)
			  
			  (sdl:draw-filled-polygon
			   (list (sdl:point :x 0 :y (aref p-s-octagon 1))
				 (sdl:point :x (- (aref p-se-rect 0) 1) :y (aref p-s-octagon 1))
				 (sdl:point :x (- (aref p-se-rect 0) 1) :y (aref p-s-negarc 1))
				 (sdl:point :x 0 :y (aref p-s-negarc 1)))
			   :surface final-mask :color color-key)
			  )))


;; Full frame icons:
(defparameter infantry '(progn
			 (sdl:draw-filled-polygon ;; line from upper left to lower right
			  (list
			   (shift-coord nw 0 line-width)
			   nw
			   (shift-coord se 0 (- line-width))
			   se)
			  :surface field-surface :color line-color)
			 (sdl:draw-filled-polygon ;; line from lower left to upper right
			  (list
			   (shift-coord sw 0 (- line-width))
			   sw
			   (shift-coord ne 0 line-width)
			   ne)
			  :surface field-surface :color line-color)))

;; wtf is an "air assault with organic lift"? A bird?
;; Let's just do the more useful ones...
(defparameter air-defense '(let ((curve-y-top-shift (- (floor (* 0.3 octagon-diameter)) (- (aref p-s-octagon 1) (aref se 1))))
				 (temp-mask (sdl:create-surface (compute-field-width octagon-diameter)
								(compute-field-height octagon-diameter)
								:color-key sdl:*red*)))
			    (sdl:fill-surface sdl:*red* :surface temp-mask)
			    (sdl:draw-filled-ellipse  ;; line
			     ;; s
			     (sdl:point
			      :x (aref s 0)
			      :y (aref se 1))                         ;; centre of ellipse
			     (floor (- (aref se 0) (aref sw 0)) 2)    ;; x-radius
			     curve-y-top-shift                        ;; y-radius
			     :surface temp-mask :color line-color)
			    (sdl:draw-filled-ellipse ;; fill line's underside
			     ;; s
			     (sdl:point
			      :x (aref s 0)
			      :y (aref se 1))
			     (- (floor (- (aref se 0) (aref sw 0)) 2) line-width)
			     (- curve-y-top-shift line-width)
			     :surface temp-mask :color sdl:*red*)
			    (sdl:draw-filled-polygon (list
						      (sdl:point :x 0
								 :y (aref sw 1))
						      (sdl:point :x field-width
								 :y (aref sw 1))
						      (sdl:point :x field-width
								 :y field-height)
						      (sdl:point :x 0
								 :y field-height))
			     :surface temp-mask :color sdl:*red*)
			    (sdl:blit-surface temp-mask field-surface)
			    (sdl:free temp-mask)))

(defparameter anti-tank '(progn
			  (sdl:draw-filled-polygon
			   (list
			    sw
			    (shift-coord sw 0 (- line-width))
			    n
			    (shift-coord se 0 (- line-width))
			    se
			    (shift-coord n 0 line-width))
			   :surface field-surface :color line-color)))

;; the mountain sector 2 modifier is apparently special in that
;; it behaves more like a full frame icon as bottom must touch frame bottom
;; hostile non air/space requires some tricks
(defparameter mountain '(let ((icon-size (* 0.3 octagon-diameter))
			      (icon-bottom-y
			       (cond ((equal affiliation 'hostile)
				      (eval 666)) ;; TODO calculate y of base vertexes
				     (t (aref s 1)))))
			 (sdl:draw-filled-polygon
			  (list
			   (sdl:point :x (floor field-width 2) ;; peak
				      :y (- icon-bottom-y icon-size))
			   (shift-coord s (floor icon-size 2) 0)
			   (shift-coord s (- (floor icon-size 2)) 0))
			  :surface field-surface :color line-color)))


(defun shift-coord (coord-arr shift-arr-x shift-arr-y)
  (make-array
   2 :initial-contents `(,(+ (aref coord-arr 0) shift-arr-x)
			  ,(+ (aref coord-arr 1) shift-arr-y))))


(defun nato-color-init ()
  (defparameter color-key (sdl:color :r 255 :g 0 :b 255))
  (defparameter blue (sdl:color :r 128 :g 224 :b 255))
  (defparameter yellow (sdl:color :r 255 :g 255 :b 128))
  (defparameter green (sdl:color :r 170 :g 255 :b 170))
  (defparameter red (sdl:color :r 255 :g 128 :b 128))
  (defparameter black (sdl:color :r 0 :g 0 :b 0))
  (defparameter white (sdl:color :r 239 :g 239 :b 239)))

(defun compute-field-width (octagon-diameter)
  (floor (* octagon-diameter 1.5)))

(defun compute-field-height (octagon-diameter)
  (floor (* octagon-diameter 1.74)))

(defun nato-dimension-init (octagon-diameter)
  (let* ((x-shift (* 0.75 octagon-diameter)) ;; refers to north vertex of central octagon
	 (y-shift (* 0.37 octagon-diameter)) ;; -''-
	 (octagon-radius (/ octagon-diameter 2))
	 (sin45 (* (sin (* pi 1/4)) octagon-radius))
	 (sin45b (- octagon-radius sin45))
	 (diamond-extrusion (- (sqrt (* octagon-radius octagon-radius 2))
			       octagon-radius)) ;; Extremities of sharp hostiles
	 (hat-y (- octagon-radius
		   (* (tan (* pi 1/4))
		      (- octagon-radius diamond-extrusion)))) ;; the corners between wall and roof of hostile air/sub
	 (arc-x (* 1.1 octagon-radius)) ;; The x-coordinates for negative and positive arc beginnings

	 )

    (defparameter n nil) (defparameter ne nil)
    (defparameter e nil) (defparameter se nil)
    (defparameter s nil) (defparameter sw nil)
    (defparameter w nil) (defparameter nw nil)

    (defparameter field-width (floor (* octagon-diameter 1.5)))
    (defparameter field-height (floor (* octagon-diameter 1.74)))

    (defparameter p-centre-octagon (sdl:point :x x-shift :y (+ y-shift octagon-radius)))
    (defparameter octagon-diameter octagon-diameter)
    (defparameter octagon-radius (/ octagon-diameter 2))
    
    ;; Origin is top vertex of octagon
    (defparameter p-n-octagon (sdl:point :x (+ 0 x-shift) :y (+ 0 y-shift))) ; unused?
    (defparameter p-ne-octagon (sdl:point :x (+ sin45 x-shift) :y (+ sin45b y-shift)))
    (defparameter p-e-octagon (sdl:point :x (+ octagon-radius x-shift) :y (+ octagon-radius y-shift))) ; unused?
    (defparameter p-se-octagon (sdl:point :x (+ sin45 x-shift)
					  :y (+ (+ octagon-radius sin45) y-shift)))
    (defparameter p-s-octagon (sdl:point :x (+ 0 x-shift) :y (+ octagon-diameter y-shift))) ; unused?
    (defparameter p-sw-octagon (sdl:point :x (+ (- sin45) x-shift)
					  :y (+ (+ octagon-radius sin45) y-shift)))
    (defparameter p-w-octagon (sdl:point :x (+ (- octagon-radius) x-shift) :y (+ octagon-radius y-shift)))
    (defparameter p-nw-octagon (sdl:point :x (+ (- sin45) x-shift) :y (+ sin45b y-shift)))

    (defparameter p-sw-posarc (sdl:point :x (+ (- arc-x) x-shift)
					 :y (+ octagon-diameter y-shift)))
    (defparameter p-se-posarc (sdl:point :x (+ arc-x x-shift)
					 :y (+ octagon-diameter y-shift)))
    (defparameter p-n-posarc (sdl:point :x x-shift :y 0))
    (defparameter p-centre-posarc (sdl:point :x x-shift :y (+ y-shift octagon-diameter)))

    (defparameter p-nw-posarc (sdl:point :x (+ (- arc-x) x-shift) :y y-shift))
    (defparameter p-ne-posarc (sdl:point :x (+ arc-x x-shift) :y y-shift))
    
    (defparameter p-nw-negarc (sdl:point :x (+ (- arc-x) x-shift)
					 :y (+ 0 y-shift)))
    (defparameter p-ne-negarc (sdl:point :x (+ arc-x x-shift)
					 :y (+ 0 y-shift)))
    (defparameter p-s-negarc (sdl:point :x x-shift :y (+ y-shift (* 1.37 octagon-diameter))))
    (defparameter p-centre-negarc (sdl:point :x x-shift :y y-shift))

    (defparameter p-sw-negarc (sdl:point :x (+ (- arc-x) x-shift) :y (+ octagon-diameter y-shift)))
    (defparameter p-se-negarc (sdl:point :x (+ arc-x x-shift) :y (+ octagon-diameter y-shift)))

    (defparameter p-ne-box (sdl:point :x (+ octagon-radius x-shift) :y (+ 0 y-shift)))
    (defparameter p-se-box (sdl:point :x (+ octagon-radius x-shift) :y (+ octagon-diameter y-shift)))
    (defparameter p-sw-box (sdl:point :x (+ (- octagon-radius) x-shift) :y (+ octagon-diameter y-shift)))
    (defparameter p-nw-box (sdl:point :x (+ (- octagon-radius) x-shift) :y (+ 0 y-shift)))

    (defparameter p-n-diamond (sdl:point :x (+ 0 x-shift)
					 :y (+ (- diamond-extrusion) y-shift)))
    (defparameter p-e-diamond (sdl:point :x (+ (+ octagon-radius diamond-extrusion) x-shift)
					 :y (+ octagon-radius y-shift)))
    (defparameter p-s-diamond (sdl:point :x (+ 0 x-shift)
					 :y (+ (+ octagon-diameter diamond-extrusion) y-shift)))
    (defparameter p-w-diamond (sdl:point :x (+ (- (+ octagon-radius diamond-extrusion)) x-shift)
					 :y (+ octagon-radius y-shift)))

    (defparameter p-nw-poshat (sdl:point :x (+ (- octagon-radius) x-shift)
					 :y (+ hat-y y-shift)))
    (defparameter p-ne-poshat (sdl:point :x (+ octagon-radius x-shift)
					 :y (+ hat-y y-shift)))
    (defparameter p-sw-neghat (sdl:point :x (+ (- octagon-radius) x-shift)
					 :y (+ (- octagon-diameter hat-y) y-shift)))
    (defparameter p-se-neghat (sdl:point :x (+ octagon-radius x-shift)
					 :y (+ (- octagon-diameter hat-y) y-shift)))

    (defparameter p-ne-rect (sdl:point :x (+ (* octagon-radius 1.5) x-shift)
				       :y (+ 0 y-shift)))
    (defparameter p-se-rect (sdl:point :x (+ (* octagon-radius 1.5) x-shift)
				       :y (+ octagon-diameter y-shift)))
    (defparameter p-sw-rect (sdl:point :x (+ (* octagon-radius -1.5) x-shift)
				       :y (+ octagon-diameter y-shift)))
    (defparameter p-nw-rect (sdl:point :x (+ (* octagon-radius -1.5) x-shift)
				       :y (+ 0 y-shift)))

    (defparameter p-n-flower (sdl:point :x (aref p-n-octagon 0)
					:y (- (aref p-nw-octagon 1) ;; y at top of upper half circle
					      (/ (- (aref p-ne-octagon 0)
						    (aref p-nw-octagon 0))
						 2))))
    (defparameter p-s-flower (sdl:point :x (aref p-n-flower 0)
					:y (- field-height (aref p-n-flower 1))))
    (defparameter p-w-flower (sdl:point :x (aref p-n-flower 1)
					:y (aref p-w-octagon 1)))
    (defparameter p-e-flower (sdl:point :x (- field-width (aref p-w-flower 0))
					:y (aref p-w-octagon 1)))))
