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


(defmacro create-nato-symbol (octagon-diameter affiliation dimension icon-list)
  `(let ((line-color) (line-width) (fill-color)
	 (field-surface (sdl:create-surface (compute-field-width ,octagon-diameter)
					    (compute-field-height ,octagon-diameter)
					    :color-key color-key))
	 (affiliation ,affiliation)
	 (n) (ne) (e) (se) (s) (sw) (w) (nw)
	 (final-mask (sdl:create-surface (compute-field-width ,octagon-diameter)
					    (compute-field-height ,octagon-diameter)
					    :color-key sdl:*red*)))
     (sdl:fill-surface color-key :surface final-mask)
     (sdl:clear-display color-key :surface field-surface)
     
     ,(eval affiliation)
     ,(eval dimension)

     (progn ,@(mapcar #'eval icon-list)) ;; ooga booga
     
     (sdl:blit-surface final-mask field-surface) ;; <- modifies second by first arg
     (sdl:free final-mask)
     field-surface
     ))


(defun nato-gen (octagon-diameter)
  (nato-dimension-init octagon-diameter)
  (nato-color-init)
  (let ((width 500)
	(height 500))
    (sdl:with-init ()
      (let ((main-win (sdl:window width height :title-caption "counter generator test"))
	    (field-symbol))
	(setf (sdl:frame-rate) 10)
	(sdl:clear-display sdl:*black*)
	
	(sdl:with-events ()
	  (:quit-event () t)

	  (:idle ()

		 (sdl:draw-rectangle-* 0 0 field-width field-height
				       :surface main-win :color sdl:*red*)

		 (setf field-symbol
		       (create-nato-symbol octagon-diameter friendly air (infantry mountain)))
		 (sdl:draw-surface field-symbol :surface main-win)
		 (sdl:free field-symbol)
		 
		 (sdl:update-display)))))))

(defparameter friendly '(progn
			 (setf line-color black)
			 (setf line-width 2)
			 (setf fill-color blue)))

(defparameter neutral '(progn
			(setf line-color black)
			(setf line-width 2)
			(setf fill-color green)))

(defparameter land '(cond ((equal affiliation friendly) ;; The final mask will need to be set up in these
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
				  (shift-coord se (- (+ 1 line-width)) (* 1 (- line-width)))
				  (shift-coord sw line-width (* 1 (- line-width))))
			    :surface field-surface :color fill-color)
			   (sdl:draw-filled-polygon ;; Final mask
			    (list nw ne se sw)
			    :surface final-mask :color sdl:*red*))))

(defparameter air '(cond ((equal affiliation friendly)
			  (setf sw p-sw-posarc) (setf se p-se-posarc)
			  (setf s p-s-octagon) (setf n p-n-posarc)
			  (setf nw p-nw-posarc) (setf ne p-ne-posarc)
			  (setf e nil) (setf w nil) ;;do later
			  (sdl:draw-filled-ellipse
			   s ;; centre of ellipse
			   (floor (- (aref se 0) (aref sw 0)) 2) ;; x radius aka. semi-major axis
			   (aref p-s-octagon 1) ;; y, semi-minor
			   :surface field-surface :color line-color)
			  (sdl:draw-filled-ellipse
			   s
			   (- (floor (- (aref se 0) (aref sw 0)) 2) (floor line-width 2))
			   (- (aref p-s-octagon 1) (floor line-width 2))
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
(defparameter air-defense '(let ((curve-y-top-shift (floor (* 0.3 octagon-diameter))))
			    (sdl:draw-filled-ellipse                ;; line
			     s                                        ;; centre of ellipse
			     (floor (- (aref se 0) (aref sw 0)) 2)    ;; x-radius
			     curve-y-top-shift                        ;; y-radius
			     :surface field-surface :color line-color)
			    (sdl:draw-filled-ellipse ;; fill line's underside
			     s
			     (- (floor (- (aref se 0) (aref sw 0)) 2) (* 2 line-width))
			     (- curve-y-top-shift line-width)
			     :surface field-surface :color fill-color)))

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
	 (diamond-extrusion (- octagon-diameter
			       (sqrt (* octagon-radius octagon-radius 2)))) ;; Extremities of sharp hostiles
	 (hat-y (- octagon-radius
		   (* (tan (* pi 1/4))
		      (- octagon-radius diamond-extrusion)))) ;; the corners between wall and roof of hostile air/sub
	 (arc-x (* 1.1 octagon-radius)) ;; The x-coordinates for negative and positive arc beginnings

	 )

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
    (defparameter p-nw-octagon (sdl:point :x (- (+ sin45) x-shift) :y (+ sin45b y-shift)))

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
    (defparameter p-se-box (sdl:point :x (+ octagon-radius x-shift) :y (+ octagon-radius y-shift)))
    (defparameter p-sw-box (sdl:point :x (+ (- octagon-radius) x-shift) :y (+ octagon-radius y-shift)))
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
				       :y (+ 0 y-shift)))))
