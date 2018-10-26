(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-gfx)

;; Supposed to work something like:
;; (CREATE friendly land mountain infantry corps halftrack) -> pukes out sdlsurface

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


(defun nato-gen (octagon-diameter)
  (nato-dimension-init octagon-diameter)
  (let ((width 500)
	(height 500))
    (sdl:with-init ()
      (let ((main-win (sdl:window width height :title-caption "counter generator test")))
	(setf (sdl:frame-rate) 10)
	(sdl:clear-display sdl:*black*)
	
	(sdl:with-events ()
	  (:quit-event () t)

	  (:idle ()

		 (sdl:draw-rectangle-* 0 0 field-width field-height
				       :surface main-win :color sdl:*red*)

		 (sdl:draw-surface (create-nato-symbol octagon-diameter friendly land)
				   :surface main-win)
		 
		 (sdl:update-display)))))))

(defun create-field (octagon-diameter)
  (nato-dimension-init octagon-diameter)
  (let ((field-surface (sdl:create-surface field-width field-height)))
    ))

(defparameter friendly '(progn
			 (setf line-color black)
			 (setf line-width 2)
			 (setf fill-color blue)))

(defparameter neutral '(progn
			(setf line-color black)
			(setf line-width 2)
			(setf fill-color green)))

(defparameter land '(cond ((equal affiliation friendly)
			   (sdl:draw-filled-polygon
			    (list p-nw-rect p-ne-rect p-se-rect p-sw-rect)
			    :surface field-surface :color line-color)
			   (sdl:draw-filled-polygon
			    (list (shift-coord p-nw-rect line-width line-width)
				  (shift-coord p-ne-rect (* 2 (- line-width)) line-width) ;;CLOSE ENOUGH
				  (shift-coord p-se-rect (* 2 (- line-width)) (* 2 (- line-width)))
				  (shift-coord p-sw-rect line-width (* 2 (- line-width))))
			    :surface field-surface :color fill-color))))


(defmacro create-nato-symbol (octagon-diameter affiliation dimension)
  `(let ((line-color) (line-width) (fill-color)
	 (field-surface (sdl:create-surface (compute-field-width ,octagon-diameter)
					    (compute-field-height ,octagon-diameter)
					    :color-key color-key ))
	 (affiliation ,affiliation))
     (sdl:clear-display color-key :surface field-surface)
     ,(eval affiliation)
     ,(eval dimension)
     field-surface
     ))

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
  (let* ((x-shift (* 0.75 octagon-diameter))
	 (y-shift (* 0.37 octagon-diameter))
	 (octagon-radius (/ octagon-diameter 2))
	 (sin45 (* (sin (* pi 1/4)) octagon-radius))
	 (sin45b (- octagon-radius sin45))
	 (diamond-extrusion (- octagon-diameter
			       (sqrt (* octagon-radius octagon-radius 2))))
	 (hat-y (- octagon-radius
		   (* (tan (* pi 1/4))
		      (- octagon-radius diamond-extrusion)))))

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

    (defparameter p-sw-posarc (sdl:point :x (+ (* -1.1 octagon-radius) x-shift)
					 :y (+ octagon-diameter y-shift)))
    (defparameter p-se-posarc (sdl:point :x (+ (* 1.1 octagon-radius) x-shift)
					 :y (+ octagon-diameter y-shift)))
    (defparameter p-centre-posarc (sdl:point :x x-shift :y (+ y-shift octagon-diameter)))
    (defparameter p-nw-negarc (sdl:point :x (+ (* -1.1 octagon-radius) x-shift)
					 :y (+ 0 y-shift)))
    (defparameter p-ne-negarc (sdl:point :x (+ (* 1.1 octagon-radius) x-shift)
					 :y (+ 0 y-shift)))
    (defparameter p-centre-negarc (sdl:point :x x-shift :y y-shift))

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

(defun draw-symbol-frame (octagon-diameter unit)
  (let ((frame-surface (sdl:create-surface  (* octagon-diameter 1.5)
					    (* octagon-diameter 3) ;; dunno how high air/subs go
					    :alpha 254 :pixel-alpha t)))

    
    
    ))
