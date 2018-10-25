(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-gfx)

(defstruct unit-data
  (affiliation) ;; Friendly, neutral, hostile, unknown
  (dimension)   ;; Air, land-unit, land-equipment, sea surface, sub
  (icon)        ;; Infantry, cavalry, submarine etc..
  (size)        ;; Squad, corps, army etc..
  (mobility)    ;; Wheeled, halftrack etc..
  (hq))         ;; 


(defun nato-gen ()
  (let ((width 200)
	(height 200))
    (sdl:with-init ()
      (let ((main-win (sdl:window width height :title-caption "counter generator test")))
	(setf (sdl:frame-rate) 10)
	(sdl:clear-display sdl:*black*)
	
	(sdl:with-events ()
	  (:quit-event () t)

	  (:idle ()

		 ;;(sdl:draw-surface-at-* (draw-symbol-frame 'test) 0 0)
		 ;;(sdl:draw-ellipse-* 100 100 75 25 :surface main-win :color sdl:*white*)
		 (sdl:draw-curve (list (sdl:point :x 50 :y 1000)
				       (sdl:point :x 50 :y 100) ;;from
				       (sdl:point :x 150 :y 100) ;;to
				       (sdl:point :x 150 :y 1000))
				 :surface main-win :color sdl:*white*)
		 (sdl:update-display)))))))

(defun nato-dimension-init (octagon-diameter)
  (let* ((octagon-radius (/ octagon-diameter 2))
	 (sin45 (* (sin (* pi 1/4)) octagon-radius))
	 (sin45b (- octagon-radius sin45))
	 (diamond-extrusion (- octagon-diameter
			       (sqrt (* octagon-radius octagon-radius 2))))
	 (hat-y (- octagon-radius
		   (* (tan (* pi 1/4))
		      (- octagon-radius diamond-extrusion)))))
    ;; Origin is top vertex of octagon
    (defparameter p-n-octagon (sdl:point :x 0 :y 0)) ; unused?
    (defparameter p-ne-octagon (sdl:point :x sin45 :y sin45b))
    (defparameter p-e-octagon (sdl:point :x octagon-radius :y octagon-radius)) ; unused?
    (defparameter p-se-octagon (sdl:point :x sin45
					  :y (+ octagon-radius sin45)))
    (defparameter p-s-octagon (sdl:point :x 0 :y octagon-diameter)) ; unused?
    (defparameter p-sw-octagon (sdl:point :x (- sin45)
					  :y (+ octagon-radius sin45)))
    (defparameter p-w-octagon (sdl:point :x (- octagon-radius) :y octagon-radius))
    (defparameter p-nw-octagon (sdl:point :x (- sin45) :y sin45b))

    (defparameter p-sw-posarc (sdl:point :x (* -1.1 octagon-radius)
					 :y octagon-diameter))
    (defparameter p-se-posarc (sdl:point :x (* 1.1 octagon-radius)
					 :y octagon-diameter))
    (defparameter p-nw-negarc (sdl:point :x (* -1.1 octagon-radius)
					 :y 0))
    (defparameter p-ne-negarc (sdl:point :x (* 1.1 octagon-radius)
					 :y 0))

    (defparameter p-ne-box (sdl:point :x octagon-radius :y 0))
    (defparameter p-se-box (sdl:point :x octagon-radius :y octagon-radius))
    (defparameter p-sw-box (sdl:point :x (- octagon-radius) :y octagon-radius))
    (defparameter p-nw-box (sdl:point :x (- octagon-radius) :y 0))

    (defparameter p-n-diamond (sdl:point :x 0
					 :y (- diamond-extrusion)))
    (defparameter p-e-diamond (sdl:point :x (+ octagon-radius diamond-extrusion)
					 :y octagon-radius))
    (defparameter p-s-diamond (sdl:point :x 0
					 :y (+ octagon-diameter diamond-extrusion)))
    (defparameter p-w-diamond (sdl:point :x (- (+ octagon-radius diamond-extrusion))
					 :y octagon-radius))

    (defparameter p-nw-poshat (sdl:point :x (- octagon-radius)
					 :y hat-y))
    (defparameter p-ne-poshat (sdl:point :x octagon-radius
					 :y hat-y))
    (defparameter p-sw-neghat (sdl:point :x (- octagon-radius)
					 :y (- octagon-diameter hat-y)))
    (defparameter p-se-neghat (sdl:point :x octagon-radius
					 :y (- octagon-diameter hat-y)))

    (defparameter p-ne-rect (sdl:point :x (* octagon-radius 1.25)
				       :y 0))
    (defparameter p-se-rect (sdl:point :x (* octagon-radius 1.25)
				       :y octagon-diameter))
    (defparameter p-sw-rect (sdl:point :x (* octagon-radius -1.25)
				       :y octagon-diameter))
    (defparameter p-nw-rect (sdl:point :x (* octagon-radius -1.25)
				       :y 0))))

(defun draw-symbol-frame (unit-data)
  (let ((frame-surface (sdl:create-surface  *field-dim*
					    *field-dim*
					    :alpha 254 :pixel-alpha t)))

    
    ))
