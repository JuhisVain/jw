(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-gfx)

(defvar *oct-r* 50)


(defstruct unit-data
  (affiliation) ;; Friendly, neutral, hostile, unknown
  (dimension)   ;; Air, land-unit, land-equipment, sea surface, sub
  (role)        ;; Infantry, cavalry, submarine etc..
  (size)        ;; Squad, corps, army etc..
  (mobility)    ;; Wheeled, halftrack etc..
  (hq))         ;; 


(defun nato-gen ()
  (let ((width 200)
	(height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "counter generator test")
      (sdl:clear-display sdl:*black*)
      
      (sdl:with-events ()
	(:quit-event () t)

	(:idle ()

	       (sdl:draw-surface-at-* (draw-symbol-frame 'test) 0 0)
	       (sdl:update-display))))))

(defun draw-symbol-frame (unit-data)
  (let ((frame-surface (sdl:create-surface  (* *oct-r* 2)
					    (* *oct-r* 2)
					    :alpha 254 :pixel-alpha t)))
    (sdl:draw-filled-circle-* 50 50 49 :surface frame-surface :color sdl:*blue*)

    
    (let* ((oct-up (- *oct-r*
		      (* (cos (* pi 0.25)) *oct-r*)))
	   (oct-down (- (* 2 *oct-r*) oct-up))
	   (oct-left (- *oct-r*
			(* (sin (* pi 0.25)) *oct-r*)))
	   (oct-right (- (* 2 *oct-r*) oct-left)))

      ;;octagon to represent primary drawing area:
      (sdl:draw-filled-polygon (list (sdl:point :x *oct-r* :y 0) ;;top
				     (sdl:point :x oct-left :y oct-up)
				     (sdl:point :x oct-right :y oct-up))
			       :color sdl:*white* :surface frame-surface)
      (sdl:draw-filled-polygon (list (sdl:point :x oct-left :y oct-up) ;; center left top
				     (sdl:point :x oct-right :y oct-up)
				     (sdl:point :x oct-left :y oct-down))
			       :color sdl:*white* :surface frame-surface)
      (sdl:draw-filled-polygon (list (sdl:point :x oct-right :y oct-up) ;;center right bottom
				     (sdl:point :x oct-left :y oct-down)
				     (sdl:point :x oct-right :y oct-down))
			       :color sdl:*white* :surface frame-surface)
      (sdl:draw-filled-polygon (list (sdl:point :x 0 :y *oct-r*) ;; left
				     (sdl:point :x oct-left :y oct-up)
				     (sdl:point :x oct-left :y oct-down))
			       :color sdl:*white* :surface frame-surface)
      (sdl:draw-filled-polygon (list (sdl:point :x (* 2 *oct-r*) :y *oct-r*) ;; right
				     (sdl:point :x oct-right :y oct-up)
				     (sdl:point :x oct-right :y oct-down))
			       :color sdl:*white* :surface frame-surface)
      (sdl:draw-filled-polygon (list(sdl:point :x oct-left :y oct-down) ;; bottom
				    (sdl:point :x oct-right :y oct-down)
				    (sdl:point :x *oct-r* :y (* 2 *oct-r*)))
			       :color sdl:*white*  :surface frame-surface)
      )
    frame-surface))
