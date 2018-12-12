(in-package :war)

(defvar *tile-template*)

;; Since we are creating a hexagon sitting on it's side
;; (sitting side's length: hor-line-length), intending to
;; stack these in columns, the tiling will happen on x-axis
;; at multiples of
;; ((tile-width - hor-line-length) / 2) + hor-line-length
;; and on y-axis at tile-height
;; possibly recommended to keep args as even numbers
(defun chop-test (&optional (hor-line-length 76) (tile-width 128) (tile-height 104))
  (sdl:with-init ()
    (sdl:initialise-default-font)
    (let* ((left-border-tri-x (floor (- tile-width hor-line-length) 2))
	   (right-border-tri-x (+ left-border-tri-x hor-line-length -1)) ;; More like right-border minus tri-x
	   (middle-height-lower (floor tile-height 2))
	   (middle-height-upper (- middle-height-lower 1))
	   
	   

	   (temp-win (sdl:window 300 300 :title-caption "test"))
	   (template (sdl:create-surface
		      tile-width tile-height
		      :color-key sdl:*white*))
	   (final-color-key (sdl:color :r 255 :g 0 :b 255)))
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
      
      
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	       (sdl:update-surface template)
	       (sdl:save-image template "template.bmp")
	       (sdl:push-quit-event))))))
