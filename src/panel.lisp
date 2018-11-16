(in-package #:war)

(defvar *panel-width* 200)
(defparameter *panel* nil)

(defun draw-panel (selected-tile selected-unit)
  (sdl:draw-filled-polygon
   (list (sdl:point :x (- (sdl:width window)
			  *panel-width*)
		    :y 0)
	 (sdl:point :x (sdl:width window)
		    :y 0)
	 (sdl:point :x (sdl:width window)
		    :y (sdl:height window))
	 (sdl:point :x (- (sdl:width window)
			  *panel-width*)
		    :y (sdl:height window)))
   :surface window :color sdl:*red*)
  (draw-unit-list selected-tile selected-unit))

(defparameter *panel-list-height* 100)

(defun draw-unit-list (selected-tile selected-unit)
  (if (or (null selected-tile) (< (car selected-tile) 0) (< (cdr selected-tile) 0))
      (return-from draw-unit-list))
  (do* ((unit-list (reverse (tile-units (tile-at
					 (car selected-tile) (cdr selected-tile))))
		   (cdr unit-list))
	(unit (car unit-list) (car unit-list))
	(unit-shift 0 (+ unit-shift *panel-list-height*)))
       ((null unit))

    (if (eq unit selected-unit)
	(sdl:draw-rectangle-* (- (sdl:width window) *panel-width*) (+ 0 unit-shift)
			      100 100 :surface window :color sdl:*black*))
    
    (sdl:draw-surface-at-* (graphics-surface (army-counter unit))
			   (- (sdl:width window) *panel-width*)
			   (+ 0 unit-shift)
			   :surface window)

    (sdl:draw-string-solid-* (write-to-string (car (army-troops unit)))
			     (+ (- (sdl:width window) *panel-width*) 100)
			     (+ 25 unit-shift)
			     :surface window :color sdl:*white*)
    ))

(defun select-from-panel (mouse-y selected-tile)
  (nth (floor mouse-y *panel-list-height*) (reverse (tile-units selected-tile))))
