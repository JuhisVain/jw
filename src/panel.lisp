(in-package #:war)

(defvar *panel-width* 200)
(defparameter *panel* nil)

(defparameter *panel-unitlist-height* 450) ;; Height of whole unit list on panel
(defparameter *panel-list-height* 50) ;; Height of 'unit' in selected tile's unit list


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


(defun draw-unit-list (selected-tile selected-unit)
  (if (or (null selected-tile) (< (car selected-tile) 0) (< (cdr selected-tile) 0))
      (return-from draw-unit-list))
  (do* ((unit-list (reverse (tile-units (tile-at
					 (car selected-tile) (cdr selected-tile))))
		   (cdr unit-list))
	(unit (car unit-list) (car unit-list))
	(unit-shift 0 (+ unit-shift *panel-list-height*)))
       ((or (null unit) (>= unit-shift *panel-unitlist-height*)))

    (if (eq unit selected-unit)
	(sdl:draw-rectangle-* (- (sdl:width window) *panel-width*) (+ 0 unit-shift)
			      100 *panel-list-height* :surface window :color sdl:*black*))
    
    (sdl:draw-surface-at-* (graphics-surface (army-counter unit))
			   (- (sdl:width window) *panel-width*)
			   (+ 0 unit-shift)
			   :surface window)

    (sdl:draw-string-solid-* (write-to-string (car (army-troops unit)))
			     (+ (- (sdl:width window) *panel-width*) 100)
			     (+ 25 unit-shift)
			     :surface window :color sdl:*white*)
    ))

(defun click-panel (mouse-button mouse-button-state mouse-x mouse-y)
  )

(defun select-from-panel (mouse-y selected-tile)
  (nth (floor mouse-y *panel-list-height*) (reverse (tile-units selected-tile))))
