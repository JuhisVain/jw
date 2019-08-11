(in-package #:war)

(defvar *panel-width* 200)
(defparameter *panel* nil)

(defparameter *panel-unitlist-height* 450) ;; Height of whole unit list on panel
(defparameter *panel-list-height* 50) ;; Height of 'unit' in selected tile's unit list

(defparameter *panel-setup* nil)

(defstruct panel
  (x nil)
  (y nil)
  (width nil)
  (height nil)
  (elements nil))

(defstruct OBSOLETEpanel-button
  (position nil)
  (width nil)
  (height nil)
  (icon nil)
  (action nil))

(defstruct OBSOLETEpanel-list
  (position nil)
  (width nil)
  (height nil)
  (display nil)
  (element-height)
  (action nil))

(defclass panel-button ()
  ((x
    :accessor panel-element-x
    :initarg :x)
   (y
    :accessor panel-element-y
    :initarg :y)
   (width
    :accessor panel-element-width
    :initarg :width)
   (height
    :accessor panel-element-height
    :initarg :height)
   (icon
    :accessor panel-button-icon
    :initarg :icon)
   (action
    :accessor panel-button-action
    :initarg :action)
   (parent
    :accessor panel-button-parent
    :initarg :parent)
   ))

(defmethod display ((button panel-button))
  (sdl:draw-surface-at-* (panel-button-icon button)
			 (+ (panel-element-x button) (panel-x (panel-button-parent button)))
			 (+ (panel-element-y button) (panel-y (panel-button-parent button)))
			 :surface window))

(defmethod click ((button panel-button) ) ;; Might wanna use mouse state later
  (funcall (panel-button-action button)))

;; Some more sensible names in here might be good
(defclass panel-list ()
  ((x
    :accessor panel-element-x
    :initarg :x)
   (y
    :accessor panel-element-y
    :initarg :y)
   (width
    :accessor panel-element-width
    :initarg :width)
   (height
    :accessor panel-element-height
    :initarg :height)
   (element-count
    :accessor panel-list-element-count
    :initarg :element-count)
   (element-height
    :accessor panel-list-element-height
    :initarg :element-height)
   (element-width
    :accessor panel-list-element-width
    :initarg :element-width)
   (element-display-fun ; Evals to graphics: needs to take one argument: index of list's element
    :accessor panel-list-element-display-fun
    :initarg :element-display-fun)
   (action
    :accessor panel-list-action
    :initarg :action)
   (parent
    :accessor panel-list-parent
    :initarg :parent)))

(defmethod display ((list panel-list))
  (dotimes (index (panel-list-element-count list))
    (let ((graphics (apply (panel-list-element-display-fun list)
			   (list index))))
      (if (not graphics) (return-from display))
      (sdl:draw-surface-at-* graphics
			     (+ (panel-element-x list) (panel-x (panel-list-parent list)))
			     (+ (panel-element-y list)
				(panel-y (panel-list-parent list))
				(* index (panel-list-element-height list)))
			     :surface window))))

(defmacro create-panel (x y width height &rest elements)
  ;; Sorting the elements from greatest to smallest
  ;;   -> will end up in panel slot smallest to greatest
  (let ((panel (gensym))
	(element (gensym))
	(sorted-elements
	 (sort elements #'(lambda (one two)
			    (cond ((> (getf one :y) (getf two :y)) t)
			          ((equal (getf one :y) (getf two :y))
				   (> (getf one :x) (getf two :x)))
				  (t nil))))))

    `(let ((,panel (make-panel :x ,x :y ,y
			      :width ,width :height ,height
			      :elements nil)))
       (dolist (,element ',sorted-elements)
	 (setf ,element (nconc ,element (list :parent ,panel)))
	 (setf (panel-elements ,panel)
	       (push (eval ,element) (panel-elements ,panel))))
       ,panel))) ;; panel panel panel PANEL PANEL PANEL

(defun setup-panels ()
  (setf *panel-setup* nil)
  ;; TODO: pushnew seems to work here for now butbutbut...
  ;; also will need to do testing with sdl-image:load-image
  (let ((new-panel 
	 (create-panel (- (sdl:width window) 200)
		       0
		       200
		       (sdl:height window)
		       (make-instance 'panel-button
				      :x 4 :y 4
				      :width 48 :height 48
				      :icon (sdl-image:load-image "graphics/PANEL_BUTTON_TEST.png")
				      :action #'(lambda () (format t "~&Moo!~%")))
		       (make-instance 'panel-button
				      :x 56 :y 4
				      :width 48 :height 48
				      :icon (sdl-image:load-image "graphics/PANEL_BUTTON_TEST.png")
				      :action #'(lambda () (format t "~&Quack quack!~%")))
		       (make-instance 'panel-button
				      :x 108 :y 4
				      :width 48 :height 48
				      :icon (sdl-image:load-image "graphics/PANEL_BUTTON_TEST.png")
				      :action #'(lambda () (set-test-unit)))
		       (make-instance 'panel-button
				      :x 156 :y 4
				      :width 48 :height 48
				      :icon (sdl-image:load-image "graphics/PANEL_BUTTON_TEST.png")
				      :action #'(lambda () (set-test-enemy-unit)))
  		       (make-instance 'panel-list
				      :x 4 :y 56
				      :width 150 :height 300
				      :element-count 5
				      :element-height 60
				      :element-display-fun
				      #'(lambda (index)
					; Pretty inefficient ps. and dumb
					  (block panel-list-lambda
					    (graphics-surface
					     (army-counter
					      (or
					       (nth index
						    (tile-units
						     (or
						      (tile-at (car selected-tile) (cdr selected-tile))
						      (return-from panel-list-lambda nil))))
					       (return-from panel-list-lambda nil))))))))))
    ;; Trying to push create-panel directly won't work
    (push new-panel *panel-setup*)))

(defun draw-panels ()
    (mapcar #'(lambda (panel)
		;; Frame of this panel
		(sdl:draw-filled-polygon
		 (list (sdl:point :x (panel-x panel)
				  :y (panel-y panel))
		       (sdl:point :x (+ (panel-x panel) (panel-width panel))
				  :y (panel-y panel))
		       (sdl:point :x (+ (panel-x panel) (panel-width panel))
				  :y (+ (panel-y panel) (panel-height panel)))
		       (sdl:point :x (panel-x panel)
				  :y (+ (panel-y panel) (panel-height panel))))
		 :surface window :color sdl:*red*)

		(mapcar #'(lambda (element)
				   (display element))
			(panel-elements panel)))
     *panel-setup*))

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


(defun draw-unit-list (selected-tile selected-unit panel-list)
  (let ((panel-element-height (panel-list-height panel-list))
	(panel-list-element-height (panel-list-element-height panel-list))
	(panel-list-width (panel-list-width panel-list)))
    (if (or (null selected-tile) (< (car selected-tile) 0) (< (cdr selected-tile) 0))
	(return-from draw-unit-list))
    (do* ((unit-list (reverse (tile-units (tile-at
					   (car selected-tile) (cdr selected-tile))))
		     (cdr unit-list))
	  (unit (car unit-list) (car unit-list))
	  (unit-shift 0 (+ unit-shift panel-list-element-height)))
	 ((or (null unit) (>= unit-shift panel-list-height)))

      (if (eq unit selected-unit) ; indicating selected unit
	  (sdl:draw-rectangle-* (panel-list-x panel-list)
				(+ (panel-list-y panel-list) unit-shift)
				(panel-list-element-width panel-list)
				(panel-list-element-height panel-list)
				:surface window :color sdl:*black*))
      
      (sdl:draw-surface-at-* (graphics-surface (army-counter unit))
			     (- (sdl:width window) *panel-width*)
			     (+ 0 unit-shift)
			     :surface window)

      (sdl:draw-string-solid-* (write-to-string (car (army-troops unit)))
			       (+ (- (sdl:width window) *panel-width*) 100)
			       (+ 25 unit-shift)
			       :surface window :color sdl:*white*)
      )))

(defun click-panel (mouse-button mouse-button-state mouse-x mouse-y
		    &optional (panel (car *panel-setup*)))
  (let ((relative-x (- mouse-x (panel-x panel)))
	(relative-y (- mouse-y (panel-y panel))))
    (dolist (element (panel-elements panel))
      (cond ((typep element 'panel-button)
	     (and ; Depending on the alignment of panel these could be optimized to some other order
	      (> relative-x (panel-element-x element))
	      (<= relative-x (+ (panel-element-x element)
				(panel-element-width element)))
	      (> relative-y (panel-element-y element))
	      (<= relative-y (+ (panel-element-y element)
				(panel-element-height element)))
	      (funcall (panel-button-action element))))))))

(defun select-from-panel (mouse-y selected-tile)
  (nth (floor mouse-y *panel-list-height*) (reverse (tile-units selected-tile))))
