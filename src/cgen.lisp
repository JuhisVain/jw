(in-package :war)

(ql:quickload :vecto)

(defvar *nato-symbol-lib* (make-hash-table :test 'equal))

(defmacro find-first (predicate item-list list)
  (let ((element (gensym)))
    `(dolist (,element ,list)
       (if (or
	    ,@(mapcar #'(lambda (x) `(,predicate ,element ',x)) item-list))
	   (return ,element)))))

;; Return element from symbol lib if found, otherwise generate it first.
(defun description-to-surface (width height description)
  (setf description (sort description #'string< :key #'symbol-name))
  (or (gethash (cons (cons width height) description) *nato-symbol-lib*)
      (generate-natosymbol-from width height description)))
  
(defun generate-natosymbol-from (width height description)
  (let* ((octagon-dia 100)
	 (octagon-rad (floor octagon-dia 2))
	 (field-width (floor (* 1.5 octagon-dia)))
	 (field-height (floor (* 1.74 octagon-dia)))
	 (centre-x (floor field-width 2))
	 (centre-y (floor field-height 2))
	 (sin45 (* octagon-rad (sin (/ pi 4))))
	 (affiliation
	  (find-first eq (friendly hostile neutral unknown) description))
	 (dimension
	  (find-first eq (air space land surface subsurface
			      equipment installation activity)
		      description)))
    (vecto:with-canvas (:width field-width :height field-height)
      (vecto:set-rgb-fill 1.0 0.0 1.0) ; color key for sdl
      (vecto:clear-canvas) ; fill all with color key
      
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
	    (sw-y)(se-x)(se-y)(w-x)(w-y)(e-x)(e-y)
	    
	    (fill-color '(1.0 1.0 1.0))
	    (line-color '(0.0 0.0 0.0)))
	
	(cond ((eq affiliation 'friendly)
	       (vecto:set-rgb-fill (/ 128 255) (/ 224 255) 1.0) ; blue
	       (vecto:set-rgb-stroke 0 0 0) ; black
	       (cond ((eq dimension 'land);friendly land: rectangle
		      (setf
		       n-y (+ octagon-rad centre-y)
		       w-x 0
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
		      (vecto:rectangle sw-x sw-y field-width octagon-dia))
		     ((eq dimension 'air) ; friendly air: half ellipse, open bottom
		      (setf
		       n-y field-height
		       w-x oct-w-x
		       w-y centre-y
		       e-x oct-e-x
		       e-y centre-y
		       nw-x (car oct-nw)
		       nw-y (+ octagon-rad centre-y)
		       s-y (- centre-y octagon-rad)
		       sw-x (- w-x (* 0.05 octagon-rad))
		       sw-y s-y
		       ne-x (car oct-ne)
		       ne-y nw-y
		       se-x (+ e-x (* 0.05 octagon-rad))
		       se-y s-y)

		      (vecto:ellipse-arc centre-x s-y
					 (* 1.05 octagon-rad)
					 (* 1.37 octagon-dia)
					 0 0 (* 2 pi))

		      (vecto:fill-and-stroke)

		      ;; Can't figure out clipping or making open half-ellipses: just draw a bottom
		      (vecto:set-rgb-fill 1.0 0.0 1.0) ; color-key
		      (vecto:move-to 0 s-y)
		      (vecto:line-to field-width s-y )
		      (vecto:line-to field-width 0)
		      (vecto:line-to 0 0)
		      (vecto:close-subpath)
		      
		      (vecto:fill-path)

		      (vecto:set-rgb-fill (/ 128 255) (/ 224 255) 1.0) ; blue

		      ;;TEST remove
		      ;;(vecto:fill-and-stroke)
		      ;;(vecto:move-to nw-x nw-y)
		      ;;(vecto:line-to se-x se-y)
		      ;;(vecto:stroke)
		      ;;(vecto:save-png "vectosave")
		      ))))
	))))



