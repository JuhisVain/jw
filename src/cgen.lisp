(in-package :war)

(ql:quickload :vecto)

(defvar *nato-symbol-lib* (make-hash-table :test 'equal))

(defmacro OBSfind-first (predicate item-list list)
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
	    
	    (n-y)(nw-x)(nw-y)(ne-x)(ne-y)(s-y)(sw-x)
	    (sw-y)(se-x)(se-y)(w-x)(w-y)(e-x)(e-y)
	    
	    (fill-color '(1.0 1.0 1.0))
	    (line-color '(0.0 0.0 0.0)))
	
	(cond ((eq affiliation 'friendly)
	       (setf fill-color (list (/ 1.0 3) 1.0 (/ 1.0 3)))
	       ; line color already set up
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
		       se-y s-y
		       ))))
	))))



