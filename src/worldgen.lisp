(in-package :war)

(defun init-world (width height &key (algo :random) (faction-count 2) (mirror nil))
  (cond ((eq algo :perlin)
	 nil);(make-perlin-world width height faction-count mirror))
	((eq algo :random)
	 (make-random-world width height))
	((eq algo :testing)
	 (make-testing-world width height faction-count mirror))
	))

(defun make-testing-world (width height faction-count mirror)
  (let* ((world (make-world :width (- width 1)
			    :height (- height 1)
			    :map (make-array (list width height))
			    :factions nil))
	 (grid-res 1) ; ???
	 (tw-width (ceiling width grid-res))
	 (tw-height (ceiling height grid-res))
	 (template-world (make-world :width tw-width
				     :height tw-height
				     :map (make-array (list tw-width tw-height))
				     :factions nil)))

    ;; Initialize world with sea tiles:
    (dotimes (x width)
      (dotimes (y height)
	;;(setf (aref (world-map world) x y) (make-tile))
	(setf (tile-at x y world) (make-tile))
	))
    
    ;; Create some random noise to use for breadth-first-fill:
    (dotimes (x tw-width)
      (dotimes (y tw-height)
	(setf (aref (world-map template-world) x y)
	      (make-tile :type (list (random 10))) ; This is the WEIGHT of filling the tile
	      )))

    ;; Select a random coordinate and fire breadth-first-fill at it:
    ;; TODO: select more coordinates for other types of tiles and more land and such
    (let ((fill-these-tiles
	   (breadth-first-fill (cons (random width) (random height))
			       (+ 30 (random 20))
			       template-world)))

      (maphash #'(lambda (key value)
		   (format t "~&k:~a v:~a~%" key value)
		   (setf (tile-type (aref (world-map world) (car key) (cdr key)))
			 (list 'grass)))
	       fill-these-tiles)
      
      )
    world))

(defun breadth-first-fill (start range world)
  (let ((frontier (make-heap))
	(came-from (make-hash-table :test 'equal)))

    (heap-insert frontier start range)
    (setf (gethash start came-from) (list range nil))

    (do ((current))
	((heap-empty frontier))

      (setf current (heap-remove-max frontier))

      (if (> (car current) 0)
	  (dolist (neighbour (mapcar #'(lambda (direction)
					 (neighbour-tile-coords
					  (cadr current)
					  (cddr current)
					  direction world))
				     '(n ne se s sw nw)))
	    
	    (cond ((null neighbour) nil)
		  ((null (gethash neighbour came-from))
		   (let ((move-cost (- (car current)
				       (eval
					(car (last
					      (tile-type (aref (world-map world)
							       (car neighbour)
							       (cdr neighbour)))))))))
		     (cond ((>= move-cost 0)
			    (heap-insert frontier neighbour move-cost)
			    (setf (gethash neighbour came-from)
				  (cons move-cost (cdr current))))))))))
      
      )
    came-from))


(defun make-random-world (width height) ;; the original and worst
  (let ((world (make-world)))
    (setf (world-width world) (- width 1))
    (setf (world-height world) (- height 1))
    (setf (world-map world) (make-array `(,width ,height)))

    ;; Make totally random map:
    (do ((x 0)
	 (y 0))
	((>= x (array-dimension (world-map world) 0)))
      (setf (aref (world-map world) x y)
	    (make-tile :type (if (< (random 4) 1)
				 (cons 'sea nil) ; '(sea) results in all tile types pointing to EQ type
				 (cons 'grass nil))))
      (and (member 'grass (tile-type (aref (world-map world) x y)))
	   (< (random 10) 1)
	   (prog1 1 (format t "~&Gonna build me a city at ~a, ~a~%" x y))
	   (create-city x y world))
	  
      (incf y)
      (if (>= y (array-dimension (world-map world) 1))
	  (progn (incf x)
		 (setf y 0))))

    ;; Add in sea coasts to land tiles:
    ;; update: also city outskirts
    (do ((x 0)
	 (y 0))
	((>= x (array-dimension (world-map world) 0)))

      (format t "~&~a,~a" x y)

      (if (not (member 'sea (tile-type (tile-at x y world)))) ; don't do for sea tiles
	  (dolist (direction (list 'N 'NE 'SE 'S 'SW 'NW))
	    ;;(format t "doing list~%")
	    (let ((neighbour-tile (neighbour-tile-coords x y direction world)))
	      ;;(format t "~&neigbour: ~a,~a" (car neighbour-tile) (cdr neighbour-tile))
	      (if neighbour-tile
		  ;; what's all this then?
		  (progn
		    (if (member 'sea (tile-type (aref (world-map world) (car neighbour-tile) (cdr neighbour-tile))))
			(push (intern (concatenate 'string "COAST-" (symbol-name direction)))
			      (tile-variant (aref (world-map world) x y))))
		    (if (member :city (tile-location (aref (world-map world) (car neighbour-tile) (cdr neighbour-tile))))
			(push (intern (concatenate 'string "CITY-OUTSKIRTS-" (symbol-name direction)))
			      (tile-variant (aref (world-map world) x y)))))))))

      ;; TODO: Whatever this was written to do it doesn't do.
      ;; the tile-variant graphics will need to be ordered according to some smart priority 
      (setf (tile-variant (aref (world-map world) x y)) (nreverse (tile-variant (aref (world-map world) x y))))
      

      (incf y)
      (if (>= y (array-dimension (world-map world) 1))
	  (progn (incf x)
		 (setf y 0))))

    world))
