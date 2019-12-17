(in-package :war)

(defmacro funit (movement carry size count)
  `(make-UNIT-STACK
    :TYPE (make-FACTION-UNIT
	   :MOVEMENT ',movement
	   :CARRY-SPACE ,carry
	   :SIZE ,size
	   :NAME "x")
    :COUNT ,count))

(defun t-slowest-movecosts ()
  (let ((*unit-type-movecosts* (make-hash-table :test 'eq)))
    (setf (gethash 'slow *unit-type-movecosts*) '((a 1000)(b 2000)(c 3000)))
    (setf (gethash 'medium *unit-type-movecosts*) '((a 100)(b 200)(c 300)))
    (setf (gethash 'fast *unit-type-movecosts*) '((a 10)(b 20)(c 30)))

    (slowest-movecosts
     (troop-movecarry-data
      (list
       (funit slow 0 1 20)
       (funit slow 0 1 10)
       (funit medium 2 2 5)
       (funit fast 20 10 1)
       )))
    ))

(defun t-set-ready ()
  (flet ((rand-desc ()
	   (let ((seed (random 8)))
	     (list (prog1 (cond ((eq seed 0) 'air)
				((eq seed 1) 'space)
				((eq seed 2) 'land)
				((eq seed 3) 'surface)
				((eq seed 4) 'subsurface)
				((eq seed 5) 'equipment)
				((eq seed 6) 'installation)
				((eq seed 7) 'activity))
		     (setf seed (random 6)))
		   (prog1 (cond ((eq seed 0) 'air-assault-with-organic-lift)
				((eq seed 1) 'air-defense)
				((eq seed 2) 'amphibious)
				((eq seed 3) 'analysis)
				((eq seed 4) 'antitank)
				((eq seed 5) 'broadcast-transmitter-antenna)))))))

    (test-movecosts)
    (road-from-to 'rail 7 0 7 5)
    
    (road-from-to 'rail 7 0 6 0)
    (road-from-to 'rail 6 0 4 0)
    (road-from-to 'rail 4 0 2 0)
    (road-from-to 'rail 2 0 1 0)
    (road-from-to 'rail 1 0 1 1)
    (road-from-to 'rail 1 1 2 4)
    (road-from-to 'rail 11 6 19 22)
    (road-from-to 'rail 19 22 20 23) ; 20,23 is one tile out of wheeled range

    (road-from-to 'rail 7 5 11 6)

    (create-port 10 7)
    
    (loop for x from 2 to 14
       do (loop for y from 0 to 8
	     do (change-tile-owner x y (cadr (world-factions *world*)))))
					  
    
    ;;HQ:
    (army-validate-supply 
     (new-army (cadr (world-factions *world*))
	       10 7
	       :troops (list
			(make-unit-stack
			 :type (unit-type-by-name "Supply"
						  (cadr (world-factions *world*)))
			 :count 20000
			 :readiness 100)
			(make-unit-stack
			 :type (unit-type-by-name "Truck"
						  (cadr (world-factions *world*)))
			 :count 250
			 :readiness 100)
			(make-unit-stack
			 :type (unit-type-by-name "Pendolino"
						  (cadr (world-factions *world*)))
			 :count 50
			 :readiness 100)
			(make-unit-stack
			 :type (unit-type-by-name "Cargo ship"
						  (cadr (world-factions *world*)))
			 :count 1
			 :readiness 100))
	       :counter-desc '(equipment cavalry)))

    ;; cannon fodder under direct command of HQ
    (mapcar #'(lambda (x y)
		(new-army (cadr (world-factions *world*))
		      x y
		      :troops (list (make-unit-stack
				     :type (unit-type-by-name "Commando"
							      (cadr (world-factions *world*)))
				     :count 250
				     :readiness 100))
		      :counter-desc (rand-desc)))
	    '(7 7 7 7 9 5 6)
	    '(0 1 2 3 1 6 7))

    ;; Some ships
    (new-army (cadr (world-factions *world*))
	      10 10
	      :troops (list (make-unit-stack
			     :type
			     (unit-type-by-name "Cargo ship"
						(cadr (world-factions *world*)))
			     :count 10
			     :readiness 100)))
    
    ;;Sub hq
    (oob-pos-promote
     (army-coc
      (new-army (cadr (world-factions *world*))
		7 5
		:troops (list
			 (make-unit-stack
			  :type (unit-type-by-name "Commando"
						   (cadr (world-factions *world*)))
			  :count 150
			  :readiness 100)
			 (make-unit-stack
			  :type (unit-type-by-name "Truck"
						   (cadr (world-factions *world*)))
			  :count 100
			  :readiness 100))
		:counter-desc '(equipment cavalry))))

    ;; Sub hq's underlings
    (oob-transfer-hq (army-coc (car (tile-units (tile-at 5 6))))
		     (army-coc (car (tile-units (tile-at 7 5)))))

    (oob-transfer-hq (army-coc (car (tile-units (tile-at 6 7))))
		     (army-coc (car (tile-units (tile-at 7 5)))))
    ))

(defun print-coc (hq)
  (declare (hq hq))
  (format t "~a ~{~20,10T~a~%~}" ; Maybe I'll figure these out one day.
	  (list (army-x (hq-army hq))
		(army-y (hq-army hq))
		(hq-general hq))
	  (mapcar #'(lambda (x)
		      (typecase x
			(oob-pos (list (army-x (oob-pos-army x))
				       (army-y (oob-pos-army x))))
			(hq (print-coc x))))
		  (hq-subordinates hq))))

(defun full-hash-path (xy ht)
  (let ((step (gethash xy ht)))
    (when step
      (cons step
	    (full-hash-path (cadr step) ht)))))


(defun check-tile-variant-lists ()
  "If this returns something other than nil, there's been some kind of worldgen
screw up. Maybe with graphics priorities."
  (let ((tiles nil))
    (dotimes (x (1+ (world-width *world*)))
      (dotimes (y (1+ (world-height *world*)))
	(let ((f (car (tile-variant (tile-at x y)))))
	  (if (or (eq f 'grass-a) (eq f 'sea-a)) nil
	      (push (cons x y) tiles)))))
    tiles))
