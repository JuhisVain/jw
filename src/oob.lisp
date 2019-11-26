(in-package :war)

(defstruct general
  (name nil :type string)
  ;; experience, bonuses etc..
  )

(defstruct oob-element
  (army nil :type (or null army)))

(defstruct (hq (:include oob-element))
  (general nil :type (or general null))
  (subordinates nil)
  (supply-sources nil))

(defstruct (supreme-hq (:include hq)
		       (:print-object supreme-hq-printer))
  )

(defstruct (sub-hq (:include hq)
		   (:print-object sub-hq-printer))
  (superior nil :type (or sub-hq supreme-hq)))

(defstruct (oob-pos (:include oob-element)
		    (:print-object oob-pos-printer))
  (superior nil :type (or sub-hq supreme-hq)))

(defun oob-pos-printer (this stream)
  (declare (oob-pos this))
  (format stream "~&Subordinate of ~a~%" (oob-pos-superior this)))

(defun sub-hq-printer (this stream)
  (declare (sub-hq this))
  (format
   stream
   "Sub-HQ of general ~a~%with ~a subordinates~%Commanding-HQ ~a~%"
   (sub-hq-general this)
   (length (sub-hq-subordinates this))
   (sub-hq-superior this)))

(defun supreme-hq-printer (this stream)
  (declare (supreme-hq this))
  (format stream "Supreme-HQ of general ~a~%with ~a subordinates~%"
	  (supreme-hq-general this)
	  (length (supreme-hq-subordinates this))))

(defun oob-pos-promote (position)
  "Promotes an oob-pos to a sub-hq retaining superior HQ."
  (declare (oob-pos position))
  (let ((new-hq (make-sub-hq
		 :army (oob-pos-army position)
		 :superior (oob-pos-superior position)))
	(superior-hq (oob-pos-superior position)))
    (setf (army-coc (oob-pos-army position)) new-hq
	  (hq-subordinates superior-hq)
	  (delete position (push new-hq (hq-subordinates superior-hq))))))

(defun oob-transfer-hq (transfer hq)
  "Changes the superior-hq of TRANSFER to HQ."
  (declare ((or oob-pos sub-hq) transfer)
	   (hq hq))
  (etypecase transfer ; should have gone with objects...
    (sub-hq
     (setf (hq-subordinates (sub-hq-superior transfer))
	   (delete transfer (hq-subordinates (sub-hq-superior transfer)) :test #'eq))
     (setf (sub-hq-superior transfer) hq))
    (oob-pos
     (setf (hq-subordinates (oob-pos-superior transfer))
	   (delete transfer (hq-subordinates (oob-pos-superior transfer)) :test #'eq))
     (setf (oob-pos-superior transfer) hq)))
  (pushnew transfer (hq-subordinates hq)))


;; HQs will use their WHEELED units with carry-space and full action-points to distribute supply
;; to subordinates. Might want a separate distribution for trains?
;; Need to start at cannon fodder requesting supplies at bottom of tree and move up through HQs
;; But also need HQs to know how much all of their subordinates need before can distribute...

(defun total-supply-request (hq)
  "Return total supply requested by HQ and all underlings combined."
  (declare (hq hq))
  (+
   (army-supply-request (oob-element-army hq))
   (apply #'+
	  (mapcar #'(lambda (sub)
		      (etypecase sub
			(sub-hq (total-supply-request sub))
			(oob-pos (army-supply-request (oob-element-army sub)))))
		  (hq-subordinates hq)))))

(defun hq-useable-cargo (hq move-type)
  "How much cargo HQ can move around using MOVE-TYPE cargo-movers."
  (declare (hq hq) (symbol move-type))
  (reduce #'+ (army-troops (hq-army hq))
	  :key #'(lambda (troop)
		   (if (eq (troop-movement troop)
			   move-type)
		       (round
			(* ;; total carry space * working capability percentage
			 (troop-carry-space troop)
			 (unit-stack-count troop)
			 (/ (unit-stack-action-points troop) 100)
			 (/ (unit-stack-readiness troop) 100)))
		       0))))

(defun range-delivery-percentage (range)
  "Percentage of supply supplier can supply to suplee at RANGE."
  (cond ((<= range 100) 100/100)
	((<= range 150) 75/100)
	((<= range 200) 50/100)
	(t 0/100)))

(defun hq-transfer-supply (origin destination amount)
  (format t "~&~a sends ~a supplies to ~a~%"
	  (let ((oa (oob-element-army origin)))
	    (cons (army-x oa)
		  (army-y oa)))
	  (floor amount)
	  (let ((da (oob-element-army destination)))
	    (cons (army-x da)
		  (army-y da)))))

(defun movetype-distance (faction movetype xy0 xy1)
  "The cost in action points that a troop of MOVETYPE belonging to FACTION
takes to move from coordinates XY0 to XY1."
  (declare (faction faction) (symbol movetype)
	   (coordinates xy0 xy1))
  (car ; the CAR of the destination's hash value is the cost to get there
   (gethash xy1
	    (a* (car xy0) (cdr xy0)
		(car xy1) (cdr xy1)
		:costfunc #'(lambda (from to)
			      ;; supply cannot travel through unknown or enemy lands:
			      (if (eq (tile-owner (tile-at (car to) (cdr to)))
				      faction)
				  (step-cost nil (car from) (cdr from)
					     (neighbourp from to)
					     *world*
					     faction
					     nil
					     (gethash movetype *unit-type-movecosts*)
					     )
				  1000000))
		:heuristic #'(lambda (from to)
			       (* (distance (car from) (cdr from)
					    (car to) (cdr to))
				  (cadr (assoc 'rail ; fastest possible TODO: dunno what to do
					       (gethash movetype *unit-type-movecosts*))))))
	    )))

(defun supply-system (faction)

  (unless (faction-chain-of-command faction)
    (error "~&Faction ~a has no supreme HQ!~%" (faction-name faction)))

  ;; TODO: First it would be better to check railways supplies
  ;; Maybe supplies can be delivered using all troops?

  ;; Wheeled supplies:
  (let* ((hq (faction-chain-of-command faction))
	 (cargo-space (hq-useable-cargo hq 'WHEELED))
	 (sub-request-list
	  (mapcar
	   #'(lambda (sub)
	       (let ((hq-army (hq-army hq))
		     (sub-army (oob-element-army sub)))
		 (* ; request * range%
		  (typecase sub
		    (sub-hq (total-supply-request sub))
		    (t (army-supply-request sub-army)))
		  (range-delivery-percentage
		   (movetype-distance
		    faction 'WHEELED (army-xy hq-army) (army-xy sub-army))))
		 
		 ))
	   (hq-subordinates hq)
	   )))

    (let* ((all-requests (cons (army-supply-request (hq-army hq))
			       sub-request-list))
	   (total-req (apply #'+ all-requests))
	   (req-capability (/ (min cargo-space total-req
				   (army-supply-stockpiles-count (hq-army hq)))
			      total-req)))
      (loop
	 for sub-request in all-requests
	 for sub in (cons hq (hq-subordinates hq))
	 do (hq-transfer-supply hq sub (* sub-request req-capability)))
      )


    
    ))
