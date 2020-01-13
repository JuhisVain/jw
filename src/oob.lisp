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

(defmacro do-oob ((var hq) &body body)
  `(dolist (,var (list-oob-elements ,hq))
     ,@body))

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

(defun useable-cargo (stack)
  "How much cargo a hq can move around using the STACK unit-stack."
  (declare ((or null unit-stack) stack))
  (unless stack (return-from useable-cargo 0)) ; unit-stack is null, abort.
  (round
   (* ;; total carry space * working capability percentage
    (troop-carry-space stack)
    (unit-stack-count stack)
    (/ (unit-stack-action-points stack) 100)
    (/ (unit-stack-readiness stack) 100))))

(defun hq-useable-cargo-move-type-totals (hq move-type)
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
  "Moves supplies from a supply stack at army of oob-element ORIGIN to
army of oob-element DESTINATION by AMOUNT of free supplies available at
ORIGIN. Finally moves supply from unit-stack to army-supplies slot as required."
  (declare (oob-element origin destination)
	   ((integer 0 *) amount))
  ;;Some debug prints, this data should be logged at (inc-supply-stockpiles)
  '(format t "~&~a sends ~a supplies to ~a~%"
    (let ((oa (oob-element-army origin)))
      (cons (army-x oa)
	    (army-y oa)))
    (floor amount)
    (let ((da (oob-element-army destination)))
      (cons (army-x da)
	    (army-y da))))

  (inc-supply-stockpiles
   (oob-element-army destination)
   (- (inc-supply-stockpiles (oob-element-army origin)
			     (- amount))))
  (army-validate-supply (oob-element-army origin))
  (army-validate-supply (oob-element-army destination))
  nil)

(defun movetype-distance (faction movetype xy0 xy1)
  "The cost in action points that a troop of MOVETYPE belonging to FACTION
takes to move from coordinates XY0 to XY1 while staying on tiles owned by 
FACTION."
  (declare (faction faction) (symbol movetype)
	   (coordinates xy0 xy1))
  (car ; the CAR of the destination's hash value is the cost to get there
   (gethash xy1
	    (or
	     (a* (car xy0) (cdr xy0)
		 (car xy1) (cdr xy1)
		 :costfunc #'(lambda (from to) ;; No idea if my froms and tos are correct
			       ;; supply cannot travel through unknown or enemy lands:
			       (if (eq (tile-owner (tile-at (car from) (cdr from)))
				       faction)
				   (step-cost nil (car to) (cdr to)
					      (neighbourp to from)
					      *world*
					      faction
					      nil
					      (gethash movetype *unit-type-movecosts*)
					      )
				   +inf+))
		 :heuristic #'(lambda (from to)
				(* (distance (car from) (cdr from)
					     (car to) (cdr to))
				   (cadr (assoc 'rail ; fastest possible TODO: dunno what to do
						(gethash movetype *unit-type-movecosts*))))))
	     (return-from movetype-distance +inf+))
	    )))

(defun list-requests (hq)
  (declare (hq hq))
  (mapcar
   #'(lambda (sub)
       (let ((sub-army (oob-element-army sub)))
	 (typecase sub
	   (sub-hq (total-supply-request sub))
	   (t (army-supply-request sub-army)))
	 ))
   (hq-subordinates hq)))

(defun list-ranged-requests-by-movetype (hq movetype)
  "Returns list of supply requests modified by range-delivery-percentage to
reqesing army using movement type MOVETYPE. in same order as HQ's
hq-subordinates."
  (declare (hq hq))
  (mapcar
   #'(lambda (sub)
       (let* ((hq-army (hq-army hq))
	      (sub-army (oob-element-army sub))
	      (deliver-percent
	       (range-delivery-percentage
		(movetype-distance
		 (army-owner (hq-army hq))
		 movetype
		 (army-xy hq-army)
		 (army-xy sub-army)))))
	 (* ; request * range%
	  (typecase sub
	    (sub-hq (total-supply-request sub))
	    (t (army-supply-request sub-army)))
	  deliver-percent)))
   (hq-subordinates hq)))

;; HQs will use their WHEELED units with carry-space and full action-points to distribute supply
;; to subordinates. Might want a separate distribution for trains?
;; Need to start at cannon fodder requesting supplies at bottom of tree and move up through HQs
;; But also need HQs to know how much all of their subordinates need before can distribute...

;; Armies should consume (army-supply-use x) at either start or end of turn


;;;; Do shipping first, then trains, then trucks -> only specialized units may transport supply
;; boats should only be able to travel and transport port to port



;;;; i have no idea how to do what i want to do
;;; -> just do balanced cargoships, balanced trains, balanced trucks

(defun get-cargo-unit (move-type army)
  "Return the designated supply carrier with movement MOVE-TYPE from ARMY's
troops."
  (find-if #'(lambda (x)
	       ;; should maybe use symbols for names instead of strings.
	       (string= 
		(troop-name x)
		(case move-type
		  (wheeled "Truck")
		  (sea "Cargo ship")
		  (rail "Pendolino"))))
	   (army-troops army)))
	   

;;TODO: would be nice to log the amount of cargo carriers used...
(defun hq-supply-distribution (hq)
  (declare (hq hq))

  ;; Move supply from stockpile to use for this HQ
  ;; this will give HQ full supplies as long as stockpiles last
  (army-validate-supply (hq-army hq))
  ;;Todo: HQ should not be so greedy

  (let* ((totals (list-requests hq))
	 (totals-left totals)
	 (rv nil))

    (flet
	((movetype-distribution (movetype)
	   (let*(;;Sub requests modified by range of current cargo carrier
		 ;;or what's left
		 (ranged-requests
		  (mapcar #'min
			  totals-left
			  (list-ranged-requests-by-movetype hq movetype)))
		 ;;Total of ranged requests of current carrier
		 (requests-total (let ((reqtot (apply #'+ ranged-requests)))
				   (if (zerop reqtot) ; no requests: abort
				       (return-from movetype-distribution nil)
				       reqtot)))
		 ;;Amount of supply HQ can move with current carrier
		 (cargo-cap (useable-cargo
			     (get-cargo-unit movetype (hq-army hq))))
		 ;;Ranged requests multiplied by capability
		 ;;Out of range leads to "wasted" cargo capability
		 (total-delivery-rats (mapcar #'(lambda (range-req)
						  (* range-req
						     (/ (min cargo-cap
							     requests-total)
							requests-total)))
					      ranged-requests))
		 ;;Integer remainder of flooring total-deliveries
		 (rem-total 0) ; todo: add to integers below TODOTODOTODO
		 ;;Floor fractions of capability ranged requests
		 (total-delivery-ints (mapcar #'(lambda (delivery)
						  (multiple-value-bind (int rem)
						      (floor delivery)
						    (incf rem-total rem)
						    int))
					      total-delivery-rats))
		 )

	     (setf totals-left
		   (mapcar #'- totals-left total-delivery-ints))
	     
	     ;;debug
	     (push (list movetype
			 totals-left
			 ranged-requests
			 requests-total
			 cargo-cap
			 total-delivery-rats
			 total-delivery-ints
			 rem-total)
		   rv)
	     ;;debug end
	     
	     total-delivery-ints)))
      
      (dolist (movetype '(SEA
			  RAIL
			  WHEELED
			  ))
	(loop for subordinate in (hq-subordinates hq)
	   for delivery in (movetype-distribution movetype)
	   do (hq-transfer-supply hq subordinate delivery)
	   do (format t " using ~a~%" movetype)
	   if (hq-p subordinate)
	   do (hq-supply-distribution subordinate) ;ta-dah!
	     ))
      
      (list totals totals-left)
      rv
      nil
      
      )))

(defun list-oob-elements (hq)
  "Recursively list HQ and HQ's order of battle for oob-elements."
  (cons
   hq
   (loop for sub in (hq-subordinates hq)
      if (hq-p sub)
      append (list-oob-elements sub)
      else
      collect sub)))

(defun supply-system (faction)
  (hq-supply-distribution
   (faction-chain-of-command faction))
  (do-oob (position (faction-chain-of-command faction))
    (army-validate-supply (oob-element-army position))
    (army-consume-supply (oob-element-army position)))
  )
