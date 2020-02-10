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
  (send-surplus-upstream nil :type boolean)
  (send-production-upstream nil :type boolean))

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
  ;(army-validate-supply (oob-element-army origin))
  ;(army-validate-supply (oob-element-army destination))
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

;; 17.1.2020 update
;;;; It might be better to have the player design the long distance
;; super-hq to sub hqs routes and have a simple flood filled region for sub-hq
;; to subordiates..

;;; Maybe allow sub-hqs to be programmed to send supply (or anything) up the oob
;; as well. These depots could then sit on cities to send production to their
;; superiors and on harbors to transfer by ships etc..

;; subordinate field armies should not have the capability to juggle resources
;; automatically, thus only HQs stationed on cities will be able to send up.

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
	   if (hq-p subordinate)
	   do (hq-supply-distribution subordinate) ;ta-dah!
	     ))
      
      ;;(list totals totals-left)
      ;;rv
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

(defun steps-from-supreme-hq (oob-element)
  "Returns the Bacon number of OOB-ELEMENT when Kevin Bacon is supreme HQ."
  (declare (oob-element oob-element))
  (labels ((count-to-supreme (oob-element &optional (count 0))
	   ;; should have used CLOS for multiple inheritance..
	   (typecase oob-element
	     (oob-pos
	      (count-to-supreme (oob-pos-superior oob-element) (1+ count)))
	     (sub-hq
	      (count-to-supreme (sub-hq-superior oob-element) (1+ count)))
	     (supreme-hq
	      count))))
    (count-to-supreme oob-element)))

(defun highest-oob (oob-element-list)
  "Returns oob-element in OOB-ELEMENT-LIST closest to supreme HQ."
  (labels ((get-higher (oob-element-list highest min-steps)
	     (when (null oob-element-list)
	       (return-from get-higher highest))
	     (let ((current-steps
		    (steps-from-supreme-hq
		     (car oob-element-list))))
	       (if
		(< current-steps
		   min-steps)
		(get-higher (cdr oob-element-list)
			    (car oob-element-list)
			    current-steps)
		(get-higher (cdr oob-element-list)
			    highest
			    min-steps)))))
    (get-higher oob-element-list
		nil ; will return this if list empty
		+inf+)))

(defun unit-production-total-materiel-cost (faction)
  (let ((total-materiel-cost 0))
    (dolist (city (faction-cities faction))
      
      ;; Cars of city production conses are shares of total production
      (let ((total-proportion
	     (reduce #'+
		     (city-unit-production city)
		     :key #'car)))
	
	(dolist (num-product (city-unit-production city))
	  (let ((proportional-production
		 (* (city-manpower city)
		    (/ (car num-product) total-proportion))))
	    (incf
	     total-materiel-cost
	     (* proportional-production
		(faction-unit-cost-materiel (cdr num-product))))
	    

	    ))))
    total-materiel-cost))

(defun unit-production-system (faction)
  (unit-prod-sub-system
   faction
   (faction-cities faction)
   0))

(defun unit-prod-sub-system (faction city-list total-materiel-cost)
  
  (when (null city-list) (return-from unit-prod-sub-system total-materiel-cost))
  
  ;; Cars of city production conses are shares of total production
  (let* ((city (car city-list))
	 
	 (total-proportion
	  (reduce #'+
		  (city-unit-production city)
		  :key #'car))
	 
	 (production-amount-list
	  (mapcar #'(lambda (prop-prod)
		      (estimate-production (/ (car prop-prod)
					      total-proportion)
					   (cdr prop-prod)
					   (city-manpower city)))
		  (city-unit-production city)))
	 
	 (tentative-total-manpower ; actual may be reduced due to shortages
	  (apply #'+ production-amount-list))
	 
	 ;; The amount of materiel to be used for each element:
	 (tentative-materiel-use-list
	  (mapcar #'(lambda (prop-prod count)
		      (* count
			 (faction-unit-cost-materiel (cdr prop-prod))))
		  (city-unit-production city)
		  production-amount-list))
	 
	 ;; Total of above
	 (total-materiel-cost-here
	  (apply #'+ tentative-materiel-use-list)))

    (setf total-materiel-cost
	  (unit-prod-sub-system
	   faction (cdr city-list) (+ total-materiel-cost
				      total-materiel-cost-here)))

    (let ((materiel-capability
	   (min 1 (/ (faction-materiel faction)
		     total-materiel-cost))))
      (loop for prod in (city-unit-production city)
	 for count in production-amount-list
	 do (format t "Producing ~a of ~a using ~a materiel~%"
		    (floor (* count materiel-capability)) ;; amount of units produced
		    (faction-unit-name (cdr prod))
		    (* (floor (* count materiel-capability)) ;; actual cost
		       (faction-unit-cost-materiel (cdr prod))))
	 do (produce-unit (tile-at (city-x city) (city-y city))
			  (cdr prod)
			  (floor (* count materiel-capability)))
	   )
      )
    
    (format t "~a~% - ~a~%"
	    (cons(city-x city)(city-y city))
					;tentative-materiel-use-list
	    1 ;(faction)
	    ))
	
  total-materiel-cost)

(defun produce-unit (tile unit count)
  (declare (tile tile)
	   (faction-unit unit)
	   ((integer 0 *) count))

  (when (<= count 0)
    (return-from produce-unit))
  
  (let ((top-dog
	 (highest-oob
	  (mapcar #'army-coc
		  (tile-units tile))))
	(stack
	 (make-unit-stack :type unit
			  :count count
			  ;; readiness and morale ought to be better than 0
			  )))
    (if top-dog
	(army-add-troops
	 (oob-element-army top-dog) stack)
	(new-army (tile-owner tile)
		  ;; there should be a location here:
		  (location-x (car (tile-location tile)))
		  (location-y (car (tile-location tile)))
		  :troops (list stack)))

    ;; Pay for it:
    (decf (faction-materiel (tile-owner tile))
	  (* (faction-unit-cost-materiel unit)
	     (unit-stack-count stack)))
    
    ))

(defun estimate-production (proportion production-type manpower)
  "PROPORTION should be rational of MANPOWER to use and PRODUCTION-TYPE
faction-unit struct to be produced using amount MANPOWER. Returns values
total count of production & leftover manpower."
  (declare (rational proportion)
	   (faction-unit production-type)
	   ((integer 0 *) manpower))
  (floor (* manpower proportion)
	 (faction-unit-cost-manpower production-type)))


(defun supply-system (faction)
  "To be executed at start of FACTION's turn
1. produces fuel and materiel resources from respective production locations
2. does city production
3. distributes supply through chain of command tree
4. armies consume supply"
  (dolist (location (faction-locations faction))
    (location-produce location))

  (unit-production-system faction)

  (when (faction-chain-of-command faction)
    (hq-supply-distribution
     (faction-chain-of-command faction))
  
    (do-oob (position (faction-chain-of-command faction))    
      (army-consume-supply (oob-element-army position))
      (army-validate-supply (oob-element-army position))))
  )
