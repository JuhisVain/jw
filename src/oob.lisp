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


;; HQs will use their WHEELED units with carry-space and full action-points to distribute supply
;; to subordinates. Might want a separate distribution for trains?
;; Need to start at cannon fodder requesting supplies at bottom of tree and move up through HQs
;; But also need HQs to know how much all of their subordinates need before can distribute...

(defun total-supply-request (hq)
  "Return total supply requested by HQ and all underlings combined."
  (declare ((or sub-hq supreme-hq) hq))
  (+
   (army-supply-request (oob-element-army hq))
   (apply #'+
	  (mapcar #'(lambda (sub)
		      (etypecase sub
			(sub-hq (total-supply-request sub))
			(oob-pos (army-supply-request (oob-element-army sub)))))
		  (hq-subordinates hq)))))

(defun hq-usable-cargo (hq move-type)
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

(defun supply-system (faction)

  (unless (faction-chain-of-command faction)
    (error "~&Faction ~a has no supreme HQ!~%" (faction-name faction)))

  ;; TODO: First it would be better to check railways supplies
  ;; Maybe supplies can be delivered using all troops?

  ;; Wheeled supplies:
  (let* ((hq (faction-chain-of-command faction))
	 (cargo-space (hq-useable-cargo hq)) ;if not wheeled

	 )

    ))
