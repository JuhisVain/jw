(in-package :war)

(defstruct general
  (name nil :type string)
  ;; experience, bonuses etc..
  )

(defstruct oob-element
  (army nil :type (or null army)))

(defstruct (supreme-hq (:include oob-element)
		       (:print-object supreme-hq-printer))
  (general nil :type (or general null))
  (subordinates nil)
  (supply-sources nil))

(defstruct (sub-hq (:include oob-element)
		   (:print-object sub-hq-printer))
  (general nil :type (or general null))
  (superior nil :type (or sub-hq supreme-hq))
  (subordinates nil)
  (supply-sources nil))

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

(defvar *turn-readiness-replenishment* 25)

(defun readiness-replenish-mod (readiness)
  "Returns multiplier to be used with unit's standard supply-use."
  ;; Currently grows linearly from 1 up to 2 at *turn-readiness-replenishment*
  (+ 1
     (/ (min (- 100 readiness)
	     *turn-readiness-replenishment*)
	*turn-readiness-replenishment*)))

;;Note: Will need to add to this to replenish army-supplies too
(defun army-supply-request (army)
  "Returns how many units of supply to request from HQ for maximum replenishment."

  (* (/ (army-supply-req army)
	100)
     
     (+
      ;; Use by turn:
      (apply #'+
	     (mapcar #'(lambda (troop)
			 (*
			  (faction-unit-supply-use (unit-stack-type troop))
			  (readiness-replenish-mod (unit-stack-readiness troop))
			  (unit-stack-count troop)))
		     (army-troops army)))

      ;; Replenish army stocks:
      (apply #'+
	     (mapcar #'(lambda (troop)
			 (*
			  (faction-unit-supply-space (unit-stack-type troop))
			  (unit-stack-count troop)))
		     (army-troops army)))

      )))

;; HQs will use their units with carry-space and full action-points to distribute supply
;; to subordinates.
