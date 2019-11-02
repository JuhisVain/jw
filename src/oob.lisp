(in-package :war)

(defstruct general
  (name nil :type string)
  ;; experience, bonuses etc..
  )

(defstruct oob-element
  (army nil :type (or null army)))

(defstruct (supreme-hq (:include oob-element))
  (general nil :type (or general null))
  (subordinates nil)
  (supply-sources nil))

(defstruct (sub-hq (:include oob-element))
  (general nil :type (or general null))
  (superior nil :type (or sub-hq supreme-hq))
  (subordinates nil)
  (supply-sources nil))

(defstruct (oob-pos (:include oob-element))
  (superior nil :type (or sub-hq supreme-hq)))

(defvar *turn-readiness-replenishment* 25)

(defun readiness-replenish-mod (readiness)
  "Returns multiplier to be used with unit's standard supply-use."
  ;; Currently grows linearly from 1 up to 2 at *turn-readiness-replenishment*
  (+ 1
     (/ (min (- 100 readiness)
	     *turn-readiness-replenishment*)
	*turn-readiness-replenishment*)))

(defun army-supply-request (army)
  "Returns how many units of supply to request from HQ for maximum replenishment."
  (apply #'+
	 (mapcar #'(lambda (troop)
		     (*
		      (faction-unit-supply-use (unit-stack-type troop))
		      (readiness-replenish-mod (unit-stack-readiness troop))
		      (unit-stack-count troop)))
		 (army-troops army))))
