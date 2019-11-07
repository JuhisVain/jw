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
;;  (general nil :type (or general null))
;;  (subordinates nil)
;;  (supply-sources nil)
  )

(defstruct (sub-hq (:include hq)
		   (:print-object sub-hq-printer))
;;  (general nil :type (or general null))
  (superior nil :type (or sub-hq supreme-hq))
;;  (subordinates nil)
;;  (supply-sources nil)
  )

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


;; HQs will use their WHEELED units with carry-space and full action-points to distribute supply
;; to subordinates. Might want a separate distribution for trains?

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

(defun supply-system (faction)

  (unless (faction-chain-of-command faction)
    (error "~&Faction ~a has no supreme HQ!~%" (faction-name faction)))

  )