(in-package :war)

(ql:quickload :vecto)

(defvar *nato-symbol-lib* (make-hash-table :test 'equal))

(defmacro find-first (predicate item-list list)
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
  
(defun generate-natosymbol-from (field-width field-height description)
  (vecto:with-canvas (:width 150 :height 174)
    (let ((affiliation
	   (find-first eq (friendly hostile neutral unknown) description))
	  (dimension
	   (find-first eq (air space land surface subsurface
			       equipment installation activity) ; weird indentation
		       description)))
      
      )))

