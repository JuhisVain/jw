(in-package #:war)

(defstruct heap
  (head (make-array 1000 :initial-element (cons nil nil)))
  ;;                                    priority^   ^value
  (rightmost-index 1) ;;rightmost nil
  (test #'>))

(defun heap-insert (heap element priority)
  (setf (aref (heap-head heap) (heap-rightmost-index heap)) (cons priority element))
  (heap-insert-switch heap (heap-rightmost-index heap) element priority)
  (setf (heap-rightmost-index heap) (+ 1 (heap-rightmost-index heap))))


(defun heap-insert-switch (heap index element priority)
  (let* ((parent-index (floor index 2))
	 (parent (aref (heap-head heap) parent-index)))
    (cond ((null (car parent)) nil)
	  ((funcall (heap-test heap)
		    (car (aref (heap-head heap) index))
		    (car parent))
	   (setf (aref (heap-head heap) parent-index) (cons priority element))
	   (setf (aref (heap-head heap) index) parent)
	   (heap-insert-switch heap parent-index element priority)))))
    
(defun heap-remove-max (heap)
  (setf (heap-rightmost-index heap) (1- (heap-rightmost-index heap)))
  (let ((max (aref (heap-head heap) 1)))
    (setf (aref (heap-head heap) 1)
	  (aref (heap-head heap) (heap-rightmost-index heap)))
    (setf (aref (heap-head heap) (heap-rightmost-index heap))
	  '(nil))
    (heap-remove-switch heap 1 (aref (heap-head heap) 1))
    max))

(defun heap-remove-switch (heap index priority-element)

  (if (>= (* index 2) (length (heap-head heap)))
      (progn
	(format t "~&heap-remove-switch index out of bounds~%")
	(return-from heap-remove-switch)))
  (if (or
       (null (car (aref (heap-head heap) (* index 2))))
       (null (car (aref (heap-head heap) (+ (* index 2) 1)))))
       (return-from heap-remove-switch))
  (let ((greater-child-index (if (>= (car (aref (heap-head heap) (* index 2)))
				     (car (aref (heap-head heap) (+ (* index 2) 1))))
				 (* index 2)
				 (+ (* index 2) 1))))
    (cond ((< (car (aref (heap-head heap) index))
 	      (car (aref (heap-head heap) greater-child-index)))
	   (setf (aref (heap-head heap) index)
		 (aref (heap-head heap) greater-child-index))
	   (setf (aref (heap-head heap) greater-child-index)
		 priority-element)
	   (heap-remove-switch heap greater-child-index priority-element)))))

(defun heaptest ()
  ;; Appears to be working
  (defparameter testheap (make-heap))
  (dotimes (i 999)
    (heap-insert testheap (cons (random 100) (random 100)) (random 10000)))

  (dotimes (i 994)
    (let ((max
	   (heap-remove-max testheap)))
      (do ((index 0 (incf index)))
	  ((equal (car (aref (heap-head testheap) index)) nil))
	(if (< (car max) (car (aref (heap-head testheap) index)))
	    (format t "~%FAIL-FAIL-FAIL ~a is not supposed to be lesser than ~a~&"
		    max (aref (heap-head testheap) index)))))))
