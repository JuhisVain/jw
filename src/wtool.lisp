(in-package #:war)


;;;; Testing maximum values:
;; In a world with a single movetype
;; COST    HEAPSIZE <- that's the rightmost index which is off by one
;; 101     2
;; 51      7
;; 34      19
;; 26      43
;; 21      91
;; 17      187
;; 16      379
;; 15      379       same range
;; 14      763
;; 13      763       same range
;; 12      1531
;; 11      3067
;; 10      6139
;;  9      12283     noticeably slow
;;  8      24571
;;  7      98299     extremely slow, reaches two tiles farther than 8
;;  6      393211    reaches 2 farther than 7
;; useless to test more

(defstruct heap
  (head (make-array 6139 :initial-element (cons nil nil)))
  ;;                                    priority^   ^value
  (rightmost-index 1) ;;rightmost nil
  (test #'>))

(defun heap-empty (heap)
  (if (eql (heap-rightmost-index heap) 1) t))

(defun heap-insert (heap element priority)
  (setf (aref (heap-head heap) (heap-rightmost-index heap)) (cons priority element))
  (heap-insert-switch heap (heap-rightmost-index heap) element priority)
  (setf (heap-rightmost-index heap) (+ 1 (heap-rightmost-index heap))))


(defun heap-insert-switch (heap index element priority)
  "Don't call. Only used inside heap-insert"
  (let* ((parent-index (floor index 2))
	 (parent (aref (heap-head heap) parent-index)))
    (cond ((null (car parent)) nil)
	  ((funcall (heap-test heap)
		    (car (aref (heap-head heap) index))
		    (car parent))
	   (setf (aref (heap-head heap) parent-index) (cons priority element))
	   (setf (aref (heap-head heap) index) parent)
	   (heap-insert-switch heap parent-index element priority)))))

(defun heap-peek (heap)
  (cdr (aref (heap-head heap) 1)))

(defun heap-search (heap value &key (test #'equal) (return-value 'value))
  (do* ((index 1 (1+ index))
	(at-index (aref (heap-head heap) index)
		  (aref (heap-head heap) index)))
       ((or (equal at-index '(nil))
	    (funcall test (cdr at-index) value))
	(cond ((equal return-value 'value) (cdr at-index))
	      ((equal return-value 'index) (and (cdr at-index)index))))))

(defun heap-remove-by-index (heap index-to-rem)
  (format t "~&hrbi~%")
  (if (not (numberp index-to-rem)) (return-from heap-remove-by-index nil))
  (format t "~&index tr: ~a~%" index-to-rem)

  (do ((index index-to-rem))
      (nil)
    (let* (
	   (lc-index (* index 2))
	   (rc-index (1+ (* index 2)))
	   ;; Greater Child index will be nil if unusable
	   (gc-index (and (< rc-index (length (heap-head heap)))   ; both within array bounds
			  (car (aref (heap-head heap) rc-index))   ; both have a usable value
			  (if (> (car (aref (heap-head heap) lc-index))  ; determine greater child
				 (car (aref (heap-head heap) rc-index)))
			      lc-index
			      rc-index))))


      (format t "~&i: ~a lc: ~a rc: ~a gc: ~a~%" index lc-index rc-index gc-index)
      
      (cond (gc-index
	     (setf (aref (heap-head heap) index) (aref (heap-head heap) gc-index))
	     (setf index gc-index))
	    (t
	     (setf (heap-rightmost-index heap) (1- (heap-rightmost-index heap)))
	     (setf (aref (heap-head heap) index) (aref (heap-head heap) (heap-rightmost-index heap)))
	     (setf (aref (heap-head heap) (heap-rightmost-index heap)) (cons nil nil))
	     (return))))))

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
  "Don't call. Only used inside heap-remove-max"
  (if (>= (* index 2) (length (heap-head heap)))
      (progn
	;;(format t "~&heap-remove-switch index out of bounds~%")
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

  (heap-remove-by-index testheap 2)
  (heap-remove-by-index testheap 15)
  (heap-remove-by-index testheap 150)
  (heap-remove-by-index testheap 231)
  (heap-remove-by-index testheap 500)
  (heap-remove-by-index testheap 501)
  (heap-remove-by-index testheap 954)
  
  (dotimes (i 991)
    (let ((max
	   (heap-remove-max testheap)))
      (do ((index 0 (incf index)))
	  ((equal (car (aref (heap-head testheap) index)) nil))
	(if (< (car max) (car (aref (heap-head testheap) index)))
	    (format t "~%FAIL-FAIL-FAIL ~a is not supposed to be lesser than ~a~&"
		    max (aref (heap-head testheap) index)))))))


