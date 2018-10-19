(in-package #:war)

(defstruct heap
  (head nil)
  (test #'>=))

(defun push-heap (element priority heap)
  ;;((priority . count-under-me) element (left-child . right-child))
  ;;((p . c) e (l . r)) <- l & r are lists thus no . in actual
  ;;((p.c)e(((p.c)e(((p.c)e(l.r)).((p.c)e(l.r)))).((p.c)e(l.r))))
  (setf (heap-head heap)
	(push-heap-traveller element priority (heap-head heap) (heap-test heap))))

