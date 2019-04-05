(in-package #:war)

;;; Need to define functions
;;    (load-tiles ())
;;    (set-tile-size (size))

;;; Need to setup variables
;;    *graphics-variants*


;;; (GUGS large-tile-width large-tile-height large-tile-horizontal
;;        small-tile-width small-tile-height small-tile-horizontal
;;        :full ((aaa :large (priority x-ofs y-ofs)
;;                    :small (priority x-ofs y-ofs))
;;               (bbb :large (priority x-ofs y-ofs)
;;                    :small (priority x-ofs y-ofs)))
;;        :border ((stream :large (:north (priority x-ofs y-ofs)
;;                                :north-west (priority x-ofs y-ofs)
;;                                :south-west (priority x-ofs y-ofs))
;;                         :small (priority x-ofs y-ofs))))

;;;; spec notes:
;;;  - All full and overflowing tiles should be named 'NAME_VARIANT_SIZE.png'
;;       all caps
;;       -name of graphics, as used logically within game as identifying symbol for type
;;       -variant's id, proceeding alphabetically from A, ISO basic latin alphabet, Z->AA->AB
;;       -size is either LARGE or SMALL
;;   - All border tiles should be named 'NAME_VARIANT_SIZE_BORDER.png'
;;       as above
;;       -border is one of   N NW SW


;;; Other:
;; Might want to have a (reload-graphics tile-symbol) function


'(grand-unified-graphics-setup
 :full
 '((sea :large (100 -4 -9) :small (0 0 0))
   (grass :large (0 0 0) :small (0 0 0))
   (field :large (25 -4 -9) :small (25 0 0))
   (forest :large (75 -4 -17) :small (75 0 0))
   (city :large (50 -6 -6) :small (50 0 0))
   (suburb :large (50 0 0) :small (50 0 0))
   (swamp :large (1 0 0) :small (1 0 0)))

 :border '(rivers maybe roads?)

 :misc
 '((selector :large (200 11 0) :small (200 5 0))
   (missing :large (300 0 0) :small (300 0 0)))
 )

(defun grand-unified-graphics-setup (&rest args)
  (let ((load-tiles-list nil)
	(set-large-list nil)
	(set-small-list nil))

    ;; Process full tiles:
    (do ((head (getf args :full) (cdr head)))
	((null head))
      (let ((head-results (process-gform (car head) 'full)))
	(setf load-tiles-list (nconc (car head-results) load-tiles-list))
	(setf set-large-list (nconc (cadr head-results) set-large-list))
	(setf set-small-list (nconc (caddr head-results) set-small-list))))

    ;; Process border tiles
    ;; todo

    ;; Process miscellaneous graphics:
    (do ((head (getf args :misc) (cdr head)))
	((null head))
      (let ((head-results (process-gform (car head) 'misc)))
	(setf load-tiles-list (nconc (car head-results) load-tiles-list))
      ))

    
    ;;(list load-tiles-list set-large-list set-small-list) ;; just testing TODO remove

    (setf (symbol-function 'load-tiles)
	  (compile nil (append '(lambda ()) load-tiles-list '((set-tile-size 'large)))))

    ;; If there is not set-tile-size func, make dummy func
    ;;   before it's called in load-tiles
    ;;(unless (fboundp 'set-tile-size) ; This will actually have to be forced until old set-tile-size is wiped
    (setf (symbol-function 'set-tile-size)
	  #'(lambda (x) x))
    ;;)

    ;; Generates overflown border graphics required for set-tile-size func:
    (load-tiles) ; Living on the razor's edge


    (do* ((head (getf args :full) (cdr head)))
	 ((null head))
      (let ((defpars
	     (mapcar #'(lambda (direction)
			 (let* ((symbol-name (symbol-name (caar head)))
				(border-sym-name-large
				 (intern (concatenate 'string ; 'field-a-large-border-north-east etc..
						      symbol-name
						      "-A-LARGE-BORDER-"
						      direction)))
				(border-sym-name-small
				 (intern (concatenate 'string
						      symbol-name
						      "-A-SMALL-BORDER-"
						      direction))))
			   (list ;; ((defparameter field-outskirts-ne field-a-large-border-north-east)
			    ;;  (defparameter field-outskirts-ne missing-small)) etc..
			    `(defparameter
				 ,(intern (concatenate 'string
						       symbol-name
						       "-OUTSKIRTS-"
						       (long-dir-short-string direction)))
			       ,(if (boundp border-sym-name-large)
				    border-sym-name-large
				    'placeholder))
			    ;; Using the placeholder to initialize logic symbols should be made redundant
			    ;;   instead make some list for the finalizer functions to check what borders to use

			    `(defparameter
				 ,(intern (concatenate 'string
						       symbol-name
						       "-OUTSKIRTS-"
						       (long-dir-short-string direction)))
			       ,(if (boundp border-sym-name-small)
				    border-sym-name-small
				    'placeholder)))))
		     (list "NORTH" "NORTH-WEST" "NORTH-EAST" "SOUTH" "SOUTH-WEST" "SOUTH-EAST"))))
	(dolist (defpar defpars)
	  (push (car defpar) set-large-list)
	  (push (cadr defpar) set-small-list))))

    (setf (symbol-function 'set-tile-size)
	  (compile nil (append '(lambda (size))
			       '((sdl:clear-display sdl:*black*)
				 (defvar placeholder (make-graphics :surface (sdl:create-surface 0 0))))
			       `((cond ((eq size 'large)
					(defparameter selector selector-large)
					(defparameter tile-size-x (car tile-large-size))
					(defparameter tile-size-y (cdr tile-large-size))
					(defparameter tile-size-full-x tile-large-size-full-x)
					(defparameter tile-size-hor-x tile-large-size-full-hor-x)
					(defparameter tile-size tile-large-size)
					,@set-large-list)
				       ((eq size 'small)
					(defparameter selector selector-small)
					(defparameter tile-size-x (car tile-small-size))
					(defparameter tile-size-y (cdr tile-small-size))
					(defparameter tile-size-full-x tile-small-size-full-x)
					(defparameter tile-size-hor-x tile-small-size-full-hor-x)
					(defparameter tile-size tile-small-size)
					,@set-small-list))))))
    
    ))


(defun long-dir-short-string (direction-string)
  (let ((ass-dir '(("NORTH" . "N")
		   ("NORTH-WEST" . "NW")
		   ("NORTH-EAST" . "NE")
		   ("SOUTH" . "S")
		   ("SOUTH-WEST" . "SW")
		   ("SOUTH-EAST" . "SE"))))
    (cdr (assoc direction-string ass-dir :test #'string=))))
    
(defun process-gform (graphics-form graphics-type)
  "Returns '((load-tiles contents) (set-tile-size 'large contents) (setsmall contents))"
  (let* ((load-tiles) (set-large) (set-small) ; Lists to return
	 (symbol (car graphics-form)) ; Base name of graphic
	 (vars-large (getf (cdr graphics-form) :large))
	 (priority-large (car vars-large))
	 (x-ofs-large (cadr vars-large))
	 (y-ofs-large (caddr vars-large))
	 
	 (vars-small (getf (cdr graphics-form) :small))
	 (priority-small (car vars-small))
	 (x-ofs-small (cadr vars-small))
	 (y-ofs-small (caddr vars-small))

	 (variant-list-large
	  (find-variant-files symbol "LARGE")) ; '(field field-a field-b field-c)
	 (variant-list-small
	  (find-variant-files symbol "SMALL")))


    ;; Store variants
    (when (eq graphics-type 'full)
      (push `(push ',variant-list-large *graphics-variants*) load-tiles))

    (setf load-tiles ; Call tile graphics setup for everything found
	  (nconc (form-graphics-setups
		  variant-list-large priority-large x-ofs-large y-ofs-large 'large graphics-type)
		 (form-graphics-setups
		  variant-list-small priority-small x-ofs-small y-ofs-small 'small graphics-type)
		 load-tiles))

    (let ((variant-defpars (form-variant-defpars variant-list-large
						 variant-list-small)))
      (setf
       set-large
       (append (car variant-defpars) set-large)
       set-small
       (append (cadr variant-defpars) set-small)))

    (list load-tiles set-large set-small)
    ))


;; (form-variant-defpars (find-variant-files 'field) (find-variant-files 'field "SMALL"))
(defun form-variant-defpars (variant-list-large variant-list-small)
  "Returns list containing two lists containing defparameter statements,
used as switchable pointers to the actual graphics.
CAR holds large's list and CADR holds small's list."
  (let ((ll-sl (list ()())))
    (dolist (large-var (cdr variant-list-large))
      (push `(defparameter ,large-var ,(abs-til-sym large-var 'large))
	    (car ll-sl))

      (if (member large-var variant-list-small)
	  (push `(defparameter ,large-var ,(abs-til-sym large-var 'small))
		(cadr ll-sl))
	  (push `(defparameter ,large-var missing-small)
		(cadr ll-sl))))
    ll-sl))


(defun form-graphics-setups (variant-list priority x-offset y-offset size-symbol graphics-type)
  "If graphics-type is 'full returns a list containing tile-graphics-setup calls for
elements in (cdr variant-list) of size size-symbol. Graphics-type nil "
  (and
   (or (cdr variant-list) ; return nil if no variants in list
       (eq graphics-type 'misc))
   (let ((variant-setup-list nil))
     (push `(tile-graphics-setup
	     ',(abs-til-sym (car variant-list)
			   size-symbol
			   (cond ((eq graphics-type 'full) 'a)
				 ((eq graphics-type 'misc) nil)))
	     ,priority ,x-offset ,y-offset)
	   variant-setup-list)
     ;; Escape when no variants requested:
     (when (eq graphics-type 'misc) (return-from form-graphics-setups variant-setup-list))
     
     (dolist (variant (cddr variant-list)) ; remaining variants
       (push `(tile-graphics-setup
	       ',(abs-til-sym variant size-symbol)
	       ,priority 0 0)
	     variant-setup-list))
     variant-setup-list)))


(defun abs-til-sym (tile-symbol size-symbol &optional (force-variant nil))
  "Return absolute tile symbol such as FIELD-A-LARGE"
  (if force-variant
      (intern (concatenate 'string (symbol-name tile-symbol)
			   "-" (symbol-name force-variant)
			   "-" (symbol-name size-symbol)))
      (intern (concatenate 'string (symbol-name tile-symbol)
			   "-" (symbol-name size-symbol)))))


(defun find-variant-files (symbol &optional (size-string "LARGE"))
  "Return list containing symbol and variant-names (xxx-A) of found graphics"
  (do* ((return-list (list symbol))
	(var-name (string-upcase (string symbol)))
	(var-id "A" (next-variant-id var-id))
	(file-path (concatenate 'string "./graphics/" var-name "_"))
	;; All tiles should have same large and small variants:
	(file-path-large (concatenate 'string file-path var-id "_" size-string ".png")
			 (concatenate 'string file-path var-id "_" size-string ".png")))
       ((null (probe-file file-path-large)) (reverse return-list))
    (push (intern (concatenate 'string var-name "-" var-id))
	  return-list)))

(defun next-variant-id (old-char-string)
  "Returns next character to be used as variant symbol/file identifier
'A' -> 'B' ; 'Z' -> 'AA' ; 'AA' -> 'AB'"
  (do* ((rev-string (reverse old-char-string))
	(i 0 (incf i))
	(character (char rev-string i) (char rev-string i)))
       (nil)
    (if (char= character #\Z)
	(progn
	  (setf (elt rev-string i) #\A)
	  (if (= (+ i 1) (length rev-string))
	      (progn
		(setf rev-string (concatenate 'string rev-string "A"))
		(return-from next-variant-id (reverse rev-string))
	      )))
	(progn
	  (setf (elt rev-string i) (code-char (1+ (char-code character))))
	  (return-from next-variant-id (reverse rev-string))))))
