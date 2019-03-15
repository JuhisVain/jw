(in-package #:war)

;;; Need to define functions
;;    (load-tiles ())
;;    (set-tile-size (size))

;;; Need to setup variables
;;    *graphics-variants*


;;; (GUGS (aaa :large (priority x-ofs y-ofs)
;;             :small (priority x-ofs y-ofs))
;;        (bbb :large (priority x-ofs y-ofs)
;;             :small (priority x-ofs y-ofs))

;;; All tile graphics files in graphics folder should be
;;   named x-a for primary and x-b, x-c, x-d etc.. for variants


;;;; spec notes:
;;;  - All full and overflowing tiles should be named 'NAME_VARIANT_SIZE.png'
;;       all caps
;;       name of graphics, as used logically within game as identifying symbol for type
;;       variant's id, proceeding alphabetically from A, ISO basic latin alphabet, Z->AA->AB
;;       size is either LARGE or SMALL

(defmacro gugs (&rest args)
  (dolist (form args)
    

    )
  )

(defun find-variant-files (symbol)
  "Return list containing symbol and variant-names (xxx-A) of found graphics"
  (do* ((return-list (list symbol))
	(var-name (string-upcase (string symbol)))
	(var-id "A" (next-variant-id var-id))
	(file-path (concatenate 'string "./graphics/" var-name "_"))
	;; All tiles should have same large and small variants:
	(file-path-large (concatenate 'string file-path var-id "_LARGE.png")
			 (concatenate 'string file-path var-id "_LARGE.png")))
       ((null (probe-file file-path-large)) (reverse return-list))
    (setf return-list
	  (push (intern (concatenate 'string var-name "-" var-id))
		return-list))))

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

;; (macroexpand-1 '(grand-unified-graphics-setup (field :large (1 2 3) :small (3 2 1))))
(defmacro grand-unified-graphics-setup (&rest args)
  ;; The xxx_A.png files specify primary graphics -> overflowing borders should only be found in those
  
  
  ;; To be used like:
  ;; (gugs (xxx :large (priority x-ofs y-ofs)
  ;;            :small (priority x-ofs y-ofs))
  ;;       etc..)
  ;; should create xxx-a and variants xxx-b xxx-c etc. stored in *graphics-variants*
  (do* ((current args (cdr args))
	(form (car current) (car current))
	(load-tiles-list)
	(set-tile-size-list))
       ((null current) (list load-tiles-list set-tile-size-list))
    (setf load-tiles-list
	  (push `(create-tile ,@form) ; Create the tile-type symbol and
		load-tiles-list))     ; if and only when needed: primary border symbols
				      ; for use in variant assignment in size change func

    (do* ((variant-char "A" (next-variant-char variant-char)) ; You'd think this could be simpler;TODO: make fun
	  (variant-path-large
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
				    (string variant-char) "_LARGE.png"))
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
				    (string variant-char) "_LARGE.png")))
	  (variant-path-small
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
			 (string variant-char) "_SMALL.png"))
	   (probe-file (concatenate 'string "./graphics/" (string-upcase (string (car form))) "_"
				    (string variant-char) "_SMALL.png"))))
	 ((null variant-path-large))
      (setf set-tile-size-list ; will need to duplicate + append the prime large/small variants
	    (push `(,(intern (concatenate 'string
					  (string-upcase (string (car form)))
					  "-" (string variant-char))))
		  set-tile-size-list))
      )
    )
  )

