
;;; render/Medium

(in-package :mcclim-render)

(defgeneric medium-draw-box-using-ink*
    (medium ink left top right bottom filled))

(defgeneric climi::medium-draw-box* (medium left top right bottom filled))

(defmethod climi::medium-draw-box* ((medium render-medium-mixin) left top right bottom filled)
  (medium-draw-box-using-ink* medium (medium-ink medium)
                                    left top right bottom filled))

(defmethod medium-draw-box-using-ink* ((medium render-medium-mixin) (ink t) left top right bottom filled)
  (if (< right left)
      (rotatef left right))
  (if (< bottom top)
      (rotatef top bottom))
  (let ((path (make-path left top)))
    (line-to path right top)
    (line-to path right bottom)
    (line-to path left bottom)
    (close-path path)
    (if filled
	(%medium-fill-paths medium (list path))
	(%medium-stroke-paths medium (list path)))))

;; Null/medium

(in-package :clim-null)

(defmethod climi::medium-draw-box* ((medium null-medium) left top right bottom filled)
  (declare (ignore left top right bottom filled))
  nil)


;; CLX/medium

(in-package :clim-clx)

(defgeneric medium-draw-box-using-ink*
    (medium ink left top right bottom filled))

(defmethod climi::medium-draw-box* ((medium clx-medium) left top right bottom filled)
  (medium-draw-box-using-ink* medium (medium-ink medium)
                                    left top right bottom filled))

(defmethod medium-draw-box-using-ink* ((medium clx-medium) (ink t) left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
        (with-clx-graphics () medium
          (if (< right left)
              (rotatef left right))
          (if (< bottom top)
              (rotatef top bottom))
          (let ((left   (round-coordinate left))
                (top    (round-coordinate top))
                (right  (round-coordinate right))
                (bottom (round-coordinate bottom)))
            ;; To clip boxs, we just need to clamp the
	    ;; coordinates
            (xlib:draw-rectangle mirror gc
                                 (max #x-8000 (min #x7FFF left))
                                 (max #x-8000 (min #x7FFF top))
                                 (max 0 (min #xFFFF (- right left)))
                                 (max 0 (min #xFFFF (- bottom top)))
                                 filled)))))))

#+CLX-EXT-RENDER
(defmethod medium-draw-box-using-ink* ((medium clx-medium) (ink climi::uniform-compositum)
                                             x1 y1 x2 y2 filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (multiple-value-bind (r g b) (color-rgb (slot-value ink 'climi::ink))
            (let ((a (opacity-value (slot-value ink 'climi::mask))))
              ;; Hmm, XRender uses pre-multiplied alpha, how useful!
              (setf r (min #xffff (max 0 (round (* #xffff a r))))
                    g (min #xffff (max 0 (round (* #xffff a g))))
                    b (min #xffff (max 0 (round (* #xffff a b))))
                    a (min #xffff (max 0 (round (* #xffff a)))))
              (let ((picture (clx-medium-picture medium)))
                (xlib:render-fill-rectangle picture :over (list r g b a)
                                            (max #x-8000 (min #x7FFF x1))
                                            (max #x-8000 (min #x7FFF y1))
                                            (max 0 (min #xFFFF (- x2 x1)))
                                            (max 0 (min #xFFFF (- y2 y1))))))))))))

;;; PostScript/graphics

(in-package :clim-postscript)

(define-postscript-procedure
    (put-box* :postscript-name "pr"
                     :postscript-body
                     "/y2 exch def /x2 exch def /y1 exch def /x1 exch def
x1 y1 moveto x1 y2 lineto x2 y2 lineto x2 y1 lineto x1 y1 lineto"
                     :extra-entries 4)
    (stream x1 y1 x2 y2)
  (write-coordinates stream x1 y1)
  (write-coordinates stream x2 y2)
  (format stream "pr~%"))

(defmethod climi::medium-draw-box*
    ((medium postscript-medium) x1 y1 x2 y2 filled)
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium))))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (put-box* stream x1 y1 x2 y2)
    (format stream (if filled "fill~%" "stroke~%"))))

;;; clim-basic/encapsulate

(in-package :clim-internals)

(def-stream-method medium-draw-box*
    ((stream standard-encapsulating-stream) x1 y1 x2 y2 filled))

;;; clim-basic/recording

(in-package :clim-internals)

(defun expand-box-coords (left top right bottom)
  "Expand the two corners of a rectangle into a polygon coord-seq"
  (vector left top right top right bottom left bottom))

(def-grecording draw-box ((gs-line-style-mixin)
				left top right bottom filled) (:medium-fn nil)
  (let* ((transform (medium-transformation medium))
         (border     (graphics-state-line-style-border graphic medium))
         (pre-coords (expand-box-coords left top right bottom))
         (coords     (transform-positions transform pre-coords)))
    (setf (values left top) (transform-position transform left top))
    (setf (values right bottom) (transform-position transform right bottom))
    (polygon-record-bounding-rectangle coords t filled line-style border
                                       (medium-miter-limit medium))))

(defmethod medium-draw-box* :around ((stream output-recording-stream) left top right bottom filled)
  (let ((tr (medium-transformation stream)))
    (if (rectilinear-transformation-p tr)
        (generate-medium-recording-body draw-box-output-record
					medium-draw-box*
                                        (left top right bottom filled))
	(medium-draw-polygon* stream
			      (expand-box-coords left top right bottom)
                              t
			      filled))))

(defmacro with-standard-box* ((&key x1 y1 x2 y2) rectangle &body body)
  (with-gensyms (coords)
    `(let ((,coords (slot-value ,rectangle 'coordinates)))
       (declare (type (simple-array coordinate (4)) ,coords))
       (let (,@(and x1 `((,x1 (aref ,coords 0))))
	     ,@(and y1 `((,y1 (aref ,coords 1))))
	     ,@(and x2 `((,x2 (aref ,coords 2))))
	     ,@(and y2 `((,y2 (aref ,coords 3)))))
	 (declare (type coordinate
			,@(and x1 `(,x1))
			,@(and y1 `(,y1))
			,@(and x2 `(,x2))
			,@(and y2 `(,y2))))
	 ,@body))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-box-output-record))
  (with-standard-box* (:x1 x1 :y1 y1)
      record
    (with-slots (left top right bottom)
	record
      (let ((dx (- nx x1))
	    (dy (- ny y1)))
	(multiple-value-prog1
	    (call-next-method)
	  (incf left dx)
	  (incf top dy)
	  (incf right dx)
	  (incf bottom dy))))))

(defrecord-predicate draw-box-output-record (left top right bottom filled)
  (and (if-supplied (left coordinate)
	 (coordinate= (slot-value record 'left) left))
       (if-supplied (top coordinate)
	 (coordinate= (slot-value record 'top) top))
       (if-supplied (right coordinate)
	 (coordinate= (slot-value record 'right) right))
       (if-supplied (bottom coordinate)
	 (coordinate= (slot-value record 'bottom) bottom))
       (if-supplied (filled)
	 (eql (slot-value record 'filled) filled))))

;;; clim-basic/transform

(in-package :clim-internals)

(defgeneric transform-box* (transformation x1 y1 x2 y2))

(defmethod transform-box* ((transformation transformation) x1 y1 x2 y2)
  (if (rectilinear-transformation-p transformation)
      (multiple-value-bind (x1 y1) (transform-position transformation x1 y1)
        (multiple-value-bind (x2 y2) (transform-position transformation x2 y2)
          (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))))
    (error 'rectangle-transformation-error
           :transformation transformation
           :rect (list x1 y2 x2 y2))))

(defmethod untransform-box* ((transformation transformation) x1 y1 x2 y2)
  (transform-box* (invert-transformation transformation) x1 y1 x2 y2))

;;; clim-basic/medium

(defmethod medium-draw-box* :around ((medium transform-coordinates-mixin) left top right bottom filled)
  (let ((tr (medium-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (multiple-value-bind (left top right bottom)
            (transform-box* tr left top right bottom)
          (call-next-method medium left top right bottom filled))
        (medium-draw-polygon* medium (expand-box-coords left top right bottom)
                              t filled))) )

;;; clim-basic/graphics

(in-package :clim-internals)

(defun draw-box* (sheet x1 y1 x2 y2
			&rest args
			&key (filled t) ink clipping-region transformation
			  line-style line-thickness line-unit line-dashes
			  line-joint-shape)
  (declare (ignore ink clipping-region transformation line-style line-thickness
		   line-unit line-dashes line-joint-shape))
  (with-medium-options (sheet args)
    (medium-draw-box* medium x1 y1 x2 y2 filled)))

(def-graphic-op draw-box (left top right bottom filled))

