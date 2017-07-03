(defpackage #:clim-meta-test
  (:use #:clim-lisp #:clim)
  (:import-from #:clim-mop
                #:standard-class))

(in-package :clim-meta-test)

;;;;


;; metaclass

(defclass output-record-class (clim-mop:standard-class)
  ((drawing-function :initarg :drawing-function :accessor drawing-function)
   (medium-drawing-function
    :initarg :medium-drawing-function
    :accessor medium-drawing-function))
  (:documentation "metaclass for output-record classes."))

(defmethod clim-mop:validate-superclass ((class output-record-class)
                                         (superclass clim-mop:standard-class))
  t)

(defmethod initialize-instance :after
    ((class output-record-class) &rest all-keys &key &allow-other-keys)
  (declare (ignore all-keys))
  (clim-mop:finalize-inheritance class))

(defmethod initialize-instance :around
    ((class output-record-class) &rest all-keys
                                 &key drawing-function
                                      medium-drawing-function
                                      &allow-other-keys)
  (when drawing-function
    (setf (slot-value class 'drawing-function)
          (car drawing-function)))
  (when medium-drawing-function
    (setf (slot-value class 'medium-drawing-function)
          (car medium-drawing-function)))
  (apply #'call-next-method class
         (climi::remove-keywords all-keys
                                 '(:drawing-function
                                   :medium-drawing-function))))

(defmethod reinitialize-instance :around
    ((class output-record-class) &rest all-keys
                                 &key drawing-function
                                      medium-drawing-function
                                      &allow-other-keys)
  (declare (ignore drawing-function
                   medium-drawing-function))
  (apply #'call-next-method class
         (climi::remove-keywords all-keys
                                 '(:drawing-function
                                   :medium-drawing-function))))

(defmethod recorded-slots (class)
  (remove-if-not (lambda (x) (typep x'effective-recorded-slot))
                 (clim-mop:class-slots class)))

(defun output-record-superclasses (class)
  "finds all superclasses of class that are output-record-classes"
  (let ((record-superclasses nil))
    (labels ((test-class (class)
               (when (typep class 'output-record-class)
                 (pushnew class record-superclasses))
               (map nil #'test-class (clim-mop:class-direct-superclasses class))))
      (map nil #'test-class (clim-mop:class-direct-superclasses class))
      record-superclasses)))

;; slot

(defclass direct-recorded-slot (clim-mop:standard-direct-slot-definition)
  ())

(defmethod shared-initialize :after ((slot direct-recorded-slot) slot-names
                                     &key record &allow-other-keys)
  (declare (ignore record)))

(defmethod clim-mop:direct-slot-definition-class
    ((class output-record-class) &key record &allow-other-keys)
  (if record
      (find-class 'direct-recorded-slot)
      (call-next-method)))

(defparameter *direct-recorded-slot* nil
  "This is used to communicate the fact that a slot is recorded to
  effective-slot-definition-class.")

(defclass effective-recorded-slot (clim-mop:standard-effective-slot-definition)
  ((direct-slot :initform *direct-recorded-slot* :reader slot-recorded)))

(defmethod clim-mop:compute-effective-slot-definition ((class output-record-class)
                                              name direct-slot-definitions)
  (declare (ignore name))
  (flet ((is-recorded-slot (slot) (typep slot 'direct-recorded-slot)))
    (let ((*direct-recorded-slot* (find-if #'is-recorded-slot direct-slot-definitions)))
      (call-next-method))))

(defmethod clim-mop:effective-slot-definition-class
    ((class output-record-class) &rest initargs)
  (declare (ignore initargs))
  (if *direct-recorded-slot*
      (find-class 'effective-recorded-slot)
      (call-next-method)))

(defmethod keyword-argument ((slot effective-recorded-slot))
  (intern (symbol-name (clim-mop:slot-definition-name slot)) :keyword))

;; example classes

(defclass my-gs-line-style-mixin (climi::graphics-state)
  ((line-style :initarg :line-style :accessor graphics-state-line-style :record t))
  (:metaclass output-record-class))

(defmethod initialize-instance :after ((obj my-gs-line-style-mixin)
				       &key (stream nil)
                                            (medium (when stream
                                                      (climi::sheet-medium stream))))
  (when medium
    (unless (slot-boundp obj 'line-style)
      (setf (slot-value obj 'line-style)
            (climi::medium-line-style medium)))))

(defmethod climi::graphics-state-line-style-border ((record my-gs-line-style-mixin) medium)
  (/ (climi::line-style-effective-thickness (graphics-state-line-style record)
                                            medium)
     2))

;; draw point

(defclass my-draw-point-output-record
           (my-gs-line-style-mixin climi::standard-graphics-displayed-output-record)
  ((point-x :initarg :point-x :record t)
   (point-y :initarg :point-y :record t))
  (:metaclass output-record-class)
  (:drawing-function 'draw-point)
  (:medium-drawing-function 'medium-draw-point*))

(find-class 'my-draw-point-output-record)

;; hacks below

(defmethod initialize-instance :after
    ((graphic my-draw-point-output-record) &key)
   (with-slots (stream point-x point-y)
       graphic
     (let* ((medium (sheet-medium stream)))
       (let ((border
              (climi::graphics-state-line-style-border graphic medium)))
         (climi::with-transformed-position ((climi::medium-transformation medium)
                                     point-x point-y)
           (setf (slot-value graphic 'point-x) point-x
                 (slot-value graphic 'point-y) point-y)
           (setf (rectangle-edges* graphic)
                 (values (- point-x border)
                         (- point-y border)
                         (+ point-x border)
                         (+ point-y border))))))))

(defmethod climi::medium-draw-point* :around
    ((stream output-recording-stream) point-x point-y)
  (when (climi::stream-recording-p stream)
    (let ((record (make-instance 'my-draw-point-output-record
                                 :stream stream
                                 :point-x point-x
                                 :point-y point-y)))
      (climi::stream-add-output-record stream record)))
  (when (climi::stream-drawing-p stream)
    (clim:with-sheet-medium (medium stream)
      (clim:medium-draw-point* medium point-x point-y))))

(defmethod climi::replay-output-record ((record my-draw-point-output-record) stream
             &optional (region +everywhere+) (x-offset 0) (y-offset 0))
   (declare (ignore x-offset y-offset region))
   (with-slots (point-x point-y)
       record
     (let ((medium (clim:sheet-medium stream)))
       (clim:medium-draw-point* medium point-x point-Y))))

(climi::defmethod* (setf climi::output-record-position) :around
    (nx ny (record my-draw-point-output-record))
    (climi::with-standard-rectangle* (:x1 x1 :y1 y1)
	record
      (with-slots (point-x point-y)
	  record
	(let ((dx (- nx x1))
	      (dy (- ny y1)))
	  (multiple-value-prog1
	      (call-next-method)
	    (incf point-x dx)
	    (incf point-y dy))))))

(climi::defrecord-predicate my-draw-point-output-record (point-x point-y)
  (and (climi::if-supplied (point-x coordinate)
                           (climi::coordinate= (slot-value climi::record 'point-x) point-x))
       (climi::if-supplied (point-y coordinate)
                           (climi::coordinate= (slot-value climi::record 'point-y) point-y))))

;; end hacks


;; example objects

(defparameter *foo*
  (make-instance 'my-draw-point-output-record
                 :line-style '(:thickness 2)
                 :point-x 192
                 :point-y 64))

(recorded-slots (class-of *foo*))

(let ((c (class-of *foo*)))
  (mapcar (lambda (slot)
            (cons
             (keyword-argument slot)
             (clim-mop:slot-value-using-class c *foo* slot)))
          (recorded-slots c)))

(output-record-superclasses (class-of *foo*))

;;;;


