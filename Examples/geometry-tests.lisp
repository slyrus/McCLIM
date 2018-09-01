
(cl:defpackage :geometry-tests
  (:use #:clim-lisp #:clim)
  (:export #:geometry-tests))

(in-package :geometry-tests)

;;
;; Let's start with a very simple pane.
;;
;; What can we do with this?
;;
;; 1. We can resize the pane and we see that the pane's sheet region
;; is.
(defclass simple-pane (permanent-medium-sheet-output-mixin basic-pane)
  ((resize-count :accessor resize-count :initform 0)))

(defmethod handle-repaint ((pane simple-pane) region)
  (declare (ignore region))
  (multiple-value-bind (x1 y1 x2 y2)
      (bounding-rectangle* (sheet-region pane))
    (draw-rectangle* pane (+ x1 10) (+ y1 10) (- x2 10) (- y2 10)
                     :line-thickness 5 :ink +red+ :filled nil)
    (draw-rectangle* pane (+ x1 15) (+ y1 15) (- x2 15) (- y2 15)
                     :line-thickness 5 :ink +light-blue+ :filled t)
    (draw-polygon* pane
                   '(100 100 200 100 180 140 130 130 100 100)
                   :line-thickness 5 :ink +brown+ :filled nil)
    (draw-text* pane (format nil "sheet-region: (~D ~D ~D ~D)" x1 y1 x2 y2) 30 30)
    (draw-text* pane (format nil "resize count ~D" (resize-count pane)) 30 60)
    (draw-text* pane "Some Huge Text" 30 200 :text-size :huge)))

(defmethod note-sheet-region-changed ((pane simple-pane))
  (incf (resize-count pane)))

(define-application-frame simple-app () ()
  (:panes
   (simple (make-pane 'simple-pane :width 400 :height 400)))
  (:layouts
   (default simple)))

(defvar *simple-app*)

(defun simple-app-main (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'simple-app)))
             (setf *simple-app* frame)
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Simple App")
        (run))))


;;
;; Now let's make a more flexible pane that we can do things
;; with. We'll use basic-pane as a superclass but, unfortunately
;; that's not enough for current McCLIM. It would be great if we could
;; subclass a basic-pane directly and only sepcialize the behavior we
;; need, but to get things working we have to add
;; permanent-medium-sheet-output-mixin as a superclass as well.
;;
;; This not an application pane.
(defclass flexible-pane (permanent-medium-sheet-output-mixin basic-pane)
  ((resize-count :accessor resize-count :initform 0)
   (transform-count :accessor transform-count :initform 0)))

(defmethod handle-repaint ((pane flexible-pane) region)
  (declare (ignore region))
  (multiple-value-bind (x1 y1 x2 y2)
      (bounding-rectangle* (sheet-region pane))
    (draw-rectangle* pane (+ x1 10) (+ y1 10) (- x2 10) (- y2 10)
                     :line-thickness 5 :ink +red+ :filled nil)
    (draw-rectangle* pane (+ x1 15) (+ y1 15) (- x2 15) (- y2 15)
                     :line-thickness 5 :ink +light-blue+ :filled t)
    (draw-polygon* pane
                   '(100 120 200 120 180 160 130 150 100 120)
                   :line-thickness 5 :ink +brown+ :filled nil)
    (draw-text* pane (format nil "sheet-region: (~D ~D ~D ~D)" x1 y1 x2 y2) 30 30)
    (multiple-value-bind (mxx mxy myx myy tx ty)
        (climi::get-transformation
         (sheet-transformation pane))
      (draw-text* pane
                  (format nil "sheet-transformation: (~D ~D ~D ~D ~D ~D)"
                          mxx myx mxy myy tx ty)
                  30 60))
    (draw-text* pane (format nil "resize counter ~D" (resize-count pane)) 30 90)
    (draw-text* pane (format nil "transform counter ~D" (transform-count pane)) 150 90)
    (draw-text* pane "24 pt serif text" 30 220 :text-size 24 :text-family :serif)
    (draw-text* pane "48 pt orange text" 30 300 :text-size 48 :ink +orange+)))

(defmethod note-sheet-region-changed ((pane flexible-pane))
  (incf (resize-count pane)))

(defmethod note-sheet-transformation-changed ((pane flexible-pane))
  (incf (transform-count pane)))

(define-application-frame flexible-app () ()
  (:panes
   (flexible (make-pane 'flexible-pane :width 400 :height 400)))
  (:layouts
   (default flexible)))

(defvar *flexible-app*)

(defun flexible-app-main (&key (new-process t))
  (flet ((run ()
           (let ((clim:*default-server-path*
                  clim:*default-server-path*))
             (setf (getf (cdr clim:*default-server-path*) :mirroring) :full)
             (let ((frame (make-application-frame 'flexible-app)))
               (setf *flexible-app* frame)
               (run-frame-top-level frame)))))
    (if new-process
        (clim-sys:make-process #'run :name "Flexible App")
        (run))))

#|
(let ((pane (find-pane-named *flexible-app* 'flexible)))
  (multiple-value-bind (mxx mxy myx myy tx ty)
      (climi::get-transformation
       (sheet-transformation pane))
    (setf (sheet-transformation pane)
          (make-transformation 1 0 myx 1 tx 4))
    (repaint-sheet pane +everywhere+)))


(let ((clim:*default-server-path* '(:clx :mirroring :single)))
  (flexible-app-main))
|#
