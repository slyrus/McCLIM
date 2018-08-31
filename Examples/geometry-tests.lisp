
(cl:defpackage :geometry-tests
  (:use #:clim-lisp #:clim)
  (:export #:geometry-tests))

(in-package :geometry-tests)

;;
;; Let's start with a very simple pane.
(defclass simple-pane (permanent-medium-sheet-output-mixin basic-pane)
  ())

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
                   :line-thickness 5 :ink +brown+ :filled t :filled nil)
    (draw-text* pane (format nil "sheet-region: (~D ~D ~D ~D)" x1 y1 x2 y2) 30 30)))

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
