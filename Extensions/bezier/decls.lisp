
;;;
;;; it's unclear if we need this, but, just in case we do, here it
;;; is. For the moment this is not loaded by the ASDF system.

;;; decls

(in-package :clim-internals)

(declfun draw-quadrangle* (sheet x1 y1 x2 y2
                                 &rest args
                                 &key (filled t)
                                 ink clipping-region transformation line-style line-thickness
                                 line-unit line-dashes line-joint-shape))
