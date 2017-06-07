;;; -*- Syntax: Common-lisp; Package: User -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(defpackage #:scigraph.package
  (:use :cl))

(in-package #:scigraph.package)

(defparameter *dwim-exported-symbol-name-hash-table*
  (let ((h (make-hash-table :test 'equal)))
    (do-external-symbols (x :dwim)
      (setf (gethash (symbol-name x) h) x))
    h))

(defparameter *clim-exported-symbols*
  (let (l)
    (do-external-symbols (x :clim)
      (unless (gethash (symbol-name x) *dwim-exported-symbol-name-hash-table*)
        (push x l)))
    l))

(when (find-package :tool)
  (map nil (lambda (x)
             (multiple-value-bind (symbol status)
                 (find-symbol (symbol-name x) :tool)
               (declare (ignore symbol))
               (when (eql status :external)
                 (unexport x :tool))))
       *clim-exported-symbols*))

(defpackage #:tool
  (:use #:clim-lisp #:dwim-lisp #:dwim)
  (:export #:with-stack-list-copy
           #:with-slot-copying
           #:copy-slot
           #:copy-set-slot
           #:copy-slots
           #:copyable-mixin
           #:copy-inner-class
           #:copy-self
           #:copy-inner
           #:copy-top-level
           #:copy

           #:dump-forms
           #:with-slot-dumping
           #:dump-set-slot
           #:dump-slot
           #:dump-slots
           #:final-dump
           #:dumpable-mixin

           #:duplicate-set
           #:duplicate-slots
           #:duplicator-methods

           #:several-choose
           #:choose-character-style
           #:window-edit-text
           #:string-size

           #:named-mixin
           #:name
           #:name-string
           #:make-name
           #:named-object
           #:declare-required-method))

(map nil (lambda (x)
           (multiple-value-bind (symbol status)
               (find-symbol (symbol-name x) :tool)
             (declare (ignore status))
             (unless symbol
               (import x :tool))))
     *clim-exported-symbols*)

(defpackage #:statistics
  (:nicknames stat st)
  (:use #:clim-lisp #:dwim-lisp #:dwim)
  (:export #:with-seed
           #:uniform
           #:uniform-0-1
           #:uniform-between
           #:gaussian-random
           #:gaussian
           #:random-yes-no
           #:erf))

(when (find-package :graph)
  (map nil (lambda (x)
             (multiple-value-bind (symbol status)
                 (find-symbol (symbol-name x) :graph)
               (declare (ignore symbol))
               (when (eql status :external)
                 (unexport x :graph))))
       *clim-exported-symbols*))

(defpackage #:graph
  (:shadow #:variable)
  (:use #:clim-lisp #:dwim-lisp #:tool #:statistics #:dwim)
                    ; shouldn't be inherited but is
  (:export #:make-demo-frame
           #:view-graphs
           #:display-graph
           #:save-postscript-graph
           #:display-graphs
           #:graph-presentation-type
           #:graph-under-presentation
           #:present-self-p
           #:fill-window-with-graphs
           #:graphs-for-slider
           #:autoscale-graphs
           #:auto-scale-needed
           #:auto-scale
           #:graph-auto-scale-limits
           #:display-data
           #:display-datum
           #:displayed?
           #:datum-position
           #:device-draw-line
           #:thickness
           #:symbologies
           #:graph-p
           #:graph-data-p
           #:map-data
           #:map-data-xy
           #:missing-data-threshold
           #:display
           #:erase
           #:move
           #:refresh
           #:zoom-stack
           #:copy
           #:dump-forms
           #:final-dump
           #:duplicator-methods
           #:duplicate-slots
           #:pop-accept-items
           #:pop-accept-label
           #:popup-accept-forms
           #:popup-accept
           #:popup-accept-standard-loop
           #:graph-under-mouse
           #:add-dataset
           #:datasets
           #:data
           #:define-graph-command
           #:duplicator-methods
           #:xy-inside
           #:set-xy-inside
           #:xy-to-uv
           #:xy-to-stream
           #:uv-to-xy
           #:screen-to-uv
           #:uv-to-screen
           #:name
           #:annotation
           #:point-annotation
           #:interval-annotation
           #:annotate
           #:annotate-graph
           #:annotate-interval
           #:annotate-point
           #:annotate-data-interval
           #:annotate-data-point
           #:description-choices
           #:default-text-style
           #:x-label
           #:y-label

           #:graph-data
           #:timeseries-data
           #:presentable-data-mixin
           #:graph-data-limits-mixin
           #:graph-data-auto-scale-mixin
           #:graph-data-color-mixin
           #:graph-data-symbology-mixin
           #:graph-data-add-datum-mixin
           #:presentable-graph-data-legend-mixin
           #:graph-data-legend-mixin
           #:basic-list-datum-mixin
           #:graph-data-list-map-mixin
           #:essential-graph-data-map-mixin
           #:basic-graph-data
           #:equation-data
           #:sample-data
           #:histogram-data
           #:multidimensional-data

           #:graph
           #:annotated-graph
           #:presentable-graph-mixin
           #:graph-datasets-ob-mixin
           #:graph-datasets-mixin
           #:graph-legend-mixin
           #:graph-relative-size-mixin
           #:graph-zoom-mixin
           #:graph-slider-interaction-mixin
           #:graph-slider-mixin
           #:graph-handle-mouse-mixin
           #:graph-mouse-resolution-mixin
           #:graph-auto-scale-ob-mixin
           #:graph-auto-scale-extensions-ob-mixin
           #:graph-auto-scale-extensions-mixin
           #:graph-limits-mixin			
           #:graph-auto-scale-mixin
           #:graph-grid-ob-mixin
           #:graph-grid-mixin
           #:horizontal-y-border-mixin
           #:vertical-y-border-mixin
           #:graph-border-ob-mixin
           #:graph-border-mixin
           #:basic-graph-ob-mixin
           #:basic-graph
           #:graph-with-reselectable-axes))

(map nil (lambda (x)
           (multiple-value-bind (symbol status)
               (find-symbol (symbol-name x) :graph)
             (declare (ignore status))
             (unless symbol
               (import x :graph))))
     *clim-exported-symbols*)

(in-package :graph)

(dwim:make-command-table :graph)

