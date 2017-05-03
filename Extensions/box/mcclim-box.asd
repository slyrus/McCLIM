;;;
;;; Copyright (c) 2017, Cyrus Harmon (ch-lisp@bobobeach.com)
;;;
;;; See file 'LICENSE' for the copyright details
;;;

(defsystem #:mcclim-box
  :description "Support for boxes in McCLIM."
  :depends-on (#:clim-basic #:mcclim-render)
  :components ((:file "box")))
