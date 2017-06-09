;;; -*- Syntax: Common-lisp; Package: DWIM -*-
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

(in-package :dwim)

;;;These symbols inherited from other packages.
(eval-when (compile load eval)
  (export '(defpackage 
	    present
	    present-to-string
	    presentation-type
	    menu-choose)
	  'dwim))

;;;from present.lisp
(eval-when (compile load eval)
  (export '(presentation-under-pointer
	    presentation-p
	    presentation-object
	    presentation-subtypep
	    presentation-type-p
	    present-to-string
	    describe-presentation-type
	    bounding-rectangle*
	    redisplay
	    redisplayable-format
	    accept
	    accepting-values
	    accept-values
	    accept-variable-values
	    menu-choose
	    formatting-table
	    formatting-row
	    formatting-column
	    formatting-column-headings
	    formatting-cell
	    formatting-item-list
	    format-item-list
	    read-char-for-accept
	    peek-char-for-accept
	    unread-char-for-accept
	    compare-char-for-accept
	    read-token
	    input-position
	    insertion-pointer
	    input-not-of-required-type
	    catching-parser-failures
	    validate-object
	    with-accept-activation-chars
	    accept-activation-p
	    with-accept-blip-chars
	    accept-blip-p
	    with-activation-characters
	    with-blip-characters
	    completing-from-suggestions
	    suggest
	    complete-from-sequence
	    with-presentation-input-context
	    with-input-context
	    sheet
	    accept-values-choose-from-sequence
	    alist-subset
	    invisible-object)
	  'dwim))

;;;from draw.lisp
(eval-when (compile load eval)
  (export '(make-color-rgb
	    color-exists-p
	    color-stream-p
	    with-clipping-from-output
	    with-underlining
	    surrounding-output-with-border
	    %flip %draw %erase %alu
	    draw-point
	    draw-line
	    draw-string
	    draw-string-image
	    draw-polygon
	    draw-triangle
	    draw-circle
	    draw-rectangle
	    draw-ellipse)
	  'dwim))

;;;from tv.lisp
(eval-when (compile load eval)
  (export '(window-under-mouse
	    change-size
	    stream-line-height
	    stream-character-width
	    interactive-stream-p
	    stream-cursor-position*
	    stream-set-cursor-position*
	    stream-viewport
	    stream-viewport-size

	    stream-pointer-position*
	    stream-set-pointer-position*
	    pointer-input-rectangle*
	    noting-progress
	    note-progress

	    get-reusable-frame
	    start-frame
	    reset-frame
	    make-application-frame
	    set-frame-layout
	    window-set-viewport-position*
	    window-history-limits
	    select-frame
	    launch-frame)
	  'dwim))

;;;from macros.lisp
(eval-when (compile load eval)
  (export '(printing-random-object
	    with-stack-list
	    define-command
	    install-command
	    define-presentation-to-command-translator
	    define-presentation-translator
	    define-presentation-action
	    define-presentation-type
	    with-output-as-presentation
	    with-output-truncation
	    with-output-recording-enabled
	    with-output-recording-disabled
	    with-redisplayable-output
	    with-character-face
	    with-text-face
	    with-character-style
	    with-character-size
	    with-character-family
	    with-text-style)
	  'dwim))

;;;export the presentation types and declarations.
(eval-when (compile load eval)
  (export '(boolean alist-member command expression) 'dwim)
  (export '(downward-funarg downward-function array-register dynamic-extent)
	  'dwim))

;;;************************************************************
;;;
;;; DWIM-LISP is what user programs should use as the lisp package.  
;;; It's purpose is to handle all the hairy importing and shadowing 
;;; constraints so that users don't have to go through this every time 
;;; they define a package that uses dwim stuff.  DWIM-LISP exports 
;;; Common Lisp, CLOS, and the relevant DWIM symbols.  As ANSI
;;; Common Lisp comes along, DWIM should go along with it.
;;; User package definitions should look very simple, e.g.
;;;    (in-package 'my-package :use '(dwim-lisp))
;;; 

(defun export-inherited-symbols (package)
  (unless (packagep package)
    (setq package (find-package package)))
  (do-symbols (symbol package)
    (multiple-value-bind (sym presence)
	(find-symbol (symbol-name symbol) package)
      (declare (ignore presence))
      (unless (eq (symbol-package sym) package)
	(export sym package))))
  ;; These next lines are here so that the symbol NIL will get exported.
  (import '(nil) package)
  (export '(nil) package))

(eval-when (compile load eval)
  (defpackage dwim-lisp
    ;; Shadow the things defined in dwim that offer potential name conflicts
    ;; with common lisp.
    (:shadowing-import-from 
     "DWIM"
     :interactive-stream-p
     :process-run-function)
    (:use :common-lisp :dwim)))

(eval-when (load eval)
  (do-external-symbols (symbol (find-package :clim))
    ;; import everything from clim that offers no name conflicts.
    (unless (find-symbol (symbol-name symbol) :dwim-lisp)
      (import symbol :dwim-lisp)))
  (dwim::export-inherited-symbols (find-package "DWIM-LISP"))
  (pushnew :dwim *features*))

