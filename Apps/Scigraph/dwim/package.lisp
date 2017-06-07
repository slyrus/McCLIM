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

(cl:defpackage #:dwim
  (:use #:clim #:clim-lisp)
  (:shadow #:parse-text-style
           #:command-table-inherit-from
           #:menu-choose
           #:continuation-output-size
           #:with-room-for-output
           #:erase-graphics-presentation
           #:presentation-under-pointer
           #:presentation-p
           #:presentation-superior
           #:presentation-object
           #:presentation-subtypep
           #:presentation-type-p
           #:describe-presentation-type
           #:bounding-rectangle*
           #:redisplay
           #:redisplayable-format
           #:accept
           #:accepting-values
           #:accept-values
           #:accept-variable-values
           #:menu-choose
           #:formatting-table
           #:formatting-row
           #:formatting-column
           #:formatting-column-headings
           #:formatting-cell
           #:formatting-item-list
           #:format-item-list
           #:read-char-for-accept
           #:peek-char-for-accept
           #:unread-char-for-accept
           #:compare-char-for-accept
           #:read-token
           #:input-position
           #:insertion-pointer
           #:input-not-of-required-type
           #:catching-parser-failures
           #:validate-object
           #:with-accept-activation-chars
           #:accept-activation-p
           #:with-accept-blip-chars
           #:accept-blip-p
           #:with-activation-characters
           #:with-blip-characters
           #:completing-from-suggestions
           #:complete-from-sequence
           #:with-presentation-input-context
           #:with-input-context
           #:sheet
           #:accept-values-choose-from-sequence
           #:alist-subset
           #:invisible-object
           
           #:status-pane
           #:status-line
           #:set-status-line
           #:mouse-documentation-pane 
           #:*include-machine-name-in-status-line-p*
           #:*frame-for-status-line*
           #:*time-type*
           #:initialize-status-line
           #:make-status-line
           #:refresh-status-line
           #:noting-progress
           #:note-progress

           #:window-under-mouse
           #:window-clear
           #:change-size
           #:pane-frame
           #:redisplay-frame-pane
           #:sheet-parent
           #:stream-current-text-style
           #:stream-merged-text-style
           #:stream-line-height
           #:stream-character-width
           #:stream-string-width
           #:stream-cursor-position*
           #:stream-set-cursor-position*
           #:stream-increment-cursor-position*
           #:stream-viewport
           #:stream-viewport-size
           #:sheet-inside-size
           #:sheet-inside-width
           #:sheet-inside-height
           #:sheet-left-margin-size
           #:sheet-top-margin-size

           #:stream-pointer-position*
           #:stream-set-pointer-position*
           #:pointer-input-rectangle*
           #:noting-progress
           #:note-progress

           #:frame-manager
           #:find-frame-manager
           #:get-reusable-frame
           #:start-frame
           #:reset-frame
           #:make-application-frame
           #:move-frame
           #:size-frame
           #:get-frame-pane
           #:frame-current-layout
           #:set-frame-layout
           #:window-set-viewport-position*
           #:window-history-limits
           #:select-frame
           #:find-program-window
           #:launch-frame

           #:make-color-rgb
           #:color-exists-p
           #:color-stream-p
           #:with-clipping-from-output
           #:with-underlining
           #:surrounding-output-with-border
           #:%flip
           #:%draw
           #:%erase
           #:%alu
           #:draw-point
           #:draw-line
           #:draw-string
           #:draw-string-image
           #:draw-polygon
           #:draw-triangle
           #:draw-circle
           #:draw-rectangle
           #:draw-ellipse

           #:printing-random-object
           #:with-stack-list
           #:with-stack-array
           #:store-conditional
           #:stack-let
           #:without-interrupts
           #:condition-case
           #:make-command-table
           #:define-command-table
           #:find-command-table
           #:install-command
           #:define-presentation-to-command-translator
           #:define-presentation-translator
           #:define-presentation-action
           #:define-presentation-type
           #:with-output-as-presentation
           #:with-output-truncation
           #:with-output-recording-enabled
           #:with-output-recording-disabled
           #:with-redisplayable-output
           #:with-character-face
           #:with-text-face
           #:with-character-style
           #:with-character-size
           #:with-character-family
           #:with-text-style
           #:with-frame
           #:with-program

           #:with-rem-keywords
           #:rem-keywords
           #:*source-pathname*
           #:working-directory
           #:getenv
           #:run-shell-command
           #:process-wait
           #:process-run-function
           #:with-process-lock
           #:dotimes-unrolled
           #:instancep
           #:type-specifier-p
           #:file-type-for-binaries
           #:file-type-for-sources
           #:dump-objects-to-file


           #:alist-member

           #:array-register)
  (:export #:menu-choose

           #:continuation-output-size
           #:with-room-for-output
           #:erase-graphics-presentation
           #:presentation-under-pointer
           #:presentation-p
           #:presentation-superior
           #:presentation-object
           #:presentation-subtypep
           #:presentation-type-p
           #:present-to-string
           #:describe-presentation-type
           #:bounding-rectangle*
           #:redisplay
           #:redisplayable-format
           #:accept
           #:accepting-values
           #:accept-values
           #:accept-variable-values
           #:menu-choose
           #:formatting-table
           #:formatting-row
           #:formatting-column
           #:formatting-column-headings
           #:formatting-cell
           #:formatting-item-list
           #:format-item-list
           #:read-char-for-accept
           #:peek-char-for-accept
           #:unread-char-for-accept
           #:compare-char-for-accept
           #:read-token
           #:input-position
           #:insertion-pointer
           #:input-not-of-required-type
           #:catching-parser-failures
           #:validate-object
           #:with-accept-activation-chars
           #:accept-activation-p
           #:with-accept-blip-chars
           #:accept-blip-p
           #:with-activation-characters
           #:with-blip-characters
           #:completing-from-suggestions
           #:complete-from-sequence
           #:with-presentation-input-context
           #:with-input-context
           #:sheet
           #:accept-values-choose-from-sequence
           #:alist-subset
           #:invisible-object
           
           #:status-pane
           #:status-line
           #:set-status-line
           #:mouse-documentation-pane 
           #:*include-machine-name-in-status-line-p*
           #:*frame-for-status-line*
           #:*time-type*
           #:initialize-status-line
           #:make-status-line
           #:refresh-status-line
           #:noting-progress
           #:note-progress

           #:window-under-mouse
           #:window-clear
           #:change-size
           #:pane-frame
           #:redisplay-frame-pane
           #:sheet-parent
           #:stream-current-text-style
           #:stream-merged-text-style
           #:stream-line-height
           #:stream-character-width
           #:stream-string-width
           #:stream-cursor-position*
           #:stream-set-cursor-position*
           #:stream-increment-cursor-position*
           #:stream-viewport
           #:stream-viewport-size
           #:sheet-inside-size
           #:sheet-inside-width
           #:sheet-inside-height
           #:sheet-left-margin-size
           #:sheet-top-margin-size

           #:stream-pointer-position*
           #:stream-set-pointer-position*
           #:pointer-input-rectangle*
           #:noting-progress
           #:note-progress

           #:frame-manager
           #:find-frame-manager
           #:get-reusable-frame
           #:start-frame
           #:reset-frame
           #:make-application-frame
           #:move-frame
           #:size-frame
           #:get-frame-pane
           #:frame-current-layout
           #:set-frame-layout
           #:window-set-viewport-position*
           #:window-history-limits
           #:select-frame
           #:find-program-window
           #:launch-frame

           #:make-color-rgb
           #:color-exists-p
           #:color-stream-p
           #:with-clipping-from-output
           #:with-underlining
           #:surrounding-output-with-border
           #:%flip %draw %erase %alu
           #:draw-point
           #:draw-line
           #:draw-string
           #:draw-string-image
           #:draw-polygon
           #:draw-triangle
           #:draw-circle
           #:draw-rectangle
           #:draw-ellipse

           #:printing-random-object
           #:with-stack-list
           #:with-stack-array
           #:store-conditional
           #:stack-let
           #:without-interrupts
           #:condition-case
           #:make-command-table
           #:define-command-table
           #:find-command-table
           #:install-command
           #:define-presentation-to-command-translator
           #:define-presentation-translator
           #:define-presentation-action
           #:define-presentation-type
           #:with-output-as-presentation
           #:with-output-truncation
           #:with-output-recording-enabled
           #:with-output-recording-disabled
           #:with-redisplayable-output
           #:with-character-face
           #:with-text-face
           #:with-character-style
           #:with-character-size
           #:with-character-family
           #:with-text-style
           #:with-frame
           #:with-program

           #:with-rem-keywords
           #:rem-keywords
           #:*source-pathname*
           #:working-directory
           #:getenv
           #:run-shell-command
           #:process-wait
           #:process-run-function
           #:with-process-lock
           #:dotimes-unrolled
           #:instancep
           #:type-specifier-p
           #:file-type-for-binaries
           #:file-type-for-sources
           #:dump-objects-to-file


           #:alist-member

           #:array-register))

(defpackage #:dwim-lisp
  (:use #:dwim #:clim #:clim-lisp)
  (:shadowing-import-from #:dwim
                          #:present
                          #:present-to-string
                          #:presentation-type
                          #:menu-choose

                          #:continuation-output-size
                          #:with-room-for-output
                          #:erase-graphics-presentation
                          #:presentation-under-pointer
                          #:presentation-p
                          #:presentation-superior
                          #:presentation-object
                          #:presentation-subtypep
                          #:presentation-type-p
                          #:present-to-string
                          #:describe-presentation-type
                          #:bounding-rectangle*
                          #:redisplay
                          #:redisplayable-format
                          #:accept
                          #:accepting-values
                          #:accept-values
                          #:accept-variable-values
                          #:menu-choose
                          #:formatting-table
                          #:formatting-row
                          #:formatting-column
                          #:formatting-column-headings
                          #:formatting-cell
                          #:formatting-item-list
                          #:format-item-list
                          #:read-char-for-accept
                          #:peek-char-for-accept
                          #:unread-char-for-accept
                          #:compare-char-for-accept
                          #:read-token
                          #:input-position
                          #:insertion-pointer
                          #:input-not-of-required-type
                          #:catching-parser-failures
                          #:validate-object
                          #:with-accept-activation-chars
                          #:accept-activation-p
                          #:with-accept-blip-chars
                          #:accept-blip-p
                          #:with-activation-characters
                          #:with-blip-characters
                          #:completing-from-suggestions
                          #:complete-from-sequence
                          #:with-presentation-input-context
                          #:with-input-context
                          #:sheet
                          #:accept-values-choose-from-sequence
                          #:alist-subset
                          #:invisible-object
                          
                          #:status-pane
                          #:status-line
                          #:set-status-line
                          #:mouse-documentation-pane 
                          #:*include-machine-name-in-status-line-p*
                          #:*frame-for-status-line*
                          #:*time-type*
                          #:initialize-status-line
                          #:make-status-line
                          #:refresh-status-line
                          #:noting-progress
                          #:note-progress

                          #:window-under-mouse
                          #:window-clear
                          #:change-size
                          #:pane-frame
                          #:redisplay-frame-pane
                          #:sheet-parent
                          #:stream-current-text-style
                          #:stream-merged-text-style
                          #:stream-line-height
                          #:stream-character-width
                          #:stream-string-width
                          #:stream-cursor-position*
                          #:stream-set-cursor-position*
                          #:stream-increment-cursor-position*
                          #:stream-viewport
                          #:stream-viewport-size
                          #:sheet-inside-size
                          #:sheet-inside-width
                          #:sheet-inside-height
                          #:sheet-left-margin-size
                          #:sheet-top-margin-size
                          #:beep

                          #:stream-pointer-position*
                          #:stream-set-pointer-position*
                          #:pointer-input-rectangle*
                          #:noting-progress
                          #:note-progress

                          #:frame-manager
                          #:find-frame-manager
                          #:get-reusable-frame
                          #:start-frame
                          #:reset-frame
                          #:make-application-frame
                          #:move-frame
                          #:size-frame
                          #:get-frame-pane
                          #:frame-current-layout
                          #:set-frame-layout
                          #:window-set-viewport-position*
                          #:window-history-limits
                          #:select-frame
                          #:find-program-window
                          #:launch-frame

                          #:make-color-rgb
                          #:color-exists-p
                          #:color-stream-p
                          #:with-clipping-from-output
                          #:with-underlining
                          #:surrounding-output-with-border
                          #:%flip
                          #:%draw
                          #:%erase
                          #:%alu
                          #:draw-point
                          #:draw-line
                          #:draw-string
                          #:draw-string-image
                          #:draw-polygon
                          #:draw-triangle
                          #:draw-circle
                          #:draw-rectangle
                          #:draw-ellipse

                          #:printing-random-object
                          #:with-stack-list
                          #:with-stack-array
                          #:store-conditional
                          #:stack-let
                          #:without-interrupts
                          #:condition-case
                          #:make-command-table
                          #:define-command-table
                          #:find-command-table
                          #:install-command
                          #:define-presentation-to-command-translator
                          #:define-presentation-translator
                          #:define-presentation-action
                          #:define-presentation-type
                          #:with-output-as-presentation
                          #:with-output-truncation
                          #:with-output-recording-enabled
                          #:with-output-recording-disabled
                          #:with-redisplayable-output
                          #:with-character-face
                          #:with-text-face
                          #:with-character-style
                          #:with-character-size
                          #:with-character-family
                          #:with-text-style
                          #:with-frame
                          #:with-program

                          #:with-rem-keywords
                          #:rem-keywords
                          #:*source-pathname*
                          #:working-directory
                          #:getenv
                          #:run-shell-command
                          #:process-wait
                          #:process-run-function
                          #:with-process-lock
                          #:dotimes-unrolled
                          #:instancep
                          #:type-specifier-p
                          #:file-type-for-binaries
                          #:file-type-for-sources
                          #:dump-objects-to-file


                          #:alist-member

                          #:array-register))

(pushnew :dwim *features*)

