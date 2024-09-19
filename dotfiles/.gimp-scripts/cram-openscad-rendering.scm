(define (cram-openscad-rendering theImage width height colorCount)
	(let*
		(
			(oldWidth (car (gimp-image-width theImage)))
			(oldHeight (car (gimp-image-height theImage)))
			(squaresize (max oldWidth oldHeight))
			(theLayer (car (gimp-image-get-active-layer theImage))) ; Assuming one active layer for now
		)
		; (gimp-message (string-append "image: " (number->string theImage) ", oldW/H: " (number->string oldWidth) " " (number->string oldHeight) ", squaresize: " (number->string squaresize)))
		(gimp-image-resize theImage squaresize squaresize (/ (- squaresize oldWidth) 2) (/ (- squaresize oldHeight) 2))
		(gimp-layer-resize-to-image-size theLayer) ; Uses the background color to fill
		(gimp-context-set-interpolation INTERPOLATION-CUBIC)
		(gimp-image-scale theImage width height)
		(gimp-image-convert-indexed theImage
			CONVERT-DITHER-NONE
			CONVERT-PALETTE-GENERATE
			colorCount
			FALSE
			TRUE
			"")
		(gimp-layer-resize-to-image-size theLayer)
	)
)

(define (cram-current-openscad-rendering theImage width height colorCount)
	(gimp-undo-push-group-start theImage)
	(cram-openscad-rendering theImage width height colorCount)
	(gimp-undo-push-group-end theImage)
)

(script-fu-register
	"cram-current-openscad-rendering"                        ;function name
	"_Cram OpenSCAD Rendering"                                  ;menu label
	"Crams the image into a square and reduces the color depth"
	"TOGoS"                             ;author
	"copyright 2024, TOGoS"        ;copyright notice
	"2024-09-19"                          ;date created
	"*"                                      ;image type that the script works on
	SF-IMAGE       "The image" 0
	SF-ADJUSTMENT  "Width"     '(256 1 1024 32 32 0 1)
	SF-ADJUSTMENT  "Height"    '(256 1 1024 32 32 0 1)
	SF-ADJUSTMENT  "Colorrs"   '( 64 2  256  1  1 0 1)
)
(script-fu-menu-register "cram-current-openscad-rendering" "<Image>/Image/OpenS_CAD Renderings")

; Would be nice; don't know how to get at clipboard
;(script-fu-register
;	"cram-openscad-rendering-from-clipboard"                        ;function name
;	"_Cram OpenSCAD Rendering"                                  ;menu label
;	"Crams the image into a square and reduces the color depth"
;	"TOGoS"                             ;author
;	"copyright 2024, TOGoS"        ;copyright notice
;	"2024-09-19"                          ;date created
;	"*"                                      ;image type that the script works on
;	SF-ADJUSTMENT  "Width"     '(256 1 1024 32 32 0 1)
;	SF-ADJUSTMENT  "Height"    '(256 1 1024 32 32 0 1)
;	SF-ADJUSTMENT  "Colorrs"   '( 64 2  256  1  1 0 1)
;)
;(script-fu-menu-register "cram-openscad-rendering-from-clipboard" "<Image>File/Create/OpenS_CAD Renderings")
