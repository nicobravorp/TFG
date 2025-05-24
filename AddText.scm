(define (script-fu-add-text img drawable text pos-x-percent pos-y-percent font size opacity color)
  (let* (
         (width (car (gimp-image-get-width img)))
         (height (car (gimp-image-get-height img)))
         (pos-x (truncate (* (/ pos-x-percent 100) width)))
         (pos-y (truncate (* (/ pos-y-percent 100) height)))
        )
    ; Establecer color antes de crear la capa de texto
    (gimp-context-set-foreground color)
    (let* (
           (text-layer (car (gimp-text-layer-new img text font size PIXELS)))
           (text-width (car (gimp-drawable-get-width text-layer)))
           (text-height (car (gimp-drawable-get-height text-layer)))
           (final-x (- pos-x (/ text-width 2)))
           (final-y (- pos-y (/ text-height 2)))
          )
      (gimp-image-insert-layer img text-layer 0 -1)
      (gimp-layer-set-offsets text-layer final-x final-y)
      (gimp-layer-set-opacity text-layer opacity)
      (gimp-displays-flush))))

(script-fu-register
 "script-fu-add-text"
 "Add Text"
 "Añade una capa de texto sobre la imagen."
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 "RGB*, GRAY*"
 SF-IMAGE      "Imagen"               0
 SF-DRAWABLE   "Capa activa"          0
 SF-STRING     "Texto"                "Texto"
 SF-ADJUSTMENT "Posición X (%)"      '(50 0 100 1 10 0 1)
 SF-ADJUSTMENT "Posición Y (%)"      '(50 0 100 1 10 0 1)
 SF-FONT       "Fuente"               "Sans"
 SF-ADJUSTMENT "Tamaño fuente"       '(30 1 500 1 10 0 1)
 SF-ADJUSTMENT "Opacidad (%)"        '(100 0 100 1 10 0 1)
 SF-COLOR      "Color del texto"     '(0 0 0))

(script-fu-menu-register "script-fu-add-text" "<Image>/Filters/Custom")
