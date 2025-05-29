(define (script-fu-add-text img drawable text pos-x-prcnt pos-y-prcnt font size opacity color)
  (let* (
         ; Obtener el ancho y alto de la imagen
         (width (car (gimp-image-get-width img)))
         (height (car (gimp-image-get-height img)))

         ; Calcular posicion X e Y en pixeles a partir del porcentaje dado
         (pos-x (truncate (* (/ pos-x-prcnt 100) width)))
         (pos-y (truncate (* (/ pos-y-prcnt 100) height)))
        )
        
    ; Establecer el color para el texto
    (gimp-context-set-foreground color)

    (let* (
           ; Crear una nueva capa de texto con los parametros dados
           (text-layer (car (gimp-text-layer-new img text font size PIXELS)))

           ; Obtener las dimensiones del texto creado
           (text-width (car (gimp-drawable-get-width text-layer)))
           (text-height (car (gimp-drawable-get-height text-layer)))

           ; Calcular las coordenadas finales para centrar el texto
           (final-x (- pos-x (/ text-width 2)))
           (final-y (- pos-y (/ text-height 2)))
          )

      ; Insertar la capa de texto en la imagen
      (gimp-image-insert-layer img text-layer 0 -1)

      ; Establecer la posición de la capa de texto
      (gimp-layer-set-offsets text-layer final-x final-y)

      ; Establecer la opacidad de la capa de texto
      (gimp-layer-set-opacity text-layer opacity)

      ; Actualizar la interfaz de GIMP para reflejar los cambios
      (gimp-displays-flush))))

(script-fu-register
 "script-fu-add-text"                       
 "Add Text"
 "Inserta una capa de texto sobre la imagen."
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

(script-fu-menu-register "script-fu-add-text" "<Image>/Filters/Script-Fu")
