(define (script-fu-expandir-seleccion img layer factor)
  (let* (
         ; Validar si hay seleccion activa
         (selection (= (car (gimp-selection-is-empty img)) FALSE))
        )

    (if (not selection)
        (gimp-message "No hay ninguna seleccion activa.")
        (begin
          ; Crear una copia de la capa activa
          (let* ((new-layer (car (gimp-layer-copy layer TRUE))))

            (gimp-image-undo-group-start img)
            (gimp-context-set-interpolation INTERPOLATION-CUBIC)
            (gimp-image-insert-layer img new-layer 0 -1)

            ; Aplicar mascara basada en la seleccion
            (let ((mask (car (gimp-layer-create-mask new-layer ADD-MASK-SELECTION))))
              (gimp-layer-add-mask new-layer mask)
              (gimp-layer-remove-mask new-layer MASK-APPLY))

            ; Comprobar si la capa resultante contiene algo visible
            (if (gimp-drawable-has-alpha new-layer)
                (let* (
                       (width (car (gimp-drawable-get-width new-layer)))
                       (height  (car (gimp-drawable-get-height new-layer)))
                       (offsets (gimp-drawable-get-offsets new-layer))
                       (offset-x (car offsets))
                       (offset-y (cadr offsets))
                       (new-width (* width factor))
                       (new-height  (* height factor))
                       (new-offset-x (- offset-x (/ (- new-width width) 2)))
                       (new-offset-y (- offset-y (/ (- new-height height) 2)))
                      )

                  ; Escalar y centrar
                  (gimp-layer-scale new-layer new-width new-height FALSE)
                  (gimp-layer-set-offsets new-layer new-offset-x new-offset-y)
                  (gimp-item-set-name new-layer "Seleccion Expandida"))

                ; Si no hay pixeles visibles, eliminar la capa nueva
                (begin
                  (gimp-image-remove-layer img new-layer)
                  (gimp-message "La seleccion no contiene pixeles visibles."))))

            (gimp-displays-flush)
            (gimp-image-undo-group-end img)))))

(script-fu-register
 "script-fu-expandir-seleccion"
 "Expand"
 "Duplica y expande solo la seleccion activa usando interpolacion cubica."
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 "RGB*, GRAY*"
 SF-IMAGE "Imagen" 0
 SF-DRAWABLE "Capa" 0
 SF-ADJUSTMENT "Factor de expansión" '(1.5 0.1 10.0 0.1 1 2))

(script-fu-menu-register "script-fu-expandir-seleccion" "<Image>/Filters/Script-Fu")
