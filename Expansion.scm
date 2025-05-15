(define (script-fu-expandir-seleccion imagen capa factor)
  (let* (
         ; Validar si hay selección activa
         (hay-seleccion (= (car (gimp-selection-is-empty imagen)) FALSE))
        )

    (if (not hay-seleccion)
        (gimp-message "No hay ninguna selección activa.")
        (begin
          ; Crear una copia de la capa activa
          (let* (
                 (nueva-capa (car (gimp-layer-copy capa TRUE)))
                 (anchura (car (gimp-drawable-get-width capa)))
                 (altura  (car (gimp-drawable-get-height capa)))
                )

            (gimp-image-undo-group-start imagen)
            (gimp-context-set-interpolation INTERPOLATION-CUBIC)
            (gimp-image-insert-layer imagen nueva-capa 0 -1)

            ; Aplicar máscara basada en la selección
            (let ((mask (car (gimp-layer-create-mask nueva-capa ADD-MASK-SELECTION))))
              (gimp-layer-add-mask nueva-capa mask)
              (gimp-layer-remove-mask nueva-capa MASK-APPLY))

            ; Comprobar si la capa resultante contiene algo visible
            (if (gimp-drawable-has-alpha nueva-capa)
                (let* (
                       (anchura-selec (car (gimp-drawable-get-width nueva-capa)))
                       (altura-selec  (car (gimp-drawable-get-height nueva-capa)))
                       (offsets (gimp-drawable-get-offsets nueva-capa))
                       (offset-x (car offsets))
                       (offset-y (cadr offsets))
                       (nueva-anchura (* anchura-selec factor))
                       (nueva-altura  (* altura-selec factor))
                       (nuevo-offset-x (- offset-x (/ (- nueva-anchura anchura-selec) 2)))
                       (nuevo-offset-y (- offset-y (/ (- nueva-altura altura-selec) 2)))
                      )

                  ; Escalar y centrar
                  (gimp-layer-scale nueva-capa nueva-anchura nueva-altura FALSE)
                  (gimp-layer-set-offsets nueva-capa nuevo-offset-x nuevo-offset-y)
                  (gimp-item-set-name nueva-capa "Selección Expandida"))

                ; Si no hay píxeles visibles, eliminar la capa nueva
                (begin
                  (gimp-image-remove-layer imagen nueva-capa)
                  (gimp-message "La selección no contiene píxeles visibles."))))

            (gimp-displays-flush)
            (gimp-image-undo-group-end imagen)))))

(script-fu-register
 "script-fu-expandir-seleccion"
 "Expandir Selección"
 "Duplica y expande solo la selección activa usando interpolación cúbica"
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 "RGB*, GRAY*"
 SF-IMAGE "Imagen" 0
 SF-DRAWABLE "Capa" 0
 SF-ADJUSTMENT "Factor de expansión" '(1.5 0.1 10.0 0.1 1 2))

(script-fu-menu-register "script-fu-expandir-seleccion" "<Image>/Filters/Custom")
