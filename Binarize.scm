(define (binarize imagen drawable umbral)
  (let* (
          (copia (car (gimp-layer-copy drawable TRUE)))
        )

    ; Insertar la capa duplicada
    (gimp-image-insert-layer imagen copia 0 -1)

    ; Convertir a escala de grises
    (gimp-drawable-desaturate copia DESATURATE-LIGHTNESS)

    ; Aplicar umbral fijo
    (gimp-drawable-threshold copia HISTOGRAM-VALUE (/ umbral 255.0) 1.0)

    ; Refrescar la vista
    (gimp-displays-flush)
  )
)

(script-fu-register
  "binarize"
  "Binarize"
  "Convierte una imagen a blanco y negro usando umbral fijo."
  "Nicolas Bravo"
  "Nicolas Bravo"
  "2025"
  "RGB*, GRAY*"
  SF-IMAGE      "Imagen"         0
  SF-DRAWABLE   "Capa"           0
  SF-ADJUSTMENT "Umbral"         '(128 0 255 1 10 0 0)
)

(script-fu-menu-register "binarize" "<Image>/Filters/Custom")
