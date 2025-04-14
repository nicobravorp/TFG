(define (script-fu-convert-image-format input-path output-dir format)
  (let* (
         ;; Cargar imagen
         (image (car (gimp-file-load RUN-NONINTERACTIVE input-path input-path)))

         ;; Construir nombre de salida
         (path-parts (list->vector (strbreakup input-path "/")))
         (filename (vector-ref path-parts (- (vector-length path-parts) 1)))
         (name-parts (list->vector (strbreakup filename ".")))
         (name-no-ext (vector-ref name-parts 0))
         (output-filename (string-append output-dir "/" name-no-ext "." format))
        )

    ;; Guardar/exportar con gimp-file-save
    (gimp-file-save RUN-NONINTERACTIVE image output-filename output-filename #f)

    ;; Cerrar imagen
    (gimp-image-delete image)
    (gimp-message (string-append "Imagen guardada como: " output-filename))
  )
)

(script-fu-register
 "script-fu-convert-image-format"
 "Convert Image Format"
 "Convierte una imagen a otro formato (usando extensión)"
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 ""
 SF-FILENAME "Imagen de entrada" ""
 SF-DIRNAME  "Carpeta de salida" ""
 SF-STRING   "Formato de salida (ej. png, jpg, bmp, webp...)" "png"
)

(script-fu-menu-register "script-fu-convert-image-format" "<Image>/Filters/Custom")
