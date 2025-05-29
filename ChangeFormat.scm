; Lista de formatos aceptados
(define valid-formats '("jpg" "jpeg" "png" "bmp" "tiff" "webp" "xcf"))

(define (script-fu-convert-image-format path dir name exitformat)
  (let* (
         ; Obtener el formato real a partir del indice seleccionado
         (format (list-ref valid-formats exitformat))

         ; Verificar que el formato está en la lista
         (format-valid? (member format valid-formats))

         ; Cargar imagen
         (image (car (gimp-file-load RUN-NONINTERACTIVE path path)))

         ; Extraer nombre original
         (path-parts (list->vector (strbreakup path "/")))
         (filename (vector-ref path-parts (- (vector-length path-parts) 1)))
         (name-parts (list->vector (strbreakup filename ".")))
         (name-no-ext (vector-ref name-parts 0))

         ; Usar name si no está vacio
         (final-name (if (string=? name "")
                         name-no-ext
                         name))

         (output-filename (string-append dir "/" final-name "." format))
        )

    ; Verificar formato
    (if (not format-valid?)
        (begin
          (gimp-message (string-append "Error: formato no válido: " format))
          (error "Formato de salida no admitido.")))

    ; Guardar imagen
    (gimp-file-save RUN-NONINTERACTIVE image output-filename output-filename)

    ; Cerrar imagen
    (gimp-image-delete image)
    (gimp-message (string-append "Imagen guardada como: " output-filename))
  )
)

(script-fu-register
 "script-fu-convert-image-format"
 "Convert Image Format"
 "Convierte una imagen a otro formato."
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 ""
 SF-FILENAME "Imagen de entrada" ""
 SF-DIRNAME  "Carpeta de salida" ""
 SF-STRING   "Nombre de salida" ""
 SF-OPTION   "Formato de salida" '("jpg" "jpeg" "png" "bmp" "tiff" "webp" "xcf")
)

(script-fu-menu-register "script-fu-convert-image-format" "<Image>/Filters/Script-Fu")
