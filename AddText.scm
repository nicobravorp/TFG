(define valid-formats '("jpg" "jpeg" "png" "bmp" "tiff" "webp" "xcf"))

(define (script-fu-convert-image-format input-path output-dir format-index custom-name)
  (let* (
         ; Obtener el formato real a partir del índice seleccionado
         (format (list-ref valid-formats format-index))

         ; Verificar que el formato está en la lista (repetido aquí por seguridad si se invoca desde terminal)
         (format-valid? (member format valid-formats))

         ; Cargar imagen
         (image (car (gimp-file-load RUN-NONINTERACTIVE input-path input-path)))

         ; Extraer nombre original
         (path-parts (list->vector (strbreakup input-path "/")))
         (filename (vector-ref path-parts (- (vector-length path-parts) 1)))
         (name-parts (list->vector (strbreakup filename ".")))
         (name-no-ext (vector-ref name-parts 0))

         ; Usar custom-name si no está vacío
         (final-name (if (string=? custom-name "")
                         name-no-ext
                         custom-name))

         (output-filename (string-append output-dir "/" final-name "." format))
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
 "Convert Image Forma"
 "Convierte una imagen a otro formato, con validación y nombre opcional"
 "Nicolás Bravo"
 "Nicolás Bravo"
 "2025"
 ""
 SF-FILENAME "Imagen de entrada" ""
 SF-DIRNAME  "Carpeta de salida" ""
 SF-OPTION   "Formato de salida" '("jpg" "jpeg" "png" "bmp" "tiff" "webp" "xcf")
 SF-STRING   "Nombre de salida" ""
)

(script-fu-menu-register "script-fu-convert-image-format" "<Image>/Filters/Custom")
