; Funcion auxiliar que verifica si una cadena str termina en el sufijo suffix
(define (string-suffix? suffix str)
  (let ((slen (string-length suffix))
        (len (string-length str)))
    (and (<= slen len)
         (string=? (substring str (- len slen) len) suffix))))

; Funcion auxiliar que obtiene el nombre del archivo sin la extension a partir de una ruta completa
(define (get-filename-without-extension path)
  (let* ((parts (strbreakup path "/"))
         (filename (list-ref parts (- (length parts) 1)))
         (name-parts (strbreakup filename "."))
         (base (if (> (length name-parts) 1)
                   (list-ref name-parts 0)
                   filename)))
    base))

(define (script-fu-binarize path dir umbral name)

  ; Asegurarse de que name es una cadena valida
  (set! name (if (string? name) name ""))

  (let* (
         ; Carga la imagen desde disco
         (image (car (gimp-file-load RUN-NONINTERACTIVE path path)))

         ; Obtiene las capas de la imagen
         (layers (gimp-image-get-layers image))

         ; Selecciona la primera capa o aplana la imagen si no hay ninguna
         (layer (if (and (vector? layers) (> (vector-length layers) 0))
                    (aref layers 0)
                    (begin
                      (car (gimp-image-flatten image)))))

         ; Crea una copia de la capa para aplicar el filtro
         (copy (car (gimp-layer-copy layer TRUE)))

         ; Extrae el nombre base del archivo de entrada
         (basename (get-filename-without-extension path))

         ; Elimina una barra final en el directorio si la hay
         (output-dir (if (string-suffix? "/" dir)
                               (substring dir 0 (- (string-length dir) 1))
                               dir))

         ; Construye el nombre del archivo de salida
         (filename (if (string=? name "")
                       (string-append basename "_binarized.png")
                       (if (string-suffix? ".png" name)
                           name
                           (string-append name ".png"))))

         ; Ruta completa de salida
         (output-name (string-append output-dir "/" filename))
        )

    ; Inserta la capa copiada en la imagen
    (gimp-image-insert-layer image copy 0 -1)

    ; Convierte a escala de grises
    (gimp-drawable-desaturate copy DESATURATE-LIGHTNESS)

    ; Aplica umbral de binarizacion segun el valor dado
    (gimp-drawable-threshold copy HISTOGRAM-VALUE (/ umbral 255.0) 1.0)

    ; Guarda la imagen resultante
    (gimp-file-save RUN-NONINTERACTIVE image output-name #f)

    ; Elimina la imagen de la memoria
    (gimp-image-delete image)
  )
)

(script-fu-register
 "script-fu-binarize"
 "Binarize"
 "Carga una imagen, aplica binarizacion y guarda el resultado."
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 ""
 SF-FILENAME "Archivo de imagen" ""
 SF-DIRNAME  "Directorio de salida" ""
 SF-ADJUSTMENT "Umbral" '(128 0 255 1 10 0 0)
 SF-STRING "Nombre de salida" ""
)

(script-fu-menu-register "script-fu-binarize" "<Image>/Filters/Script-Fu")
