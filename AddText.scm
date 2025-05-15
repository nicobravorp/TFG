(define (string-suffix? suffix str)
  (let ((slen (string-length suffix))
        (len (string-length str)))
    (and (<= slen len)
         (string=? (substring str (- len slen) len) suffix))))

(define (get-filename-without-extension path)
  (let* ((parts (strbreakup path "/"))
         (filename (list-ref parts (- (length parts) 1)))
         (name-parts (strbreakup filename "."))
         (base (if (> (length name-parts) 1)
                   (list-ref name-parts 0)
                   filename)))
    base))

(define (script-fu-add-text-to-file input-path output-dir text pos-x-percent pos-y-percent font size opacity color)
  (gimp-message "Inicio del script de texto")

  (let* (
         (image (car (gimp-file-load RUN-NONINTERACTIVE input-path input-path)))
         (layers (gimp-image-get-layers image))
         (layer (if (and (vector? layers) (> (vector-length layers) 0))
                    (aref layers 0)
                    (begin
                      (gimp-message "Vector vacío: usando gimp-image-flatten")
                      (car (gimp-image-flatten image)))))
         (width (car (gimp-image-get-width image)))
         (height (car (gimp-image-get-height image)))
         (pos-x (truncate (* (/ pos-x-percent 100.0) width)))
         (pos-y (truncate (* (/ pos-y-percent 100.0) height)))
         (text-layer (car (gimp-text-layer-new image text font size PIXELS)))
         (text-width (car (gimp-drawable-get-width text-layer)))
         (text-height (car (gimp-drawable-get-height text-layer)))
         (offset-x (- pos-x (/ text-width 2)))
         (offset-y (- pos-y (/ text-height 2)))
         (basename (get-filename-without-extension input-path))
         (output-dir-clean (if (string-suffix? "/" output-dir)
                               (substring output-dir 0 (- (string-length output-dir) 1))
                               output-dir))
         (output-path (string-append output-dir-clean "/" basename "_text.png"))
        )

    (gimp-image-insert-layer image text-layer 0 -1)
    (gimp-text-layer-set-color text-layer color)
    (gimp-layer-set-offsets text-layer offset-x offset-y)
    (gimp-layer-set-opacity text-layer opacity)

    (gimp-message (string-append "Guardando en: " output-path))
    (gimp-file-save RUN-NONINTERACTIVE image output-path #f)
    (gimp-image-delete image)
    (gimp-message "Script de texto finalizado")
  )
)

(script-fu-register
 "script-fu-add-text-to-file"
 "Add Text To File"
 "Carga una imagen desde archivo, añade texto centrado y la guarda como PNG."
 "Nicolás Bravo"
 "Nicolás Bravo"
 "2025"
 ""
 SF-FILENAME "Archivo de imagen" ""
 SF-DIRNAME  "Directorio de salida" ""
 SF-STRING   "Texto"           "Texto aquí"
 SF-ADJUSTMENT "Posición X (%)"  '(50 0 100 1 10 0 1)
 SF-ADJUSTMENT "Posición Y (%)"  '(50 0 100 1 10 0 1)
 SF-FONT     "Fuente"          "Sans"
 SF-ADJUSTMENT "Tamaño fuente" '(30 1 500 1 10 0 1)
 SF-ADJUSTMENT "Opacidad (%)"  '(100 0 100 1 10 0 1)
 SF-COLOR    "Color del texto" '(0 0 0)
)

(script-fu-menu-register "script-fu-add-text-to-file" "<Image>/Filters/Custom")
