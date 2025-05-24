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

(define (script-fu-binarize input-path output-dir umbral)
  (gimp-message "Inicio de script")

  (let* (
         (image (car (gimp-file-load RUN-NONINTERACTIVE input-path input-path)))
         (layers (gimp-image-get-layers image))
         (layer (if (and (vector? layers) (> (vector-length layers) 0))
                    (aref layers 0)
                    (begin
                      (gimp-message "Vector vacío: usando gimp-image-flatten")
                      (car (gimp-image-flatten image)))))
         (copy (car (gimp-layer-copy layer TRUE)))
         (basename (get-filename-without-extension input-path))
         (output-dir-clean (if (string-suffix? "/" output-dir)
                               (substring output-dir 0 (- (string-length output-dir) 1))
                               output-dir))
         (output-name (string-append output-dir-clean "/" basename "_binarized.png"))
        )

    (gimp-message (string-append "DEBUG output-name = [" output-name "]"))

    (gimp-image-insert-layer image copy 0 -1)
    (gimp-message "Capa preparada")

    (gimp-drawable-desaturate copy DESATURATE-LIGHTNESS)
    (gimp-message "Desaturación aplicada")

    (gimp-drawable-threshold copy HISTOGRAM-VALUE (/ umbral 255.0) 1.0)
    (gimp-message "Umbral aplicado")

    (gimp-file-save RUN-NONINTERACTIVE image output-name #f)
    (gimp-message (string-append "Guardado en: " output-name))

    (gimp-image-delete image)
    (gimp-message "Imagen eliminada de la memoria")
  )
)

(script-fu-register
 "script-fu-binarize"
 "Binarize"
 "Carga una imagen, aplica binarización y guarda el resultado."
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 ""
 SF-FILENAME "Archivo de imagen" ""
 SF-DIRNAME  "Directorio de salida" ""
 SF-ADJUSTMENT "Umbral" '(128 0 255 1 10 0 0)
)

(script-fu-menu-register "script-fu-binarize" "<Image>/Filters/Custom")
