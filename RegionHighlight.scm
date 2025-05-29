; Funcion auxiliar que compara dos colores RGB y verifica si son similares dentro de una tolerancia dada
(define (color-similar? c1 c2 tolerance)
  (let ((r1 (list-ref c1 0)) (g1 (list-ref c1 1)) (b1 (list-ref c1 2))
        (r2 (list-ref c2 0)) (g2 (list-ref c2 1)) (b2 (list-ref c2 2)))
    ; Compara componente por componente rgb dentro de la tolerancia
    (and (<= (abs (- r1 r2)) tolerance)
         (<= (abs (- g1 g2)) tolerance)
         (<= (abs (- b1 b2)) tolerance))))

(define (script-fu-highlight-region img drawable tolerance margin xref yref mode highlight-color opacity)
  (let* (
         ; Obtiene dimensiones de la imagen
         (width (car (gimp-image-get-width img)))
         (height (car (gimp-image-get-height img)))

         ; Obtiene el color del pixel de referencia en las coordenadas dadas
         (color-ref (car (gimp-drawable-get-pixel drawable xref yref)))

         ; Define los limites del area a recorrer segun el margen especificado
         (xmin (max 0 (- xref margin)))
         (xmax (min (- width 1) (+ xref margin)))
         (ymin (max 0 (- yref margin)))
         (ymax (min (- height 1) (+ yref margin)))

         ; Crea una nueva capa transparente
         (new-layer (car (gimp-layer-new img "Color Similar" width height RGBA-IMAGE 100 LAYER-MODE-NORMAL)))

         ; Convierte la opacidad (0-100) a valor alfa (0-255)
         (alpha (inexact->exact (round (* 2.55 opacity))))
        )

    ; Inserta la nueva capa en la imagen
    (gimp-image-insert-layer img new-layer 0 -1)

    ; Rellena la nueva capa con transparencia
    (gimp-drawable-fill new-layer 4)

    ; Recorre los pixeles dentro del area definida
    (do ((x xmin (+ x 1)))
        ((> x xmax))
      (do ((y ymin (+ y 1)))
          ((> y ymax))
        (let* (
               ; Obtiene el color del pixel actual
               (pixel (car (gimp-drawable-get-pixel drawable x y))))
          ; Si el color es similar al color de referencia
          (when (color-similar? pixel color-ref tolerance)
            (if (= mode 0)
                ; Modo 0: copia el color original a la nueva capa
                (gimp-drawable-set-pixel new-layer x y pixel)
                ; Modo 1: aplica un color resaltado con opacidad
                (gimp-drawable-set-pixel new-layer x y
                  (list (list-ref highlight-color 0)
                        (list-ref highlight-color 1)
                        (list-ref highlight-color 2)
                        alpha)))))))

    ; Refresca la pantalla
    (gimp-displays-flush)
    (gimp-message "Region resaltada en nueva capa.")))

(script-fu-register
 "script-fu-highlight-region"
 "Highlight Region"
 "Crea una nueva capa con los pixeles similares al indicado. Se puede resaltar con el color original o un color escogido."
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 "RGB*, GRAY*"
 SF-IMAGE      "Imagen"     0
 SF-DRAWABLE   "Capa"       0
 SF-ADJUSTMENT "Tolerancia" '(30 0 255 1 10 0)
 SF-ADJUSTMENT "Margen (px)" '(20 1 200 1 10 0)
 SF-ADJUSTMENT "Coordenada X del pixel" '(0 0 10000 1 10 0)
 SF-ADJUSTMENT "Coordenada Y del pixel" '(0 0 10000 1 10 0)
 SF-OPTION     "Modo de resaltado" '("Color original" "Color resaltado")
 SF-COLOR      "Color resaltado" '(255 0 0)
 SF-ADJUSTMENT "Opacidad (0–100)" '(50 0 100 1 5 0)
)

(script-fu-menu-register "script-fu-highlight-region" "<Image>/Filters/Script-Fu")
