(define (color-similar? c1 c2 tolerance)
  (let ((r1 (list-ref c1 0)) (g1 (list-ref c1 1)) (b1 (list-ref c1 2))
        (r2 (list-ref c2 0)) (g2 (list-ref c2 1)) (b2 (list-ref c2 2)))
    (and (<= (abs (- r1 r2)) tolerance)
         (<= (abs (- g1 g2)) tolerance)
         (<= (abs (- b1 b2)) tolerance))))

(define (script-fu-highlight-region img drawable tolerance margin xref yref)
  (let* (
         (w (car (gimp-image-get-width img)))
         (h (car (gimp-image-get-height img)))
         (color-ref (car (gimp-drawable-get-pixel drawable xref yref)))
         (xmin (max 0 (- xref margin)))
         (xmax (min (- w 1) (+ xref margin)))
         (ymin (max 0 (- yref margin)))
         (ymax (min (- h 1) (+ yref margin)))
         (new-layer (car (gimp-layer-new img "Color Similar" w h RGBA-IMAGE 100 LAYER-MODE-NORMAL)))
        )

    (gimp-image-insert-layer img new-layer 0 -1)

    (do ((x xmin (+ x 1)))
        ((> x xmax))
      (do ((y ymin (+ y 1)))
          ((> y ymax))
        (let* ((p (car (gimp-drawable-get-pixel drawable x y))))
          (when (color-similar? p color-ref tolerance)
            (gimp-drawable-set-pixel new-layer x y p)))))

    (gimp-displays-flush)
    (gimp-message "Región resaltada en una nueva capa con colores originales.")))

(script-fu-register
 "script-fu-highlight-region"
 "Highlight Region"
 "Crea una nueva capa con los píxeles similares al indicado (coordenadas manuales)"
 "Nicolas Bravo"
 "Nicolas Bravo"
 "2025"
 "RGB*, GRAY*"
 SF-IMAGE      "Imagen"     0
 SF-DRAWABLE   "Capa"       0
 SF-ADJUSTMENT "Tolerancia" '(30 0 255 1 10 0)
 SF-ADJUSTMENT "Margen (px)" '(20 1 200 1 10 0)
 SF-ADJUSTMENT "Coordenada X del píxel" '(0 0 10000 1 10 0)
 SF-ADJUSTMENT "Coordenada Y del píxel" '(0 0 10000 1 10 0)
)

(script-fu-menu-register "script-fu-highlight-region"
                         "<Image>/Filters/Custom")
