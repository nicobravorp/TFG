(define (script-fu-add-text-layer img drawable text pos-x-percent pos-y-percent font size opacity)
  ;; Obtener dimensiones de la imagen
  (let* ((width (car (gimp-image-get-width img)))
         (height (car (gimp-image-get-height img)))
         ;; Calcular posición real en píxeles basada en porcentaje
         (pos-x (truncate (* (/ pos-x-percent 100) width)))
         (pos-y (truncate (* (/ pos-y-percent 100) height)))
         ;; Crear la capa de texto con los parámetros especificados
         (text-layer (car (gimp-text-layer-new img text font size PIXELS))))

    ;; Añadir la capa de texto creada a la imagen
    (gimp-image-insert-layer img text-layer 0 -1)

    ;; Posicionar la capa de texto según coordenadas calculadas
    (gimp-layer-set-offsets text-layer pos-x pos-y)

    ;; Establecer la opacidad de la capa (0% transparente, 100% opaca)
    (gimp-layer-set-opacity text-layer opacity)

    ;; Refrescar la visualización de la imagen en la interfaz gráfica de GIMP
    (gimp-displays-flush)))

;; Registro del script para que aparezca en el menú de GIMP
(script-fu-register
  "script-fu-add-text-layer"                ;; Nombre del script (interno)
  "Add Text Layer"                 ;; Nombre en el menú
  "Añade una capa de texto proporcional sobre la imagen actual." ;; Descripción
  "Tu Nombre"                               ;; Autor
  "Tu Licencia"                             ;; Licencia
  "2025"                                    ;; Fecha
  "RGB*, GRAY*"                             ;; Tipos de imagen compatibles
  SF-IMAGE      "Imagen"          0
  SF-DRAWABLE   "Capa Activa"     0
  SF-STRING     "Texto"           "Texto aquí"
  SF-ADJUSTMENT "Posición X (%)"  '(50 0 100 1 10 0 1)
  SF-ADJUSTMENT "Posición Y (%)"  '(50 0 100 1 10 0 1)
  SF-FONT       "Fuente"          "Sans"
  SF-ADJUSTMENT "Tamaño fuente"   '(30 1 500 1 10 0 1)
  SF-ADJUSTMENT "Opacidad (%)"    '(100 0 100 1 10 0 1))

;; Registrar ubicación en el menú de GIMP
(script-fu-menu-register "script-fu-add-text-layer" "<Image>/Filters/Custom")
