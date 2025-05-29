
import cv2
import argparse

def main():
    parser = argparse.ArgumentParser(description="Agregar texto a una imagen usando OpenCV.")
    parser.add_argument('--image', type=str, required=True, help='Ruta de la imagen original.')
    parser.add_argument('--text', type=str, required=True, help='Texto a agregar.')
    parser.add_argument('--x', type=int, default=50, help='Posicion X del texto.')
    parser.add_argument('--y', type=int, default=50, help='Posicion Y del texto.')
    parser.add_argument('--font', type=int, default=0, help='Tipo de fuente OpenCV (0 a 7).')
    parser.add_argument('--size', type=float, default=1.0, help='Tamano del texto.')
    parser.add_argument('--color', type=int, nargs=3, default=[255, 255, 255], help='Color del texto en BGR (ej: 255 255 255).')
    parser.add_argument('--exit', type=str, default='imagen_salida.png', help='Ruta de la imagen de salida.')

    args = parser.parse_args()

    # Cargar la imagen
    img = cv2.imread(args.image)
    if img is None:
        print(f"No se pudo cargar la imagen: {args.image}")
        return

    # Insertar texto en la imagen
    font = args.font if 0 <= args.font <= 7 else 0
    color = tuple(args.color)
    cv2.putText(img, args.text, (args.x, args.y), font, args.size, color, 2, cv2.LINE_AA)

    # Guardar imagen
    cv2.imwrite(args.exit, img)
    print(f"Imagen guardada en: {args.exit}")

if __name__ == "__main__":
    main()
