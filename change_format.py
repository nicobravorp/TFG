import cv2
import argparse

def main():
    parser = argparse.ArgumentParser(description="Cambiar el formato de una imagen usando OpenCV.")
    parser.add_argument('--image', type=str, required=True, help='Ruta de la imagen original.')
    parser.add_argument('--format', type=str, required=True, help='Nuevo formato (png, jpg, bmp, etc.).')
    parser.add_argument('--exit_path', type=str, default='', help='Ruta opcional de la imagen de salida.')

    args = parser.parse_args()

    # Cargar la imagen
    img = cv2.imread(args.image)
    if img is None:
        print(f"No se pudo cargar la imagen: {args.image}")
        return

    # Determinar ruta de salida
    path = args.exit_path if args.exit_path else f"{args.image.rsplit('.', 1)[0]}.{args.format}"

    # Guardar imagen en nuevo formato
    cv2.imwrite(path, img)
    print(f"Imagen guardada en: {path}")

if __name__ == "__main__":
    main()
