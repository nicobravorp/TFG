import cv2
import argparse
import os

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
    if args.exit_path:
        root, ext = os.path.splitext(args.exit_path)
        if not ext:
            path = f"{args.exit_path}.{args.format}"
        else:
            path = args.exit_path
    else:
        path = f"{args.image.rsplit('.', 1)[0]}.{args.format}"

    # Guardar imagen en nuevo formato
    result = cv2.imwrite(path, img)
    if result:
        print(f"Imagen guardada en: {path}")
    else:
        print(f"No se pudo guardar la imagen en el formato {args.format}")

if __name__ == "__main__":
    main()
