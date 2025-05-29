import cv2
import argparse
import os

def string_suffix(suffix, string):
    return string.endswith(suffix)

def get_filename_without_extension(path):
    return os.path.splitext(os.path.basename(path))[0]

def main():
    parser = argparse.ArgumentParser(description="Binarizar una imagen usando OpenCV.")
    parser.add_argument('--image', type=str, required=True, help='Ruta de la imagen original.')
    parser.add_argument('--directory', type=str, required=True, help='Directorio de salida.')
    parser.add_argument('--threshold', type=int, default=128, help='Umbral para la binarización (0-255).')
    parser.add_argument('--name', type=str, default='', help='Nombre opcional para la imagen de salida.')

    args = parser.parse_args()

    # Cargar la imagen en escala de grises
    img = cv2.imread(args.image, cv2.IMREAD_GRAYSCALE)
    if img is None:
        print(f"No se pudo cargar la imagen: {args.image}")
        return

    # Aplicar binarización
    _, binarized = cv2.threshold(img, args.threshold, 255, cv2.THRESH_BINARY)

    # Preparar nombre del archivo de salida
    og_name = get_filename_without_extension(args.image)
    exit_name = args.name if args.name else f"{og_name}_binarized.png"
    if not string_suffix(".png", exit_name):
        exit_name += ".png"

    # Asegurar que el directorio no termina con barra
    exit_dir = args.directory.rstrip('/')

    # Ruta completa de salida
    exit_path = os.path.join(exit_dir, exit_name)

    # Guardar imagen binarizada
    cv2.imwrite(exit_path, binarized)
    print(f"Imagen binarizada guardada en: {exit_path}")

if __name__ == "__main__":
    main()
