#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import gi
import sys

gi.require_version("Gimp", "3.0")
gi.require_version("Gtk", "3.0")
from gi.repository import Gimp, GObject, Gtk

class AddText(Gimp.PlugIn):
    def do_query_procedures(self):
        return ["python-fu-add-text"]

    def do_set_i18n(self, domain):
        return False

    def do_create_procedure(self, name):
        proc = Gimp.ImageProcedure.new(
            self, name, Gimp.PDBProcType.PLUGIN,
            self.run, None)

        proc.set_image_types("RGB*, GRAY*")
        proc.set_menu_label("Add text")  
        proc.add_menu_path("<Image>/Filters/Python-Fu/") 

        # Argumentos
        proc.add_string_argument("text", "Texto", "Texto a anadir", "Hola desde GIMP", GObject.ParamFlags.READWRITE)
        proc.add_double_argument("x", "Posicion X", "Posicion X en pixeles", 0, 1000, 10, GObject.ParamFlags.READWRITE)
        proc.add_double_argument("y", "Posicion Y", "Posicion Y en pixeles", 0, 1000, 10, GObject.ParamFlags.READWRITE)
        
        # Fuente por defecto
        default_font = Gimp.Font.get_by_name("Sans")
        proc.add_font_argument("font", "Fuente", "Fuente del texto", False, default_font, True, GObject.ParamFlags.READWRITE)

        proc.add_double_argument("size", "Tamano", "Tamano fuente", 1, 200, 40, GObject.ParamFlags.READWRITE)

        return proc

    def run(self, procedure, run_mode, image, drawables, config, run_data):

        # Cuadro de dialogo GTK
        if run_mode == Gimp.RunMode.INTERACTIVE:
            dialog = Gtk.Dialog(title="Add text", transient_for=None, flags=0)
            dialog.add_buttons(Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL,
                               Gtk.STOCK_OK, Gtk.ResponseType.OK)
            dialog.set_modal(True)
            dialog.set_default_size(300, 150)

            box = dialog.get_content_area()

            # Widget introducir texto y posicion
            label_text = Gtk.Label(label="Texto a anadir:")
            entry_text = Gtk.Entry()
            entry_text.set_text(config.get_property("text"))

            label_x = Gtk.Label(label="Posicion X (px):")
            entry_x = Gtk.SpinButton.new_with_range(0, 10000, 1)
            entry_x.set_value(config.get_property("x"))

            label_y = Gtk.Label(label="Posicion Y (px):")
            entry_y = Gtk.SpinButton.new_with_range(0, 10000, 1)
            entry_y.set_value(config.get_property("y"))

            # Organizar widget
            grid = Gtk.Grid(column_spacing=10, row_spacing=10, margin=10)
            grid.attach(label_text, 0, 0, 1, 1)
            grid.attach(entry_text, 1, 0, 1, 1)
            grid.attach(label_x, 0, 1, 1, 1)
            grid.attach(entry_x, 1, 1, 1, 1)
            grid.attach(label_y, 0, 2, 1, 1)
            grid.attach(entry_y, 1, 2, 1, 1)

            box.add(grid)
            dialog.show_all()

            response = dialog.run()

            if response == Gtk.ResponseType.OK:
                text = entry_text.get_text()
                x = entry_x.get_value()
                y = entry_y.get_value()
            else:
                dialog.destroy()
                return procedure.new_return_values(Gimp.PDBStatusType.CANCEL, None)

            dialog.destroy()

        else:
            return procedure.new_return_values(Gimp.PDBStatusType.CANCEL, None)

        font = config.get_property("font")
        size = config.get_property("size")
        unit = Gimp.Unit.pixel()  

        # Crear capa de texto con los parametros dados
        text_layer = Gimp.TextLayer.new(image, text, font, size, unit)
        image.insert_layer(text_layer, None, 0)
        text_layer.set_offsets(int(x), int(y))  

        Gimp.displays_flush()

        return procedure.new_return_values(Gimp.PDBStatusType.SUCCESS, None)

Gimp.main(AddText.__gtype__, sys.argv)
