from .vec2_float import Vec2Float
from .color_float import ColorFloat
class ColoredVertex:
    def __init__(self, position, color):
        self.position = position
        self.color = color
    @staticmethod
    def read_from(stream):
        position = Vec2Float.read_from(stream)
        color = ColorFloat.read_from(stream)
        return ColoredVertex(position, color)
    def write_to(self, stream):
        self.position.write_to(stream)
        self.color.write_to(stream)
    def __repr__(self):
        return "ColoredVertex(" + \
            repr(self.position) + "," + \
            repr(self.color) + \
            ")"
