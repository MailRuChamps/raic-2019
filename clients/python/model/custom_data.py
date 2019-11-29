class CustomData:
    @staticmethod
    def read_from(stream):
        discriminant = stream.read_int()
        if discriminant == Log.TAG:
            return CustomData.Log.read_from(stream)
        if discriminant == Rect.TAG:
            return CustomData.Rect.read_from(stream)
        if discriminant == Line.TAG:
            return CustomData.Line.read_from(stream)
        if discriminant == Polygon.TAG:
            return CustomData.Polygon.read_from(stream)
        if discriminant == PlacedText.TAG:
            return CustomData.PlacedText.read_from(stream)
        raise Exception("Unexpected discriminant value")

class Log(CustomData):
    TAG = 0
    def __init__(self, text):
        self.text = text
    @staticmethod
    def read_from(stream):
        text = stream.read_string()
        return Log(text)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_string(self.text)
    def __repr__(self):
        return "Log(" + \
            repr(self.text) + \
            ")"
CustomData.Log = Log
from .vec2_float import Vec2Float
from .vec2_float import Vec2Float
from .color_float import ColorFloat
class Rect(CustomData):
    TAG = 1
    def __init__(self, pos, size, color):
        self.pos = pos
        self.size = size
        self.color = color
    @staticmethod
    def read_from(stream):
        pos = Vec2Float.read_from(stream)
        size = Vec2Float.read_from(stream)
        color = ColorFloat.read_from(stream)
        return Rect(pos, size, color)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        self.pos.write_to(stream)
        self.size.write_to(stream)
        self.color.write_to(stream)
    def __repr__(self):
        return "Rect(" + \
            repr(self.pos) + "," + \
            repr(self.size) + "," + \
            repr(self.color) + \
            ")"
CustomData.Rect = Rect
from .vec2_float import Vec2Float
from .vec2_float import Vec2Float
from .color_float import ColorFloat
class Line(CustomData):
    TAG = 2
    def __init__(self, p1, p2, width, color):
        self.p1 = p1
        self.p2 = p2
        self.width = width
        self.color = color
    @staticmethod
    def read_from(stream):
        p1 = Vec2Float.read_from(stream)
        p2 = Vec2Float.read_from(stream)
        width = stream.read_float()
        color = ColorFloat.read_from(stream)
        return Line(p1, p2, width, color)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        self.p1.write_to(stream)
        self.p2.write_to(stream)
        stream.write_float(self.width)
        self.color.write_to(stream)
    def __repr__(self):
        return "Line(" + \
            repr(self.p1) + "," + \
            repr(self.p2) + "," + \
            repr(self.width) + "," + \
            repr(self.color) + \
            ")"
CustomData.Line = Line
from .colored_vertex import ColoredVertex
class Polygon(CustomData):
    TAG = 3
    def __init__(self, vertices):
        self.vertices = vertices
    @staticmethod
    def read_from(stream):
        vertices = []
        for _ in range(stream.read_int()):
            vertices_element = ColoredVertex.read_from(stream)
            vertices.append(vertices_element)
        return Polygon(vertices)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_int(len(self.vertices))
        for element in self.vertices:
            element.write_to(stream)
    def __repr__(self):
        return "Polygon(" + \
            repr(self.vertices) + \
            ")"
CustomData.Polygon = Polygon
from .vec2_float import Vec2Float
from .text_alignment import TextAlignment
from .color_float import ColorFloat
class PlacedText(CustomData):
    TAG = 4
    def __init__(self, text, pos, alignment, size, color):
        self.text = text
        self.pos = pos
        self.alignment = alignment
        self.size = size
        self.color = color
    @staticmethod
    def read_from(stream):
        text = stream.read_string()
        pos = Vec2Float.read_from(stream)
        alignment = TextAlignment(stream.read_int())
        size = stream.read_float()
        color = ColorFloat.read_from(stream)
        return PlacedText(text, pos, alignment, size, color)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_string(self.text)
        self.pos.write_to(stream)
        stream.write_int(self.alignment)
        stream.write_float(self.size)
        self.color.write_to(stream)
    def __repr__(self):
        return "PlacedText(" + \
            repr(self.text) + "," + \
            repr(self.pos) + "," + \
            repr(self.alignment) + "," + \
            repr(self.size) + "," + \
            repr(self.color) + \
            ")"
CustomData.PlacedText = PlacedText
