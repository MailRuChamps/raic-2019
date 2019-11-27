from .vec2_double import Vec2Double
from .vec2_double import Vec2Double
from .item import Item
class LootBox:
    def __init__(self, position, size, item):
        self.position = position
        self.size = size
        self.item = item
    @staticmethod
    def read_from(stream):
        position = Vec2Double.read_from(stream)
        size = Vec2Double.read_from(stream)
        item = Item.read_from(stream)
        return LootBox(position, size, item)
    def write_to(self, stream):
        self.position.write_to(stream)
        self.size.write_to(stream)
        self.item.write_to(stream)
    def __repr__(self):
        return "LootBox(" + \
            repr(self.position) + "," + \
            repr(self.size) + "," + \
            repr(self.item) + \
            ")"
