from .tile import Tile
class Level:
    def __init__(self, tiles):
        self.tiles = tiles
    @staticmethod
    def read_from(stream):
        tiles = []
        for _ in range(stream.read_int()):
            tiles_element = []
            for _ in range(stream.read_int()):
                tiles_element_element = Tile(stream.read_int())
                tiles_element.append(tiles_element_element)
            tiles.append(tiles_element)
        return Level(tiles)
    def write_to(self, stream):
        stream.write_int(len(self.tiles))
        for element in self.tiles:
            stream.write_int(len(element))
            for element in element:
                stream.write_int(element)
    def __repr__(self):
        return "Level(" + \
            repr(self.tiles) + \
            ")"
