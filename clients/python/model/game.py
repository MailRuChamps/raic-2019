from .properties import Properties
from .level import Level
from .player import Player
from .unit import Unit
from .bullet import Bullet
from .mine import Mine
from .loot_box import LootBox
class Game:
    def __init__(self, current_tick, properties, level, players, units, bullets, mines, loot_boxes):
        self.current_tick = current_tick
        self.properties = properties
        self.level = level
        self.players = players
        self.units = units
        self.bullets = bullets
        self.mines = mines
        self.loot_boxes = loot_boxes
    @staticmethod
    def read_from(stream):
        current_tick = stream.read_int()
        properties = Properties.read_from(stream)
        level = Level.read_from(stream)
        players = []
        for _ in range(stream.read_int()):
            players_element = Player.read_from(stream)
            players.append(players_element)
        units = []
        for _ in range(stream.read_int()):
            units_element = Unit.read_from(stream)
            units.append(units_element)
        bullets = []
        for _ in range(stream.read_int()):
            bullets_element = Bullet.read_from(stream)
            bullets.append(bullets_element)
        mines = []
        for _ in range(stream.read_int()):
            mines_element = Mine.read_from(stream)
            mines.append(mines_element)
        loot_boxes = []
        for _ in range(stream.read_int()):
            loot_boxes_element = LootBox.read_from(stream)
            loot_boxes.append(loot_boxes_element)
        return Game(current_tick, properties, level, players, units, bullets, mines, loot_boxes)
    def write_to(self, stream):
        stream.write_int(self.current_tick)
        self.properties.write_to(stream)
        self.level.write_to(stream)
        stream.write_int(len(self.players))
        for element in self.players:
            element.write_to(stream)
        stream.write_int(len(self.units))
        for element in self.units:
            element.write_to(stream)
        stream.write_int(len(self.bullets))
        for element in self.bullets:
            element.write_to(stream)
        stream.write_int(len(self.mines))
        for element in self.mines:
            element.write_to(stream)
        stream.write_int(len(self.loot_boxes))
        for element in self.loot_boxes:
            element.write_to(stream)
    def __repr__(self):
        return "Game(" + \
            repr(self.current_tick) + "," + \
            repr(self.properties) + "," + \
            repr(self.level) + "," + \
            repr(self.players) + "," + \
            repr(self.units) + "," + \
            repr(self.bullets) + "," + \
            repr(self.mines) + "," + \
            repr(self.loot_boxes) + \
            ")"
