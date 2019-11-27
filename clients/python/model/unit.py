from .vec2_double import Vec2Double
from .vec2_double import Vec2Double
from .jump_state import JumpState
from .weapon import Weapon
class Unit:
    def __init__(self, player_id, id, health, position, size, jump_state, walked_right, stand, on_ground, on_ladder, mines, weapon):
        self.player_id = player_id
        self.id = id
        self.health = health
        self.position = position
        self.size = size
        self.jump_state = jump_state
        self.walked_right = walked_right
        self.stand = stand
        self.on_ground = on_ground
        self.on_ladder = on_ladder
        self.mines = mines
        self.weapon = weapon
    @staticmethod
    def read_from(stream):
        player_id = stream.read_int()
        id = stream.read_int()
        health = stream.read_int()
        position = Vec2Double.read_from(stream)
        size = Vec2Double.read_from(stream)
        jump_state = JumpState.read_from(stream)
        walked_right = stream.read_bool()
        stand = stream.read_bool()
        on_ground = stream.read_bool()
        on_ladder = stream.read_bool()
        mines = stream.read_int()
        if stream.read_bool():
            weapon = Weapon.read_from(stream)
        else:
            weapon = None
        return Unit(player_id, id, health, position, size, jump_state, walked_right, stand, on_ground, on_ladder, mines, weapon)
    def write_to(self, stream):
        stream.write_int(self.player_id)
        stream.write_int(self.id)
        stream.write_int(self.health)
        self.position.write_to(stream)
        self.size.write_to(stream)
        self.jump_state.write_to(stream)
        stream.write_bool(self.walked_right)
        stream.write_bool(self.stand)
        stream.write_bool(self.on_ground)
        stream.write_bool(self.on_ladder)
        stream.write_int(self.mines)
        if self.weapon is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.weapon.write_to(stream)
    def __repr__(self):
        return "Unit(" + \
            repr(self.player_id) + "," + \
            repr(self.id) + "," + \
            repr(self.health) + "," + \
            repr(self.position) + "," + \
            repr(self.size) + "," + \
            repr(self.jump_state) + "," + \
            repr(self.walked_right) + "," + \
            repr(self.stand) + "," + \
            repr(self.on_ground) + "," + \
            repr(self.on_ladder) + "," + \
            repr(self.mines) + "," + \
            repr(self.weapon) + \
            ")"
