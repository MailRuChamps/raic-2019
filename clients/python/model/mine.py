from .vec2_double import Vec2Double
from .vec2_double import Vec2Double
from .mine_state import MineState
from .explosion_params import ExplosionParams
class Mine:
    def __init__(self, player_id, position, size, state, timer, trigger_radius, explosion_params):
        self.player_id = player_id
        self.position = position
        self.size = size
        self.state = state
        self.timer = timer
        self.trigger_radius = trigger_radius
        self.explosion_params = explosion_params
    @staticmethod
    def read_from(stream):
        player_id = stream.read_int()
        position = Vec2Double.read_from(stream)
        size = Vec2Double.read_from(stream)
        state = MineState(stream.read_int())
        if stream.read_bool():
            timer = stream.read_double()
        else:
            timer = None
        trigger_radius = stream.read_double()
        explosion_params = ExplosionParams.read_from(stream)
        return Mine(player_id, position, size, state, timer, trigger_radius, explosion_params)
    def write_to(self, stream):
        stream.write_int(self.player_id)
        self.position.write_to(stream)
        self.size.write_to(stream)
        stream.write_int(self.state)
        if self.timer is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_double(self.timer)
        stream.write_double(self.trigger_radius)
        self.explosion_params.write_to(stream)
    def __repr__(self):
        return "Mine(" + \
            repr(self.player_id) + "," + \
            repr(self.position) + "," + \
            repr(self.size) + "," + \
            repr(self.state) + "," + \
            repr(self.timer) + "," + \
            repr(self.trigger_radius) + "," + \
            repr(self.explosion_params) + \
            ")"
