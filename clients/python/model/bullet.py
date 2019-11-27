from .weapon_type import WeaponType
from .vec2_double import Vec2Double
from .vec2_double import Vec2Double
from .explosion_params import ExplosionParams
class Bullet:
    def __init__(self, weapon_type, unit_id, player_id, position, velocity, damage, size, explosion_params):
        self.weapon_type = weapon_type
        self.unit_id = unit_id
        self.player_id = player_id
        self.position = position
        self.velocity = velocity
        self.damage = damage
        self.size = size
        self.explosion_params = explosion_params
    @staticmethod
    def read_from(stream):
        weapon_type = WeaponType(stream.read_int())
        unit_id = stream.read_int()
        player_id = stream.read_int()
        position = Vec2Double.read_from(stream)
        velocity = Vec2Double.read_from(stream)
        damage = stream.read_int()
        size = stream.read_double()
        if stream.read_bool():
            explosion_params = ExplosionParams.read_from(stream)
        else:
            explosion_params = None
        return Bullet(weapon_type, unit_id, player_id, position, velocity, damage, size, explosion_params)
    def write_to(self, stream):
        stream.write_int(self.weapon_type)
        stream.write_int(self.unit_id)
        stream.write_int(self.player_id)
        self.position.write_to(stream)
        self.velocity.write_to(stream)
        stream.write_int(self.damage)
        stream.write_double(self.size)
        if self.explosion_params is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.explosion_params.write_to(stream)
    def __repr__(self):
        return "Bullet(" + \
            repr(self.weapon_type) + "," + \
            repr(self.unit_id) + "," + \
            repr(self.player_id) + "," + \
            repr(self.position) + "," + \
            repr(self.velocity) + "," + \
            repr(self.damage) + "," + \
            repr(self.size) + "," + \
            repr(self.explosion_params) + \
            ")"
