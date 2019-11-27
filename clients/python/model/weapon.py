from .weapon_type import WeaponType
from .weapon_params import WeaponParams
class Weapon:
    def __init__(self, typ, params, magazine, was_shooting, spread, fire_timer, last_angle, last_fire_tick):
        self.typ = typ
        self.params = params
        self.magazine = magazine
        self.was_shooting = was_shooting
        self.spread = spread
        self.fire_timer = fire_timer
        self.last_angle = last_angle
        self.last_fire_tick = last_fire_tick
    @staticmethod
    def read_from(stream):
        typ = WeaponType(stream.read_int())
        params = WeaponParams.read_from(stream)
        magazine = stream.read_int()
        was_shooting = stream.read_bool()
        spread = stream.read_double()
        if stream.read_bool():
            fire_timer = stream.read_double()
        else:
            fire_timer = None
        if stream.read_bool():
            last_angle = stream.read_double()
        else:
            last_angle = None
        if stream.read_bool():
            last_fire_tick = stream.read_int()
        else:
            last_fire_tick = None
        return Weapon(typ, params, magazine, was_shooting, spread, fire_timer, last_angle, last_fire_tick)
    def write_to(self, stream):
        stream.write_int(self.typ)
        self.params.write_to(stream)
        stream.write_int(self.magazine)
        stream.write_bool(self.was_shooting)
        stream.write_double(self.spread)
        if self.fire_timer is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_double(self.fire_timer)
        if self.last_angle is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_double(self.last_angle)
        if self.last_fire_tick is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_int(self.last_fire_tick)
    def __repr__(self):
        return "Weapon(" + \
            repr(self.typ) + "," + \
            repr(self.params) + "," + \
            repr(self.magazine) + "," + \
            repr(self.was_shooting) + "," + \
            repr(self.spread) + "," + \
            repr(self.fire_timer) + "," + \
            repr(self.last_angle) + "," + \
            repr(self.last_fire_tick) + \
            ")"
