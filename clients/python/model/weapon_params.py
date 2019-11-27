from .bullet_params import BulletParams
from .explosion_params import ExplosionParams
class WeaponParams:
    def __init__(self, magazine_size, fire_rate, reload_time, min_spread, max_spread, recoil, aim_speed, bullet, explosion):
        self.magazine_size = magazine_size
        self.fire_rate = fire_rate
        self.reload_time = reload_time
        self.min_spread = min_spread
        self.max_spread = max_spread
        self.recoil = recoil
        self.aim_speed = aim_speed
        self.bullet = bullet
        self.explosion = explosion
    @staticmethod
    def read_from(stream):
        magazine_size = stream.read_int()
        fire_rate = stream.read_double()
        reload_time = stream.read_double()
        min_spread = stream.read_double()
        max_spread = stream.read_double()
        recoil = stream.read_double()
        aim_speed = stream.read_double()
        bullet = BulletParams.read_from(stream)
        if stream.read_bool():
            explosion = ExplosionParams.read_from(stream)
        else:
            explosion = None
        return WeaponParams(magazine_size, fire_rate, reload_time, min_spread, max_spread, recoil, aim_speed, bullet, explosion)
    def write_to(self, stream):
        stream.write_int(self.magazine_size)
        stream.write_double(self.fire_rate)
        stream.write_double(self.reload_time)
        stream.write_double(self.min_spread)
        stream.write_double(self.max_spread)
        stream.write_double(self.recoil)
        stream.write_double(self.aim_speed)
        self.bullet.write_to(stream)
        if self.explosion is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.explosion.write_to(stream)
    def __repr__(self):
        return "WeaponParams(" + \
            repr(self.magazine_size) + "," + \
            repr(self.fire_rate) + "," + \
            repr(self.reload_time) + "," + \
            repr(self.min_spread) + "," + \
            repr(self.max_spread) + "," + \
            repr(self.recoil) + "," + \
            repr(self.aim_speed) + "," + \
            repr(self.bullet) + "," + \
            repr(self.explosion) + \
            ")"
