from .vec2_double import Vec2Double
class UnitAction:
    def __init__(self, velocity, jump, jump_down, aim, shoot, reload, swap_weapon, plant_mine):
        self.velocity = velocity
        self.jump = jump
        self.jump_down = jump_down
        self.aim = aim
        self.shoot = shoot
        self.reload = reload
        self.swap_weapon = swap_weapon
        self.plant_mine = plant_mine
    @staticmethod
    def read_from(stream):
        velocity = stream.read_double()
        jump = stream.read_bool()
        jump_down = stream.read_bool()
        aim = Vec2Double.read_from(stream)
        shoot = stream.read_bool()
        reload = stream.read_bool()
        swap_weapon = stream.read_bool()
        plant_mine = stream.read_bool()
        return UnitAction(velocity, jump, jump_down, aim, shoot, reload, swap_weapon, plant_mine)
    def write_to(self, stream):
        stream.write_double(self.velocity)
        stream.write_bool(self.jump)
        stream.write_bool(self.jump_down)
        self.aim.write_to(stream)
        stream.write_bool(self.shoot)
        stream.write_bool(self.reload)
        stream.write_bool(self.swap_weapon)
        stream.write_bool(self.plant_mine)
    def __repr__(self):
        return "UnitAction(" + \
            repr(self.velocity) + "," + \
            repr(self.jump) + "," + \
            repr(self.jump_down) + "," + \
            repr(self.aim) + "," + \
            repr(self.shoot) + "," + \
            repr(self.reload) + "," + \
            repr(self.swap_weapon) + "," + \
            repr(self.plant_mine) + \
            ")"
