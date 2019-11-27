class ExplosionParams:
    def __init__(self, radius, damage):
        self.radius = radius
        self.damage = damage
    @staticmethod
    def read_from(stream):
        radius = stream.read_double()
        damage = stream.read_int()
        return ExplosionParams(radius, damage)
    def write_to(self, stream):
        stream.write_double(self.radius)
        stream.write_int(self.damage)
    def __repr__(self):
        return "ExplosionParams(" + \
            repr(self.radius) + "," + \
            repr(self.damage) + \
            ")"
