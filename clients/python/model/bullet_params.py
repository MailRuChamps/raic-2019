class BulletParams:
    def __init__(self, speed, size, damage):
        self.speed = speed
        self.size = size
        self.damage = damage
    @staticmethod
    def read_from(stream):
        speed = stream.read_double()
        size = stream.read_double()
        damage = stream.read_int()
        return BulletParams(speed, size, damage)
    def write_to(self, stream):
        stream.write_double(self.speed)
        stream.write_double(self.size)
        stream.write_int(self.damage)
    def __repr__(self):
        return "BulletParams(" + \
            repr(self.speed) + "," + \
            repr(self.size) + "," + \
            repr(self.damage) + \
            ")"
