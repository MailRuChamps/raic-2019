class Vec2Float:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    @staticmethod
    def read_from(stream):
        x = stream.read_float()
        y = stream.read_float()
        return Vec2Float(x, y)
    def write_to(self, stream):
        stream.write_float(self.x)
        stream.write_float(self.y)
    def __repr__(self):
        return "Vec2Float(" + \
            repr(self.x) + "," + \
            repr(self.y) + \
            ")"
