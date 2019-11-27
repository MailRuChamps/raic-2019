class Vec2Double:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    @staticmethod
    def read_from(stream):
        x = stream.read_double()
        y = stream.read_double()
        return Vec2Double(x, y)
    def write_to(self, stream):
        stream.write_double(self.x)
        stream.write_double(self.y)
    def __repr__(self):
        return "Vec2Double(" + \
            repr(self.x) + "," + \
            repr(self.y) + \
            ")"
