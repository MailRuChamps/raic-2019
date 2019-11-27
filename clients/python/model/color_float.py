class ColorFloat:
    def __init__(self, r, g, b, a):
        self.r = r
        self.g = g
        self.b = b
        self.a = a
    @staticmethod
    def read_from(stream):
        r = stream.read_float()
        g = stream.read_float()
        b = stream.read_float()
        a = stream.read_float()
        return ColorFloat(r, g, b, a)
    def write_to(self, stream):
        stream.write_float(self.r)
        stream.write_float(self.g)
        stream.write_float(self.b)
        stream.write_float(self.a)
    def __repr__(self):
        return "ColorFloat(" + \
            repr(self.r) + "," + \
            repr(self.g) + "," + \
            repr(self.b) + "," + \
            repr(self.a) + \
            ")"
