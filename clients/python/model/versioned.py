from .unit_action import UnitAction
class Versioned:
    def __init__(self, inner):
        self.inner = inner
    @staticmethod
    def read_from(stream):
        inner = {}
        for _ in range(stream.read_int()):
            inner_key = stream.read_int()
            inner_value = UnitAction.read_from(stream)
            inner[inner_key] = inner_value
        return Versioned(inner)
    def write_to(self, stream):
        stream.write_int(43981)
        stream.write_int(len(self.inner))
        for key, value in self.inner.items():
            stream.write_int(key)
            value.write_to(stream)
    def __repr__(self):
        return "Versioned(" + \
            repr(self.inner) + \
            ")"
