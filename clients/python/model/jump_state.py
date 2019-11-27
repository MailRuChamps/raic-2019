class JumpState:
    def __init__(self, can_jump, speed, max_time, can_cancel):
        self.can_jump = can_jump
        self.speed = speed
        self.max_time = max_time
        self.can_cancel = can_cancel
    @staticmethod
    def read_from(stream):
        can_jump = stream.read_bool()
        speed = stream.read_double()
        max_time = stream.read_double()
        can_cancel = stream.read_bool()
        return JumpState(can_jump, speed, max_time, can_cancel)
    def write_to(self, stream):
        stream.write_bool(self.can_jump)
        stream.write_double(self.speed)
        stream.write_double(self.max_time)
        stream.write_bool(self.can_cancel)
    def __repr__(self):
        return "JumpState(" + \
            repr(self.can_jump) + "," + \
            repr(self.speed) + "," + \
            repr(self.max_time) + "," + \
            repr(self.can_cancel) + \
            ")"
