class JumpState
    attr_accessor :can_jump
    attr_accessor :speed
    attr_accessor :max_time
    attr_accessor :can_cancel
    def initialize(can_jump, speed, max_time, can_cancel)
        @can_jump = can_jump
        @speed = speed
        @max_time = max_time
        @can_cancel = can_cancel
    end
    def self.read_from(stream)
        can_jump = stream.read_bool()
        speed = stream.read_double()
        max_time = stream.read_double()
        can_cancel = stream.read_bool()
        JumpState.new(can_jump, speed, max_time, can_cancel)
    end
    def write_to(stream)
        stream.write_bool(@can_jump)
        stream.write_double(@speed)
        stream.write_double(@max_time)
        stream.write_bool(@can_cancel)
    end
end
