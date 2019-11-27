class BulletParams
    attr_accessor :speed
    attr_accessor :size
    attr_accessor :damage
    def initialize(speed, size, damage)
        @speed = speed
        @size = size
        @damage = damage
    end
    def self.read_from(stream)
        speed = stream.read_double()
        size = stream.read_double()
        damage = stream.read_int()
        BulletParams.new(speed, size, damage)
    end
    def write_to(stream)
        stream.write_double(@speed)
        stream.write_double(@size)
        stream.write_int(@damage)
    end
end
