class ExplosionParams
    attr_accessor :radius
    attr_accessor :damage
    def initialize(radius, damage)
        @radius = radius
        @damage = damage
    end
    def self.read_from(stream)
        radius = stream.read_double()
        damage = stream.read_int()
        ExplosionParams.new(radius, damage)
    end
    def write_to(stream)
        stream.write_double(@radius)
        stream.write_int(@damage)
    end
end
