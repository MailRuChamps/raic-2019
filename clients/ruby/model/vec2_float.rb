class Vec2Float
    attr_accessor :x
    attr_accessor :y
    def initialize(x, y)
        @x = x
        @y = y
    end
    def self.read_from(stream)
        x = stream.read_float()
        y = stream.read_float()
        Vec2Float.new(x, y)
    end
    def write_to(stream)
        stream.write_float(@x)
        stream.write_float(@y)
    end
end
