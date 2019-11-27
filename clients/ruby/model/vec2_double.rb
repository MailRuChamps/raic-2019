class Vec2Double
    attr_accessor :x
    attr_accessor :y
    def initialize(x, y)
        @x = x
        @y = y
    end
    def self.read_from(stream)
        x = stream.read_double()
        y = stream.read_double()
        Vec2Double.new(x, y)
    end
    def write_to(stream)
        stream.write_double(@x)
        stream.write_double(@y)
    end
end
