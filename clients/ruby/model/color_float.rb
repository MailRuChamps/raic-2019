class ColorFloat
    attr_accessor :r
    attr_accessor :g
    attr_accessor :b
    attr_accessor :a
    def initialize(r, g, b, a)
        @r = r
        @g = g
        @b = b
        @a = a
    end
    def self.read_from(stream)
        r = stream.read_float()
        g = stream.read_float()
        b = stream.read_float()
        a = stream.read_float()
        ColorFloat.new(r, g, b, a)
    end
    def write_to(stream)
        stream.write_float(@r)
        stream.write_float(@g)
        stream.write_float(@b)
        stream.write_float(@a)
    end
end
