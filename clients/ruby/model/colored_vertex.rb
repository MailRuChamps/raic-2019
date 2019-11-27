require_relative 'vec2_float'
require_relative 'color_float'
class ColoredVertex
    attr_accessor :position
    attr_accessor :color
    def initialize(position, color)
        @position = position
        @color = color
    end
    def self.read_from(stream)
        position = Vec2Float.read_from(stream)
        color = ColorFloat.read_from(stream)
        ColoredVertex.new(position, color)
    end
    def write_to(stream)
        @position.write_to(stream)
        @color.write_to(stream)
    end
end
