require_relative 'vec2_float'
require_relative 'vec2_float'
require_relative 'color_float'
require_relative 'vec2_float'
require_relative 'vec2_float'
require_relative 'color_float'
require_relative 'colored_vertex'
require_relative 'vec2_float'
require_relative 'text_alignment'
require_relative 'color_float'
class CustomData
    def self.read_from(stream)
        discriminant = stream.read_int()
        if discriminant == CustomData::Log::TAG
            return CustomData::Log.read_from(stream)
        end
        if discriminant == CustomData::Rect::TAG
            return CustomData::Rect.read_from(stream)
        end
        if discriminant == CustomData::Line::TAG
            return CustomData::Line.read_from(stream)
        end
        if discriminant == CustomData::Polygon::TAG
            return CustomData::Polygon.read_from(stream)
        end
        if discriminant == CustomData::PlacedText::TAG
            return CustomData::PlacedText.read_from(stream)
        end
        raise "Unexpected discriminant value"
    end

    class Log
        TAG = 0
        attr_accessor :text
        def initialize(text)
            @text = text
        end
        def self.read_from(stream)
            text = stream.read_string()
            Log.new(text)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_string(@text)
        end
    end
    class Rect
        TAG = 1
        attr_accessor :pos
        attr_accessor :size
        attr_accessor :color
        def initialize(pos, size, color)
            @pos = pos
            @size = size
            @color = color
        end
        def self.read_from(stream)
            pos = Vec2Float.read_from(stream)
            size = Vec2Float.read_from(stream)
            color = ColorFloat.read_from(stream)
            Rect.new(pos, size, color)
        end
        def write_to(stream)
            stream.write_int(TAG)
            @pos.write_to(stream)
            @size.write_to(stream)
            @color.write_to(stream)
        end
    end
    class Line
        TAG = 2
        attr_accessor :p1
        attr_accessor :p2
        attr_accessor :width
        attr_accessor :color
        def initialize(p1, p2, width, color)
            @p1 = p1
            @p2 = p2
            @width = width
            @color = color
        end
        def self.read_from(stream)
            p1 = Vec2Float.read_from(stream)
            p2 = Vec2Float.read_from(stream)
            width = stream.read_float()
            color = ColorFloat.read_from(stream)
            Line.new(p1, p2, width, color)
        end
        def write_to(stream)
            stream.write_int(TAG)
            @p1.write_to(stream)
            @p2.write_to(stream)
            stream.write_float(@width)
            @color.write_to(stream)
        end
    end
    class Polygon
        TAG = 3
        attr_accessor :vertices
        def initialize(vertices)
            @vertices = vertices
        end
        def self.read_from(stream)
            vertices = []
            stream.read_int().times do |_|
                vertices_element = ColoredVertex.read_from(stream)
                vertices.push(vertices_element)
            end
            Polygon.new(vertices)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@vertices.length())
            @vertices.each do |element|
                element.write_to(stream)
            end
        end
    end
    class PlacedText
        TAG = 4
        attr_accessor :text
        attr_accessor :pos
        attr_accessor :alignment
        attr_accessor :size
        attr_accessor :color
        def initialize(text, pos, alignment, size, color)
            @text = text
            @pos = pos
            @alignment = alignment
            @size = size
            @color = color
        end
        def self.read_from(stream)
            text = stream.read_string()
            pos = Vec2Float.read_from(stream)
            alignment = stream.read_int()
            if alignment < 0 || alignment > 3
                raise "Unexpected discriminant value"
            end
            size = stream.read_float()
            color = ColorFloat.read_from(stream)
            PlacedText.new(text, pos, alignment, size, color)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_string(@text)
            @pos.write_to(stream)
            stream.write_int(@alignment)
            stream.write_float(@size)
            @color.write_to(stream)
        end
    end
end
