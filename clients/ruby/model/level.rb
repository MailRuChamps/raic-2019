require_relative 'tile'
class Level
    attr_accessor :tiles
    def initialize(tiles)
        @tiles = tiles
    end
    def self.read_from(stream)
        tiles = []
        stream.read_int().times do |_|
            tiles_element = []
            stream.read_int().times do |_|
                tiles_element_element = stream.read_int()
                if tiles_element_element < 0 || tiles_element_element > 5
                    raise "Unexpected discriminant value"
                end
                tiles_element.push(tiles_element_element)
            end
            tiles.push(tiles_element)
        end
        Level.new(tiles)
    end
    def write_to(stream)
        stream.write_int(@tiles.length())
        @tiles.each do |element|
            stream.write_int(element.length())
            element.each do |element|
                stream.write_int(element)
            end
        end
    end
end
