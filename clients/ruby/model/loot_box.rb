require_relative 'vec2_double'
require_relative 'vec2_double'
require_relative 'item'
class LootBox
    attr_accessor :position
    attr_accessor :size
    attr_accessor :item
    def initialize(position, size, item)
        @position = position
        @size = size
        @item = item
    end
    def self.read_from(stream)
        position = Vec2Double.read_from(stream)
        size = Vec2Double.read_from(stream)
        item = Item.read_from(stream)
        LootBox.new(position, size, item)
    end
    def write_to(stream)
        @position.write_to(stream)
        @size.write_to(stream)
        @item.write_to(stream)
    end
end
