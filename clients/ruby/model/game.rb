require_relative 'properties'
require_relative 'level'
require_relative 'player'
require_relative 'unit'
require_relative 'bullet'
require_relative 'mine'
require_relative 'loot_box'
class Game
    attr_accessor :current_tick
    attr_accessor :properties
    attr_accessor :level
    attr_accessor :players
    attr_accessor :units
    attr_accessor :bullets
    attr_accessor :mines
    attr_accessor :loot_boxes
    def initialize(current_tick, properties, level, players, units, bullets, mines, loot_boxes)
        @current_tick = current_tick
        @properties = properties
        @level = level
        @players = players
        @units = units
        @bullets = bullets
        @mines = mines
        @loot_boxes = loot_boxes
    end
    def self.read_from(stream)
        current_tick = stream.read_int()
        properties = Properties.read_from(stream)
        level = Level.read_from(stream)
        players = []
        stream.read_int().times do |_|
            players_element = Player.read_from(stream)
            players.push(players_element)
        end
        units = []
        stream.read_int().times do |_|
            units_element = Unit.read_from(stream)
            units.push(units_element)
        end
        bullets = []
        stream.read_int().times do |_|
            bullets_element = Bullet.read_from(stream)
            bullets.push(bullets_element)
        end
        mines = []
        stream.read_int().times do |_|
            mines_element = Mine.read_from(stream)
            mines.push(mines_element)
        end
        loot_boxes = []
        stream.read_int().times do |_|
            loot_boxes_element = LootBox.read_from(stream)
            loot_boxes.push(loot_boxes_element)
        end
        Game.new(current_tick, properties, level, players, units, bullets, mines, loot_boxes)
    end
    def write_to(stream)
        stream.write_int(@current_tick)
        @properties.write_to(stream)
        @level.write_to(stream)
        stream.write_int(@players.length())
        @players.each do |element|
            element.write_to(stream)
        end
        stream.write_int(@units.length())
        @units.each do |element|
            element.write_to(stream)
        end
        stream.write_int(@bullets.length())
        @bullets.each do |element|
            element.write_to(stream)
        end
        stream.write_int(@mines.length())
        @mines.each do |element|
            element.write_to(stream)
        end
        stream.write_int(@loot_boxes.length())
        @loot_boxes.each do |element|
            element.write_to(stream)
        end
    end
end
