require_relative 'vec2_double'
require_relative 'vec2_double'
require_relative 'jump_state'
require_relative 'weapon'
class Unit
    attr_accessor :player_id
    attr_accessor :id
    attr_accessor :health
    attr_accessor :position
    attr_accessor :size
    attr_accessor :jump_state
    attr_accessor :walked_right
    attr_accessor :stand
    attr_accessor :on_ground
    attr_accessor :on_ladder
    attr_accessor :mines
    attr_accessor :weapon
    def initialize(player_id, id, health, position, size, jump_state, walked_right, stand, on_ground, on_ladder, mines, weapon)
        @player_id = player_id
        @id = id
        @health = health
        @position = position
        @size = size
        @jump_state = jump_state
        @walked_right = walked_right
        @stand = stand
        @on_ground = on_ground
        @on_ladder = on_ladder
        @mines = mines
        @weapon = weapon
    end
    def self.read_from(stream)
        player_id = stream.read_int()
        id = stream.read_int()
        health = stream.read_int()
        position = Vec2Double.read_from(stream)
        size = Vec2Double.read_from(stream)
        jump_state = JumpState.read_from(stream)
        walked_right = stream.read_bool()
        stand = stream.read_bool()
        on_ground = stream.read_bool()
        on_ladder = stream.read_bool()
        mines = stream.read_int()
        if stream.read_bool()
            weapon = Weapon.read_from(stream)
        else
            weapon = nil
        end
        Unit.new(player_id, id, health, position, size, jump_state, walked_right, stand, on_ground, on_ladder, mines, weapon)
    end
    def write_to(stream)
        stream.write_int(@player_id)
        stream.write_int(@id)
        stream.write_int(@health)
        @position.write_to(stream)
        @size.write_to(stream)
        @jump_state.write_to(stream)
        stream.write_bool(@walked_right)
        stream.write_bool(@stand)
        stream.write_bool(@on_ground)
        stream.write_bool(@on_ladder)
        stream.write_int(@mines)
        if @weapon.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @weapon.write_to(stream)
        end
    end
end
