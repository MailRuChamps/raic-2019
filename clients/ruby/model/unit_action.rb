require_relative 'vec2_double'
class UnitAction
    attr_accessor :velocity
    attr_accessor :jump
    attr_accessor :jump_down
    attr_accessor :aim
    attr_accessor :shoot
    attr_accessor :swap_weapon
    attr_accessor :plant_mine
    def initialize(velocity, jump, jump_down, aim, shoot, swap_weapon, plant_mine)
        @velocity = velocity
        @jump = jump
        @jump_down = jump_down
        @aim = aim
        @shoot = shoot
        @swap_weapon = swap_weapon
        @plant_mine = plant_mine
    end
    def self.read_from(stream)
        velocity = stream.read_double()
        jump = stream.read_bool()
        jump_down = stream.read_bool()
        aim = Vec2Double.read_from(stream)
        shoot = stream.read_bool()
        swap_weapon = stream.read_bool()
        plant_mine = stream.read_bool()
        UnitAction.new(velocity, jump, jump_down, aim, shoot, swap_weapon, plant_mine)
    end
    def write_to(stream)
        stream.write_double(@velocity)
        stream.write_bool(@jump)
        stream.write_bool(@jump_down)
        @aim.write_to(stream)
        stream.write_bool(@shoot)
        stream.write_bool(@swap_weapon)
        stream.write_bool(@plant_mine)
    end
end
