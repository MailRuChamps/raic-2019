require_relative 'weapon_type'
require_relative 'vec2_double'
require_relative 'vec2_double'
require_relative 'explosion_params'
class Bullet
    attr_accessor :weapon_type
    attr_accessor :unit_id
    attr_accessor :player_id
    attr_accessor :position
    attr_accessor :velocity
    attr_accessor :damage
    attr_accessor :size
    attr_accessor :explosion_params
    def initialize(weapon_type, unit_id, player_id, position, velocity, damage, size, explosion_params)
        @weapon_type = weapon_type
        @unit_id = unit_id
        @player_id = player_id
        @position = position
        @velocity = velocity
        @damage = damage
        @size = size
        @explosion_params = explosion_params
    end
    def self.read_from(stream)
        weapon_type = stream.read_int()
        if weapon_type < 0 || weapon_type > 3
            raise "Unexpected discriminant value"
        end
        unit_id = stream.read_int()
        player_id = stream.read_int()
        position = Vec2Double.read_from(stream)
        velocity = Vec2Double.read_from(stream)
        damage = stream.read_int()
        size = stream.read_double()
        if stream.read_bool()
            explosion_params = ExplosionParams.read_from(stream)
        else
            explosion_params = nil
        end
        Bullet.new(weapon_type, unit_id, player_id, position, velocity, damage, size, explosion_params)
    end
    def write_to(stream)
        stream.write_int(@weapon_type)
        stream.write_int(@unit_id)
        stream.write_int(@player_id)
        @position.write_to(stream)
        @velocity.write_to(stream)
        stream.write_int(@damage)
        stream.write_double(@size)
        if @explosion_params.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @explosion_params.write_to(stream)
        end
    end
end
