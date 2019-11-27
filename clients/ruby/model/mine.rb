require_relative 'vec2_double'
require_relative 'vec2_double'
require_relative 'mine_state'
require_relative 'explosion_params'
class Mine
    attr_accessor :player_id
    attr_accessor :position
    attr_accessor :size
    attr_accessor :state
    attr_accessor :timer
    attr_accessor :trigger_radius
    attr_accessor :explosion_params
    def initialize(player_id, position, size, state, timer, trigger_radius, explosion_params)
        @player_id = player_id
        @position = position
        @size = size
        @state = state
        @timer = timer
        @trigger_radius = trigger_radius
        @explosion_params = explosion_params
    end
    def self.read_from(stream)
        player_id = stream.read_int()
        position = Vec2Double.read_from(stream)
        size = Vec2Double.read_from(stream)
        state = stream.read_int()
        if state < 0 || state > 4
            raise "Unexpected discriminant value"
        end
        if stream.read_bool()
            timer = stream.read_double()
        else
            timer = nil
        end
        trigger_radius = stream.read_double()
        explosion_params = ExplosionParams.read_from(stream)
        Mine.new(player_id, position, size, state, timer, trigger_radius, explosion_params)
    end
    def write_to(stream)
        stream.write_int(@player_id)
        @position.write_to(stream)
        @size.write_to(stream)
        stream.write_int(@state)
        if @timer.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            stream.write_double(@timer)
        end
        stream.write_double(@trigger_radius)
        @explosion_params.write_to(stream)
    end
end
