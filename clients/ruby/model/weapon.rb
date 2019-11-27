require_relative 'weapon_type'
require_relative 'weapon_params'
class Weapon
    attr_accessor :typ
    attr_accessor :params
    attr_accessor :magazine
    attr_accessor :was_shooting
    attr_accessor :spread
    attr_accessor :fire_timer
    attr_accessor :last_angle
    attr_accessor :last_fire_tick
    def initialize(typ, params, magazine, was_shooting, spread, fire_timer, last_angle, last_fire_tick)
        @typ = typ
        @params = params
        @magazine = magazine
        @was_shooting = was_shooting
        @spread = spread
        @fire_timer = fire_timer
        @last_angle = last_angle
        @last_fire_tick = last_fire_tick
    end
    def self.read_from(stream)
        typ = stream.read_int()
        if typ < 0 || typ > 3
            raise "Unexpected discriminant value"
        end
        params = WeaponParams.read_from(stream)
        magazine = stream.read_int()
        was_shooting = stream.read_bool()
        spread = stream.read_double()
        if stream.read_bool()
            fire_timer = stream.read_double()
        else
            fire_timer = nil
        end
        if stream.read_bool()
            last_angle = stream.read_double()
        else
            last_angle = nil
        end
        if stream.read_bool()
            last_fire_tick = stream.read_int()
        else
            last_fire_tick = nil
        end
        Weapon.new(typ, params, magazine, was_shooting, spread, fire_timer, last_angle, last_fire_tick)
    end
    def write_to(stream)
        stream.write_int(@typ)
        @params.write_to(stream)
        stream.write_int(@magazine)
        stream.write_bool(@was_shooting)
        stream.write_double(@spread)
        if @fire_timer.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            stream.write_double(@fire_timer)
        end
        if @last_angle.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            stream.write_double(@last_angle)
        end
        if @last_fire_tick.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            stream.write_int(@last_fire_tick)
        end
    end
end
