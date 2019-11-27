require_relative 'bullet_params'
require_relative 'explosion_params'
class WeaponParams
    attr_accessor :magazine_size
    attr_accessor :fire_rate
    attr_accessor :reload_time
    attr_accessor :min_spread
    attr_accessor :max_spread
    attr_accessor :recoil
    attr_accessor :aim_speed
    attr_accessor :bullet
    attr_accessor :explosion
    def initialize(magazine_size, fire_rate, reload_time, min_spread, max_spread, recoil, aim_speed, bullet, explosion)
        @magazine_size = magazine_size
        @fire_rate = fire_rate
        @reload_time = reload_time
        @min_spread = min_spread
        @max_spread = max_spread
        @recoil = recoil
        @aim_speed = aim_speed
        @bullet = bullet
        @explosion = explosion
    end
    def self.read_from(stream)
        magazine_size = stream.read_int()
        fire_rate = stream.read_double()
        reload_time = stream.read_double()
        min_spread = stream.read_double()
        max_spread = stream.read_double()
        recoil = stream.read_double()
        aim_speed = stream.read_double()
        bullet = BulletParams.read_from(stream)
        if stream.read_bool()
            explosion = ExplosionParams.read_from(stream)
        else
            explosion = nil
        end
        WeaponParams.new(magazine_size, fire_rate, reload_time, min_spread, max_spread, recoil, aim_speed, bullet, explosion)
    end
    def write_to(stream)
        stream.write_int(@magazine_size)
        stream.write_double(@fire_rate)
        stream.write_double(@reload_time)
        stream.write_double(@min_spread)
        stream.write_double(@max_spread)
        stream.write_double(@recoil)
        stream.write_double(@aim_speed)
        @bullet.write_to(stream)
        if @explosion.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @explosion.write_to(stream)
        end
    end
end
