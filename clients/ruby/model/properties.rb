require_relative 'vec2_double'
require_relative 'vec2_double'
require_relative 'weapon_type'
require_relative 'weapon_params'
require_relative 'vec2_double'
require_relative 'explosion_params'
class Properties
    attr_accessor :max_tick_count
    attr_accessor :team_size
    attr_accessor :ticks_per_second
    attr_accessor :updates_per_tick
    attr_accessor :loot_box_size
    attr_accessor :unit_size
    attr_accessor :unit_max_horizontal_speed
    attr_accessor :unit_fall_speed
    attr_accessor :unit_jump_time
    attr_accessor :unit_jump_speed
    attr_accessor :jump_pad_jump_time
    attr_accessor :jump_pad_jump_speed
    attr_accessor :unit_max_health
    attr_accessor :health_pack_health
    attr_accessor :weapon_params
    attr_accessor :mine_size
    attr_accessor :mine_explosion_params
    attr_accessor :mine_prepare_time
    attr_accessor :mine_trigger_time
    attr_accessor :mine_trigger_radius
    attr_accessor :kill_score
    def initialize(max_tick_count, team_size, ticks_per_second, updates_per_tick, loot_box_size, unit_size, unit_max_horizontal_speed, unit_fall_speed, unit_jump_time, unit_jump_speed, jump_pad_jump_time, jump_pad_jump_speed, unit_max_health, health_pack_health, weapon_params, mine_size, mine_explosion_params, mine_prepare_time, mine_trigger_time, mine_trigger_radius, kill_score)
        @max_tick_count = max_tick_count
        @team_size = team_size
        @ticks_per_second = ticks_per_second
        @updates_per_tick = updates_per_tick
        @loot_box_size = loot_box_size
        @unit_size = unit_size
        @unit_max_horizontal_speed = unit_max_horizontal_speed
        @unit_fall_speed = unit_fall_speed
        @unit_jump_time = unit_jump_time
        @unit_jump_speed = unit_jump_speed
        @jump_pad_jump_time = jump_pad_jump_time
        @jump_pad_jump_speed = jump_pad_jump_speed
        @unit_max_health = unit_max_health
        @health_pack_health = health_pack_health
        @weapon_params = weapon_params
        @mine_size = mine_size
        @mine_explosion_params = mine_explosion_params
        @mine_prepare_time = mine_prepare_time
        @mine_trigger_time = mine_trigger_time
        @mine_trigger_radius = mine_trigger_radius
        @kill_score = kill_score
    end
    def self.read_from(stream)
        max_tick_count = stream.read_int()
        team_size = stream.read_int()
        ticks_per_second = stream.read_double()
        updates_per_tick = stream.read_int()
        loot_box_size = Vec2Double.read_from(stream)
        unit_size = Vec2Double.read_from(stream)
        unit_max_horizontal_speed = stream.read_double()
        unit_fall_speed = stream.read_double()
        unit_jump_time = stream.read_double()
        unit_jump_speed = stream.read_double()
        jump_pad_jump_time = stream.read_double()
        jump_pad_jump_speed = stream.read_double()
        unit_max_health = stream.read_int()
        health_pack_health = stream.read_int()
        weapon_params = Hash.new
        stream.read_int().times do |_|
            weapon_params_key = stream.read_int()
            if weapon_params_key < 0 || weapon_params_key > 3
                raise "Unexpected discriminant value"
            end
            weapon_params_value = WeaponParams.read_from(stream)
            weapon_params[weapon_params_key] = weapon_params_value
        end
        mine_size = Vec2Double.read_from(stream)
        mine_explosion_params = ExplosionParams.read_from(stream)
        mine_prepare_time = stream.read_double()
        mine_trigger_time = stream.read_double()
        mine_trigger_radius = stream.read_double()
        kill_score = stream.read_int()
        Properties.new(max_tick_count, team_size, ticks_per_second, updates_per_tick, loot_box_size, unit_size, unit_max_horizontal_speed, unit_fall_speed, unit_jump_time, unit_jump_speed, jump_pad_jump_time, jump_pad_jump_speed, unit_max_health, health_pack_health, weapon_params, mine_size, mine_explosion_params, mine_prepare_time, mine_trigger_time, mine_trigger_radius, kill_score)
    end
    def write_to(stream)
        stream.write_int(@max_tick_count)
        stream.write_int(@team_size)
        stream.write_double(@ticks_per_second)
        stream.write_int(@updates_per_tick)
        @loot_box_size.write_to(stream)
        @unit_size.write_to(stream)
        stream.write_double(@unit_max_horizontal_speed)
        stream.write_double(@unit_fall_speed)
        stream.write_double(@unit_jump_time)
        stream.write_double(@unit_jump_speed)
        stream.write_double(@jump_pad_jump_time)
        stream.write_double(@jump_pad_jump_speed)
        stream.write_int(@unit_max_health)
        stream.write_int(@health_pack_health)
        stream.write_int(@weapon_params.length())
        @weapon_params.each do |key, value|
            stream.write_int(key)
            value.write_to(stream)
        end
        @mine_size.write_to(stream)
        @mine_explosion_params.write_to(stream)
        stream.write_double(@mine_prepare_time)
        stream.write_double(@mine_trigger_time)
        stream.write_double(@mine_trigger_radius)
        stream.write_int(@kill_score)
    end
end
