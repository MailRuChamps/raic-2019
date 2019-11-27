from .vec2_double import Vec2Double
from .vec2_double import Vec2Double
from .weapon_type import WeaponType
from .weapon_params import WeaponParams
from .vec2_double import Vec2Double
from .explosion_params import ExplosionParams
class Properties:
    def __init__(self, max_tick_count, team_size, ticks_per_second, updates_per_tick, loot_box_size, unit_size, unit_max_horizontal_speed, unit_fall_speed, unit_jump_time, unit_jump_speed, jump_pad_jump_time, jump_pad_jump_speed, unit_max_health, health_pack_health, weapon_params, mine_size, mine_explosion_params, mine_prepare_time, mine_trigger_time, mine_trigger_radius, kill_score):
        self.max_tick_count = max_tick_count
        self.team_size = team_size
        self.ticks_per_second = ticks_per_second
        self.updates_per_tick = updates_per_tick
        self.loot_box_size = loot_box_size
        self.unit_size = unit_size
        self.unit_max_horizontal_speed = unit_max_horizontal_speed
        self.unit_fall_speed = unit_fall_speed
        self.unit_jump_time = unit_jump_time
        self.unit_jump_speed = unit_jump_speed
        self.jump_pad_jump_time = jump_pad_jump_time
        self.jump_pad_jump_speed = jump_pad_jump_speed
        self.unit_max_health = unit_max_health
        self.health_pack_health = health_pack_health
        self.weapon_params = weapon_params
        self.mine_size = mine_size
        self.mine_explosion_params = mine_explosion_params
        self.mine_prepare_time = mine_prepare_time
        self.mine_trigger_time = mine_trigger_time
        self.mine_trigger_radius = mine_trigger_radius
        self.kill_score = kill_score
    @staticmethod
    def read_from(stream):
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
        weapon_params = {}
        for _ in range(stream.read_int()):
            weapon_params_key = WeaponType(stream.read_int())
            weapon_params_value = WeaponParams.read_from(stream)
            weapon_params[weapon_params_key] = weapon_params_value
        mine_size = Vec2Double.read_from(stream)
        mine_explosion_params = ExplosionParams.read_from(stream)
        mine_prepare_time = stream.read_double()
        mine_trigger_time = stream.read_double()
        mine_trigger_radius = stream.read_double()
        kill_score = stream.read_int()
        return Properties(max_tick_count, team_size, ticks_per_second, updates_per_tick, loot_box_size, unit_size, unit_max_horizontal_speed, unit_fall_speed, unit_jump_time, unit_jump_speed, jump_pad_jump_time, jump_pad_jump_speed, unit_max_health, health_pack_health, weapon_params, mine_size, mine_explosion_params, mine_prepare_time, mine_trigger_time, mine_trigger_radius, kill_score)
    def write_to(self, stream):
        stream.write_int(self.max_tick_count)
        stream.write_int(self.team_size)
        stream.write_double(self.ticks_per_second)
        stream.write_int(self.updates_per_tick)
        self.loot_box_size.write_to(stream)
        self.unit_size.write_to(stream)
        stream.write_double(self.unit_max_horizontal_speed)
        stream.write_double(self.unit_fall_speed)
        stream.write_double(self.unit_jump_time)
        stream.write_double(self.unit_jump_speed)
        stream.write_double(self.jump_pad_jump_time)
        stream.write_double(self.jump_pad_jump_speed)
        stream.write_int(self.unit_max_health)
        stream.write_int(self.health_pack_health)
        stream.write_int(len(self.weapon_params))
        for key, value in self.weapon_params.items():
            stream.write_int(key)
            value.write_to(stream)
        self.mine_size.write_to(stream)
        self.mine_explosion_params.write_to(stream)
        stream.write_double(self.mine_prepare_time)
        stream.write_double(self.mine_trigger_time)
        stream.write_double(self.mine_trigger_radius)
        stream.write_int(self.kill_score)
    def __repr__(self):
        return "Properties(" + \
            repr(self.max_tick_count) + "," + \
            repr(self.team_size) + "," + \
            repr(self.ticks_per_second) + "," + \
            repr(self.updates_per_tick) + "," + \
            repr(self.loot_box_size) + "," + \
            repr(self.unit_size) + "," + \
            repr(self.unit_max_horizontal_speed) + "," + \
            repr(self.unit_fall_speed) + "," + \
            repr(self.unit_jump_time) + "," + \
            repr(self.unit_jump_speed) + "," + \
            repr(self.jump_pad_jump_time) + "," + \
            repr(self.jump_pad_jump_speed) + "," + \
            repr(self.unit_max_health) + "," + \
            repr(self.health_pack_health) + "," + \
            repr(self.weapon_params) + "," + \
            repr(self.mine_size) + "," + \
            repr(self.mine_explosion_params) + "," + \
            repr(self.mine_prepare_time) + "," + \
            repr(self.mine_trigger_time) + "," + \
            repr(self.mine_trigger_radius) + "," + \
            repr(self.kill_score) + \
            ")"
