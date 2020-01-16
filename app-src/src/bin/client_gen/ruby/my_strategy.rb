require_relative 'model'


class MyStrategy
    def get_action(unit, game, debug)
        def distance_sqr(a, b)
            (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)
        end
        nearest_enemy = nil
        game.units.each do |other|
            if other.player_id != unit.player_id
                if nearest_enemy == nil || distance_sqr(unit.position, other.position) < distance_sqr(unit.position, nearest_enemy.position)
                    nearest_enemy = other
                end
            end
        end
        nearest_weapon = nil
        game.loot_boxes.each do |loot_box|
            if loot_box.item.instance_of? Item::Weapon
                if nearest_weapon == nil || distance_sqr(unit.position, loot_box.position) < distance_sqr(unit.position, nearest_weapon.position)
                    nearest_weapon = loot_box
                end
            end
        end
        target_pos = unit.position
        if unit.weapon == nil && nearest_weapon != nil
            target_pos = nearest_weapon.position
        elsif nearest_enemy != nil
            target_pos = nearest_enemy.position
        end
        debug.draw(CustomData::Log.new("Target pos: #{target_pos}"))
        aim = Vec2Double.new(0, 0)
        if nearest_enemy != nil
            aim = Vec2Double.new(
                nearest_enemy.position.x - unit.position.x,
                nearest_enemy.position.y - unit.position.y)
        end
        velocity = target_pos.x - unit.position.x
        jump = target_pos.y > unit.position.y
        if target_pos.x > unit.position.x and game.level.tiles[(unit.position.x + 1).to_i][(unit.position.y).to_i] == Tile::WALL
            jump = true
        end
        if target_pos.x < unit.position.x and game.level.tiles[(unit.position.x - 1).to_i][(unit.position.y).to_i] == Tile::WALL
            jump = true
        end
        jump_down = !jump
        aim = aim
        shoot = true
        reload = false
        swap_weapon = false
        plant_mine = false
        UnitAction.new(velocity, jump, jump_down, aim, shoot, reload, swap_weapon, plant_mine)
    end
end