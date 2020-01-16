use super::*;

fn ladder_rect(unit: &Unit) -> AABB<R64> {
    AABB::pos_size(unit.position, vec2(r64(0.0), unit.size.y / r64(2.0)))
}

struct Explosion {
    position: Vec2<R64>,
    params: ExplosionParams,
    player_id: Id,
}

impl Game {
    fn process_shooting(
        &mut self,
        rng: &mut dyn rand::RngCore,
        events: &mut Vec<Event>,
        unit_actions: &HashMap<Id, UnitAction>,
        delta_time: R64,
    ) {
        for unit in &mut self.units {
            let action = unit_actions.get(&unit.id).cloned().unwrap_or_else(default);

            let unit_center = unit.center();

            if let Some(weapon) = &mut unit.weapon {
                if action.aim.len() > r64(0.5) {
                    weapon.aim(action.aim);
                    weapon.set_shoot(action.shoot);
                    if action.reload {
                        weapon.reload();
                    }
                    if action.shoot {
                        if let Some(bullet) = weapon.fire(
                            self.current_tick,
                            unit.id,
                            unit.player_id,
                            unit_center,
                            action.aim,
                            rng,
                        ) {
                            events.push(Event::Shot {
                                weapon_type: bullet.weapon_type,
                            });
                            self.bullets.push(bullet);
                        }
                    }
                }
                weapon.update(delta_time);
            }
        }
    }
    fn process_movement(&mut self, unit_actions: &HashMap<Id, UnitAction>, delta_time: R64) {
        fn update(old: R64, direction: R64, to: R64) -> R64 {
            if direction > old {
                max(old, to)
            } else {
                min(old, to)
            }
        }
        let properties = &self.properties;
        for unit_index in 0..self.units.len() {
            let mut unit_position;
            let mut unit_jump_state;
            let mut on_ground = false;
            {
                let unit = &self.units[unit_index];
                let action = unit_actions.get(&unit.id).cloned().unwrap_or_else(default);

                unit_position = unit.position;
                unit_jump_state = unit.jump_state.clone();
                let mut new_position = unit_position;

                new_position.x +=
                    clamp_abs(action.velocity, properties.unit_max_horizontal_speed) * delta_time;

                if unit_jump_state.can_cancel && !action.jump {
                    unit_jump_state = JumpState::falling();
                }
                if unit_jump_state.can_jump {
                    new_position.y += unit_jump_state.speed * delta_time;
                    unit_jump_state.max_time -= delta_time;
                    if unit_jump_state.max_time < r64(0.0) {
                        unit_jump_state = JumpState::falling();
                    }
                } else if action.jump_down
                    || !self
                        .level
                        .find_tiles(ladder_rect(unit))
                        .any(|(_, tile)| *tile == Tile::Ladder)
                {
                    new_position.y -= properties.unit_fall_speed * delta_time;
                }

                let eps = r64(1e-9);

                // Horizontal
                for other in &self.units {
                    if other.id == unit.id {
                        continue;
                    }
                    if unit
                        .rect_at(vec2(new_position.x, unit_position.y))
                        .intersects(&other.rect())
                    {
                        new_position.x = update(
                            unit_position.x,
                            new_position.x,
                            other.position.x
                                + (unit_position.x - other.position.x).signum()
                                    * ((unit.size.x + other.size.x) / r64(2.0) + eps),
                        );
                    }
                }
                if self
                    .level
                    .find_tiles(unit.rect_at(vec2(new_position.x, unit_position.y)))
                    .any(|(_, tile)| tile.is_blocking())
                {
                    if new_position.x > unit_position.x {
                        unit_position.x = update(
                            unit_position.x,
                            new_position.x,
                            unit_position.x + (unit.rect().x_max - eps).ceil()
                                - unit.rect().x_max
                                - eps,
                        );
                    } else {
                        unit_position.x = update(
                            unit_position.x,
                            new_position.x,
                            unit_position.x + (unit.rect().x_min + eps).floor() - unit.rect().x_min
                                + eps,
                        );
                    }
                } else {
                    unit_position.x = new_position.x;
                }
                new_position.x = unit_position.x;

                // Vertical
                for other in &self.units {
                    if other.id == unit.id {
                        continue;
                    }
                    if unit.rect_at(new_position).intersects(&other.rect()) {
                        if new_position.y > unit_position.y {
                            new_position.y = update(
                                unit_position.y,
                                new_position.y,
                                other.position.y - unit.size.y - eps,
                            );
                            unit_jump_state = JumpState::falling();
                        } else {
                            new_position.y = update(
                                unit_position.y,
                                new_position.y,
                                other.position.y + other.size.y + eps,
                            );
                            unit_jump_state = JumpState {
                                can_jump: true,
                                speed: properties.unit_jump_speed,
                                max_time: properties.unit_jump_time,
                                can_cancel: true,
                            };
                        }
                    }
                }
                if self
                    .level
                    .find_tiles(unit.rect_at(new_position))
                    .any(|(pos, tile)| {
                        let falling_from = if action.jump_down {
                            None
                        } else {
                            Some(unit_position.y)
                        };
                        match tile {
                            Tile::Empty | Tile::JumpPad => false,
                            Tile::Wall => true,
                            Tile::Platform => {
                                if let Some(height) = falling_from {
                                    height > r64(0.0) && height.raw() as usize > pos.y
                                } else {
                                    false
                                }
                            }
                            Tile::Ladder => {
                                if let Some(height) = falling_from {
                                    height > r64(0.0)
                                        && height.raw() as usize > pos.y
                                        && unit_position.x > r64(0.0)
                                        && unit_position.x.raw() as usize == pos.x
                                } else {
                                    false
                                }
                            }
                        }
                    })
                {
                    if new_position.y < unit_position.y {
                        unit_jump_state = JumpState {
                            can_jump: true,
                            speed: properties.unit_jump_speed,
                            max_time: properties.unit_jump_time,
                            can_cancel: true,
                        };
                        on_ground = true;
                    } else {
                        unit_jump_state = JumpState::falling();
                    }
                    if new_position.y > unit_position.y {
                        unit_position.y = update(
                            unit_position.y,
                            new_position.y,
                            unit_position.y + (unit.rect().y_max - eps).ceil()
                                - unit.rect().y_max
                                - eps,
                        );
                    } else {
                        unit_position.y = update(
                            unit_position.y,
                            new_position.y,
                            unit_position.y + (unit.rect().y_min + eps).floor() - unit.rect().y_min
                                + eps,
                        );
                    }
                } else {
                    unit_position.y = new_position.y;
                }
                new_position.y = unit_position.y;
            }
            let unit = &mut self.units[unit_index];
            unit.on_ground = on_ground;
            unit.stand = unit_position.x == unit.position.x;
            if !unit.stand {
                unit.walked_right = unit_position.x > unit.position.x;
            }
            unit.position = unit_position;
            unit.jump_state = unit_jump_state;
        }
    }
    fn process_level_jumps(&mut self, events: &mut Vec<Event>) {
        let properties = &self.properties;
        for unit in &mut self.units {
            unit.on_ladder = false;
            let jump_pad_pos = self.level.find_tiles(unit.rect()).find_map(|(pos, tile)| {
                if *tile == Tile::JumpPad {
                    Some(pos)
                } else {
                    None
                }
            });
            if let Some(pos) = jump_pad_pos {
                unit.jump_state = JumpState {
                    can_jump: true,
                    speed: properties.jump_pad_jump_speed,
                    max_time: properties.jump_pad_jump_time,
                    can_cancel: false,
                };
                events.push(Event::LevelEvent { used_tile: pos });
            }
            if self
                .level
                .find_tiles(ladder_rect(unit))
                .any(|(_, tile)| *tile == Tile::Ladder)
            {
                unit.jump_state = JumpState {
                    can_jump: true,
                    speed: properties.unit_jump_speed,
                    max_time: unit.jump_state.max_time.max(properties.unit_jump_time),
                    can_cancel: true,
                };
                unit.on_ground = true;
                unit.on_ladder = true;
            }
        }
    }
    fn process_bullets(
        &mut self,
        events: &mut Vec<Event>,
        explosions: &mut Vec<Explosion>,
        delta_time: R64,
    ) {
        {
            let players = &mut self.players;
            let units = &mut self.units;
            let mines = &mut self.mines;
            self.bullets.retain(|bullet| {
                for unit in units.iter_mut() {
                    if unit.id == bullet.unit_id {
                        continue;
                    }
                    if bullet.rect().intersects(&unit.rect()) {
                        events.push(Event::Hit);
                        let score = bullet.damage.min(unit.health).max(0) as usize;
                        unit.health -= bullet.damage;
                        if unit.player_id != bullet.player_id {
                            players
                                .iter_mut()
                                .find(|player| player.id == bullet.player_id)
                                .unwrap()
                                .score += score;
                        }
                        if let Some(params) = &bullet.explosion_params {
                            explosions.push(Explosion {
                                player_id: bullet.player_id,
                                position: bullet.position,
                                params: params.clone(),
                            });
                        }
                        return false;
                    }
                }
                for mine in mines.iter_mut() {
                    if bullet.rect().intersects(&mine.rect()) {
                        mine.state = MineState::Exploded;
                        if let Some(params) = &bullet.explosion_params {
                            explosions.push(Explosion {
                                player_id: bullet.player_id,
                                position: bullet.position,
                                params: params.clone(),
                            });
                        }
                        return false;
                    }
                }
                true
            });
        }
        for bullet in &mut self.bullets {
            bullet.position += bullet.velocity * delta_time;
        }
        let level = &self.level;
        self.bullets.retain(|bullet| {
            if level
                .find_tiles(bullet.rect())
                .any(|(_, tile)| tile.is_blocking())
            {
                if let Some(params) = &bullet.explosion_params {
                    explosions.push(Explosion {
                        player_id: bullet.player_id,
                        position: bullet.position,
                        params: params.clone(),
                    });
                }
                false
            } else {
                true
            }
        });
    }
    fn process_loot_boxes(
        &mut self,
        events: &mut Vec<Event>,
        unit_actions: &HashMap<Id, UnitAction>,
        unit_swapped: &mut HashSet<Id>,
    ) {
        let properties = &self.properties;
        for unit in &mut self.units {
            let swap_weapon = unit_actions
                .get(&unit.id)
                .map(|action| action.swap_weapon)
                .unwrap_or(false)
                && !unit_swapped.contains(&unit.id);
            let loot_boxes = self
                .loot_boxes
                .drain(..)
                .filter_map(|loot_box| {
                    if !loot_box.rect().intersects(&unit.rect()) {
                        return Some(loot_box);
                    }
                    let mut handle_item = |item: Item| -> Option<Item> {
                        match item {
                            Item::HealthPack { health } => {
                                if unit.health < properties.unit_max_health {
                                    events.push(Event::Heal);
                                    unit.health =
                                        (unit.health + health).min(properties.unit_max_health);
                                    None
                                } else {
                                    Some(Item::HealthPack { health })
                                }
                            }
                            Item::Weapon { mut weapon_type } => {
                                if let Some(unit_weapon) = &mut unit.weapon {
                                    if swap_weapon {
                                        let new_unit_weapon = Weapon::new(properties, weapon_type);
                                        weapon_type = unit_weapon.typ;
                                        *unit_weapon = new_unit_weapon;
                                        unit_swapped.insert(unit.id);
                                    }
                                    Some(Item::Weapon { weapon_type })
                                } else {
                                    events.push(Event::PickupWeapon);
                                    unit.weapon = Some(Weapon::new(properties, weapon_type));
                                    None
                                }
                            }
                            Item::Mine {} => {
                                events.push(Event::PickupMine);
                                unit.mines += 1;
                                None
                            }
                        }
                    };
                    if let Some(item) = handle_item(loot_box.item) {
                        Some(LootBox {
                            position: loot_box.position,
                            size: loot_box.size,
                            item,
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            self.loot_boxes.extend(loot_boxes);
        }
    }
    fn process_planting_mines(
        &mut self,
        unit_actions: &HashMap<Id, UnitAction>,
        unit_planted_mine: &mut HashSet<Id>,
    ) {
        for unit in &mut self.units {
            let action = unit_actions.get(&unit.id).cloned().unwrap_or_else(default);
            if action.plant_mine && !unit_planted_mine.contains(&unit.id) {
                if unit.mines > 0
                    && unit.on_ground
                    && !unit.on_ladder
                    && self
                        .level
                        .get(
                            unit.position.x.raw().max(0.0) as usize,
                            (unit.position.y.raw().max(0.0) as usize).max(1) - 1,
                        )
                        .map_or(false, |tile| tile.can_plant_mine())
                {
                    unit.mines -= 1;
                    self.mines
                        .push(Mine::spawn(unit.player_id, &self.properties, unit.position));
                }
                unit_planted_mine.insert(unit.id);
            }
        }
    }
    fn process_mines(&mut self, explosions: &mut Vec<Explosion>, delta_time: R64) {
        for mine in &mut self.mines {
            mine.timer = match mine.timer {
                Some(mut timer) => {
                    timer -= delta_time;
                    if timer > r64(0.0) {
                        Some(timer)
                    } else {
                        None
                    }
                }
                None => None,
            };
            match mine.state {
                MineState::Preparing => {
                    if mine.timer.is_none() {
                        mine.state = MineState::Idle;
                    }
                }
                MineState::Idle => {
                    for unit in &self.units {
                        let distance = unit.rect().distance_to(&mine.rect());
                        if distance < mine.trigger_radius {
                            mine.state = MineState::Triggered;
                            mine.timer = Some(self.properties.mine_trigger_time);
                            break;
                        }
                    }
                }
                MineState::Triggered => {
                    if mine.timer.is_none() {
                        mine.state = MineState::Exploded;
                    }
                }
                MineState::Exploded => {}
            };
            if mine.state == MineState::Exploded {
                explosions.push(Explosion {
                    position: mine.rect().center(),
                    params: mine.explosion_params.clone(),
                    player_id: mine.player_id,
                });
            }
        }
        self.mines.retain(|mine| mine.state != MineState::Exploded);
    }
    fn process_explosions(&mut self, explosions: &[Explosion]) {
        for explosion in explosions {
            let rect = AABB::from_corners(
                explosion.position - vec2(explosion.params.radius, explosion.params.radius),
                explosion.position + vec2(explosion.params.radius, explosion.params.radius),
            );
            for unit in &mut self.units {
                if unit.rect().intersects(&rect) {
                    let score = explosion.params.damage.min(unit.health).max(0) as usize;
                    unit.health -= explosion.params.damage;
                    if unit.player_id != explosion.player_id {
                        self.players
                            .iter_mut()
                            .find(|player| player.id == explosion.player_id)
                            .unwrap()
                            .score += score;
                    }
                }
            }
            for mine in &mut self.mines {
                if mine.rect().intersects(&rect) {
                    mine.state = MineState::Exploded;
                }
            }
        }
    }
    pub fn update(
        &mut self,
        rng: &mut dyn rand::RngCore,
        events: &mut Vec<Event>,
        unit_planted_mine: &mut HashSet<Id>,
        unit_swapped: &mut HashSet<Id>,
        unit_actions: &HashMap<Id, UnitAction>,
        delta_time: R64,
    ) {
        let mut explosions: Vec<Explosion> = Vec::new();
        self.process_planting_mines(unit_actions, unit_planted_mine);
        self.process_shooting(rng, events, unit_actions, delta_time);
        self.process_loot_boxes(events, unit_actions, unit_swapped);
        self.process_movement(unit_actions, delta_time);
        self.process_level_jumps(events);
        self.process_bullets(events, &mut explosions, delta_time);
        self.process_mines(&mut explosions, delta_time);
        self.process_explosions(&explosions);
        for unit in &self.units {
            if unit.health <= 0 {
                self.players
                    .iter_mut()
                    .find(|player| player.id != unit.player_id)
                    .unwrap()
                    .score += self.properties.kill_score;
            }
        }
        self.units.retain(|unit| unit.health > 0);
        for explosion in explosions {
            events.push(Event::Explosion {
                player_id: explosion.player_id,
                position: explosion.position,
                params: explosion.params,
            });
        }
    }
}
