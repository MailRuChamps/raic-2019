use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Trans, Schematic)]
pub struct Properties {
    pub max_tick_count: usize,
    pub team_size: usize,
    pub ticks_per_second: R64,
    pub updates_per_tick: usize,
    pub loot_box_size: Vec2<R64>,
    pub unit_size: Vec2<R64>,
    pub unit_max_horizontal_speed: R64,
    pub unit_fall_speed: R64,
    pub unit_jump_time: R64,
    pub unit_jump_speed: R64,
    pub jump_pad_jump_time: R64,
    pub jump_pad_jump_speed: R64,
    pub unit_max_health: i32,
    pub health_pack_health: i32,
    pub weapon_params: HashMap<WeaponType, WeaponParams>,
    pub mine_size: Vec2<R64>,
    pub mine_explosion_params: ExplosionParams,
    pub mine_prepare_time: R64,
    pub mine_trigger_time: R64,
    pub mine_trigger_radius: R64,
    pub kill_score: usize,
}

impl Default for Properties {
    fn default() -> Self {
        Self {
            max_tick_count: 3_600,
            team_size: 1,
            ticks_per_second: r64(60.0),
            #[cfg(target_arch = "wasm32")]
            updates_per_tick: 1,
            #[cfg(not(target_arch = "wasm32"))]
            updates_per_tick: 100,
            loot_box_size: vec2(r64(0.5), r64(0.5)),
            unit_size: vec2(r64(0.9), r64(1.8)),
            unit_max_horizontal_speed: r64(10.0),
            unit_fall_speed: r64(10.0),
            unit_jump_time: r64(0.55),
            unit_jump_speed: r64(10.0),
            jump_pad_jump_time: r64(0.525),
            jump_pad_jump_speed: r64(20.0),
            unit_max_health: 100,
            health_pack_health: 50,
            mine_size: vec2(r64(0.5), r64(0.5)),
            mine_explosion_params: ExplosionParams {
                radius: r64(3.0),
                damage: 50,
            },
            mine_prepare_time: r64(1.0),
            mine_trigger_time: r64(0.5),
            mine_trigger_radius: r64(1.0),
            weapon_params: hashmap![
                WeaponType::Pistol => WeaponParams {
                    fire_rate: r64(0.4),
                    magazine_size: 8,
                    reload_time: r64(1.0),
                    min_spread: r64(0.05),
                    max_spread: r64(0.5),
                    recoil: r64(0.5),
                    aim_speed: r64(1.0),
                    bullet: BulletParams {
                        speed: r64(50.0),
                        size: r64(0.2),
                        damage: 20,
                    },
                    explosion: None,
                },
                WeaponType::AssaultRifle => WeaponParams {
                    fire_rate: r64(0.1),
                    magazine_size: 20,
                    reload_time: r64(1.0),
                    min_spread: r64(0.1),
                    max_spread: r64(0.5),
                    recoil: r64(0.2),
                    aim_speed: r64(1.9),
                    bullet: BulletParams {
                        speed: r64(50.0),
                        size: r64(0.2),
                        damage: 5,
                    },
                    explosion: None,
                },
                WeaponType::RocketLauncher => WeaponParams {
                    fire_rate: r64(1.0),
                    magazine_size: 1,
                    reload_time: r64(1.0),
                    min_spread: r64(0.1),
                    max_spread: r64(0.5),
                    recoil: r64(1.0),
                    aim_speed: r64(1.0),
                    bullet: BulletParams {
                        speed: r64(20.0),
                        size: r64(0.4),
                        damage: 30,
                    },
                    explosion: Some(ExplosionParams{
                        radius: r64(3.0),
                        damage: 50,
                    }),
                },
            ],
            kill_score: 1000,
        }
    }
}
