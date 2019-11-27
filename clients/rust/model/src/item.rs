use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub enum Item {
    HealthPack {
        health: i32,
    },
    Weapon {
        weapon_type: WeaponType,
    },
    Mine {
    },
}
