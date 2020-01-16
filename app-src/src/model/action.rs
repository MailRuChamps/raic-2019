use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
#[schematic(rename = "UnitAction")]
pub struct OldUnitAction {
    pub velocity: R64,
    pub jump: bool,
    pub jump_down: bool,
    pub aim: Vec2<R64>,
    pub shoot: bool,
    pub swap_weapon: bool,
    pub plant_mine: bool,
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct UnitAction {
    pub velocity: R64,
    pub jump: bool,
    pub jump_down: bool,
    pub aim: Vec2<R64>,
    pub shoot: bool,
    pub reload: bool,
    pub swap_weapon: bool,
    pub plant_mine: bool,
}

impl From<OldUnitAction> for UnitAction {
    fn from(old: OldUnitAction) -> Self {
        Self {
            velocity: old.velocity,
            jump: old.jump,
            jump_down: old.jump_down,
            aim: old.aim,
            shoot: old.shoot,
            reload: false,
            swap_weapon: old.swap_weapon,
            plant_mine: old.plant_mine,
        }
    }
}

impl Default for UnitAction {
    fn default() -> Self {
        Self {
            velocity: r64(0.0),
            jump: false,
            jump_down: false,
            aim: vec2(r64(0.0), r64(0.0)),
            shoot: false,
            reload: false,
            swap_weapon: false,
            plant_mine: false,
        }
    }
}
