use super::*;

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub enum Item {
    HealthPack { health: i32 },
    Weapon { weapon_type: WeaponType },
    Mine {},
}

#[derive(Debug, Serialize, Deserialize, Clone, Trans, Schematic)]
pub struct LootBox {
    pub position: Vec2<R64>,
    pub size: Vec2<R64>,
    pub item: Item,
}

impl LootBox {
    pub fn spawn(properties: &Properties, position: Vec2<R64>, item: Item) -> Self {
        Self {
            position,
            size: properties.loot_box_size,
            item,
        }
    }
    pub fn rect(&self) -> AABB<R64> {
        AABB::pos_size(
            self.position - vec2(self.size.x / r64(2.0), r64(0.0)),
            self.size,
        )
    }
}
