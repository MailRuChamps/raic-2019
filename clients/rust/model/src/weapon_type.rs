use crate::*;
#[derive(Clone, Debug, PartialEq, Eq, Hash, trans::Trans)]
pub enum WeaponType {
    Pistol,
    AssaultRifle,
    RocketLauncher,
}
