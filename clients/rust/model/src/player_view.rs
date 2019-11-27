use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct PlayerView {
    pub my_id: i32,
    pub game: Game,
}
