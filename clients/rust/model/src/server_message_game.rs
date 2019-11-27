use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub struct ServerMessageGame {
    pub player_view: Option<PlayerView>,
}
