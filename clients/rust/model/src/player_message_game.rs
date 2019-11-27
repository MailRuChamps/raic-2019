use crate::*;
#[derive(Clone, Debug, trans::Trans)]
pub enum PlayerMessageGame {
    CustomDataMessage {
        data: CustomData,
    },
    ActionMessage {
        action: std::collections::HashMap<i32, UnitAction>,
    },
}
