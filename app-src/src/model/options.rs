use super::*;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Options {
    pub level: LevelOptions,
    pub properties: Option<Properties>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            level: LevelOptions::EmptyBox {
                width: 40,
                height: 30,
            },
            properties: None,
        }
    }
}

use crate::*;

#[cfg(feature = "rendering")]
mod config {
    use super::*;

    pub struct OptionsConfig {
        geng: Rc<Geng>,
        theme: Rc<ui::Theme>,
        predefined_maps: Vec<(String, model::LevelOptions)>,
        team_size: usize,
        team_size_button: ui::TextButton,
        current_option: usize,
        level_button: ui::TextButton,
        custom_level: Rc<RefCell<Option<model::LevelOptions>>>,
        level_editor_button: ui::TextButton,
        request_level_editor: bool,
    }

    impl OptionsConfig {
        pub fn new(
            geng: &Rc<Geng>,
            theme: &Rc<ui::Theme>,
            predefined_maps: Vec<(String, model::LevelOptions)>,
        ) -> Self {
            Self {
                geng: geng.clone(),
                theme: theme.clone(),
                predefined_maps,
                team_size: 1,
                team_size_button: ui::TextButton::new(geng, theme, String::new(), 32.0),
                current_option: 0,
                level_button: ui::TextButton::new(geng, theme, String::new(), 32.0),
                custom_level: Rc::new(RefCell::new(None)),
                level_editor_button: ui::TextButton::new(
                    geng,
                    theme,
                    translate("level editor").to_owned(),
                    32.0,
                ),
                request_level_editor: false,
            }
        }
    }

    impl ui::Config<OptionsPreset> for OptionsConfig {
        fn get(&self) -> OptionsPreset {
            OptionsPreset::Custom(model::Options {
                level: self.predefined_maps[self.current_option].1.clone(),
                properties: Some(model::Properties {
                    team_size: self.team_size,
                    ..default()
                }),
            })
        }
        fn ui<'a>(&'a mut self) -> Box<dyn ui::Widget + 'a> {
            use ui::*;
            if let Some(options) = &*self.custom_level.borrow() {
                if self.predefined_maps.last().unwrap().0 == "custom" {
                    self.predefined_maps.last_mut().unwrap().1 = options.clone();
                } else {
                    self.current_option = self.predefined_maps.len();
                    self.predefined_maps
                        .push(("custom".to_owned(), options.clone()));
                }
            }
            self.level_button.text = self.predefined_maps[self.current_option].0.clone();
            self.team_size_button.text = self.team_size.to_string();
            let request_level_editor = &mut self.request_level_editor;
            let ui = ui::column![
                row![
                    text(translate("Team size:"), &self.theme.font, 32.0, Color::GRAY)
                        .padding_right(32.0),
                    self.team_size_button.ui(Box::new({
                        let team_size = &mut self.team_size;
                        let change = ((*team_size) % 2) + 1;
                        move || {
                            *team_size = change;
                        }
                    })),
                ]
                .align(vec2(0.5, 0.5)),
                row![
                    text(translate("Level:"), &self.theme.font, 32.0, Color::GRAY)
                        .padding_right(32.0),
                    self.level_button.ui(Box::new({
                        let current_option = &mut self.current_option;
                        let change = (*current_option + 1) % self.predefined_maps.len();
                        move || {
                            *current_option = change;
                        }
                    })),
                ]
                .align(vec2(0.5, 0.5)),
                // self.level_editor_button
                //     .ui(Box::new(move || {
                //         *request_level_editor = true;
                //     }))
                //     .align(vec2(0.5, 0.5)),
                // self.properties.ui().fixed_size(vec2(300.0, 200.0)),
            ];
            Box::new(ui)
        }
    }

    impl codegame::DeepConfig<OptionsPreset> for OptionsConfig {
        fn transition(&mut self) -> Option<Box<dyn geng::State>> {
            // if self.request_level_editor {
            //     self.request_level_editor = false;
            //     return Some(Box::new(LevelEditor::new(
            //         &self.geng,
            //         &Rc::new(SimpleRenderer::new(&self.geng)),
            //         &self.theme,
            //         &self.custom_level,
            //         vec2(40, 30),
            //     )));
            // }
            None
        }
    }
}
#[cfg(feature = "rendering")]
pub use config::OptionsConfig;
