pub use codegame::prelude::*;
#[cfg(feature = "rendering")]
use geng::ui;

#[cfg(feature = "rendering")]
mod keyboard_player;
// #[cfg(feature = "rendering")]
// mod level_editor;
pub mod model;
mod quickstart_player;
mod random_player;
#[cfg(feature = "rendering")]
mod renderer;

// #[cfg(feature = "rendering")]
// use level_editor::*;
#[cfg(feature = "rendering")]
use renderer::*;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum OptionsPreset {
    Round1,
    Round2,
    Finals,
    Custom(model::Options),
}

impl From<OptionsPreset> for model::Options {
    fn from(preset: OptionsPreset) -> model::Options {
        match preset {
            // TODO
            OptionsPreset::Round1 => model::Options {
                level: model::LevelOptions::Simple,
                properties: Some(model::Properties {
                    team_size: 1,
                    ..default()
                }),
            },
            OptionsPreset::Round2 => model::Options {
                level: model::LevelOptions::Simple,
                properties: Some(model::Properties {
                    team_size: 2,
                    ..default()
                }),
            },
            OptionsPreset::Finals => model::Options {
                level: model::LevelOptions::Complex,
                properties: Some(model::Properties {
                    team_size: 2,
                    ..default()
                }),
            },
            OptionsPreset::Custom(options) => options,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum PlayerOptions {
    #[cfg(feature = "rendering")]
    Keyboard,
    Tcp(codegame::TcpPlayerOptions),
    Quickstart,
    Empty(codegame::EmptyPlayerOptions),
    Random(random_player::RandomPlayerOptions),
}

impl From<codegame::TcpPlayerOptions> for PlayerOptions {
    fn from(options: codegame::TcpPlayerOptions) -> Self {
        Self::Tcp(options)
    }
}

impl From<codegame::EmptyPlayerOptions> for PlayerOptions {
    fn from(options: codegame::EmptyPlayerOptions) -> Self {
        Self::Empty(options)
    }
}

pub struct PlayerExtraData {
    #[cfg(feature = "rendering")]
    keyboard_input: Arc<Mutex<keyboard_player::Input>>,
}

impl codegame::PlayerOptions<model::Game> for PlayerOptions {
    fn get(
        &self,
        extra_data: &PlayerExtraData,
    ) -> Pin<
        Box<
            dyn Future<
                Output = Result<Box<dyn codegame::Player<model::Game>>, codegame::PlayerError>,
            >,
        >,
    > {
        match self {
            #[cfg(feature = "rendering")]
            Self::Keyboard => futures::future::ready(Ok(Box::new(
                keyboard_player::KeyboardPlayer::new(&extra_data.keyboard_input),
            )
                as Box<dyn codegame::Player<model::Game>>))
            .boxed_local(),
            Self::Tcp(options) => codegame::TcpPlayer::new(options.clone())
                .map(|result| match result {
                    Ok(player) => Ok(Box::new(player) as Box<dyn codegame::Player<model::Game>>),
                    Err(e) => Err(codegame::PlayerError::IOError(e)),
                })
                .boxed_local(),
            Self::Quickstart => {
                futures::future::ready(Ok(Box::new(quickstart_player::QuickstartPlayer::new())
                    as Box<dyn codegame::Player<model::Game>>))
                .boxed_local()
            }
            Self::Empty(_) => futures::future::ready(Ok(
                Box::new(codegame::EmptyPlayer) as Box<dyn codegame::Player<model::Game>>
            ))
            .boxed_local(),
            Self::Random(options) => futures::future::ready(Ok(Box::new(
                random_player::RandomPlayer::new(options.clone()),
            )
                as Box<dyn codegame::Player<model::Game>>))
            .boxed_local(),
        }
    }
}

#[cfg(feature = "rendering")]
#[cfg(target_arch = "wasm32")]
#[derive(geng::Assets)]
pub struct Levels {
    #[asset(path = "level.txt")]
    level: String,
}

#[cfg(feature = "rendering")]
#[derive(geng::Assets)]
pub struct Assets {
    #[cfg(target_arch = "wasm32")]
    #[asset(path = "levels")]
    levels: Levels,
    #[asset(path = "assets")]
    renderer_assets: renderer::Assets,
}

#[derive(Debug, StructOpt, Default)]
pub struct Opt {
    #[structopt(long)]
    pub lang: Option<String>,
    #[structopt(long)]
    pub replay: Option<String>,
    #[structopt(long)]
    pub repeat: Option<String>,
    #[structopt(long)]
    pub save_replay: Option<String>,
    #[structopt(long)]
    pub save_results: Option<String>,
    #[structopt(long)]
    pub config: Option<std::path::PathBuf>,
    #[structopt(skip = None)]
    pub actual_config: Option<codegame::FullOptions<model::Game>>,
    #[structopt(long)]
    pub batch_mode: bool,
    #[structopt(long)]
    pub log_level: Option<log::LevelFilter>,
    #[structopt(long)]
    pub player_names: Vec<String>,
}

pub fn run_with(opt: Opt) {
    let mut config: Option<codegame::FullOptions<model::Game>> = opt.actual_config;
    #[cfg(not(target_arch = "wasm32"))]
    {
        if let Some(path) = opt.config {
            config = Some(
                codegame::FullOptions::<model::Game>::load(
                    std::fs::File::open(path).expect("Could not open file"),
                )
                .expect("Could not load config"),
            );
        }
    }

    let player_extra_data = PlayerExtraData {
        #[cfg(feature = "rendering")]
        keyboard_input: Arc::new(Mutex::new(keyboard_player::Input {
            mouse_pos: vec2(r64(0.0), r64(0.0)),
            pressed_keys: HashSet::new(),
            pressed_buttons: HashSet::new(),
        })),
    };

    let processor = if let Some(config) = config {
        let mut processor = if let Some(path) = opt.repeat {
            codegame::GameProcessor::<model::Game>::repeat_full(
                config,
                &player_extra_data,
                std::fs::File::open(path).expect("Failed to open game log file"),
            )
        } else {
            codegame::GameProcessor::<model::Game>::new_full(config, &player_extra_data)
        };
        if let Some(path) = &opt.save_replay {
            processor.set_tick_handler(codegame::save_replay_tick_handler(
                std::io::BufWriter::new(
                    std::fs::File::create(path).expect("Failed to create replay file"),
                ),
            ));
        }
        if let Some(path) = &opt.save_results {
            let writer = std::io::BufWriter::new(
                std::fs::File::create(path).expect("Failed to create results file"),
            );
            processor.set_results_handler(Box::new(move |results| {
                serde_json::to_writer_pretty(writer, &results).expect("Failed to write results");
            }));
        }
        Some(processor)
    } else {
        None
    };

    if opt.batch_mode {
        match processor {
            Some(processor) => processor.run(),
            None => panic!("Batch mode can only be used with --config option"),
        }
        return;
    }

    #[cfg(not(feature = "rendering"))]
    panic!("Rendering feature not enabled");

    #[cfg(feature = "rendering")]
    {
        let geng = Rc::new(Geng::new(geng::ContextOptions {
            title: "AI Cup 2019".to_owned(),
            ..default()
        }));
        let theme = Rc::new(geng::ui::Theme::default(&geng));

        let replay: futures::future::OptionFuture<_> = opt
            .replay
            .map(|path| codegame::History::<model::Game, Renderer>::load(&path))
            .into();

        let player_names = opt.player_names;

        let preferences = Rc::new(RefCell::new(AutoSave::<
            codegame::AppPreferences<RendererPreferences>,
        >::load("aicup2019-preferences.json")));

        let state = geng::LoadingScreen::new(
            &geng,
            geng::EmptyLoadingScreen,
            futures::future::join(<Assets as geng::LoadAsset>::load(&geng, "."), replay),
            {
                let geng = geng.clone();
                move |(assets, replay)| {
                    let assets = assets.expect("Failed to load assets");
                    let renderer = Renderer::new(
                        &geng,
                        preferences.clone(),
                        &player_extra_data.keyboard_input,
                        player_names,
                        assets.renderer_assets,
                    );
                    if let Some(processor) = processor {
                        Box::new(codegame::GameScreen::new(
                            &geng,
                            processor,
                            renderer,
                            preferences,
                        )) as Box<dyn geng::State>
                    } else if let Some(replay) = replay {
                        Box::new(codegame::GameScreen::replay(
                            &geng,
                            replay,
                            renderer,
                            preferences,
                        )) as Box<dyn geng::State>
                    } else {
                        let player_config_options = vec![
                            keyboard_player::Config::constructor(
                                &geng,
                                &theme,
                                &player_extra_data.keyboard_input,
                            ),
                            #[cfg(not(target_arch = "wasm32"))]
                            codegame::TcpPlayerConfig::constructor(&geng, &theme),
                            quickstart_player::Config::constructor(&geng, &theme),
                            random_player::Config::constructor(&geng, &theme),
                            codegame::EmptyPlayerConfig::constructor(&geng, &theme),
                        ];
                        let mut levels = vec![
                            (translate("simple").to_owned(), model::LevelOptions::Simple),
                            (
                                translate("complex").to_owned(),
                                model::LevelOptions::Complex,
                            ),
                        ];
                        #[cfg(target_arch = "wasm32")]
                        {
                            levels.push((
                                "level.txt".to_owned(),
                                model::LevelOptions::parse(&assets.levels.level),
                            ));
                        }
                        #[cfg(not(target_arch = "wasm32"))]
                        {
                            fn add_levels(
                                levels: &mut Vec<(String, model::LevelOptions)>,
                            ) -> std::io::Result<()> {
                                for entry in std::fs::read_dir("levels")? {
                                    let entry = entry?;
                                    if entry.path().is_file() {
                                        levels.push((
                                            entry.path().to_str().unwrap().to_owned(),
                                            model::LevelOptions::LoadFrom { path: entry.path() },
                                        ));
                                    }
                                }
                                Ok(())
                            }
                            add_levels(&mut levels).expect("Failed to get list of levels");
                        }
                        let game_options_config = model::OptionsConfig::new(&geng, &theme, levels);
                        Box::new(codegame::ConfigScreen::new(
                            &geng,
                            &theme,
                            Box::new(game_options_config),
                            player_config_options,
                            vec![0, 1],
                            renderer,
                            preferences,
                        ))
                    }
                }
            },
        );
        geng::run(geng, state);
    }
}

pub fn run() {
    let opt: Opt = program_args::parse();
    if let Some(level) = opt.log_level {
        logger::init_with_level(level)
    } else {
        logger::init()
    }
    .expect("Failed to initialize logger");
    #[cfg(feature = "rendering")]
    logger::add_logger(Box::new(geng::logger()));
    add_translations(include_str!("translations.txt"));
    if let Some(lang) = &opt.lang {
        set_locale(lang);
    }
    if let Ok(dir) = std::env::var("CARGO_MANIFEST_DIR") {
        std::env::set_current_dir(std::path::Path::new(&dir).join("static")).unwrap();
    } else {
        #[cfg(not(target_arch = "wasm32"))]
        {
            if let Some(path) = std::env::current_exe().unwrap().parent() {
                std::env::set_current_dir(path).unwrap();
            }
        }
    }
    run_with(program_args::parse());
}
