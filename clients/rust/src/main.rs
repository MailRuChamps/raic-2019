mod my_strategy;

use my_strategy::MyStrategy;

struct Args {
    host: String,
    port: u16,
    token: String,
}

impl Args {
    fn parse() -> Self {
        let mut args = std::env::args();
        args.next().unwrap();
        let host = args.next().unwrap_or("127.0.0.1".to_owned());
        let port = args
            .next()
            .map_or(31001, |s| s.parse().expect("Can't parse port"));
        let token = args.next().unwrap_or("0000000000000000".to_string());
        Self { host, port, token }
    }
}

struct Runner {
    reader: Box<dyn std::io::BufRead>,
    writer: Box<dyn std::io::Write>,
}

pub struct Debug<'a>(&'a mut dyn std::io::Write);

impl Debug<'_> {
    fn draw(&mut self, data: model::CustomData) {
        use trans::Trans;
        model::PlayerMessageGame::CustomDataMessage { data }
            .write_to(&mut self.0)
            .expect("Failed to write custom debug data");
    }
}

impl Runner {
    fn new(args: &Args) -> std::io::Result<Self> {
        use std::io::Write;
        use trans::Trans;
        let stream = std::net::TcpStream::connect((args.host.as_str(), args.port))?;
        stream.set_nodelay(true)?;
        let stream_clone = stream.try_clone()?;
        let reader = std::io::BufReader::new(stream);
        let mut writer = std::io::BufWriter::new(stream_clone);
        args.token.write_to(&mut writer)?;
        writer.flush()?;
        Ok(Self {
            reader: Box::new(reader),
            writer: Box::new(writer),
        })
    }
    fn run(mut self) -> std::io::Result<()> {
        use trans::Trans;
        let mut strategy = MyStrategy::new();
        loop {
            let message = model::ServerMessageGame::read_from(&mut self.reader)?;
            let player_view = match message.player_view {
                Some(view) => view,
                None => break,
            };
            let mut actions = std::collections::HashMap::new();
            for unit in player_view
                .game
                .units
                .iter()
                .filter(|unit| unit.player_id == player_view.my_id)
            {
                let action =
                    strategy.get_action(unit, &player_view.game, &mut Debug(&mut self.writer));
                actions.insert(unit.id, action);
            }
            let message = model::PlayerMessageGame::ActionMessage {
                action: model::Versioned { inner: actions },
            };
            message.write_to(&mut self.writer)?;
            self.writer.flush()?;
        }
        Ok(())
    }
}

fn main() -> std::io::Result<()> {
    Runner::new(&Args::parse())?.run()
}
