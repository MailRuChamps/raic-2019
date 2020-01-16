use super::*;

mod complex;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum LevelOptions {
    EmptyBox {
        width: usize,
        height: usize,
    },
    LoadFrom {
        path: std::path::PathBuf,
    },
    Ready {
        level: Level,
        spawn_points: Vec<SpawnPoint>,
    },
    Simple,
    Complex,
}

impl LevelOptions {
    pub fn parse(s: &str) -> Self {
        let mut spawn_points = Vec::new();
        let mut extra_spawn_points = Vec::new();
        let mut tiles = Vec::new();
        for (y, line) in s.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                while x >= tiles.len() {
                    tiles.push(Vec::new());
                }
                while y >= tiles[x].len() {
                    tiles[x].push(Tile::Empty);
                }
                tiles[x][y] = match c {
                    '#' => Tile::Wall,
                    '.' => Tile::Empty,
                    '^' => Tile::Platform,
                    'H' => Tile::Ladder,
                    'T' => Tile::JumpPad,
                    'P' => {
                        spawn_points.push(vec2(r64(x as f64 + 0.5), r64(y as _)));
                        Tile::Empty
                    }
                    _ => {
                        if let Some(digit) = c.to_digit(10) {
                            extra_spawn_points
                                .push((digit, vec2(r64(x as f64 + 0.5), r64(y as _))));
                            Tile::Empty
                        } else {
                            panic!("Unexpected character");
                        }
                    }
                }
            }
        }
        extra_spawn_points.sort_by_key(|&(digit, _)| digit);
        for (_, point) in extra_spawn_points {
            spawn_points.push(point);
        }
        let size = vec2(tiles.len(), tiles[0].len());
        for row in &mut tiles {
            assert_eq!(row.len(), size.y);
            row.reverse();
        }
        for spawn_point in &mut spawn_points {
            spawn_point.y = r64(size.y as _) - spawn_point.y - r64(1.0);
        }
        LevelOptions::Ready {
            level: Level::new(tiles),
            spawn_points,
        }
    }
}

pub type SpawnPoint = Vec2<R64>;

impl LevelOptions {
    pub fn create(self, rng: &mut dyn rand::RngCore) -> (Level, Vec<SpawnPoint>) {
        match self {
            LevelOptions::EmptyBox { width, height } => {
                assert!(width > 2 && height > 2);
                let mut tiles = vec![vec![Tile::Empty; height]; width];
                for (x, row) in tiles.iter_mut().enumerate() {
                    for (y, tile) in row.iter_mut().enumerate() {
                        if x == 0 || x + 1 == width || y == 0 || y + 1 == height {
                            *tile = Tile::Wall;
                        }
                    }
                }
                let mut spawn_points = Vec::new();
                for i in 1..width / 2 {
                    spawn_points.push(vec2(r64(i as f64 + 0.5), r64(1.0)));
                    spawn_points.push(vec2(r64(width as f64 - i as f64 - 0.5), r64(1.0)));
                }
                (Level::new(tiles), spawn_points)
            }
            LevelOptions::LoadFrom { path } => {
                fn load(path: std::path::PathBuf) -> std::io::Result<LevelOptions> {
                    let mut content = String::new();
                    std::fs::File::open(path)?.read_to_string(&mut content)?;
                    Ok(LevelOptions::parse(&content))
                }
                load(path).expect("Failed to load map").create(rng)
            }
            LevelOptions::Ready {
                level,
                spawn_points,
            } => (level, spawn_points),
            LevelOptions::Simple => {
                let width = 40;
                let height = 30;
                let mut tiles = vec![vec![Tile::Empty; height]; width];
                for y in 0..height {
                    tiles[0][y] = Tile::Wall;
                }
                let step = 5;
                let mut y = 0;
                let mut spawn_points = Vec::new();
                for x in 0..width / 2 {
                    tiles[x][height - 1] = Tile::Wall;
                    if x % step == 0 {
                        let next_y = rng.gen_range(0, height - 5);
                        for y in min(y, next_y)..=max(y, next_y) {
                            tiles[x][y] = Tile::Wall;
                        }
                        if x != 0 {
                            let options = if max(next_y, y) - min(next_y, y) < 10 {
                                3
                            } else {
                                2
                            };
                            match rng.gen_range(0, options) {
                                0 => {
                                    let x = if next_y > y { x - 1 } else { x + 1 };
                                    for y in (min(y, next_y) + 1)..=max(y, next_y) {
                                        tiles[x][y] = Tile::Ladder;
                                    }
                                }
                                1 => {
                                    let x_range = if next_y > y {
                                        (x - 2)..x
                                    } else {
                                        (x + 1)..(x + 3)
                                    };
                                    for x in x_range {
                                        for y in
                                            ((min(y, next_y))..=max(y, next_y)).step_by(3).skip(1)
                                        {
                                            tiles[x][y] = Tile::Platform;
                                        }
                                    }
                                }
                                2 => {
                                    let x = if next_y > y { x - 1 } else { x + 1 };
                                    tiles[x][min(y, next_y) + 1] = Tile::JumpPad;
                                }
                                _ => unreachable!(),
                            }
                        }
                        y = next_y;
                    }
                    tiles[x][y] = Tile::Wall;
                    if x > 0 {
                        spawn_points.push(vec2(r64(x as f64 + 0.5), r64(y as f64 + 1.0)));
                        spawn_points.push(vec2(
                            r64(width as f64 - x as f64 - 0.5),
                            r64(y as f64 + 1.0),
                        ));
                    }
                }
                for x in 0..width / 2 {
                    for y in 0..height {
                        tiles[width - 1 - x][y] = tiles[x][y].clone();
                    }
                }
                (Level::new(tiles), spawn_points)
            }
            LevelOptions::Complex => complex::gen(rng),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Trans, Schematic)]
pub enum Tile {
    Empty,
    Wall,
    Platform,
    Ladder,
    JumpPad,
}

impl Tile {
    pub fn is_blocking(&self) -> bool {
        match self {
            Tile::Empty => false,
            Tile::Wall => true,
            Tile::Platform => false,
            Tile::Ladder => false,
            Tile::JumpPad => false,
        }
    }
    pub fn can_plant_mine(&self) -> bool {
        match self {
            Tile::Empty => false,
            Tile::Wall => true,
            Tile::Platform => true,
            Tile::Ladder => false,
            Tile::JumpPad => false,
        }
    }
    pub fn is_walkable(&self) -> bool {
        match self {
            Tile::Empty => false,
            Tile::Wall => true,
            Tile::Platform => true,
            Tile::Ladder => true,
            Tile::JumpPad => false,
        }
    }
    pub fn is_destructible(&self) -> bool {
        match self {
            _ => true,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Diff, PartialEq, Eq, Trans, Schematic)]
pub struct Level {
    #[diff = "eq"]
    pub tiles: Vec<Vec<Tile>>,
}

impl<'a> IntoIterator for &'a Level {
    type Item = (Vec2<usize>, &'a Tile);
    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.tiles.iter().enumerate().flat_map(move |(x, row)| {
            row.iter()
                .enumerate()
                .map(move |(y, tile)| (vec2(x, y), tile))
        }))
    }
}

impl Level {
    fn new(tiles: Vec<Vec<Tile>>) -> Self {
        Self { tiles }
    }
    pub fn get(&self, x: usize, y: usize) -> Option<&Tile> {
        self.tiles.get(x).and_then(|row| row.get(y))
    }
    pub fn iter(&self) -> impl Iterator<Item = (Vec2<usize>, &Tile)> {
        self.into_iter()
    }
    pub fn size(&self) -> Vec2<usize> {
        vec2(self.tiles.len(), self.tiles[0].len())
    }
    pub fn find_tiles(&self, rect: AABB<R64>) -> impl Iterator<Item = (Vec2<usize>, &Tile)> {
        let start = rect
            .bottom_left()
            .map(|t| t.raw().floor().max(0.0) as usize);
        let end = rect.top_right().map(|t| t.raw().ceil().max(0.0) as usize);
        let start = vec2(min(start.x, self.size().x), min(start.y, self.size().y));
        let end = vec2(min(end.x, self.size().x), min(end.y, self.size().y));
        self.tiles[start.x..end.x]
            .iter()
            .enumerate()
            .flat_map(move |(dx, row)| {
                row[start.y..end.y]
                    .iter()
                    .enumerate()
                    .map(move |(dy, tile)| (vec2(start.x + dx, start.y + dy), tile))
            })
    }
    pub fn find_tiles_mut(
        &mut self,
        rect: AABB<R64>,
    ) -> impl Iterator<Item = (Vec2<usize>, &mut Tile)> {
        let start = rect
            .bottom_left()
            .map(|t| t.raw().floor().max(0.0) as usize);
        let end = rect.top_right().map(|t| t.raw().ceil().max(0.0) as usize);
        let end = vec2(min(end.x, self.size().x), min(end.y, self.size().y));
        self.tiles[start.x..end.x]
            .iter_mut()
            .enumerate()
            .flat_map(move |(dx, row)| {
                row[start.y..end.y]
                    .iter_mut()
                    .enumerate()
                    .map(move |(dy, tile)| (vec2(start.x + dx, start.y + dy), tile))
            })
    }
    pub fn save(
        &self,
        mut out: impl Write,
        spawn_points: &HashSet<Vec2<usize>>,
    ) -> std::io::Result<()> {
        for row in (0..self.size().y).rev() {
            for col in 0..self.size().x {
                let c = if spawn_points.contains(&vec2(col, row)) {
                    'P'
                } else {
                    match self.tiles[col][row] {
                        Tile::Empty => '.',
                        Tile::Wall => '#',
                        Tile::Ladder => 'H',
                        Tile::JumpPad => 'T',
                        Tile::Platform => '^',
                    }
                };
                write!(out, "{}", c)?
            }
            writeln!(out)?
        }
        Ok(())
    }
}
