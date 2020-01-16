use super::*;

pub fn gen(rng: &mut dyn rand::RngCore) -> (Level, Vec<SpawnPoint>) {
    let width = 40;
    let height = 30;

    let w = 5;
    let h = 5;
    assert_eq!(width % w, 0);
    assert_eq!(height % h, 0);

    #[derive(Debug, Clone)]
    struct BigTile {
        left: bool,
        down: bool,
    };
    impl BigTile {
        fn new(left: bool, down: bool) -> Self {
            Self { left, down }
        }
    }
    let sw = width / w / 2;
    let sh = height / h;
    let mut big = vec![vec![BigTile::new(false, false); sh]; sw];
    let mut nx = vec![vec![vec2(0, 0); sh]; sw];
    for i in 0..sw {
        for j in 0..sh {
            nx[i][j] = vec2(i, j);
        }
    }
    fn get_root(nx: &[Vec<Vec2<usize>>], p: Vec2<usize>) -> Vec2<usize> {
        let mut p = p;
        while nx[p.x][p.y] != p {
            p = nx[p.x][p.y];
        }
        p
    }
    fn connected(nx: &[Vec<Vec2<usize>>]) -> bool {
        for i in 0..nx.len() {
            for j in 0..nx[0].len() {
                if get_root(nx, vec2(i, j)) != get_root(nx, vec2(0, 0)) {
                    return false;
                }
            }
        }
        true
    }
    fn unite(nx: &mut [Vec<Vec2<usize>>], p1: Vec2<usize>, p2: Vec2<usize>) {
        let r1 = get_root(nx, p1);
        let r2 = get_root(nx, p2);
        nx[r1.x][r1.y] = r2;
    }
    while !connected(&nx) {
        let mut options = Vec::new();
        for i in 0..sw {
            for j in 0..sh {
                if i > 0 && !big[i][j].left {
                    options.push((i, j, 0));
                }
                if j > 0 && !big[i][j].down {
                    options.push((i, j, 1));
                }
            }
        }
        let (i, j, side) = options[rng.gen_range(0, options.len())];
        match side {
            0 => {
                big[i][j].left = true;
                unite(&mut nx, vec2(i, j), vec2(i - 1, j));
            }
            1 => {
                big[i][j].down = true;
                unite(&mut nx, vec2(i, j), vec2(i, j - 1));
            }
            _ => unreachable!(),
        }
    }

    let mut level = Level {
        tiles: vec![vec![Tile::Empty; height]; width],
    };
    for i in 0..sw {
        let mut j = 0;
        while j < sh {
            let mut nj = j + 1;
            let mx = rng.gen_range(1, 3);
            while nj < j + mx + 1 && nj < sh && big[i][nj].down {
                nj += 1;
            }
            let nj = nj - 1;
            let y = nj * h;
            if nj > j {
                if nj + 1 < sh && big[i][nj + 1].down {
                    for x in i * w..(i + 1) * w {
                        level.tiles[x][y] = Tile::Platform;
                    }
                }
                match nj - j {
                    1 => {}
                    2 => {
                        let x = rng.gen_range(i * w + 2, (i + 1) * w - 1);
                        if !(nj + 1 < sh && big[i][nj + 1].down) || rng.gen() {
                            let y = j * h + 1;
                            level.tiles[x][y] = Tile::JumpPad;
                        } else {
                            for y in j * h + 1..=nj * h {
                                level.tiles[x][y] = Tile::Ladder;
                            }
                        }
                    }
                    _ => unreachable!(),
                }
                j = nj;
            } else {
                j += 1;
            }
        }
    }
    for i in 0..sw {
        for j in 0..sh {
            if !big[i][j].left {
                let x = i * w;
                for y in j * h..(j + 1) * h + 1 {
                    if let Some(tile) = level.tiles[x].get_mut(y) {
                        *tile = Tile::Wall;
                    }
                }
            }
            if !big[i][j].down {
                let y = j * h;
                for x in i * w..(i + 1) * w + 1 {
                    level.tiles[x][y] = Tile::Wall;
                }
            }
        }
    }
    for x in 0..width / 2 {
        for y in 0..height {
            level.tiles[width - 1 - x][y] = level.tiles[x][y].clone();
        }
    }
    for x in 0..width {
        level.tiles[x][height - 1] = Tile::Wall;
    }

    let mut spawn_points = Vec::new();
    for i in 1..3 {
        spawn_points.push(vec2(r64(i as f64 + 0.5), r64(1.0)));
        spawn_points.push(vec2(r64(width as f64 - i as f64 - 0.5), r64(1.0)));
    }
    (level, spawn_points)
}
