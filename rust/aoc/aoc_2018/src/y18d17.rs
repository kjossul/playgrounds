use crate::grid::Grid;
use regex::Regex;
use std::collections::HashSet;

#[derive(PartialEq, Eq)]
enum Tile {
    CLAY,
    WATER,
}

#[aoc(day17, part1)]
pub fn part1(input: &str) -> usize {
    let mut grid = parser(input);
    let maxy = grid.get_bounds()[3];
    let mut drop_points = Vec::new();
    drop_points.push((500, 0));
    let mut completed = HashSet::new();
    let mut c = 0;
    'filling: loop {
        c += 1;
        if drop_points.iter().find(|&t| !completed.contains(t)).is_none() {
            break;
        }
        let (x, y) = *drop_points.iter().find(|&t| !completed.contains(t)).unwrap();
        let mut y1 = y;
        while grid.get(x, y1 + 1).is_none() && y1 < maxy {
            y1 += 1;
        }
        if y == y1 {
            completed.insert((x, y));
            let (mut lb, mut rb) = (false, false);
            for i in 1.. {
                if !lb && grid.get(x - i, y1).is_none() {
                    grid.insert(x - i + 1, y1, Tile::WATER);
                } else if !lb {
                    grid.insert(x - i + 1, y1, Tile::WATER);
                    lb = true;
                }
                if !rb && grid.get(x + i, y1).is_none() {
                    grid.insert(x + i - 1, y1, Tile::WATER);
                } else if !rb {
                    grid.insert(x + i - 1, y1, Tile::WATER);
                    rb = true;
                }
                if lb && rb {
                    break;
                }
            }
            continue;
        }
        if c > 150 {
            break;
        } else if y1 == maxy {
            continue 'filling;
        }
        grid.insert(x, y1, Tile::WATER);
        let (mut lb, mut rb) = (false, false);
        for i in 1.. {
            if !lb && grid.get(x - i, y1).is_none() && grid.get(x - i + 1, y1 + 1).is_some() {
                grid.insert(x - i + 1, y1, Tile::WATER);
            } else if !lb {
                lb = true;
                grid.insert(x - i + 1, y1, Tile::WATER);
                if grid.get(x - i + 1, y1 + 1).is_none() {
                    drop_points.push((x - i + 1, y1));
                    if !completed.insert((x, y)) {
                        drop_points.push((x, y + 1));
                    }
                }
            }
            if !rb && grid.get(x + i, y1).is_none() && grid.get(x + i - 1, y1 + 1).is_some() {
                grid.insert(x + i - 1, y1, Tile::WATER);
            } else if !rb {
                rb = true;
                grid.insert(x + i - 1, y1, Tile::WATER);
                if grid.get(x + i - 1, y1 + 1).is_none() {
                    drop_points.push((x + i - 1, y1));
                    if !completed.insert((x, y)) {
                        drop_points.push((x, y + 1));
                    }
                }
            }
            if lb && rb {
                break;
            }
        }
    }
    for &(x, mut y) in &drop_points {
        while grid.get(x, y + 1).is_none() && y < maxy {
            grid.insert(x, y, Tile::WATER);
            y += 1;
        }
        grid.insert(x, y, Tile::WATER);
    }
    grid.cells.retain(|&k, _| k.0 < 570);
    grid.print(|t| match t {
        Some(Tile::CLAY) => '#',
        Some(Tile::WATER) => '+',
        _ => '.',
    });
    0
}

fn parser(input: &str) -> Grid<Tile> {
    let mut grid = Grid::new();
    let re = Regex::new(r"\d+").unwrap();
    for line in input.lines() {
        let caps = re.find_iter(line).flat_map(|m| m.as_str().parse()).collect::<Vec<_>>();
        let is_h = line.chars().next() == Some('y');
        for j in caps[1]..=caps[2] {
            if is_h {
                grid.insert(j, caps[0], Tile::CLAY);
            } else {
                grid.insert(caps[0], j, Tile::CLAY);
            }
        }
    }
    grid
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d17test() {}
}