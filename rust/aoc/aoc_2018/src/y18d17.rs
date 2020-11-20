use crate::grid::Grid;
use regex::Regex;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum Tile {
    CLAY,
    WATER,
}

#[aoc(day17, part1)]
pub fn part1(input: &str) -> usize {
    let mut grid = parser(input);
    let bounds = grid.get_bounds();
    let completed = fill(&mut grid);
    for &(x, mut y) in &completed {
        while grid.get(x, y + 1).is_none() && y <= bounds[3] {
            grid.insert(x, y, Tile::WATER);
            y += 1;
        }
        grid.insert(x, y, Tile::WATER);
    }
    grid.cells.retain(|&(_, y), _| y >= bounds[1] && y <= bounds[3]);
    grid.cells.iter().filter(|(_, &t)| t == Tile::WATER).count()
}

#[aoc(day17, part2)]
pub fn part2(input: &str) -> usize {
    let mut grid = parser(input);
    let bounds = grid.get_bounds();
    let completed = fill(&mut grid);
    for &(x, y) in &completed {
        if grid.get(x, y - 1) == Some(&Tile::WATER) {
            continue;
        }
        let mut i = 1;
        let (mut lb, mut rb) = (false, false);
        while !(lb && rb) {
            lb = lb || grid.get(x - i, y) != Some(&Tile::WATER);
            rb = rb || grid.get(x + i, y) != Some(&Tile::WATER);
            if !lb {
                grid.remove(x - i, y);
            }
            if !rb {
                grid.remove(x + i, y);
            }
            i += 1;
        }
    }
    grid.cells.retain(|&(_, y), _| y >= bounds[1] && y <= bounds[3]);
    grid.cells.iter().filter(|(_, &t)| t == Tile::WATER).count()
}

fn fill(grid: &mut Grid<Tile>) -> HashSet<(isize, isize)> {
    let bounds = grid.get_bounds();
    let mut drop_points = HashSet::new();
    drop_points.insert((500, 0));
    let mut completed = HashSet::new();
    'main: loop {
        if drop_points.is_empty() {
            return completed;
        }
        let (x, y) = *drop_points.iter().next().unwrap();
        let mut y1 = y;
        while grid.get(x, y1 + 1).is_none() && y1 <= bounds[3] {
            y1 += 1;
        }
        let spilling_on_water = grid.get(x, y1 + 1) == Some(&Tile::WATER);
        if y == y1 || y1 == bounds[3] + 1 {
            completed.insert((x, y));
            drop_points.remove(&(x, y));
            continue;
        }
        let mut finished = false;
        let mut j = y1;
        while !finished {
            let (mut lb, mut rb) = (false, false);
            let mut i = 0;
            while !(lb && rb) {
                if !lb {
                    if grid.get(x - i, j + 1).is_some() {
                        grid.insert(x - i, j, Tile::WATER);
                        lb = grid.get(x - i - 1, j).is_some();  // reached left boundary
                    } else {
                        // in this situation, we are pouring water over an already filled container, remove layer
                        if j == y1 && spilling_on_water {
                            lb = false;
                            rb = false;
                            for k in 0..=i {
                                lb = lb || grid.get(x - k, j) != Some(&Tile::WATER);
                                rb = rb || grid.get(x + k, j) != Some(&Tile::WATER);
                                if !lb {
                                    grid.remove(x - k, j);
                                }
                                if !rb {
                                    grid.remove(x + k, j);
                                }
                            }
                            completed.insert((x, y));
                            drop_points.remove(&(x, y));
                            continue 'main;
                        }
                        if !drop_points.contains(&(x - i, j + 1)) {
                            drop_points.insert((x - i, j));
                        }
                        completed.insert((x, y));
                        drop_points.remove(&(x, y));
                        lb = true;
                        finished = true;
                    }
                }
                if !rb {
                    if grid.get(x + i, j + 1).is_some() {
                        grid.insert(x + i, j, Tile::WATER);
                        rb = grid.get(x + i + 1, j).is_some();  // reached right boundary
                    } else {
                        // in this situation, we are pouring water over an already filled container, remove layer
                        if j == y1 && spilling_on_water {
                            for k in 0..=i {
                                if grid.get(x - k, j) == Some(&Tile::WATER) {
                                    grid.remove(x - k, j);
                                }
                                if grid.get(x + k, j) == Some(&Tile::WATER) {
                                    grid.remove(x + k, j);
                                }
                            }
                            completed.insert((x, y));
                            drop_points.remove(&(x, y));
                            continue 'main;
                        }
                        if !drop_points.contains(&(x + i, j + 1)) {
                            drop_points.insert((x + i, j));
                        }
                        completed.insert((x, y));
                        drop_points.remove(&(x, y));
                        rb = true;
                        finished = true;
                    }
                }
                i += 1;
            }
            j -= 1;
        }
    }
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
    pub fn y18d17test() {
        let s = indoc! {"\
            x=495, y=2..7
            y=7, x=495..501
            x=501, y=3..7
            x=498, y=2..4
            x=506, y=1..2
            x=498, y=10..13
            x=504, y=10..13
            y=13, x=498..504
        "};
        assert_eq!(part1(s), 57);
        assert_eq!(part2(s), 29);
    }
}