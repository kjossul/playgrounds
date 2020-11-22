use itertools::Itertools;
use crate::grid::{Grid, manh_distance};
use std::collections::{BinaryHeap, HashSet};

#[derive(Copy, Clone)]
enum GeoType {
    MOUTH,
    ROCKY,
    NARROW,
    WET,
    TARGET,
}

#[derive(Ord, Eq, PartialOrd, PartialEq, Copy, Clone, Hash)]
enum State {
    TORCH,
    GEAR,
    NONE,
}

#[aoc(day22, part1)]
pub fn part1(input: &str) -> isize {
    let (grid, _, _, _) = parser(input);
    grid.cells.values().map(|(n, _)| n % 3).sum()
}

#[aoc(day22, part2)]
pub fn part2(input: &str) -> isize {
    let (mut grid, depth, xt, yt) = parser(input);
    let mut heap = BinaryHeap::new();
    heap.push((0, manh_distance(0, 0, xt, yt), 0, 0, State::TORCH));  // -minutes, -distance, x, y, state
    let mut visited = HashSet::new();
    loop {
        let (t, mut d, x, y, s) = heap.pop().unwrap();
        if !visited.insert((x, y, s)) {
            continue;
        }
        let (_, gt) = grid.get(x, y).unwrap();
        match gt {  // swap of gear
            GeoType::MOUTH => {
                heap.push((t - 7, -d, x, y, State::TORCH));
                heap.push((t - 7, -d, x, y, State::GEAR));
                heap.push((t - 7, -d, x, y, State::NONE));
            }
            GeoType::ROCKY => {
                heap.push((t - 7, -d, x, y, State::TORCH));
                heap.push((t - 7, -d, x, y, State::GEAR));
            }
            GeoType::NARROW => {
                heap.push((t - 7, -d, x, y, State::TORCH));
                heap.push((t - 7, -d, x, y, State::NONE));
            }
            GeoType::WET => {
                heap.push((t - 7, -d, x, y, State::GEAR));
                heap.push((t - 7, -d, x, y, State::NONE));
            }
            GeoType::TARGET => {
                if s == State::TORCH {
                    return -t;
                } else {
                    heap.push((t - 7, -d, x, y, State::TORCH));
                }
            }
        }
        for (x1, y1, _) in grid.adjacent_coords_unfiltered(x, y) {
            if x1 < 0 || y1 < 0 || y > depth {
                continue;
            }
            d = manh_distance(x1, y1, xt, yt);
            let mut tup = grid.get(x1, y1);
            if tup.is_none() {
                update_grid(&mut grid, x1, y1, depth);
                tup = grid.get(x1, y1);
            }
            match (tup.unwrap().1, s) { // target terrain, current state
                (GeoType::ROCKY, State::TORCH) | (GeoType::ROCKY, State::GEAR) |
                (GeoType::TARGET, State::TORCH) | (GeoType::TARGET, State::GEAR) |
                (GeoType::WET, State::GEAR) | (GeoType::WET, State::NONE) |
                (GeoType::NARROW, State::TORCH) | (GeoType::NARROW, State::NONE) |
                (GeoType::MOUTH, _) => {
                    heap.push((t - 1, -d, x1, y1, s))
                },
                _ => {}
            }
        }
    }
}

fn parser(input: &str) -> (Grid<(isize, GeoType)>, isize, isize, isize) {
    let ls = input.lines().collect::<Vec<_>>();
    let depth: isize = ls[0].split_whitespace().last().unwrap().parse().unwrap();
    let (tx, ty) = ls[1].split_whitespace().last().unwrap().split(',')
        .flat_map(|s| s.parse()).next_tuple().unwrap();
    let mut grid = Grid::new();
    for y in 0..=ty {
        for x in 0..=tx {
            update_grid(&mut grid, x, y, depth);
        }
    }
    grid.insert(tx, ty, (0, GeoType::TARGET));
    (grid, depth, tx, ty)
}

fn update_grid(grid: &mut Grid<(isize, GeoType)>, x: isize, y: isize, depth: isize) {
    let t = if (x, y) == (0, 0) {
        (0, GeoType::MOUTH)
    } else if y == 0 {
        let geo = x * 16807;
        erosion(geo, depth)
    } else if x == 0 {
        let geo = y * 48271;
        erosion(geo, depth)
    } else {
        if grid.get(x - 1, y).is_none() {
            update_grid(grid, x - 1, y, depth);
        }
        if grid.get(x, y - 1).is_none() {
            update_grid(grid, x, y - 1, depth);
        }
        let (ero1, _) = grid.get(x - 1, y).unwrap();
        let (ero2, _) = grid.get(x, y - 1).unwrap();
        erosion(ero1 * ero2, depth)
    };
    grid.insert(x, y, t);
}

fn erosion(geo: isize, depth: isize) -> (isize, GeoType) {
    let erosion = (geo + depth) % 20183;
    (erosion, get_type(erosion))
}

fn get_type(erosion: isize) -> GeoType {
    match erosion % 3 {
        0 => GeoType::ROCKY,
        1 => GeoType::WET,
        2 => GeoType::NARROW,
        _ => panic!("{}", erosion),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d22test() {
        let s = indoc! {"\
                depth: 510
                target: 10,10
        "};
        assert_eq!(part2(s), 45);
    }
}