use std::collections::{HashMap, HashSet};
use itertools::Itertools;
use crate::grid::Grid;
use counter::Counter;

#[aoc(day6, part1)]
pub fn part1(input: &str) -> usize {
    let mut points = HashMap::new();
    let mut grid = Grid::new();
    let (mut minx, mut maxx, mut miny, mut maxy) = (isize::MAX, isize::MIN, isize::MAX, isize::MIN);
    for (i, line) in input.lines().enumerate() {
        let (x, y) = line.split(", ").flat_map(str::parse).next_tuple().unwrap();
        minx = std::cmp::min(minx, x);
        miny = std::cmp::min(miny, y);
        maxx = std::cmp::max(maxx, x);
        maxy = std::cmp::max(maxy, y);
        points.insert((x, y), i);
    }
    for y in miny..=maxy {
        for x in minx..=maxx {
            let mut distances = points.iter().map(|((xp, yp), i)|
                (isize::abs(x - xp) + isize::abs(y - yp), i)
            ).collect::<Vec<_>>();
            distances.sort();
            if distances[0].0 != distances[1].0 {
                grid.insert(x, y, *distances[0].1);
            }
        }
    }
    let (minx, miny, maxx, maxy) = grid.get_bounds().into_iter().next_tuple().unwrap();
    let excluded = grid.cells.iter().filter(|&((x, y), _)|
        (*x == minx || *x == maxx) || (*y == miny || *y == maxy)
    ).map(|(_, &i)| i).collect::<HashSet<usize>>();
    let counter = grid.cells.values().collect::<Counter<_>>();
    counter.most_common().iter().find(|&(k, _)| !excluded.contains(k)).unwrap().1
}

#[aoc(day6, part2)]
pub fn part2(input: &str) -> usize {
    let mut points = Vec::new();
    let mut count = 0;
    let (mut minx, mut maxx, mut miny, mut maxy) = (isize::MAX, isize::MIN, isize::MAX, isize::MIN);
    for line in input.lines() {
        let (x, y) = line.split(", ").flat_map(str::parse).next_tuple().unwrap();
        minx = std::cmp::min(minx, x);
        miny = std::cmp::min(miny, y);
        maxx = std::cmp::max(maxx, x);
        maxy = std::cmp::max(maxy, y);
        points.push((x, y));
    }
    for y in miny..=maxy {
        for x in minx..=maxx {
            let total_distance: isize = points.iter().map(|(xp, yp)|
                isize::abs(x - xp) + isize::abs(y - yp)
            ).sum();
            if total_distance < 10000 {
                count += 1;
            }
        }
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d06test() {
        let s = indoc! {"\
            1, 1
            1, 6
            8, 3
            3, 4
            5, 5
            8, 9
        "};
        assert_eq!(part1(s), 17);
    }
}