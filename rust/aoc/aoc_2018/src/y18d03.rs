use std::collections::HashSet;
use regex::Regex;
use crate::grid::Grid;

#[aoc_generator(day3)]
pub fn gen(input: &str) -> Grid<HashSet<isize>> {
    let mut grid = Grid::new();
    let re = Regex::new(r"#(\d+).*?(\d+),(\d+): (\d+)x(\d+)").unwrap();
    for line in input.lines() {
        let groups = re.captures(line).unwrap();
        let ns = (1..groups.len()).flat_map(|i| groups[i].parse::<isize>()).collect::<Vec<_>>();
        for y in ns[2]..ns[2] + ns[4] {
            for x in ns[1]..ns[1] + ns[3] {
                grid.cells.entry((x, y)).or_insert(HashSet::new()).insert(ns[0]);
            }
        }
    }
    grid
}

#[aoc(day3, part1)]
pub fn part1(grid: &Grid<HashSet<isize>>) -> usize {
    grid.cells.values().filter(|&set| set.len() > 1).count()
}

#[aoc(day3, part2)]
pub fn part2(grid: &Grid<HashSet<isize>>) -> isize {
    let mut overlapping = HashSet::<isize>::new();
    for set in grid.cells.values() {
        if set.len() > 1 {
            for &id in set.iter() {
                overlapping.insert(id);
            }
        }
    }
    (1..).find(|d| !overlapping.contains(d)).unwrap()
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d03test() {
        let s = indoc! {"
            #1 @ 1,3: 4x4
            #2 @ 3,1: 4x4
            #3 @ 5,5: 2x2
        "};
        assert_eq!(part1(&gen(s)), 5);
    }
}