use std::cmp;
use crate::grid::Grid;

#[aoc_generator(day11)]
pub fn gen(input: &str) -> isize {
    input.parse().unwrap()
}

#[aoc(day11, part1)]
pub fn part1(serial: &isize) -> String {
    let mut grid = Grid::new();
    for y in 1..=300 {
        for x in 1..=300 {
            let rack = x + 10;
            let mut level = rack * y;
            level += serial;
            level *= rack;
            level = (level % 1000) / 100 - 5;
            grid.insert(x, y, level);
        }
    }
    let mut best = (0, 0, 0);
    for y in 1..=298 {
        for x in 1..=298 {
            let mut power = 0;
            for i in 0..3 {
                for j in 0..3 {
                    power += grid.get(x + i, y + j).unwrap();
                }
            }
            best = cmp::max(best, (power, x, y));
        }
    }
    format!("{},{}", best.1, best.2)
}

#[aoc(day11, part2)]
pub fn part2(serial: &isize) -> String {
    let mut grid = Grid::new();
    for y in 1..=300 {
        for x in 1..=300 {
            let rack = x + 10;
            let mut level = rack * y;
            level += serial;
            level *= rack;
            level = (level % 1000) / 100 - 5;
            grid.insert(x, y, level);
        }
    }
    let mut best = (0, 0, 0, 0);
    for y in 1..=298 {
        for x in 1..=298 {
            let mut power = *grid.get(x, y).unwrap();
            for size in 2..cmp::min(30, 300 - cmp::max(x, y) - 1) {
//                dbg!(x, y, size);
                for i in x..x + size {
                    power += *grid.get(i, y + size).unwrap();
                }
                for i in y..y + size {
                    power += *grid.get(x + size, i).unwrap();
                }
                power += *grid.get(x + size, y + size).unwrap();
                best = cmp::max(best, (power, x, y, size + 1));
            }
        }
    }
    format!("{},{},{}", best.1, best.2, best.3)

}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub fn y18d11test() {
        assert_eq!(part2(&18), String::from("90,269,16"));
    }
}