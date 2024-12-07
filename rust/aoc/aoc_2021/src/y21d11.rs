use itertools::Itertools;
use crate::grid::Grid;

struct Octopus {
    energy: u8,
    flashed: bool,
}

impl Octopus {
    pub fn increase_energy(&mut self) {
        self.energy += 1;
        if self.energy > 9 {
            self.flashed = true;
        }
    }

    pub fn reset_flash(&mut self) {
        if self.flashed {
            self.energy = 0;
            self.flashed = false;
        }
    }
}

#[aoc(day11, part1)]
pub fn part1(input: &str) -> Option<usize> {
    let mut grid = parser(input);
    Some((0..100).map(|_| {
        grid.do_step()
    }).sum())
}

#[aoc(day11, part2)]
pub fn part2(input: &str) -> Option<usize> {
    let mut grid = parser(input);
    for i in 1.. {
        if grid.do_step() == grid.cells.len() {
            return Some(i);
        }
    }
    panic!()
}

impl Grid<Octopus> {
    fn do_step(&mut self) -> usize {
        // 1. Increase all energies by one
        for (_, o) in self.cells.iter_mut() {
            o.increase_energy();
        }
        let mut visited: Vec<(isize, isize)> = vec![];
        let mut flashing: Vec<(isize, isize)> = self.cells.iter()
            .filter(|(&(x, y), o)| o.flashed && !visited.contains(&(x, y)))
            .map(|(&(x, y), _)| (x, y))
            .collect();
        let mut flashes = 0;
        while !flashing.is_empty() {
            flashes += flashing.len();
            for (x, y) in flashing.drain(..) {
                visited.push((x, y));
                let deltas = [-1, 0, 1];
                for (i, j) in deltas.iter().cartesian_product(deltas.iter())
                    .filter(|(&i, &j)| (i, j) != (0, 0)) {
                    if let Some(o) = self.get_mut(x + i, y + j) {
                        o.increase_energy();
                    }
                }
            }
            flashing = self.cells.iter()
                .filter(|(&(x, y), o)| o.flashed && !visited.contains(&(x, y)))
                .map(|(&(x, y), _)| (x, y))
                .collect();
        }
        for (_, o) in self.cells.iter_mut() {
            o.reset_flash();
        }
        flashes
    }
}

fn parser(input: &str) -> Grid<Octopus> {
    let m: Vec<Vec<Octopus>> = input.lines().map(|l| {
        l.chars().map(|c|
            Octopus { energy: c.to_digit(10).unwrap() as u8, flashed: false }
        ).collect()
    }).collect();
    Grid::from_maze_matrix(m, |_| false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d11test() {
        let s = indoc! {"\
            5483143223
            2745854711
            5264556173
            6141336146
            6357385478
            4167524645
            2176841721
            6882881134
            4846848554
            5283751526
        "};

        let small = indoc! {"\
            11111
            19991
            19191
            19991
            11111
        "};
        let mut grid = parser(small);
        let score: usize = (0..2).map(|_| grid.do_step()).sum();

        assert_eq!(score, 9);
        assert_eq!(part1(s), Some(1656));
    }
}