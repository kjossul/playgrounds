use crate::grid::Grid;
use std::cell::Cell;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
struct Unit {
    y: isize,
    x: isize,
    hp: isize,
    is_elf: bool,
    atk: isize,
}

impl Unit {
    fn turn(&mut self, mut grid: &mut Grid<bool>, cells: &[Cell<Unit>]) {
        if self.hp <= 0 {
            return;
        }
        let mut paths = Grid::<u32>::new();
        let mut i = 0;
        for c in cells {
            let u = c.get();
            if self.is_elf != u.is_elf && u.hp > 0 {
                paths.insert(u.x, u.y, i);
            }
        }
        loop {
            let mut adjacents = paths.adjacent_coords(self.x, self.y);
            if !adjacents.is_empty() {
                adjacents.sort_by_key(|&(x, y, _)| (y, x));
                if i > 0 {
                    let (x, y, _) = adjacents[0];
                    self.take_step(&mut grid, x, y);
                }
                self.attack(grid, cells);
                break;
            }

            for (&(x, y), &s) in paths.clone().cells.iter() {
                if s == i {
                    for (x1, y1, _) in grid.adjacent_coords(x, y) {
                        if *grid.get(x1, y1).unwrap() {
                            paths.insert(x1, y1, i + 1);
                        }
                    }
                }
            }
//            paths.print(|x| match x {
//                Some(x) => std::char::from_digit(*x as u32, 16).unwrap_or('X'),
//                None => ' ',
//            });
            if i > 100 {
                break;
            }
            i += 1;
        }
    }

    fn attack(&self, grid: &mut Grid<bool>, cells: &[Cell<Unit>]) {
        let mut close_enemies = cells.iter().filter(|c| {
            let u = c.get();
            self.is_elf != u.is_elf && u.hp > 0 && self.distance(&u) == 1
        }).collect::<Vec<_>>();
        if close_enemies.is_empty() {
//            println!("{:?} has no close enemies", &self);
            return;
        }
        close_enemies.sort_by_key(|c| c.get().hp);
        let &mut target;
        if close_enemies.len() > 1 && close_enemies[0].get().hp == close_enemies[1].get().hp &&
            close_enemies[0].get() > close_enemies[1].get() {
            target = *close_enemies.get(1).unwrap();
        } else {
            target = *close_enemies.get(0).unwrap();
        }
//        println!("{:?} attacking {:?}", &self, &target);
        let mut new_enemy = target.get().clone();
        new_enemy.hp -= self.atk;
        if new_enemy.hp <= 0 {
            grid.insert(new_enemy.x, new_enemy.y, true);
        }
        target.set(new_enemy);
    }

    fn take_step(&mut self, grid: &mut Grid<bool>, x: isize, y: isize) {
//        println!("({}, {}) moves to ({}, {})", self.x, self.y, x, y);
        grid.insert(self.x, self.y, true);
        self.x = x;
        self.y = y;
        grid.insert(x, y, false);
    }

    fn distance(&self, other: &Unit) -> isize {
        isize::abs(self.x - other.x) + isize::abs(self.y - other.y)
    }
}

#[aoc(day15, part1)]
pub fn part1(input: &str) -> isize {
    let (mut grid, mut cells) = parser(input);
    let mut i: isize = 0;
    while cells.iter().any(|c| c.get().is_elf && c.get().hp > 0) {
        for c in &cells {
            let mut u = c.get().clone();
            u.turn(&mut grid, &cells);
            c.set(u);
//            println!("{:?}", &cells);
        }
        cells.sort();
        i += 1;
//        println!("{:?}", &cells);
//        grid.print(|x| match x {
//            Some(true) => '.',
//            _ => '#',
//        });
    }
    println!("Last turn: {}, {:?}", i, &cells);
    (i - 1) * cells.iter().map(|c| isize::max(0, c.get().hp)).sum::<isize>()
}

#[aoc(day15, part2)]
pub fn part2(input: &str) -> isize {
    let (grid0, cells0) = parser(input);
    let mut elf_atk = 3;
    loop {
        let mut i: isize = 0;
        let mut grid = grid0.clone();
        let mut cells = cells0.clone();
        loop {
            for c in &cells {
                let mut u = c.get().clone();
                if u.is_elf {
                    u.atk = elf_atk;
                }
                u.turn(&mut grid, &cells);
                c.set(u);
    //            println!("{:?}", &cells);
            }
            if cells.iter().any(|c| c.get().is_elf && c.get().hp <= 0) {
                elf_atk += 1;
                break;
            } else if cells.iter().all(|c| c.get().is_elf || c.get().hp <= 0) {
                return i * cells.iter().map(|c| isize::max(0, c.get().hp)).sum::<isize>()
            }
            cells.sort();
            i += 1;
    //        println!("{:?}", &cells);
    //        grid.print(|x| match x {
    //            Some(true) => '.',
    //            _ => '#',
    //        });
        }
    }
}

fn parser(input: &str) -> (Grid<bool>, Vec<Cell<Unit>>) {
    let mut grid = Grid::new();  // true if cell is free
    let mut units = Vec::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                '#' => continue,
                'G' | 'E' => {
                    units.push(Cell::new(Unit { y: y as isize, x: x as isize, hp: 200, is_elf: c == 'E', atk: 3}));
                },
                _ => {},
            }
            grid.insert(x as isize, y as isize, c == '.');
        }
    }
    (grid, units)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d15test() {
        let m1 = indoc!{"\
            #######
            #E.G#.#
            #.#G..#
            #G.#.G#
            #G..#.#
            #...E.#
            #######
        "};
        let m2 = indoc!{"\
            #######
            #.E...#
            #.#..G#
            #.###.#
            #E#G#G#
            #...#G#
            #######
        "};
        assert_eq!(part1(m1), 27755);
        assert_eq!(part1(m2), 28944);
    }
}