use std::collections::{HashSet, BinaryHeap};
use crate::grid::{Grid, Direction};

#[derive(Eq, PartialEq, Hash)]
struct Room {
    neighbors: Vec<Direction>,
}

#[derive(Clone, Debug)]
enum Instruction {
    SINGLE(Direction),
    MULTI(Vec<Instruction>),
    CHOICE(Vec<Instruction>),
}

impl Room {
    fn new() -> Self { Self { neighbors: Vec::new() } }
}

#[aoc(day20, part1)]
pub fn part1(input: &str) -> isize {
    let directions = parser(&mut input.chars());
    let mut grid = Grid::<Room>::new();
    let (x, y) = (0, 0);
    grid.insert(x, y, Room::new());
    explorer(&mut grid, directions, x, y);
    let mut discovered = HashSet::new();
    let mut heap = BinaryHeap::new();
    heap.push((0, 0, 0));
    loop {
        let (c, x, y) = heap.pop().unwrap();
        for &d in &grid.get(x, y).unwrap().neighbors {
            let t = grid.get_adjacent_coord(x, y, d);
            if !discovered.contains(&t) {
                heap.push((c - 1, t.0, t.1));
            }
        }
        discovered.insert((x, y));
        if discovered.len() == grid.cells.len() {
            return -c
        }
    }
}
#[aoc(day20, part2)]
pub fn part2(input: &str) -> isize {
    let directions = parser(&mut input.chars());
    let mut grid = Grid::<Room>::new();
    let (x, y) = (0, 0);
    grid.insert(x, y, Room::new());
    explorer(&mut grid, directions, x, y);
    let mut discovered = HashSet::new();
    let mut heap = BinaryHeap::new();
    heap.push((0, 0, 0));
    let mut distant_doors = 0;
    loop {
        let (c, x, y) = heap.pop().unwrap();
        if discovered.contains(&(x, y)) {
            continue;
        } else if c <= -1000 {
            distant_doors += 1;
        }
        for &d in &grid.get(x, y).unwrap().neighbors {
            let t = grid.get_adjacent_coord(x, y, d);
            if !discovered.contains(&t) {
                heap.push((c - 1, t.0, t.1));
            }
        }
        discovered.insert((x, y));
        if discovered.len() == grid.cells.len() {
            return distant_doors
        }
    }
}

fn parser<I>(chars: &mut I) -> Instruction
    where I: Iterator<Item=char> {
    let mut v = Vec::new();
    let mut choices = Vec::new();
    loop {
        match chars.next() {
            Some('^') => {}
            Some('$') | Some(')') | None => break,
            Some('(') => {
                v.push(parser(chars));
            }
            Some('|') => {
                choices.push(Instruction::MULTI(v.clone()));
                v.clear();
            }
            Some('E') => { v.push(Instruction::SINGLE(Direction::E)) }
            Some('N') => { v.push(Instruction::SINGLE(Direction::N)) }
            Some('W') => { v.push(Instruction::SINGLE(Direction::W)) }
            Some('S') => { v.push(Instruction::SINGLE(Direction::S)) }
            _ => panic!(),
        }
    }
    if choices.len() > 0 {
        choices.push(Instruction::MULTI(v));
        Instruction::CHOICE(choices)
    } else {
        Instruction::MULTI(v)
    }
}

fn explorer(grid: &mut Grid<Room>, instructions: Instruction, mut x: isize, mut y: isize) {
    if let Instruction::MULTI(v) = instructions {
        for i in v {
            match i {
                Instruction::SINGLE(d) => {
                    let (x1, y1) = grid.get_adjacent_coord(x, y, d);
                    let neighbor = grid.cells.entry((x1, y1)).or_insert(Room::new());
                    neighbor.neighbors.push(Direction::reverse(d));
                    grid.cells.get_mut(&(x, y)).unwrap().neighbors.push(d);
                    x = x1;
                    y = y1;
                },
                Instruction::CHOICE(inner) => {
                    for j in inner {
                        explorer(grid, j, x, y);
                    }
                }
                Instruction::MULTI(_) => panic!(),  // this should only be top level
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y18d20test() {
        assert_eq!(part1("^WNE$"), 3);
        assert_eq!(part1("^^ENWWW(NEEE|SSE(EE|N))$"), 10);
    }
}