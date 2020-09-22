use crate::grid::{Grid, Direction};

#[aoc_generator(day19)]
pub fn generator(input: &str) -> Vec<Vec<char>> {
    input.lines().map(|l| l.chars().collect::<Vec<char>>()).collect()
}

#[aoc(day19, part1)]
pub fn part1(map: &[Vec<char>]) -> String {
    let (s, _) = solver(map);
    s
}

#[aoc(day19, part2)]
pub fn part2(map: &[Vec<char>]) -> u32 {
    let (_, c) = solver(map);
    c
}

fn solver(map: &[Vec<char>]) -> (String, u32) {
    let mut s = String::new();
    let maze = Grid::from_maze_matrix(map, |&&c| c == ' ');
    let (mut x, mut y) = maze.cells.keys().filter(|(_, y)| *y == 0).next().unwrap();
    let mut direction = Direction::S;
    let mut prev = (x, y);
    let mut counter = 0;
    loop {
        counter += 1;
        let c = *maze.get(x, y).unwrap();
        if c.is_alphabetic() {
            s.push(*c);
        }
        let neighbors = maze.adjacent_coords(x, y);
        if let Some(&(x1, y1, _)) = neighbors.iter().filter(|(_, _, d)| *d == direction).next() {
            // we can continue on the same direction
            prev = (x, y);
            x = x1;
            y = y1;
        } else if let Some(&(x1, y1, d1)) =
        neighbors.iter().filter(|(x, y, _)| (*x, *y) != prev).next() {
            // change of direction
            prev = (x, y);
            x = x1;
            y = y1;
            direction = d1;
        } else {
            // path is finished
            break (s, counter);
        }
    }
}