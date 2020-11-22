use crate::grid::Grid;

#[derive(Eq, PartialEq, Clone, Copy)]
enum Tile {
    OPEN,
    TREE,
    YARD,
}

#[aoc(day18, part1)]
pub fn part1(input: &str) -> usize {
    let mut grid = parser(input);
    for _ in 0..10 {
        grid = f(grid);
    }
    score(&grid)
}

#[aoc(day18, part2)]
pub fn part2(input: &str) -> usize {
    let mut grid = parser(input);
    let mut tortoise = f(grid.clone());
    let mut hare = f(f(grid.clone()));
    while tortoise != hare {
        tortoise = f(tortoise);
        hare = f(f(hare));
    }
    let mut mu = 0;
    tortoise = grid.clone();
    while tortoise != hare {
        tortoise = f(tortoise);
        hare = f(hare);
        mu += 1;
    }
    let mut lambda = 1;
    hare = f(tortoise.clone());
    while tortoise != hare {
        hare = f(hare);
        lambda += 1;
    }
    for _ in 0..mu + (1000000000 - mu) % lambda {
        grid = f(grid);
    }
    score(&grid)
}

fn score(grid: &Grid<Tile>) -> usize {
    grid.cells.values().filter(|&&t| t == Tile::TREE).count() * grid.cells.values().filter(|&&t| t == Tile::YARD).count()
}

fn f(grid: Grid<Tile>) -> Grid<Tile> {
    let mut new_grid = grid.clone();
    for (&(x, y), v) in new_grid.cells.iter_mut() {
        let neighbors = grid.neighbors(x, y);
        match *v {
            Tile::OPEN => if neighbors.iter().filter(|&&&t| t == Tile::TREE).count() >= 3 {
                *v = Tile::TREE;
            },
            Tile::TREE => if neighbors.iter().filter(|&&&t| t == Tile::YARD).count() >= 3 {
                *v = Tile::YARD;
            },
            Tile::YARD => if neighbors.iter().filter(|&&&t| t == Tile::TREE).count() == 0 ||
                neighbors.iter().filter(|&&&t| t == Tile::YARD).count() == 0 {
                *v = Tile::OPEN;
            },
        }
    }
    new_grid
}

fn parser(input: &str) -> Grid<Tile> {
    let v: Vec<Vec<Tile>> = input.lines().map(|l| l.chars().map(|c| {
        match c {
            '.' => Tile::OPEN,
            '|' => Tile::TREE,
            '#' => Tile::YARD,
            _ => panic!(),
        }
    }).collect()).collect();
    Grid::from_maze_matrix(v, |_| false)
}