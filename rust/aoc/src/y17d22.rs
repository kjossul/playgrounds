use crate::grid::{Grid, Direction};

#[aoc(day22, part1)]
pub fn part1(input: &str) -> usize {
    let mut count = 0;
    let mut grid = Grid::from_maze_matrix(input.lines().map(|l| l.chars()), |_| false);
    let dim = (f32::sqrt(grid.cells.len() as f32) / 2.0) as isize;
    let mut pos = (dim, dim);
    let mut c = *grid.cells.get(&pos).unwrap();
    let mut direction = Direction::N;
    for _ in 0..10000 {
        if c == '#' {
            grid.cells.insert(pos, '.');
            direction = Direction::turn_right(direction);
        } else {
            grid.cells.insert(pos, '#');
            direction = Direction::turn_left(direction);
            count += 1;
        }
        pos = grid.get_adjacent_coord(pos.0, pos.1, direction);
        c = *grid.cells.entry(pos).or_insert('.');
    }
    count
}


#[aoc(day22, part2)]
pub fn part2(input: &str) -> usize {
    let mut count = 0;
    let mut grid = Grid::from_maze_matrix(input.lines().map(|l| l.chars()), |_| false);
    let dim = (f32::sqrt(grid.cells.len() as f32) / 2.0) as isize;
    let mut pos = (dim, dim);
    let mut c = *grid.cells.get(&pos).unwrap();
    let mut direction = Direction::N;
    for _ in 0..10000000 {
        match c {
            '#' => {
                grid.cells.insert(pos, 'F');
                direction = Direction::turn_right(direction);
            },
            '.' => {
                grid.cells.insert(pos, 'W');
                direction = Direction::turn_left(direction);
            },
            'W' => {
                grid.cells.insert(pos, '#');
                count += 1;
            },
            'F' => {
                grid.cells.insert(pos, '.');
                direction = Direction::reverse(direction);
            },
            _ => {panic!("woops");}
        }
        pos = grid.get_adjacent_coord(pos.0, pos.1, direction);
        c = *grid.cells.entry(pos).or_insert('.');
    }
    count
}