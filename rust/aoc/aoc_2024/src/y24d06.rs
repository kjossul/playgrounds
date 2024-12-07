use std::collections::HashSet;

use pathfinding::matrix::{directions::{E, N, S, W}, Matrix};

#[aoc_generator(day6)]
fn parser(input: &str) -> Matrix<char> {
    Matrix::from_rows(input.lines().map(|l| l.chars())).unwrap()
}

#[aoc(day6, part1)]
pub fn part1(m: &Matrix<char>) -> Option<usize> {
    let visited = patrol(m)?;
    Some(visited.len())
}

#[aoc(day6, part2)]
pub fn part2(m: &Matrix<char>) -> Option<usize> {
    let visited = patrol(m)?;
    Some(visited.iter().filter(|&&pos| {
        let mut m1 = m.clone();
        *m1.get_mut(pos).unwrap() = '#';
        part1(&m1).is_none()
    }).count() - 1)
}

fn patrol(m: &Matrix<char>) -> Option<HashSet<(usize, usize)>> {
    let (mut pos, _) = m.items().filter(|(_, &c)| c == '^').next()?;
    let mut direction = N;
    let mut visited = HashSet::from([pos]);
    let mut visited_with_direction = HashSet::from([(pos, N)]);
    while let Some(next_pos) = m.move_in_direction(pos, direction) {
        let target = m.get(next_pos)?;
        if *target == '#' {
            direction = rotate_direction_cw(direction);
        } else {
            pos = next_pos;
            visited.insert(pos);
        }
        if !visited_with_direction.insert((pos, direction)) {
            return None
        }
    }
    Some(visited)
}

fn rotate_direction_cw(direction: (isize, isize)) -> (isize, isize) {
    match direction {
        E => S,
        S => W,
        W => N,
        N => E,
        _ => direction
    }
}

