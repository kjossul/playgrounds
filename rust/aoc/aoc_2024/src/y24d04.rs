use std::collections::HashSet;

use pathfinding::matrix::*;

#[aoc(day4, part1)]
pub fn part1(input: &str) -> usize {
    let m = parser(input);
    m.items()
    .filter(|(_, &c)| c == 'X')
    .map(|(start, _)| {
        directions::DIRECTIONS_8.iter()
        .map(|&direction| m.in_direction(start, direction).collect::<Vec<_>>())
        .filter(|ds| {
            ds.len() > 2 && m.get(ds[0]) == Some(&'M') && m.get(ds[1]) == Some(&'A') && m.get(ds[2]) == Some(&'S') 
        }).count()
    }).sum()
}

#[aoc(day4, part2)]
pub fn part2(input: &str) -> usize {
    let m = parser(input);
    m.items()
    .filter(|(_, &c)| c == 'A')
    .filter(|(start, _)| {
        let x: Vec<_> = [directions::NE, directions::SW, directions::NW, directions::SE].iter()
        .map(|&d| m.move_in_direction(*start, d).and_then(|t| m.get(t).cloned()))
        .collect();
        let target = HashSet::from([Some('M'), Some('S')]);
        target == HashSet::from_iter(x.iter().take(2).cloned()) && 
        target == HashSet::from_iter(x.iter().skip(2).cloned())
    }).count()
}

fn parser(input: &str) -> Matrix<char> {
    Matrix::from_rows(input.lines().map(|l| l.chars())).unwrap()
}