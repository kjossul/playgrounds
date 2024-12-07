use std::iter::repeat_n;
use itertools::Itertools;

type Equation = (u64, Vec<u64>);

#[aoc_generator(day7)]
pub fn parser(input: &str) -> Vec<Equation> {
    input.lines().map(|l| line_parser(l)).collect()
}

fn line_parser(input: &str) -> Equation {
    let (s1, s2) = input.split(": ").collect_tuple().unwrap();
    (s1.parse().unwrap(), s2.split(' ').flat_map(str::parse).collect())
}

#[aoc(day7, part1)]
pub fn part1(equations: &Vec<Equation>) -> Option<u64> {
    Some(equations.iter()
    .filter(|eq| permutations_with_replacement(vec!['+', '*'], eq.1.len()-1).any(|ops| is_true(eq, &ops)))
    .map(|eq| eq.0)
    .sum())
}

#[aoc(day7, part2)]
pub fn part2(equations: &Vec<Equation>) -> Option<u64> {
    Some(equations.iter()
    .filter(|eq| permutations_with_replacement(vec!['+', '*', '|'], eq.1.len()-1).any(|ops| is_true(eq, &ops)))
    .map(|eq| eq.0)
    .sum())
}

fn permutations_with_replacement(ops: Vec<char>, k: usize) -> impl Iterator<Item = Vec<char>> {
    repeat_n(ops, k).multi_cartesian_product()
}

fn is_true(eq: &Equation, ops: &Vec<char>) -> bool {
    let target = eq.0;
    let mut eq = eq.1.clone();
    let init = eq.remove(0);
    let result = eq.iter().zip(ops).fold(init, |acc, (n, op)| {
        match *op {
            '+' => acc + n,
            '*' => acc * n,
            '|' => format!("{acc}{n}").parse().unwrap(),
            _ => panic!()
        }
    });
    target == result
}