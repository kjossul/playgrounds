use regex::Regex;
use std::iter::zip;

#[aoc(day1, part1)]
pub fn part1(input: &str) -> u32 {
    let (mut a, mut b) = parser(input);
    a.sort();
    b.sort();
    zip(a, b).map(|(x, y)| x.abs_diff(y)).sum()
}

#[aoc(day1, part2)]
pub fn part2(input: &str) -> u32 {
    let (a, b) = parser(input);
    a.iter().map(|x| b.iter().filter(|&y| x == y).count() as u32 * x).sum()
}

fn parser(input: &str) -> (Vec<u32>, Vec<u32>) {
    let re = Regex::new(r"(\d+)   (\d+)").unwrap();
    let (mut a, mut b) = (vec![], vec![]);
    for (_, [x, y]) in re.captures_iter(input).map(|c| c.extract()) {
        a.push(x.parse().unwrap());
        b.push(y.parse().unwrap());
    }
    (a, b)
}