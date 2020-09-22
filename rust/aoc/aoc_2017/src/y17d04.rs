use std::collections::HashSet;
use std::iter::FromIterator;

#[aoc_generator(day4)]
pub fn generator(input: &str) -> Vec<Vec<String>> {
    input
        .lines()
        .map(|l| l.trim().split(' ').map(|w| String::from(w)).collect())
        .collect()
}

#[aoc(day4, part1)]
pub fn part1(lines: &[Vec<String>]) -> u32 {
    lines
        .iter()
        .filter(|l| {
            let words: HashSet<String> = HashSet::from_iter(l.iter().cloned());
            l.len() == words.len()
        }).count() as u32
}

#[aoc(day4, part2)]
pub fn part2(lines: &[Vec<String>]) -> u32 {
    lines
        .iter()
        .filter(|l| {
            let words: HashSet<String> = HashSet::from_iter(l.iter().map(|w| {
                let mut v = w.chars().collect::<Vec<char>>();
                v.sort();
                v.into_iter().collect::<String>()
            }));
            l.len() == words.len()
        }).count() as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn y17d04test() {
        assert_eq!(part1(&generator("aa bb cc dd aa")), 0);
        assert_eq!(part1(&generator("aa bb cc dd aaa")), 1);
        assert_eq!(part2(&generator("abcde xyz ecdab")), 0);
        assert_eq!(part2(&generator("iiii oiii ooii oooi oooo")), 1);
    }
}