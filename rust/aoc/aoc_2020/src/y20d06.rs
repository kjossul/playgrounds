use std::collections::HashSet;

#[aoc(day6, part1)]
pub fn part1(input: &str) -> usize {
    let groups = parser(input);
    groups.iter().map(|g| {
        let set = g.chars().filter(|&c| c.is_alphabetic()).collect::<HashSet<_>>();
        set.len()
    }).sum()
}

#[aoc(day6, part2)]
pub fn part2(input: &str) -> usize {
    let groups = parser(input);
    groups.iter().map(|g| {
        let set0 = g.lines().next().unwrap().chars().collect::<HashSet<char>>();
        g.lines()
            .map(|l| l.chars().collect::<HashSet<char>>())
            .fold(set0, |acc, x| acc.intersection(&x).copied().collect::<HashSet<char>>())
            .len()
    }).sum()
}


fn parser(input: &str) -> Vec<String> {
    input.split("\n\n").map(String::from).collect()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d06test() {
    }
}