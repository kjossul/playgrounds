use std::collections::HashMap;

#[aoc(day15, part1)]
pub fn part1(input: &str) -> u32 {
    let ns = parser(input);
    solver(&ns, 2020)
}

#[aoc(day15, part2)]
pub fn part2(input: &str) -> u32 {
    let ns = parser(input);
    solver(&ns, 30000000)
}

fn parser(input: &str) -> Vec<u32> {
    input.split(',').flat_map(str::parse).collect()
}

fn solver(ns: &[u32], end: usize) -> u32 {
    // uses double memory for no reason but don't want to rewrite, problem is meh
    let mut last = *ns.iter().last().unwrap();
    let mut spoken: HashMap<u32, usize> = HashMap::new();
    let mut previous: HashMap<u32, usize> = ns.iter().enumerate().map(|(i, n)| (*n, i + 1)).collect();
    for i in ns.len() + 1..=end {
        if let Some(recent) = spoken.get(&last) {
            let prev = *previous.get(&last).unwrap_or(&i);
            last = recent.saturating_sub(prev) as u32;
        } else {
            last = 0;
        }
        if let Some(prev) = spoken.insert(last, i) {
            previous.insert(last, prev);
        }
    }
    last
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y20d15test() {
        assert_eq!(part1("0,3,6"), 436);
    }
}
