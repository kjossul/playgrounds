use std::collections::HashMap;
use counter::Counter;
use itertools::Itertools;

#[aoc(day14, part1)]
pub fn part1(input: &str) -> Option<usize> {
    solve(input, 10)
}


#[aoc(day14, part2)]
pub fn part2(input: &str) -> Option<usize> {
    solve(input, 40)
}

fn solve(input: &str, n: usize) -> Option<usize> {
    let (polymer, rules) = parser(input);
    let mut pairs: Counter<(char, char)> = Counter::from_iter(
        polymer.iter().zip(polymer[1..].iter()).map(|(&c1, &c2)| (c1, c2))
    );
    let mut chars: Counter<char> = Counter::from_iter(polymer.into_iter());
    for _ in 0..n {
        for ((a, b), c) in pairs.clone().into_iter() {
            let x = *rules.get(&(*a, *b))?;
            pairs.entry((*a, *b)).and_modify(|n| *n -= c).or_default();
            pairs.entry((*a, x)).and_modify(|n| *n += c).or_insert(*c);
            pairs.entry((x, *b)).and_modify(|n| *n += c).or_insert(*c);
            chars.entry(x).and_modify(|n| *n += c).or_insert(*c);
        }
    }
    let freqs = chars.most_common();
    Some(freqs.first()?.1 - freqs.last()?.1)
}

fn parser(input: &str) -> (Vec<char>, HashMap<(char, char), char>) {
    let (template, rules) = input.split("\n\n").next_tuple().unwrap();
    (
        template.chars().collect(),
        rules.lines().map(|l| {
            let v: Vec<char> = l.chars().collect();
            ((v[0], v[1]), v[6])
        }).collect()
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d14test() {
        let s = indoc!{"\
            NNCB

            CH -> B
            HH -> N
            CB -> H
            NH -> C
            HB -> C
            HC -> B
            HN -> C
            NN -> C
            BH -> H
            NC -> B
            NB -> B
            BN -> B
            BB -> N
            BC -> B
            CC -> N
            CN -> C
        "};

        assert_eq!(part1(s), Some(1588));
    }
}