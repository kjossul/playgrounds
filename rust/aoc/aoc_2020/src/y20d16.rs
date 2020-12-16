use std::collections::HashMap;
use std::collections::HashSet;
use itertools::Itertools;
use regex::Regex;

struct Solver {
    rules: HashMap<String, Range>,
    personal: Vec<u32>,
    nearby: Vec<Vec<u32>>,
}

impl Solver {
    fn part1(&self) -> u32 {
        self.nearby.iter().map(|v| self.ticket_error_rate(v)).sum()
    }

    fn part2(&self) -> u64 {
        let valid_nearby = self.nearby.iter()
            .filter(|v| self.ticket_error_rate(v) == 0)
            .collect::<Vec<_>>();
        let personal_fields = self.possible_fields(&self.personal);
        let mut intersection = valid_nearby.iter()
            .map(|v| self.possible_fields(v))
            .fold(personal_fields, |acc, x| {
                acc.iter().zip(x.iter())
                    .map(|(s1, s2)| s1.intersection(s2).copied().collect()).collect()
            });
        let mut d = HashMap::<&String, usize>::new();
        while d.len() != intersection.len() {
            let (i, s) = intersection.iter_mut().enumerate().find(|(_, s)| s.len() == 1).unwrap();
            let k = s.drain().next().unwrap();
            d.insert(k, i);
            for set in &mut intersection {
                set.remove(k);
            }
        }
        d.iter().filter(|(s, _)| s.contains("departure")).map(|(_s, &i)| {
            self.personal[i] as u64
        }).fold(1, |acc, x| acc * x)
    }

    fn ticket_error_rate(&self, v: &[u32]) -> u32 {
        v.iter().filter(|&&n| !self.rules.values().any(|rule| rule.is_valid(n))).sum()
    }

    fn possible_fields(&self, v: &[u32]) -> Vec<HashSet<&String>> {
        v.iter()
            .map(|&n| self.rules.iter()
                .filter(|(_k, rule)| rule.is_valid(n))
                .map(|(k, _v)| k)
                .collect())
            .collect()
    }
}

struct Range {
    lo1: u32,
    hi1: u32,
    lo2: u32,
    hi2: u32,
}

impl Range {
    fn new(s: &str) -> Self {
        let re = Regex::new(r"\d+").unwrap();
        let (lo1, hi1, lo2, hi2) = re.find_iter(s).flat_map(|m| m.as_str().parse()).next_tuple().unwrap();
        Self { lo1, hi1, lo2, hi2 }
    }

    fn is_valid(&self, n: u32) -> bool {
        (self.lo1 <= n && n <= self.hi1) || (self.lo2 <= n && n <= self.hi2)
    }
}

#[aoc(day16, part1)]
pub fn part1(input: &str) -> u32 {
    let solver = parser(input);
    solver.part1()
}

#[aoc(day16, part2)]
pub fn part2(input: &str) -> u64 {
    let solver = parser(input);
    solver.part2()
}

fn parser(input: &str) -> Solver {
    let (a, b, c) = input.split("\n\n").next_tuple().unwrap();
    let rules = a.lines().map(|l| {
        let (key, v) = l.split(": ").next_tuple().unwrap();
        (key.to_owned(), Range::new(v))
    }).collect();
    let personal = b.lines().skip(1).next().unwrap().split(',').flat_map(str::parse).collect();
    let nearby = c.lines().skip(1).map(|l| {
        l.split(',').flat_map(str::parse).collect()
    }).collect();
    Solver { rules, personal, nearby }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y20d16test() {}
}
