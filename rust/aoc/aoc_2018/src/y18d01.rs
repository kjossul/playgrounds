use std::collections::HashSet;

#[aoc_generator(day1)]
pub fn gen(input: &str) -> Vec<i32> {
    input.lines().flat_map(|i| i.parse()).collect()
}

#[aoc(day1, part1)]
pub fn part1(ns: &Vec<i32>) -> i32 {
    ns.iter().sum()
}

#[aoc(day1, part2)]
pub fn part2(ns: &Vec<i32>) -> i32 {
    let mut set = HashSet::new();
    let mut current = 0;
    for n in ns.into_iter().cycle() {
        if !set.insert(current) {
            return current;
        }
        current += n;
    }
    panic!("Not found!");
}
