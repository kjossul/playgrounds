use regex::Regex;
use std::collections::HashMap;
use petgraph::graphmap::DiGraphMap;
use petgraph::algo::has_path_connecting;

#[aoc(day7, part1)]
pub fn part1(input: &str) -> usize {
    let (map, graph) = parser(input);
    let gold = *map.get(&String::from("shiny gold")).unwrap();
    map.values()
        .filter(|&&n| has_path_connecting(&graph, n, gold, None))
        .count().saturating_sub(1)  // removes the gold node from the count
}

#[aoc(day7, part2)]
pub fn part2(input: &str) -> u32 {
    let (map, graph) = parser(input);
    let mut stack = vec![(1, *map.get(&String::from("shiny gold")).unwrap())];
    let mut count = 0;
    while let Some((mul, bag)) = stack.pop() {
        count += mul;
        for (_, to, &weight) in graph.edges(bag) {
            stack.push((mul * weight, to));
        }
    }
    count - 1
}

pub fn parser(input: &str) -> (HashMap<String, usize>, DiGraphMap<usize, u32>) {
    let mut g = DiGraphMap::new();
    let mut indexes = HashMap::new();
    let re = Regex::new(r"(.*?) bags contain (.*).").unwrap();
    for l in input.lines() {
        let caps = re.captures(l).unwrap();
        let a = String::from(&caps[1]);
        let mut len = indexes.len();
        let a_i = *indexes.entry(a).or_insert(len);
        if &caps[2] != "no other bags" {
            for s in (&caps[2]).split(", ") {
                let chunks = s.split_whitespace().collect::<Vec<_>>();
                let n = (&chunks[0]).parse::<u32>().unwrap();
                let b = format!("{} {}", &chunks[1], &chunks[2]);
                len = indexes.len();
                let b_i = *indexes.entry(b).or_insert(len);
                g.add_edge(a_i, b_i, n);
            }
        }
    }
    (indexes, g)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d07test() {}
}