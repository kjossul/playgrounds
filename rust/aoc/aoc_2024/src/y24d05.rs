use itertools::Itertools;
use petgraph::{algo::toposort, prelude::DiGraphMap};

#[aoc(day5, part1)]
pub fn part1(input: &str) -> u32 {
    let (rules, updates) = parser(input);
    updates.iter().filter(|&update| is_ordered(&rules, update))
    .map(|v| {
        v[v.len() / 2]
    }).sum()
}

#[aoc(day5, part2)]
pub fn part2(input: &str) -> u32 {
    let (rules, updates) = parser(input);
    updates.iter().filter(|&update| !is_ordered(&rules, update))
    .map(|update| {
        let subrules = subgraph(&rules, update);
        let ordered = toposort(&subrules, None).unwrap();
        ordered[ordered.len() / 2]
    }).sum()
}

fn parser(input: &str) -> (DiGraphMap<u32, ()>, Vec<Vec<u32>>) {
    let s: Vec<&str> = input.split("\n\n").collect();
    let edges: Vec<(u32, u32)> = s[0].lines().flat_map(|l| l.split('|').flat_map(str::parse).collect_tuple()).collect();
    let g = DiGraphMap::<u32, ()>::from_edges(&edges);
    let v: Vec<Vec<u32>> = s[1].lines().map(|l| l.split(',').flat_map(str::parse).collect()).collect();
    (g, v)
}

fn is_ordered(g: &DiGraphMap<u32, ()>, update: &Vec<u32>) -> bool {
    update.iter().zip(update.iter().skip(1)).all(|(&a, &b)| g.contains_edge(a, b))
}

fn subgraph(g: &DiGraphMap<u32, ()>, update: &Vec<u32>) -> DiGraphMap<u32, ()> {
    DiGraphMap::from_edges(g.all_edges()
    .filter(|(a, b, _)| update.contains(a) && update.contains(b)))
}