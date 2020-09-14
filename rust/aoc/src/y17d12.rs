use itertools::Itertools;
use petgraph::graphmap::UnGraphMap;
use petgraph::algo::{bellman_ford, connected_components};

#[aoc(day12, part1)]
pub fn part1(input: &str) -> usize {
    let graph = graph_builder(input);
    let (cost, _) = bellman_ford(&graph, "0").unwrap();
    cost.into_iter().filter(|&c| c != f32::INFINITY).count()
}

#[aoc(day12, part2)]
pub fn part2(input: &str) -> usize {
    let graph = graph_builder(input);
    connected_components(&graph)
}

fn graph_builder(input: &str) -> UnGraphMap<&str, f32> {
    let mut graph = UnGraphMap::new();
    for line in input.lines() {
        let (node, r) = line.split(" <-> ").next_tuple().unwrap();
        for other in r.split(", ") {
            graph.add_edge(node, other, 1.0);
        }
    }
    graph
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y17d12test() {
        let s = indoc! {"
            0 <-> 2
            1 <-> 1
            2 <-> 0, 3, 4
            3 <-> 2, 4
            4 <-> 2, 3, 6
            5 <-> 6
            6 <-> 4, 5
        "};
        assert_eq!(part1(s), 6);
        assert_eq!(part2(s), 2);
    }
}