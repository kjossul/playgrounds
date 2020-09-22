use std::collections::HashMap;
use std::collections::HashSet;
use petgraph::Graph;
use petgraph::Direction::{Incoming, Outgoing};
use petgraph::graph::NodeIndex;
use petgraph::graphmap::DiGraphMap;
use regex::Regex;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug)]
struct Program<'a> {
    name: &'a str,
    weight: i32,
    children_weight: i32,
}


#[aoc(day7, part1)]
pub fn part1(input: &str) -> String {
    let tree = parser(input);
    let root = tree[tree.externals(Incoming).next().unwrap()];
    String::from(root.name)
}

#[aoc(day7, part2)]
pub fn part2(input: &str) -> i32 {
    let mut tree = parser(input);
    let mut frontier = tree.externals(Outgoing).collect::<HashSet<NodeIndex>>();
    loop {
        let mut new_frontier = HashSet::<NodeIndex>::new();
        let mut weights = HashMap::<NodeIndex, HashMap<i32, i32>>::new();
        for &child in &frontier {
            match tree.neighbors_directed(child, Incoming).next() {
                Some(parent) => {
                    let child_weight = tree[child].weight + tree[child].children_weight;
                    tree.node_weight_mut(parent).unwrap().children_weight += child_weight;
                    *weights.entry(parent).or_insert(HashMap::new())
                        .entry(child_weight).or_insert(0) += 1;
                    new_frontier.insert(parent);
                },
                None => panic!("Root reached."),
            };
        }
        for (_, weight) in weights.iter_mut() {
            if weight.len() > 1 {
                let (mut norm, mut outlier) = (0, 0);
                for (k, v) in weight.drain() {
                    if v == 1 {
                        outlier = k;
                    } else {
                        norm = k;
                    }
                }
                for &ni in &frontier {
                    let program = tree[ni];
                    let fixed = program.weight + norm - outlier;
                    if program.weight + program.children_weight == outlier && fixed > 0 {
                        return fixed;
                    }
                }
            }
        }
        frontier = new_frontier;
    }
}

fn parser(input: &str) -> Graph<Program, ()> {
    let mut tree = DiGraphMap::new();
    let mut weights = HashMap::<&str, i32>::new();
    let re = Regex::new(r"(\w+) \((\d+)\)(?: -> (.*))?").unwrap();
    for line in input.lines() {
        let groups = re.captures(line).unwrap();
        let name = groups.get(1).unwrap().as_str();
        let weight = groups.get(2).unwrap().as_str().parse().unwrap();
        weights.insert(name, weight);
        match groups.get(3) {
            Some(m) => {
                for other in m.as_str().split(", ") {
                    tree.add_edge(name, other, ());
                }
            }
            None => {}
        }
    }
    DiGraphMap::<_, ()>::from_edges(tree.all_edges().map(|(n1, n2, _)| {
        (Program { name: n1, weight: *weights.get(n1).unwrap(), children_weight: 0 },
         Program { name: n2, weight: *weights.get(n2).unwrap(), children_weight: 0 })
    })).into_graph()
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn y17d07test() {
        let s = indoc! {"
            pbga (66)
            xhth (57)
            ebii (61)
            havc (66)
            ktlj (57)
            fwft (72) -> ktlj, cntj, xhth
            qoyq (66)
            padx (45) -> pbga, havc, qoyq
            tknk (41) -> ugml, padx, fwft
            jptl (61)
            ugml (68) -> gyxo, ebii, jptl
            gyxo (61)
            cntj (57)
        "};
        assert_eq!(part1(s), "tknk");
        assert_eq!(part2(s), 60);
    }
}