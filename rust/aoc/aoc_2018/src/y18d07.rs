use std::collections::HashMap;
use petgraph::Direction::Incoming;
use petgraph::graphmap::DiGraphMap;


#[aoc(day7, part1)]
pub fn part1(input: &str) -> String {
    let mut graph = parser(input);
    let mut s = String::new();
    let len = graph.node_count();
    while s.len() < len {
        let c = (65..=65 + len).map(|i| char::from(i as u8))
            .find(|&c| {
                graph.contains_node(c) && graph.neighbors_directed(c, Incoming).next() == None
            }).unwrap();
        graph.remove_node(c);
        s.push(c);
    }
    s
}

#[aoc(day7, part2)]
pub fn part2(input: &str) -> usize {
    let mut graph = parser(input);
    let len = graph.node_count() as u8;
    let mut minute = 0;
    let mut active = HashMap::new();
    loop {
        let free = (65..=65 + len).map(char::from)
            .filter(|&c| {
                graph.contains_node(c) && graph.neighbors_directed(c, Incoming).next() == None &&
                    !active.contains_key(&c)
            }).collect::<Vec<_>>();
        if free.is_empty() && active.is_empty() {
            break minute;
        }
        for &c in free.iter().take(5 - active.len()) {
            active.insert(c, c as u8 - 4);
        }
        for k in active.clone().keys() {
            let v = active.get_mut(k).unwrap();
            *v -= 1;
            if *v == 0 {
                active.remove(k);
                graph.remove_node(*k);
            }
        }
        minute += 1;
    }
}

pub fn parser(input: &str) -> DiGraphMap<char, ()> {
    let mut graph = DiGraphMap::new();
    for line in input.lines() {
        let words = line.split(" ").collect::<Vec<_>>();
        let l = words[1].chars().next().unwrap();
        let r = words[7].chars().next().unwrap();
        graph.add_edge(l, r, ());
    };
    graph
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d07test() {
        let s = indoc! {"\
        Step C must be finished before step A can begin.
        Step C must be finished before step F can begin.
        Step A must be finished before step B can begin.
        Step A must be finished before step D can begin.
        Step B must be finished before step E can begin.
        Step D must be finished before step E can begin.
        Step F must be finished before step E can begin.
        "};
        assert_eq!(part1(s), String::from("CABDFE"));
        assert_eq!(part2(s), 253);
    }
}