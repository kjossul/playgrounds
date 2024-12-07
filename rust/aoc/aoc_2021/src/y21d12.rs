use std::collections::HashSet;
use itertools::Itertools;
use petgraph::graphmap::UnGraphMap;
use counter::Counter;

#[aoc(day12, part1)]
pub fn part1(input: &str) -> Option<usize> {
    let graph = parser(input);
    solve(graph, is_valid_path)
}

#[aoc(day12, part2)]
pub fn part2(input: &str) -> Option<usize> {
    let graph = parser(input);
    solve(graph, is_valid_path_2)
}

fn solve(g: UnGraphMap<&str, ()>, f: fn(&Vec<&str>) -> bool) -> Option<usize> {
    let mut stack = vec![vec!["start"]];
    let mut c = 0;
    while let Some(path) = stack.pop() {
        let last = *path.last()?;
        if last.eq("end") {
            c += 1;
            continue;
        }
        stack.extend(g.neighbors(path.last()?).flat_map(|s| {
            let mut new_path = path.clone();
            new_path.push(s);
            if f(&new_path) {
                Some(new_path)
            } else {
                None
            }
        }))
    }
    Some(c)
}

fn is_valid_path(path: &Vec<&str>) -> bool {
    let small_caves: Vec<String> = path.iter()
        .map(|&s| String::from(s))
        .filter(|s| s.to_lowercase().eq(s)).collect();
    small_caves.len() == HashSet::<String>::from_iter(small_caves.into_iter()).len()
}

fn is_valid_path_2(path: &Vec<&str>) -> bool {
    let small_caves: Vec<String> = path.iter()
        .map(|&s| String::from(s))
        .filter(|s| s.to_lowercase().eq(s)).collect();
    let counts: Counter<String> = Counter::from_iter(small_caves.into_iter());
    *counts.get(&String::from("start")).unwrap() == 1 &&
        {
            let most_common = counts.most_common();
            most_common[0].1 <= 2 && (most_common.len() < 2 || most_common[1].1 == 1)
        }
}

fn parser(input: &str) -> UnGraphMap<&str, ()> {
    let edges: Vec<(&str, &str)> = input.lines().map(|l|
        l.split("-").collect_tuple().unwrap()
    ).collect();
    UnGraphMap::from_edges(edges)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d12test() {
        let s = indoc! {"\
            start-A
            start-b
            A-c
            A-b
            b-d
            A-end
            b-end
        "};
        assert!(is_valid_path(&vec!["start", "a", "A", "b", "end"]));
        assert!(!is_valid_path(&vec!["start", "a", "A", "a", "end"]));
        assert_eq!(part1(s), Some(10));
        assert_eq!(part2(s), Some(36));
    }
}