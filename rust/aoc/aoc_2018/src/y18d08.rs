use itertools::Itertools;

#[aoc(day8, part1)]
pub fn part1(input: &str) -> usize {
    let mut ns = parser(input);
    solver1(&mut ns)
}

#[aoc(day8, part2)]
pub fn part2(input: &str) -> usize {
    let mut ns = parser(input);
    solver2(&mut ns)
}

fn solver1(ns: &mut Vec<usize>) -> usize {
    let (children, entries) = ns.drain(..2).next_tuple().unwrap();
    let mut children_sum = 0;
    for _ in 0..children {
        children_sum += solver1(ns);
    }
    children_sum + ns.drain(..entries).sum::<usize>()
}

fn solver2(ns: &mut Vec<usize>) -> usize {
    let (children, entries) = ns.drain(..2).next_tuple().unwrap();
    if children == 0 {
        ns.drain(..entries).sum()
    } else {
        let children = (0..children).map(|_| solver2(ns)).collect::<Vec<_>>();
        let meta = ns.drain(0..entries).collect::<Vec<_>>();
        meta.iter().map(|&i| *children.get(i - 1).unwrap_or(&0)).sum()
    }
}

fn parser(input: &str) -> Vec<usize> {
    input.split_whitespace().flat_map(str::parse).collect()
}