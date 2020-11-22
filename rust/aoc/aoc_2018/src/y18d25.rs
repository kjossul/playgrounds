use petgraph::graphmap::UnGraphMap;
use petgraph::algo::connected_components;


#[aoc(day25, part1)]
pub fn part1(input: &str) -> usize {
    let stars = parser(input);
    let mut map = UnGraphMap::new();
    for s1 in &stars {
        for s2 in &stars {
            if is_close(s1, s2) {
                map.add_edge(s1, s2, 1);
            }
        }
    }
    connected_components(&map)
}

fn parser(input: &str) -> Vec<Vec<isize>> {
    input.lines().map(|l| l.split(',').flat_map(|s| s.parse()).collect()).collect()
}

fn is_close(s1: &[isize], s2: &[isize]) -> bool {
    s1.iter().zip(s2.iter()).map(|(n, m)| isize::abs(n - m)).sum::<isize>() <= 3
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d25test() {
        let s1 = indoc! {"\
            0,0,0,0
            3,0,0,0
            0,3,0,0
            0,0,3,0
            0,0,0,3
            0,0,0,6
            9,0,0,0
            12,0,0,0
        "};
        assert_eq!(part1(s1), 2);
    }
}