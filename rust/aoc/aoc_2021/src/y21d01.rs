use itertools::Itertools;

#[aoc(day1, part1)]
pub fn part1(input: &str) -> usize {
    let ns = parser(input);
    ns.into_iter().tuple_windows().filter(|(x, y)| x < y).count()
}

#[aoc(day1, part2)]
pub fn part2(input: &str) -> usize {
    let ns = parser(input);
    ns.into_iter().tuple_windows().map(|(x, y, z)| x + y + z)
        .tuple_windows().filter(|(x, y)| x < y).count()
}

fn parser(input: &str) -> Vec<u32> {
    input.lines().flat_map(str::parse).collect()
}