#[aoc(day7, part1)]
pub fn part1(input: &str) -> u32 {
    let mut ns = parser(input);
    let median = median(&mut ns);
    ns.into_iter().map(|n| if n < median { median - n } else { n - median }).sum()
}

#[aoc(day7, part2)]
pub fn part2(input: &str) -> u32 {
    let mut ns = parser(input);
    ns.sort();
    (*ns.first().unwrap()..*ns.last().unwrap()).map(|n| {
        ns.iter().map(|&m| {
            let gap = if n < m { m - n } else { n - m };
            sum_of_first_naturals(gap)
        }).sum::<u32>()
    }).min().unwrap().clone()
}

fn median(ns: &mut [u32]) -> u32 {
    ns.sort();
    ns[ns.len() / 2]
}

fn sum_of_first_naturals(n: u32) -> u32 {
    n * (n + 1) / 2
}

fn parser(input: &str) -> Vec<u32> {
    input.split(",").flat_map(str::parse).collect()
}
