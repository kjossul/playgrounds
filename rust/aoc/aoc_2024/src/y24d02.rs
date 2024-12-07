#[aoc(day2, part1)]
pub fn part1(input: &str) -> usize {
    let m = parser(input);
    m.iter()
    .filter(|ns| check(ns))
    .count()
}

#[aoc(day2, part2)]
pub fn part2(input: &str) -> usize {
    let m = parser(input);
    m.iter()
    .filter(|ns| (0..ns.len()).any(|i| check(&[&ns[..i], &ns[i+1..]].concat())))
    .count()
}


fn parser(input: &str) -> Vec<Vec<u32>> {
    input.lines().map(|l| l.split(' ').flat_map(str::parse).collect()).collect()
}

fn check(ns: &Vec<u32>) -> bool {
    is_safe(ns) && (ns.is_sorted_by(|a, b| a < b) || ns.is_sorted_by(|a, b| a > b))
}

fn is_safe(ns: &Vec<u32>) -> bool {
    ns.iter()
    .zip(ns.iter().skip(1))
    .all(|(&x, &y)| {
        let diff = x.abs_diff(y);
        diff >= 1 && diff <= 3
    })
}