#[aoc(day3, part1)]
pub fn part1(input: &str) -> u32 {
    let ns = parser(input);
    let gamma = convert(&bit_criteria(&ns, true));
    let epsilon = convert(&bit_criteria(&ns, false));
    gamma * epsilon
}

#[aoc(day3, part2)]
pub fn part2(input: &str) -> u32 {
    let ns = parser(input);
    let gamma = convert(&find_rating(&ns, true, 0).first().unwrap());
    let epsilon = convert(&find_rating(&ns, false, 0).first().unwrap());
    gamma * epsilon
}

fn find_rating(ns: &Vec<Vec<u32>>, most_common: bool, i: usize) -> Vec<Vec<u32>> {
    let mask = bit_criteria(ns, most_common);
    let mut next = ns.clone();
    next.retain(|v| v[i] == mask[i]);
    if next.len() == 1 {
        next
    } else {
        find_rating(&next, most_common, i + 1)
    }
}

fn convert(bits: &[u32]) -> u32 {
    bits.iter()
        .fold(0, |result, &bit| {
            (result << 1) ^ bit
        })
}

fn bit_criteria(ns: &Vec<Vec<u32>>, most_common: bool) -> Vec<u32>{
    let half = ns.len() as u32 / 2;
    let digits = ns[0].len();
    ns.into_iter().fold(vec![0; digits], |acc, v|
        acc.iter().zip(v.iter()).map(|(&x, &y)| x + y).collect()
    ).into_iter().map(|x| ((x >= half) ^ most_common) as u32).collect::<Vec<u32>>()
}

fn parser(input: &str) -> Vec<Vec<u32>> {
    input.lines().map(|l| l.chars().flat_map(|c| c.to_digit(10)).collect()).collect()
}