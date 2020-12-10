use itertools::Itertools;
use itertools::MinMaxResult;

const N: usize = 25;  // change to 5 for test

#[aoc(day9, part1)]
pub fn part1(input: &str) -> u64 {
    let (mut allowed, numbers) = parser(input);
    for i in N..input.len() {
        if let Some(_) = allowed.iter().find(|&&n| n == numbers[i]) {
            allowed = preamble(&numbers[i.saturating_sub(N) + 1..i + 1]);
        } else {
            return numbers[i];
        }
    }
    panic!();
}

#[aoc(day9, part2)]
pub fn part2(input: &str) -> u64 {
    let target = part1(input);
    let (_allowed, ns) = parser(input);
    for i in 0..ns.len() {
        let mut acc = 0;
        for j in i..ns.len() {
            acc += ns[j];
            if acc == target {
                if let MinMaxResult::MinMax(a, b) = ns[i..=j].iter().minmax() {
                    return *a + *b;
                }
            } else if acc > target {
                break;
            }
        }
    }
    panic!();
}


fn parser(input: &str) -> (Vec<u64>, Vec<u64>) {
    let ns = input.lines().flat_map(str::parse).collect::<Vec<_>>();
    (preamble(&ns[..N]), ns)
}

fn preamble(ns: &[u64]) -> Vec<u64> {
    ns.iter().combinations(2).map(|v| v.into_iter().sum()).collect()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d09test() {
        let s = indoc!{"\
            35
            20
            15
            25
            47
            40
            62
            55
            65
            95
            102
            117
            150
            182
            127
            219
            299
            277
            309
            576
        "};
        assert_eq!(part1(s), 127);
    }
}