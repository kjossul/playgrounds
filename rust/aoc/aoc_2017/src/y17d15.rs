#[aoc_generator(day15)]
pub fn generator(input: &str) -> Vec<u32> {
    input.lines().map(|l| {
        let n = l.split(' ').last().unwrap();
        n.parse().unwrap()
    }).collect()
}

#[aoc(day15, part1)]
pub fn part1(ns: &[u32]) -> usize {
    let mut a = ns[0];
    let mut b = ns[1];
    let (fa, fb) = (16807, 48271);
    let div = 2147483647;
    (0..40*10_u64.pow(6)).filter(|_| {
        a = ((a as u64 * fa) % div) as u32;
        b = ((b as u64 * fb) % div) as u32;
        (a << 16) >> 16 == (b << 16) >> 16
    }).count()
}

#[aoc(day15, part2)]
pub fn part2(ns: &[u32]) -> usize {
    let mut a = ns[0];
    let mut b = ns[1];
    let (fa, fb) = (16807, 48271);
    let div = 2147483647;
    (0..5*10_u64.pow(6)).filter(|_| {
        while {
            a = ((a as u64 * fa) % div) as u32;
            a % 4 != 0
        } {}
        while {
            b = ((b as u64 * fb) % div) as u32;
            b % 8 != 0
        } {}
        (a << 16) >> 16 == (b << 16) >> 16
    }).count()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y17d15test() {
        assert_eq!(part1(&generator("65\n8921")), 588);
        assert_eq!(part2(&generator("65\n8921")), 309);
    }
}