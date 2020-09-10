#[aoc_generator(day1)]
pub fn input_generator(input: &str) -> Vec<u8> {
    input.chars()
        .map(|c| c.to_digit(10).unwrap() as u8)
        .collect::<Vec<u8>>()
}

#[aoc(day1, part1)]
pub fn part1(digits: &[u8]) -> u32 {
    let f = |i, n| (n + i - 1) % n;
    solver(digits, f)
}

#[aoc(day1, part2)]
pub fn part2(digits: &[u8]) -> u32 {
    let f = |i, n| (i + n / 2) % n;
    solver(digits, f)
}

fn solver(digits: &[u8], f: fn(usize, usize) -> usize) -> u32 {
    let n = digits.len();
    digits
        .iter()
        .enumerate()
        .map(|(i, &e)| {
            let j = f(i, n);
            if e == digits[j] {e as u32} else {0}
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!(part1(&input_generator("1122")), 3);
        assert_eq!(part1(&input_generator("1111")), 4);
        assert_eq!(part1(&input_generator("1234")), 0);
        assert_eq!(part1(&input_generator("91212129")), 9);

        assert_eq!(part2(&input_generator("1212")), 6);
        assert_eq!(part2(&input_generator("1221")), 0);
        assert_eq!(part2(&input_generator("123123")), 12);
        assert_eq!(part2(&input_generator("12131415")), 4);
    }
}