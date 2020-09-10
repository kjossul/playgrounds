use regex::Regex;

#[aoc_generator(day2)]
pub fn input_generator(input: &str) -> Vec<Vec<u32>> {
    let r = Regex::new(r"[ \t]").unwrap();
    input
        .lines()
        .map(|l| r
            .split(l.trim())
            .map(|d| d.parse().unwrap())
            .collect()
        )
        .collect()
}

#[aoc(day2, part1)]
pub fn part1(lines: &[Vec<u32>]) -> u32 {
    lines
        .iter()
        .map(|v| v.iter().max().unwrap() - v.iter().min().unwrap())
        .sum()
}

#[aoc(day2, part2)]
pub fn part2(lines: &[Vec<u32>]) -> u32 {
    lines
        .iter()
        .map(|v| {
            for (i, &n1) in v.iter().enumerate() {
                for &n2 in &v[i+1..] {
                    if n1 % n2 == 0 {
                        return n1 / n2;
                    } else if n2 % n1 == 0 {
                        return n2 / n1;
                    }
                }
            }
            panic!("No evenly divisible numbers found!");
        })
        .sum()
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test() {
        let s1 = indoc! {"
            5 1 9 5
            7 5 3
            2 4 6 8
        "};
        let s2 = indoc! {"
            5 9 2 8
            9 4 7 3
            3 8 6 5
        "};
        assert_eq!(part1(&input_generator(s1)), 18);
        assert_eq!(part2(&input_generator(s2)), 9);
    }
}