use regex::Regex;

#[aoc(day3, part1)]
pub fn part1(input: &str) -> u32 {
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    re.captures_iter(input).map(|c| {
        let (_, [a, b]) = c.extract();
        a.parse::<u32>().unwrap() * b.parse::<u32>().unwrap()
    }).sum()
}

#[aoc(day3, part2)]
pub fn part2(input: &str) -> u32 {
    input.split("do()")
    .flat_map(|s| s.split("don't()").next())
    .map(|s| part1(s)).sum()
}