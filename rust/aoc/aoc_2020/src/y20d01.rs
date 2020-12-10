#[aoc(day1, part1)]
pub fn part1(input: &str) -> u32 {
    let ns = parser(input);
    for &n in &ns {
        for &m in &ns {
            if n + m == 2020 {
                return n * m;
            }
        }
    }
    panic!();
}

#[aoc(day1, part2)]
pub fn part2(input: &str) -> u32 {
    let ns = parser(input);
    for &n in &ns {
        for &m in &ns {
            for &o in &ns {
                if n + m + o == 2020 {
                    return n * m * o;
                }
            }
        }
    }
    panic!();
}

fn parser(input: &str) -> Vec<u32> {
    input.lines().flat_map(str::parse).collect()
}