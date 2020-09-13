#[aoc(day9, part1)]
pub fn part1(input: &str) -> u32 {
    let (group, _) = solver(input);
    group
}

#[aoc(day9, part2)]
pub fn part2(input: &str) -> u32 {
    let (_, garbage) = solver(input);
    garbage
}

fn solver(input: &str) -> (u32, u32) {
    let mut it = input.chars();
    let mut group_count = 0;
    let mut garbage_count = 0;
    let mut group = 0;
    let mut garbage = 0;
    while let Some(c) = it.next() {
        match (c, garbage > 0) {
            ('{', false) => group += 1,
            ('<', false) => garbage += 1,
            ('!', _) => { it.next(); },
            ('>', _) => garbage -= 1,
            ('}', false) => {
                group_count += group;
                group -= 1;
            },
            (_, true) => garbage_count += 1,
            (_, _) => continue,
        }
    }
    (group_count, garbage_count)
}

#[cfg(test)]
mod tests {
    #[test]
    fn y17d09test() {}
}