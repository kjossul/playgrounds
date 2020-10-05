#[aoc(day5, part1)]
pub fn part1(input: &str) -> usize {
    let mut v = input.trim().chars().collect::<Vec<_>>();
    'outer: loop {
        let mut i = 0;
        while i < v.len() - 1 {
            if v[i] != v[i + 1] && v[i].to_ascii_lowercase() == v[i + 1].to_ascii_lowercase() {
                v.remove(i);
                v.remove(i);
                continue 'outer;
            }
            i += 1;
        }
        break v.len();
    }
}

#[aoc(day5, part2)]
pub fn part2(input: &str) -> usize {
    let mut min = usize::MAX;
    for (l, u) in (b'a'..=b'z').zip(b'A'..=b'Z') {
        let string = input.trim().chars().filter(
            |&c| c != char::from(l) && c != char::from(u)
        ).collect::<String>();
        min = std::cmp::min(min, part1(&string));
    }
    min
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y18d05test() {
        assert_eq!(part1("dabAcCaCBAcCcaDA"), 10);
    }
}