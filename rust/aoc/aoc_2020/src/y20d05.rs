#[aoc(day5, part1)]
pub fn part1(input: &str) -> usize {
    let boarding_passes = parser(input);
    boarding_passes.iter().map(|bp| {
        seat_id(bp)
    }).max().unwrap()
}

#[aoc(day5, part2)]
pub fn part2(input: &str) -> usize {
    let mut boarding_passes = parser(input);
    boarding_passes.sort_by_key(|bp| seat_id(bp));
    let mut previous = String::from("FFFFFFFLLL");
    for bp in &boarding_passes {
        let prev = seat_id(&previous);
        let curr = seat_id(&bp);
        if prev + 2 == curr {
            return curr - 1;
        }
        previous = bp.clone();
    };
    panic!()
}

fn parser(input: &str) -> Vec<String> {
    input.lines().map(|l| String::from(l.trim())).collect()
}

fn binary(input: &str, mut lo: usize, mut hi: usize, is_hi: fn(char) -> bool) -> usize {
    for c in input.chars() {
        let diff = (hi - lo + 1) / 2;
        if is_hi(c) {
            lo += diff;
        } else {
            hi -= diff;
        }
    }
    assert_eq!(lo, hi);
    lo
}

fn seat_id(bp: &str) -> usize {
    let row = binary(&bp[..7], 0, 127, |c| c == 'B');
    let hi = binary(&bp[7..], 0, 7, |c| c == 'R');
    row * 8 + hi
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d05test() {
    }
}