use std::convert::TryFrom;

#[aoc_generator(day5)]
pub fn generator(input: &str) -> Vec<i32> {
    input.lines().map(|d| d.parse().unwrap()).collect()
}

#[aoc(day5, part1)]
pub fn part1(ns: &[i32]) -> u32 {
    let f = |n: &mut i32| {
        *n += 1;
    };
    solver(ns, f)
}

#[aoc(day5, part2)]
pub fn part2(ns: &[i32]) -> u32 {
    let f = |n: &mut i32| {
        if *n >= 3 {
            *n -= 1;
        } else {
            *n += 1;
        }
    };
    solver(ns, f)
}

pub fn solver(ns: &[i32], f: fn(&mut i32) -> ()) -> u32 {
    let mut instructions = ns.to_vec();
    let mut count = 0;
    let mut i: usize = 0;
    loop {
        match instructions.get_mut(i) {
            Some(n) => {
                count += 1;
                match usize::try_from(i as i32 + *n) {
                    Ok(n) => i = n,
                    Err(_) => break count,
                };
                f(n);
            },
            None => break count,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!(part1(&generator("0\n3\n0\n1\n-3")), 5);
    }
}