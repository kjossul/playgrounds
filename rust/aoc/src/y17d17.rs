#[aoc(day17, part1)]
pub fn part1(input: &str) -> u32 {
    let steps: u32 = input.parse().unwrap();
    let mut v = vec![0];
    for n in 1..=2017 {
        // n is also the current length of the vector before insertion
        let offset = steps % n;
        v.rotate_left(offset as usize + 1);
        v.insert(0, n);
    }
    v[1]
}

#[aoc(day17, part2)]
pub fn part2(input: &str) -> u32 {
    /*
    In this version the vector is not rotated, instead a pos variable is used and the output value
    is updated just when the position ends on the element at pos=1 (pos=0 always 0)
    */
    let steps: u32 = input.parse().unwrap();
    let mut v = 0;
    let mut pos = 0;
    for n in 1..=50 * 10_u32.pow(6) {
        // n is also the current length of the vector before insertion
        pos = (pos + steps % n) % n + 1;
        if pos == 1 {
            v = n;
        }
    }
    v
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y17d17test() {
        assert_eq!(part1("3"), 638);
    }
}