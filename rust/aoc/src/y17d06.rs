use std::collections::HashMap;

#[aoc_generator(day6)]
pub fn generator(input: &str) -> Vec<u32> {
    input.trim().split('\t').flat_map(|d| d.parse()).collect()
}

#[aoc(day6, part1)]
pub fn part1(ns: &[u32]) -> u32 {
    let mut numbers = ns.to_vec();
    let (x, _) = solver(&mut numbers);
    x
}

#[aoc(day6, part2)]
pub fn part2(ns: &[u32]) -> u32 {
    let mut numbers = ns.to_vec();
    let (_, y) = solver(&mut numbers);
    y
}

pub fn solver(numbers: &mut Vec<u32>) -> (u32, u32) {
    let len = numbers.len() as u32;
    let mut found: HashMap<Vec<u32>, u32> = HashMap::new();
    loop {
        let clone = numbers.clone();
        match found.get(&clone) {
            Some(v) => {
                break (*found.values().max().unwrap(), *v);
            },
            None => {
                found.insert(clone, 0);
                found.iter_mut().for_each(|(_, v)| *v += 1);
            },
        }
        let (i, n) = numbers
            .iter_mut()
            .enumerate()
            .max_by(|(i1, n1), (i2, n2)| (n1, i2).cmp(&(n2, i1)))  // smaller indexes first
            .unwrap();
        let quot = *n / len;
        let remainder = (*n % len) as usize;
        *n = 0;
        numbers.rotate_left(i + 1);
        numbers.iter_mut().for_each(|n| *n += quot);
        &numbers[..remainder].iter_mut().for_each(|n| *n += 1);
        numbers.rotate_right(i + 1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!(part1(&generator("0\t2\t7\t0")), 5);
        assert_eq!(part2(&generator("0\t2\t7\t0")), 4);
    }
}