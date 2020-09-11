use std::collections::HashMap;

#[aoc_generator(day6)]
pub fn generator(input: &str) -> Vec<u32> {
    input.trim().split('\t').flat_map(|d| d.parse()).collect()
}

#[aoc(day6, part1, naive)]
pub fn part1_naive(ns: &[u32]) -> u32 {
    let (x, _) = naive_solver(ns);
    x
}

#[aoc(day6, part2, naive)]
pub fn part2_naive(ns: &[u32]) -> u32 {
    let (_, y) = naive_solver(ns);
    y
}

#[aoc(day6, part1, floyd)]
pub fn part1_floyd(ns: &[u32]) -> u32 {
    let (lambda, mu) = floyd_solver(ns);
    lambda + mu
}

#[aoc(day6, part2, floyd)]
pub fn part2_floyd(ns: &[u32]) -> u32 {
    let (lambda, _) = floyd_solver(ns);
    lambda
}

pub fn naive_solver(ns: &[u32]) -> (u32, u32) {
    let mut found: HashMap<Vec<u32>, u32> = HashMap::new();
    let mut current = ns.to_vec();
    loop {
        match found.get(&current) {
            Some(v) => {
                break (*found.values().max().unwrap(), *v);
            },
            None => {
                found.insert(current.clone(), 0);
                found.iter_mut().for_each(|(_, v)| *v += 1);
            },
        }
        current = mutator(current);
    }
}

pub fn floyd_solver(ns: &[u32]) -> (u32, u32) {
    // https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare
    let mut tortoise = mutator(ns.to_vec());
    let mut hare = mutator(mutator(ns.to_vec()));
    while tortoise != hare {
        tortoise = mutator(tortoise);
        hare = mutator(mutator(hare));
    }
    tortoise = ns.to_vec();
    let mut mu = 0;
    while tortoise != hare {
        mu += 1;
        tortoise = mutator(tortoise);
        hare = mutator(hare);
    }
    hare = mutator(hare);
    let mut lambda = 1;
    while tortoise != hare {
        hare = mutator(hare);
        lambda += 1;
    }
    (lambda, mu)
}

pub fn mutator(ns: Vec<u32>) -> Vec<u32> {
    let len = ns.len() as u32;
    let mut numbers = ns;
    let (i, n) = numbers
        .iter_mut()
        .enumerate()
        .max_by(|(i1, n1), (i2, n2)| (n1, i2).cmp(&(n2, i1)))  // smaller indexes first
        .unwrap();
    let quot = *n / len;
    let remainder = (*n % len) as usize;
    *n = 0;
    numbers.rotate_left(i + 1);
    numbers.iter_mut().enumerate().for_each(|(i, n)| {
        *n += quot;
        if i < remainder {
            *n += 1;
        }
    });
    numbers.rotate_right(i + 1);
    numbers
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn y17d06test() {
        assert_eq!(part1_naive(&generator("0\t2\t7\t0")), 5);
        assert_eq!(part2_naive(&generator("0\t2\t7\t0")), 4);
        assert_eq!(part1_floyd(&generator("0\t2\t7\t0")), 5);
        assert_eq!(part2_floyd(&generator("0\t2\t7\t0")), 4);
    }
}