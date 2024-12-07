use std::collections::HashMap;

#[aoc(day6, part1)]
pub fn part1(input: &str) -> u64 {
    let fishes = parser(input);
    let mut cache = HashMap::new();
    fishes.into_iter().map(|f| 1 + prole_generated(&mut cache, 80, f)).sum()
}

#[aoc(day6, part2)]
pub fn part2(input: &str) -> u64 {
    let fishes = parser(input);
    let mut cache = HashMap::new();
    fishes.into_iter().map(|f| 1 + prole_generated(&mut cache, 256, f)).sum()
}

fn prole_generated(cache: &mut HashMap<(u64, u64), u64>, days: u64, timer: u64) -> u64 {
    if let Some(&x) = cache.get(&(days, timer)) {
        x
    } else {
        let x = {
            if days <= timer {
                0
            } else {
                (0..=(days - timer - 1) / 7).map(|i| {
                    1 + prole_generated(cache, days - 1 - timer - 7 * i, 8)
                }).sum()
            }
        };
        cache.insert((days, timer), x);
        x
    }
}

fn parser(input: &str) -> Vec<u64> {
    input.split(",").flat_map(str::parse).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y21d06test() {
        assert_eq!(prole_generated(&mut HashMap::new(), 80, 0), 1420);
        assert_eq!(prole_generated(&mut HashMap::new(), 80, 1), 1400);
        assert_eq!(prole_generated(&mut HashMap::new(), 256, 1), 1400);
    }
}