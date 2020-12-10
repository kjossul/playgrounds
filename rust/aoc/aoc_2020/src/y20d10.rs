use std::collections::HashMap;

#[aoc(day10, part1)]
pub fn part1(input: &str) -> u32 {
    let ns = parser(input);
    let (mut diff1, mut diff3) = (0, 0);
    let mut prev = 0;
    for &n in &ns {
        if n - prev == 1 {
            diff1 += 1;
        } else if n - prev == 3 {
            diff3 += 1;
        }
        prev = n;
    }
    diff1 * diff3
}

#[aoc(day10, part2)]
pub fn part2(input: &str) -> usize {
    let ns = parser(input);
    let mut cache = HashMap::new();
    dp(&ns, 0, &mut cache)
}

fn parser(input: &str) -> Vec<u32> {
    let mut ns = input.lines().flat_map(str::parse).collect::<Vec<_>>();
    ns.push(0);
    ns.sort();
    ns.push(ns.last().unwrap() + 3);
    ns
}

fn dp(xs: &[u32], i: usize, cache: &mut HashMap<usize, usize>) -> usize {
    if i == xs.len() - 1 {
        return 1;
    } else if let Some(j) = cache.get(&i) {
        return *j;
    }
    let mut ans = 0;
    for j in i + 1..xs.len() {
        if xs[j] - xs[i] <= 3 {
            ans += dp(xs, j, cache);
        }
    }
    cache.insert(i, ans);
    ans
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d10test() {}
}
