#[aoc(day14, part1)]
pub fn part1(input: &str) -> String {
    let mut v = vec![3, 7];
    let (mut i, mut j) = (0, 1);
    let n = input.trim().parse().unwrap();
    while v.len() < n + 10 {
        let mut sum = v[i] + v[j];
        if sum > 9 {
            v.push(1);
            sum %= 10;
        }
        v.push(sum);
        i = (i + v[i] + 1) % v.len();
        j = (j + v[j] + 1) % v.len();
        if i == j {
            j += 1;
        }
    }
    v[n..n + 10].iter().flat_map(|d| d.to_string().chars().next()).collect()
}

#[aoc(day14, part2)]
pub fn part2(input: &str) -> usize {
    let target = input.trim().chars().flat_map(|c| c.to_digit(10)).collect::<Vec<_>>();
    let target_len = target.len();
    let mut v: Vec<u32> = vec![3, 7];
    let (mut i, mut j, mut k) = (0, 1, 0);
    loop {
        let mut sum = v[i] + v[j];
        if sum > 9 {
            v.push(1);
            sum %= 10;
        }
        v.push(sum);
        i = (i + v[i] as usize + 1) % v.len();
        j = (j + v[j] as usize + 1) % v.len();
        if i == j {
            j += 1;
        }
        if let Some(diff) = (v.len()).checked_sub(target_len + k) {
            for k1 in k..=k + diff {
                if &target[..] == &v[k1..k1 + target_len] {
                    return k1;
                }
            }
            k += diff + 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y18d14test() {
        assert_eq!(part1("9"), String::from("5158916779"));
        assert_eq!(part2("59414"), 2018);
    }
}