const LEN: usize = 256;

#[aoc(day10, part1)]
pub fn part1(input: &str) -> usize {
    let ns: Vec<usize> = input.split(',').map(|d| d.parse().unwrap()).collect();
    let mut v = (0..LEN).collect::<Vec<usize>>();
    let mut pos = 0;
    let mut skip = 0;
    for n in ns {
        hash(&mut v, &pos, n);
        pos = (pos + n + skip) % LEN;
        skip += 1;
    }
    v[0] * v[1]
}

#[aoc(day10, part2)]
pub fn part2(input: &str) -> String {
    let ns = [input.as_bytes(), &[17, 31, 73, 47, 23]].concat();
    let mut v = (0..LEN).collect::<Vec<usize>>();
    let mut pos = 0;
    let mut skip = 0;
    for _ in 0..64 {
        for &n in &ns {
            hash(&mut v, &pos, n as usize);
            pos = (pos + n as usize + skip) % LEN;
            skip += 1;
        }
    }
    (0..16).map(|i| {
        let n = v[i * 16..(i + 1) * 16].iter().fold(0, |acc, n| acc ^ n);
        format!("{:02x}", n)
    }).collect::<Vec<String>>().join("")
}

fn hash(v: &mut Vec<usize>, &pos: &usize, n: usize) {
    v.rotate_left(pos);
    let left = &mut v[0..n];
    left.reverse();
    v.rotate_right(pos);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn y17d10test() {
//        assert_eq!(part1(&generator("3,4,1,5")), 12);
        assert_eq!(part2(""), "a2582a3a0e66e6e86e3812dcb672a272");
    }
}