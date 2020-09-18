use std::collections::HashMap;
use itertools::Itertools;

#[aoc(day16, part1)]
pub fn part1(input: &str) -> String {
    let mut v = "abcdefghijklmnop".chars().collect::<Vec<char>>();
    f(&mut v, input);
    v.iter().collect()
}

#[aoc(day16, part2)]
pub fn part2(input: &str) -> String {
    let mut v = "abcdefghijklmnop".chars().collect::<Vec<char>>();
    let mut visited = HashMap::<Vec<char>, usize>::new();
    let mut count = 0;
    loop {
        match visited.insert(v.clone(), count) {
            Some(mu) => {  // starting position of the cycle
                let max = visited.values().max().unwrap();
                let lambda = max - mu;  // length of the cycle
                let i = (10_usize.pow(9) - mu) % lambda + mu;
                let (k, _) = visited.iter().find(|(_, &v)| v == i).unwrap_or((&v, &0));
                return k.iter().collect()
            },
            None => { count += 1; },
        }
        f(&mut v, input);
    }
}

fn f(v: &mut Vec<char>, input: &str) {
    for token in input.split(',') {
        let (op, rest) = token.split_at(1);
        match op {
            "s" => {
                let n: usize = rest.parse().unwrap();
                v.rotate_right(n);
            },
            "x" => {
                let (i, j) = rest.split('/').map(|s| s.parse().unwrap()).next_tuple().unwrap();
                v.swap(i, j);
            },
            "p" => {
                let (c1, c2) = (rest.chars().nth(0).unwrap(), rest.chars().nth(2).unwrap());
                let (i, j) = (v.iter().position(|&x| x == c1).unwrap(),
                              v.iter().position(|&x| x == c2).unwrap());
                v.swap(i, j);

            },
            _ => panic!("Unrecognized!"),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    pub fn y17d16test() {
//        assert_eq!(part2("s1,x3/4,pe/b"), "");
    }
}