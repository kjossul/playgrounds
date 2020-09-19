use std::collections::HashMap;
use itertools::Itertools;

struct Art {
    pattern: Vec<Vec<char>>,
}

impl Art {
    fn transform(&mut self, d: &HashMap<Vec<Vec<char>>, Vec<Vec<char>>>) {
        let l = self.pattern.len();
        let (new_dim, window);
        if l % 2 == 0 {
            new_dim = l / 2 * 3;
            window = 2;
        } else {
            new_dim = l / 3 * 4;
            window = 3;
        };
        let mut new_pattern = std::iter::repeat(vec![]).take(new_dim).collect::<Vec<_>>();
        for i in 0..l / window {
            for j in 0..l / window {
                let view = self.pattern[i * window..(i + 1) * window].iter().map(|v| {
                    v[j * window..(j + 1) * window].iter().cloned().collect::<Vec<char>>()
                }).collect::<Vec<_>>();
                for (k, row) in d.get(&view).expect("Pattern not found!").into_iter().enumerate() {
                    new_pattern[k + i * (window + 1)].extend_from_slice(row);
                }
            }
        }
        self.pattern = new_pattern;
    }
}

#[aoc_generator(day21)]
pub fn generator(input: &str) -> HashMap<Vec<Vec<char>>, Vec<Vec<char>>> {
    let mut map = HashMap::new();
    for line in input.lines() {
        let (mut k, v) = line.split(" => ").map(|l| line_to_matrix(l)).next_tuple().unwrap();
        for _ in 0..=3 {
            map.insert(k.clone(), v.clone());
            map.insert(flip_y(&k), v.clone());
            k = rotate(&k);
        }
    }
    map
}

fn line_to_matrix(line: &str) -> Vec<Vec<char>> {
    line.split("/").map(|l| l.chars().collect::<Vec<char>>()).collect()
}

fn rotate(m: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    // rotates 90° cw
    let mut v = (0..m.len()).map(|_| Vec::<char>::new()).collect::<Vec<Vec<char>>>();
    for i in (0..m.len()).rev() {
        for j in (0..m.len()).rev() {
            v[i].push(m[j][i]);
        }
    }
    v
}

fn flip_y(m: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    (0..m.len()).rev().map(|i| m[i].clone()).collect::<Vec<Vec<char>>>()
}

#[aoc(day21, part1)]
pub fn part1(d: &HashMap<Vec<Vec<char>>, Vec<Vec<char>>>) -> usize {
    let mut art = Art { pattern: line_to_matrix(".#./..#/###") };
    for _ in 0..5 {
        art.transform(d);
    }
    art.pattern.iter().flatten().filter(|&&x| x == '#').count()
}

#[aoc(day21, part2)]
pub fn part2(d: &HashMap<Vec<Vec<char>>, Vec<Vec<char>>>) -> usize {
    let mut art = Art { pattern: line_to_matrix(".#./..#/###") };
    for _ in 0..18 {
        art.transform(d);
    }
    art.pattern.iter().flatten().filter(|&&x| x == '#').count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y17d21test() {
        let s = indoc! {"
            ../.# => ##./#../...
            .#./..#/### => #..#/..../..../#..#
        "};
        assert_eq!(part1(&generator(s)), 12);
    }
}