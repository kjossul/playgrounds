use std::collections::HashMap;
use regex::Regex;

#[derive(Debug)]
struct Line {
    x1: usize,
    y1: usize,
    x2: usize,
    y2: usize,
}

#[aoc(day5, part1)]
pub fn part1(input: &str) -> usize {
    let lines = parser(input);
    let mut map: HashMap<(usize, usize), u32> = HashMap::new();
    for line in lines {
        if line.x1 == line.x2 {
            for j in line.y1..=line.y2 {
                *map.entry((line.x1, j)).or_insert(0) += 1;
            }
        } else if line.y1 == line.y2 {
            for i in line.x1..=line.x2 {
                *map.entry((i, line.y1)).or_insert(0) += 1;
            }
        }
    }
    map.values().filter(|&&v| v > 1).count()
}

#[aoc(day5, part2)]
pub fn part2(input: &str) -> usize {
    let lines = parser(input);
    let mut map: HashMap<(usize, usize), u32> = HashMap::new();
    for line in lines {
        if line.x1 == line.x2 {
            for j in line.y1..=line.y2 {
                *map.entry((line.x1, j)).or_insert(0) += 1;
            }
        } else if line.y1 == line.y2 {
            for i in line.x1..=line.x2 {
                *map.entry((i, line.y1)).or_insert(0) += 1;
            }
        } else {
            for i in 0..=line.x2 - line.x1 {
                if line.y1 < line.y2 {
                    *map.entry((line.x1 + i, line.y1 + i)).or_insert(0) += 1;
                } else {
                    *map.entry((line.x1 + i, line.y1 - i)).or_insert(0) += 1;
                }
            }
        }
    }
    map.values().filter(|&&v| v > 1).count()
}


fn parser(input: &str) -> Vec<Line> {
    let re = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();
    input.lines().map(|l| {
        let caps = re.captures(l).unwrap();
        let x1 = caps[1].parse().unwrap();
        let y1 = caps[2].parse().unwrap();
        let x2 = caps[3].parse().unwrap();
        let y2 = caps[4].parse().unwrap();
        if x1 < x2 || (x1 == x2 && y1 < y2) {
            Line {x1, y1, x2, y2}
        } else {
            Line {x1: x2, y1: y2, x2: x1, y2: y1}
        }
    }).collect()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y21d05test() {
        let s = indoc! {"\
            0,9 -> 5,9
            8,0 -> 0,8
            9,4 -> 3,4
            2,2 -> 2,1
            7,0 -> 7,4
            6,4 -> 2,0
            0,9 -> 2,9
            3,4 -> 1,4
            0,0 -> 8,8
            5,5 -> 8,2
        "};
        assert_eq!(part1(s), 5);
    }
}