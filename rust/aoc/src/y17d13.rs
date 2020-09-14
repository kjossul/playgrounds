use std::collections::HashMap;
use itertools::Itertools;

#[derive(Debug)]
struct Firewall {
    layers: HashMap<isize, Scanner>,
}

#[derive(Debug)]
struct Scanner {
    range: isize,
    pos: isize,
    direction: isize,
    period: isize,
}

impl Firewall {
    fn new() -> Self {
        Self { layers: HashMap::new() }
    }

    fn step_all(&mut self) {
        for scanner in self.layers.values_mut() {
            scanner.step();
        }
    }
}

impl Scanner {
    fn new(range: isize) -> Self {
        Self { range, pos: 0, direction: 1, period: (range - 1) * 2 }
    }

    fn step(&mut self) {
        if self.range == 1 {
            return;
        }
        self.pos += self.direction;
        if self.pos < 0 {
            self.pos = 1;
            self.direction = 1;
        } else if self.pos >= self.range {
            self.pos = self.range - 2;
            self.direction = -1;
        }
    }

    fn collides(&self, t: isize, shift: isize) -> bool {
        // Each scanner will return to position 0 with a period of 2(depth-1). If we want to find
        // collisions, we have to say that t = period * n - shift, with n being a natural number
        (t + shift) % self.period == 0
    }
}

fn generator(input: &str) -> Firewall {
    let mut fw = Firewall::new();
    for line in input.lines() {
        let (depth, range) = line.split(": ").map(|d| d.parse().unwrap()).next_tuple().unwrap();
        let scanner = Scanner::new(range);
        fw.layers.insert(depth, scanner);
    }
    fw
}

#[aoc(day13, part1)]
pub fn part1(input: &str) -> isize {
    let mut fw = generator(input);
    let last = *fw.layers.keys().max().unwrap();
    let mut severity = 0;
    for i in 0..=last {
        match fw.layers.get(&i) {
            Some(scanner) => {
                if scanner.pos == 0 {
                    severity += i * scanner.range;
                }
            }
            None => {}
        }
        fw.step_all();
    }
    severity
}

#[aoc(day13, part2)]
pub fn part2(input: &str) -> isize {
    let fw = generator(input);
    (1..)
        .find(|&t| fw.layers.iter()
            .all(|(&shift, scanner)| !scanner.collides(t, shift))
        ).unwrap()
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn y17d13test() {
        let s = indoc! {"
            0: 3
            1: 2
            4: 4
            6: 4
        "};
        assert_eq!(part1(s), 24);
        assert_eq!(part2(s), 10);
    }
}