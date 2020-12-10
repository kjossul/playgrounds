use regex::Regex;

struct Password {
    pw: String,
    c: char,
    lo: usize,
    hi: usize,
}

impl Password {
    fn is_valid1(&self) -> bool {
        let count = self.pw.chars().filter(|&c| c == self.c).count();
        self.lo <= count && count <= self.hi
    }

    fn is_valid2(&self) -> bool {
        let lo = self.pw.chars().nth(self.lo.saturating_sub(1)) == Some(self.c);
        let hi = self.pw.chars().nth(self.hi.saturating_sub(1)) == Some(self.c);
        lo != hi
    }
}

#[aoc(day2, part1)]
pub fn part1(input: &str) -> usize {
    let pws = parser(input);
    pws.iter().filter(|pw| pw.is_valid1()).count()
}

#[aoc(day2, part2)]
pub fn part2(input: &str) -> usize {
    let pws = parser(input);
    pws.iter().filter(|pw| pw.is_valid2()).count()
}

fn parser(input: &str) -> Vec<Password> {
    let re = Regex::new(r"(\d+)-(\d+) (\w): (\w+)").unwrap();
    input.lines().map(|l| {
        let caps = re.captures(l).unwrap();
        let lo = caps[1].parse().unwrap();
        let hi = caps[2].parse().unwrap();
        let c = caps[3].chars().next().unwrap();
        let pw = String::from(&caps[4]);
        Password {pw, c, lo, hi}
    }).collect()
}