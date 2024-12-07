use std::fmt::{Display, Formatter};
use itertools::Itertools;

#[derive(PartialEq)]
struct Pair {
    left: Box<SnailFish>,
    right: Box<SnailFish>,
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{},{}]", self.left, self.right)
    }
}

#[derive(PartialEq)]
enum SnailFish {
    Number(u8),
    Pair(Pair),
}

#[derive(Clone, Copy, PartialEq)]
enum ReduceType {
    Split,
    Explosion,
}

impl Display for SnailFish {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SnailFish::Number(n) => { n.fmt(f) }
            SnailFish::Pair(p) => { p.fmt(f) }
        }
    }
}

#[derive(PartialEq)]
enum ReduceResult {
    Explode(Option<u8>, Option<u8>, u8),
    Split(SnailFish),
    Modified,
    FullyReduced,
}

impl SnailFish {
    fn number(&mut self) -> Option<&mut u8> {
        if let SnailFish::Number(n) = self {
            Some(n)
        } else {
            None
        }
    }

    fn pair(&mut self) -> Option<&mut Pair> {
        if let SnailFish::Pair(p) = self {
            Some(p)
        } else {
            None
        }
    }

    fn simplify(&mut self) {
        while self.reduce(0, ReduceType::Explosion) != ReduceResult::FullyReduced ||
            self.reduce(0, ReduceType::Split) != ReduceResult::FullyReduced { }
    }

    fn reduce(&mut self, depth: u8, t: ReduceType) -> ReduceResult {
        match self {
            SnailFish::Number(n) => {
                if *n > 9 && t == ReduceType::Split {
                    let left = Box::new(SnailFish::Number(*n / 2));
                    let right = Box::new(SnailFish::Number(*n / 2 + *n % 2));
                    ReduceResult::Split(SnailFish::Pair(Pair { left, right }))
                } else {
                    ReduceResult::FullyReduced
                }
            }
            SnailFish::Pair(Pair { left, right }) => {
                if depth == 4 && t == ReduceType::Explosion {
                    return ReduceResult::Explode(Some(*self.pair().unwrap().left.number().unwrap()),
                                                 Some(*self.pair().unwrap().right.number().unwrap()), depth);
                }
                match left.reduce(depth + 1, t) {
                    ReduceResult::Explode(a, mut b, explosion_depth) => {
                        if let Some(n) = b {
                            let target = self.pair().unwrap().right.as_mut();
                            if let Some(m) = target.number() {
                                *m += n;
                            } else {
                                target.propagate_number(n, |p| p.left.as_mut());
                            }
                            b = None;
                        }
                        if depth + 1 == explosion_depth {
                            *self.pair().unwrap().left = SnailFish::Number(0);
                        }
                        return ReduceResult::Explode(a, b, explosion_depth);
                    }
                    ReduceResult::Split(new_left) => {
                        self.pair().unwrap().left = Box::new(new_left);
                        return ReduceResult::Modified;
                    }
                    ReduceResult::Modified => { return ReduceResult::Modified; }
                    ReduceResult::FullyReduced => {}
                }
                match right.reduce(depth + 1, t) {
                    ReduceResult::Explode(mut a, b, explosion_depth) => {
                        if let Some(n) = a {
                            let target = self.pair().unwrap().left.as_mut();
                            if let Some(m) = target.number() {
                                *m += n;
                            } else {
                                target.propagate_number(n, |p| p.right.as_mut());
                            }
                            a = None;
                        }
                        if depth + 1 == explosion_depth {
                            *self.pair().unwrap().right = SnailFish::Number(0);
                        }
                        return ReduceResult::Explode(a, b, explosion_depth);
                    }
                    ReduceResult::Split(new_right) => {
                        self.pair().unwrap().right = Box::new(new_right);
                        return ReduceResult::Modified;
                    }
                    ReduceResult::Modified => { return ReduceResult::Modified; }
                    ReduceResult::FullyReduced => {}
                }
                ReduceResult::FullyReduced
            }
        }
    }

    fn propagate_number(&mut self, n: u8, f: fn(p: &mut Pair) -> &mut SnailFish) {
        let target = f(self.pair().unwrap());
        if let Some(m) = target.number() {
            *m += n;
        } else {
            target.propagate_number(n, f);
        }
    }

    fn add(self, other: SnailFish) -> SnailFish {
        SnailFish::Pair(Pair { left: Box::new(self), right: Box::new(other) })
    }

    fn magnitude(&self) -> u64 {
        match self {
            SnailFish::Number(n) => { *n as u64 }
            SnailFish::Pair(Pair { left, right }) => { 3 * left.magnitude() + 2 * right.magnitude() }
        }
    }
}

#[aoc(day18, part1)]
pub fn part1(input: &str) -> Option<u64> {
    let mut fishes = parser(input);
    let f0 = fishes.remove(0);
    let sum = fishes.into_iter().fold(f0, |acc, f| {
        let mut new_fish = acc.add(f);
        new_fish.simplify();
        new_fish
    });
    Some(sum.magnitude())
}


#[aoc(day18, part2)]
pub fn part2(input: &str) -> Option<u64> {
    input.lines().combinations(2).map(|ls| {
        let left = part1([ls[0], ls[1]].join("\n").as_str());
        let right = part1([ls[1], ls[0]].join("\n").as_str());
        u64::max(left.unwrap(), right.unwrap())
    }).max()
}


fn parser(input: &str) -> Vec<SnailFish> {
    input.lines().map(|l| {
        let mut it = l.chars().into_iter();
        parse_snail_fish(&mut it)
    }).collect()
}

fn parse_snail_fish<I>(it: &mut I) -> SnailFish
    where I: Iterator<Item=char> {
    if let Some(c) = it.next() {
        if let Some(value) = c.to_digit(10) {
            return SnailFish::Number(value as u8);
        } else {
            match c {
                '[' => {
                    let left = Box::new(parse_snail_fish(it));
                    assert_eq!(it.next(), Some(','));
                    let right = Box::new(parse_snail_fish(it));
                    assert_eq!(it.next(), Some(']'));
                    return SnailFish::Pair(Pair { left, right });
                }
                _ => panic!(),
            }
        }
    }
    panic!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d18test() {
        let mut s = "[[1,2],[[3,4],5]]";
        let mut it = s.chars().into_iter();
        let mut fish = parse_snail_fish(&mut it);
        assert_eq!(fish.magnitude(), 143);
        s = indoc! {"\
            [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
            [[[5,[2,8]],4],[5,[[9,9],0]]]
            [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
            [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
            [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
            [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
            [[[[5,4],[7,7]],8],[[8,3],8]]
            [[9,3],[[9,9],[6,[4,9]]]]
            [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
            [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
        "};
        it = s.chars().into_iter();
        fish = parse_snail_fish(&mut it);
        fish.simplify();
        println!("{}", fish);
        assert_eq!(part1(s), Some(4140));
    }
}