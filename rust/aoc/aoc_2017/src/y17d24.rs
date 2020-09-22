use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Component {
    l: u32,
    r: u32,
    occupied: Option<u32>,
}

impl Component {
    fn strength(&self) -> u32 {
        self.l + self.r
    }

    fn has_pin(&self, pin: u32) -> bool {
        self.l == pin || self.r == pin
    }

    fn free_pin(&self) -> u32 {
        if self.occupied == Some(self.l) {
            self.r
        } else {
            self.l
        }
    }

    fn attach(&mut self, other: &Component) {
        if other.free_pin() == self.l {
            self.occupied = Some(self.l)
        } else {
            self.occupied = Some(self.r)
        }
    }
}


#[aoc(day24, part1)]
pub fn part1(input: &str) -> u32 {
    let components = parser(input);
    let mut max = 0;
    let mut stack: Vec<Vec<Component>> = components.iter().filter(|c| c.occupied == Some(0))
        .map(|c| vec![c.clone()]).collect();
    while let Some(current) = stack.pop() {
        let sum = current.iter().map(|c| c.strength()).sum();
        max = std::cmp::max(max, sum);
        let last = current.last().unwrap();
        for new in components.iter().filter(|&&c| c.has_pin(last.free_pin()) &&
            current.iter().find(|&&other| other.l == c.l && other.r == c.r).is_none()) {
            let mut next = current.clone();
            let mut copy = new.clone();
            copy.attach(&last);
            next.push(copy);
            stack.push(next);
        }
    }
    max
}



#[aoc(day24, part2)]
pub fn part2(input: &str) -> u32 {
    let components = parser(input);
    let mut longest = vec![];
    let mut longest_sum = 0;
    let mut stack: Vec<Vec<Component>> = components.iter().filter(|c| c.occupied == Some(0))
        .map(|c| vec![c.clone()]).collect();
    while let Some(current) = stack.pop() {
        let sum = current.iter().map(|c| c.strength()).sum();
        if longest.len() < current.len() || (longest.len() == current.len() && sum > longest_sum) {
            longest = current.clone();
            longest_sum = sum;
        }
        let last = current.last().unwrap();
        for new in components.iter().filter(|&&c| c.has_pin(last.free_pin()) &&
            current.iter().find(|&&other| other.l == c.l && other.r == c.r).is_none()) {
            let mut next = current.clone();
            let mut copy = new.clone();
            copy.attach(&last);
            next.push(copy);
            stack.push(next);
        }
    }
    longest_sum
}

pub fn parser(input: &str) -> Vec<Component> {
    let mut v = Vec::new();
    for line in input.lines() {
        let (l, r) = line.split('/').map(|d| d.parse().unwrap()).next_tuple().unwrap();
        let component = Component { l, r, occupied: if l * r == 0 { Some(0) } else { None } };
        v.push(component);
    }
    v
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y17d24test() {
        let s = indoc! {"
            0/2
            2/2
            2/3
            3/4
            3/5
            0/1
            10/1
            9/10
        "};
        assert_eq!(part1(s), 31);
    }
}