use regex::Regex;

#[derive(Debug, Eq, PartialEq)]
enum Outcome {
    Success(i32),
    Miss,
}

#[aoc(day17, part1)]
pub fn part1(input: &str) -> Option<i32> {
    let bounds = parser(input)?;
    Some(probe_tuner(&bounds))
}

#[aoc(day17, part2)]
pub fn part2(input: &str) -> Option<i32> {
    let bounds = parser(input)?;
    let mut successes = 0;
    for vx in 0..=bounds[1] {
        for vy in bounds[2]..=-bounds[2] {
            match simulate(vx, vy, &bounds) {
                Outcome::Miss => { continue; }
                Outcome::Success(_) => { successes += 1; }
            }
        }
    }
    Some(successes)
}

fn probe_tuner(bounds: &Vec<i32>) -> i32 {
    let mut highest_y = 0;
    for vx in 0..bounds[1] {
        for vy in 0..=-bounds[2] {
            match simulate(vx, vy, bounds) {
                Outcome::Miss => { continue; }
                Outcome::Success(y) => { highest_y = i32::max(y, highest_y); }
            }
        }
    }
    highest_y
}

fn simulate(mut vx: i32, mut vy: i32, bounds: &Vec<i32>) -> Outcome {
    let (mut x, mut y) = (0, 0);
    let mut y_max = 0;
    let mut x_ever_on_target = false;
    loop {
        x += vx;
        y += vy;
        y_max = i32::max(y_max, y);
        vx = i32::max(0, vx - 1);
        vy -= 1;
        let x_currently_on_target = x >= bounds[0] && x <= bounds[1];
        let y_currently_on_target = y >= bounds[2] && y <= bounds[3];
        x_ever_on_target = x_ever_on_target || x_currently_on_target;
        if x_currently_on_target && y_currently_on_target {
            break Outcome::Success(y_max);
        } else if y < bounds[2] && vx == 0 {
            break Outcome::Miss;
        }
    }
}

fn parser(input: &str) -> Option<Vec<i32>> {
    let re = Regex::new(r"-?\d+").ok()?;
    Some(re.find_iter(input).flat_map(|mat| mat.as_str().parse()).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y21d17test() {
        let s = "target area: x=20..30, y=-10..-5";
        let bounds = parser(s).unwrap();
        assert_eq!(simulate(7, 2, &bounds), Outcome::Success(3));
        assert_eq!(simulate(6, 3, &bounds), Outcome::Success(6));
        assert_eq!(simulate(6, 9, &bounds), Outcome::Success(45));
        assert_eq!(simulate(17, -4, &bounds), Outcome::Miss);
        assert_eq!(simulate(6, 10, &bounds), Outcome::Miss);
        assert_eq!(simulate(6, 13, &bounds), Outcome::Miss);
        assert_eq!(part1(s), Some(45));
        assert_eq!(part2(s), Some(112));
    }
}