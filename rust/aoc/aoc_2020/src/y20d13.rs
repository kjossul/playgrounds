use itertools::Itertools;

#[aoc(day13, part1)]
pub fn part1(input: &str) -> u32 {
    let (ts, services) = parser(input);
    services.iter().map(|opt| {
        if let Some(bus) = opt {
            let offset = ts % bus;
            (bus - offset, *bus)
        } else {
            (u32::MAX, 0)
        }
    }).sorted().map(|(t, b)| t * b).next().unwrap()
}

#[aoc(day13, part2)]
pub fn part2(input: &str) -> u32 {
    let (_, services) = parser(input);
    for (i, service) in services.iter().enumerate() {
        if let Some(bus) = service {
            dbg!((i, bus));
        }
    }
    // https://www.wolframalpha.com/input/?i=t+mod+29+%3D+0%2C+%28t+%2B+23%29+mod+37++%3D+0%2C+%28t+%2B+29%29+mod+631++%3D+0%2C+%28t+%2B+47%29+mod+13++%3D+0%2C+%28t+%2B+48%29+mod+19+%3D+0%2C+%28t+%2B+52%29+mod+23+%3D+0%2C+%28t+%2B+60%29+mod+383++%3D+0%2C+%28t+%2B+70%29+mod+41++%3D+0%2C+%28t+%2B+77%29+mod+17++%3D+0
    // all my homies hate esoteric math problems
    0
}

fn parser(input: &str) -> (u32, Vec<Option<u32>>) {
    let mut lines = input.lines();
    let ts = lines.next().unwrap().parse().unwrap();
    let services = lines.next().unwrap().split(',').map(|s| s.parse().ok()).collect();
    (ts, services)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d12test() {}
}
