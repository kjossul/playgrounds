use regex::Regex;

enum Direction {
    Forward(i32),
    Up(i32),
    Down(i32),
}

#[aoc(day2, part1)]
pub fn part1(input: &str) -> i32 {
    let directions = parser(input);
    let (mut x, mut y) = (0, 0);
    for direction in directions {
        match direction {
            Direction::Forward(v) => x += v,
            Direction::Down(v) => y += v,
            Direction::Up(v) => y -= v,
        }
    };
    x * y
}

#[aoc(day2, part2)]
pub fn part2(input: &str) -> i32 {
    let directions = parser(input);
    let (mut x, mut y, mut aim) = (0, 0, 0);
    for direction in directions {
        match direction {
            Direction::Forward(v) => {
                x += v;
                y += aim * v;
            },
            Direction::Down(v) => aim += v,
            Direction::Up(v) => aim -= v,
        }
    };
    x * y
}

fn parser(input: &str) -> Vec<Direction> {
    let re = Regex::new(r"(\w+) (\d+)").unwrap();
    input.lines().map(|l| {
        let caps = re.captures(l).unwrap();
        let k = String::from(&caps[1]);
        let v = caps[2].parse().unwrap();
        match k.as_str() {
            "forward" => Direction::Forward(v),
            "up" => Direction::Up(v),
            "down" => Direction::Down(v),
            _ => panic!(),
        }
    }).collect()
}