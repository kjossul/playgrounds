use itertools::Itertools;
use regex::Regex;
use crate::grid::Grid;

pub struct Point {
    sx: isize,
    sy: isize,
    vx: isize,
    vy: isize,
}

impl Point {
    pub fn step(&mut self) {
        self.sx += self.vx;
        self.sy += self.vy;
    }
}

pub fn gen(input: &str) -> Vec<Point> {
    let re = Regex::new(r"<(.*?),(.*?)>.*<(.*?),(.*?)>").unwrap();
    let mut points = Vec::new();
    for line in input.lines() {
        let caps = re.captures(line).unwrap();
        let (sx, sy, vx, vy) = caps.iter().skip(1)
            .flat_map(|s| s.unwrap().as_str().trim().parse()).next_tuple().unwrap();
        points.push(Point { sx, sy, vx, vy });
    }
    points
}

#[aoc(day10, part1)]
pub fn part1(input: &str) -> isize {
    let mut points = gen(input);
    let mut i = 0;
    loop {
        let mut grid = Grid::new();
        for point in &mut points {
            point.step();
            grid.insert(point.sx, point.sy, '#');
        }
        i += 1;
        let bounds = grid.get_bounds();
        if bounds[2] - bounds[0] == 61 {
            grid.print(|opt| *opt.unwrap_or(&' '));
            break;
        }
    }
    i
}