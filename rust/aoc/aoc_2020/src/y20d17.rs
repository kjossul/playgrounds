use itertools::iproduct;
use std::collections::HashMap;

// thanks https://gitlab.com/mboehnke/aoc-2020/-/blob/master/aoc-2020-17/src/solution.rs

struct Grid {
    bounds: (
        (isize, isize),
        (isize, isize),
        (isize, isize),
        (isize, isize),
    ),
    cubes: HashMap<Pos, bool>,
}

type Pos = (isize, isize, isize, isize);

#[aoc(day17, part1)]
pub fn part1(input: &str) -> usize {
    Grid::from(input).cycles(6, false).active()
}

#[aoc(day17, part2)]
pub fn part2(input: &str) -> usize {
    Grid::from(input).cycles(6, true).active()
}

impl From<&str> for Grid {
    fn from(s: &str) -> Self {
        let cubes = s
            .lines()
            .enumerate()
            .flat_map(|(y, l)| {
                l.chars()
                    .enumerate()
                    .map(move |(x, c)| ((x as isize, y as isize, 0, 0), c == '#'))
            })
            .collect::<HashMap<_, _>>();
        let (max_x, max_y) = cubes
            .keys()
            .fold((0, 0), |(mx, my), &(x, y, _, _)| (mx.max(x), my.max(y)));
        Grid {
            bounds: ((0, max_x), (0, max_y), (0, 0), (0, 0)),
            cubes,
        }
    }
}

impl Grid {
    fn active(&self) -> usize {
        self.cubes.values().filter(|x| **x).count()
    }

    fn cycles(mut self, n: usize, d4: bool) -> Self {
        (0..n).for_each(|_| self.next(d4));
        self
    }

    fn neighbors((x, y, z, w): Pos) -> impl Iterator<Item = Pos> {
        iproduct!(x - 1..=x + 1, y - 1..=y + 1, z - 1..=z + 1, w - 1..=w + 1)
            .filter(move |p| *p != (x, y, z, w))
    }

    fn cube_next(&self, pos: Pos) -> bool {
        let n = Self::neighbors(pos)
            .filter(|pos| *self.cubes.get(pos).unwrap_or(&false))
            .count();
        let c = *self.cubes.get(&pos).unwrap_or(&false);
        n == 3 || (c && n == 2)
    }

    fn bounds_next(&mut self) {
        let ((min_x, max_x), (min_y, max_y), (min_z, max_z), (min_w, max_w)) = self.bounds;
        self.bounds = (
            (min_x - 1, max_x + 1),
            (min_y - 1, max_y + 1),
            (min_z - 1, max_z + 1),
            (min_w - 1, max_w + 1),
        );
    }

    fn next(&mut self, d4: bool) {
        self.bounds_next();
        let ((min_x, max_x), (min_y, max_y), (min_z, max_z), (mut min_w, mut max_w)) = self.bounds;
        if !d4 {
            min_w = 0;
            max_w = 0
        }
        self.cubes = iproduct!(min_x..=max_x, min_y..=max_y, min_z..=max_z, min_w..=max_w)
            .map(|pos| (pos, self.cube_next(pos)))
            .collect::<HashMap<_, _>>();
    }
}
