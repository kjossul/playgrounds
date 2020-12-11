use crate::grid::Grid;

#[aoc(day11, part1)]
pub fn part1(input: &str) -> usize {
    let mut grid = parser(input);
    loop {
        let mut new = grid.clone();
        for (&(x, y), c) in new.cells.iter_mut() {
            let adjacent_occupied = grid.neighbors(x, y).iter().filter(|&&&c| c == '#').count();
            match grid.get(x, y) {
                Some('L') => {
                    if adjacent_occupied == 0 {
                        *c = '#';
                    }
                },
                Some('#') => {
                    if adjacent_occupied >= 4 {
                        *c = 'L';
                    }
                },
                Some('.') => {},
                _ => panic!(),
            }
        }
        if grid == new {
             break grid.cells.values().filter(|&&v| v == '#').count();
        } else {
            grid = new;
        }
    }
}

#[aoc(day11, part2)]
pub fn part2(input: &str) -> usize {
    let mut grid = parser(input);
    loop {
        let mut new = grid.clone();
        for (&(x, y), c) in new.cells.iter_mut() {
            let adjacent_occupied = see_around(&grid, x, y);
            match grid.get(x, y) {
                Some('L') => {
                    if adjacent_occupied == 0 {
                        *c = '#';
                    }
                },
                Some('#') => {
                    if adjacent_occupied >= 5 {
                        *c = 'L';
                    }
                },
                Some('.') => {},
                _ => panic!(),
            }
        }
        if grid == new {
             break grid.cells.values().filter(|&&v| v == '#').count();
        } else {
            grid = new;
        }
    }
}

fn parser(input: &str) -> Grid<char> {
    Grid::from_string_grid(input)
}

fn see_around(grid: &Grid<char>, x: isize, y: isize) -> usize {
    let offsets = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)];
    offsets.iter().map(|(i, j)| {
        let mut step = 1;
        while let Some(c) = grid.get(x + i * step, y + j * step) {
            match c {
                'L' => return 0,
                '#' => return 1,
                '.' => step += 1,
                _ => panic!(),
            }
        }
        0
    }).sum()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d11test() {
        let s = indoc!{"\
            L.LL.LL.LL
            LLLLLLL.LL
            L.L.L..L..
            LLLL.LL.LL
            L.LL.LL.LL
            L.LLLLL.LL
            ..L.L.....
            LLLLLLLLLL
            L.LLLLLL.L
            L.LLLLL.LL
        "};
        assert_eq!(part2(s), 26);
    }
}
