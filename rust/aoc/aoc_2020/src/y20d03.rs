use crate::grid::Grid;

#[aoc(day3, part1)]
pub fn part1(input: &str) -> usize {
    let grid = parser(input);
    solve(&grid, 3, 1)
}

#[aoc(day3, part2)]
pub fn part2(input: &str) -> usize {
    let grid = parser(input);
    let mut product = 1;
    for &(x, y) in &[(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] {
        product *= solve(&grid, x, y);
    }
    product
}

fn parser(input: &str) -> Grid<char> {
    Grid::from_maze_matrix(input.lines()
                               .map(|l| l.trim().chars().collect::<Vec<_>>())
                               .collect::<Vec<_>>(), |_| false)
}

fn solve(grid: &Grid<char>, x: isize, y: isize) -> usize {
    let mut trees = 0;
    let bounds = grid.get_bounds();
    let maxx = bounds[2] + 1;
    let maxy = bounds[3] + 1;
    let (mut i, mut j) = (x, y);
    loop {
        if let Some(c) = grid.get(i % maxx, j) {
            if *c == '#' {
                trees += 1;
            }
            i += x;
            j += y;
            if j > maxy {
                break trees;
            }
        } else {
            break trees;
        }
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;
    use super::*;
    #[test]
    pub fn y20d03test() {
        let s = indoc!{"\
        ..##.......
        #...#...#..
        .#....#..#.
        ..#.#...#.#
        .#...##..#.
        ..#.##.....
        .#.#.#....#
        .#........#
        #.##...#...
        #...##....#
        .#..#...#.#
        "};
        assert_eq!(part1(s), 7);
        assert_eq!(part2(s), 336);
    }
}