use petgraph::algo::astar;
use petgraph::visit::EdgeRef;
use crate::grid::Grid;

#[aoc(day15, part1)]
pub fn part1(input: &str) -> Option<u32> {
    let grid = parser(input);
    solver(&grid)
}

#[aoc(day15, part2)]
pub fn part2(input: &str) -> Option<u32> {
    let mut grid = parser(input);
    let mut bounds = grid.get_bounds();
    for i in 0..4 {
        for x in bounds[0]..=bounds[2] {
            for y in bounds[1]..=bounds[3] {
                let v = *grid.get(x, y + (bounds[3] + 1) * i)?;
                grid.insert(x, y + (bounds[3] + 1) * (i + 1), if v == 9 {1} else {v + 1});
            }
        }
    }
    bounds = grid.get_bounds();
    for i in 0..4 {
        for x in bounds[0]..=bounds[2] {
            for y in bounds[1]..=bounds[3] {
                let v = *grid.get(x + (bounds[2] + 1) * i, y)?;
                grid.insert(x + (bounds[2] + 1) * (i + 1), y, if v == 9 {1} else {v + 1});
            }
        }
    }
    solver(&grid)
}

fn solver(grid: &Grid<u32>) -> Option<u32> {
    let bounds = grid.get_bounds();
    let graph = grid.get_maze_graph();
    let shortest = astar(&graph, (0, 0), |n| n == (bounds[2], bounds[3]),
                         |e| *grid.cells.get(&e.target()).unwrap(), |_| 0);
    shortest.and_then(|t| Some(t.0))
}


fn parser(input: &str) -> Grid<u32> {
    Grid::from_string_grid(input, |c| c.to_digit(10).unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d15test() {
        let s = indoc!{"\
            1163751742
            1381373672
            2136511328
            3694931569
            7463417111
            1319128137
            1359912421
            3125421639
            1293138521
            2311944581
        "};
        assert_eq!(part1(s), Some(40));
        assert_eq!(part2(s), Some(315));
    }
}