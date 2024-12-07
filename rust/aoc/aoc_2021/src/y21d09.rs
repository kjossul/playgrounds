use crate::grid::Grid;
use petgraph::algo::kosaraju_scc;

#[aoc(day9, part1)]
pub fn part1(input: &str) -> Option<u32> {
    let grid = parser(input);
    let bounds = grid.get_bounds();
    let mut risk = 0;
    for y in 0..=bounds[3] {
        'mark: for x in 0..=bounds[2] {
            let point = grid.get(x, y)?;
            for (o, _) in grid.adjacent_neighbors(x, y) {
                if let Some(neighbor) = o {
                    if neighbor <= point {
                        continue 'mark;
                    }
                }
            }
            risk += *point + 1;
        }
    }
    Some(risk)
}

#[aoc(day9, part2)]
pub fn part2(input: &str) -> usize {
    let grid = parser_with_walls(input);
    let graph = grid.get_maze_graph();
    let basins = kosaraju_scc(&graph);
    let mut sizes: Vec<usize> = basins.into_iter().map(|v| v.len()).collect();
    sizes.sort();
    sizes.reverse();
    sizes[0] * sizes[1] * sizes[2]
}

fn parser(input: &str) -> Grid<u32> {
    let mat: Vec<Vec<u32>> = input.lines().map(|l| {
        l.chars().flat_map(|c| c.to_digit(10)).collect()
    }).collect();
    Grid::from_maze_matrix(mat, |_| false)
}

fn parser_with_walls(input: &str) -> Grid<u32> {
    let mat: Vec<Vec<u32>> = input.lines().map(|l| {
        l.chars().flat_map(|c| c.to_digit(10)).collect()
    }).collect();
    Grid::from_maze_matrix(mat, |&n| n == 9)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d09test() {
        let s = indoc! {"\
            2199943210
            3987894921
            9856789892
            8767896789
            9899965678
        "};

        assert_eq!(part1(s), Some(15));
        assert_eq!(part2(s), 1134);
    }
}