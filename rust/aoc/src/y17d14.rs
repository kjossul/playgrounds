use crate::grid::Grid;
use crate::y17d10;
use petgraph::algo::connected_components;

#[aoc(day14, part1)]
pub fn part1(input: &str) -> usize {
    let result = solver(input);
    result.into_iter().flatten().filter(|&s| s == '1').count()
}

#[aoc(day14, part2)]
pub fn part2(input: &str) -> usize {
    let result = solver(input);
    let maze = Grid::from_maze_matrix(result, |&c| c == '0');
    let graph_map = maze.get_maze_graph();
    connected_components(&graph_map)
}

fn solver(input: &str) -> Vec<Vec<char>> {
    let mut v = Vec::<Vec<char>>::new();
    for i in 0..128 {
        let key = format!("{}-{}", &input, i);
        let hash = y17d10::part2(&key);
        v.push(hash.chars().flat_map(|c| hex_to_bin(c).chars()).collect::<Vec<char>>());
    }
    v
}

fn hex_to_bin<'a>(c: char) -> &'a str {
    match c {
        '0' => "0000",
        '1' => "0001",
        '2' => "0010",
        '3' => "0011",
        '4' => "0100",
        '5' => "0101",
        '6' => "0110",
        '7' => "0111",
        '8' => "1000",
        '9' => "1001",
        'a' => "1010",
        'b' => "1011",
        'c' => "1100",
        'd' => "1101",
        'e' => "1110",
        'f' => "1111",
        _ => panic!("Unexpected token")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y17d14test() {
        let key = "flqrgnkx";
        assert_eq!(part1(key), 8108);
        assert_eq!(part2(key), 1242);
    }
}