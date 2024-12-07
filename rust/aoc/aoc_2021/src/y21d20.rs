use itertools::Itertools;
use crate::grid::Grid;

#[aoc(day20, part1)]
pub fn part1(input: &str) -> Option<usize> {
    let (algo, img) = parser(input);
    let (img, outside_char) = converter(&algo, img, '0', 3);
    let (img, _) = converter(&algo, img, outside_char, 3);
    Some(img.cells.values().filter(|&&c| c == '1').count())
}

#[aoc(day20, part2)]
pub fn part2(input: &str) -> Option<usize> {
    let (algo, mut img) = parser(input);
    let mut outside_char = '0';
    for _ in 0..50 {
        let t = converter(&algo, img, outside_char, 3);
        img = t.0;
        outside_char = t.1;
    }
    Some(img.cells.values().filter(|&&c| c == '1').count())
}

fn parser(input: &str) -> (String, Grid<char>) {
    let s = input.replace(".", "0").replace("#", "1");
    let (a, b) = s.split("\n\n").next_tuple().unwrap();
    (a.to_string(), Grid::from_string_grid(b.clone(), |c| c))
}

fn converter(algo: &String, img: Grid<char>, outside_char: char, kernel_size: isize) -> (Grid<char>, char) {
    let outside_vec = vec![outside_char; 9];
    let new_outside_char = algo.chars().nth(vec_to_binary(&outside_vec)).unwrap();
    let mut out = Grid::new();
    let bounds = img.get_bounds();
    let size_increase = kernel_size / 2 + 1;
    for y in 0..=bounds[3] + size_increase {
        for x in 0..=bounds[2] + size_increase {
            let mut v = vec![];
            for j in 0..kernel_size {
                for i in 0..kernel_size {
                    v.push(*img.get(x - 2 + i, y - 2 + j).unwrap_or(&outside_char));
                }
            }
            let n = vec_to_binary(&v);
            out.insert(x, y, algo.chars().nth(n).unwrap());
        }
    }
    (out, new_outside_char)
}

fn vec_to_binary(v: &Vec<char>) -> usize {
    usize::from_str_radix(v.iter().collect::<String>().as_str(), 2).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d20test() {
        let s = indoc!{"\
            ..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

            #..#.
            #....
            ##..#
            ..#..
            ..###
        "};
        assert_eq!(part1(s), Some(35));
        assert_eq!(part2(s), Some(3351));
    }
}