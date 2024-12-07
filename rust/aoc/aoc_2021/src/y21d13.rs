use itertools::Itertools;
use regex::Regex;

enum FoldType {
    Up,
    Left,
}

#[aoc(day13, part1)]
pub fn part1(input: &str) -> usize {
    let (map, folds) = parser(input);
    let width = map.iter().map(|&(x, _)| x).max().unwrap();
    let height = map.iter().map(|&(_, y)| y).max().unwrap();
    (match folds[0].1 {
        FoldType::Up => { fold_up(map, folds[0].0, height) }
        FoldType::Left => { fold_left(map, folds[0].0, width) }
    }).len()
}

#[aoc(day13, part2)]
pub fn part2(input: &str) -> usize {
    let (mut map, folds) = parser(input);
    let mut width = map.iter().map(|&(x, _)| x).max().unwrap();
    let mut height = map.iter().map(|&(_, y)| y).max().unwrap();
    for (axis, fold_type) in folds {
        match fold_type {
            FoldType::Up => { map = fold_up(map, axis, height); height = height / 2 - 1; }
            FoldType::Left => { map = fold_left(map, axis, width); width = width / 2 - 1; }
        }
    }
    print_map(map, height, width);
    1
}

fn print_map(map: Vec<(u32, u32)>, height: u32, width: u32) {
    for y in 0..=height {
        for x in 0..=width {
            print!("{}", if map.contains(&(x, y)) {'*'} else {' '});
        }
        println!();
    }
}

fn fold_up(map: Vec<(u32, u32)>, axis: u32, height: u32) -> Vec<(u32, u32)> {
    map.into_iter().map(|(x, y)| {
        if y < axis { (x, y) } else { (x, height - y) }
    }).unique().collect()
}

fn fold_left(map: Vec<(u32, u32)>, axis: u32, width: u32) -> Vec<(u32, u32)> {
    map.into_iter().map(|(x, y)| {
        if x < axis { (x, y) } else { (width - x, y) }
    }).unique().collect()
}


fn parser(input: &str) -> (Vec<(u32, u32)>, Vec<(u32, FoldType)>) {
    let (points, folds) = input.split("\n\n").next_tuple().unwrap();
    let re = Regex::new(r"(\d+)").unwrap();
    (
        points.lines().flat_map(|l| {
            l.split(",").flat_map(str::parse).next_tuple()
        }).collect(),
        folds.lines().map(|l| {
            let direction = match l.chars().nth(11).unwrap() {
                'x' => FoldType::Left,
                'y' => FoldType::Up,
                _ => panic!(),
            };
            let axis = re.find(l).unwrap().as_str().parse().unwrap();
            (axis, direction)
        }).collect()
    )
}

#[cfg(test)]
mod tests {

    #[test]
    pub fn y21d13test() {
    }
}