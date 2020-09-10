use crate::grid::Grid;

macro_rules! unwrap_or_return {
    ( $e:expr ) => {
        match $e {
            Ok(x) => x,
            Err(x) => return x
        }
    };
}

#[aoc(day3, part1)]
pub fn part1(s: &str) -> i32 {
    let n: i32 = s.trim().parse().unwrap();
    // length of the square containing all the numbers (always odd)
    let dim = (1..)
        .filter(|n| n % 2 == 1)
        .find(|&l: &i32| l.pow(2) >= n).unwrap();
    if dim == 1 {
        return 0
    }
    let distance = dim / 2;
    let first_centre = (dim - 2).pow(2) + distance;  // first centre of the side of the outer circle
    // this are the numbers in the middle of each side
    let side_centers: Vec<i32> = (0..=3).map(|i| first_centre + i * (dim - 1)).collect();
    // sums the distance from the outer square to the distance to the closer center
    distance + side_centers
        .iter()
        .map(|m| i32::abs(m - n))
        .min().unwrap()
}

#[aoc(day3, part2)]
pub fn part2(s: &str) -> i32 {
    let n: i32 = s.trim().parse().unwrap();
    let mut grid: Grid<i32> = Grid::new();
    grid.insert(0, 0, 1);
    let mut circle = 1;
    loop {
        // right side
        for j in (-circle..circle).rev() {
            let m = unwrap_or_return!(sum_neighbors(&grid, circle, j, n));
            grid.insert(circle, j, m);
        }
        // top side
        for i in (-circle..circle).rev() {
            let m = unwrap_or_return!(sum_neighbors(&grid, i, -circle, n));
            grid.insert(i, -circle, m);
        }
        // left side
        for j in -circle+1..circle {
            let m = unwrap_or_return!(sum_neighbors(&grid, -circle, j, n));
            grid.insert(-circle, j, m);
        }
        // bottom side
        for i in -circle..=circle {
            let m = unwrap_or_return!(sum_neighbors(&grid, i, circle, n));
            grid.insert(i, circle, m);
        }
        circle += 1;
    }
}

fn sum_neighbors(grid: &Grid<i32>, i: isize, j: isize, n: i32) -> Result<i32, i32> {
    let sum = grid.neighbors(i, j).into_iter().sum();
    if sum > n { Err(sum) } else { Ok(sum) }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!(part1("1"), 0);
        assert_eq!(part1("12"), 3);
        assert_eq!(part1("23"), 2);
        assert_eq!(part1("1024"), 31);
    }
}