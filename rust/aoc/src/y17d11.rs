use std::cmp;

#[aoc(day11, part1)]
pub fn part1(input: &str) -> i32 {
    let (d, _) = solver(input);
    d
}

#[aoc(day11, part2)]
pub fn part2(input: &str) -> i32 {
    let (_, max) = solver(input);
    max
}

fn solver(input: &str) -> (i32, i32) {
    let (mut dl, mut dr) = (0i32, 0i32);  // diagonal left and right
    let mut max = 0;
    for d in input.split(',') {
        match d {
            "ne" => dl += 1,
            "nw" => dr += 1,
            "sw" => dl -= 1,
            "se" => dr -= 1,
            "n" => {
                dl += 1;
                dr += 1;
            },
            "s" => {
                dl -= 1;
                dr -= 1;
            },
            _ => panic!("Unexpected token"),
        }
        max = cmp::max(max, hex_distance(dl, dr));
    }
    (hex_distance(dl, dr), max)
}

fn hex_distance(dl: i32, dr: i32) -> i32 {
    let l = dl.abs();
    let r = dr.abs();
    if dl * dr < 0 {
        l + r
    } else {
        cmp::max(l, r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn y17d11test() {
        assert_eq!(part1("se,sw,se,sw,sw"), 3);
        assert_eq!(part1("se,se,se,n"), 3);
    }
}