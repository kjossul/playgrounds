use crate::grid::*;

#[aoc(day12, part1)]
pub fn part1(input: &str) -> isize {
    let instructions = parser(input);
    let mut direction = Direction::E;
    let (mut x, mut y) = (0, 0);
    for (c, steps) in instructions {
        let facing = match c {
            'E' => Direction::E,
            'N' => Direction::N,
            'W' => Direction::W,
            'S' => Direction::S,
            'F' => direction,
            'L' | 'R' => {
                let turns = steps as usize / 90;
                for _ in 0..turns {
                    if c == 'L' {
                        direction = Direction::turn_left(direction);
                    } else {
                        direction = Direction::turn_right(direction);
                    }
                }
                continue;
            }
            _ => panic!(),
        };
        for _ in 0..steps as usize {
            let t = get_adjacent_coord(x, y, facing);
            x = t.0;
            y = t.1;
        }
    }
    manh_distance(0, 0, x, y)
}

#[aoc(day12, part2)]
pub fn part2(input: &str) -> i32 {
    let instructions = parser(input);
    let (mut x, mut y) = (0, 0);
    let (mut w_x, mut w_y) = (10, 1);
    for (c, steps) in instructions {
        match (c, steps) {
            ('N', _) => w_y += steps,
            ('S', _) => w_y -= steps,
            ('E', _) => w_x += steps,
            ('W', _) => w_x -= steps,
            ('R', 90) | ('L', 270) => {
                std::mem::swap(&mut w_x, &mut w_y);
                w_y *= -1;
            },
            ('R', 180) | ('L', 180) => {
                w_x *= -1;
                w_y *= -1;
            },
            ('R', 270) | ('L', 90) => {
                std::mem::swap(&mut w_x, &mut w_y);
                w_x *= -1;
            },
            ('F', _) => {
                x += w_x * steps;
                y += w_y * steps;
            },
            _ => panic!(),
        }

    }
    x.abs() + y.abs()
}

fn parser(input: &str) -> Vec<(char, i32)> {
    input.lines().map(|l| {
        let c = l.chars().next();
        (c.unwrap(), l[1..].parse().unwrap())
    }).collect()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d12test() {
        let s = indoc!{"\
            F10
            N3
            F7
            R90
            F11
        "};
        assert_eq!(part1(s), 25);
        assert_eq!(part2(s), 286);
    }
}
