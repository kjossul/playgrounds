use crate::grid::{Grid, Direction};
use counter::Counter;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
struct Cart {
    y: isize,
    x: isize,
    direction: Direction,
    turns: usize,
}

impl Cart {
    fn tick(&mut self, grid: &Grid<char>) {
        let coords = grid.adjacent_coords(self.x, self.y);
        if let Some(new) = coords.iter().find(|(_, _, dir)| *dir == self.direction) {
            // continue in this direction
            self.x = new.0;
            self.y = new.1;
        } else {
            panic!("Cart is facing wrong direction!");
        }
        let c = *grid.get(self.x, self.y).unwrap();
        if c == '+' {
            match self.turns {
                0 => self.direction = Direction::turn_left(self.direction),
                2 => self.direction = Direction::turn_right(self.direction),
                _ => {}
            };
            self.turns = (self.turns + 1) % 3;
        } else {
            self.turn(c);
        }
    }

    fn turn(&mut self, corner: char) {
        match (self.direction, corner) {
            (Direction::E, '\\') | (Direction::W, '\\') | (Direction::N, '/') | (Direction::S, '/')
            => self.direction = Direction::turn_right(self.direction),
            (Direction::E, '/') | (Direction::W, '/') | (Direction::N, '\\') | (Direction::S, '\\')
            => self.direction = Direction::turn_left(self.direction),
            _ => {}
        }
    }

    fn collides(&self, other: &Cart) -> bool {
        self.x == other.x && self.y == other.y
    }
}

#[aoc(day13, part1)]
pub fn part1(input: &str) -> String {
    let (grid, mut carts) = parser(input);
    for _ in 1.. {
        for cart in &mut carts {
            cart.tick(&grid);
        }
        let (coords, count) = carts.iter().map(|cart| (cart.x, cart.y)).collect::<Counter<_>>()
            .most_common()[0];
        if count > 1 {
            return format!("{:?}", coords);
        }
    }
    panic!();
}

#[aoc(day13, part2)]
pub fn part2(input: &str) -> String {
    let (grid, mut carts) = parser(input);
    dbg!(carts.len());
    while carts.len() > 1 {
        let mut new_carts = Vec::new();
        for cart in &mut carts {
            let mut collides = false;
            new_carts.retain(|other| {
                let x = cart.collides(other);
                collides = collides || x;
                !x
            });
            if collides {
                continue;
            }
            cart.tick(&grid);
            new_carts.retain(|other| {
                let x = cart.collides(other);
                collides = collides || x;
                !x
            });
            if !collides {
                new_carts.push(*cart);
            }
        }
        carts = new_carts;
        carts.sort();
    }
    let remaining = carts[0];
    format!("{},{}", remaining.x, remaining.y)
}

fn parser(input: &str) -> (Grid<char>, Vec<Cart>) {
    let mut grid = Grid::new();
    let mut carts = Vec::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == ' ' {
                continue;
            }
            grid.insert(x as isize, y as isize, c);
            match c {
                '>' => {
                    carts.push(Cart { x: x as isize, y: y as isize, direction: Direction::E, turns: 0 });
                }
                '^' => {
                    carts.push(Cart { x: x as isize, y: y as isize, direction: Direction::N, turns: 0 });
                }
                '<' => {
                    carts.push(Cart { x: x as isize, y: y as isize, direction: Direction::W, turns: 0 });
                }
                'v' => {
                    carts.push(Cart { x: x as isize, y: y as isize, direction: Direction::S, turns: 0 });
                }
                _ => {}
            }
        }
    }
    (grid, carts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y18d13test() {
        let map1 = r"/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/";
        let map2 = r"/>-<\
|   |
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/";
        assert_eq!(part1(map1), String::from("(7, 3)"));
        assert_eq!(part2(map2), String::from("6,4"));
    }
}