use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;

enum Instruction {
    Mask(Vec<Option<bool>>),
    Write(usize, u64),
}

#[aoc(day14, part1)]
pub fn part1(input: &str) -> u64 {
    let inss = parser(input);
    let mut mem = HashMap::new();
    let mut mask = vec![];
    for ins in inss {
        match ins {
            Instruction::Mask(v) => mask = v,
            Instruction::Write(i, mut n) => {
                for (j, o) in mask.iter().rev().enumerate() {
                    match o {
                        Some(true) => {
                            let bits = 1 << j;
                            n |= bits;
                        }
                        Some(false) => {
                            let bits = !(1 << j);
                            n &= bits;
                        }
                        None => {}
                    }
                }
                mem.insert(i, n);
            }
        }
    }
    mem.values().sum()
}

#[aoc(day14, part2)]
pub fn part2(input: &str) -> u64 {
    let inss = parser(input);
    let mut mem = HashMap::new();
    let mut mask = vec![];
    for ins in inss {
        match ins {
            Instruction::Mask(v) => {
                mask = v;
            }
            Instruction::Write(i, n) => {
                let mut addresses = vec![i];
                for (j, o) in mask.iter().rev().enumerate() {
                    match o {
                        Some(true) => {
                            let bits = 1 << j;
                            for addr in &mut addresses {
                                *addr |= bits;
                            }
                        }
                        Some(false) => {}
                        None => {
                            let bits = 1 << j;
                            for addr in addresses.clone() {
                                addresses.push(addr ^ bits)
                            }
                        }
                    }
                }
                for addr in addresses {
                    mem.insert(addr, n);
                }
            }
        }
    }
    mem.values().sum()
}

fn parser(input: &str) -> Vec<Instruction> {
    input.lines().map(|l| {
        let (a, b) = l.split(" = ").next_tuple().unwrap();
        if a == "mask" {
            let v = b.chars().map(|c| {
                match c {
                    '1' => Some(true),
                    '0' => Some(false),
                    'X' => None,
                    _ => panic!(),
                }
            }).collect();
            Instruction::Mask(v)
        } else {
            let re = Regex::new(r"\d+").unwrap();
            let i = re.find(a).unwrap().as_str().parse().unwrap();
            let n = b.parse().unwrap();
            Instruction::Write(i, n)
        }
    }).collect()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d14test() {
        let s = indoc! {"\
            mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
            mem[8] = 11
            mem[7] = 101
            mem[8] = 0
        "};
        assert_eq!(part1(s), 165);
        let s = indoc! {"\
            mask = 000000000000000000000000000000X1001X
            mem[42] = 100
            mask = 00000000000000000000000000000000X0XX
            mem[26] = 1
        "};
        assert_eq!(part2(s), 208);
    }
}
