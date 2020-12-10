use std::collections::HashSet;
use itertools::Itertools;

#[derive(SmartDefault)]
pub struct Program {
    acc: i32,
    pc: usize,
    instructions: Vec<(String, i32)>,
    visited: HashSet<usize>,
}

impl Program {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn exec(&mut self) -> Result<(), bool> {
        if self.visited.contains(&self.pc) {
            Err(false)
        } else {
            self.visited.insert(self.pc);
            if let Some(instruction) = self.instructions.get(self.pc) {
                let (opcode, n) = instruction;
                match opcode.as_str() {
                    "acc" => self.acc += n,
                    "jmp" => {
                        self.pc = (self.pc as i32 + n) as usize;
                        return Ok(());
                    },
                    "nop" => (),
                    _ => panic!(),
                }
                self.pc += 1;
                Ok(())
            } else {
                Err(true)
            }
        }
    }
}

#[aoc(day8, part1)]
pub fn part1(input: &str) -> i32 {
    let mut program = Program::new();
    program.instructions = parser(input);
    while let Ok(_) = program.exec() {}
    program.acc
}

#[aoc(day8, part2)]
pub fn part2(input: &str) -> i32 {
    let instructions = parser(input);
    for i in 0..instructions.len() {
        let mut copy = instructions.clone();
        let (ref mut opcode, _) = copy[i];
        if opcode == "nop" {
            *opcode = String::from("jmp");
        } else if opcode == "jmp" {
            *opcode = String::from("nop");
        }
        let mut program = Program::new();
        program.instructions = copy;
        loop {
            if let Err(success) = program.exec() {
                if success {
                    return program.acc;
                } else {
                    break;
                }
            }
        }
    }
    panic!();
}

pub fn parser(input: &str) -> Vec<(String, i32)> {
    input.lines().map(|s| {
        let (opcode, val) = s.split_whitespace().next_tuple().unwrap();
        (String::from(opcode), val.parse().unwrap())
    }).collect()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d08test() {}
}