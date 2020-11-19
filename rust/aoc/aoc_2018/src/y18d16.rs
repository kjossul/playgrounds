use regex::Regex;
use std::collections::{HashMap};

struct Program {
    registers: Vec<usize>,
}

impl Program {
    fn new() -> Self { Self { registers: vec![0, 0, 0, 0] } }

    fn addr(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] + self.registers[ops[1]];
    }
    fn addi(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] + ops[1];
    }
    fn mulr(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] * self.registers[ops[1]];
    }
    fn muli(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] * ops[1];
    }
    fn banr(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] & self.registers[ops[1]];
    }
    fn bani(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] & ops[1];
    }
    fn borr(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] | self.registers[ops[1]];
    }
    fn bori(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]] | ops[1];
    }
    fn setr(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = self.registers[ops[0]];
    }
    fn seti(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = ops[0];
    }
    fn gtir(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = (ops[0] > self.registers[ops[1]]) as usize;
    }
    fn gtri(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = (self.registers[ops[0]] > ops[1]) as usize;
    }
    fn gtrr(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = (self.registers[ops[0]] > self.registers[ops[1]]) as usize;
    }
    fn eqir(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = (ops[0] == self.registers[ops[1]]) as usize;
    }
    fn eqri(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = (self.registers[ops[0]] == ops[1]) as usize;
    }
    fn eqrr(&mut self, ops: &[usize]) {
        self.registers[ops[2]] = (self.registers[ops[0]] == self.registers[ops[1]]) as usize;
    }
}

#[aoc(day16, part1)]
pub fn part1(input: &str) -> usize {
    let (diagnostics, _) = parser(input);
    let ops = [Program::addr, Program::addi, Program::mulr, Program::muli, Program::banr, Program::bani, Program::borr, Program::bori, Program::setr, Program::seti, Program::gtir, Program::gtri, Program::gtrr, Program::eqir, Program::eqri, Program::eqrr];
    diagnostics.iter().filter(|d| {
        let matching = ops.iter().filter(|op| {
            let mut program = Program { registers: d[0].clone() };
            op(&mut program, &d[1][1..]);
            program.registers == d[2]
        }).count();
        matching >= 3
    }).count()
}

#[aoc(day16, part2)]
pub fn part2(input: &str) -> usize {
    let (diagnostics, prog) = parser(input);
    let ops = [Program::addr, Program::addi, Program::mulr, Program::muli, Program::banr, Program::bani, Program::borr, Program::bori, Program::setr, Program::seti, Program::gtir, Program::gtri, Program::gtrr, Program::eqir, Program::eqri, Program::eqrr];
    let mut assocs = vec![];
    let mut i_to_op = HashMap::new();
    for (i, d) in diagnostics.iter().enumerate() {
        assocs.push(vec![]);
        for (j, op) in ops.iter().enumerate() {
            let mut program = Program { registers: d[0].clone() };
            op(&mut program, &d[1][1..]);
            if program.registers == d[2] {
                assocs[i].push((d[1][0], j));
            }
        }
    }
    while i_to_op.len() < 16 {
        assocs.sort_by_key(|v| v.len());
        assert_eq!(assocs[0].len(), 1);
        let (k, v) = assocs[0][0];
        i_to_op.insert(k, v);
        for vec in assocs.iter_mut() {
            vec.retain(|&(a, b)| a != k && b != v);
        }
        assocs.retain(|v| v.len() > 0);
    }
    let mut program = Program::new();
    for line in prog {
        let op = i_to_op[&line[0]];
        ops[op](&mut program, &line[1..]);
    }
    program.registers[0]
}

fn parser(input: &str) -> (Vec<Vec<Vec<usize>>>, Vec<Vec<usize>>) {
    let mut blocks = input.split("\n\n\n\n");
    let instructions = blocks.next().unwrap();
    let re = Regex::new(r"\d+").unwrap();
    let diagnostics = instructions.split("\n\n").map(|ls| {
        let mut v = vec![vec![], vec![], vec![]];
        for (i, m) in re.find_iter(ls).enumerate() {
            v[i / 4].push(m.as_str().parse().unwrap());
        }
        v
    }).collect();
    let program = blocks.next().unwrap().lines().map(|l| {
        re.find_iter(l).flat_map(|m| m.as_str().parse()).collect()
    }).collect();
    (diagnostics, program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d16test() {
        let s = indoc!{"\
            Before: [3, 2, 1, 1]
            9 2 1 2
            After:  [3, 2, 2, 1]
        "};
        assert_eq!(part1(s), 1);
    }
}