use std::collections::HashMap;

#[derive(Default, Debug)]
struct Program {
    registers: HashMap<String, i64>,
    pc: i64,
    frequency: i64,
    parsed_int: i64,
    is_waiting: bool,
    queue: Vec<i64>,
}

struct Instruction {
    f: fn(program: &mut Program, args: &[String]) -> Option<i64>,
    args: Vec<String>,
}

impl Program {
    fn step(&mut self, instructions: &[Instruction]) -> Option<i64> {
        if let Some(instruction) = instructions.get(self.pc as usize) {
            self.pc += 1;
            (instruction.f)(self, &instruction.args)
        } else {
            Some(self.frequency)
        }

    }

    fn set(&mut self, args: &[String]) -> Option<i64> {
        *self.get_value(&args[0]) = *self.get_value(&args[1]);
        None
    }

    fn add(&mut self, args: &[String]) -> Option<i64> {
        *self.get_value(&args[0]) += *self.get_value(&args[1]);
        None
    }

    fn mul(&mut self, args: &[String]) -> Option<i64> {
        *self.get_value(&args[0]) *= *self.get_value(&args[1]);
        None
    }

    fn modulus(&mut self, args: &[String]) -> Option<i64> {
        *self.get_value(&args[0]) %= *self.get_value(&args[1]);
        None
    }

    fn jgz(&mut self, args: &[String]) -> Option<i64> {
        if *self.get_value(&args[0]) > 0 {
            self.pc += *self.get_value(&args[1]) - 1;
        }
        None
    }

    fn snd1(&mut self, args: &[String]) -> Option<i64> {
        self.frequency = *self.get_value(&args[0]);
        None
    }

    fn rcv1(&mut self, args: &[String]) -> Option<i64> {
        if *self.get_value(&args[0]) != 0 {
            Some(self.frequency)
        } else {
            None
        }
    }

    fn snd2(&mut self, args: &[String]) -> Option<i64> {
        Some(*self.get_value(&args[0]))
    }

    fn rcv2(&mut self, args: &[String]) -> Option<i64> {
        if let Some(i) = self.queue.pop() {
            *self.get_value(&args[0]) = i;
            self.is_waiting = false;
        } else {
            self.pc -= 1;
            self.is_waiting = true;
        }
        None
    }


    fn get_value(&mut self, arg: &String) -> &mut i64 {
        if let Ok(x) = arg.as_str().parse::<i64>() {
            self.parsed_int = x;
            &mut self.parsed_int
        } else {
            self.registers.entry(arg.clone()).or_insert(0)
        }
    }
}

#[aoc(day18, part1)]
pub fn part1(input: &str) -> i64 {
    let (mut program, instructions) = parse_program(input, false);
    loop {
        match program.step(&instructions) {
            Some(i) => break i,
            None => {}
        }
    }
}

#[aoc(day18, part2)]
pub fn part2(input: &str) -> i64 {
    let mut counter = 0;
    let (mut program0, instructions1) = parse_program(input, true);
    program0.registers.insert(String::from("p"), 0);
    let (mut program1, instructions2) = parse_program(input, true);
    program1.registers.insert(String::from("p"), 1);
    while !program0.is_waiting || !program1.is_waiting {
        match program0.step(&instructions1) {
            Some(i) => {
                program1.queue.insert(0, i);
            },
            None => {}
        }
        match program1.step(&instructions2) {
            Some(i) => {
                counter += 1;
                program0.queue.insert(0, i);
            },
            None => {}
        }
    }
    counter
}

fn parse_program(input: &str, part2: bool) -> (Program, Vec<Instruction>) {
    let program = Program { ..Default::default() };
    let mut instructions = Vec::<Instruction>::new();
    for line in input.lines() {
        let mut it = line.split(' ');
        let f = match it.next().unwrap() {
            "snd" => if !part2 {Program::snd1} else {Program::snd2},
            "set" => Program::set,
            "add" => Program::add,
            "mul" => Program::mul,
            "mod" => Program::modulus,
            "rcv" => if !part2 {Program::rcv1} else {Program::rcv2},
            "jgz" => Program::jgz,
            x => panic!("Instruction not recognized, {}", x),
        };
        instructions.push(Instruction { f, args: it.map(|s| String::from(s)).collect() });
    }
    (program, instructions)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y17d18test() {
        let s1 = indoc! {"
            set a 1
            add a 2
            mul a a
            mod a 5
            snd a
            set a 0
            rcv a
            jgz a -1
            set a 1
            jgz a -2
        "};
        let s2 = indoc! {"
            snd 1
            snd 2
            snd p
            rcv a
            rcv b
            rcv c
            rcv d
        "};
        assert_eq!(part1(s1), 4);
        assert_eq!(part2(s2), 3);
    }
}