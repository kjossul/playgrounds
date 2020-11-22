struct Program {
    registers: Vec<usize>,
    pc_i: usize,
}

impl Program {
    fn new() -> Self { Self { registers: vec![0, 0, 0, 0, 0, 0], pc_i: 0 } }

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

#[aoc(day19, part1)]
pub fn part1(input: &str) -> usize {
    let (mut program, instructions) = parser(input);
    let ops = [Program::addr, Program::addi, Program::mulr, Program::muli, Program::banr, Program::bani, Program::borr, Program::bori, Program::setr, Program::seti, Program::gtir, Program::gtri, Program::gtrr, Program::eqir, Program::eqri, Program::eqrr];
    let mut pc = 0;
    loop {
        if pc >= instructions.len() {
            return program.registers[0];
        }
        let instruction = &instructions[pc];
        program.registers[program.pc_i] = pc;
        ops[instruction[0]](&mut program, &instruction[1..]);
        pc = program.registers[program.pc_i];
        pc += 1;
    }
}


#[aoc(day19, part2)]
pub fn part2(_input: &str) -> usize {
    /*
    By analyzing the instruction dump, the program behaves as following:
    r2 initialized to some value (919 in part1, 10551319 in part2 for me)
    r0 = 0;
    r4 = 1;
    do {
        r5 = 1;
        do {
            if r4 * r5 == r2 {
                r0 += r4;
            }
            r5 += 1;
        } while r5 <= r2;
        r4 += 1;
    } while r4 <= r2;
    Basically the result is the sum of all natural divisors of n
    */
    let n = 10551319;
    let mut s = 0;
    for i in 1..(n as f32).sqrt() as usize {
        if n % i == 0 {
            s += i + n / i;
        }
    }
    s
}



fn parser(input: &str) -> (Program, Vec<Vec<usize>> ) {
    let ops = ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"];
    let mut lines = input.lines();
    let pc_i = lines.next().unwrap().split_whitespace().last().unwrap().parse().unwrap();
    let mut program = Program::new();
    program.pc_i = pc_i;
    let mut instructions = Vec::new();
    for line in lines {
        let mut chunks = line.split_whitespace();
        let op = &chunks.next().unwrap();
        let mut v = vec![ops.iter().position(|s| s == op).unwrap()];
        v.extend(chunks.flat_map(|d| d.parse::<usize>()));
        instructions.push(v);
    }
    (program, instructions)
}
