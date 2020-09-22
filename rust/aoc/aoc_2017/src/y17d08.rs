use std::collections::HashMap;
use std::cmp;
use eval::{Expr, to_value};
use regex::{Regex, Captures};


#[derive(Debug)]
struct Program {
    registers: HashMap<String, i32>,
    maxval: Option<i32>,
}

impl Program {
    fn new() -> Program {
        Program { registers: HashMap::new(), maxval: None }
    }

    fn parse_lines(&mut self, input: &str) {
        let re = Regex::new(r"(\w+) (\w+) (\S+) if (\w+) (\S*) (\S+)").unwrap();
        for line in input.lines() {
            self.parse_command(re.captures(line).unwrap());
        }
    }

    fn parse_command(&mut self, captures: Captures) {
        let register = self.registers.entry(String::from(&captures[4])).or_insert(0);
        // eval cannot handle two operands in a row, so we pull all members to the left to avoid
        // expressions like x > -10, which are not allowed
        let mut operand: i32 = captures.get(6).unwrap().as_str().parse().unwrap();
        let formatted = format!("{} {}", if operand > 0 {"-"} else {"+"}, operand.abs());
        let s = format!("{} {} {} 0", &captures[4], formatted, &captures[5]);
        let condition = Expr::new(s).value(&captures[4], register).exec().unwrap();
        if condition == to_value(true) {
            operand = captures.get(3).unwrap().as_str().parse().unwrap();
            let assignee = self.registers.entry(String::from(&captures[1])).or_insert(0);
            match &captures[2] {
                "inc" => {
                    *assignee += operand;
                },
                "dec" => {
                    *assignee -= operand;
                },
                _ => panic!("Op not implemented!"),
            }
            self.maxval.replace(cmp::max(self.maxval.unwrap_or(*assignee), *assignee));
        }
    }


}

#[aoc(day8, part1)]
pub fn part1(input: &str) -> i32 {
    let mut program = Program::new();
    program.parse_lines(input);
    *program.registers.values().max().unwrap()
}

#[aoc(day8, part2)]
pub fn part2(input: &str) -> i32 {
    let mut program = Program::new();
    program.parse_lines(input);
    program.maxval.unwrap()
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn y17d08test() {
        let s = indoc! {"
            b inc 5 if a > 1
            a inc 1 if b < 5
            c dec -10 if a >= 1
            c inc -20 if c == 10
        "};

        assert_eq!(part1(s), 1);
        assert_eq!(part2(s), 10);
    }
}