use crate::y17d18::{parse_program};
use primes::is_prime;

#[aoc(day23, part1)]
pub fn part1(input: &str) -> i32 {
    let (mut program, instructions) = parse_program(input, false);
    loop {
        match program.step(&instructions) {
            Some(_) => {
                dbg!(program.registers.get(&String::from('h')));
                break program.nmuls;
            }
            None => {}
        }
    }
}

#[aoc(day23, part2)]
pub fn part2(_input: &str) -> i32 {
    // The program counts the number of non prime numbers between b and c
    let mut b = 106500;
    let c = 123500;
    let mut h = 0;
    while b != c + 17 {
        if !is_prime(b) {
            h += 1;
        }
        b += 17;
    }
    h
}
