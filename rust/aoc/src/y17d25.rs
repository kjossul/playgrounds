use std::collections::HashMap;

enum State {
    A,
    B,
    C,
    D,
    E,
    F,
}

impl Default for State {
    fn default() -> State {
        State::A
    }
}

#[derive(Default)]
struct Turing {
    tape: HashMap<isize, bool>,
    cursor: isize,
    state: State,
}

impl Turing {
    pub fn checksum(&mut self, steps: usize) -> usize {
        for _ in 0..steps {
            self.step();
        }
        self.tape.values().filter(|&&v| v).count()
    }

    fn step(&mut self) {
        match self.state {
            State::A => {
                if !*self.tape.entry(self.cursor).or_insert(false) {
                    self.tape.insert(self.cursor, true);
                    self.cursor += 1;
                    self.state = State::B;
                } else {
                    self.tape.insert(self.cursor, false);
                    self.cursor -= 1;
                    self.state = State::C;
                }
            }
            State::B => {
                if !*self.tape.entry(self.cursor).or_insert(false) {
                    self.tape.insert(self.cursor, true);
                    self.cursor -= 1;
                    self.state = State::A;
                } else {
                    self.cursor -= 1;
                    self.state = State::D;
                }
            }
            State::C => {
                if !*self.tape.entry(self.cursor).or_insert(false) {
                    self.tape.insert(self.cursor, true);
                    self.cursor += 1;
                    self.state = State::D;
                } else {
                    self.tape.insert(self.cursor, false);
                    self.cursor += 1;
                    self.state = State::C;
                }
            }
            State::D => {
                if !*self.tape.entry(self.cursor).or_insert(false) {
                    self.cursor -= 1;
                    self.state = State::B;
                } else {
                    self.tape.insert(self.cursor, false);
                    self.cursor += 1;
                    self.state = State::E;
                }
            }
            State::E => {
                if !*self.tape.entry(self.cursor).or_insert(false) {
                    self.tape.insert(self.cursor, true);
                    self.cursor += 1;
                    self.state = State::C;
                } else {
                    self.cursor -= 1;
                    self.state = State::F;
                }
            }
            State::F => {
                if !*self.tape.entry(self.cursor).or_insert(false) {
                    self.tape.insert(self.cursor, true);
                    self.cursor -= 1;
                    self.state = State::E;
                } else {
                    self.cursor += 1;
                    self.state = State::A;
                }
            }
        }
    }
}

#[aoc(day25, part1)]
pub fn part1(_input: &str) -> usize {
    let mut turing = Turing { ..Default::default() };
    turing.checksum(12656374)
}
