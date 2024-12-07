use std::collections::HashMap;

#[derive(SmartDefault, Debug)]
struct BingoBoard {
    numbers: HashMap<u32, BingoSlot>,
    #[default(_code = "vec![0; 5]")]
    rows: Vec<u32>,
    #[default(_code = "vec![0; 5]")]
    cols: Vec<u32>,
    unmarked_sum: u32
}

#[derive(Debug)]
struct BingoSlot {
    i: usize,
    j: usize,
    marked: bool
}

impl BingoBoard {
    fn from_numbers(ns: Vec<u32>) -> Self {
        let mut unmarked_sum = 0;
        let numbers = ns.into_iter().enumerate().map(|(i, n)| {
            unmarked_sum += n;
            (n, BingoSlot {i: i / 5, j: i % 5, marked: false})
        }).collect();
        Self {numbers, unmarked_sum, ..Default::default()}
    }

    fn is_bingo(&self) -> bool {
        self.rows.iter().any(|&x| x == 5) || self.cols.iter().any(|&x| x == 5)
    }

    fn check_number(&mut self, n: u32) {
        if let Some(slot) = self.numbers.get_mut(&n) {
            slot.marked = true;
            self.rows[slot.i] += 1;
            self.cols[slot.j] += 1;
            self.unmarked_sum -= n;
        }
    }
}

#[aoc(day4, part1)]
pub fn part1(input: &str) -> u32 {
    let (ns, mut boards) = parser(input);
    for n in ns {
        for board in &mut boards {
            board.check_number(n);
            if board.is_bingo() {
                return n * board.unmarked_sum
            }
        }
    }
    panic!()
}

#[aoc(day4, part2)]
pub fn part2(input: &str) -> u32 {
    let (ns, mut boards) = parser(input);
    for &n in &ns {
        let boards_remaining = boards.len();
        for board in &mut boards {
            board.check_number(n);
            if boards_remaining == 1 && board.is_bingo() {
                return n * board.unmarked_sum
            }
        }
        boards.retain(|board| !board.is_bingo());
    }
    panic!()
}


fn parser(input: &str) -> (Vec<u32>, Vec<BingoBoard>) {
    let v: Vec<&str> = input.split_whitespace().collect();
    let ns = v[0].split(",").flat_map(str::parse).collect();
    let boards = (&v[1..]).chunks(25).map(|chunk| {
        let numbers = chunk.iter().flat_map(|n| n.parse()).collect();
        BingoBoard::from_numbers(numbers)
    }).collect();
    (ns, boards)
}