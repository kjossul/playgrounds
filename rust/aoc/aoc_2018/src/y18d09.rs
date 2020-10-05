use std::collections::VecDeque;

#[aoc_generator(day9)]
pub fn gen(input: &str) -> (usize, usize) {
    let words = input.split_whitespace().collect::<Vec<_>>();
    (words[0].parse().unwrap(), words[6].parse().unwrap())
}

#[aoc(day9, part1)]
pub fn part1(&(players, marble): &(usize, usize)) -> usize {
    let mut scores = vec![0; players];
    let mut player = 0;
    let mut ns = VecDeque::from(vec![0]);
    for n in 1..=marble {
        player = turn(&mut scores, player, &mut ns, n);
    }
    *scores.iter().max().unwrap()
}

#[aoc(day9, part2)]
pub fn part2(&(players, marble): &(usize, usize)) -> usize {
    let mut scores = vec![0; players];
    let mut player = 0;
    let mut ns = VecDeque::from(vec![0]);
    for n in 1..=marble * 100 {
        player = turn(&mut scores, player, &mut ns, n);
    }
    *scores.iter().max().unwrap()
}

fn turn(scores: &mut Vec<usize>, player: usize, ns: &mut VecDeque<usize>, n: usize) -> usize {
    if n % 23 != 0 {
        ns.rotate_left(1);
        ns.push_back(n);
    } else {
        ns.rotate_right(7);
        let popped = ns.pop_back().unwrap();
        scores[player] += n + popped;
        ns.rotate_left(1);
    }
    (player + 1) % scores.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y18d09test() {
        assert_eq!(part1(&gen("9 players; last marble is worth 25 points")),
                   32);
        assert_eq!(part1(&gen("17 players; last marble is worth 1104 points")),
                   2764);
    }
}