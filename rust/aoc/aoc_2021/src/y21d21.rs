use std::collections::HashMap;

#[aoc(day21, part1)]
pub fn part1(input: &str) -> Option<u64> {
    let mut pos = parser(input);
    let mut die = (1..=100).cycle();
    let mut scores = vec![0, 0];
    let mut active_player = 0;
    let mut c = 0;
    while scores[0] < 1000 && scores[1] < 1000 {
        let v: u64 = die.by_ref().take(3).sum();
        c += 3;
        pos[active_player] = (pos[active_player] - 1 + v) % 10 + 1;
        scores[active_player] += pos[active_player];
        active_player = 1 - active_player;
    }
    Some(scores[active_player] * c)
}

#[aoc(day21, part2)]
pub fn part2(input: &str) -> Option<u64> {
    let pos = parser(input);
    let (a, b) = play(pos[0], pos[1], 0, 0, &mut HashMap::new());
    Some(if a > b {a} else {b})
}

fn play(pos1: u64, pos2: u64, score1: u64, score2: u64, cache: &mut HashMap<(u64, u64, u64, u64), (u64, u64)>) -> (u64, u64){
    if let Some(t) = cache.get(&(pos1, pos2, score1, score2)) {
        return *t;
    }
    if score2 >= 21 {
        return (0, 1);
    }
    let (mut wins1, mut wins2) = (0, 0);
    for (mov, n) in [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)] {
        let new_pos = (pos1 -1 + mov) % 10 + 1;
        let (w2, w1) = play(pos2, new_pos, score2, score1 + new_pos, cache);
        wins1 += n * w1;
        wins2 += n * w2;
    }
    cache.insert((pos1, pos2, score1, score2), (wins1, wins2));
    (wins1, wins2)
}

fn parser(input: &str) -> Vec<u64> {
    input.lines().map(|l| l.chars().last().unwrap().to_digit(10).unwrap() as u64).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d21test() {
        let s = indoc!{"\
            Player 1 starting position: 4
            Player 2 starting position: 8
        "};
        assert_eq!(part1(s), Some(739785));
        assert_eq!(part2(s), Some(444356092776315));
    }
}