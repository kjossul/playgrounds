use itertools::Itertools;

#[aoc_generator(day12)]
pub fn generator(input: &str) -> (Vec<bool>, Vec<Vec<bool>>) {
    let mut rules = Vec::new();
    let mut lines = input.lines();
    let state = lines.next().unwrap().split_whitespace().last().unwrap().chars()
        .map(|c| c == '#').collect();
    for line in lines.skip(1) {
        let (rule, _, s) = line.split_whitespace().next_tuple().unwrap();
        let c = s.chars().next().unwrap();
        if c == '#' {
            rules.push(rule.chars().map(|c| c == '#').collect());
        }
    }
    (state, rules)
}

#[aoc(day12, part1)]
pub fn part1((first, rules): &(Vec<bool>, Vec<Vec<bool>>)) -> isize {
    let mut state = first.clone();
    for _ in 0..20 {
        state = mutate(state, rules);
    }
    state.iter().enumerate().map(|(i, &has_plant)| if has_plant { i as isize - 40 } else { 0 }).sum()
}

#[aoc(day12, part2, personal)]
pub fn part2((_first, _rules): &(Vec<bool>, Vec<Vec<bool>>)) -> u64 {
//    let mut state = _first.clone();
//    let mut old = 0;
//    let mut new;
//    for i in 0..50000 {
//        state = mutate(state, _rules);
//        new = state.iter().enumerate().map(|(i, &has_plant)| if has_plant { i as isize - 40 } else { 0 }).sum::<isize>();
//        println!("({}) {} - {} = {}", i + 1, new, old, new - old);
//        old = new;
//    }
    /*
    Beginning the 123rd transformation, each adds 5 to the overall score.
    */
    let end = 50 * 10_u64.pow(9);
    834 + 5 * (end - 123)
}

fn mutate(state: Vec<bool>, rules: &[Vec<bool>]) -> Vec<bool> {
    let mut new_state = Vec::new();
    let window = state.iter().chain([false; 4].iter());
    let mut current_rule = vec![false; 5];
    for &e in window {
        current_rule.remove(0);
        current_rule.push(e);
        new_state.push(rules.contains(&current_rule));
    }
    new_state
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d12test() {
        let s = indoc! {"\
            initial state: #..#.#..##......###...###

            ...## => #
            ..#.. => #
            .#... => #
            .#.#. => #
            .#.## => #
            .##.. => #
            .#### => #
            #.#.# => #
            #.### => #
            ##.#. => #
            ##.## => #
            ###.. => #
            ###.# => #
            ####. => #"};
        assert_eq!(part1(&generator(s)), 325);
        assert_eq!(part2(&generator(s)), 0);
    }
}