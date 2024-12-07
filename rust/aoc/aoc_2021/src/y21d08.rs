use regex::Regex;
use itertools::Itertools;
use std::collections::HashSet;
use std::collections::HashMap;

#[aoc(day8, part1)]
pub fn part1(input: &str) -> usize {
    let vs = parser(input);
    let targets = vec![2, 3, 4, 7];
    vs.into_iter().map(|v|
        (&v[10..=13]).iter().filter(|s| targets.contains(&s.len())).count()
    ).sum()
}

#[aoc(day8, part2)]
pub fn part2(input: &str) -> usize {
    let vs = parser(input);
    vs.into_iter().map(|v| {
        let mut sets: Vec<HashSet<char>> = (&v[0..=9]).iter().map(|string|
            string.chars().collect()
        ).collect();
        sets.sort_by_key(|s| s.len());
        // distinguishing zero, six, and nine, only digits with six pips
        // six is not a superset of one
        let six = (&sets[6..9]).iter().find(|&s| !s.is_superset(&sets[0])).unwrap();
        // zero is not a superset of four and is not six
        let zero = (&sets[6..9]).iter().find(|&s| !s.is_superset(&sets[2]) && s != six).unwrap();
        // nine by process of elimination
        let nine = (&sets[6..9]).iter().find(|&s| s != zero && s != six).unwrap();
        // distinguishing two, three and five, only digits with five pips
        // three is only superset of one
        let three = (&sets[3..6]).iter().find(|&s| s.is_superset(&sets[0])).unwrap();
        // five is a subset of six
        let five = (&sets[3..6]).iter().find(|&s| s.is_subset(six)).unwrap();
        // two by process of elimination
        let two = (&sets[3..6]).iter().find(|&s| s != three && s != five).unwrap();
        let signals: HashMap<String, usize> = [zero, &sets[0], two, three, &sets[2], five, six, &sets[1], &sets[9], nine].iter().enumerate()
            .map(|(i, s)| (s.into_iter().sorted().collect::<String>(), i)).collect();
        let number = (&v[10..=13]).into_iter().enumerate().map(|(i, s)|
            usize::pow(10, 3 - i as u32) * signals.get(s).unwrap()
        ).sum::<usize>();
        // println!("{:?}\n{:?}\n{:?}", signals, v, number);
        number
    }).sum()
}

fn parser(input: &str) -> Vec<Vec<String>> {
    let re = Regex::new(r"(\w+)").unwrap();
    input.lines().map(|l| re.find_iter(l).map(|mat|
        mat.as_str().chars().sorted().collect()
    ).collect()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d08test() {
        let s = indoc! {"\
            be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
            edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
            fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
            fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
            aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
            fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
            dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
            bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
            egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
            gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
        "};

        assert_eq!(part1(s), 26);
    }
}