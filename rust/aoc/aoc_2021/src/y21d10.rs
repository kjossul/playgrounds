#[aoc(day10, part1)]
pub fn part1(input: &str) -> Option<u32> {
    let mut score = 0;
    'lines: for line in input.lines() {
        let mut stack: Vec<char> = vec!['|'];
        for c in line.chars() {
            match (c, stack.last()?) {
                ('(', _) | ('[', _) | ('{', _) | ('<', _) => stack.push(c),
                (')', '(') | (']', '[') | ('}', '{') | ('>', '<') => { stack.pop(); },
                (')', _) => {
                    score += 3;
                    continue 'lines;
                }
                (']', _) => {
                    score += 57;
                    continue 'lines;
                }
                ('}', _) => {
                    score += 1197;
                    continue 'lines;
                }
                ('>', _) => {
                    score += 25137;
                    continue 'lines;
                }
                _ => panic!(),
            }
        }
    }
    Some(score)
}

#[aoc(day10, part2)]
pub fn part2(input: &str) -> Option<u64> {
    let mut scores = vec![];
    'lines: for line in input.lines() {
        let mut stack: Vec<char> = vec!['|'];
        for c in line.chars() {
            match (c, stack.last()?) {
                ('(', _) | ('[', _) | ('{', _) | ('<', _) => stack.push(c),
                (')', '(') | (']', '[') | ('}', '{') | ('>', '<') => { stack.pop(); },
                (')', _) | (']', _) | ('}', _) | ('>', _) => { continue 'lines; }
                _ => panic!(),
            }
        }
        let mut score = 0;
        for c in (stack[1..]).iter().rev() {
            score *= 5;
            score += match c {
                '(' => 1,
                '[' => 2,
                '{' => 3,
                '<' => 4,
                _ => panic!(),
            }
        }
        scores.push(score);
    }
    scores.sort();
    Some(scores[scores.len() / 2])
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y21d10test() {
        let s = indoc! {"\
            [({(<(())[]>[[{[]{<()<>>
            [(()[<>])]({[<{<<[]>>(
            {([(<{}[<>[]}>{[]{[(<()>
            (((({<>}<{<{<>}{[]{[]{}
            [[<[([]))<([[{}[[()]]]
            [{[{({}]{}}([{[{{{}}([]
            {<[[]]>}<{[{[{[]{()[[[]
            [<(<(<(<{}))><([]([]()
            <{([([[(<>()){}]>(<<{{
            <{([{{}}[<[[[<>{}]]]>[]]
        "};

        assert_eq!(part2(s), Some(0));
    }
}