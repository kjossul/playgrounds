use counter::Counter;


#[aoc(day2, part1)]
pub fn part1(input: &str) -> u32 {
    let (mut a, mut b) = (0, 0);
    for line in input.lines() {
        let (a1, b1) = score(line);
        a += u32::from(a1);
        b += u32::from(b1);
    }
    a * b
}

#[aoc(day2, part2)]
pub fn part2(input: &str) -> String {
    let mut found = Vec::<&str>::new();
    for word1 in input.lines() {
        'check: for &word2 in &found {
            let mut i = None;
            for (j, (c1, c2)) in word1.chars().zip(word2.chars()).enumerate() {
                if c1 != c2 {
                    if let Some(_) = i.replace(j) {
                        continue 'check;
                    }
                }
            }
            return word1.chars().enumerate().map(|(j, c)| if i == Some(j) { '_' } else { c }).collect();
        }
        found.push(word1);
    }
    panic!("Match not found");
}

fn score(s: &str) -> (bool, bool) {
    let c = s.chars().collect::<Counter<_>>();
    (c.values().any(|&n| n == 2), c.values().any(|&n| n == 3))
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d02test() {
        let s = indoc!{
            "
            abcde
            fghij
            klmno
            pqrst
            fguij
            axcye
            wvxyz
        "
        };
        assert_eq!(part2(s), String::from("fg_ij"));
    }
}
