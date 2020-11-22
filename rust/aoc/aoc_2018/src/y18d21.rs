use std::collections::HashSet;

#[aoc(day21, part1)]
pub fn part1(_input: &str) -> usize {
    f(0)
}

#[aoc(day21, part2)]
pub fn part2(_input: &str) -> usize {
    let mut r5 = 0;
    let mut visited = HashSet::new();
    let mut old = 0;
    while !visited.contains(&r5) {
        visited.insert(r5);
        old = r5;
        r5 = f(r5);
    }
    old
}

fn f(mut r5: usize) -> usize {
    /*
    ...
    r5 = 0;
    START
    r3 = r5 | 65536;
    r5 = 10828530;
    loop {
    r5 += r3 & 255;
    r5 = r5 & 16777215;
    r5 *= 65899;
    r5 = r5 & 16777215;
    if 256 > r3 {
        if r5 == r0 { break; } else { GOTO START }
    }
    r2 = 0;
    loop {
        if (r2 + 1) * 256 > r3 {
            break;
        }
        r2 += 1;
    }
    r3 = r2;
    }
    */
    loop {
        let mut r3 = r5 | 65536;
        r5 = 10828530;
        loop {
            r5 += r3 & 255;
            r5 = r5 & 16777215;
            r5 *= 65899;
            r5 = r5 & 16777215;
            if 256 > r3 {
                return r5
            }
            r3 /= 256;
        }
    }
}