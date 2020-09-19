use std::collections::HashMap;
use std::collections::HashSet;
use itertools::izip;
use regex::Regex;

#[aoc_generator(day20)]
pub fn generator(input: &str) -> Vec<Vec<Vec<i64>>> {
    let re = Regex::new("<(.*?)>").unwrap();
    input.lines().map(|l| {
        re.captures_iter(l).map(|capture| {
            capture[1].split(',').map(|d| d.parse().unwrap()).collect::<Vec<i64>>()
        }).collect::<Vec<Vec<i64>>>()
    }).collect()
}

#[aoc(day20, part1)]
pub fn part1(vss: &[Vec<Vec<i64>>]) -> usize {
    let origin = [0, 0, 0];
    let t = 1000;
    let (i, _) = vss.iter().enumerate().min_by(|(_, vs1), (_, vs2)| {
        distance(&origin, &motion(vs1, t))
            .cmp(&distance(&origin, &motion(vs2, t)))
    }).unwrap();
    i
}

#[aoc(day20, part2)]
pub fn part2(vss: &[Vec<Vec<i64>>]) -> usize {
    let mut particles: Vec<Vec<Vec<i64>>> = vss.to_vec();
    for _t in 1..1000 {
        particles = move_and_remove(&particles);
    }
    particles.len()
}

fn move_and_remove(vss: &Vec<Vec<Vec<i64>>>) -> Vec<Vec<Vec<i64>>> {
    let mut found = HashMap::<Vec<i64>, Vec<Vec<i64>>>::new();
    let mut valid = HashSet::<Vec<Vec<i64>>>::new();
    for vs in vss.iter() {
        let new_vel = vec![vs[1][0] + vs[2][0], vs[1][1] + vs[2][1], vs[1][2] + vs[2][2]];
        let new_pos = vec![vs[0][0] + new_vel[0], vs[0][1] + new_vel[1], vs[0][2] + new_vel[2]];
        if let Some(other) = found.get(&new_pos) {
            valid.remove(other);
        } else {
            let v = vec![new_pos.clone(), new_vel.clone(), vs[2].clone()];
            valid.insert(v.clone());
            found.insert(new_pos.clone(), v.clone());
        }
    }
    valid.into_iter().collect()
}

fn motion(vs: &[Vec<i64>], t: i64) -> Vec<i64> {
    izip!(&vs[0], &vs[1], &vs[2]).map(|(s, v, a)| {
        s + v * t + a * t * t / 2  // Equation of motion
    }).collect()
}

fn distance(p1: &[i64], p2: &[i64]) -> i64 {
    izip!(p1, p2).map(|(c1, c2)| {
        (c1 - c2).abs()
    }).sum()
}


#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y17d20test() {
        let input = indoc! {"
            p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
            p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
            p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
            p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>
        "};
        assert_eq!(part2(&generator(input)), 1);
    }
}