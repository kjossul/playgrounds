use regex::Regex;
use std::cmp;

#[derive(Debug, Clone)]
pub struct NanoBot {
    x: isize,
    y: isize,
    z: isize,
    r: isize,
}

impl NanoBot {
    fn manhattan(&self, other: &Self) -> isize {
        isize::abs(self.x - other.x) + isize::abs(self.y - other.y) + isize::abs(self.z - other.z)
    }
}

#[aoc(day23, part1)]
pub fn part1(input: &str) -> usize {
    let mut bots = parser(input);
    bots.sort_by_key(|b| -b.r);
    bots.iter().filter(|o| bots[0].manhattan(o) <= bots[0].r).count()
}

// thanks <3 https://gist.github.com/fryguy1013/6267644843c7b6ba1fe504f6a9f3c937

pub fn find_most_overlapping(input: &Vec<NanoBot>, in_range: &Vec<Vec<bool>>, remaining_input: &[usize], working_set: &mut Vec<usize>, best: &mut Vec<usize>) {
    for i in 0..remaining_input.len() {
        if best.len() > remaining_input.len() - i + working_set.len() {
            break;
        }

// check if it fits in
        if !working_set.iter().all(|&r| in_range[r][remaining_input[i]]) {
            continue;
        }

        working_set.push(remaining_input[i].clone());
        find_most_overlapping(input, in_range, &remaining_input[i + 1..], working_set, best);
        working_set.pop();
    }

    if working_set.len() > best.len() {
        let loc = find_location(input, working_set);
        println!("{} : {:?}", working_set.len(), loc);
        if let Some(loc) = loc {
            println!("{}", loc.x.abs() + loc.y.abs() + loc.z.abs());
        }
        *best = working_set.clone();
    }
}

fn find_location(input: &Vec<NanoBot>, best: &Vec<usize>) -> Option<NanoBot> {
    let mut loc = input[best[0]].clone();
    loc.r = 0;
    loop {
        let mut dist_moved = 0;
        for out_of_range in best.iter() {
            let target = &input[*out_of_range];

            if loc.manhattan(&target) <= target.r {
                continue;
            }

            let mut dist_to_move = loc.manhattan(&target) - target.r;
            dist_moved = cmp::max(dist_moved, dist_to_move);
            while dist_to_move > 0 {
                let max_x = (target.x - loc.x).abs();
                let max_y = (target.y - loc.y).abs();
                let max_z = (target.z - loc.z).abs();
                let num_nonzero = (max_x > 0) as isize + (max_y > 0) as isize + (max_z > 0) as isize;
//println!("need to move {} from {:?} to {:?} ", dist_to_move, loc, target);

                if max_x > 0 && (max_x <= max_y || max_y == 0) && (max_x <= max_z || max_z == 0) {
                    let move_amt = cmp::min(max_x, (dist_to_move + num_nonzero - 1) / num_nonzero);
                    loc.x += (target.x - loc.x).signum() * move_amt;
                    loc.y += (target.y - loc.y).signum() * move_amt;
                    loc.z += (target.z - loc.z).signum() * move_amt;
                } else if max_y > 0 && (max_y <= max_z || max_z == 0) {
                    let move_amt = cmp::min(max_y, (dist_to_move + num_nonzero - 1) / num_nonzero);
                    loc.x += (target.x - loc.x).signum() * move_amt;
                    loc.y += (target.y - loc.y).signum() * move_amt;
                    loc.z += (target.z - loc.z).signum() * move_amt;
                } else if max_z > 0 {
                    let move_amt = cmp::min(max_z, (dist_to_move + num_nonzero - 1) / num_nonzero);
                    loc.x += (target.x - loc.x).signum() * move_amt;
                    loc.y += (target.y - loc.y).signum() * move_amt;
                    loc.z += (target.z - loc.z).signum() * move_amt;
                } else {
                    panic!("not all three should be zero");
                }
                dist_to_move = loc.manhattan(&target) - target.r;
            }
        }

        if dist_moved < 3 {
            break;
        }
    }

    for dx in -3..=3 {
        for dy in -3..=3 {
            for dz in -3..=3 {
                let t = NanoBot { x: loc.x + dx, y: loc.y + dy, z: loc.z + dz, r: 0 };
                if !best.iter().any(|&e| t.manhattan(&input[e]) > input[e].r) {
                    return Some(t);
                }
            }
        }
    }

    None
}

#[aoc(day23, part2)]
pub fn solve_part2(s: &str) -> usize {
//println!("{:?}", input);
    let input = parser(s);
    let mut in_range_map: Vec<Vec<bool>> = vec![vec![false; input.len()]; input.len()];
    for i in 0..input.len() {
        for j in 0..input.len() {
            in_range_map[i][j] = input[i].manhattan(&input[j]) <= input[i].r + input[j].r;
        }
    }
    let num_in_range_neighbors: Vec<usize> = in_range_map.iter().map(|row| row.iter().map(|&c| c as usize).sum()).collect();


    let mut indicies: Vec<usize> = (0..input.len()).collect();
    indicies.sort_by(|&l, &r| num_in_range_neighbors[r].cmp(&num_in_range_neighbors[l]));

    let mut best: Vec<usize> = Vec::new();
    find_most_overlapping(&input, &in_range_map, &indicies, &mut Vec::new(), &mut best);
//println!("{:?}", best);

    let matched_all = find_location(&input, &best).unwrap();

    println!("{:?}", matched_all);

    (matched_all.x.abs() + matched_all.y.abs() + matched_all.z.abs()) as usize
}

fn parser(input: &str) -> Vec<NanoBot> {
    let re = Regex::new(r"pos=<(.*),(.*),(.*)>, r=(.*)").unwrap();
    input.lines().map(|l| {
        let caps = re.captures(l).unwrap().iter()
            .flat_map(|o| o.unwrap().as_str().parse()).collect::<Vec<_>>();
        NanoBot { x: caps[0], y: caps[1], z: caps[2], r: caps[3] }
    }).collect()
}
