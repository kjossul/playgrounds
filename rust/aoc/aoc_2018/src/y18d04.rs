use std::collections::HashMap;
use chrono::{Duration, NaiveDateTime, NaiveDate, Timelike};
use regex::Regex;

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Copy)]
pub enum Action {
    BEGIN(usize),
    SLEEP,
    WAKE,
}


pub fn gen(input: &str) -> Vec<(NaiveDateTime, Action)> {
    let mut v = Vec::new();
    let re = Regex::new(r"\[(.*)] (.*)").unwrap();
    let re_id = Regex::new(r".*#(\d+).*").unwrap();
    for line in input.lines() {
        let caps = re.captures(line).unwrap();
        let mut date = NaiveDateTime::parse_from_str(&caps[1], "%Y-%m-%d %H:%M").unwrap();
        if date.time().hour() == 23 {
            date += Duration::minutes((60 - date.time().minute()).into());
        }
        let action = match &caps[2] {
            "wakes up" => Action::WAKE,
            "falls asleep" => Action::SLEEP,
            text => {
                let caps_id = re_id.captures(text).unwrap();
                Action::BEGIN(caps_id[1].parse().unwrap())
            }
        };
        v.push((date, action));
    }
    v.sort();
    v
}

fn schedules(input: &str) -> HashMap<usize, HashMap<NaiveDate, [u32; 60]>> {
    let v = gen(input);
    let mut guards: HashMap<usize, HashMap<NaiveDate, [u32; 60]>> = HashMap::new();
    let (mut guard, mut current_date);
    if let (ndt, Action::BEGIN(g)) = v[0] {
        current_date = ndt.date();
        guard = g;
    } else {
        panic!("Wrong first value!");
    }
    let mut current_minute = 0;
    let mut is_awake = 0;
    for (ndt, action) in v {
        let schedule = guards.entry(guard).or_insert(HashMap::new());
        let timetable = schedule.entry(current_date).or_insert([0; 60]);
        for i in current_minute as usize..60 {
            timetable[i] = is_awake;
        }
        current_minute = ndt.time().minute();
        match action {
            Action::BEGIN(id) => {
                current_date = ndt.date();
                guard = id;
                is_awake = 1;
            }
            Action::SLEEP => {
                is_awake = 0;
            }
            Action::WAKE => {
                is_awake = 1;
            }
        }
    }
    guards
}

#[aoc(day4, part1)]
pub fn part1(input: &str) -> usize {
    let guards = schedules(input);
    let (most_asleep, schedule) = guards.iter().max_by_key(|(_, schedule)|
        schedule.values().map(|tt| 60 - tt.iter().sum::<u32>()).sum::<u32>()
    ).unwrap();
    let minute = (0..60).min_by_key(|&i| schedule.values().map(|v| v[i]).sum::<u32>()).unwrap();
    most_asleep * minute
}

#[aoc(day4, part2)]
pub fn part2(input: &str) -> usize {
    let guards = schedules(input);
    let (mut guard, mut minute) = (0, 0);
    let mut most_time_asleep = 0;
    for i in 0..60 {
        for (&g, schedule) in &guards {
            let time_asleep = schedule.values().map(|v| 1 - v[i]).sum();
            if time_asleep > most_time_asleep {
                most_time_asleep = time_asleep;
                guard = g;
                minute = i;
            }
        }
    }
    guard * minute
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d04test() {
        let s = indoc!{"
            [1518-11-01 00:00] Guard #10 begins shift
            [1518-11-01 00:05] falls asleep
            [1518-11-01 00:25] wakes up
            [1518-11-01 00:30] falls asleep
            [1518-11-01 00:55] wakes up
            [1518-11-01 23:58] Guard #99 begins shift
            [1518-11-02 00:40] falls asleep
            [1518-11-02 00:50] wakes up
            [1518-11-03 00:05] Guard #10 begins shift
            [1518-11-03 00:24] falls asleep
            [1518-11-03 00:29] wakes up
            [1518-11-04 00:02] Guard #99 begins shift
            [1518-11-04 00:36] falls asleep
            [1518-11-04 00:46] wakes up
            [1518-11-05 00:03] Guard #99 begins shift
            [1518-11-05 00:45] falls asleep
            [1518-11-05 00:55] wakes up
        "};
        assert_eq!(part1(s), 240);
    }
}
