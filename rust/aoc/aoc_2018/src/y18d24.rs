use regex::Regex;
use std::collections::HashMap;

static mut BOOST: isize = 0;

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
enum Element {
    BLUDGE,
    COLD,
    FIRE,
    RAD,
    SLASH,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
struct Group {
    id: isize,
    hp: isize,
    weak: Vec<Element>,
    immune: Vec<Element>,
    atk_v: isize,
    atk_t: Element,
    initiative: isize,
    units: isize,
    is_infection: bool,
}

impl Group {
    fn power(&self) -> isize {
        self.atk_v * self.units
    }

    fn is_alive(&self) -> bool {
        self.units > 0
    }

    fn find_target<'a>(&self, groups: &HashMap<isize, Group>, targets: &HashMap<isize, isize>) -> Option<isize> {
        groups.values()
            .filter(|g| {
                self.is_infection != g.is_infection && g.is_alive() && targets.values().find(|&&id| id == g.id).is_none() && self.scan(g) > 0
            }).map(|g| g.id)
            .max_by_key(|id| {
                let def = groups.get(id).unwrap();
                (self.scan(&def), def.power(), def.initiative)
            })
    }


    fn scan(&self, other: &Group) -> isize {
        let mut damage = self.power();
        if other.weak.iter().find(|&&e| e == self.atk_t).is_some() {
            damage *= 2;
        } else if other.immune.iter().find(|&&e| e == self.atk_t).is_some() {
            damage = 0;
        }
        damage
    }

    fn receive_dmg(&mut self, dmg: isize) {
        self.units -= dmg / self.hp;
        self.units = isize::max(self.units, 0);
//        println!("{} killed, {} remaining", dmg / self.hp, self.units);
    }
}

#[aoc(day24, part1)]
pub fn part1(input: &str) -> isize {
    let gz = parser(input);
    let (imm, inf) = solver(gz);
    isize::max(imm, inf)
}

#[aoc(day24, part2)]
pub fn part2(input: &str) -> isize {
    unsafe {
        loop {
            let gz = parser(input);
            let (imm, _inf) = solver(gz);
            if imm > 0 {
                return imm;
            }
            BOOST += 1;
        }
    }
}

fn solver(mut gz: Vec<Group>) -> (isize, isize) {
    gz.sort_by_key(|g| -g.initiative);
    let atk_order = gz.iter().map(|g| g.id).collect::<Vec<_>>();
    let mut groups: HashMap<isize, Group> = gz.into_iter().map(|g| (g.id, g)).collect();
    let (mut old_system, mut old_infection) = (0, 0);
    loop {
        let system_units = groups.values().filter(|g| {
            !g.is_infection
        }).map(|g| g.units).sum();
        let infection_units = groups.values().filter(|g| {
            g.is_infection
        }).map(|g| g.units).sum();
        if system_units <= 0 || infection_units <= 0 {
            return (system_units, infection_units);
        }  else if old_system == system_units && old_infection == infection_units {
            return (-1, -1);
        } else {
            old_system = system_units;
            old_infection = infection_units;
        }
        /* SELECTION */
        let mut targets: HashMap<isize, isize> = HashMap::new();
        let mut selection_order = groups.values().map(|g| g.id).collect::<Vec<_>>();
        selection_order.sort_by_key(|id| {
            let g = groups.get(id).unwrap();
            (-g.power(), -g.initiative)
        });
//        println!("{:?}", &selection_order);
        for id in &selection_order {
            let g = groups.get(id).unwrap();
            if !g.is_alive() {
                continue;
            }
            if let Some(other_id) = g.find_target(&groups, &targets) {
                targets.insert(*id, other_id);
            }
        }
//        println!("{:?}", &targets);
//        println!("{:?}", &atk_order);
        /* ATTACK */
        for id in &atk_order {
            let atk = groups.get(id).unwrap().clone();
            if !atk.is_alive() {
                continue;
            }
            if let Some(def_id) = targets.get(id) {
                let def = groups.get_mut(def_id).unwrap();
                let dmg = atk.scan(def);
                def.receive_dmg(dmg);
            }
        }
    }
}

fn parser(input: &str) -> Vec<Group> {
    let groups = input.split("\n\n").collect::<Vec<_>>();
    let mut v = parse_group(groups[0], false);
    v.extend(parse_group(groups[1], true));
    v
}

fn parse_group(group: &str, is_infection: bool) -> Vec<Group> {
    let re = Regex::new(r"(\d+).*?(\d+) hit points (\(.*\))?.*?(\d+) (\w+).*?(\d+)").unwrap();
    let words = Regex::new(r"\w{3,}").unwrap();
    let mut g = Vec::new();
    for (i, cap) in re.captures_iter(group).enumerate() {
        let mul = if is_infection { -1 } else { 1 };
        let id = (i as isize + 1) * mul;
        let units = (&cap[1]).parse().unwrap();
        let hp = (&cap[2]).parse().unwrap();
        let mut weak = Vec::new();
        let mut immune = Vec::new();
        if let Some(s) = cap.get(3) {
            let mut v = &mut weak;
            for m in words.find_iter(s.as_str()) {
                match m.as_str() {
                    "immune" => v = &mut immune,
                    "weak" => v = &mut weak,
                    w => { v.push(parse_element(w)); }
                }
            }
        }
        let mut boost = 0;
        if !is_infection {
            unsafe {
                boost += BOOST;
            }
        }
        let atk_v = (&cap[4]).parse::<isize>().unwrap() + boost;
        let atk_t = parse_element(&cap[5]);
        let initiative = (&cap[6]).parse().unwrap();
        g.push(Group { id, hp, weak, immune, atk_v, atk_t, initiative, units, is_infection });
    }
    g
}

fn parse_element(e: &str) -> Element {
    match e {
        "bludgeoning" => Element::BLUDGE,
        "cold" => Element::COLD,
        "fire" => Element::FIRE,
        "radiation" => Element::RAD,
        "slashing" => Element::SLASH,
        _ => panic!("{}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn y18d24test() {
        let s = indoc! {"\
Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
        "};
        assert_eq!(part1(s), 5216);
        unsafe {
            BOOST = 1570;
        }
        assert_eq!(part2(s), 51);
    }
}