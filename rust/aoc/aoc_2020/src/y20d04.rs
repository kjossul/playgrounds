use std::collections::HashMap;
use itertools::Itertools;
use regex::Regex;

#[derive(Debug)]
struct Passport {
    fields: HashMap<String, String>,
}

impl Passport {
    fn valid_byr(&self) -> bool {
        if let Some(byr) = self.fields.get(&String::from("byr")) {
            if let Ok(y) = byr.parse::<u32>() {
                if 1920 <= y && y <= 2002 {
                    return true;
                }
            }
        }
        false
    }

    fn valid_iyr(&self) -> bool {
        if let Some(iyr) = self.fields.get(&String::from("iyr")) {
            if let Ok(y) = iyr.parse::<u32>() {
                if 2010 <= y && y <= 2020 {
                    return true;
                }
            }
        }
        false
    }
    fn valid_eyr(&self) -> bool {
        if let Some(eyr) = self.fields.get(&String::from("eyr")) {
            if let Ok(y) = eyr.parse::<u32>() {
                if 2020 <= y && y <= 2030 {
                    return true;
                }
            }
        }
        false
    }

    fn valid_hgt(&self) -> bool {
        let re_cm = Regex::new(r"^(\d{3})cm").unwrap();
        let re_in = Regex::new(r"^(\d{2})in").unwrap();
        if let Some(hgt) = self.fields.get(&String::from("hgt")) {
            if let Some(caps) = re_cm.captures(hgt) {
                if let Ok(h) = (&caps[1]).parse::<u32>() {
                    return 150 <= h && h <= 193;
                }
            } else if let Some(caps) = re_in.captures(hgt) {
                if let Ok(h) = (&caps[1]).parse::<u32>() {
                    return 59 <= h && h <= 76;
                }
            }
        }
        false
    }

    fn valid_hcl(&self) -> bool {
        let re = Regex::new(r"#[0-9a-f]{6}$").unwrap();
        if let Some(hcl) = self.fields.get(&String::from("hcl")) {
            return re.find(hcl).is_some();
        }
        false

    }

    fn valid_ecl(&self) -> bool {
        if let Some(ecl) = self.fields.get(&String::from("ecl")) {
            match ecl.as_str() {
                "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => return true,
                _ => return false,
            }
        }
        false
    }

    fn valid_pid(&self) -> bool {
        if let Some(pid) = self.fields.get(&String::from("pid")) {
            let re = Regex::new(r"^\d{9}$").unwrap();
            return re.find(pid).is_some();
        }
        false
    }
}

#[aoc(day4, part1)]
pub fn part1(input: &str) -> usize {
    let passports = parser(input);
    passports.iter().filter(|p|
        p.fields.len() == 8 || (p.fields.len() == 7 && !p.fields.contains_key(&String::from("cid")))
    ).count()
}

#[aoc(day4, part2)]
pub fn part2(input: &str) -> usize {
    let passports = parser(input);
    passports.iter().filter(|p| {
        p.valid_byr() && p.valid_iyr() && p.valid_eyr() && p.valid_hgt() && p.valid_hcl() && p.valid_ecl() && p.valid_pid()
    }).count()
}

fn parser(input: &str) -> Vec<Passport> {
    input.split("\n\n").map(|s| {
        let mut fields = HashMap::new();
        for s1 in s.split_whitespace() {
            let (k, v) = s1.split(':').next_tuple().unwrap();
            fields.insert(String::from(k), String::from(v));
        }
        Passport { fields }
    }).collect()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use super::*;

    #[test]
    pub fn y20d04test() {
        let s = indoc! {"\
            pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
            hcl:#623a2f

            eyr:2029 ecl:blu cid:129 byr:1989
            iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

            hcl:#888785
            hgt:164cm byr:2001 iyr:2015 cid:88
            pid:545766238 ecl:hzl
            eyr:2022

            iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
        "};
        assert_eq!(part2(s), 4);
    }
}