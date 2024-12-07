use crate::packet::*;

#[aoc(day16, part1)]
pub fn part1(input: &str) -> Option<u64> {
    let v = parser(input);
    Some(sum_versions(&v))
}

#[aoc(day16, part2)]
pub fn part2(input: &str) -> Option<u64> {
    let v = parser(input);
    Some(eval(&v[0]))
}

fn sum_versions(packets: &Vec<Packet>) -> u64 {
    packets.iter().map(|p| match p {
        Packet::Literal(version, _) => { *version as u64 }
        Packet::Operator(version, _, vec) => { *version as u64 + sum_versions(vec) }
    }).sum()
}

fn parser(input: &str) -> Vec<Packet> {
    decoder(parse_hex(input).as_str())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn y21d16test() {
        let mut s = "8A004A801A8002F478";
        assert_eq!(part1(s), Some(16));
        s = "620080001611562C8802118E34";
        assert_eq!(part1(s), Some(12));
    }
}