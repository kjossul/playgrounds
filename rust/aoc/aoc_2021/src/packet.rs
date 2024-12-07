use crate::packet::Packet::{Literal, Operator};

#[derive(Debug)]
pub enum Packet {
    Literal(u64, u64),
    Operator(u64, u64, Vec<Packet>),
}

pub fn parse_hex(input: &str) -> String {
    input.chars().map(|c| format!("{:04b}", c.to_digit(16).unwrap())).collect()
}

pub fn decoder(bits: &str) -> Vec<Packet> {
    let mut it = bits.chars();
    let v = parse_iterator(&mut it);
    v
}

pub fn eval(packet: &Packet) -> u64 {
    match packet {
        Literal(_, v) => *v,
        Operator(_, op, packets) => {
            match op {
                0 => packets.iter().fold(0, |acc, p| acc + eval(p)),
                1 => packets.iter().fold(1, |acc, p| acc * eval(p)),
                2 => packets.iter().map(eval).min().unwrap(),
                3 => packets.iter().map(eval).max().unwrap(),
                5 => (eval(&packets[0]) > eval(&packets[1])) as u64,
                6 => (eval(&packets[0]) < eval(&packets[1])) as u64,
                7 => (eval(&packets[0]) == eval(&packets[1])) as u64,
                _ => panic!("Operation not supported"),
            }
        }
    }
}

fn parse_iterator<I>(it: &mut I) -> Vec<Packet>
    where I: Iterator<Item=char> {
    let mut v = Vec::new();
    while let Some(packet) = parse_packet(it) {
        v.push(packet);
    }
    v
}

fn parse_packet<I>(it: &mut I) -> Option<Packet>
    where I: Iterator<Item=char> {
    let version = u64::from_str_radix(&it.take(3).collect::<String>(), 2).ok()?;
    let type_id = u64::from_str_radix(&it.take(3).collect::<String>(), 2).ok()?;
    return match type_id {
        4 => {
            let mut s = String::new();
            loop {
                let group = it.next()?;
                s.extend(it.take(4));
                if group == '0' { break; }
            }
            Some(Literal(version, u64::from_str_radix(&s, 2).ok()?))
        },
        _ => {
            let length_type_id = it.next()?;
            if length_type_id == '0' {
                let sub_packet_length = usize::from_str_radix(&it.take(15).collect::<String>(), 2).ok()?;
                let substr = it.take(sub_packet_length).collect::<String>();
                let sub_packets = decoder(&substr);
                Some(Operator(version, type_id, sub_packets))
            } else {
                let packet_count = usize::from_str_radix(&it.take(11).collect::<String>(), 2).ok()?;
                let sub_packets = (0..packet_count).flat_map(|_| parse_packet(it)).collect();
                Some(Operator(version, type_id, sub_packets))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    // use indoc::indoc;

    #[test]
    pub fn packet_test() {
    }
}