use std::collections::HashMap;
use std::io;

#[derive(Debug)]
struct Company(HashMap<String, Vec<String>>);

impl Company {
    fn add_employee(&mut self, department: &str, employee: &str) {
        let employees = self.0.entry(String::from(department))
            .or_insert(Vec::new());
        employees.push(String::from(employee));
    }

    fn get_all(&self) -> Vec<String> {
        let mut employees = Vec::new();
        for department in self.0.keys() {
            employees.extend(self.get_department(department));
        }
        employees.sort();
        employees
    }

    fn get_department(&self, department: &str) -> Vec<String> {
        let mut employees = self.0.get(department).unwrap().clone();
        employees.sort();
        employees
    }
}

fn main() {
    let mut n = [1, 2, 3, 4, 5, 42, 3, 3, 4, 3, 42, 42, 42, 42, 42];
    dbg!((avg(&n), median(&mut n), mode(&n)));
    let s = "boyoyoy";
    dbg!(to_pig_latin(s));
    let mut company = Company(HashMap::new());
    loop {
        println!("Commands:");
        println!("Add _name_ to _department_");
        println!("Get [all|_department_]");
        let mut command = String::new();
        io::stdin().read_line(&mut command).expect("Failed to read line");
        match &command[..3] {
            "Add" => {
                let ls: Vec<&str> = command[4..].trim().split(' ').collect();
                company.add_employee(ls[2], ls[0]);
            },
            "Get" => {
                let department = command[4..].trim();
                match department {
                    "all" => println!("{:?}", company.get_all()),
                    _ => println!("{:?}", company.get_department(department))
                }

            },
            _ => {
                println!("Command not recognized!");
                break
            },
        };
    }
}


fn avg(x: &[i32]) -> f64 {
    x.iter().sum::<i32>() as f64 / x.len() as f64
}

fn median(x: &mut [i32]) -> i32 {
    x.sort();
    let i = x.len() / 2;
    x[i]
}

fn mode(x: &[i32]) -> i32 {
    let mut counts = HashMap::new();
    for &e in x {
        *counts.entry(e).or_insert(0) += 1;
    }
    *counts.iter().max_by_key(|&(_, count)| count).unwrap().0
}

fn to_pig_latin(s: &str) -> String {
    let vowels = vec!['a', 'e', 'i', 'o', 'u'];
    let mut iter = s.chars();
    let first = iter.next().unwrap();
    let rest = iter.collect::<String>();
    if vowels.contains(&first) {
        format!("{}{}-hay", first, rest)
    } else {
        format!("{}-{}ay", rest, first)
    }
}