use std::io;

fn main() {
    println!("Insert a number");
    let mut s = String::new();
    io::stdin().read_line(&mut s).expect("Couldn't read line");
    let n: u32 = s.trim().parse().expect("Please type a number!");
    let t = fahr_to_celsius(n as f32);
    let f = fibo(n - 1);
    println!("As celsius: {}\nnth fibo number: {}", t, f);
    lyrics();
}


fn fahr_to_celsius(t: f32) -> f32 {
    (t - 32.0) * 5.0 / 9.0
}

fn fibo(n: u32) -> u32 {
    if n <= 1 {
        1
    } else {
        fibo(n - 1) + fibo(n - 2)
    }
}

fn lyrics() {
    let ordinals = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"];
    let things = ["a partridge in a pear tree", "two turtle doves", "three French hens", "four calling birds", "five gold rings", "six geese a laying",
                            "seven swans a swimming", "eight maids a milking", "nine ladies dancing", "ten lords a leaping", "eleven pipers piping", "12 drummers drumming"];
    for (i, ordinal) in ordinals.iter().enumerate() {
        println!("On the {} day of Christmas\nMy true love gave to me", ordinal);
        let mut j: i8 = i as i8;
        while j >= 0 {
            println!("{}", things[j as usize]);
            j -= 1;
        }
    }
}