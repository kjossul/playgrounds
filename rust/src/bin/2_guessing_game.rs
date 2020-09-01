use rand::Rng;
use std::io;
use std::cmp::Ordering;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1, 101);

    loop {
        println!("Guess the number!");
        println!("Please input your guess between 1 and 100.");
        let mut guess = String::new();  // mut: mutable

        io::stdin()  // returns instance of Stdin()
            .read_line(&mut guess)  // reference to a mutable object
            .expect("Failed to read line");
        /*
            read_line returns a Result value, which is an enumeration type. Every result instance has a
            expect method, which is used to handle failures. Not calling this method would generate a
            compile time warning
            */
        // shadowing! we can reuse the name given to a previous variable, but change the type
        // We also switch from an expect expression to a match one, to handle the error
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue  // _ is a catchall value: match all error values
        };
        // match pattern, compares the value against each arm
        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal => {
                println!("That's right!");
                break;
            },
        }
    }
}