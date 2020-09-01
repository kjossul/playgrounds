fn main() {
    /*
    Each value in Rust has a variable that’s called its owner.
    There can only be one owner at a time.
    When the owner goes out of scope (i.e. it's not valid anymore), the value will be dropped.

    Here we have moved s1, and after s2 declaration we can no longer access it. This is to prevent double free problems
    Basically there is no shallow copy for data on the heap, just moving
    bools, integers, float, char and tuples are all stored on the stack and thus get not moved
    */

    let s1 = String::from("Hello");
    let s2 = s1;  // s1 is moved cannot be used!
    println!("{}", s2);
    /* To avoid this issue, we need to use references. When we acquire a reference to another variable,
    we don't take ownership, so the referenced variable doesn't get freed when the reference gets
    out of scope -> BORROWING
    We are not allowed to change values from immutable references, we need mutable references.
    However,  mutable references have one big restriction: you can have only one mutable reference
    to a particular piece of data in a particular scope AND you cannot have a mutable reference while
    you also have immutable ones. THIS PREVENTS DATA RACES AT COMPILE TIME:
    At any given time, you can have either one mutable reference or any number of immutable references.
    References must always be valid.
    */
    let s = String::from("Hello World!");
    let result = first_word(&s);
    println!("{}", result);

}

fn no_change(some_string: &String) {
    // some_string.push_str(", world");  this won't compile because we are trying to change an immutable reference
}

fn change(s: &mut String) {
    s.push_str(", world");
    let r1 = &s; // no problem
    let r2 = &s; // no problem
    println!("{} and {}", r1, r2);
    // r1 and r2 are no longer used after this point

    let r3 = s; // no problem
    println!("{}", r3);
}

fn first_word(s: &str) -> &str {
    // returns the first word found in the sentence s
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];  // slice operator: reference to a part of a string
        }
    }

    &s[..]
}