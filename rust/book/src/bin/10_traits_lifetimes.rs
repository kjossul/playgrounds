/*
Traits are similar to interfaces in other languages.
Here we use generics to write a largest function, specifying that the type of the array passed as
parameter must implement the PartialOrd trait. We also specify the Copy trait, such that the largest
item can be "moved out", otherwise we need to adjust our code to return &T instead.
*/

use std::fmt::Display;

fn largest<T: PartialOrd + Copy>(list: &[T]) -> T {
    let mut largest = list[0];

    for &item in list {
        if item > largest {
            largest = item;
        }
    }

    largest  // moving out ownership, can be done thanks to the Copy trait
}

struct Pair<T> {
    x: T,
    y: T,
}

impl<T> Pair<T> {
    fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

// conditional implementation: cmp_display is only implemented for types that satisfy those traits
impl<T: Display + PartialOrd> Pair<T> {
    fn cmp_display(&self) {
        if self.x >= self.y {
            println!("The largest member is x = {}", self.x);
        } else {
            println!("The largest member is y = {}", self.y);
        }
    }
}

fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let result = largest(&number_list);
    println!("The largest number is {}", result);

    let char_list = vec!['y', 'm', 'a', 'q'];

    let result = largest(&char_list);
    println!("The largest char is {}", result);

    let string1 = String::from("abcd");
    let string2 = "xyz";
    // without specifying lifetimes this wouldn't work!
    let result = longest(string1.as_str(), string2);
    println!("The longest string is {}", result);
    /* this won't work because string2 doesn't leave long enough with the annotated lifetimes
    let string1 = String::from("long string is long");
    let result;
    {
        let string2 = String::from("xyz");
        result = longest(string1.as_str(), string2.as_str());
    }
    println!("The longest string is {}", result);  -- borrow used on possibly invalid value
    */
}

// LIFETIMES - think about connecting lifetimes of parameters and return values
// These don't automatically make code work, rather tell the borrow checker which code is invalid
// Note: sometimes it's not necessary to specify lifetimes due to elision, but here we have two
// parameters in a non-method function thus we need to be explicit because compiler won't know
// Here we specify that both parameters and return value must have the same lifetime 'a
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

// the instance of ImportantExcerpt is not allowed to outlive the reference it holds in its field
struct ImportantExcerpt<'a> {
    part: &'a str,
}
