enum Message {  // enums have variants inside them
    Quit,
    Move { x: i32, y: i32 },  // anonymous struct
    Write(String),
    ChangeColor(i32, i32, i32),  // tuple
}

fn main() {
    let five = Some(5);
    let six = plus_one(five);
    let none = plus_one(None);
}

fn plus_one(n: Option<i8>) -> Option<i8> {
    match n {
        None => None,
        Some(n) => Some(n + 1),
    }
}