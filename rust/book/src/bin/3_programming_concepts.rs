fn main() {
    // tuple destructuring: we can give different types to each tuple member
    let x: (i32, f64, u8) = (500, 6.4, 1);
    // and access it with the dot + index notation
    let five_hundred = x.0;
    let six_point_four = x.1;
    let one = x.2;
    // array: still fixed length as tuples, but MUST be of the same type
    let a: [i32; 5] = [1, 2, 3, 4, 5];
    a[3];  // access through index notation. if index > len, runtime error happens
    for element in a.iter() {
        println!("Value is {}", element);
    }
    let x = another(42);
    println!("{}", x);
}

// declaration of another function, types must be explicitly declared. -> is return value
fn another(x: i32) -> i32 {
    let y = {  // {} block that creates a new scope
        let x = x + 3;
        x + 1  // no ending semicolon  -> this is an EXPRESSION, not a statement, so it returns a value
    };
    println!("Value of x passed: {}", x);
    y
    // x here is unchanged
}
