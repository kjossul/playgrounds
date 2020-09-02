fn main() {
    // VECTORS
    let mut v = Vec::new();
    v.push(42);
    let n = v[0];
    let r = &mut v[0];
    *r = 0;
    println!("{}, {}", n, r);
    v.push(3);
    println!("{:?}", v);  // n and v can be used here, but r not
    // STRINGS
    let mut s = String::from("foo");
    s.push_str("bar");  // push_str uses string slices because it doesn't take ownership
    println!("{}", s);
    let s1 = String::from("Hello");
    let s2 = String::from("world!");
    let s3 = format!("{}, {}", s1, s2);  // MACROS DON'T TAKE OWNERSHIP
    println!("{} {} {}", s1, s2, s3);
    // note s1 has been moved here and can no longer be used
    // concatenation with + (add method) requires ownership of first string
    let s3 = s1 + ", " + &s2;
    println!("{} {}", s3, s2);
    // strings cannot be indexed due to how many different encodings work
    // s2[0]; <- this would panic
    s2.chars().nth(0).unwrap();  // this works but it's O(n) due to how it's implemented
    // HASHMAPS
    /*
    HashMaps don't have a literal initialization, because they shouldn't be used often. Use the
    maplit macro if you need this. However, maps should be used when keys are not known at compile
    time
    */
}
