#[derive(Debug)]
/*
Storing references in a struct requires to specify lifetimes, that's why in this example String is
used instead of str&
*/

struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

impl User {  // implementation block
    /*
    TYPES OF OWNERSHIP:
    * &self  -> reading while borrowing
    * &mut self -> mutating while borrowing
    * self -> consuming while taking ownership

    self refers to the instance relative to this struct. If omitted, this becomes a function and not
    a method anymore, called associated function
    */
    fn change_user(&mut self, s: String) {
        self.username = s;
    }
}

struct Color(i32, i32, i32);  // struct tuple, unnamed fields

fn main() {
    let mut user1 = create_user(String::from("Foo"), String::from("bar@baz"));
    user1.sign_in_count = 3;
    let mut user2 = User {
        email: String::from("foo@bar"),
        ..user1  // struct update syntax, missing fields will be taken from user1
    };
    user2.change_user(String::from("Baz"));
    println!("{:#?}", user2);  // pretty print available thanks to the derive clause at the top

}

fn create_user(username: String, email: String) -> User {
    User {
        username,  // field init shorthand: since they have same name, we don't need to username: username
        email,
        active: true,
        sign_in_count: 1,
    }
}