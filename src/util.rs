#[macro_export]
macro_rules! regex {
    ($pattern: expr) => {{
        lazy_static::lazy_static! {
            static ref REGEX: regex::Regex = regex::Regex::new($pattern).unwrap();
        }
        &REGEX
    }};
}

use std::{
    iter::FromIterator,
    collections::HashMap,
};


/// Helper for `translate`, does the same thing as string.maketrans in python
/// requires that f and t are the same length, or it takes the shorter one
pub fn maketrans(f: &str, t: &str) -> HashMap<u8, char> {
    HashMap::from_iter(f.bytes().zip(t.chars()))
}

/// Does effectively the same as string.translate in python, in 
/// conjunction with `maketrans`, basically just replaces
pub fn translate(s: String, table: HashMap<u8, char>) -> String {
    let mut output = String::new();
    for byte in s.bytes() {
        if let Some(c) = table.get(&byte) {
            output.push(*c);
        } else {
            output.push(byte as char);
        }
    }

    output
}

#[test]
fn test_maketrans() {
    let mut expected = HashMap::new();
    expected.insert(0x61, 'd');
    expected.insert(0x62, 'e');
    expected.insert(0x63, 'f');
    assert_eq!(
        maketrans(
            "abc",
            "def",
        ),
        expected
    );
}

#[test]
fn test_translate() {
    // >>> "Hello World!".translate(str.maketrans("HlW", "aBc"))
    // 'aeBBo corBd!'

    let expected = "aeBBo corBd!".to_owned();

    assert_eq!(
        translate(
            "Hello World!".to_owned(),
            maketrans(
                "HlW",
                "aBc",
            )
        ),
        expected,
    )
}
