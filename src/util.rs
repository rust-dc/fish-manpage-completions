#[macro_export]
macro_rules! regex {
    ($pattern: expr) => {{
        lazy_static::lazy_static! {
            static ref REGEX: regex::Regex = regex::Regex::new($pattern).unwrap();
        }
        &REGEX
    }};
}

use std::{collections::HashMap, iter::FromIterator};

/// Helper for `translate`, does the same thing as string.maketrans in python
/// requires that f and t are the same length, or it takes the shorter one
pub fn maketrans(f: &str, t: &str) -> HashMap<char, char> {
    HashMap::from_iter(f.chars().zip(t.chars()))
}

/// Does effectively the same as string.translate in python, in
/// conjunction with `maketrans`, basically just replaces
pub fn translate(s: String, table: HashMap<char, char>) -> String {
    s.chars()
        .map(|c| *table.get(&c).unwrap_or(&c))
        .collect::<String>()
}

#[test]
fn test_maketrans() {
    let mut expected = HashMap::new();
    expected.insert('a', 'd');
    expected.insert('b', 'e');
    expected.insert('c', 'f');
    assert_eq!(maketrans("abc", "def",), expected);
}

#[test]
fn test_translate() {
    // >>> "Hello World!".translate(str.maketrans("HlW", "aBc"))
    // 'aeBBo corBd!'

    let expected = "aeBBo corBd!".to_owned();

    assert_eq!(
        translate("Hello World!".to_owned(), maketrans("HlW", "aBc",)),
        expected,
    )
}
