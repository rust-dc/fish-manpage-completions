#[macro_export]
macro_rules! regex {
    ($pattern: expr) => {{
        lazy_static::lazy_static! {
            static ref REGEX: regex::Regex = regex::Regex::new($pattern).unwrap();
        }
        &REGEX
    }};
}

use crate::char_len;
use std::{collections::HashMap, iter::FromIterator};

pub struct TranslationTable {
    table: HashMap<char, char>,
}
impl TranslationTable {
    pub fn new(f: &str, t: &str) -> Result<Self, &'static str> {
        if char_len(f) != char_len(t) {
            Err("Arguments passed to `TranslationTable::new` must have equal character length")
        } else {
            Ok(TranslationTable {
                table: HashMap::from_iter(f.chars().zip(t.chars())),
            })
        }
    }

    pub fn translate(&self, s: String) -> String {
        s.chars()
            .map(|c| *self.table.get(&c).unwrap_or(&c))
            .collect()
    }
}

#[test]
fn test_translationtable_new() {
    assert!(TranslationTable::new("aaa", "Incorrect Length").is_err());
    assert!(TranslationTable::new("aaa", "bbb").is_ok());
    let tr = TranslationTable::new("ab!", "cd.").unwrap();

    let mut expected = HashMap::new();
    expected.insert('a', 'c');
    expected.insert('b', 'd');
    expected.insert('!', '.');

    assert_eq!(tr.table, expected);

    // Unicode tests
    let tr = TranslationTable::new("🗻🚀🚁", "mrh").unwrap();
    let mut expected = HashMap::new();
    expected.insert('🗻', 'm');
    expected.insert('🚀', 'r');
    expected.insert('🚁', 'h');

    assert_eq!(tr.table, expected);
}

#[test]
fn test_translationtable_translate() {
    let tr = TranslationTable::new("ab!", "cd.").unwrap();

    assert_eq!(tr.translate("aabb!!".into()), "ccdd..".to_owned());

    assert_eq!(
        tr.translate("Hello World!".into()),
        "Hello World.".to_owned(),
    );

    assert_eq!(tr.translate("applebees!".into()), "cppledees.".to_owned(),);

    // Unicode tests
    let tr = TranslationTable::new("🗻🚀🚁", "mrh").unwrap();

    assert_eq!(
        tr.translate("This 🗻 is a mountain!".into()),
        "This m is a mountain!".to_owned(),
    );

    assert_eq!(
        tr.translate("This 🚀 is a rocket! (rocket.rs :))".into()),
        "This r is a rocket! (rocket.rs :))".to_owned(),
    );

    assert_eq!(
        tr.translate("This 🚁 is a helicopter!".into()),
        "This h is a helicopter!".to_owned(),
    )
}
