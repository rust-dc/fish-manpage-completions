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
use std::cell::Cell;
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

    pub fn translate(&self, s: &str) -> String {
        s.chars()
            .map(|c| *self.table.get(&c).unwrap_or(&c))
            .collect()
    }
}

pub trait CellMap<T> {
    fn map_<F, R>(&self, f: F) -> R
    where
        F: Fn(&T) -> R;
}

impl<T: Default> CellMap<T> for Cell<T> {
    fn map_<F, R>(&self, f: F) -> R
    where
        F: Fn(&T) -> R,
    {
        let internal = self.take();
        let result = f(&internal);
        self.set(internal);
        result
    }
}

#[test]
fn test_cell_map() {
    let cell = Cell::from("Hello World!".to_owned());
    assert!(cell.map_(|s| s.contains("Hello")));
    assert_eq!(cell.take(), "Hello World!");
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

    assert_eq!(tr.translate("aabb!!"), "ccdd..".to_owned());

    assert_eq!(tr.translate("Hello World!"), "Hello World.".to_owned(),);

    assert_eq!(tr.translate("applebees!"), "cppledees.".to_owned(),);

    // Unicode tests
    let tr = TranslationTable::new("🗻🚀🚁", "mrh").unwrap();

    assert_eq!(
        tr.translate("This 🗻 is a mountain!"),
        "This m is a mountain!".to_owned(),
    );

    assert_eq!(
        tr.translate("This 🚀 is a rocket! (rocket.rs :))"),
        "This r is a rocket! (rocket.rs :))".to_owned(),
    );

    assert_eq!(
        tr.translate("This 🚁 is a helicopter!"),
        "This h is a helicopter!".to_owned(),
    )
}
