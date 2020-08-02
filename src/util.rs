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

    pub fn translate(&self, s: &str) -> String {
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

// Tries to guess the encoding, not perfect but does it's job
pub fn decode_bytes<Bytes: Into<Vec<u8>>>(bytes: Bytes) -> Option<String> {
    let bytes = bytes.into();
    let (label, confidence, _) = chardet::detect(&bytes);

    println!("Trying to decode with {}", label);
    if confidence < 0.5 {
        // TODO: What should we do here?
        eprintln!("low confidence ({})", confidence);
    }

    let encoding = encoding_rs::Encoding::for_label(label.as_bytes())?;

    let (output, had_misses) = encoding.decode_with_bom_removal(&bytes);
    if had_misses {
        // TODO: What should we do here?
        eprintln!("Had misses :(");
    }
    Some(output.into_owned())
}

#[test]
fn test_decode_bytes() {
    // TODO: Add more tests, pls be forgiving, it's not perfect, it's a guess
    // better than nothing ?
    let bytes = (0xB0..=0xEF).collect::<Vec<u8>>();
    assert_eq!(
        decode_bytes(bytes).unwrap(),
        "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдежзийклмнопрстуфхцчшщъыьэюя"
    );
}
