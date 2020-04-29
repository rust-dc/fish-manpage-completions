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
use std::borrow::Cow;
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

pub enum CompletionsError {
    IOError(std::io::Error),
    DecoderError(std::io::Error),
    BZ2Error(bzip2::Error),
    UnknownExtension(String),
    EncodingError(Cow<'static, str>),
    NoStem(String),
    IgnoredCommand(String),
    LinkToBuiltin,
    NoExtension(String),
}
impl Into<std::io::Error> for CompletionsError {
    fn into(self) -> std::io::Error {
        let err_s = match self {
            Self::IOError(e) => return e,
            Self::DecoderError(e) => return e,
            Self::XZ2Error(e) => return e,
            Self::BZ2Error(e) => format!("{}", e),
            Self::UnknownExtension(e) => e,
            Self::EncodingError(e) => e.into(),
            Self::NoStem(e) => format!("Expected stem in file name {:?}", e),
            Self::IgnoredCommand(e) => format!("Skipped ignored command {:?}", e),
            Self::NoExtension(e) => format!("Unable to determine file type, no extension {}", e),
            Self::LinkToBuiltin => format!("Skipped link to builtin.1"),
        };

        std::io::Error::new(std::io::ErrorKind::Other, err_s)
    }
}
