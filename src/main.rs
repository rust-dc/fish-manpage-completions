//! A translation of https://github.com/fish-shell/fish-shell/blob/e7bfd1d71ca54df726a4f1ea14bd6b0957b75752/share/tools/create_manpage_completions.py
//!
//! Copyright/license of original Python source:
//!
//! Copyright (c) 2012, Siteshwar Vashisht
//! All rights reserved.
//!
//! Redistribution and use in source and binary forms, with or without
//! modification, are permitted provided that the following conditions
//! are met:
//!
//! Redistributions of source code must retain the above copyright
//! notice, this list of conditions and the following disclaimer.
//!
//! Redistributions in binary form must reproduce the above copyright
//! notice, this list of conditions and the following disclaimer in the
//! documentation and/or other materials provided with the distribution.
//!
//! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
//! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
//! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
//! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
//! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
//! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
//! POSSIBILITY OF SUCH DAMAGE.

use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fmt};

use bzip2::read::BzDecoder;
use flate2::read::GzDecoder;
use indicatif::ParallelProgressIterator;
use itertools::Itertools;
use rayon::prelude::*;
use structopt::StructOpt;
use tracing_subscriber::filter::LevelFilter;
use xz2::read::XzDecoder;

#[cfg(test)]
use pretty_assertions::assert_eq;

mod util;

macro_rules! regex {
    ($pattern: expr) => {{
        lazy_static::lazy_static! {
            static ref REGEX: regex::Regex = regex::Regex::new($pattern).unwrap();
        }
        &REGEX
    }};
}

// def unquote_double_quotes(data):
//     if (len(data) < 2):
//         return data
//     if data[0] == '"' and data[len(data)-1] == '"':
//         data = data[1:len(data)-1]
//     return data

fn unquote_double_quotes(data: &str) -> &str {
    if data.len() > 2 && (data.as_bytes()[0], *data.as_bytes().last().unwrap()) == (b'"', b'"') {
        &data[1..(data.len() - 1)]
    } else {
        data
    }
}

// def unquote_single_quotes(data):
//     if (len(data) < 2):
//         return data
//     if data[0] == '`' and data[len(data)-1] == '\'':
//         data = data[1:len(data)-1]
//     return data

fn unquote_single_quotes(data: &str) -> &str {
    if data.len() > 2 && (data.as_bytes()[0], *data.as_bytes().last().unwrap()) == (b'\'', b'\'') {
        &data[1..(data.len() - 1)]
    } else {
        data
    }
}

fn fish_escape_single_quote(s: &str) -> String {
    if s.chars()
        .all(|c| c.is_ascii_alphanumeric() || "_+-|/:=@~".contains(c))
    {
        String::from(s)
    } else {
        format!("'{}'", s.replace(r"\", r"\\").replace(r"'", r"\'"))
    }
}

// Rust port replaced with String::from_utf8_lossy(s.as_bytes())
//
// # Make a string Unicode by attempting to decode it as latin-1, or UTF8. See #658
// def lossy_unicode(s):
//     # All strings are unicode in Python 3
//     if IS_PY3 or isinstance(s, unicode): return s
//     try:
//         return s.decode('latin-1')
//     except UnicodeEncodeError:
//         pass
//     try:
//         return s.decode('utf-8')
//     except UnicodeEncodeError:
//         pass
//     return s.decode('latin-1', 'ignore')

// #[test]
// fn test_lossy_unicode() {
//     let bytes = b"123 456";
//     let s = lossy_unicode(bytes);
//     assert_eq!("123 456", s);
//
//     let bad_bytes = &[255];
//     let bad_s = lossy_unicode(bad_bytes);
//     assert_eq!("�", bad_s);
// }

const MAX_DESCRIPTION_WIDTH: usize = 78;
const TRUNCATION_SUFFIX: char = '…';

fn char_len(string: &str) -> usize {
    string.chars().count()
}

fn fish_options(options: &str, existing_options: &mut HashSet<String>) -> Vec<String> {
    let mut out = vec![];

    for option in regex!(r###"[ ,"=\[\]]"###).split(options) {
        let option = regex!(r###"\[.*\]"###).replace_all(option, "");
        let option = regex!(
            r###"(?x)
                ^ [ \t\r\n\[\](){}.,:!]
                | [ \t\r\n\[\](){}.,:!] $
            "###
        )
        .replace_all(&option, "");

        if option == "-" || option == "--" {
            continue;
        }

        if regex!(r###"[{}()]"###).is_match(&option) {
            continue;
        }

        let (fish_opt, num_dashes) = if option.starts_with("--") {
            ("l", 2)
        } else if option.starts_with("-") {
            (if option.len() == 2 { "s" } else { "o" }, 1)
        } else {
            continue;
        };

        let option = format!(
            "-{} {}",
            fish_opt,
            // Direct indexing of `option` won't panic due to how `num_dashes`
            // is calculated. (I promise!)
            fish_escape_single_quote(&option[num_dashes..])
        );

        if existing_options.insert(option.clone()) {
            out.push(option);
        }
    }

    out
}

#[test]
fn test_fish_options() {
    use std::iter::once;

    let expected_output: Vec<String> = vec!["-s f".into(), "-l force".into()];
    let options = "-f, --force[=false]";
    let mut existing_options: HashSet<String> = Default::default();
    assert_eq!(
        fish_options(options, &mut existing_options),
        expected_output
    );
    assert_eq!(
        expected_output.iter().cloned().collect::<HashSet<_>>(),
        existing_options
    );

    let expected_output: Vec<String> = vec!["-s \'\\\'\'".into()];
    let options = "-'";
    let mut existing_options: HashSet<String> = Default::default();
    assert_eq!(
        fish_options(options, &mut existing_options),
        expected_output
    );
    assert_eq!(
        expected_output.iter().cloned().collect::<HashSet<_>>(),
        existing_options
    );

    let expected_output: Vec<String> = vec!["-l force".into()];
    let options = "-f, --force[=false]";
    let mut existing_options: HashSet<String> = Default::default();
    existing_options.insert("-s f".into());
    existing_options.insert("-l something".into());
    assert_eq!(
        fish_options(options, &mut existing_options),
        expected_output
    );
    assert_eq!(
        expected_output
            .iter()
            .cloned()
            .chain(once("-s f".to_string()))
            .chain(once("-l something".to_string()))
            .collect::<HashSet<_>>(),
        existing_options,
    );
}

/// # Panics
/// If `max_length` is zero.
fn char_truncate_string(string: &str, max_length: usize, truncator: char) -> Cow<str> {
    let mut char_indices = string
        .char_indices()
        .skip(max_length.checked_sub(1).unwrap())
        .map(|(idx, _)| idx);

    let penultimate = char_indices.next();

    if char_indices.next().is_some() {
        // Okay to unwrap since the item element _following_ `penultimate` is
        // `Some`.
        format!("{}{}", &string[0..penultimate.unwrap()], truncator).into()
    } else {
        string.into()
    }
}

#[test]
fn test_string_truncation() {
    assert_eq!(char_truncate_string("abc", 3, '…'), "abc");

    assert_eq!(char_truncate_string("abcd", 3, '…'), "ab…");

    assert_eq!("ßbc".len(), 4);
    assert_eq!(char_truncate_string("ßbc", 3, '…'), "ßbc");

    assert_eq!(char_truncate_string("abß", 3, '…'), "abß");

    assert_eq!("aßbc".len(), 5);
    assert_eq!("aß…".len(), 6);
    assert_eq!(char_truncate_string("aßbc", 3, '…'), "aß…");
}

fn truncated_description(description: &str) -> String {
    let sentences = description.replace(r"\'", "'").replace(r"\.", ".");

    let mut sentences = sentences
        .split(".")
        .filter(|sentence| !sentence.trim().is_empty());

    let out = sentences.next().unwrap_or_default();
    let mut out = format!("{}.", String::from_utf8_lossy(out.as_bytes()));
    let mut out_len = char_len(&out);

    if out_len > MAX_DESCRIPTION_WIDTH {
        out = char_truncate_string(&out, MAX_DESCRIPTION_WIDTH, TRUNCATION_SUFFIX).into_owned();
    } else {
        for line in sentences {
            out_len += 1 // space
                + char_len(&line)
                + 1; // period
            if out_len > MAX_DESCRIPTION_WIDTH {
                break;
            }
            out = format!("{} {}.", out, String::from_utf8_lossy(line.as_bytes()));
        }
    }

    fish_escape_single_quote(&out.trim_end_matches('.'))
}

#[test]
fn test_truncated_description() {
    assert_eq!(truncated_description(r"\'\."), r"'\''");

    assert_eq!(
        truncated_description(r"Don't use this command."),
        r"'Don\'t use this command'"
    );

    assert_eq!(
        truncated_description(r"Don't use this command. It's really dumb."),
        r"'Don\'t use this command.  It\'s really dumb'"
    );

    assert_eq!(
        truncated_description(
            r"The description for the command is so long. This second sentence will be dropped, in fact, because it is too long to be displayed comfortably."
        ),
        r"'The description for the command is so long'"
    );

    assert_eq!(
        truncated_description(
            r"This single, initial sentence exceeds the `MAX_DESCRIPTION_WIDTH` and so it will not be displayed in its entirety, which is a crying shame."
        ),
        r"'This single, initial sentence exceeds the `MAX_DESCRIPTION_WIDTH` and so it w…'"
    );

    assert_eq!(
        // Note: This behavior seems wrong. Should probably change to remove extra spaces.
        truncated_description(
            r"     Dumb command.   \It's really dumb\.  Extra spaces aren\'t removed.    "
        ),
        r"'     Dumb command.    \\It\'s really dumb.   Extra spaces aren\'t removed'"
    );
}

struct Completions<'a> {
    cmdname: &'a str,
    // TODO should we store the whole built_command here?
    built_command_output: Vec<String>,
    existing_options: HashSet<String>,
}

impl<'a> Completions<'a> {
    fn new(cmdname: &'a str) -> Completions {
        Completions {
            cmdname,
            built_command_output: Vec::new(),
            existing_options: HashSet::new(),
        }
    }

    fn add(&mut self, option_name: &str, option_desc: &str) {
        let fish_options = fish_options(option_name, &mut self.existing_options);

        if fish_options.is_empty() {
            return;
        }

        self.built_command_output.push(complete_command(
            &fish_escape_single_quote(self.cmdname),
            fish_options,
            &truncated_description(option_desc),
        ));
    }

    fn build(self) -> Option<String> {
        let mut s = self.built_command_output.join("\n");
        if s.is_empty() {
            None
        } else {
            s.push('\n'); // add trailing whitespace
            Some(s)
        }
    }
}

/// Generate fish `complete` command.
fn complete_command(cmdname: &str, args: Vec<String>, description: &str) -> String {
    let mut out = format!("complete -c {} {}", cmdname, args.join(" "));
    if !description.is_empty() {
        out.push_str(" --description ");
        out.push_str(description);
    }
    out
}

#[test]
fn test_complete_command() {
    assert_eq!(
        complete_command(
            "tr".into(),
            vec![
                "-s".into(),
                "s".into(),
                "-l".into(),
                "squeeze-repeats".into(),
            ],
            "'replace each input sequence of a repeated character that is listed in SET1 …'",
        ),
        "complete \
         -c tr \
         -s s \
         -l squeeze-repeats \
         --description 'replace each input sequence of a repeated character that is listed in SET1 …'"
    );

    assert_eq!(
        complete_command(
            "tr".into(),
            vec![
                "-s".into(),
                "s".into(),
                "-l".into(),
                "squeeze-repeats".into(),
            ],
            "",
        ),
        "complete \
         -c tr \
         -s s \
         -l squeeze-repeats"
    );
}

fn remove_groff_formatting(data: &str) -> Cow<str> {
    // // TODO Can we remove all of these strings in one go?
    // let mut data = data.to_owned();
    // for marker in &[
    //     r"\fI", r"\fP", r"\f1", r"\fB", r"\fR", r"\e", r".BI", r".BR", r"0.5i", r".rb", r"\^",
    //     r"{ ", r" }", ".B",
    //     ".I",
    //     //     The next ones are odd. Putting them into a python file makes my
    //     //     python linter warn about anomalous backslash and python2 vs python3
    //     //     seems to make no difference
    //     //     data = data.replace("\ ","")
    //     //     data = data.replace("\-","-")
    //     //"\ ",
    //     //"\&",
    //     //"\f",
    // ] {
    //     data = data.replace(marker, "");
    // }
    // // See note above about anomalous backslash
    // //data = data.replace("\-", "-");
    // let data = regex!(r##".PD( \d+)"##).replace_all(&data, "");
    // data.to_string()
    // using regex is twice as fast as manual replace
    let re1 = regex!(
        r"\\fI|\\fP|\\f1|\\fB|\\fR|\\e|\.BI|\.BR|0\.5i|\.rb|\\\^|\{ | \}|\.B|\.I|\f|(.PD( \d+))"
    );
    let re2 = regex!(r"\\-");
    let re3 = regex!(r"\(cq");
    match re1.replace_all(&data, "") {
        Cow::Borrowed(s) => match re2.replace_all(&s, "-") {
            Cow::Borrowed(s) => re3.replace_all(&s, "'"),
            Cow::Owned(s) => Cow::Owned(re3.replace_all(&s, "'").into_owned()),
        },
        Cow::Owned(s) => Cow::Owned(re3.replace_all(&re2.replace_all(&s, "-"), "'").into_owned()),
    }
}

#[test]
fn test_remove_groff_formatting() {
    assert_eq!(
        remove_groff_formatting(r#"Foo\fIbar\fP Zoom.PD 325 Zoom"#),
        "Foobar Zoom Zoom"
    );
    assert_eq!(
        remove_groff_formatting(
            r#"\n\\fB\\-\\-working\\-directory\\fR=\\fIvalue\\fR\nWorking directory.\n"#
        ),
        "\\n\\\\-\\-working\\-directory\\=\\value\\\\nWorking directory.\\n"
    );
}

trait ManParser {
    fn is_my_type(&self, manpage: &str) -> bool;

    fn parse_man_page(&self, _manpage: &str, _cmdname: &str) -> Option<String>;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type1;

impl ManParser for Type1 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(r#".SH "OPTIONS""#)
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r#"\.SH "OPTIONS"((?s:.)*?)(\.SH|\z)"#);
        let options_section_matched = options_section_re.find(manpage);
        let mut options_section = options_section_matched.unwrap().as_str();

        let options_parts_re = regex!(r"\.PP((?s:.)*?)\.RE");
        let mut options_matched = options_parts_re.captures(options_section);
        tracing::info!("Command is {}", cmdname);

        if options_matched.is_none() {
            tracing::info!("Unable to find options");
            return self
                .fallback(options_section, cmdname)
                .or_else(|| self.fallback2(options_section, cmdname));
        }

        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let mut data = mat.get(1).unwrap().as_str();
            let last_dotpp_index = data.rfind(".PP");
            if let Some(idx) = last_dotpp_index {
                data = &data[idx + 3..];
            }

            let data = remove_groff_formatting(data);
            if let Some((option_name, option_desc)) = data.splitn(2, ".RS 4").next_tuple::<(_, _)>()
            {
                let option_name = option_name.trim();
                if option_name.contains('-') {
                    let option_name = unquote_double_quotes(option_name);
                    let option_name = unquote_single_quotes(option_name);
                    let option_desc = option_desc.trim().replace('\n', " ");
                    completions.add(option_name, &option_desc);
                } else {
                    tracing::info!("{:?} doesn't contain '-'", option_name);
                }
            } else {
                tracing::info!("Unable to split option from description");
                return None;
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }
        completions.build()
    }
}

impl Type1 {
    fn fallback(&self, mut options_section: &str, cmdname: &str) -> Option<String> {
        tracing::info!("Trying fallback");
        let options_parts_re = regex!(r"\.TP( \d+)?((?s:.)*?)\.TP");
        let mut options_matched = options_parts_re.captures(options_section);
        if options_matched.is_none() {
            tracing::info!("Still not found");
            return None;
        }
        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(2).unwrap().as_str();
            let data = remove_groff_formatting(data);
            let data = data.splitn(2, '\n').next_tuple::<(_, _)>();
            if data.filter(|data| !data.1.trim().is_empty()).is_none() {
                tracing::info!("Unable to split option from description");
                return None;
            }
            let option_name = data.unwrap().0.trim();
            if option_name.contains('-') {
                let option_name = unquote_double_quotes(option_name);
                let option_name = unquote_single_quotes(option_name);
                let option_desc = data.unwrap().1.trim().replace('\n', " ");
                completions.add(option_name, &option_desc);
            } else {
                tracing::info!("{:?} does not contain '-'", option_name);
            }
            // XXX possible to add fallback2 here

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }
        completions.build()
    }

    fn fallback2(&self, options_section: &str, cmdname: &str) -> Option<String> {
        tracing::info!("Trying last chance fallback");
        let ix_remover_re = regex!(r"\.IX.*");
        let trailing_num_re = regex!(r"\d+$");
        let options_parts_re = regex!(r"\.IP ((?s:.)*?)\.IP");

        let mut options_section = &*ix_remover_re.replace_all(options_section, "");
        let mut options_matched = options_parts_re.captures(&options_section);
        if options_matched.is_none() {
            tracing::info!("Still (still!) not found");
            return None;
        }
        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(1).unwrap().as_str();
            let data = remove_groff_formatting(data);
            let data: Vec<&str> = data.splitn(2, '\n').collect();
            if data.len() < 2 || data[1].trim().is_empty() {
                tracing::info!("Unable to split option from description");
                return None;
            }
            let option_name = trailing_num_re.replace_all(data[0].trim(), "");
            if option_name.contains('-') {
                let option_name = option_name.trim();
                let option_name = unquote_double_quotes(option_name);
                let option_name = unquote_single_quotes(option_name);
                let option_desc = data[1].trim().replace('\n', " ");
                completions.add(option_name, &option_desc);
            } else {
                tracing::info!("{:?} doesn't contain '-'", option_name);
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(&options_section);
        }
        completions.build()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type2;

impl ManParser for Type2 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH OPTIONS")
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r#"\.SH OPTIONS((?s:.)*?)(\.SH|\z)"#);
        let options_section_matched = options_section_re.captures(manpage);
        let mut options_section = options_section_matched.unwrap().get(1).unwrap().as_str();

        let options_parts_re = regex!(r#"\.[IT]P( \d+(\.\d)?i?)?((?s:.)*?)\.([IT]P|UNINDENT)"#);
        let mut options_matched = options_parts_re.captures(options_section);
        tracing::info!("Command is {}", cmdname);

        if options_matched.is_none() {
            tracing::info!("Unable to find options");
            return None;
        }

        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(3).unwrap().as_str();
            let data = remove_groff_formatting(data);
            let data = data.trim().splitn(2, '\n').next_tuple::<(_, _)>();
            if let Some((option_name, option_desc)) =
                data.filter(|(_, desc)| !desc.trim().is_empty())
            {
                let option_name = option_name.trim();
                if option_name.contains('-') {
                    let option_name = unquote_double_quotes(option_name);
                    let option_name = unquote_single_quotes(option_name);
                    let option_desc = option_desc.trim().replace('\n', " ");
                    completions.add(option_name, &option_desc);
                } else {
                    tracing::info!("{:?} doesn't contain '-'", option_name);
                }
            } else {
                tracing::info!("Unable to split option from description");
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }
        completions.build()
        // TODO not sure why but the original version never succeed here
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type3;

impl ManParser for Type3 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH DESCRIPTION")
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r"\.SH DESCRIPTION((?s:.)*?)(\.SH|\z)");
        let options_section_matched = options_section_re.find(manpage);
        let mut options_section = options_section_matched.unwrap().as_str();

        let options_parts_re = regex!(r"\.TP((?s:.)*?)\.TP");
        let mut options_matched = options_parts_re.captures(options_section);
        tracing::info!("Command is {}", cmdname);

        if options_matched.is_none() {
            tracing::info!("Unable to find options section");
            return None;
        }

        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(1).unwrap().as_str();

            let data = remove_groff_formatting(data);
            let data = data.trim();
            let (option_name, option_desc) = match data.splitn(2, '\n').next_tuple() {
                Some(tuple) => tuple,
                None => {
                    tracing::info!("Unable to split option from description");
                    return None;
                }
            };
            let option_name = option_name.trim();
            if option_name.contains('-') {
                let option_name = unquote_double_quotes(option_name);
                let option_name = unquote_single_quotes(option_name);
                let option_desc = option_desc.trim().replace("\n", " ");
                completions.add(&option_name, &option_desc);
            } else {
                tracing::info!("{:?} doesn't contain '-'", option_name);
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(&options_section);
        }
        completions.build()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type4;

impl ManParser for Type4 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH FUNCTION LETTERS")
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r"\.SH FUNCTION LETTERS((?s:.)*?)(\.SH|\z)");
        let options_section_matched = options_section_re.captures(manpage);
        let mut options_section = options_section_matched.unwrap().get(1).unwrap().as_str();

        let options_parts_re = regex!(r"\.TP((?s:.)*?)\.TP");
        let mut options_matched = options_parts_re.captures(options_section);
        tracing::info!("Command is {}", cmdname);

        if options_matched.is_none() {
            tracing::info!("Unable to find options section");
            return None;
        }

        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(1).unwrap().as_str();
            let data = remove_groff_formatting(data);
            if let Some((option_name, option_desc)) = data.trim().splitn(2, '\n').next_tuple() {
                let option_name = option_name.trim();
                if option_name.contains('-') {
                    let option_name = unquote_double_quotes(option_name);
                    let option_name = unquote_single_quotes(option_name);
                    let option_desc = option_desc.trim().replace('\n', " ");
                    completions.add(option_name, &option_desc);
                } else {
                    tracing::info!("{} doesn't contain '-'", option_name);
                }
            } else {
                tracing::info!("Unable to split option from description");
                return None;
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }
        completions.build()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TypeScdoc;

impl ManParser for TypeScdoc {
    fn is_my_type(&self, manpage: &str) -> bool {
        regex!(r#"\.\\" Generated by scdoc(?s:.)?*\.SH OPTIONS"#).is_match(manpage)
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r"\.SH OPTIONS((?s:.)*?)\.SH");
        let options_section_matched = options_section_re.captures(manpage);
        let mut options_section = options_section_matched.unwrap().get(1)?.as_str();

        let options_parts_re = regex!(r"((?s:.)*?)\.RE");
        let mut options_matched = options_parts_re.captures(options_section);
        tracing::info!("Command is {}", cmdname);

        if options_matched.is_none() {
            tracing::info!("Unable to find options section");
            return None;
        }

        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(1).unwrap().as_str();
            let data = remove_groff_formatting(data);

            // Should be at least two lines, split name and desc, other lines ignored
            let lines = data.split('\n');
            let mut iter = lines.filter(|s| !["", ".P", ".RS 4"].contains(s));
            if let Some((option_name, option_desc)) = iter.next_tuple() {
                let option_name = unquote_double_quotes(option_name);
                let option_name = unquote_single_quotes(option_name);
                if !option_name.contains('-') {
                    tracing::info!("{} doesn't contain '-'", option_name);
                }
                completions.add(option_name, option_desc);
            } else {
                tracing::info!("Unable to split option from description");
            }

            options_section = &options_section[mat.get(0).unwrap().end()..];
            options_matched = options_parts_re.captures(options_section);
        }
        completions.build()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TypeDarwin;

impl ManParser for TypeDarwin {
    fn is_my_type(&self, manpage: &str) -> bool {
        regex!(r##"\.S[hH] DESCRIPTION"##).is_match(manpage)
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let mut lines = manpage.split_terminator("\n").skip_while(|cond| {
            !cond.starts_with(".Sh DESCRIPTION") || !cond.starts_with(".SH DESCRIPTION")
        });

        let mut completions = Completions::new(cmdname);
        while let Some(line) = lines.next() {
            if !Self::is_option(line) {
                continue;
            }

            // Try to guess how many dashes this argument has
            let dash_count = Self::count_argument_dashes(line);

            let line = Self::groff_replace_escapes(line);
            let line = Self::trim_groff(&line);
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            // Extract the name
            let name = line.split_whitespace().next().unwrap();

            // Extract the description
            let desc = lines
                .by_ref()
                .take_while(|line| Self::is_option(line))
                .filter(|line| line.starts_with(".") && !line.starts_with(".\"")) // Ignore comments
                .map(Self::groff_replace_escapes)
                .map(|line| Self::trim_groff(&line))
                .filter(|line| !line.is_empty())
                .collect::<Vec<_>>()
                .join(" ");

            if name == "-" {
                // Skip double -- arguments
                continue;
            }
            let name = if name.len() == 1 {
                format!("{}{}", "-".repeat(dash_count as usize), name)
            } else {
                format!("-{}", name)
            };
            completions.add(&name, &desc);
        }
        completions.build()
    }
}

#[test]
fn test_type_darwin_trim_groff() {
    assert_eq!(TypeDarwin::trim_groff(". Test"), "Test");
    assert_eq!(TypeDarwin::trim_groff("..."), "..");
    assert_eq!(TypeDarwin::trim_groff(" Test"), "Test");
    assert_eq!(TypeDarwin::trim_groff("Test ."), "Test.");
    assert_eq!(TypeDarwin::trim_groff("Test ,"), "Test,");
    assert_eq!(TypeDarwin::trim_groff("Ab "), "");
    assert_eq!(TypeDarwin::trim_groff(".Ab Dd Fz ZZ ."), "ZZ.");
    assert_eq!(TypeDarwin::trim_groff("Test , ."), "Test ,.");
    assert_eq!(TypeDarwin::trim_groff("Test . ,"), "Test .,");
}

impl TypeDarwin {
    fn trim_groff(line: &str) -> String {
        // Orig Python code would transform:
        // "This is a comment. An interesting example."
        // into " interesting example."
        // This port changes the regex to find the pattern at the start of the line
        // instead of anywhere in the line.

        // Remove initial period
        // Skip leading groff crud
        let line = regex!(r"^\.?([A-Z][a-z]\s)*").replace(&line, "");
        // If the line ends with a space and then a period or comma, then erase the space
        regex!(r" ([.,])$").replace(&line, "$1").trim().to_string()
    }
}

#[test]
fn test_replace_all() {
    let (string, num) = replace_all("");
    assert_eq!(string, "");
    assert_eq!(num, 0);

    let (string, num) = replace_all(".xyzppp");
    assert_eq!(string, "ppp");
    assert_eq!(num, 0);

    let (string, num) = replace_all("Fl jkl");
    assert_eq!(string, "Fl jkl");
    assert_eq!(num, 0);

    let (string, num) = replace_all(".xxxFl jkl");
    assert_eq!(string, "jkl");
    assert_eq!(num, 1);

    let (string, num) = replace_all(".Fl Fl Fl jkl");
    assert_eq!(string, "jkl");
    assert_eq!(num, 2);
}

use std::borrow::Cow;
fn replace_all(line: &str) -> (Cow<str>, u32) {
    let mut result = 0;
    (
        regex!(r"^(?:\....)((?:Fl\s)*)").replace(&line, |captures: &regex::Captures| {
            result = (captures[1].len() / 3) as u32; // Divide by 3 since there are 3 bytes per `Fl\s` pattern
            ""
        }),
        result,
    )
}

#[test]
fn test_type_darwin_count_argument_dashes() {
    assert_eq!(TypeDarwin::count_argument_dashes(".Fl Fl xx"), 1);
    assert_eq!(TypeDarwin::count_argument_dashes(".xxxFl Fl "), 2);
    assert_eq!(TypeDarwin::count_argument_dashes(".xxxFl FL "), 1);
    assert_eq!(TypeDarwin::count_argument_dashes("Fl Fl Fl "), 0);
    assert_eq!(TypeDarwin::count_argument_dashes(".Fl "), 0);
}

impl TypeDarwin {
    fn count_argument_dashes(line: &str) -> u32 {
        replace_all(&line).1
    }
}

#[test]
fn test_type_darwin_groff_replace_escapes() {
    // tests for expected replacements
    assert!(TypeDarwin::groff_replace_escapes(".Nm") == "CMDNAME");
    assert!(TypeDarwin::groff_replace_escapes("\\ ") == " ");
    assert!(TypeDarwin::groff_replace_escapes(r"& ") == "");
    assert!(TypeDarwin::groff_replace_escapes(r"\ .Nm & ") == " CMDNAME ");
    // tests for no expected replacement
    assert!(TypeDarwin::groff_replace_escapes(".N") == ".N");
    assert!(TypeDarwin::groff_replace_escapes(r"\x ") == r"\x ");
    assert!(TypeDarwin::groff_replace_escapes(r"&") == "&");
}

impl TypeDarwin {
    fn groff_replace_escapes(line: &str) -> String {
        line.replace(".Nm", "CMDNAME")
            .replace("\\ ", " ")
            .replace(r"& ", "")
    }
}

#[test]
fn test_type_darwin_is_option() {
    assert!(!TypeDarwin::is_option("Not an Option"));
    assert!(TypeDarwin::is_option(".It Fl Is an Option"));
    assert!(!TypeDarwin::is_option(""));
}

impl TypeDarwin {
    fn is_option(line: &str) -> bool {
        line.starts_with(".It Fl")
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TypeDeroff;

impl ManParser for TypeDeroff {
    fn is_my_type(&self, _manpage: &str) -> bool {
        // TODO Revisit post-MVP
        // I think this is just to account for TypeDeroff being the last ManParser implementation
        // that is checked; it's the fallback.
        true // We're optimists
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let mut deroffer = deroff::Deroffer::new();
        deroffer.deroff(manpage.to_owned());
        let output = deroffer.get_output();
        let lines = output.lines();

        let mut lines = lines
            // Discard lines until we get to DESCRIPTION or OPTIONS
            .skip_while(|line| {
                !(line.starts_with("DESCRIPTION")
                    || line.starts_with("OPTIONS")
                    || line.starts_with("COMMAND OPTIONS"))
            })
            // Look for BUGS and stop there
            .take_while(|line| !line.starts_with("BUGS"))
            .peekable();

        let mut completions = Completions::new(cmdname);
        while let Some(options) = lines.next() {
            // Skip until we get to the next option
            if !TypeDeroff::is_option(options) {
                continue;
            }

            // Pop until we get to either an empty line or a line starting with -
            let description: Vec<_> = lines
                .peeking_take_while(|line| TypeDeroff::could_be_description(line))
                .collect();
            let description = description.join(" ");

            completions.add(&options, &description);
        }
        completions.build()
    }
}

#[test]
fn test_type_deroff_is_option() {
    assert!(!TypeDeroff::is_option("Not an Option"));
    assert!(TypeDeroff::is_option("-Is an Option"));
    assert!(!TypeDeroff::is_option(""));
}

impl TypeDeroff {
    fn is_option(line: &str) -> bool {
        line.starts_with("-")
    }
}

#[test]
fn test_could_be_description() {
    assert!(TypeDeroff::could_be_description("Test Pass Line"));
    assert!(!TypeDeroff::could_be_description("-Test Fail Line"));
    assert!(!TypeDeroff::could_be_description(""));
}

impl TypeDeroff {
    fn could_be_description(line: &str) -> bool {
        line.len() > 0 && !line.starts_with("-")
    }
}

#[test]
fn test_file_is_overwritable() {
    use std::path::Path;
    use tests::FileKind;

    // Setup file paths

    // Good File Paths
    let mut good_string = env::temp_dir();
    good_string.push("test_file_is_overwritable__good.txt");
    let good_path = Path::new(&good_string);

    // Bad File paths
    let mut bad_string = env::temp_dir();
    bad_string.push("test_file_is_overwritable__bad.txt");
    let bad_path = Path::new(&bad_string);

    // Remove any leftover files if the already
    // exist for a previous test failure
    tests::remove_test_file(good_path).ok();
    tests::remove_test_file(bad_path).ok();

    // Create good test file
    tests::create_test_file(good_path, FileKind::Good).ok();

    // Test for IO Error when File doesn't exist
    let result = file_is_overwritable(bad_path);
    assert!(result.is_err());

    // Create bad test file
    tests::create_test_file(bad_path, FileKind::Bad).ok();

    // Tests
    let result = file_is_overwritable(good_path);
    assert_eq!(result, Ok(true));
    let result = file_is_overwritable(bad_path);
    assert_eq!(result, Ok(false));

    // Tear down
    tests::remove_test_file(good_path).ok();
    tests::remove_test_file(bad_path).ok();
}

// Return whether the file at the given path is overwritable
// Raises IOError if it cannot be opened
fn file_is_overwritable(path: &Path) -> Result<bool, String> {
    use bstr::ByteSlice;
    let display = path.display();
    let f = File::open(path).map_err(|error| format!("{:?}", error))?;
    let file = BufReader::new(&f);
    Ok(file
        .split(b'\n')
        .map(|line| {
            // Okay to panic via `expect` here since we've already verified
            // that we can open the file for reading.
            bstr::B(&line.expect(&format!("I/O error encountered reading {}", display)))
                .trim()
                .to_owned()
        })
        .filter(|line| !line.is_empty())
        .take_while(|line| line.starts_with(b"#"))
        .any(|line: Vec<u8>| line.contains_str("Autogenerated")))
}

/// Remove any and all autogenerated completions in the given directory
fn cleanup_autogenerated_completions_in_directory(dir: &Path) -> io::Result<()> {
    Ok(for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.extension().map_or(false, |ext| ext == "fish") {
            cleanup_autogenerated_file(&path);
        }
    })
}

#[test]
fn test_cleanup_autogenerated_file_in_directory() {
    use tests::FileKind;

    // Setup test dir
    let test_dir = env::temp_dir().join("fish-manpage-completions-test");
    fs::create_dir(&test_dir).unwrap();
    let good_path = test_dir.join("good.fish");
    let bad1_path = test_dir.join("bad.fish");
    let bad2_path = test_dir.join("bad.txt");

    // Remove leftovers from previous failed tests
    tests::remove_test_file(&good_path).ok();
    tests::remove_test_file(&bad1_path).ok();
    tests::remove_test_file(&bad2_path).ok();

    // Create files
    tests::create_test_file(&good_path, FileKind::Good).ok();
    tests::create_test_file(&bad1_path, FileKind::Bad).ok();
    tests::create_test_file(&bad2_path, FileKind::Bad).ok();

    // Tests
    assert!(cleanup_autogenerated_completions_in_directory(&test_dir).is_ok());
    assert!(!good_path.exists());
    assert!(bad1_path.exists());
    assert!(bad2_path.exists());

    // Tear down
    tests::remove_test_file(&good_path).ok();
    tests::remove_test_file(&bad1_path).ok();
    tests::remove_test_file(&bad2_path).ok();
    fs::remove_dir(&test_dir).unwrap();

    // Fail if dir not exist
    assert!(cleanup_autogenerated_completions_in_directory(&test_dir).is_err());
}

/// Delete the file if it is autogenerated
fn cleanup_autogenerated_file(path: &Path) {
    if file_is_overwritable(path) == Ok(true) {
        // Original proceeds while ignoring errors
        if let Err(err) = std::fs::remove_file(path) {
            eprintln!(
                "Error in cleaning up auto-generated file ({}): {}",
                path.display(),
                err.to_string(),
            );
        }
    }
}

#[test]
fn test_cleanup_autogenerated_file() {
    use std::path::Path;
    use tests::FileKind;

    // Setup file paths
    // Good File Paths
    let mut good_string = env::temp_dir();
    good_string.push("test_cleanup_autogenerated_file__good.txt");
    let good_path = Path::new(&good_string);

    // Bad File paths
    let mut bad_string = env::temp_dir();
    bad_string.push("test_cleanup_autogenerated_file__bad.txt");
    let bad_path = Path::new(&bad_string);

    // Remove any leftover files if the already
    // exist for a previous test failure
    tests::remove_test_file(good_path).ok();
    tests::remove_test_file(bad_path).ok();

    // Create good test file
    tests::create_test_file(good_path, FileKind::Good).ok();

    // Test for IO Error when File doesn't exist
    //let result = cleanup_autogenerated_file(bad_path);
    // assert!(result.is_err());

    // Create bad test file
    tests::create_test_file(bad_path, FileKind::Bad).ok();

    // Tests
    cleanup_autogenerated_file(good_path);
    assert_eq!(good_path.exists(), false);
    cleanup_autogenerated_file(bad_path);
    assert_eq!(bad_path.exists(), true);

    // Tear down
    tests::remove_test_file(good_path).ok();
    tests::remove_test_file(bad_path).ok();
}

fn parse_manpage_at_path(
    manpage_path: &Path,
    output_directory: Option<&Path>,
    deroff_only: bool,
) -> io::Result<bool> {
    // First level span
    let span = tracing::info_span!("Considering", "{}", manpage_path.display());
    let _enter = span.enter();

    // Get the "base" command, e.g. gcc.1.gz -> gcc
    // These casts are safe as OsStr is internally a wrapper around [u8] on all
    // platforms. Taken from libstd.
    let cmdname = manpage_path
        .file_name()
        .and_then(|file| file.as_bytes().splitn(2, |b| *b == b'.').next());
    let cmdname = String::from_utf8_lossy(cmdname.unwrap());
    let ignored_commands = [
        "cc", "g++", "gcc", "c++", "cpp", "emacs", "gprof", "wget", "ld", "awk",
    ];
    if ignored_commands.contains(&cmdname.as_ref()) {
        return Ok(false);
    }

    let mut manpage = String::new();
    let extension = manpage_path.extension().unwrap_or_default();
    let extension = extension.to_string_lossy();
    if extension.as_ref() == "gz" {
        let mut gz = GzDecoder::new(File::open(manpage_path)?);
        gz.read_to_string(&mut manpage)?;
    } else if extension.as_ref() == "bz2" {
        let mut bz = BzDecoder::new(File::open(manpage_path)?);
        bz.read_to_string(&mut manpage)?;
    } else if extension.as_ref() == "xz" || extension.as_ref() == "lzma" {
        let mut xz = XzDecoder::new(File::open(manpage_path)?);
        xz.read_to_string(&mut manpage)?;
    } else if (1..=9).any(|suffix| suffix.to_string() == extension.as_ref()) {
        File::open(manpage_path)?.read_to_string(&mut manpage)?;
    }

    // Ignore perl's gazillion man pages
    let ignored_prefixes = ["perl", "zsh"];
    if ignored_prefixes
        .iter()
        .any(|prefix| cmdname.starts_with(prefix))
    {
        return Ok(false);
    }

    // Ignore the millions of links to BUILTIN(1)
    if manpage.contains("BUILTIN 1") || manpage.contains("builtin.1") {
        return Ok(false);
    }

    let parsers = if deroff_only {
        &[ManType::TypeDeroff(TypeDeroff)]
    } else {
        ManType::ALL
    };
    let parsers = parsers.iter().filter(|parser| parser.is_my_type(&manpage));
    let mut parsers = parsers.peekable();

    if parsers.peek().is_none() {
        tracing::info!("{}: Not supported", manpage_path.display());
    }

    if let Some(mut completions) = parsers.find_map(|parser| {
        // Second (last) level span
        let span = tracing::info_span!("Trying", "{}", parser);
        let _enter = span.enter();
        parser.parse_man_page(&manpage, &cmdname)
    }) {
        let comments = format!(
            "# {}\n# Autogenerated from man page {}\n",
            &cmdname,
            manpage_path.display()
        );
        completions.insert_str(0, &comments);

        if let Some(output_directory) = output_directory {
            let fullpath = output_directory
                .join(cmdname.as_ref())
                .with_extension("fish");
            match File::create(&fullpath) {
                Ok(mut file) => file.write_all(completions.as_bytes())?,
                Err(err) => {
                    tracing::info!("Unable to open file '{}': {}", &fullpath.display(), err);
                    return Err(err);
                }
            }
        } else {
            io::stdout().lock().write_all(completions.as_bytes())?;
        }
        tracing::info!("{} parsed successfully", manpage_path.display());
        Ok(true)
    } else {
        let parser_names = parsers.join(", ");
        tracing::warn!(
            "{} contains no options or is unparsable (tried parser {})",
            manpage_path.display(),
            parser_names
        );
        Ok(false)
    }
}

/// Get the number of digits in num
// fn num_digits(n: usize) -> usize {
//     (1.max(n) as f32).log10() as usize + 1
// }

// #[test]
// fn test_num_digits() {
//     assert_eq!(num_digits(1000), 4);
//     assert_eq!(num_digits(100), 3);
//     assert_eq!(num_digits(33), 2);
//     assert_eq!(num_digits(123456789012345), 15);
//     assert_eq!(num_digits(0), 1);
// }

fn parse_and_output_man_pages(
    paths: &mut [PathBuf],
    output_directory: Option<PathBuf>,
    show_progress: bool,
    deroff_only: bool,
) {
    paths.sort();

    // let max_digits = num_digits(paths.len());
    // let (tx, rx) = mpsc::channel::<PathBuf>();

    // // XXX: maybe we can do custom drawing logic or paint ourselves?
    // if let Some(output_directory) = output_directory.as_ref() {
    //     if show_progress {
    //         println!(
    //             "Parsing man pages and writing completions to {}",
    //             output_directory.display()
    //         );

    //         // draw in another thread
    //         thread::spawn(move || {
    //             let mut index = 1;
    //             while let Ok(manpage_path) = rx.recv() {
    //                 // foo/bar/gcc.1.gz -> gcc.1.gz
    //                 let man_file_name = manpage_path
    //                     .file_name()
    //                     .map(|fname| fname.to_string_lossy())
    //                     .unwrap_or_else(|| {
    //                         panic!(
    //                             "Failed to get manfile name from {:?}",
    //                             manpage_path.display()
    //                         )
    //                     });

    //                 let progress = format!(
    //                     "{0:>1$} / {2} : {3}",
    //                     index, max_digits, total, man_file_name,
    //                 );

    //                 let stdout = std::io::stdout();
    //                 let mut lock = stdout.lock();
    //                 lock.write_all(format!("\r\x1b[K{}", progress).as_bytes())
    //                     .expect("Failed to write to stdout");
    //                 lock.flush().expect("Failed to flush stdout");
    //                 index += 1;
    //             }
    //         });
    //     }
    // }

    // let paths_iter = paths.par_iter().map_with(tx, |tx, manpage_path| {
    let paths_iter = paths.par_iter().map(|manpage_path| {
        // We know the lifetime will always be the same as another
        // painting thread but how to not clone this?
        // tx.send(manpage_path.to_owned()).unwrap();
        match parse_manpage_at_path(&manpage_path, output_directory.as_deref(), deroff_only) {
            Ok(true) => 1,
            Ok(false) => 0,
            Err(_) => {
                tracing::info!("Cannot open {}", manpage_path.display());
                0
            }
        }
    });
    let successful_count: u64 = if show_progress {
        paths_iter.progress().sum() // may not be accurate since it paints after
    } else {
        paths_iter.sum()
    };

    tracing::info!(
        "successfully parsed {} / {} pages",
        successful_count,
        paths.len()
    );
}

macro_rules! mantypes {
    ($($typ: tt),*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum ManType {
            $($typ($typ),)*
        }

        impl ManType {
            const ALL: &'static [ManType] = &[$(ManType::$typ($typ),)*];
        }

        $(
        impl From<$typ> for ManType {
            fn from(t: $typ) -> Self {
                ManType::$typ(t)
            }
        }
        )*

        impl fmt::Display for ManType {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", match self {
                    $(ManType::$typ($typ) => stringify!($typ),)*
                })
            }
        }

        impl ManParser for ManType {
            fn is_my_type(&self, manpage: &str) -> bool {
                match self {$(
                    ManType::$typ(t) => t.is_my_type(manpage),
                )*}
            }

            fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
                match self {$(
                    ManType::$typ(t) => t.parse_man_page(manpage, cmdname),
                )*}
            }
        }
    };
}

mantypes![Type1, Type2, Type3, Type4, TypeScdoc, TypeDarwin, TypeDeroff];

/// Return all the paths to man(1) and man(8) files in the manpath.
fn get_paths_from_man_locations() -> Vec<PathBuf> {
    // $MANPATH take precedence, just like with `man` on the CLI.
    let mut parent_paths: Vec<_> = if let Ok(output) = Command::new("manpath").output() {
        let output = String::from_utf8(output.stdout).unwrap();
        env::split_paths(&output.trim()).collect()
    } else if let Ok(output) = Command::new("man").arg("--path").output() {
        let output = String::from_utf8(output.stdout).unwrap();
        env::split_paths(&output.trim()).collect()
    } else if let Some(paths) = env::var_os("MANPATH") {
        env::split_paths(&paths).collect()
    // HACK: Use some fallbacks in case we can't get anything else.
    // `mandoc` does not provide `manpath` or `man --path` and $MANPATH might not be set.
    // The alternative is reading its config file (/etc/man.conf)
    } else if let Ok(file) = File::open("/etc/man.conf") {
        let lines_iter = BufReader::new(file).lines().map(|line| line.unwrap());
        lines_iter
            .filter(|line| line.starts_with("manpath") || line.starts_with("MANPATH"))
            .filter_map(|line| line.split_ascii_whitespace().nth(2).map(PathBuf::from))
            .collect()
    } else {
        Default::default()
    };
    if parent_paths.is_empty() {
        eprintln!("Unable to get the manpath, falling back to /usr/share/man:/usr/local/share/man. Please set $MANPATH if that is not correct.");
        parent_paths.push(PathBuf::from("/usr/share/man"));
        parent_paths.push(PathBuf::from("/usr/local/share/man"));
    }
    let mut paths = Vec::new();
    for parent_path in parent_paths {
        for section in &["man1", "man6", "man8"] {
            let section_path = parent_path.join(section);
            if let Ok(dir) = fs::read_dir(&section_path) {
                for entry in dir {
                    paths.push(section_path.join(entry.unwrap().path()));
                }
            }
        }
    }
    paths
}

mod deroff;

/// Generate fish completions from manpages.
#[derive(StructOpt, Debug)]
struct Opts {
    /// Level of debug output.
    #[structopt(short, long, default_value = "0", possible_values = &["0", "1", "2"])]
    verbose: u8,
    /// Write the completions to stdout.
    #[structopt(short, long, conflicts_with = "directory")]
    stdout: bool,
    /// Use deroff parser only.
    #[structopt(short = "z", long)]
    deroff_only: bool,
    /// Directory to save the completions in.
    #[structopt(short, long)]
    directory: Option<PathBuf>,
    /// Use manpath from system and environment variable.
    #[structopt(short, long)]
    manpath: bool,
    /// Show progress bar.
    #[structopt(short, long)]
    progress: bool,
    /// Directory to clean up.
    #[structopt(short, long)]
    cleanup_in: Option<PathBuf>,
    /// Keep files in target directory.
    #[structopt(short, long)]
    keep: bool,
    /// Generate fish completions.
    // TODO generate this in build.rs and remove this option
    #[structopt(long)]
    completions: bool,
    /// Files to parse and generate completions.
    files: Vec<PathBuf>,
}

fn shell() -> String {
    std::path::Path::new(
        &std::env::var_os("SHELL").expect("No valid SHELL environment variable found"),
    )
    .file_stem()
    .expect("SHELL environment variable did not appear to be a path to a shell program")
    .to_str()
    .expect("SHELL environment contained invalid Unicode characters")
    .into()
}

fn program_name() -> String {
    std::env::current_exe()
        .unwrap()
        .file_stem()
        .map(|stem| stem.to_string_lossy().into_owned())
        .expect("No extractable program name.")
}

fn main() -> Result<(), String> {
    let opts = Opts::from_args();

    let level = match opts.verbose {
        0 => LevelFilter::OFF,
        1 => LevelFilter::WARN,
        2 => LevelFilter::INFO,
        n => unreachable!(n),
    };
    tracing_subscriber::fmt()
        .with_writer(io::stderr)
        .with_max_level(level)
        .init();

    if opts.completions {
        Opts::clap().gen_completions_to(
            program_name(),
            shell().parse().unwrap(),
            &mut std::io::stdout(),
        );
        return Ok(());
    }

    if let Some(cleanup_dir) = opts.cleanup_in.as_ref() {
        cleanup_autogenerated_completions_in_directory(cleanup_dir).ok();
    }

    let mut paths = opts.files.clone();
    if opts.manpath {
        paths.extend(get_paths_from_man_locations());
    }

    if paths.is_empty() {
        println!("No paths specified");
        return Ok(());
    }

    let output_directory = opts.directory.clone().or_else(|| {
        if opts.stdout {
            None
        } else {
            let mut xdg_data_home = dirs::data_dir().unwrap();
            xdg_data_home.push("fish/generated_completions/");
            if !xdg_data_home.is_dir() {
                std::fs::create_dir_all(&xdg_data_home).expect("Failed to create directory");
            }
            Some(xdg_data_home)
        }
    });

    if let Some(output_directory) = output_directory.as_ref() {
        if !opts.keep {
            cleanup_autogenerated_completions_in_directory(output_directory).ok();
        }
    }

    parse_and_output_man_pages(
        &mut paths,
        output_directory,
        opts.progress,
        opts.deroff_only,
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Copy, Clone, Debug)]
    pub enum FileKind {
        Good,
        Bad,
    }

    impl FileKind {
        fn content(self) -> &'static [u8] {
            match self {
                FileKind::Good => {
                    b"    \n#Hello, world!\
                    \n#Hello, world! Autogenerated      "
                }
                FileKind::Bad => b"    Autogenerated     ",
            }
        }
    }

    pub fn create_test_file(path: &Path, kind: FileKind) -> std::io::Result<()> {
        use std::io::prelude::*;
        let mut file = File::create(path)?;
        file.write_all(kind.content())?;
        Ok(())
    }

    pub fn remove_test_file(path: &Path) -> std::io::Result<()> {
        fs::remove_file(path)?;
        Ok(())
    }
}
