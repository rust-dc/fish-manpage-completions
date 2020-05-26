/// A translation of https://github.com/fish-shell/fish-shell/blob/e7bfd1d71ca54df726a4f1ea14bd6b0957b75752/share/tools/create_manpage_completions.py
use std::collections::{HashMap, HashSet};
use std::env;
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{BufRead, BufReader, Read, Write};
use std::iter::Peekable;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::slice::Iter;

use itertools::Itertools;
use regex::Regex;
use structopt::StructOpt;

#[cfg(test)]
use pretty_assertions::{assert_eq, assert_ne};

mod util;

// # -*- coding: utf-8 -*-
//
// # Run me like this: ./create_manpage_completions.py /usr/share/man/man{1,8}/* > man_completions.fish
//
// """
// <OWNER> = Siteshwar Vashisht
// <YEAR> = 2012
//
// Copyright (c) 2012, Siteshwar Vashisht
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// """
//
// import string, sys, re, os.path, bz2, gzip, traceback, getopt, errno, codecs
// from deroff import Deroffer
//
// lzma_available = True
// try:
//     try:
//         import lzma
//     except ImportError:
//         from backports import lzma
// except ImportError:
//     lzma_available = False
//
// # Whether we're Python 3
// IS_PY3 = sys.version_info[0] >= 3
//
// # This gets set to the name of the command that we are currently executing
// CMDNAME = ""
//
// # Information used to track which of our parsers were successful
// PARSER_INFO = {}
//
// # built_command writes into this global variable, yuck
// built_command_output = []
//
// # Diagnostic output
// diagnostic_output = []
// diagnostic_indent = 0
//
// # Three diagnostic verbosity levels
// VERY_VERBOSE, BRIEF_VERBOSE, NOT_VERBOSE = 2, 1, 0
//
// # Pick some reasonable default values for settings
// global VERBOSITY, WRITE_TO_STDOUT, DEROFF_ONLY
// VERBOSITY, WRITE_TO_STDOUT, DEROFF_ONLY = NOT_VERBOSE, False, False
//

macro_rules! regex {
    ($pattern: expr) => {{
        lazy_static::lazy_static! {
            static ref REGEX: regex::Regex = regex::Regex::new($pattern).unwrap();
        }
        &REGEX
    }};
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Verbosity {
    Not,
    Brief,
    Very,
}

impl Default for Verbosity {
    fn default() -> Self {
        Verbosity::Not
    }
}

#[derive(Default)]
struct App {
    verbosity: Verbosity,
    diagnostic_output: String,
    diagnostic_indent: usize,
    already_output_completions: HashMap<String, HashSet<String>>,
}

// def add_diagnostic(dgn, msg_verbosity = VERY_VERBOSE):
//     # Add a diagnostic message, if msg_verbosity <= VERBOSITY
//     if msg_verbosity <= VERBOSITY:
//         diagnostic_output.append('   '*diagnostic_indent + dgn)

const DIAGNOSTIC_INDENTER: &'static str = "   ";

impl App {
    fn add_diagnostic(&mut self, diagnostic: &str, msg_verbosity: impl Into<Option<Verbosity>>) {
        let msg_verbosity = msg_verbosity.into().unwrap_or_default();

        if msg_verbosity <= self.verbosity {
            for _ in 0..self.diagnostic_indent {
                self.diagnostic_output.push_str(DIAGNOSTIC_INDENTER);
            }
            self.diagnostic_output.push_str(diagnostic);
        }
    }
}

// def flush_diagnostics(where):
//     if diagnostic_output:
//         output_str = '\n'.join(diagnostic_output) + '\n'
//         where.write(output_str)
//         diagnostic_output[:] = []

impl App {
    fn flush_diagnostics<T>(&mut self, r#where: &mut T)
    where
        T: Write,
    {
        if !self.diagnostic_output.is_empty() {
            r#where
                .write_all(self.diagnostic_output.as_bytes())
                .unwrap();
        }
        self.diagnostic_output.clear();
    }
}

// # Make sure we don't output the same completion multiple times, which can happen
// # For example, xsubpp.1.gz and xsubpp5.10.1.gz
// # This maps commands to lists of completions
// already_output_completions = {}
//
// def compile_and_search(regex, input):
//     options_section_regex = re.compile(regex , re.DOTALL)
//     options_section_matched = re.search( options_section_regex, input)
//     return options_section_matched
//

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

fn fish_escape_single_quote(string: &str) -> String {
    format!("'{}'", string.replace(r"\", r"\\").replace(r"'", r"\'"))
}

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

fn lossy_unicode(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).to_string()
}

#[test]
fn test_lossy_unicode() {
    let bytes = b"123 456";
    let s = lossy_unicode(bytes);
    assert_eq!("123 456", s);

    let bad_bytes = &[255];
    let bad_s = lossy_unicode(bad_bytes);
    assert_eq!("�", bad_s);
}

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
    {
        let expected_output: Vec<String> = vec!["-s 'f'".into(), "-l 'force'".into()];
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
    }

    {
        use std::iter::once;

        let expected_output: Vec<String> = vec!["-l 'force'".into()];
        let options = "-f, --force[=false]";
        let mut existing_options: HashSet<String> = Default::default();
        existing_options.insert("-s 'f'".into());
        existing_options.insert("-l 'something'".into());
        assert_eq!(
            fish_options(options, &mut existing_options),
            expected_output
        );
        assert_eq!(
            expected_output
                .iter()
                .cloned()
                .chain(once("-s 'f'".to_string()))
                .chain(once("-l 'something'".to_string()))
                .collect::<HashSet<_>>(),
            existing_options,
        );
    }
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

    let mut out = format!(
        "{}.",
        lossy_unicode(&sentences.next().unwrap_or_default().as_bytes())
    );
    let mut out_len = char_len(&out);

    if out_len > MAX_DESCRIPTION_WIDTH {
        out = char_truncate_string(&out, MAX_DESCRIPTION_WIDTH, TRUNCATION_SUFFIX).into_owned();
    } else {
        for line in sentences {
            let line = lossy_unicode(&line.as_bytes());
            out_len += 1 // space
                + char_len(&line)
                + 1 // period
                ;
            if out_len > MAX_DESCRIPTION_WIDTH {
                break;
            }
            out = format!("{} {}.", out, line);
        }
    }

    fish_escape_single_quote(&out)
}

#[test]
fn test_truncated_description() {
    assert_eq!(truncated_description(r"\'\."), r"'\'.'");

    assert_eq!(
        truncated_description(r"Don't use this command."),
        r"'Don\'t use this command.'"
    );

    assert_eq!(
        truncated_description(r"Don't use this command. It's really dumb."),
        r"'Don\'t use this command.  It\'s really dumb.'"
    );

    assert_eq!(
        truncated_description(
            r"The description for the command is so long. This second sentence will be dropped, in fact, because it is too long to be displayed comfortably."
        ),
        r"'The description for the command is so long.'"
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
        r"'     Dumb command.    \\It\'s really dumb.   Extra spaces aren\'t removed.'"
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

    // TODO naming and type
    fn build(self) -> String {
        self.built_command_output.join("\n")
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
    // TODO revisit this later
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
    let re =
        regex!(r"\\fI|\\fP|\\f1|\\fB|\\fR|\\e|\.BI|\.BR|0\.5i|\.rb|\\\^|\{ | \}|\.B|\.I|.PD( \d+)");
    re.replace_all(&data, "")
}

#[test]
fn test_remove_groff_formatting() {
    assert_eq!(
        remove_groff_formatting(r#"Foo\fIbar\fP Zoom.PD 325 Zoom"#),
        "Foobar Zoom Zoom"
    );
}

trait ManParser {
    fn is_my_type(&self, manpage: &str) -> bool;

    // TODO Is this the right type signature?
    fn parse_man_page(&self, _manpage: &str, _cmdname: &str) -> Option<String> {
        None
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type1;

impl ManParser for Type1 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(r#".SH "OPTIONS""#)
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r#"\.SH "OPTIONS"((?s:.)*?)(\.SH|\\Z)"#);
        let options_section_matched = options_section_re.find(manpage);
        let mut options_section = options_section_matched.unwrap().as_str();

        let options_parts_re = regex!(r"\.PP((?s:.)*?)\.RE");
        let mut options_matched = options_parts_re.captures(options_section);
        // add_diagnostic(format!("Command is {}", cmdname));

        if options_matched.is_none() {
            // add_diagnostic("Unable to find options");
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
                    // add_diagnostic(format!("{:?} doesn't contain '-'", option_name));
                }
            } else {
                // add_diagnostic("Unable to split option from description");
                return None;
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }
        Some(completions.build())
    }
}

impl Type1 {
    fn fallback(&self, mut options_section: &str, cmdname: &str) -> Option<String> {
        // add_diagnostic("Trying fallback");
        let options_parts_re = regex!(r"\.TP( \d+)?((?s:.)*?)\.TP");
        let mut options_matched = options_parts_re.captures(options_section);
        if options_matched.is_none() {
            // add_diagnostic("Still not found");
            return None;
        }
        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(2).unwrap().as_str();
            let data = remove_groff_formatting(data);
            let data = data.splitn(2, '\n').next_tuple::<(_, _)>();
            if data.filter(|data| !data.1.trim().is_empty()).is_none() {
                // add_diagnostic("Unable to split option from description");
                return None;
            }
            let option_name = data.unwrap().0.trim();
            if option_name.contains('-') {
                let option_name = unquote_double_quotes(option_name);
                let option_name = unquote_single_quotes(option_name);
                let option_desc = data.unwrap().1.trim().replace('\n', " ");
                completions.add(option_name, &option_desc);
            } else {
                // add_diagnostic(format!("{:?} does not contain '-'", option_name));
            }
            // XXX possible to add fallback2 here

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }

        Some(completions.build())
    }

    fn fallback2(&self, options_section: &str, cmdname: &str) -> Option<String> {
        // add_diagnostic("Trying last chance fallback");
        let ix_remover_re = regex!(r"\.IX.*");
        let trailing_num_re = regex!(r"\d+$");
        let options_parts_re = regex!(r"\.IP ((?s:.)*?)\.IP");

        let mut options_section = &*ix_remover_re.replace_all(options_section, "");
        let mut options_matched = options_parts_re.captures(&options_section);
        if options_matched.is_none() {
            // add_diagnostic("Still (still!) not found");
            return None;
        }
        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(1).unwrap().as_str();
            let data = remove_groff_formatting(data);
            let data: Vec<&str> = data.splitn(2, '\n').collect();
            if data.len() < 2 || data[1].trim().is_empty() {
                // add_diagnostic("Unable to split option from description");
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
                // add_diagnostic(format!("{:?} doesn't contain '-'", option_name));
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(&options_section);
        }
        Some(completions.build())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type2;

impl ManParser for Type2 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH OPTIONS")
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r#"\.SH OPTIONS((?s:.)*?)(\.SH|\Z)"#);
        let options_section_matched = options_section_re.captures(manpage);
        let mut options_section = options_section_matched.unwrap().get(1).unwrap().as_str();

        let options_parts_re = regex!(r#"\.[IT]P( \d+(\.\d)?i?)?((?s:.)*?)\.([IT]P|UNINDENT)"#);
        let mut options_matched = options_parts_re.captures(options_section);
        // add_diagnostic(format!("Command is {}", cmdname));

        if options_matched.is_none() {
            // add_diagnostic("Unable to find options");
            return None;
        }

        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let data = mat.get(3).unwrap().as_str();
            let data = remove_groff_formatting(data);
            let data = data.splitn(2, '\n').next_tuple::<(_, _)>();
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
                    // add_diagnostic(format!("{:?} doesn't contain '-'", option_name));
                }
            } else {
                // add_diagnostic("Unable to split option from description");
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }
        Some(completions.build())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type3;

impl ManParser for Type3 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH DESCRIPTION")
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r"\.SH DESCRIPTION((?s:.)*?)(\.SH|\\Z)");
        let options_section_matched = options_section_re.find(manpage);
        let mut options_section = options_section_matched.unwrap().as_str();

        let options_parts_re = regex!(r"\.TP((?s:.)*?)\.TP");
        let mut options_matched = options_parts_re.captures(options_section);
        // add_diagnostic(format!("Command is {}", cmdname));

        if options_matched.is_none() {
            // add_diagnostic("Unable to find options section");
            return None;
        }

        let mut completions = Completions::new(cmdname);
        while let Some(mat) = options_matched {
            let mut data = mat.get(1).unwrap().as_str();

            let data = remove_groff_formatting(data);
            let data = data.trim();
            let (option_name, option_desc) = match data.splitn(2, '\n').next_tuple() {
                Some(tuple) => tuple,
                None => {
                    // add_diagnostic("Unable to split option from description");
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
                // add_diagnostic(format!("{:?} doesn't contain '-'", option_name));
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(&options_section);
        }
        Some(completions.build())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type4;

impl ManParser for Type4 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH FUNCTION LETTERS")
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let options_section_re = regex!(r"\.SH FUNCTION LETTERS((?s:.)*?)(\.SH|\\Z)");
        let options_section_matched = options_section_re.captures(manpage);
        let mut options_section = options_section_matched.unwrap().get(1).unwrap().as_str();

        let options_parts_re = regex!(r"\.TP((?s:.)*?)\.TP");
        let mut options_matched = options_parts_re.captures(options_section);
        // add_diagnostic(format!("Command is {}", cmdname));

        if options_matched.is_none() {
            // add_diagnostic("Unable to find options section");
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
                    // add_diagnostic(format!("{} doesn't contain '-'", option_name));
                }
            } else {
                // add_diagnostic("Unable to split option from description");
                return None;
            }

            options_section = &options_section[mat.get(0).unwrap().end() - 3..];
            options_matched = options_parts_re.captures(options_section);
        }

        Some(completions.build())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TypeDarwin;

impl ManParser for TypeDarwin {
    fn is_my_type(&self, manpage: &str) -> bool {
        regex!(r##"\.S[hH] DESCRIPTION"##).is_match(manpage)
    }

    fn parse_man_page(&self, manpage: &str, cmdname: &str) -> Option<String> {
        let mut got_something = false;
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
            let mut desc_lines = Vec::new();
            while let Some(line) = lines.next() {
                if !Self::is_option(line) {
                    break;
                }
                // Ignore comments
                if line.starts_with(".\"") {
                    continue;
                } else if line.starts_with(".") {
                    let line = Self::groff_replace_escapes(line);
                    let line = Self::trim_groff(&line);
                    if !line.is_empty() {
                        desc_lines.push(line);
                    }
                }
            }
            let desc = desc_lines.join(" ");

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

        Some(completions.build())
    }
}

#[test]
fn test_TypeDarwin_trim_groff() {
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
fn test_TypeDarwin_count_argument_dashes() {
    assert_eq!(TypeDarwin::count_argument_dashes(".Fl Fl xx"), 1);
    assert_eq!(TypeDarwin::count_argument_dashes(".xxxFl Fl "), 2);
    assert_eq!(TypeDarwin::count_argument_dashes(".xxxFl FL "), 1);
    assert_eq!(TypeDarwin::count_argument_dashes("Fl Fl Fl "), 0);
    assert_eq!(TypeDarwin::count_argument_dashes(".Fl "), 0);
}

impl TypeDarwin {
    fn count_argument_dashes(line: &str) -> u32 {
        let (string, result) = replace_all(&line);
        result
    }
}

#[test]
fn test_TypeDarwin_groff_replace_escapes() {
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
fn test_TypeDarwin_is_option() {
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
    fn is_my_type(&self, manpage: &str) -> bool {
        // TODO Revisit post-MVP
        // I think this is just to account for TypeDeroff being the last ManParser implementation
        // that is checked; it's the fallback.
        true
    }

    fn parse_man_page(&self, manpage: &str, _cmdname: &str) -> Option<String> {
        unimplemented!();
    }
}

#[test]
fn test_TypeDeroff_is_option() {
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

// class TypeDeroffManParser(ManParser):
//     def parse_man_page(self, manpage):
//         d = Deroffer()
//         d.deroff(manpage)
//         output = d.get_output()
//         lines = output.split('\n')
//
//         got_something = False
//
//         # Discard lines until we get to DESCRIPTION or OPTIONS
//         while lines and not (lines[0].startswith('DESCRIPTION') or lines[0].startswith('OPTIONS') or lines[0].startswith('COMMAND OPTIONS')):
//             lines.pop(0)
//
//         # Look for BUGS and stop there
//         for idx in range(len(lines)):
//             line = lines[idx]
//             if line.startswith('BUGS'):
//                 # Drop remaining elements
//                 lines[idx:] = []
//                 break
//
//         while lines:
//             # Pop until we get to the next option
//             while lines and not self.is_option(lines[0]):
//                 line = lines.pop(0)
//
//             if not lines:
//                 continue
//
//             options = lines.pop(0)
//
//             # Pop until we get to either an empty line or a line starting with -
//             description = ''
//             while lines and self.could_be_description(lines[0]):
//                 if description: description += ' '
//                 description += lines.pop(0)
//
//             built_command(options, description)
//             got_something = True
//
//         return got_something

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
    tests::remove_test_file(good_path);
    tests::remove_test_file(bad_path);

    // Create good test file
    tests::create_test_file(good_path, FileKind::Good);

    // Test for IO Error when File doesn't exist
    let result = file_is_overwritable(bad_path);
    assert!(result.is_err());

    // Create bad test file
    tests::create_test_file(bad_path, FileKind::Bad);

    // Tests
    let result = file_is_overwritable(good_path);
    assert_eq!(result, Ok(true));
    let result = file_is_overwritable(bad_path);
    assert_eq!(result, Ok(false));

    // Tear down
    tests::remove_test_file(good_path);
    tests::remove_test_file(bad_path);
}

// Return whether the file at the given path is overwritable
// Raises IOError if it cannot be opened
fn file_is_overwritable(path: &Path) -> Result<bool, String> {
    use std::error::Error;
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
        .take_while(|line| line.starts_with("#"))
        .any(|line| line.contains("Autogenerated")))
}

// # Remove any and all autogenerated completions in the given directory
// def cleanup_autogenerated_completions_in_directory(dir):
//     try:
//         for filename in os.listdir(dir):
//             # Skip non .fish files
//             if not filename.endswith('.fish'): continue
//             path = os.path.join(dir, filename)
//             cleanup_autogenerated_file(path)
//     except OSError as err:
//         return False
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
    tests::remove_test_file(good_path);
    tests::remove_test_file(bad_path);

    // Create good test file
    tests::create_test_file(good_path, FileKind::Good);

    // Test for IO Error when File doesn't exist
    //let result = cleanup_autogenerated_file(bad_path);
    // assert!(result.is_err());

    // Create bad test file
    tests::create_test_file(bad_path, FileKind::Bad);

    // Tests
    cleanup_autogenerated_file(good_path);
    assert_eq!(good_path.exists(), false);
    cleanup_autogenerated_file(bad_path);
    assert_eq!(bad_path.exists(), true);

    // Tear down
    tests::remove_test_file(good_path);
    tests::remove_test_file(bad_path);
}

fn cleanup_autogenerated_file(path: &Path) {
    use std::error::Error;

    if file_is_overwritable(path) == Ok(true) {
        // Original proceeds while ignoring errors
        if let Err(err) = std::fs::remove_file(path) {
            eprintln!(
                "Error in cleaning up auto-generated file ({}): {}",
                path.to_string_lossy(),
                err.description(),
            );
        }
    }
}

// def parse_manpage_at_path(manpage_path, output_directory):
//     filename = os.path.basename(manpage_path)
//
//     # Clear diagnostics
//     global diagnostic_indent
//     diagnostic_output[:] = []
//     diagnostic_indent = 0
//
//     # Set up some diagnostics
//     add_diagnostic('Considering ' + manpage_path)
//     diagnostic_indent += 1
//
//     if manpage_path.endswith('.gz'):
//         fd = gzip.open(manpage_path, 'r')
//         manpage = fd.read()
//         if IS_PY3: manpage = manpage.decode('latin-1')
//     elif manpage_path.endswith('.bz2'):
//         fd = bz2.BZ2File(manpage_path, 'r')
//         manpage = fd.read()
//         if IS_PY3: manpage = manpage.decode('latin-1')
//     elif manpage_path.endswith('.xz') or manpage_path.endswith('.lzma'):
//         if not lzma_available:
//             return
//         fd = lzma.LZMAFile(str(manpage_path), 'r')
//         manpage = fd.read()
//         if IS_PY3: manpage = manpage.decode('latin-1')
//     elif manpage_path.endswith((".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9")):
//         if IS_PY3:
//             fd = open(manpage_path, 'r', encoding='latin-1')
//         else:
//             fd = open(manpage_path, 'r')
//         manpage = fd.read()
//     else:
//         return
//     fd.close()
//
//     manpage = str(manpage)
//
//     # Get the "base" command, e.g. gcc.1.gz -> gcc
//     cmd_base = CMDNAME.split('.', 1)[0]
//     ignoredcommands = ["cc", "g++", "gcc", "c++", "cpp", "emacs", "gprof", "wget", "ld", "awk"]
//     if cmd_base in ignoredcommands:
//         return
//
//     # Ignore perl's gazillion man pages
//     ignored_prefixes = ['perl', 'zsh']
//     for prefix in ignored_prefixes:
//         if cmd_base.startswith(prefix):
//             return
//
//     # Ignore the millions of links to BUILTIN(1)
//     if 'BUILTIN 1' in manpage or 'builtin.1' in manpage:
//         return
//
//     # Clear the output list
//     built_command_output[:] = []
//
//     if DEROFF_ONLY:
//         parsers = [TypeDeroffManParser()]
//     else:
//         parsers = [Type1ManParser(), Type2ManParser(), Type4ManParser(), Type3ManParser(), TypeDarwinManParser(), TypeDeroffManParser()]
//     parsersToTry = [p for p in parsers if p.is_my_type(manpage)]
//
//     success = False
//     if not parsersToTry:
//         add_diagnostic(manpage_path + ": Not supported")
//     else:
//         for parser in parsersToTry:
//             add_diagnostic('Trying %s' % parser.__class__.__name__)
//             diagnostic_indent += 1
//             success = parser.parse_man_page(manpage)
//             diagnostic_indent -= 1
//             # Make sure empty files aren't reported as success
//             if not built_command_output:
//                 success = False
//             if success:
//                 PARSER_INFO.setdefault(parser.__class__.__name__, []).append(CMDNAME)
//                 break
//
//         if success:
//             if WRITE_TO_STDOUT:
//                 output_file = sys.stdout
//             else:
//                 fullpath = os.path.join(output_directory, CMDNAME + '.fish')
//                 try:
//                     output_file = codecs.open(fullpath, "w", encoding="utf-8")
//                 except IOError as err:
//                     add_diagnostic("Unable to open file '%s': error(%d): %s" % (fullpath, err.errno, err.strerror))
//                     return False
//
//             built_command_output.insert(0, "# " + CMDNAME)
//
//             # Output the magic word Autogenerated so we can tell if we can overwrite this
//             built_command_output.insert(1, "# Autogenerated from man page " + manpage_path)
//             # built_command_output.insert(2, "# using " + parser.__class__.__name__) # XXX MISATTRIBUTES THE CULPABILE PARSER! Was really using Type2 but reporting TypeDeroffManParser
//
//             for line in built_command_output:
//                 output_file.write(line)
//                 output_file.write('\n')
//             output_file.write('\n')
//             add_diagnostic(manpage_path + ' parsed successfully')
//             if output_file != sys.stdout:
//                 output_file.close()
//         else:
//             parser_names =  ', '.join(p.__class__.__name__ for p in parsersToTry)
//             #add_diagnostic('%s contains no options or is unparsable' % manpage_path, BRIEF_VERBOSE)
//             add_diagnostic('%s contains no options or is unparsable (tried parser %s)' % (manpage_path, parser_names), BRIEF_VERBOSE)
//
//     return success

// def parse_and_output_man_pages(paths, output_directory, show_progress):
//     global diagnostic_indent, CMDNAME
//     paths.sort()
//     total_count = len(paths)
//     successful_count, index = 0, 0
//     padding_len = len(str(total_count))
//     last_progress_string_length = 0
//     if show_progress and not WRITE_TO_STDOUT:
//         print("Parsing man pages and writing completions to {0}".format(output_directory))
//
//     man_page_suffixes = set([os.path.splitext(m)[1][1:] for m in paths])
//     lzma_xz_occurs = "xz" in man_page_suffixes or "lzma" in man_page_suffixes
//     if lzma_xz_occurs and not lzma_available:
//         add_diagnostic('At least one man page is compressed with lzma or xz, but the "lzma" module is not available.'
//                        ' Any man page compressed with either will be skipped.',
//                        NOT_VERBOSE)
//         flush_diagnostics(sys.stderr)
//
//     for manpage_path in paths:
//         index += 1
//
//         # Get the "base" command, e.g. gcc.1.gz -> gcc
//         man_file_name = os.path.basename(manpage_path)
//         CMDNAME = man_file_name.split('.', 1)[0]
//         output_file_name = CMDNAME + '.fish'
//
//         # Show progress if we're doing that
//         if show_progress:
//             progress_str = '  {0} / {1} : {2}'.format((str(index).rjust(padding_len)), total_count, man_file_name)
//             # Pad on the right with spaces so we overwrite whatever we wrote last time
//             padded_progress_str = progress_str.ljust(last_progress_string_length)
//             last_progress_string_length = len(progress_str)
//             sys.stdout.write("\r{0}\r".format(padded_progress_str))
//             sys.stdout.flush()
//
//         # Maybe we want to skip this item
//         skip = False
//         if not WRITE_TO_STDOUT:
//             # Compute the path that we would write to
//             output_path = os.path.join(output_directory, output_file_name)
//
//         # Now skip if requested
//         if skip:
//             continue
//
//         try:
//             if parse_manpage_at_path(manpage_path, output_directory):
//                 successful_count += 1
//         except IOError:
//             diagnostic_indent = 0
//             add_diagnostic('Cannot open ' + manpage_path)
//         except (KeyboardInterrupt, SystemExit):
//             raise
//         except:
//             add_diagnostic('Error parsing %s: %s' % (manpage_path, sys.exc_info()[0]), BRIEF_VERBOSE)
//             flush_diagnostics(sys.stderr)
//             traceback.print_exc(file=sys.stderr)
//         flush_diagnostics(sys.stderr)
//     print("") #Newline after loop
//     add_diagnostic("Successfully parsed %d / %d pages" % (successful_count, total_count), BRIEF_VERBOSE)
//     flush_diagnostics(sys.stderr)

#[derive(Copy, Clone, Debug)]
struct Progress(pub bool);

// TODO Arg/output types?
fn parse_and_output_man_pages(
    _paths: impl Iterator<Item = PathBuf>,
    _output_directory: PathBuf,
    Progress(_show_progress): Progress,
) {
    unimplemented!();
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

        impl ManParser for ManType {
            fn is_my_type(&self, manpage: &str) -> bool {
                match self {$(
                    ManType::$typ(t) => t.is_my_type(manpage),
                )*}
            }
        }
    };
}

mantypes![Type1, Type2, Type3, Type4, TypeDarwin, TypeDeroff];

impl App {
    // TODO Result type
    // This function might be useable as a helper function for parse_manpage_at_path
    fn single_man_page<R: Read, W: Write>(&mut self, input: &mut R, output: &mut W, cmdname: &str) {
        let mut buf = vec![];
        input.read_to_end(&mut buf).unwrap();
        dbg!(buf.len());
        // TODO Either use lossy conversion or do something sensible with the Err
        let buf = String::from_utf8(buf).unwrap();
        // TODO mimic multiple parser logic with lazy evaluation
        let parsers = ManType::ALL.iter().filter(|parser| parser.is_my_type(&buf));
        let mut parsers = parsers.peekable();
        if parsers.peek().is_none() {
            self.add_diagnostic(&format!("{}: Not supported", cmdname), None);
        }
        if let Some(completions) = parsers.find_map(|parser| parser.parse_man_page(&buf, cmdname)) {
            output.write_all(completions.as_bytes()).unwrap();
        }
    }
}

/// Return all the paths to man(1) and man(8) files in the manpath.
fn get_paths_from_man_locations() -> Vec<PathBuf> {
    // $MANPATH take precedence, just like with `man` on the CLI.
    let mut parent_paths: Vec<_> = if let Some(paths) = env::var_os("MANPATH") {
        env::split_paths(&paths).collect()
    } else if let Ok(output) = Command::new("manpath").output() {
        let output = String::from_utf8(output.stdout).unwrap();
        env::split_paths(&output).collect()
    } else if let Ok(output) = Command::new("man").arg("--path").output() {
        let output = String::from_utf8(output.stdout).unwrap();
        env::split_paths(&output).collect()
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

#[derive(StructOpt, Debug)]
struct Opts {
    files: Vec<PathBuf>,
    #[structopt(short, long, default_value = "0")]
    verbose: u8,
    /// Print to stdout rather than to a directory of completions
    #[structopt(short, long)]
    stdout: bool,
    #[structopt(short, long)]
    directory: Option<PathBuf>,
    #[structopt(short, long)]
    manpath: Option<PathBuf>,
    #[structopt(short, long)]
    progress: bool,
    #[structopt(short, long)]
    completions: bool,
}

impl Opts {
    fn read_from_stdin(&self) -> bool {
        self.manpath.is_none() && self.files.is_empty()
    }
}

// def usage(script_name):
//     print("Usage: {0} [-v, --verbose] [-s, --stdout] [-d, --directory] [-p, --progress] files...".format(script_name))
//     print("""Command options are:
//      -h, --help\t\tShow this help message
//      -v, --verbose [0, 1, 2]\tShow debugging output to stderr. Larger is more verbose.
//      -s, --stdout\tWrite all completions to stdout (trumps the --directory option)
//      -d, --directory [dir]\tWrite all completions to the given directory, instead of to ~/.local/share/fish/generated_completions
//      -m, --manpath\tProcess all man1 and man8 files available in the manpath (as determined by manpath)
//      -p, --progress\tShow progress
//     """)

// if __name__ == "__main__":
//     script_name = sys.argv[0]
//     try:
//         opts, file_paths = getopt.gnu_getopt(sys.argv[1:], 'v:sd:hmpc:z', ['verbose=', 'stdout', 'directory=', 'cleanup-in=', 'help', 'manpath', 'progress'])
//     except getopt.GetoptError as err:
//         print(err.msg) # will print something like "option -a not recognized"
//         usage(script_name)
//         sys.exit(2)
//
//     # Directories within which we will clean up autogenerated completions
//     # This script originally wrote completions into ~/.config/fish/completions
//     # Now it writes them into a separate directory
//     cleanup_directories = []
//
//     use_manpath, show_progress, custom_dir = False, False, False
//     output_directory = ''
//     for opt, value in opts:
//         if opt in ('-v', '--verbose'):
//             VERBOSITY = int(value)
//         elif opt in ('-s', '--stdout'):
//             WRITE_TO_STDOUT = True
//         elif opt in ('-d', '--directory'):
//             output_directory = value
//         elif opt in ('-h', '--help'):
//             usage(script_name)
//             sys.exit(0)
//         elif opt in ('-m', '--manpath'):
//             use_manpath = True
//         elif opt in ('-p', '--progress'):
//             show_progress = True
//         elif opt in ('-c', '--cleanup-in'):
//             cleanup_directories.append(value)
//         elif opt in ('-z',):
//             DEROFF_ONLY = True
//         else:
//             assert False, "unhandled option"
//
//     if use_manpath:
//         # Fetch all man1 and man8 files from the manpath or man.conf
//         file_paths.extend(get_paths_from_man_locations())
//
//     if cleanup_directories:
//         for cleanup_dir in cleanup_directories:
//             cleanup_autogenerated_completions_in_directory(cleanup_dir)
//
//     if not file_paths:
//         print("No paths specified")
//         sys.exit(0)
//
//     if not WRITE_TO_STDOUT and not output_directory:
//         # Default to ~/.local/share/fish/generated_completions/
//         # Create it if it doesn't exist
//         xdg_data_home = os.getenv('XDG_DATA_HOME', '~/.local/share')
//         output_directory = os.path.expanduser(xdg_data_home + '/fish/generated_completions/')
//         try:
//             os.makedirs(output_directory)
//         except OSError as e:
//             if e.errno != errno.EEXIST:
//                 raise
//
//     if not WRITE_TO_STDOUT:
//         # Remove old generated files
//         cleanup_autogenerated_completions_in_directory(output_directory)
//
//     parse_and_output_man_pages(file_paths, output_directory, show_progress)

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

    if opts.completions {
        Opts::clap().gen_completions_to(
            program_name(),
            shell().parse().unwrap(),
            &mut std::io::stdout(),
        );
        return Ok(());
    }

    // TODO Make this less special case-y
    if opts.read_from_stdin() && opts.stdout {
        let mut stdin = std::io::stdin();
        let mut stdout = std::io::stdout();
        let mut app = App::default();
        return Ok(app.single_man_page(&mut stdin, &mut stdout, "STDIN"));
    }

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
        use std::fs::File;
        use std::io::prelude::*;
        let mut file = File::create(path)?;
        file.write_all(kind.content())?;
        Ok(())
    }

    pub fn remove_test_file(path: &Path) -> std::io::Result<()> {
        use std::fs;
        fs::remove_file(path)?;
        Ok(())
    }
}
