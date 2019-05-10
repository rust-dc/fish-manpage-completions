/// A translation of https://github.com/fish-shell/fish-shell/blob/e7bfd1d71ca54df726a4f1ea14bd6b0957b75752/share/tools/create_manpage_completions.py
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use itertools::Itertools;
use structopt::StructOpt;

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

fn unquote_double_quotes(data: String) -> String {
    if data.len() > 2 && (data.as_bytes()[0], *data.as_bytes().last().unwrap()) == (b'"', b'"') {
        data[1..(data.len() - 1)].into()
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

fn unquote_single_quotes(data: String) -> String {
    if data.len() > 2 && (data.as_bytes()[0], *data.as_bytes().last().unwrap()) == (b'\'', b'\'') {
        data[1..(data.len() - 1)].into()
    } else {
        data
    }
}

fn fish_escape_single_quote(string: String) -> String {
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

// def output_complete_command(cmdname, args, description, output_list):
//     comps = ['complete -c', cmdname]
//     comps.extend(args)
//     if description:
//         comps.append('--description')
//         comps.append(description)
//     output_list.append(lossy_unicode(' ').join([lossy_unicode(c) for c in comps]))

fn output_complete_command(
    cmdname: &str,
    args: impl Iterator<Item = String>,
    description: &str,
    output_list: &mut Vec<String>,
) {
    output_list.push(complete_command(cmdname, args, description));
}

fn complete_command(
    cmdname: &str,
    mut args: impl Iterator<Item = String>,
    description: &str,
) -> String {
    format!(
        "complete --command {cmdname} {args}{description_flag}{description}",
        cmdname = cmdname,
        args = args.join(" "),
        description_flag = if description.is_empty() {
            ""
        } else {
            " --description "
        },
        description = description,
    )
}

// def built_command(options, description):
// #    print "Options are: ", options
//     man_optionlist = re.split(" |,|\"|=|[|]", options)
//     fish_options = []
//     for optionstr in man_optionlist:
//         option = re.sub(r"(\[.*\])", "", optionstr)
//         option = option.strip(" \t\r\n[](){}.,:!")
//
//
//         # Skip some problematic cases
//         if option in ['-', '--']: continue
//         if any(c in "{}()" for c in option): continue
//
//         if option.startswith('--'):
//             # New style long option (--recursive)
//             fish_options.append('-l ' + fish_escape_single_quote(option[2:]))
//         elif option.startswith('-') and len(option) == 2:
//             # New style short option (-r)
//             fish_options.append('-s ' + fish_escape_single_quote(option[1:]))
//         elif option.startswith('-') and len(option) > 2:
//             # Old style long option (-recursive)
//             fish_options.append('-o ' + fish_escape_single_quote(option[1:]))
//
//     # Determine which options are new (not already in existing_options)
//     # Then add those to the existing options
//     existing_options = already_output_completions.setdefault(CMDNAME, set())
//     fish_options = [opt for opt in fish_options if opt not in existing_options]
//     existing_options.update(fish_options)
//
//     # Maybe it's all for naught
//     if not fish_options: return
//
//     # Here's what we'll use to truncate if necessary
//     max_description_width = 78
//     if IS_PY3:
//         truncation_suffix = '…'
//     else:
//         ELLIPSIS_CODE_POINT = 0x2026
//         truncation_suffix = unichr(ELLIPSIS_CODE_POINT)
//
//     # Try to include as many whole sentences as will fit
//     # Clean up some probably bogus escapes in the process
//     clean_desc = description.replace("\\'", "'").replace("\\.", ".")
//     sentences = clean_desc.split('.')
//
//     # Clean up "sentences" that are just whitespace
//     # But don't let it be empty
//     sentences = [x for x in sentences if x.strip()]
//     if not sentences: sentences = ['']
//
//     udot = lossy_unicode('.')
//     uspace = lossy_unicode(' ')
//
//     truncated_description = lossy_unicode(sentences[0]) + udot
//     for line in sentences[1:]:
//         if not line: continue
//         proposed_description = lossy_unicode(truncated_description) + uspace + lossy_unicode(line) + udot
//         if len(proposed_description) <= max_description_width:
//             # It fits
//             truncated_description = proposed_description
//         else:
//             # No fit
//             break
//
//     # If the first sentence does not fit, truncate if necessary
//     if len(truncated_description) > max_description_width:
//         prefix_len = max_description_width - len(truncation_suffix)
//         truncated_description = truncated_description[:prefix_len] + truncation_suffix
//
//     # Escape some more things
//     truncated_description = fish_escape_single_quote(truncated_description)
//     escaped_cmd = fish_escape_single_quote(CMDNAME)
//
//     output_complete_command(escaped_cmd, fish_options, truncated_description, built_command_output)

// TODO args / arg types?
fn built_command() {
    unimplemented!()
}

macro_rules! regex {
    ($pattern: expr) => {{
        lazy_static::lazy_static! {
            static ref REGEX: regex::Regex = regex::Regex::new($pattern).unwrap();
        }
        &REGEX
    }};
}

fn remove_groff_formatting(data: &str) -> String {
    // TODO Can we remove all of these strings in one go?
    let mut data = data.to_owned();
    for marker in &[
        r"\fI", r"\fP", r"\f1", r"\fB", r"\fR", r"\e", r".BI", r".BR", r"0.5i", r".rb", r"\^",
        r"{ ", r" }", ".B",
        ".I",
        //     The next ones are odd. Putting them into a python file makes my
        //     python linter warn about anomalous backslash and python2 vs python3
        //     seems to make no difference
        //     data = data.replace("\ ","")
        //     data = data.replace("\-","-")
        //"\ ",
        //"\&",
        //"\f",
    ] {
        data = data.replace(marker, "");
    }
    // See note above about anomalous backslash
    //data = data.replace("\-", "-");
    let data = regex!(r##".PD( \d+)"##).replace_all(&data, "");
    data.to_string()
}

#[test]
fn test_remove_groff_formatting() {
    assert_eq!(
        remove_groff_formatting(r#"Foo\fIbar\fP Zoom.PD 325 Zoom"#),
        "Foobar Zoom Zoom"
    );
}

trait ManParser {
    fn is_my_type(&self, manpage: &str) -> bool {
        false
    }

    // TODO Is this the right type signature?
    fn parse_man_page(&mut self, manpage: &str) -> Option<String> {
        None
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type1;

impl ManParser for Type1 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(r#".SH "OPTIONS""#)
    }

    fn parse_man_page(&mut self, manpage: &str) -> Option<String> {
        unimplemented!();
    }
}

// class Type1ManParser(ManParser):
//     def parse_man_page(self, manpage):
//         options_section_regex = re.compile( "\.SH \"OPTIONS\"(.*?)(\.SH|\Z)", re.DOTALL)
//         options_section_matched = re.search( options_section_regex, manpage)
//
//         options_section = options_section_matched.group(0)
//         #   print options_section
//         options_parts_regex = re.compile("\.PP(.*?)\.RE", re.DOTALL)
//         options_matched = re.search(options_parts_regex, options_section)
//         #   print options_matched
//         add_diagnostic("Command is %r" % CMDNAME)
//
//         if options_matched == None:
//             add_diagnostic('Unable to find options')
//             if( self.fallback(options_section) ):
//                 return True
//             elif (self.fallback2(options_section) ):
//                 return True
//             return False
//
//         while (options_matched != None):
//             data = options_matched.group(1)
//             last_dotpp_index = data.rfind(".PP")
//             if (last_dotpp_index != -1):
//                 data = data[last_dotpp_index+3:]
//
//             data = remove_groff_formatting(data)
//             data = data.split(".RS 4")
//             if (len (data) > 1): #and len(data[1]) <= 300):
//                 optionName = data[0].strip()
//
//                 if ( optionName.find("-") == -1):
//                     add_diagnostic("%r doesn't contain '-' " % optionName)
//                 else:
//                     optionName = unquote_double_quotes(optionName)
//                     optionName = unquote_single_quotes(optionName)
//                     optionDescription = data[1].strip().replace("\n"," ")
//                     built_command(optionName, optionDescription)
//
//             else:
//                 add_diagnostic('Unable to split option from description')
//                 return False
//
//             options_section = options_section[options_matched.end()-3:]
//             options_matched = re.search(options_parts_regex, options_section)

//     def fallback(self, options_section):
//         add_diagnostic('Trying fallback')
//         options_parts_regex = re.compile("\.TP( \d+)?(.*?)\.TP", re.DOTALL)
//         options_matched = re.search(options_parts_regex, options_section)
//         if options_matched == None:
//             add_diagnostic('Still not found')
//             return False
//         while options_matched != None:
//             data = options_matched.group(2)
//             data = remove_groff_formatting(data)
//             data = data.strip()
//             data = data.split("\n",1)
//             if (len(data)>1 and len(data[1].strip())>0): # and len(data[1])<400):
//                 optionName = data[0].strip()
//                 if ( optionName.find("-") == -1):
//                     add_diagnostic("%r doesn't contain '-'" % optionName)
//                 else:
//                     optionName = unquote_double_quotes(optionName)
//                     optionName = unquote_single_quotes(optionName)
//                     optionDescription = data[1].strip().replace("\n"," ")
//                     built_command(optionName, optionDescription)
//             else:
//                 add_diagnostic('Unable to split option from description')
//                 return False
//
//             options_section = options_section[options_matched.end()-3:]
//             options_matched = re.search(options_parts_regex, options_section)
//         return True

impl Type1 {
    // TODO Type signature
    fn fallback(&mut self, options_section: ()) {
        unimplemented!()
    }
}

//     def fallback2(self, options_section):
//         add_diagnostic('Trying last chance fallback')
//         ix_remover_regex = re.compile("\.IX.*")
//         trailing_num_regex = re.compile('\\d+$')
//         options_parts_regex = re.compile("\.IP (.*?)\.IP", re.DOTALL)
//
//         options_section = re.sub(ix_remover_regex, "", options_section)
//         options_matched = re.search(options_parts_regex, options_section)
//         if options_matched == None:
//             add_diagnostic('Still (still!) not found')
//             return False
//         while options_matched != None:
//             data = options_matched.group(1)
//
//             data = remove_groff_formatting(data)
//             data = data.strip()
//             data = data.split("\n",1)
//             if (len(data)>1 and len(data[1].strip())>0): # and len(data[1])<400):
//                 optionName = re.sub(trailing_num_regex, "", data[0].strip())
//
//                 if ('-' not in optionName):
//                     add_diagnostic("%r doesn't contain '-'" % optionName)
//                 else:
//                     optionName = optionName.strip()
//                     optionName = unquote_double_quotes(optionName)
//                     optionName = unquote_single_quotes(optionName)
//                     optionDescription = data[1].strip().replace("\n"," ")
//                     built_command(optionName, optionDescription)
//             else:
//                 add_diagnostic('Unable to split option from description')
//                 return False
//
//             options_section = options_section[options_matched.end()-3:]
//             options_matched = re.search(options_parts_regex, options_section)
//         return True

impl Type1 {
    // TODO Type signature
    fn fallback2(&mut self, options_section: ()) {
        unimplemented!()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type2;

impl ManParser for Type2 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH OPTIONS")
    }

    fn parse_man_page(&mut self, manpage: &str) -> Option<String> {
        unimplemented!();
    }
}

// class Type2ManParser(ManParser):
//     def parse_man_page(self, manpage):
//         options_section_regex = re.compile( "\.SH OPTIONS(.*?)(\.SH|\Z)", re.DOTALL)
//         options_section_matched = re.search( options_section_regex, manpage)
//
//         options_section = options_section_matched.group(1)
//
//         options_parts_regex = re.compile("\.[I|T]P( \d+(\.\d)?i?)?(.*?)\.([I|T]P|UNINDENT)", re.DOTALL)
//         options_matched = re.search(options_parts_regex, options_section)
//         add_diagnostic('Command is %r' % CMDNAME)
//
//         if options_matched == None:
//             add_diagnostic("%r: Unable to find options" % self)
//             return False
//
//         while (options_matched != None):
//             data = options_matched.group(3)
//
//             data = remove_groff_formatting(data)
//
//             data = data.strip()
//
//             data = data.split("\n",1)
//             if (len(data)>1 and len(data[1].strip())>0): # and len(data[1])<400):
//                 optionName = data[0].strip()
//                 if '-' not in optionName:
//                     add_diagnostic("%r doesn't contain '-'" % optionName)
//                 else:
//                     optionName = unquote_double_quotes(optionName)
//                     optionName = unquote_single_quotes(optionName)
//                     optionDescription = data[1].strip().replace("\n"," ")
//                     built_command(optionName, optionDescription)
//             else:
//                 add_diagnostic('Unable to split option from description')
//
//             options_section = options_section[options_matched.end()-3:]
//             options_matched = re.search(options_parts_regex, options_section)

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type3;

impl ManParser for Type3 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH DESCRIPTION")
    }

    fn parse_man_page(&mut self, manpage: &str) -> Option<String> {
        unimplemented!();
    }
}

// class Type3ManParser(ManParser):
//     def parse_man_page(self, manpage):
//         options_section_regex = re.compile( "\.SH DESCRIPTION(.*?)(\.SH|\Z)", re.DOTALL)
//         options_section_matched = re.search( options_section_regex, manpage)
//
//         options_section = options_section_matched.group(1)
//         options_parts_regex = re.compile("\.TP(.*?)\.TP", re.DOTALL)
//         options_matched = re.search(options_parts_regex, options_section)
//         add_diagnostic("Command is %r" % CMDNAME)
//
//         if options_matched == None:
//             add_diagnostic('Unable to find options section')
//             return False
//
//         while (options_matched != None):
//             data = options_matched.group(1)
//
//             data = remove_groff_formatting(data)
//             data = data.strip()
//             data = data.split("\n",1)
//
//             if (len(data)>1): # and len(data[1])<400):
//                 optionName = data[0].strip()
//                 if ( optionName.find("-") == -1):
//                     add_diagnostic("%r doesn't contain '-'" % optionName)
//                 else:
//                     optionName = unquote_double_quotes(optionName)
//                     optionName = unquote_single_quotes(optionName)
//                     optionDescription = data[1].strip().replace("\n"," ")
//                     built_command(optionName, optionDescription)
//
//             else:
//                 add_diagnostic('Unable to split option from description')
//                 return False
//
//             options_section = options_section[options_matched.end()-3:]
//             options_matched = re.search(options_parts_regex, options_section)

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Type4;

impl ManParser for Type4 {
    fn is_my_type(&self, manpage: &str) -> bool {
        manpage.contains(".SH FUNCTION LETTERS")
    }

    fn parse_man_page(&mut self, manpage: &str) -> Option<String> {
        unimplemented!();
    }
}

// class Type4ManParser(ManParser):
//     def parse_man_page(self, manpage):
//         options_section_regex = re.compile( "\.SH FUNCTION LETTERS(.*?)(\.SH|\Z)", re.DOTALL)
//         options_section_matched = re.search( options_section_regex, manpage)
//
//         options_section = options_section_matched.group(1)
//         options_parts_regex = re.compile("\.TP(.*?)\.TP", re.DOTALL)
//         options_matched = re.search(options_parts_regex, options_section)
//         add_diagnostic("Command is %r" % CMDNAME)
//
//         if options_matched == None:
//             print >> sys.stderr, "Unable to find options section"
//             return False
//
//         while (options_matched != None):
//             data = options_matched.group(1)
//
//             data = remove_groff_formatting(data)
//             data = data.strip()
//             data = data.split("\n",1)
//
//             if (len(data)>1): # and len(data[1])<400):
//                 optionName = data[0].strip()
//                 if ( optionName.find("-") == -1):
//                     add_diagnostic("%r doesn't contain '-' " % optionName)
//                 else:
//                     optionName = unquote_double_quotes(optionName)
//                     optionName = unquote_single_quotes(optionName)
//                     optionDescription = data[1].strip().replace("\n"," ")
//                     built_command(optionName, optionDescription)
//
//             else:
//                 add_diagnostic('Unable to split option from description')
//                 return False
//
//             options_section = options_section[options_matched.end()-3:]
//             options_matched = re.search(options_parts_regex, options_section)
//
//         return True

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TypeDarwin;

impl ManParser for TypeDarwin {
    fn is_my_type(&self, manpage: &str) -> bool {
        regex!(r##"\.S[hH] DESCRIPTION"##).is_match(manpage)
    }

    fn parse_man_page(&mut self, manpage: &str) -> Option<String> {
        unimplemented!();
    }
}

#[test]
fn test_TypeDarwin_trim_groff() {
    assert_eq!(TypeDarwin::trim_groff(". Test"), " Test");
    assert_eq!(TypeDarwin::trim_groff("..."), "..");
    assert_eq!(TypeDarwin::trim_groff(" Test"), " Test");
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
        regex!(r" ([.,])$").replace(&line, "$1").to_string()
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

//     def parse_man_page(self, manpage):
//         got_something = False
//         lines =  manpage.splitlines()
//         # Discard lines until we get to ".sh Description"
//         while lines and not (lines[0].startswith('.Sh DESCRIPTION') or lines[0].startswith('.SH DESCRIPTION')):
//             lines.pop(0)
//
//         while lines:
//             # Pop until we get to the next option
//             while lines and not self.is_option(lines[0]):
//                 lines.pop(0)
//
//             if not lines:
//                 continue
//
//             # Get the line and clean it up
//             line = lines.pop(0)
//
//             # Try to guess how many dashes this argument has
//             dash_count = self.count_argument_dashes(line)
//
//             line = self.groff_replace_escapes(line)
//             line = self.trim_groff(line)
//             line = line.strip()
//             if not line: continue
//
//             # Extract the name
//             name = line.split(None, 2)[0]
//
//             # Extract the description
//             desc_lines = []
//             while lines and not self.is_option(lines[0]):
//                 line = lossy_unicode(lines.pop(0).strip())
//                 # Ignore comments
//                 if line.startswith(r'.\"'):
//                     continue
//                 if line.startswith('.'):
//                     line = self.groff_replace_escapes(line)
//                     line = self.trim_groff(line).strip()
//                 if line:
//                     desc_lines.append(line)
//             desc = ' '.join(desc_lines)
//
//             if name == '-':
//                 # Skip double -- arguments
//                 continue
//             elif len(name) > 1:
//                 # Output the command
//                 built_command(('-' * dash_count) + name, desc)
//                 got_something = True
//             elif len(name) == 1:
//                 built_command('-' + name, desc)
//                 got_something = True
//
//         return got_something

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TypeDeroff;

impl ManParser for TypeDeroff {
    fn is_my_type(&self, manpage: &str) -> bool {
        // TODO Revisit post-MVP
        // I think this is just to account for TypeDeroff being the last ManParser implementation
        // that is checked; it's the fallback.
        true
    }

    fn parse_man_page(&mut self, manpage: &str) -> Option<String> {
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

// # Return whether the file at the given path is overwritable
// # Raises IOError if it cannot be opened
// def file_is_overwritable(path):
//     result = False
//     file = codecs.open(path, "r", encoding="utf-8")
//     for line in file:
//         # Skip leading empty lines
//         line = line.strip()
//         if not line:
//             continue
//
//         # We look in the initial run of lines that start with #
//         if not line.startswith('#'):
//             break
//
//         # See if this contains the magic word
//         if 'Autogenerated' in line:
//             result = True
//             break
//
//     file.close()
//     return result

fn file_is_overwritable(path: &Path) -> bool {
    unimplemented!()
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

// # Delete the file if it is autogenerated
// def cleanup_autogenerated_file(path):
//     try:
//         if file_is_overwritable(path):
//             os.remove(path)
//     except (OSError, IOError):
//         pass

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

fn parsers_to_try(input: &str) -> Vec<ManType> {
    ManType::ALL
        .iter()
        .filter(|parser| parser.is_my_type(input))
        .cloned()
        .collect()
}

#[test]
fn test_parsers_to_try() {
    assert_eq!(
        parsers_to_try(r###".SH "OPTIONS""###),
        [Type1.into(), TypeDeroff.into()],
    );

    assert_eq!(
        parsers_to_try(r###".SH OPTIONS"###),
        [Type2.into(), TypeDeroff.into()],
    );

    assert_eq!(
        parsers_to_try(".SH OPTIONS\nabc.SH DESCRIPTION"),
        [
            Type2.into(),
            Type3.into(),
            TypeDarwin.into(),
            TypeDeroff.into(),
        ],
    );

    assert_eq!(
        parsers_to_try(".SH OPTIONS\nabc.Sh DESCRIPTION"),
        [Type2.into(), TypeDarwin.into(), TypeDeroff.into()],
    );

    assert_eq!(
        parsers_to_try(".SH FUNCTION LETTERS"),
        [Type4.into(), TypeDeroff.into()],
    );
}

impl App {
    // TODO Result type
    // This function might be useable as a helper function for parse_manpage_at_path
    fn single_man_page<R: Read, W: Write>(
        &mut self,
        input: &mut R,
        output: &mut W,
        input_name: &str,
    ) {
        let mut buf = vec![];
        input.read_to_end(&mut buf).unwrap();
        dbg!(buf.len());
        // TODO Either use lossy conversion or do something sensible with the Err
        let buf = String::from_utf8(buf).unwrap();
        // TODO mimic multiple parser logic
        let mut parsers = parsers_to_try(&buf);
        if parsers.is_empty() {
            self.add_diagnostic(&format!("{}: Not supported", input_name), None);
        }
        for parser in parsers.iter_mut() {
            if let Some(completions) = parser.parse_man_page(&buf) {
                output.write_all(completions.as_bytes()).unwrap();
                return;
            }
        }
    }
}

// def get_paths_from_man_locations():
//     # Return all the paths to man(1) and man(8) files in the manpath
//     import subprocess, os
//     proc = None
//     # $MANPATH takes precedence, just like with `man` on the CLI.
//     if os.getenv("MANPATH"):
//         parent_paths = os.getenv("MANPATH").strip().split(':')
//     else:
//         # Some systems have manpath, others have `man --path` (like Haiku).
//         for prog in [['manpath'], ['man', '--path']]:
//             try:
//                 proc = subprocess.Popen(prog, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
//             except OSError: # Command does not exist, keep trying
//                 continue
//             break # Command exists, use it.
//         manpath, err_data = proc.communicate()
//         parent_paths = manpath.decode().strip().split(':')
//     if (not parent_paths) or (proc and proc.returncode > 0):
//         # HACK: Use some fallbacks in case we can't get anything else.
//         # `mandoc` does not provide `manpath` or `man --path` and $MANPATH might not be set.
//         # The alternative is reading its config file (/etc/man.conf)
//         if os.path.isfile('/etc/man.conf'):
//             data = open('/etc/man.conf', 'r')
//             for line in data:
//                 if ('manpath' in line or 'MANPATH' in line):
//                     p = line.split(' ')[1]
//                     p = p.split()[0]
//                     parent_paths.append(p)
//         if (not parent_paths):
//             sys.stderr.write("Unable to get the manpath, falling back to /usr/share/man:/usr/local/share/man. Please set $MANPATH if that is not correct.\n")
//         parent_paths = ["/usr/share/man", "/usr/local/share/man"]
//     result = []
//     for parent_path in parent_paths:
//         for section in ['man1', 'man6', 'man8']:
//             directory_path = os.path.join(parent_path, section)
//             try:
//                 names = os.listdir(directory_path)
//             except OSError as e:
//                 names = []
//             names.sort()
//             for name in names:
//                 result.append(os.path.join(directory_path, name))
//     return result

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
