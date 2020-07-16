/// A translation of https://github.com/fish-shell/fish-shell/blob/e7bfd1d71ca54df726a4f1ea14bd6b0957b75752/share/tools/deroff.py
/// Deroff, ported from deroff.py, which is ported from the venerable deroff.c
use flate2::read::GzDecoder;
use regex::Regex;

use crate::util::TranslationTable;
use std::borrow::Cow;
use std::cell::Cell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

const SKIP_LISTS: bool = false;
const SKIP_HEADERS: bool = false;

enum TblState {
    Options,
    Format,
    Data,
}

// class Deroffer:
pub struct Deroffer {
    g_re_word: &'static Regex,
    g_re_number: &'static Regex,
    g_re_not_backslash_or_whitespace: &'static Regex,
    g_re_newline_collapse: &'static Regex,
    g_re_font: &'static Regex,

    reg_table: HashMap<String, String>,
    tr_from: String,
    tr_to: String,
    tr: Option<TranslationTable>,
    specletter: bool,
    refer: bool,
    r#macro: u8,
    nobody: bool,
    inlist: bool,
    inheader: bool,
    pic: bool,
    tbl: bool,
    tblstate: TblState,
    tblTab: String,
    eqn: bool,
    output: Cell<String>,
    skipheaders: bool,
    skiplists: bool,
    name: String,

    s: String, // This is not explicitly defined in python code
}

impl Deroffer {
    pub fn new() -> Deroffer {
        Deroffer {
            g_re_word: crate::regex!(r##"[a-zA-Z_]+"##),
            g_re_number: crate::regex!(r##"^[+-]?\d+"##),
            // sequence of not backslash or whitespace
            g_re_not_backslash_or_whitespace: crate::regex!(r##"[^ \t\n\r\f\v\\]+"##),
            g_re_newline_collapse: crate::regex!(r##"\n{3,}"##),
            g_re_font: crate::regex!(
                r##"(?x)\\f(     # Starts with backslash f
                    (\(\S{2})  | # Open paren, then two printable chars
                    (\[\S*?\]) | # Open bracket, zero or more printable characters, then close bracket
                    \S)          # Any printable character
                   "##
            ),

            reg_table: HashMap::new(),
            tr_from: String::new(),
            tr_to: String::new(),
            tr: None,
            specletter: false,
            refer: false,
            r#macro: 0,
            nobody: false,
            inlist: false,
            inheader: false,
            pic: false,
            tbl: false,
            tblstate: TblState::Options,
            tblTab: String::new(),
            eqn: false,
            output: Cell::new(String::new()),
            skipheaders: false,
            skiplists: false,
            name: String::new(),

            s: String::new(), // This is not explicitly defined in python code
        }
    }

    /// Take the output, leaving the the default value.
    pub fn get_output(&self) -> String {
        let output = self.output.take();
        match self.g_re_newline_collapse.replace_all(&output, "\n") {
            Cow::Borrowed(_) => output,
            Cow::Owned(result) => result,
        }
    }

    // for the moment, return small strings, until we figure out what
    // it should really be doing
    fn g_specs_specletter(key: &str) -> Option<&'static str> {
        Some(match key {
            // Output composed latin1 letters
            "-D" => &"Ð",
            "Sd" => &"ð",
            "Tp" => &"þ",
            "TP" => &"Þ",
            "AE" => &"Æ",
            "ae" => &"æ",
            "OE" => &"OE",
            "oe" => &"oe",
            ":a" => &"ä",
            ":A" => &"Ä",
            ":e" => &"ë",
            ":E" => &"Ë",
            ":i" => &"ï",
            ":I" => &"Ï",
            ":o" => &"ö",
            ":O" => &"Ö",
            ":u" => &"ü",
            ":U" => &"Ü",
            ":y" => &"ÿ",
            "ss" => &"ß",
            "'A" => &"Á",
            "'E" => &"É",
            "'I" => &"Í",
            "'O" => &"Ó",
            "'U" => &"Ú",
            "'Y" => &"Ý",
            "'a" => &"á",
            "'e" => &"é",
            "'i" => &"í",
            "'o" => &"ó",
            "'u" => &"ú",
            "'y" => &"ý",
            "^A" => &"Â",
            "^E" => &"Ê",
            "^I" => &"Î",
            "^O" => &"Ô",
            "^U" => &"Û",
            "^a" => &"â",
            "^e" => &"ê",
            "^i" => &"î",
            "^o" => &"ô",
            "^u" => &"û",
            "`A" => &"À",
            "`E" => &"È",
            "`I" => &"Ì",
            "`O" => &"Ò",
            "`U" => &"Ù",
            "`a" => &"à",
            "`e" => &"è",
            "`i" => &"ì",
            "`o" => &"ò",
            "`u" => &"ù",
            "~A" => &"Ã",
            "~N" => &"Ñ",
            "~O" => &"Õ",
            "~a" => &"ã",
            "~n" => &"ñ",
            "~o" => &"õ",
            ",C" => &"Ç",
            ",c" => &"ç",
            "/l" => &"/l",
            "/L" => &"/L",
            "/o" => &"ø",
            "/O" => &"Ø",
            "oA" => &"Å",
            "oa" => &"å",

            // Ligatures
            "fi" => &"fi",
            "ff" => &"ff",
            "fl" => &"fl",
            "Fi" => &"ffi",
            "Ff" => &"fff",
            "Fl" => &"ffl",
            _ => return None,
        })
    }

    // Much like the above, return small strings for now until we know what
    // we might actually want to do
    fn g_specs(key: &str) -> Option<&'static str> {
        Some(match key {
            "mi" => &"-",
            "en" => &"-",
            "hy" => &"-",
            "em" => &"--",
            "lq" => &"“",
            "rq" => &"”",
            "Bq" => &",,",
            "oq" => &"`",
            "cq" => &"'",
            "aq" => &"'",
            "dq" => &"\"",
            "or" => &"|",
            "at" => &"@",
            "sh" => &"#",
            // For the moment, faithfully mimic the behavior of the Python script,
            // even though it might seem that &"€" is a more appropriate result here
            "Eu" => &"¤",
            "eu" => &"¤",
            "Do" => &"$",
            "ct" => &"¢",
            "Fo" => &"«",
            "Fc" => &"»",
            "fo" => &"<",
            "fc" => &">",
            "r!" => &"¡",
            "r?" => &"¿",
            "Of" => &"ª",
            "Om" => &"º",
            "pc" => &"·",
            "S1" => &"¹",
            "S2" => &"²",
            "S3" => &"³",
            "<-" => &"<-",
            "->" => &"->",
            "<>" => &"<->",
            "ua" => &"^",
            "da" => &"v",
            "lA" => &"<=",
            "rA" => &"=>",
            "hA" => &"<=>",
            "uA" => &"^^",
            "dA" => &"vv",
            "ba" => &"|",
            "bb" => &"|",
            "br" => &"|",
            "bv" => &"|",
            "ru" => &"_",
            "ul" => &"_",
            "ci" => &"O",
            "bu" => &"o",
            "co" => &"©",
            "rg" => &"®",
            "tm" => &"(TM)",
            "dd" => &"||",
            "dg" => &"|",
            "ps" => &"¶",
            "sc" => &"§",
            "de" => &"°",
            "%0" => &"0/00",
            "14" => &"¼",
            "12" => &"½",
            "34" => &"¾",
            "f/" => &"/",
            "sl" => &"/",
            "rs" => &"\\",
            "sq" => &"[]",
            "fm" => &"'",
            "ha" => &"^",
            "ti" => &"~",
            "lB" => &"[",
            "rB" => &"]",
            "lC" => &"{",
            "rC" => &"}",
            "la" => &"<",
            "ra" => &">",
            "lh" => &"<=",
            "rh" => &"=>",
            "tf" => &"therefore",
            "~~" => &"~~",
            "~=" => &"~=",
            "!=" => &"!=",
            "**" => &"*",
            "+-" => &"±",
            "<=" => &"<=",
            "==" => &"==",
            "=~" => &"=~",
            ">=" => &">=",
            "AN" => &"\\/",
            "OR" => &"/\\",
            "no" => &"¬",
            "te" => &"there exists",
            "fa" => &"for all",
            "Ah" => &"aleph",
            "Im" => &"imaginary",
            "Re" => &"real",
            "if" => &"infinity",
            "md" => &"·",
            "mo" => &"member of",
            "mu" => &"×",
            "nm" => &"not member of",
            "pl" => &"+",
            "eq" => &"=",
            "pt" => &"oc",
            "pp" => &"perpendicular",
            "sb" => &"(=",
            "sp" => &"=)",
            "ib" => &"(-",
            "ip" => &"-)",
            "ap" => &"~",
            "is" => &"I",
            "sr" => &"root",
            "pd" => &"d",
            "c*" => &"(x)",
            "c+" => &"(+)",
            "ca" => &"cap",
            "cu" => &"U",
            "di" => &"÷",
            "gr" => &"V",
            "es" => &"{}",
            "CR" => &"_|",
            "st" => &"such that",
            "/_" => &"/_",
            "lz" => &"<>",
            "an" => &"-",

            // Output Greek
            "*A" => &"Alpha",
            "*B" => &"Beta",
            "*C" => &"Xi",
            "*D" => &"Delta",
            "*E" => &"Epsilon",
            "*F" => &"Phi",
            "*G" => &"Gamma",
            "*H" => &"Theta",
            "*I" => &"Iota",
            "*K" => &"Kappa",
            "*L" => &"Lambda",
            "*M" => &"Mu",
            "*N" => &"Nu",
            "*O" => &"Omicron",
            "*P" => &"Pi",
            "*Q" => &"Psi",
            "*R" => &"Rho",
            "*S" => &"Sigma",
            "*T" => &"Tau",
            "*U" => &"Upsilon",
            "*W" => &"Omega",
            "*X" => &"Chi",
            "*Y" => &"Eta",
            "*Z" => &"Zeta",
            "*a" => &"alpha",
            "*b" => &"beta",
            "*c" => &"xi",
            "*d" => &"delta",
            "*e" => &"epsilon",
            "*f" => &"phi",
            "+f" => &"phi",
            "*g" => &"gamma",
            "*h" => &"theta",
            "+h" => &"theta",
            "*i" => &"iota",
            "*k" => &"kappa",
            "*l" => &"lambda",
            "*m" => &"µ",
            "*n" => &"nu",
            "*o" => &"omicron",
            "*p" => &"pi",
            "+p" => &"omega",
            "*q" => &"psi",
            "*r" => &"rho",
            "*s" => &"sigma",
            "*t" => &"tau",
            "*u" => &"upsilon",
            "*w" => &"omega",
            "*x" => &"chi",
            "*y" => &"eta",
            "*z" => &"zeta",
            "ts" => &"sigma",
            _ => return None,
        })
    }

    fn skip_char(&mut self, amount: usize) {
        self.s.drain(0..amount);
    }

    fn skip_leading_whitespace(&mut self) {
        self.s = self.s.trim_start().to_owned();
    }

    fn str_at(&self, idx: usize) -> &str {
        let s = &self.s;
        s.char_indices()
            .skip(idx)
            .next()
            .map(|(i, c)| &s[i..(i + c.len_utf8())])
            .unwrap_or_default()
    }

    fn is_white(&self, idx: usize) -> bool {
        match self.str_at(idx) {
            "" => false,
            c => c.chars().all(|c| c.is_whitespace()),
        }
    }

    fn digit(&self, idx: usize) -> bool {
        match self.str_at(idx) {
            "" => false,
            c => c.chars().all(|c| c.is_digit(10)),
        }
    }

    fn comment(&mut self) -> bool {
        let mut s = self.str_at(0);
        while !s.is_empty() && s != "\n" {
            self.skip_char(1);
            s = self.str_at(0);
        }

        true
    }

    fn text_arg(&mut self) -> bool {
        let mut got_something = false;
        loop {
            let possible = self.g_re_not_backslash_or_whitespace.find(&self.s);
            if let Some(m) = possible {
                // Output the characters in the match
                self.condputs(m.as_str());
                self.skip_char(m.end());
                got_something = true;
            }

            if self.s.is_empty() || self.is_white(0) {
                return got_something;
            }

            if !self.esc_char() {
                self.condputs(self.str_at(0));
                self.skip_char(1);
                got_something = true;
            }
        }
    }

    // Replaces the g_macro_dict lookup in the Python code
    fn g_macro_dispatch(&mut self, s: &str) -> bool {
        match s {
            "SH" => self.macro_sh(s),
            "SS" => self.macro_ss_ip(),
            "IP" => self.macro_ss_ip(),
            "H " => self.macro_ss_ip(),
            "I " => self.macro_i_ir(),
            "IR" => self.macro_i_ir(),
            "IB" => self.macro_i_ir(),
            "B " => self.macro_i_ir(),
            "BR" => self.macro_i_ir(),
            "BI" => self.macro_i_ir(),
            "R " => self.macro_i_ir(),
            "RB" => self.macro_i_ir(),
            "RI" => self.macro_i_ir(),
            "AB" => self.macro_i_ir(),
            "Nm" => self.macro_nm(),
            "] " => self.macro_close_bracket(),
            "PS" => self.macro_ps(s),
            "PE" => self.macro_pe(),
            "TS" => self.macro_ts(),
            "T&" => self.macro_t_and(),
            "TE" => self.macro_te(),
            "EQ" => self.macro_eq(),
            "EN" => self.macro_en(),
            "R1" => self.macro_r1(),
            "R2" => self.macro_r2(),
            "de" => self.macro_de(),
            "BL" => self.macro_bl_vl(),
            "VL" => self.macro_bl_vl(),
            "AL" => self.macro_bl_vl(),
            "LB" => self.macro_bl_vl(),
            "RL" => self.macro_bl_vl(),
            "ML" => self.macro_bl_vl(),
            "DL" => self.macro_bl_vl(),
            "BV" => self.macro_bv(),
            "LE" => self.macro_le(),
            "LP" => self.macro_lp_pp(),
            "PP" => self.macro_lp_pp(),
            "P\n" => self.macro_lp_pp(),
            "ds" => self.macro_ds(),
            "so" => self.macro_so_nx(),
            "nx" => self.macro_so_nx(),
            "tr" => self.macro_tr(),
            "sp" => self.macro_sp(),
            _ => self.macro_other(),
        }
    }

    fn macro_sh(&mut self, s: &str) -> bool {
        let headers = [" SYNOPSIS", " \"SYNOPSIS", " ‹BERSICHT", " \"‹BERSICHT"];
        // @TODO: In the future s[2..] should care about UTF-8
        if headers.iter().any(|header| s[2..].starts_with(header)) {
            self.inheader = true;
        } else {
            self.inheader = false;
            self.nobody = true;
        }
        false
    }

    fn macro_ss_ip(&mut self) -> bool {
        self.nobody = true;
        false
    }

    fn macro_i_ir(&mut self) -> bool {
        false
    }

    fn macro_nm(&mut self) -> bool {
        if self.s == "Nm\n" {
            self.condputs(self.name);
        } else {
            self.name = self.s.get(3..).unwrap_or_default().trim().into();
            self.name.push(' ');
        }
        true
    }

    fn macro_close_bracket(&mut self) -> bool {
        self.refer = false;
        false
    }

    fn macro_ps(&mut self, s: &str) -> bool {
        if self.is_white(2) {
            self.pic = true;
        }
        self.condputs("\n");
        true
    }

    fn macro_pe(&mut self) -> bool {
        if self.is_white(2) {
            self.pic = false
        }
        self.condputs("\n");
        true
    }

    fn macro_ts(&mut self) -> bool {
        if self.is_white(2) {
            self.tbl = true;
            self.tblstate = TblState::Options;
        }

        self.condputs("\n");
        true
    }

    fn macro_t_and(&mut self) -> bool {
        if self.is_white(2) {
            self.tbl = true;
            self.tblstate = TblState::Format;
        }

        self.condputs("\n");
        true
    }

    fn macro_te(&mut self) -> bool {
        if self.is_white(2) {
            self.tbl = false
        }

        self.condputs("\n");
        true
    }

    fn macro_eq(&mut self) -> bool {
        if self.is_white(2) {
            self.eqn = true
        }

        self.condputs("\n");
        true
    }

    fn macro_en(&mut self) -> bool {
        if self.is_white(2) {
            self.eqn = false
        }

        self.condputs("\n");
        true
    }

    fn macro_r1(&mut self) -> bool {
        // NOTE: self.refer2 is never used in the python source, so this and macro_r2 are
        // pretty much worthless
        // if self.is_white(2) {
        //     self.refer2 = true;
        // }
        self.condputs("\n");
        true
    }

    fn macro_r2(&mut self) -> bool {
        // if self.is_white(2) {
        //     NOTE: See macro_r1
        //     self.refer2 = false;
        // }
        self.condputs("\n");
        true
    }

    fn macro_de(&mut self) -> bool {
        self.r#macro = 1;
        self.condputs("\n");
        true
    }

    fn macro_bl_vl(&mut self) -> bool {
        if self.is_white(2) {
            self.inlist = true
        }
        self.condputs("\n");
        true
    }

    fn macro_bv(&mut self) -> bool {
        // TODO: Determine whether `self.white` is a bastardization of
        // `self.is_white`. (Was self.white converted to self.is_white
        // but this call site was missed?)
        //
        // If it _were_ a valid function, the original Python source
        // would translate roughly to:
        //
        //     for `self.is_white`, so I don't know what function its supposed to be
        //     if self.str_at(2) == "L" and self.white(self.str_at(3)):
        //         self.inlist = true
        //     }
        self.condputs("\n");
        true
    }

    fn macro_le(&mut self) -> bool {
        if self.is_white(2) {
            self.inlist = false;
        }
        self.condputs("\n");
        true
    }

    fn macro_lp_pp(&mut self) -> bool {
        self.condputs("\n");
        true
    }

    fn macro_ds(&mut self) -> bool {
        // Yuck
        self.skip_char(2);
        self.skip_leading_whitespace();

        if !self.str_at(0).is_empty() {
            let comps: Vec<String> = self.s.splitn(2, " ").map(|s| s.into()).collect();

            if comps.len() == 2 {
                let name: String = comps.get(0).unwrap().into();
                let value = comps.get(1).unwrap().trim_end().into();
                self.reg_table.insert(name, value);
            }
        }

        self.condputs("\n");
        true
    }

    fn macro_so_nx(&mut self) -> bool {
        true
    }

    fn macro_tr(&mut self) -> bool {
        self.skip_char(2);
        self.skip_leading_whitespace();

        while !self.s.is_empty() && &self.s[0..=0] != "\n" {
            self.tr_from.push_str(&self.s[0..=0]);

            let ns = &self.s[1..=1];
            self.tr_to
                .push_str(if ns.is_empty() || ns == "\n" { " " } else { ns });

            self.skip_char(2);
        }

        // Update our table, then swap in the slower tr-savvy condputs
        self.tr = match TranslationTable::new(&self.tr_from, &self.tr_to) {
            Ok(table) => Some(table),
            Err(e) => panic!(
                "Encountered an error creating a new translation table from {}, {}: {}",
                self.tr_from, self.tr_to, e
            ),
        };
        true
    }

    fn macro_sp(&mut self) -> bool {
        self.condputs("\n");
        true
    }

    fn macro_other(&mut self) -> bool {
        self.condputs("\n");
        true
    }

    fn request_or_macro(&mut self) -> bool {
        self.skip_char(1);

        let s0 = self.s.chars().nth(1).unwrap_or('_'); // _ will be ignored by the match

        match s0 {
            '\\' => {
                if self.str_at(1) == "\"" {
                    self.condputs("\n");
                    return true;
                }
            }
            '[' => {
                self.refer = true;
                self.condputs("\n");
                return true;
            }
            ']' => {
                self.refer = false;
                self.skip_char(1);
                return self.text();
            }
            '.' => {
                self.r#macro = 0;
                self.condputs("\n");
                return true;
            }
            _ => (),
        };

        self.nobody = false;
        let s0s1 = self.s.chars().take(2).collect::<String>();

        if self.g_macro_dispatch(&s0s1) {
            return true;
        }

        if SKIP_HEADERS && self.nobody {
            return true;
        }

        self.skip_leading_whitespace();
        while !self.s.is_empty() && !self.is_white(0) {
            self.skip_char(1);
        }

        self.skip_leading_whitespace();

        loop {
            if !self.quoted_arg() && !self.text_arg() {
                if !self.s.is_empty() {
                    self.condputs(self.str_at(0));
                    self.skip_char(1);
                } else {
                    return true;
                }
            }
        }
    }

    fn font(&mut self) -> bool {
        if let Some(m) = self.g_re_font.find(&self.s) {
            self.skip_char(m.end());
            true
        } else {
            false
        }
    }

    fn numreq(&mut self) -> bool {
        // In the python, it has a check that is already handled in esc_char_backslash, which is
        // the only place it gets called, so I'll omit that check here

        // This is written as `self.macro += 1` in the source, but I dont know why
        // it does the same thing (false -> true, true -> still true) :shrug:
        // self.r#macro = true;
        // Upon further investigation, this is the weirdest function ever
        // This is just a state placeholder thing
        if self.str_at(2) != "'" {
            return false;
        }

        self.r#macro += 1;

        self.skip_char(3);

        // This is weird, but it was in the source so it's here now.
        // I think this skips characters until we hit a '
        while self.str_at(0) != "'" && self.esc_char() {}

        if self.str_at(0) == "'" {
            self.skip_char(1);
        }

        self.r#macro -= 1;

        true
    }

    // This function is the worst, there are a few comments explaining some of it in the test (test_var)
    // its so hard to briefly put into words what this function does, basically depending on the state
    // of self.s, it will either, change self.s to "", a part of self.s, or a value in self.reg_table
    // which corresponds to a key that is part of self.s.
    // This should be like 2 or 3 functions, but it's only one. So there's that. :-)
    // NOTE: there is a call to text_arg that is commented out because it's not implemented, so the
    // tests will need revised when it gets implemented
    fn var(&mut self) -> bool {
        let s0s1 = self.s.chars().take(2).collect::<String>();

        if s0s1 == "\\n" {
            if "dy" == self.s.chars().skip(3).take(2).collect::<String>()
                || (self.str_at(2) == "(" && self.not_whitespace(3) && self.not_whitespace(4))
            {
                self.skip_char(5);
                return true;
            } else if self.str_at(2) == "[" && self.not_whitespace(3) {
                self.skip_char(3);
                while !self.str_at(0).is_empty() && self.str_at(0) != "]" {
                    self.skip_char(1);
                }
                return true;
            } else if self.not_whitespace(2) {
                self.skip_char(3);
                return true;
            } else {
                return false;
            }
        } else if s0s1 == "\\*" {
            let mut reg = String::new();
            if self.str_at(2) == "(" && self.not_whitespace(3) && self.not_whitespace(4) {
                reg = self.s[3..5].to_owned();
                self.skip_char(5);
            } else if self.str_at(2) == "[" && self.not_whitespace(3) {
                self.skip_char(3);
                while !self.str_at(0).is_empty() && self.str_at(0) != "]" {
                    reg.push_str(self.str_at(0));
                    self.skip_char(1);
                }
                if let Some(']') = self.s.chars().next() {
                    self.skip_char(1);
                } else {
                    return false;
                }
            } else if self.not_whitespace(2) {
                reg = self.str_at(2).to_owned();
                self.skip_char(3);
            } else {
                return false;
            }

            if self.reg_table.contains_key(&reg) {
                // This unwrap is safe because of the if
                self.s = self.reg_table.get(&reg).unwrap().to_owned();
                self.text_arg();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn size(&mut self) -> bool {
        // We require that the string starts with \s
        if self.digit(2) || ("-+".contains(self.str_at(2)) && self.digit(3)) {
            self.skip_char(3);
            while self.digit(0) {
                self.skip_char(1);
            }
            true
        } else {
            false
        }
    }

    fn esc(&mut self) -> bool {
        // We require that the string start with backslash
        if let Some(c) = self.s.chars().nth(1) {
            match c {
                'e' | 'E' => self.condputs("\\"),
                't' => self.condputs("\t"),
                '0' | '~' => self.condputs(" "),
                '|' | '^' | '&' | ':' => (),
                _ => self.condputs(c.to_string()),
            };
            self.skip_char(2);
            true
        } else {
            false
        }
    }

    fn word(&mut self) -> bool {
        let mut got_something = false;
        while let Some(m) = self.g_re_word.find(&self.s) {
            got_something = true;
            self.condputs(m.as_str());
            self.skip_char(m.end());

            while self.spec() {
                if !self.specletter {
                    break;
                }
            }
        }
        got_something
    }

    fn text(&mut self) -> bool {
        loop {
            if let Some(idx) = self.s.find("\\") {
                self.condputs(&self.s[..idx]);
                self.skip_char(idx);
                if !self.esc_char_backslash() {
                    self.condputs(self.str_at(0));
                    self.skip_char(1);
                }
            } else {
                self.condputs(&self.s);
                self.s = String::new();
                return true;
            }
        }
    }

    fn spec(&self) -> bool {
        unimplemented!()
    }

    fn esc_char_backslash(&mut self) -> bool {
        if let Some(c) = self.s.chars().nth(1) {
            match c {
                '"' => self.comment(),
                'f' => self.font(),
                's' => self.size(),
                'h' | 'v' | 'w' | 'u' | 'd' => self.numreq(),
                'n' | '*' => self.var(),
                '(' => self.spec(),
                _ => self.esc(),
            }
        } else {
            false
        }
    }

    /// AKA `not_whitespace`
    fn not_whitespace(&self, idx: usize) -> bool {
        // # Note that this return False for the empty string (idx >= len(self.s))
        // ch = self.s[idx:idx+1]
        // return ch not in ' \t\n'
        // TODO Investigate checking for ASCII whitespace after mvp
        self.s
            .chars()
            .nth(idx)
            .map(|op| !op.is_whitespace())
            .unwrap_or_default()
    }
    /// `condputs` (cond)itionally (puts) `s` into `self.output`
    /// if `self.tr` is set, instead of putting `s` into `self.output` directly,
    /// it `translate`s it using the set translation table and puts the result
    /// into `self.output`
    fn condputs<S: AsRef<str>>(&self, s: S) {
        let s = s.as_ref();
        let is_special = {
            self.pic || self.eqn || self.refer || self.r#macro != 0 || self.inlist || self.inheader
        };

        if !is_special {
            let mut o = self.output.take();
            if let Some(table) = &self.tr {
                o.push_str(&table.translate(s));
            } else {
                o.push_str(s);
            }
            self.output.set(o);
        }
    }

    pub fn deroff(&mut self, string: String) {
        unimplemented!()
    }

    fn flush_output<W: std::io::Write>(&mut self, mut write: W) {
        write.flush().unwrap()
    }

    fn number(&mut self) -> bool {
        if let Some(mat) = self.g_re_number.find(&self.s) {
            self.condputs(mat.as_str());
            self.skip_char(mat.end());
            true
        } else {
            false
        }
    }

    fn esc_char(&mut self) -> bool {
        if self.s.chars().next() == Some('\\') {
            self.esc_char_backslash()
        } else {
            self.word() || self.number()
        }
    }

    fn quoted_arg(&mut self) -> bool {
        if self.str_at(0) == "\"" {
            // We've now entered a portion of the source that should be
            // surrounded by double quotes. (We've found the first one—really
            // hoping we find its mate later).
            self.skip_char(1);
            while !self.s.is_empty() && self.str_at(0) != "\"" {
                if !self.esc_char() {
                    self.condputs(self.str_at(0));
                    self.skip_char(1);
                }
            }
            // We've run past the end of the string OR we've found the closing
            // double-quote to match the initial one we found at the start of
            // the function.
            true
        } else {
            // We don't start with quotes!
            false
        }
    }

    fn do_line(&self, s: &str) -> bool {
        match s.chars().nth(0) {
            Some('.') | Some('\'') => !request_or_macro(s),
            Some(c) => {
                if self.tbl {
                    do_tbl(s)
                } else {
                    text(s)
                }
            }
            None => panic!("do_line` called with empty string as argument"),
        }
    }
}

#[test]
fn test_comment() {
    let mut deroffer = Deroffer::new();

    deroffer.s = "\n".to_owned();
    deroffer.comment();
    assert_eq!(deroffer.s, "\n".to_owned());

    deroffer.s = "hello\n".to_owned();
    deroffer.comment();
    assert_eq!(deroffer.s, "\n".to_owned());

    deroffer.s = "hello\nworld".to_owned();
    deroffer.comment();
    assert_eq!(deroffer.s, "\nworld".to_owned());
}

fn deroff_files(files: &[String]) -> std::io::Result<()> {
    for arg in files {
        eprintln!("processing deroff file: {}", arg);

        let mut file = File::open(arg)?;
        let mut string = String::new();
        if arg.ends_with(".gz") {
            let mut decoder = GzDecoder::new(file);
            decoder.read_to_string(&mut string)?;
        } else {
            file.read_to_string(&mut string)?;
        }
        let mut d = Deroffer::new();
        println!("string: {}", string);

        d.deroff(string);
        d.flush_output(std::io::stdout());
    }
    Ok(())
}

#[test]
fn test_get_output() {
    let deroffer = Deroffer::new();
    deroffer.output.set("foo\n\nbar".to_string());
    assert_eq!(&deroffer.get_output(), "foo\n\nbar");
    deroffer.output.set("foo\n\n\nbar".to_string());
    assert_eq!(&deroffer.get_output(), "foo\nbar");
}

#[test]
fn test_not_whitespace() {
    let mut deroffer = Deroffer::new();

    deroffer.s = "".to_owned();
    assert_eq!(deroffer.not_whitespace(0), false);
    assert_eq!(deroffer.not_whitespace(9), false);

    deroffer.s = "ab d".to_owned();
    // idx 2 = " ", should be false
    assert_eq!(deroffer.not_whitespace(2), false);
    assert_eq!(deroffer.not_whitespace(3), true);
}

#[test]
fn test_str_at() {
    let mut deroffer = Deroffer::new();

    assert_eq!(deroffer.str_at(1), "");

    deroffer.s = "ab cd".to_owned();
    assert_eq!(deroffer.str_at(42), "");
    assert_eq!(deroffer.str_at(1), "b");

    deroffer.s = "🗻".to_owned();
    assert_eq!(deroffer.str_at(0), "🗻");
    assert_eq!(deroffer.str_at(1), "");
}

#[test]
fn test_is_white() {
    let mut deroffer = Deroffer::new();

    assert_eq!(deroffer.is_white(1), false);

    deroffer.s = "ab cd".to_owned();
    assert_eq!(deroffer.is_white(42), false);
    assert_eq!(deroffer.is_white(1), false);
    assert_eq!(deroffer.is_white(2), true);
    assert_eq!(deroffer.is_white(3), false);
}

#[test]
fn test_var() {
    let mut d = Deroffer::new();

    // "\n" successes
    d.s = "\\n dyHello".to_owned();
    assert_eq!(d.var(), true);
    assert_eq!(d.s, "Hello");

    d.s = "\\n(aaHello".to_owned();
    assert_eq!(d.var(), true);
    assert_eq!(d.s, "Hello");

    d.s = "\\n[skipme] Hello".to_owned();
    assert_eq!(d.var(), true);
    assert_eq!(d.s, "] Hello");

    d.s = "\\naHello".to_owned();
    assert_eq!(d.var(), true);
    assert_eq!(d.s, "Hello");

    // "\n" errors
    d.s = "\\n".to_owned();
    assert_eq!(d.var(), false);
    assert_eq!(d.s, "\\n");

    d.s = "\\n a".to_owned();
    assert_eq!(d.var(), false);
    assert_eq!(d.s, "\\n a");

    d.s = "\\n da".to_owned();
    assert_eq!(d.var(), false);
    assert_eq!(d.s, "\\n da");

    // "\*" successes

    d.s = "\\*(traaaaaaaaaaaaa".to_owned();
    d.reg_table
        .insert("tr".to_owned(), "Hello World!".to_owned());
    assert_eq!(d.var(), true);
    assert_eq!(d.s, " World!");
    let o = d.output.take();
    assert!(o.contains("Hello"));
    d.output.set(o);

    d.s = "\\*(aaHello World!".to_owned();
    assert_eq!(d.var(), false);
    assert_eq!(d.s, "Hello World!");

    // ideal case, B is in reg_table
    d.s = "\\*[test_reg]".to_owned();
    d.reg_table
        .insert("test_reg".to_owned(), "It me!".to_owned());
    assert_eq!(d.var(), true);
    assert_eq!(d.s, " me!");
    let o = d.output.take();
    assert!(o.contains("It"));
    d.output.set(o);

    // no "]"
    d.s = "\\*[foo bar :)".to_owned();
    assert_eq!(d.var(), false);
    assert_eq!(d.s, "");

    // B not in reg_table
    d.s = "\\*[foo bar]abcd".to_owned();
    assert_eq!(d.var(), false);
    assert_eq!(d.s, "abcd");
}

fn test_condputs() {
    let mut d = Deroffer::new();

    let o = d.output.take();
    assert_eq!(o, String::new());
    d.output.set(o);

    d.condputs("Hello World!\n");

    let o = d.output.take();
    assert_eq!(o, "Hello World!\n".to_owned());
    d.output.set(o);

    d.pic = true;
    d.condputs("This won't go to output");

    let o = d.output.take();
    assert_eq!(o, "Hello World!\n".to_owned());
    d.output.set(o);

    d.pic = false;
    d.condputs("This will go to output :)");
    let o = d.output.take();
    assert_eq!(o, "Hello World!\nThis will go to output :)".to_owned());
    d.output.set(o);

    // Test the translation check
    d.tr = TranslationTable::new("Ttr", "AAA").ok();
    d.condputs("Translate test");

    let o = d.output.take();
    assert_eq!(
        o,
        "Hello World!\nThis will go to output :)AAanslaAe AesA".to_owned()
    );
    d.output.set(o);
}

#[test]
fn test_digit() {
    let mut deroffer = Deroffer::new();

    deroffer.s = "0".to_owned();
    assert_eq!(deroffer.digit(0), true);

    deroffer.s = "9".to_owned();
    assert_eq!(deroffer.digit(0), true);

    deroffer.s = "".to_owned();
    assert_eq!(deroffer.digit(1), false);

    deroffer.s = "1".to_owned();
    assert_eq!(deroffer.digit(1), false);

    deroffer.s = "a".to_owned();
    assert_eq!(deroffer.digit(0), false);

    deroffer.s = " ".to_owned();
    assert_eq!(deroffer.digit(0), false);
}

#[test]
fn test_skip_char() {
    let mut d = Deroffer::new();
    d.s = String::from("      Hello         World");
    d.skip_char(6);
    assert_eq!(&d.s, "Hello         World");
    d.skip_char(5);
    assert_eq!(&d.s, "         World");
    d.skip_char(9);
    assert_eq!(&d.s, "World");
    d.skip_char(5);
    assert_eq!(&d.s, "");
}

#[test]
fn test_skip_leading_whitespace() {
    let mut d = Deroffer::new();
    d.s = String::from("          Hello        World");
    d.skip_leading_whitespace();
    assert_eq!(&d.s, "Hello        World");
    d.skip_char(5);
    assert_eq!(&d.s, "        World");
    d.skip_leading_whitespace();
    assert_eq!(&d.s, "World");
    d.skip_leading_whitespace();
    assert_eq!(&d.s, "World");
}

#[test]
fn test_number() {
    let mut d = Deroffer::new();

    d.s = String::from("4343xx7");
    assert_eq!(d.number(), true);
    let o = d.output.take();
    assert_eq!(o, "4343".to_string());
    d.output.set(o);

    d.s = String::from("__23");
    assert_eq!(d.number(), false);

    d.s = String::from("-18.5");
    assert_eq!(d.number(), true);
    let o = d.output.take();
    assert_eq!(o, "4343-18".to_string());
    d.output.set(o);

    d.s = String::from("+078t");
    assert_eq!(d.number(), true);
}

//     def spec(self):
//         self.specletter = False
//         if self.s[0:2] == '\\(' and self.not_whitespace(2) and self.not_whitespace(3):
//             key = self.s[2:4]
//             if key in Deroffer.g_specs_specletter:
//                 self.condputs(Deroffer.g_specs_specletter[key])
//                 self.specletter = True
//             elif key in Deroffer.g_specs:
//                 self.condputs(Deroffer.g_specs[key])
//             self.skip_char(4)
//             return True
//         elif self.s.startswith('\\%'):
//             self.specletter = True
//             self.skip_char(2)
//             return True
//         else:
//             return False

fn text(s: &str) -> bool {
    unimplemented!()
}

fn request_or_macro(s: &str) -> bool {
    unimplemented!()
}

fn do_tbl(s: &str) -> bool {
    unimplemented!()
}
//     def do_tbl(self):
//         if self.tblstate == self.OPTIONS:
//             while self.s and self.str_at(0) != ';' and self.str_at(0) != '\n':
//                 self.skip_leading_whitespace()
//                 if not self.str_at(0).isalpha():
//                     # deroff.c has a bug where it can loop forever here...we try to work around it
//                     self.skip_char()
//                 else: # Parse option
//
//                     option = self.s
//                     arg = ''
//
//                     idx = 0
//                     while option[idx:idx+1].isalpha():
//                         idx += 1
//
//                     if option[idx:idx+1] == '(':
//                         option = option[:idx]
//                         self.s = self.s[idx+1:]
//                         arg = self.s
//                     else:
//                         self.s = ''
//
//                     if arg:
//                         idx = arg.find(')')
//                         if idx != -1:
//                             arg = arg[:idx]
//                         self.s = self.s[idx+1:]
//                     else:
//                         #self.skip_char()
//                         pass
//
//                     if option.lower() == 'tab':
//                         self.tblTab = arg[0:1]
//
//             self.tblstate = self.FORMAT
//             self.condputs('\n')
//
//         elif self.tblstate == self.FORMAT:
//             while self.s and self.str_at(0) != '.' and self.str_at(0) != '\n':
//                 self.skip_leading_whitespace()
//                 if self.str_at(0): self.skip_char()
//
//             if self.str_at(0) == '.': self.tblstate = self.DATA
//             self.condputs('\n')
//         elif self.tblstate == self.DATA:
//             if self.tblTab:
//                 self.s = self.s.replace(self.tblTab, '\t')
//             self.text()
//         return True

//     def do_line(self):
//         if self.s[0:1] in ".'":
//             if not self.request_or_macro(): return False
//         elif self.tbl:
//             self.do_tbl()
//         else:
//             self.text()
//         return True

//     def deroff(self, str):
//         lines = str.split('\n')
//         for line in lines:
//             self.s = line + '\n'
//             if not self.do_line():
//                 break

// if __name__ == "__main__":
//     import gzip
//     paths = sys.argv[1:]
//     if True:
//         deroff_files(paths)
//     else:
//         import cProfile, profile, pstats
//         profile.run('deroff_files(paths)', 'fooprof')
//         p = pstats.Stats('fooprof')
//         p.sort_stats('time').print_stats(100)
//         #p.sort_stats('calls').print_callers(.5, 'startswith')
