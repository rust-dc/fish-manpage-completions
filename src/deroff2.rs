use std::io::{self, BufReader};

const DEFAULT_BUF_CAP: usize = 1024 * (1 << 10); // 1MB

type TODO_TYPE = ();

lazy_static! {
    static G_RE_WORD = crate::regex!(r##"[a-zA-Z_]+"##);
    static G_RE_NUMBER = crate::regex!(r##"[+-]?\d+"##);
    //
    // sequence of not backslash or whitespace
    static G_RE_NOT_BACKSLACE_OR_WHITESPACE = crate::regex!(r##"[^ \t\n\r\f\v\\]+"##);
    static G_RE_NEWLINE_COLLAPSE = crate::regex!(r##"\n{3,}"##);
    static G_RE_FONT = crate::regex!(
        r##"(?x)\\f(     # Starts with backslash f
            (\(\S{2})  | # Open paren, then two printable chars
            (\[\S*?\]) | # Open bracket, zero or more printable characters, then close bracket
            \S)          # Any printable character
           "##);
}

fn is_double_quote(c: char) -> bool {
    c == '"'
}

trait IsControl {
    fn is_control(&self) -> bool;
}

impl<T> IsControl for Option<T> where T: IsControl {
    fn is_control(&self) -> bool {
        match self {
            Some(t) => t.is_control(),
            None => false,
        }
    }
}
impl IsControl for char {
    fn is_control(&self) -> bool {
        (*self as u32 as u8).is_control()
    }
}

impl IsControl for u8 {
    fn is_control(&self) -> bool {
        // 39=' 46=.
        // This jazz because it's faster than `==` or match lookups
        (*self > 38 && *self < 40) || (*self > 45 || *self < 47)
    }
}

// @TODO @perf: Could use something like `lazy_static` here. However, a single
// match lookup is probably faster than a hashmap::get with this small number
// of items.
fn g_specs_specletter(key: &str) -> Option<&'static str> {
    Some(match key {
        // Output composed latin1 letters
        "-D" => "Ð",
        "Sd" => "ð",
        "Tp" => "þ",
        "TP" => "Þ",
        "AE" => "Æ",
        "ae" => "æ",
        "OE" => "OE",
        "oe" => "oe",
        ":a" => "ä",
        ":A" => "Ä",
        ":e" => "ë",
        ":E" => "Ë",
        ":i" => "ï",
        ":I" => "Ï",
        ":o" => "ö",
        ":O" => "Ö",
        ":u" => "ü",
        ":U" => "Ü",
        ":y" => "ÿ",
        "ss" => "ß",
        "'A" => "Á",
        "'E" => "É",
        "'I" => "Í",
        "'O" => "Ó",
        "'U" => "Ú",
        "'Y" => "Ý",
        "'a" => "á",
        "'e" => "é",
        "'i" => "í",
        "'o" => "ó",
        "'u" => "ú",
        "'y" => "ý",
        "^A" => "Â",
        "^E" => "Ê",
        "^I" => "Î",
        "^O" => "Ô",
        "^U" => "Û",
        "^a" => "â",
        "^e" => "ê",
        "^i" => "î",
        "^o" => "ô",
        "^u" => "û",
        "`A" => "À",
        "`E" => "È",
        "`I" => "Ì",
        "`O" => "Ò",
        "`U" => "Ù",
        "`a" => "à",
        "`e" => "è",
        "`i" => "ì",
        "`o" => "ò",
        "`u" => "ù",
        "~A" => "Ã",
        "~N" => "Ñ",
        "~O" => "Õ",
        "~a" => "ã",
        "~n" => "ñ",
        "~o" => "õ",
        ",C" => "Ç",
        ",c" => "ç",
        "/l" => "/l",
        "/L" => "/L",
        "/o" => "ø",
        "/O" => "Ø",
        "oA" => "Å",
        "oa" => "å",

        // Ligatures
        "fi" => "fi",
        "ff" => "ff",
        "fl" => "fl",
        "Fi" => "ffi",
        "Ff" => "fff",
        "Fl" => "ffl",
        _ => return None,
    })
}


// Much like the above, return small strings for now until we know what
// we might actually want to do
// @TODO @perf: Could use something like `lazy_static` here. However, a single
// match lookup is probably faster than a hashmap::get with this small number
// of items.
fn g_specs(key: &str) -> Option<&'static str> {
    Some(match key {
        "mi" => "-",
        "en" => "-",
        "hy" => "-",
        "em" => "--",
        "lq" => "“",
        "rq" => "”",
        "Bq" => ",,",
        "oq" => "`",
        "cq" => "'",
        "aq" => "'",
        "dq" => "\"",
        "or" => "|",
        "at" => "@",
        "sh" => "#",
        // For the moment, faithfully mimic the behavior of the Python script,
        // even though it might seem that &"€" is a more appropriate result here
        "Eu" => "¤",
        "eu" => "¤",
        "Do" => "$",
        "ct" => "¢",
        "Fo" => "«",
        "Fc" => "»",
        "fo" => "<",
        "fc" => ">",
        "r!" => "¡",
        "r?" => "¿",
        "Of" => "ª",
        "Om" => "º",
        "pc" => "·",
        "S1" => "¹",
        "S2" => "²",
        "S3" => "³",
        "<-" => "<-",
        "->" => "->",
        "<>" => "<->",
        "ua" => "^",
        "da" => "v",
        "lA" => "<=",
        "rA" => "=>",
        "hA" => "<=>",
        "uA" => "^^",
        "dA" => "vv",
        "ba" => "|",
        "bb" => "|",
        "br" => "|",
        "bv" => "|",
        "ru" => "_",
        "ul" => "_",
        "ci" => "O",
        "bu" => "o",
        "co" => "©",
        "rg" => "®",
        "tm" => "(TM)",
        "dd" => "||",
        "dg" => "|",
        "ps" => "¶",
        "sc" => "§",
        "de" => "°",
        "%0" => "0/00",
        "14" => "¼",
        "12" => "½",
        "34" => "¾",
        "f/" => "/",
        "sl" => "/",
        "rs" => "\\",
        "sq" => "[]",
        "fm" => "'",
        "ha" => "^",
        "ti" => "~",
        "lB" => "[",
        "rB" => "]",
        "lC" => "{",
        "rC" => "}",
        "la" => "<",
        "ra" => ">",
        "lh" => "<=",
        "rh" => "=>",
        "tf" => "therefore",
        "~~" => "~~",
        "~=" => "~=",
        "!=" => "!=",
        "**" => "*",
        "+-" => "±",
        "<=" => "<=",
        "==" => "==",
        "=~" => "=~",
        ">=" => ">=",
        "AN" => "\\/",
        "OR" => "/\\",
        "no" => "¬",
        "te" => "there exists",
        "fa" => "for all",
        "Ah" => "aleph",
        "Im" => "imaginary",
        "Re" => "real",
        "if" => "infinity",
        "md" => "·",
        "mo" => "member of",
        "mu" => "×",
        "nm" => "not member of",
        "pl" => "+",
        "eq" => "=",
        "pt" => "oc",
        "pp" => "perpendicular",
        "sb" => "(=",
        "sp" => "=)",
        "ib" => "(-",
        "ip" => "-)",
        "ap" => "~",
        "is" => "I",
        "sr" => "root",
        "pd" => "d",
        "c*" => "(x)",
        "c+" => "(+)",
        "ca" => "cap",
        "cu" => "U",
        "di" => "÷",
        "gr" => "V",
        "es" => "{}",
        "CR" => "_|",
        "st" => "such that",
        "/_" => "/_",
        "lz" => "<>",
        "an" => "-",

        // Output Greek
        "*A" => "Alpha",
        "*B" => "Beta",
        "*C" => "Xi",
        "*D" => "Delta",
        "*E" => "Epsilon",
        "*F" => "Phi",
        "*G" => "Gamma",
        "*H" => "Theta",
        "*I" => "Iota",
        "*K" => "Kappa",
        "*L" => "Lambda",
        "*M" => "Mu",
        "*N" => "Nu",
        "*O" => "Omicron",
        "*P" => "Pi",
        "*Q" => "Psi",
        "*R" => "Rho",
        "*S" => "Sigma",
        "*T" => "Tau",
        "*U" => "Upsilon",
        "*W" => "Omega",
        "*X" => "Chi",
        "*Y" => "Eta",
        "*Z" => "Zeta",
        "*a" => "alpha",
        "*b" => "beta",
        "*c" => "xi",
        "*d" => "delta",
        "*e" => "epsilon",
        "*f" => "phi",
        "+f" => "phi",
        "*g" => "gamma",
        "*h" => "theta",
        "+h" => "theta",
        "*i" => "iota",
        "*k" => "kappa",
        "*l" => "lambda",
        "*m" => "µ",
        "*n" => "nu",
        "*o" => "omicron",
        "*p" => "pi",
        "+p" => "omega",
        "*q" => "psi",
        "*r" => "rho",
        "*s" => "sigma",
        "*t" => "tau",
        "*u" => "upsilon",
        "*w" => "omega",
        "*x" => "chi",
        "*y" => "eta",
        "*z" => "zeta",
        "ts" => "sigma",
        _ => return None,
    })
}

struct Line<'a> {
    line: &'a str,
    pos: usize
}

impl<'a> Line<'a> {
    fn new(line: &str) -> Self {
        Line {
            line,
            pos: 0
        }
    }
    /// Return the `char` at absolute index `idx`
    fn abs_char_at(&self, idx: usize) -> Option<char> {
        self.line.chars().nth(0)
    }
    /// Return the `char` at relative index `idx` (relative to current postion)
    fn char_at(&self, idx: usize) -> Option<char> {
        let n = self.pos + idx;
        self.line.chars().nth(n)
    }
    /// Advance the current position
    fn skip_chars(&mut self, n: usize) {
        self.pos += n;
    }
    // @TODO: This is just slice by byte without regards to UTF-8 multi-byte
    /// Return a substring starting at asbolute position `start` and ending at
    /// absolute position `end`
    ///
    /// # Panics
    ///
    /// If `end` is greater than the line length
    fn abs_substr(&self, start: usize, end: usize) -> &str {
        assert!(end < self.line.len());
        &self.line[start:end]
    }
    // @TODO: This is just slice by byte without regards to UTF-8 multi-byte
    /// Return a substring starting at relative (to current position) `start`
    /// and ending at relative (to current position) `end`
    ///
    ///
    /// # Panics
    ///
    /// If `end+current_position` is greater than the line length
    fn rel_substr(&self, start: usize, end: usize) -> &str {
        let rel_start = start + self.pos;
        let rel_end = end + self.pos;
        assert!(rel_end < self.line.len());
        &self.line[rel_start:rel_end]
    }
    /// Returns `true` if we are at the end of the line
    fn is_end(&self) -> bool {
        self.char_at(0).is_none()
    }

    /// Advances current position until a non-whitespace char is at relative
    /// position 0 or the end of the line is reached
    fn skip_leading_whitespace(&mut self) {
        self.skip_while_fn(char::is_whitespace)
    }

    /// Advances the current position until `f` returns `true`
    fn skip_until_fn(&mut self, f: fn(char)->bool) {
        loop {
            match self.char_at(0) {
                Some(c) => if f(c) { break; } else { self.skip_chars(1); }
                None => break;
            }
        }
    }

    /// Advances the current position until `f` returns `false` or the end of
    /// the line is reached
    fn skip_while_fn(&mut self, f: fn(char)->bool) {
        loop {
            match self.char_at(0) {
                Some(c) => if f(c) { self.skip_chars(1); } else { break; }
                None => break;
            }
        }
    }

    /// Returns `true` if the character at position `idx` is whitespace
    fn is_whitespace_at(&self, idx: usize) -> bool {
        self.char_at(idx).is_whitespace()
    }

    /// Returns `true` if the character at position `idx` is numeric
    fn is_numeric_at(&self, idx: usize) -> bool {
        if let Some(c) = self.char_at(idx) {
            return c.is_numeric();
        }
        false
    }
    fn prch(&mut self, idx: usize) -> bool {
        match self.line.char_at(idx) {
            Some(' ') | Some('\t') | Some('\n') => true,
            _ => false
        }
    }
    fn raw(&self) -> &str {
        &self.line[self.pos:]
    }

    fn to_end_from(&self, idx: usize) -> &str {
        let pos = self.pos + idx;
        assert!(pos < self.line.len());
        &self.line[pos:]
    }
}

bitflags! {
    struct DeroffFlags: u16 {
        const SPEC_LETTER = 1;
        const REFER       = 1 << 1;
        const IN_LIST     = 1 << 2;
        const NO_BODY     = 1 << 3;
        const IN_HEADER   = 1 << 4;
        const PIC         = 1 << 5;
        const TBL         = 1 << 6;
        const EQN         = 1 << 7;
        const SKIP_HEADERS= 1 << 8;
        const SKIP_LISTS  = 1 << 9;
        const IGNORE_SONX = 1 << 10;
        const TR          = 1 << 11;
    }
}

pub struct Deroffer<'a> {
    // Mini statemachine for keeping track of what should be done
    flags: DeroffFlags,
    // The line we're operating on currently
    cur_line: Option<&'a str>,
    // A buffer to hold the output
    out_buf: Vec<u8>,
    reg_table: HashMap<TODO_TYPE, TODO_TYPE>,
    tr_from: String,
    tr_to: String,
    tr: String,
    nls: usize,
    r#macro: usize,
    tblstate: usize,
    tbl_tab: String,
    name: String,
    OPTIONS: usize,
    FORMAT: usize,
    DATA: usize,
}

impl Deroffer {
    fn new() -> Self {
        Deroffer {
            flags: DeroffFlags::default(),
            cur_line: None,
            out_buf: Vec::with_capacity(DEFAULT_BUF_CAP),
            reg_table: HashMap::new(),
            tr_from: String::new(),
            tr_to: String::new(),
            tr: String::new(),
            nls: 2,
            r#macro: 0,
            tblstate: 0,
            tbl_tab: String::new(),
            name: String::new(),
            OPTIONS: 0,
            FORMAT: 1,
            DATA: 2,
        }
    }

    /// Clear the Deroffer for re-use
    fn clear(&mut self) {
        self.flags.clear();
        self.line.clear();
        self.out_buf.clear();
    }

    fn macro_dispatch(&self, to_run: &str) -> bool {
        match to_run {
            "SH" => self.macro_sh(),
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
            "Nm" => self.macro_Nm(),
            "] " => self.macro_close_bracket(),
            "PS" => self.macro_ps(),
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
            "P\n"=> self.macro_lp_pp(),
            "ds" => self.macro_ds(),
            "so" => self.macro_so_nx(),
            "nx" => self.macro_so_nx(),
            "tr" => self.macro_tr(),
            "sp" => self.macro_sp(),
            _ => self.macro_other(),
        }
    }

    // Macros Macros
    fn macro_sp(&mut self) -> bool {
        self.condputs_char('\n');
        true
    }
    fn macro_other(&mut self) -> bool {
        self.condputs_char('\n');
        true
    }
    fn macro_ss_ip(&mut self) -> bool {
        self.flags.set(DeroffFlags::NO_BODY);
        false
    }
    fn macro_i_ir(&self) -> bool {
        false
    }
    fn macro_close_bracket(&mut self) -> bool {
        self.flags.unset(DeroffFlags::REFER);
        false
    }
    // PIC start
    fn macro_ps(&mut self) -> bool {
        if self.line.is_white_at(2) {
            self.flags.set(DeroffFlags::PIC);
        }
        self.condputs_char('\n');
        true
    }
    // PIC end
    fn macro_pe(&mut self) -> bool {
        if self.line.is_white_at(2) {
            self.flags.unset(DeroffFlags::PIC);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_ts(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.set(DeroffFlags::TBL);
            self.tbl_state = self.OPTIONS;
        }
        self.condputs_char('\n');
        true
    }
    fn macro_t_and(&self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.set(DeroffFlags::TBL);
            self.tbl_state = self.FORMAT;
        }
        self.condputs_char('\n');
        true
    }
    // TBL end
    fn macro_te(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.unset(DeroffFlags::TBL);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_eqn(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.set(DeroffFlags::EQN);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_en(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.unset(DeroffFlags::EQN);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_r1(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.set(DeroffFlags::REFER2);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_r2(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.unset(DeroffFlags::REFER2);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_de(&mut self) -> bool {
        self.flags.set(DeroffFlags::MACRO);
        self.condputs_char('\n');
        true
    }
    fn macro_r2(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.set(DeroffFlags::IN_LIST);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_le(&mut self) -> bool {
        if self.line.is_whitespace_at(2) {
            self.flags.unset(DeroffFlags::IN_LIST);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_lp_pp(&mut self) -> bool {
        self.condputs_char('\n');
        true
    }
    fn macro_so_nx(&mut self) -> bool {
        // We always ignore include directives
        // deroff.c for some reason allowed this to fall through to the 'tr' case
        // I think that was just a bug so I won't replicate it
        true
    }
    fn macro_Nm(&mut self) -> bool {
        if self.line.raw() == "Nm\n" {
            self.condputs(self.name);
        } else {
            self.line.skip_chars(4);
            self.name = format!("{} ", self.line.raw());
        }
        true
    }
    fn macro_bv(&mut self) -> bool {
        if self.line.char_at(2) == 'L' && self.line.is_whitespace_at(3) {
            self.flags.set(DeroffFlags::IN_LIST);
        }
        self.condputs_char('\n');
        true
    }
    fn macro_sh(&mut self) { // @TODO Should return bool?
        for header in &[" SYNOPSIS", " \"SYNOPSIS", " ‹BERSICHT", " \"‹BERSICHT"] {
            if self.line.to_end_from(2).startswith(header) {
                self.flags.set(DeroffFlags::IN_HEADER);
                break;
            } else {
                self.flags.unset(DeroffFlags::IN_HEADER);
                self.flags.set(DeroffFlags::NO_BODY);
            }
        }
    }

//    def macro_ds(self):
//        self.skip_char(2)
//        self.skip_leading_whitespace()
//        if self.str_at(0):
//            # Split at whitespace
//            comps = self.s.split(None, 2)
//            if len(comps) is 2:
//                name, value = comps
//                value = value.rstrip()
//                self.reg_table[name] = value
//        self.condputs("\n")
//        return True
//
//    def macro_tr(self):
//        self.skip_char(2)
//        self.skip_leading_whitespace()
//        while self.s and self.str_at(0) != "\n":
//            c = self.str_at(0)
//            ns = self.str_at(1)
//            self.skip_char(2)
//            if not ns or ns == "\n":
//                ns = " "
//            self.tr_from += c
//            self.tr_to += ns
//
//        # Update our table, then swap in the slower tr-savvy condputs
//        try:  # Python2
//            self.tr = string.maketrans(self.tr_from, self.tr_to)
//        except AttributeError:  # Python3
//            self.tr = "".maketrans(self.tr_from, self.tr_to)
//        self.condputs = self.condputs_tr
//        return True
//
    pub fn deroff<R>(&mut self, r: R) -> io::Result<()> where R: io::Read {
        let buf_r = BufReader::new(f);

        for line in buf_r.lines() {
            self.cur_line = Line(line);
            if !self.do_line() {
                break;
            }
        }
    }

    fn do_line(&mut self, line: &str) {
        if self.cur_line.abs_char_at(0).is_control() {
            !self.request_or_macro()
        } else if self.flags.is_set(DerofFlags::TBL) {
            self.do_tbl()
        } else {
            self.text()
        }
    }

    fn request_or_macro(&mut self) -> bool {
        // Get over the control char
        self.line.skip_chars(1);
        // @TODO @soundness: Correct? We skip the control char, then blindly
        // skip the next char...i.e.
        // s == '.TR foo' -> skip_char(1) -> s == 'TR foo' -> s0 == 'R'
        let s0 = self.line.char_at(1);
        if s0 == '\\' {
            if self.line.char_at(1) == '"' {
                self.condputs_char('\n');
                return true;
            } else {
                return true;
            }
        } else if s0 == '[' {
            self.flags.set(DeroffFlags::REFER);
            self.condputs_char('\n');
            return true;
        } else if s0 == ']' {
            self.flags.unset(DeroffFlags::REFER);
            self.skip_char(1);
            return self.text();
        } else if s0 == '.' {
            self.flags.set(DeroffFlags::MACRO);
            self.condputs_char('\n');
            return true;
        }

        self.flags.set(DeroffFlags::NO_BODY);
        let s0s1 = self.line.substr(0, 2);
        if self.macro_dispatch(s0s1) {
            return true;
        }
        // @TODO unless macro_dispatch above alters the flags NO_BODY is impossible
        if self.flags.is_set(DeroffFlags::SKIP_HEADERS & DeroffFlags::NO_BODY) {
            return true;
        }
        // @TODO @soundness wut?...skipping a word?
        self.line.skip_leading_whitespace();
        self.line.skip_until_fn(char::is_whitespace);
        self.line.skip_leading_whitespace();
        loop {
            if !self.quoted_arg() && !self.text_arg() {
                // @TODO @perf should be a much more efficient way to do this
                if !self.line.is_end() {
                    self.condputs_char(self.line.char_at(0));
                    self.skip_chars(1);
                } else {
                    return true;
                }
            }
        }
    }

    fn condputs(&mut self, s: &str) {
        let special = DeroffFlags::PIC & DeroffFlags::EQN & DeroffFlags::REFER & DeroffFlags::MACRO;
        let lists = DeroffFlags::SKIP_LISTS & DeroffFlags::IN_LIST;
        let headers = DeroffFlags::SKIP_HEADERS & DeroffFlags::IN_HEADER;
        if !(self.flags.is_set(special) || self.is_set(lists) || self.is_set(headers)) {
            if self.flags.is_set(DeroffFlags::TR) {
                self.out_buf.push_str(s)
            } else {
                self.out_buf.push_str(s)
            }
        }
    }

    fn condputs_char(&mut self, c: char) {
        let special = DeroffFlags::PIC & DeroffFlags::EQN & DeroffFlags::REFER & DeroffFlags::MACRO;
        let lists = DeroffFlags::SKIP_LISTS & DeroffFlags::IN_LIST;
        let headers = DeroffFlags::SKIP_HEADERS & DeroffFlags::IN_HEADER;
        if !(self.flags.is_set(special) || self.is_set(lists) || self.is_set(headers)) {
            if self.flags.is_set(DeroffFlags::TR) {
                self.out_buf.push(c)
            } else {
                self.out_buf.push(c)
            }
        }
    }

    fn quoted_arg(&mut self) -> bool {
        if self.line.char_at(0) == '"' {
            self.skip_chars(1);
            while let Some(c) = self.line.char_at(0) {
                if c == '"' {
                    break;
                }
                if !self.esc_char() {
                    if !self.line.is_end() {
                        // @TODO @perf should be a much more efficient want to
                        // do this
                        self.condputs_char(self.line.char_at(0));
                        self.line.skip_char(1);
                    }
                }
            }
            return true;
        }
        false
    }
    fn size(&mut self) -> bool {
        let c = self.char_at(2)
        if c.is_numeric() || ((c == '-' || c=='+') && self.line.is_numeric_at(3)) {
            self.line.skip_chars(3);
            self.line.skip_while_fn(char::is_numeric)
            return true;
        }
        false
    }
    fn font(&self) -> bool {
        if let Some(m) = G_RE_FONT.find(self.line.raw()) {
            self.line.skip_chars(m.end());
            return true;
        }
        false
    }
    fn esc_char(&self) -> bool {
        if self.line.char_at(0) == '\\' {
            return self.esc_char_backslash();
        }
        return self.word() || self.number();
    }
    fn number(&self) -> bool {
        if let Some(m) = G_RE_NUMBER.find(self.line.raw()) {
            self.condputs(m.as_str());
            self.line.skip_chars(m.end());
            return true;
        }

        false
    }
    fn esc(&self) -> bool {
        match self.line.char_at(1) {
            Some('e') | Some('E') => self.condputs_char('\\'),
            Some('t') => self.condputs_char('\t'),
            Some('0') | Some('~') => self.condputs_char(' '),
            Some('|') | Some('^') | Some('&') | Some(':') => (), // Python has 'pass' (return true?)
            Some(c) => self.condputs_char(c),
            None => return false,
        }
        self.line.skip_chars(2);
        true
    }
    fn esc_char_backslash(&mut self) -> bool {
        match self.line.char_at(1) {
            Some('"') => self.comment(),
            Some('f') => self.font(),
            Some('s') => self.size(),
            Some('h') | Some('v') | Some('w') | Some('u') | Some('d') => self.numreq(),
            Some('n') | Some('*') => self.var(),
            Some('(') => self.spec(),
            _ => self.esc()
        }
    }
    fn numreq(&mut self) -> bool {
        if self.line.char_at(2) == Some('\'') {
            self.r#macro += 1;
            self.line.skip_chars(3);
            while self.line.char_at(0) != Some('\'') && self.esc_char() { }
            if self.line.char_at(0) == Some('\'') {
                self.line.skip_chars(1);
            }
            self.r#macro -= 1;
            return true;
        }
        false
    }
    fn comment(&mut self) -> bool {
        // Here we require that the string start with \"
        self.line.skip_until_fn(|c| c == '\n');
        self.line.skip_chars(1);
        true
    }
    fn spec(&mut self) -> bool {
        self.flags.unset(DeroffFlags::SPEC_LETTER);
        if self.line.substr(0, 2) == Some("\\(") && self.line.prch(2) && self.line.prch(3) {
            key = self.line.substr(2,4);
            if let Some(specletter) = g_specs_specletter(key) {
                self.condputs(specletter);
                self.flags.set(DeroffFlags::SPEC_LETTER);
            } else if let Some(spec) = g_specs(key) {
                self.condputs(spec);
            }
            self.line.skip_chars(4);
            return true;
        } else if self.line.raw().startswith("\\%") {
            self.flags.set(DeroffFlags::SPEC_LETTER);
            self.line.skip_chars(2);
            return true;
        }
        false
    }
    fn word(&mut self) -> bool {
        let mut got_something = false;
        loop {
            if let Some(m) = G_RE_WORD.find(self.line.raw()) {
                got_something = true;
                self.condputs(m.as_str());
                self.line.skip_chars(m.end());
                while self.spec() {
                    if !self.flags.is_set(DeroffFlags::SPEC_LETTER) {
                        break;
                    }
                }
            } else {
                break;
            }
        }
        got_something
    }
    fn text(&mut self) -> bool {
        loop {
            if let Some(idx) = self.line.find_char('\\') {
                self.condputs(self.line.substr(0, idx));
                self.skip_chars(idx);
                if !self.esc_char_backslash() {
                    self.condputs(self.line.str_at(0));
                    self.skip_chars(1);
                }
            } else {
                self.condputs(self.line.raw());
                self.line.clear();
                break;
            }
        }
        true
    }
}

//class Deroffer:
//    def get_output(self):
//        res = "".join(self.output)
//        clean_res = Deroffer.g_re_newline_collapse.sub("\n", res)
//        return clean_res
//
//    # This gets swapped in in place of condputs the first time tr gets modified
//    def condputs_tr(self, str):
//        special = (
//            self.pic
//            or self.eqn
//            or self.refer
//            or self.macro
//            or (self.skiplists and self.inlist)
//            or (self.skipheaders and self.inheader)
//        )
//        if not special:
//            self.output.append(str.translate(self.tr))
//
//
//    def font2(self):
//        if self.s[0:2] == "\\f":
//            c = self.str_at(2)
//            if c == "(" and self.prch(3) and self.prch(4):
//                self.skip_char(5)
//                return True
//            elif c == "[":
//                self.skip_char(2)
//                while self.prch(0) and self.str_at(0) != "]":
//                    self.skip_char()
//                if self.str_at(0) == "]":
//                    self.skip_char()
//            elif self.prch(2):
//                self.skip_char(3)
//                return True
//        return False
//
//    def var(self):
//        reg = ""
//        s0s1 = self.s[0:2]
//        if s0s1 == "\\n":
//            if self.s[3:5] == "dy":
//                self.skip_char(5)
//                return True
//            elif self.str_at(2) == "(" and self.prch(3) and self.prch(4):
//                self.skip_char(5)
//                return True
//            elif self.str_at(2) == "[" and self.prch(3):
//                self.skip_char(3)
//                while self.str_at(0) and self.str_at(0) != "]":
//                    self.skip_char()
//                return True
//            elif self.prch(2):
//                self.skip_char(3)
//                return True
//        elif s0s1 == "\\*":
//            if self.str_at(2) == "(" and self.prch(3) and self.prch(4):
//                reg = self.s[3:5]
//                self.skip_char(5)
//            elif self.str_at(2) == "[" and self.prch(3):
//                self.skip_char(3)
//                while self.str_at(0) and self.str_at(0) != "]":
//                    reg = reg + self.str_at(0)
//                    self.skip_char()
//                if self.s[0:1] == "]":
//                    self.skip_char()
//                else:
//                    return False
//            elif self.prch(2):
//                reg = self.str_at(2)
//                self.skip_char(3)
//            else:
//                return False
//
//            if reg in self.reg_table:
//                old_s = self.s
//                self.s = self.reg_table[reg]
//                self.text_arg()
//                return True
//        return False
//
//    def text_arg(self):
//        # PCA: The deroff.c textArg() disallowed quotes at the start of an argument
//        # I'm not sure if this was a bug or not
//        got_something = False
//        while True:
//            match = Deroffer.g_re_not_backslash_or_whitespace.match(self.s)
//            if match:
//                # Output the characters in the match
//                self.condputs(match.group(0))
//                self.skip_char(match.end(0))
//                got_something = True
//
//            # Next is either an escape, or whitespace, or the end
//            # If it's the whitespace or the end, we're done
//            if not self.s or self.is_white(0):
//                return got_something
//
//            # Try an escape
//            if not self.esc_char():
//                # Some busted escape? Just output it
//                self.condputs(self.str_at(0))
//                self.skip_char()
//                got_something = True
//
//    def text_arg2(self):
//        if not self.esc_char():
//            if self.s and not self.is_white(0):
//                self.condputs(self.str_at(0))
//                self.skip_char()
//            else:
//                return False
//        while True:
//            if not self.esc_char():
//                if self.s and not self.is_white(0):
//                    self.condputs(self.str_at(0))
//                    self.skip_char()
//                else:
//                    return True
//
//    # Macro functions
//    def request_or_macro2(self):
//        self.skip_char()
//        s0 = self.s[0:1]
//        if s0 == "\\":
//            if self.str_at(1) == '"':
//                self.condputs("\n")
//                return True
//            else:
//                pass
//        elif s0 == "[":
//            self.refer = True
//            self.condputs("\n")
//            return True
//        elif s0 == "]":
//            self.refer = False
//            self.skip_char()
//            return self.text()
//        elif s0 == ".":
//            self.macro = False
//            self.condputs("\n")
//            return True
//
//        self.nobody = False
//        s0s1 = self.s[0:2]
//        if s0s1 == "SH":
//            for header_str in [" SYNOPSIS", ' "SYNOPSIS', " ‹BERSICHT", ' "‹BERSICHT']:
//                if self.s[2:].startswith(header_str):
//                    self.inheader = True
//                    break
//            else:
//                # Did not find a header string
//                self.inheader = False
//                self.nobody = True
//        elif s0s1 in ["SS", "IP", "H "]:
//            self.nobody = True
//        elif s0s1 in ["I ", "IR", "IB", "B ", "BR", "BI", "R ", "RB", "RI", "AB"]:
//            pass
//        elif s0s1 in ["] "]:
//            self.refer = False
//        elif s0s1 in ["PS"]:
//            if self.is_white(2):
//                self.pic = True
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["PE"]:
//            if self.is_white(2):
//                self.pic = False
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["TS"]:
//            if self.is_white(2):
//                self.tbl, self.tblstate = True, self.OPTIONS
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["T&"]:
//            if self.is_white(2):
//                self.tbl, self.tblstate = True, self.FORMAT
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["TE"]:
//            if self.is_white(2):
//                self.tbl = False
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["EQ"]:
//            if self.is_white(2):
//                self.eqn = True
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["EN"]:
//            if self.is_white(2):
//                self.eqn = False
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["R1"]:
//            if self.is_white(2):
//                self.refer2 = True
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["R2"]:
//            if self.is_white(2):
//                self.refer2 = False
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["de"]:
//            macro = True
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["BL", "VL", "AL", "LB", "RL", "ML", "DL"]:
//            if self.is_white(2):
//                self.inlist = True
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["BV"]:
//            if self.str_at(2) == "L" and self.white(self.str_at(3)):
//                self.inlist = True
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["LE"]:
//            if self.is_white(2):
//                self.inlist = False
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["LP", "PP", "P\n"]:
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["ds"]:
//            self.skip_char(2)
//            self.skip_leading_whitespace()
//            if self.str_at(0):
//                # Split at whitespace
//                comps = self.s.split(None, 2)
//                if len(comps) is 2:
//                    name, value = comps
//                    value = value.rstrip()
//                    self.reg_table[name] = value
//            self.condputs("\n")
//            return True
//        elif s0s1 in ["so", "nx"]:
//            # We always ignore include directives
//            # deroff.c for some reason allowed this to fall through to the 'tr' case
//            # I think that was just a bug so I won't replicate it
//            return True
//        elif s0s1 in ["tr"]:
//            self.skip_char(2)
//            self.skip_leading_whitespace()
//            while self.s and self.str_at(0) != "\n":
//                c = self.str_at(0)
//                ns = self.str_at(1)
//                self.skip_char(2)
//                if not ns or ns == "\n":
//                    ns = " "
//                self.tr_from += c
//                self.tr_to += ns
//
//            # Update our table, then swap in the slower tr-savvy condputs
//            try:  # Python2
//                self.tr = string.maketrans(self.tr_from, self.tr_to)
//            except AttributeError:  # Python3
//                self.tr = "".maketrans(self.tr_from, self.tr_to)
//            self.condputs = self.condputs_tr
//
//            return True
//        elif s0s1 in ["sp"]:
//            self.condputs("\n")
//            return True
//        else:
//            self.condputs("\n")
//            return True
//
//        if self.skipheaders and self.nobody:
//            return True
//
//        self.skip_leading_whitespace()
//        while self.s and not self.is_white(0):
//            self.skip_char()
//        self.skip_leading_whitespace()
//        while True:
//            if not self.quoted_arg() and not self.text_arg():
//                if self.s:
//                    self.condputs(self.str_at(0))
//                    self.skip_char()
//                else:
//                    return True
//
//    def do_tbl(self):
//        if self.tblstate == self.OPTIONS:
//            while self.s and self.str_at(0) != ";" and self.str_at(0) != "\n":
//                self.skip_leading_whitespace()
//                if not self.str_at(0).isalpha():
//                    # deroff.c has a bug where it can loop forever here...we try to work around it
//                    self.skip_char()
//                else:  # Parse option
//
//                    option = self.s
//                    arg = ""
//
//                    idx = 0
//                    while option[idx : idx + 1].isalpha():
//                        idx += 1
//
//                    if option[idx : idx + 1] == "(":
//                        option = option[:idx]
//                        self.s = self.s[idx + 1 :]
//                        arg = self.s
//                    else:
//                        self.s = ""
//
//                    if arg:
//                        idx = arg.find(")")
//                        if idx != -1:
//                            arg = arg[:idx]
//                        self.s = self.s[idx + 1 :]
//                    else:
//                        # self.skip_char()
//                        pass
//
//                    if option.lower() == "tab":
//                        self.tblTab = arg[0:1]
//
//            self.tblstate = self.FORMAT
//            self.condputs("\n")
//
//        elif self.tblstate == self.FORMAT:
//            while self.s and self.str_at(0) != "." and self.str_at(0) != "\n":
//                self.skip_leading_whitespace()
//                if self.str_at(0):
//                    self.skip_char()
//
//            if self.str_at(0) == ".":
//                self.tblstate = self.DATA
//            self.condputs("\n")
//        elif self.tblstate == self.DATA:
//            if self.tblTab:
//                self.s = self.s.replace(self.tblTab, "\t")
//            self.text()
//        return True
//
//
//def deroff_files(files):
//    for arg in files:
//        sys.stderr.write(arg + "\n")
//        if arg.endswith(".gz"):
//            f = gzip.open(arg, "r")
//            str = f.read()
//            if IS_PY3:
//                str = str.decode("latin-1")
//        else:
//            f = open(arg, "r")
//            str = f.read()
//        d = Deroffer()
//        d.deroff(str)
//        d.flush_output(sys.stdout)
//        f.close()
