use std::io::{self, IsTerminal, Write};

const YELLOW: u8 = 33;
const GREEN: u8 = 32;
const RESET: &str = "\x1b[0m";
const CLEAR_EOL: &str = "\x1b[K";
const COL1: &str = "\x1b[1G";
const FULL_BAR: &str = "████████";

pub fn step(name: &str, message: &str) { emit(YELLOW, FULL_BAR, false, name, message); }

pub fn done(name: &str, message: &str) { emit(GREEN, FULL_BAR, true, name, message); }

fn emit(color: u8, bar: &str, newline: bool, name: &str, message: &str) {
    let stderr = io::stderr();
    if !stderr.is_terminal() { return; }
    let label = strip_short_ext(name);
    let no_color = std::env::var_os("NO_COLOR").is_some_and(|v| !v.is_empty());
    let dumb = std::env::var("TERM").map(|t| t == "dumb").unwrap_or(false);
    let mut out = stderr.lock();
    let result = if no_color || dumb {
        write!(out, "{COL1}{bar}\t{label}\t┃ {message}{CLEAR_EOL}")
    } else {
        write!(out, "{COL1}\x1b[{color}m{bar}{RESET}\t{label}\t┃ {message}{CLEAR_EOL}")
    };
    let _ = result;
    if newline { let _ = writeln!(out); }
    let _ = out.flush();
}

fn strip_short_ext(name: &str) -> &str {
    if let Some(dot) = name.rfind('.') {
        let ext = &name[dot + 1..];
        if ext.chars().count() == 3 { return &name[..dot]; }
    }
    name
}
