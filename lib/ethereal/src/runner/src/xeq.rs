use std::io::{self, IsTerminal, Write};

const YELLOW: u8 = 33;
const GREEN: u8 = 32;
const RESET: &str = "\x1b[0m";
const CLEAR_EOL: &str = "\x1b[K";
const COL1: &str = "\x1b[1G";

pub fn step(blocks: &str, message: &str) { emit(YELLOW, blocks, false, message); }

pub fn done(blocks: &str, message: &str) { emit(GREEN, blocks, true, message); }

fn emit(color: u8, blocks: &str, newline: bool, message: &str) {
    let stderr = io::stderr();
    if !stderr.is_terminal() { return; }
    let no_color = std::env::var_os("NO_COLOR").is_some_and(|v| !v.is_empty());
    let dumb = std::env::var("TERM").map(|t| t == "dumb").unwrap_or(false);
    let mut out = stderr.lock();
    let result = if no_color || dumb {
        write!(out, "{COL1}{blocks}\txeq\t┃ {message}{CLEAR_EOL}")
    } else {
        write!(out, "{COL1}\x1b[{color}m{blocks}{RESET}\txeq\t┃ {message}{CLEAR_EOL}")
    };
    let _ = result;
    if newline { let _ = writeln!(out); }
    let _ = out.flush();
}
