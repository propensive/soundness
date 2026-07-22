#[cfg(unix)]
use std::io::Write;

#[cfg(unix)]
#[derive(Clone, Copy)]
pub struct TtyState {
    termios: Option<libc::termios>,
    is_tty: bool,
}

#[cfg(windows)]
#[derive(Clone, Copy)]
pub struct TtyState {
    stdin_mode: Option<u32>,
    stdout_mode: Option<u32>,
    is_tty: bool,
}

#[cfg(unix)]
pub fn stdin_is_tty() -> bool {
    unsafe { libc::isatty(libc::STDIN_FILENO) != 0 }
}

#[cfg(windows)]
pub fn stdin_is_tty() -> bool {
    use windows_sys::Win32::System::Console::{GetConsoleMode, GetStdHandle, STD_INPUT_HANDLE};
    unsafe {
        let h = GetStdHandle(STD_INPUT_HANDLE);
        let mut mode: u32 = 0;
        GetConsoleMode(h, &mut mode) != 0
    }
}

// The launcher must know the real terminal size so it can forward it to the
// daemon (which only sees a socket and cannot query the tty itself). Querying
// from the launcher avoids a fragile ANSI cursor-position handshake across
// the socket pipeline.
#[cfg(unix)]
pub fn terminal_size() -> Option<(u16, u16)> {
    if !stdin_is_tty() { return None; }
    let mut ws: libc::winsize = unsafe { std::mem::zeroed() };
    let result = unsafe { libc::ioctl(libc::STDIN_FILENO, libc::TIOCGWINSZ, &mut ws) };
    if result == 0 && ws.ws_col > 0 && ws.ws_row > 0 {
        Some((ws.ws_col, ws.ws_row))
    } else {
        None
    }
}

// OSC 11 query (`\e]11;?\e\\`) asks the terminal to report its background
// colour as `\e]11;rgb:RRRR/GGGG/BBBB\e\\`. Running this from the launcher
// (before forwarders start) keeps the handshake confined to one process with
// direct tty access — much more reliable than letting the daemon attempt it
// across the socket pipeline. Any non-response bytes are returned so they can
// be prepended to the daemon's stdin and not lost.
#[cfg(unix)]
pub fn query_bg_color(timeout: std::time::Duration) -> (Option<String>, Vec<u8>) {
    use std::io::Read;
    use std::os::fd::AsRawFd;
    use std::time::Instant;

    if !stdin_is_tty() { return (None, Vec::new()); }

    let mut stdout = std::io::stdout();
    if stdout.write_all(b"\x1b]11;?\x1b\\").is_err() { return (None, Vec::new()); }
    let _ = stdout.flush();

    let mut buf: Vec<u8> = Vec::with_capacity(64);
    let deadline = Instant::now() + timeout;
    let fd = std::io::stdin().as_raw_fd();

    loop {
        let now = Instant::now();
        if now >= deadline { break; }
        let remaining = (deadline - now).as_millis() as i32;
        let mut pfd = libc::pollfd { fd, events: libc::POLLIN, revents: 0 };
        let r = unsafe { libc::poll(&mut pfd, 1, remaining.max(1)) };
        if r <= 0 { break; }

        let mut chunk = [0u8; 64];
        match std::io::stdin().read(&mut chunk) {
            Ok(0) | Err(_) => break,
            Ok(n) => {
                buf.extend_from_slice(&chunk[..n]);
                // Stop as soon as we see a terminator that could end an OSC response.
                if find_subseq(&buf, b"\x1b\\").is_some() || buf.contains(&0x07) {
                    break;
                }
            }
        }
    }

    parse_osc11(&buf)
}

#[cfg(windows)]
pub fn query_bg_color(_timeout: std::time::Duration) -> (Option<String>, Vec<u8>) {
    // Windows Console doesn't reliably support OSC 11 background queries.
    (None, Vec::new())
}

#[cfg(unix)]
fn parse_osc11(buf: &[u8]) -> (Option<String>, Vec<u8>) {
    let prefix = b"\x1b]11;rgb:";
    let Some(start) = find_subseq(buf, prefix) else {
        return (None, buf.to_vec());
    };
    let after_prefix = start + prefix.len();
    let rest = &buf[after_prefix..];

    let term_st = find_subseq(rest, b"\x1b\\");
    let term_bel = rest.iter().position(|&b| b == 0x07);
    let (rgb_end, term_len) = match (term_st, term_bel) {
        (Some(a), Some(b)) if a < b => (a, 2),
        (Some(_), Some(b))           => (b, 1),
        (Some(a), None)              => (a, 2),
        (None, Some(b))              => (b, 1),
        (None, None)                 => return (None, buf.to_vec()),
    };

    let rgb_str = std::str::from_utf8(&rest[..rgb_end]).ok().map(str::to_owned);

    let mut leftover = buf[..start].to_vec();
    leftover.extend_from_slice(&rest[rgb_end + term_len..]);

    (rgb_str, leftover)
}

#[cfg(unix)]
fn find_subseq(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() || haystack.len() < needle.len() { return None; }
    haystack.windows(needle.len()).position(|w| w == needle)
}

#[cfg(windows)]
pub fn terminal_size() -> Option<(u16, u16)> {
    use windows_sys::Win32::System::Console::{
        CONSOLE_SCREEN_BUFFER_INFO, GetConsoleScreenBufferInfo, GetStdHandle, STD_OUTPUT_HANDLE,
    };
    unsafe {
        let handle = GetStdHandle(STD_OUTPUT_HANDLE);
        let mut info: CONSOLE_SCREEN_BUFFER_INFO = std::mem::zeroed();
        if GetConsoleScreenBufferInfo(handle, &mut info) != 0 {
            let cols = (info.srWindow.Right - info.srWindow.Left + 1).max(0) as u16;
            let rows = (info.srWindow.Bottom - info.srWindow.Top + 1).max(0) as u16;
            if cols > 0 && rows > 0 { Some((cols, rows)) } else { None }
        } else {
            None
        }
    }
}

#[cfg(unix)]
pub fn save_tty_state() -> TtyState {
    let mut termios: libc::termios = unsafe { std::mem::zeroed() };
    let is_tty = stdin_is_tty();
    if !is_tty {
        crate::debug!("tty: save_tty_state — stdin is not a tty, nothing to save");
        return TtyState { termios: None, is_tty: false };
    }
    unsafe {
        if libc::tcgetattr(libc::STDIN_FILENO, &mut termios) == 0 {
            crate::debug!("tty: save_tty_state — saved termios");
            return TtyState { termios: Some(termios), is_tty: true };
        }
    }
    crate::debug!(
        "tty: save_tty_state — tcgetattr failed: {}",
        std::io::Error::last_os_error(),
    );
    TtyState { termios: None, is_tty }
}

#[cfg(windows)]
pub fn save_tty_state() -> TtyState {
    use windows_sys::Win32::System::Console::{GetConsoleMode, GetStdHandle, STD_INPUT_HANDLE, STD_OUTPUT_HANDLE};
    let is_tty = stdin_is_tty();
    unsafe {
        let h_in = GetStdHandle(STD_INPUT_HANDLE);
        let h_out = GetStdHandle(STD_OUTPUT_HANDLE);
        let mut in_mode: u32 = 0;
        let mut out_mode: u32 = 0;
        let in_ok = GetConsoleMode(h_in, &mut in_mode) != 0;
        let out_ok = GetConsoleMode(h_out, &mut out_mode) != 0;
        TtyState {
            stdin_mode: if in_ok { Some(in_mode) } else { None },
            stdout_mode: if out_ok { Some(out_mode) } else { None },
            is_tty,
        }
    }
}

#[cfg(unix)]
pub fn set_raw_mode() {
    if !stdin_is_tty() {
        crate::debug!("tty: set_raw_mode skipped — stdin is not a tty");
        return;
    }
    unsafe {
        let mut t: libc::termios = std::mem::zeroed();
        if libc::tcgetattr(libc::STDIN_FILENO, &mut t) != 0 {
            crate::debug!(
                "tty: set_raw_mode — tcgetattr failed: {}",
                std::io::Error::last_os_error(),
            );
            return;
        }
        // Equivalent to: intr undef -echo icanon raw opost (keep opost)
        t.c_lflag &= !(libc::ICANON | libc::ECHO | libc::IEXTEN | libc::ISIG);
        t.c_iflag &= !(libc::IXON | libc::ICRNL | libc::BRKINT | libc::INPCK | libc::ISTRIP);
        // OPOST stays on (as in the bash prefix: `opost`)
        t.c_oflag |= libc::OPOST;
        t.c_cc[libc::VMIN] = 1;
        t.c_cc[libc::VTIME] = 0;
        // intr undef
        t.c_cc[libc::VINTR] = 0;
        if libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &t) != 0 {
            crate::debug!(
                "tty: set_raw_mode — tcsetattr failed: {}",
                std::io::Error::last_os_error(),
            );
        } else {
            crate::debug!("tty: set_raw_mode — raw mode set");
        }
    }
}

// Put the terminal back into canonical ("cooked") mode, at the daemon's request, so a
// line-based command gets the driver's own echo and line editing. The saved pre-launch
// termios *is* the user's cooked mode, so prefer restoring it verbatim; only when nothing
// was saved (tcgetattr failed) do we synthesise a plausible cooked setting.
#[cfg(unix)]
pub fn set_cooked_mode(state: &TtyState) {
    if !stdin_is_tty() {
        crate::debug!("tty: set_cooked_mode skipped — stdin is not a tty");
        return;
    }
    if state.termios.is_some() {
        restore_tty_state(state);
        crate::debug!("tty: set_cooked_mode — restored saved (cooked) termios");
        return;
    }
    unsafe {
        let mut t: libc::termios = std::mem::zeroed();
        if libc::tcgetattr(libc::STDIN_FILENO, &mut t) != 0 {
            crate::debug!(
                "tty: set_cooked_mode — tcgetattr failed: {}",
                std::io::Error::last_os_error(),
            );
            return;
        }
        t.c_lflag |= libc::ICANON | libc::ECHO | libc::ECHOE | libc::ISIG | libc::IEXTEN;
        t.c_iflag |= libc::ICRNL | libc::IXON | libc::BRKINT;
        t.c_oflag |= libc::OPOST | libc::ONLCR;
        t.c_cc[libc::VINTR] = 3;
        if libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &t) != 0 {
            crate::debug!(
                "tty: set_cooked_mode — tcsetattr failed: {}",
                std::io::Error::last_os_error(),
            );
        } else {
            crate::debug!("tty: set_cooked_mode — synthesised cooked mode set");
        }
    }
}

#[cfg(windows)]
pub fn set_cooked_mode(state: &TtyState) {
    use windows_sys::Win32::System::Console::{
        GetConsoleMode, GetStdHandle, SetConsoleMode, STD_INPUT_HANDLE,
        ENABLE_ECHO_INPUT, ENABLE_LINE_INPUT, ENABLE_PROCESSED_INPUT,
        ENABLE_VIRTUAL_TERMINAL_INPUT,
    };
    if !state.is_tty {
        crate::debug!("tty: set_cooked_mode skipped — stdin is not a console");
        return;
    }
    unsafe {
        let h_in = GetStdHandle(STD_INPUT_HANDLE);
        let mut in_mode: u32 = 0;
        if GetConsoleMode(h_in, &mut in_mode) == 0 {
            crate::debug!(
                "tty: set_cooked_mode — GetConsoleMode(stdin) failed: {}",
                std::io::Error::last_os_error(),
            );
            return;
        }
        in_mode |= ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT;
        in_mode &= !ENABLE_VIRTUAL_TERMINAL_INPUT;
        if SetConsoleMode(h_in, in_mode) == 0 {
            crate::debug!(
                "tty: set_cooked_mode — SetConsoleMode(stdin) failed: {}",
                std::io::Error::last_os_error(),
            );
        } else {
            crate::debug!("tty: set_cooked_mode — stdin cooked mode set");
        }
    }
}

#[cfg(windows)]
pub fn set_raw_mode() {
    use windows_sys::Win32::System::Console::{
        GetStdHandle, SetConsoleMode, STD_INPUT_HANDLE, STD_OUTPUT_HANDLE,
        ENABLE_ECHO_INPUT, ENABLE_LINE_INPUT, ENABLE_PROCESSED_INPUT,
        ENABLE_VIRTUAL_TERMINAL_INPUT, ENABLE_VIRTUAL_TERMINAL_PROCESSING,
    };
    unsafe {
        let h_in = GetStdHandle(STD_INPUT_HANDLE);
        let mut in_mode: u32 = 0;
        if windows_sys::Win32::System::Console::GetConsoleMode(h_in, &mut in_mode) != 0 {
            in_mode &= !(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT);
            in_mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;
            if SetConsoleMode(h_in, in_mode) == 0 {
                crate::debug!(
                    "tty: set_raw_mode — SetConsoleMode(stdin) failed: {}",
                    std::io::Error::last_os_error(),
                );
            } else {
                crate::debug!("tty: set_raw_mode — stdin raw mode set");
            }
        } else {
            crate::debug!(
                "tty: set_raw_mode — GetConsoleMode(stdin) failed: {}",
                std::io::Error::last_os_error(),
            );
        }
        let h_out = GetStdHandle(STD_OUTPUT_HANDLE);
        let mut out_mode: u32 = 0;
        if windows_sys::Win32::System::Console::GetConsoleMode(h_out, &mut out_mode) != 0 {
            out_mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            if SetConsoleMode(h_out, out_mode) == 0 {
                crate::debug!(
                    "tty: set_raw_mode — SetConsoleMode(stdout) failed: {}",
                    std::io::Error::last_os_error(),
                );
            } else {
                crate::debug!("tty: set_raw_mode — stdout vt processing enabled");
            }
        } else {
            crate::debug!(
                "tty: set_raw_mode — GetConsoleMode(stdout) failed: {}",
                std::io::Error::last_os_error(),
            );
        }
    }
}

#[cfg(unix)]
pub fn restore_tty_state(state: &TtyState) {
    if !state.is_tty {
        crate::debug!("tty: restore_tty_state — stdin is not a tty, nothing to restore");
        return;
    }
    if let Some(termios) = state.termios {
        unsafe {
            if libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &termios) != 0 {
                crate::debug!(
                    "tty: restore_tty_state — tcsetattr failed: {}",
                    std::io::Error::last_os_error(),
                );
            } else {
                crate::debug!("tty: restore_tty_state — termios restored");
            }
        }
    } else {
        crate::debug!("tty: restore_tty_state — no saved termios available");
    }
}

#[cfg(windows)]
pub fn restore_tty_state(state: &TtyState) {
    if !state.is_tty {
        crate::debug!("tty: restore_tty_state — stdin is not a tty, nothing to restore");
        return;
    }
    use windows_sys::Win32::System::Console::{GetStdHandle, SetConsoleMode, STD_INPUT_HANDLE, STD_OUTPUT_HANDLE};
    unsafe {
        if let Some(mode) = state.stdin_mode {
            if SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), mode) == 0 {
                crate::debug!(
                    "tty: restore_tty_state — SetConsoleMode(stdin) failed: {}",
                    std::io::Error::last_os_error(),
                );
            }
        }
        if let Some(mode) = state.stdout_mode {
            if SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), mode) == 0 {
                crate::debug!(
                    "tty: restore_tty_state — SetConsoleMode(stdout) failed: {}",
                    std::io::Error::last_os_error(),
                );
            }
        }
    }
}
