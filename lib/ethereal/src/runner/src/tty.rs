#[cfg(unix)]
pub struct TtyState {
    termios: Option<libc::termios>,
    is_tty: bool,
}

#[cfg(windows)]
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
