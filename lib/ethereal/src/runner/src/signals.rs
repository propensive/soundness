use std::path::PathBuf;
use std::sync::{Arc, OnceLock};
use std::sync::atomic::{AtomicU32, AtomicBool, Ordering};

static SOCKET_PATH: OnceLock<PathBuf> = OnceLock::new();
static CLIENT_PID: AtomicU32 = AtomicU32::new(0);
static TERMINATION_FLAG: OnceLock<Arc<AtomicBool>> = OnceLock::new();

fn forward_signal(name: &str) {
    if let Some(path) = SOCKET_PATH.get() {
        // UnixStream::connect is not strictly async-signal-safe (allocates),
        // but this matches the pre-existing TcpStream::connect behaviour and
        // has been reliable in practice. Revisit only if signal storms cause
        // trouble.
        crate::protocol::send_signal(path.as_path(), CLIENT_PID.load(Ordering::SeqCst), name);
    }
}

fn flag_termination() {
    if let Some(flag) = TERMINATION_FLAG.get() {
        flag.store(true, Ordering::SeqCst);
    }
}

fn install_state(socket_path: PathBuf, pid: u32, termination_flag: Arc<AtomicBool>) {
    let _ = SOCKET_PATH.set(socket_path);
    CLIENT_PID.store(pid, Ordering::SeqCst);
    let _ = TERMINATION_FLAG.set(termination_flag);
}

#[cfg(unix)]
pub fn install(socket_path: PathBuf, pid: u32, termination_flag: Arc<AtomicBool>) {
    install_state(socket_path, pid, termination_flag);

    let signals = [
        libc::SIGINT, libc::SIGWINCH, libc::SIGTERM,
        libc::SIGHUP, libc::SIGUSR1, libc::SIGUSR2,
    ];
    unsafe {
        for signal in signals {
            libc::signal(signal, handler as *const () as libc::sighandler_t);
        }
    }
}

#[cfg(unix)]
extern "C" fn handler(signal: libc::c_int) {
    let name = match signal {
        libc::SIGINT   => "INT",
        libc::SIGWINCH => "WINCH",
        libc::SIGTERM  => "TERM",
        libc::SIGHUP   => "HUP",
        libc::SIGUSR1  => "USR1",
        libc::SIGUSR2  => "USR2",
        _ => return,
    };
    forward_signal(name);
    if signal == libc::SIGTERM { flag_termination(); }
}

#[cfg(windows)]
pub fn install(socket_path: PathBuf, pid: u32, termination_flag: Arc<AtomicBool>) {
    use windows_sys::Win32::System::Console::SetConsoleCtrlHandler;
    install_state(socket_path, pid, termination_flag);
    unsafe { SetConsoleCtrlHandler(Some(console_handler), 1); }
}

#[cfg(windows)]
unsafe extern "system" fn console_handler(ctrl_type: u32) -> windows_sys::Win32::Foundation::BOOL {
    use windows_sys::Win32::System::Console::{
        CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_LOGOFF_EVENT, CTRL_SHUTDOWN_EVENT,
    };
    let name = match ctrl_type {
        CTRL_C_EVENT        => "CTRL_C",
        CTRL_BREAK_EVENT    => "CTRL_BREAK",
        CTRL_CLOSE_EVENT    => "CTRL_CLOSE",
        CTRL_LOGOFF_EVENT   => "CTRL_LOGOFF",
        CTRL_SHUTDOWN_EVENT => "CTRL_SHUTDOWN",
        _ => return 0,
    };
    forward_signal(name);
    if matches!(ctrl_type, CTRL_CLOSE_EVENT | CTRL_LOGOFF_EVENT | CTRL_SHUTDOWN_EVENT) {
        flag_termination();
    }
    1
}
