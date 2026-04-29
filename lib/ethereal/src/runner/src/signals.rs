use std::path::PathBuf;
use std::sync::{Arc, OnceLock};
use std::sync::atomic::{AtomicU32, AtomicBool, Ordering};

use crate::protocol::SignalAck;

static SOCKET_PATH: OnceLock<PathBuf> = OnceLock::new();
static CLIENT_PID: AtomicU32 = AtomicU32::new(0);
static TERMINATION_FLAG: OnceLock<Arc<AtomicBool>> = OnceLock::new();
static TIMEOUT_MS: AtomicU32 = AtomicU32::new(250);

const DEFAULT_TIMEOUT_MS: u32 = 250;

fn forward_signal(name: &str) -> SignalAck {
    if let Some(path) = SOCKET_PATH.get() {
        // UnixStream::connect is not strictly async-signal-safe (allocates),
        // but this matches the pre-existing TcpStream::connect behaviour and
        // has been reliable in practice. Revisit only if signal storms cause
        // trouble.
        crate::protocol::send_signal(
            path.as_path(),
            CLIENT_PID.load(Ordering::SeqCst),
            name,
            TIMEOUT_MS.load(Ordering::SeqCst) as u64,
        )
    } else {
        SignalAck::Timeout
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
    let timeout = std::env::var("ETHEREAL_SIGNAL_TIMEOUT_MS")
        .ok()
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(DEFAULT_TIMEOUT_MS);
    TIMEOUT_MS.store(timeout, Ordering::SeqCst);
}

#[cfg(unix)]
fn unix_signum(name: &str) -> Option<i32> {
    match name {
        "INT"  => Some(libc::SIGINT),
        "TERM" => Some(libc::SIGTERM),
        "HUP"  => Some(libc::SIGHUP),
        _      => None,
    }
}

#[cfg(unix)]
fn fallback(name: &str) {
    if let Some(signum) = unix_signum(name) {
        // Restore the OS default action for this signal and re-raise it on
        // ourselves. SIGINT/SIGTERM/SIGHUP terminate the process; the parent
        // shell sees the conventional 128+signum exit status.
        unsafe {
            libc::signal(signum, libc::SIG_DFL);
            libc::raise(signum);
        }
    }
    // WINCH, USR1, USR2: no fallback action — the daemon declined and we
    // intentionally drop the signal.
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
    let ack = forward_signal(name);
    if signal == libc::SIGTERM { flag_termination(); }
    match ack {
        SignalAck::Accept                    => {}
        SignalAck::Reject | SignalAck::Timeout => fallback(name),
    }
}

#[cfg(windows)]
fn fallback(name: &str) {
    match name {
        "CTRL_C" | "CTRL_BREAK" | "CTRL_CLOSE" | "CTRL_LOGOFF" | "CTRL_SHUTDOWN" => {
            std::process::exit(1);
        }
        _ => {}
    }
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
    let ack = forward_signal(name);
    if matches!(ctrl_type, CTRL_CLOSE_EVENT | CTRL_LOGOFF_EVENT | CTRL_SHUTDOWN_EVENT) {
        flag_termination();
    }
    match ack {
        SignalAck::Accept                    => {}
        SignalAck::Reject | SignalAck::Timeout => fallback(name),
    }
    1
}
