use std::sync::Arc;
use std::sync::atomic::AtomicBool;

#[cfg(unix)]
pub fn install(port: u16, pid: u32, term_flag: Arc<AtomicBool>) {
    use std::sync::atomic::Ordering;

    unsafe {
        let sigs = [
            (libc::SIGINT, "INT"),
            (libc::SIGWINCH, "WINCH"),
            (libc::SIGTERM, "TERM"),
            (libc::SIGHUP, "HUP"),
            (libc::SIGUSR1, "USR1"),
            (libc::SIGUSR2, "USR2"),
        ];
        for (sig, _) in sigs.iter() {
            let _ = libc::signal(*sig, handler as *const () as libc::sighandler_t);
        }
        PORT.store(port as u32, Ordering::SeqCst);
        PID.store(pid, Ordering::SeqCst);
        TERM_FLAG.set(term_flag);
    }
}

#[cfg(unix)]
static PORT: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
#[cfg(unix)]
static PID: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

#[cfg(unix)]
struct TermFlagHolder(std::sync::Mutex<Option<Arc<AtomicBool>>>);

#[cfg(unix)]
impl TermFlagHolder {
    const fn new() -> Self { Self(std::sync::Mutex::new(None)) }
    fn set(&self, f: Arc<AtomicBool>) { *self.0.lock().unwrap() = Some(f); }
    fn get(&self) -> Option<Arc<AtomicBool>> { self.0.lock().unwrap().clone() }
}

#[cfg(unix)]
static TERM_FLAG: TermFlagHolder = TermFlagHolder::new();

#[cfg(unix)]
extern "C" fn handler(sig: libc::c_int) {
    use std::sync::atomic::Ordering;
    let name = match sig {
        libc::SIGINT => "INT",
        libc::SIGWINCH => "WINCH",
        libc::SIGTERM => "TERM",
        libc::SIGHUP => "HUP",
        libc::SIGUSR1 => "USR1",
        libc::SIGUSR2 => "USR2",
        _ => return,
    };
    let port = PORT.load(Ordering::SeqCst) as u16;
    let pid = PID.load(Ordering::SeqCst);
    if port != 0 {
        crate::protocol::send_signal(port, pid, name);
    }
    if sig == libc::SIGTERM {
        if let Some(f) = TERM_FLAG.get() {
            f.store(true, Ordering::SeqCst);
        }
    }
}

#[cfg(windows)]
pub fn install(port: u16, pid: u32, term_flag: Arc<AtomicBool>) {
    use std::sync::atomic::Ordering;
    use windows_sys::Win32::System::Console::SetConsoleCtrlHandler;

    PORT.store(port as u32, Ordering::SeqCst);
    PID.store(pid, Ordering::SeqCst);
    TERM_FLAG.set(term_flag);
    unsafe { SetConsoleCtrlHandler(Some(console_handler), 1); }
}

#[cfg(windows)]
static PORT: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
#[cfg(windows)]
static PID: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

#[cfg(windows)]
struct TermFlagHolder(std::sync::Mutex<Option<Arc<AtomicBool>>>);

#[cfg(windows)]
impl TermFlagHolder {
    const fn new() -> Self { Self(std::sync::Mutex::new(None)) }
    fn set(&self, f: Arc<AtomicBool>) { *self.0.lock().unwrap() = Some(f); }
    fn get(&self) -> Option<Arc<AtomicBool>> { self.0.lock().unwrap().clone() }
}

#[cfg(windows)]
static TERM_FLAG: TermFlagHolder = TermFlagHolder::new();

#[cfg(windows)]
unsafe extern "system" fn console_handler(ctrl_type: u32) -> windows_sys::Win32::Foundation::BOOL {
    use std::sync::atomic::Ordering;
    use windows_sys::Win32::System::Console::{
        CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_LOGOFF_EVENT, CTRL_SHUTDOWN_EVENT,
    };
    let name = match ctrl_type {
        x if x == CTRL_C_EVENT        => "CTRL_C",
        x if x == CTRL_BREAK_EVENT    => "CTRL_BREAK",
        x if x == CTRL_CLOSE_EVENT    => "CTRL_CLOSE",
        x if x == CTRL_LOGOFF_EVENT   => "CTRL_LOGOFF",
        x if x == CTRL_SHUTDOWN_EVENT => "CTRL_SHUTDOWN",
        _ => return 0,
    };
    let port = PORT.load(Ordering::SeqCst) as u16;
    let pid = PID.load(Ordering::SeqCst);
    if port != 0 {
        crate::protocol::send_signal(port, pid, name);
    }
    if ctrl_type == CTRL_CLOSE_EVENT || ctrl_type == CTRL_LOGOFF_EVENT || ctrl_type == CTRL_SHUTDOWN_EVENT {
        if let Some(f) = TERM_FLAG.get() {
            f.store(true, Ordering::SeqCst);
        }
    }
    1
}
