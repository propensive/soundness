use std::process::Command;
use std::sync::atomic::{AtomicI32, Ordering};

// The wrapper mode: spawn java synchronously, forwarding TERM/INT/HUP to it.
// The runner re-invokes itself in this mode so the daemon process appears in
// `ps` under the client's name (since the launcher binary IS this runner with
// the JAR appended), making `killall <client>` target only this daemon.
//
// `args[0]` is the resolved java path; `args[1..]` are the java args.

#[cfg(unix)]
static CHILD_PID: AtomicI32 = AtomicI32::new(0);

#[cfg(unix)]
pub fn run(args: &[String]) -> ! {
    if args.is_empty() { std::process::exit(1); }
    let java = &args[0];
    let java_args = &args[1..];

    let mut child = match Command::new(java).args(java_args).spawn() {
        Ok(child) => child,
        Err(_)    => std::process::exit(1),
    };
    CHILD_PID.store(child.id() as i32, Ordering::SeqCst);

    install_handlers();

    // SIGKILL on the wrapper bypasses these handlers and orphans java; this
    // is an accepted limitation. SIGTERM via `killall` is the supported path.
    let status = child.wait().ok();
    let code = status.and_then(|status| status.code()).unwrap_or(1);
    std::process::exit(code);
}

#[cfg(unix)]
fn install_handlers() {
    let signals = [libc::SIGTERM, libc::SIGINT, libc::SIGHUP];
    unsafe {
        for signal in signals {
            libc::signal(signal, forward as *const () as libc::sighandler_t);
        }
    }
}

#[cfg(unix)]
extern "C" fn forward(signal: libc::c_int) {
    let pid = CHILD_PID.load(Ordering::SeqCst);
    if pid > 0 { unsafe { libc::kill(pid, signal); } }
}

#[cfg(windows)]
pub fn run(args: &[String]) -> ! {
    if args.is_empty() { std::process::exit(1); }
    let java = &args[0];
    let java_args = &args[1..];
    let mut child = match Command::new(java).args(java_args).spawn() {
        Ok(child) => child,
        Err(_)    => std::process::exit(1),
    };
    let status = child.wait().ok();
    std::process::exit(status.and_then(|status| status.code()).unwrap_or(1));
}
