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
    use std::os::windows::process::CommandExt;
    // The wrapper itself was launched with DETACHED_PROCESS and so has no
    // console; without CREATE_NO_WINDOW, Windows would allocate a fresh
    // console for the JVM child and flash a "java" terminal window on screen
    // for as long as the daemon runs.
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;

    if args.is_empty() { std::process::exit(1); }

    // Tie the JVM's lifetime to the wrapper's via a Job Object with
    // KILL_ON_JOB_CLOSE: when the wrapper dies — including via
    // `Stop-Process -Force` / `TerminateProcess`, which bypasses any
    // handler — the kernel closes the wrapper's handles, the job's last
    // reference goes with them, and Windows terminates every process in
    // the job. The JVM joins the job by inheritance from the wrapper.
    bind_to_kill_on_close_job();

    let java = &args[0];
    let java_args = &args[1..];
    let mut child = match Command::new(java)
        .args(java_args)
        .creation_flags(CREATE_NO_WINDOW)
        .spawn()
    {
        Ok(child) => child,
        Err(_)    => std::process::exit(1),
    };
    let status = child.wait().ok();
    std::process::exit(status.and_then(|status| status.code()).unwrap_or(1));
}

#[cfg(windows)]
fn bind_to_kill_on_close_job() {
    use windows_sys::Win32::Foundation::HANDLE;
    use windows_sys::Win32::System::JobObjects::{
        AssignProcessToJobObject, CreateJobObjectW,
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
        JobObjectExtendedLimitInformation, SetInformationJobObject,
    };
    use windows_sys::Win32::System::Threading::GetCurrentProcess;

    unsafe {
        let job: HANDLE = CreateJobObjectW(std::ptr::null(), std::ptr::null());
        if job.is_null() { return; }

        let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = std::mem::zeroed();
        info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        let info_size = std::mem::size_of::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>() as u32;
        SetInformationJobObject(
            job,
            JobObjectExtendedLimitInformation,
            &info as *const _ as *const _,
            info_size,
        );

        AssignProcessToJobObject(job, GetCurrentProcess());
        // Deliberately leak the handle: closing it now would trigger
        // KILL_ON_JOB_CLOSE immediately. The kernel reclaims it when the
        // wrapper exits, which is exactly the moment we want it to fire.
    }
}
