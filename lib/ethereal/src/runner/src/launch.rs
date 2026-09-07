use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use crate::config::BuildConfig;
use crate::progress::{self, Progress, Watch};

const STARTUP_POLL: Duration = Duration::from_millis(50);
// How long the daemon may go without any sign of progress before it is presumed hung. A
// Burdock-repackaged application reports its dependency downloads through the `progress` file
// (see `progress.rs`), and every change to that file restarts this window, so a cold cache on a
// slow link is not a hang (#1938). Without a progress file the window is simply the old fixed
// deadline measured from the spawn.
const STARTUP_IDLE_LIMIT: Duration = Duration::from_secs(10);
const BUILD_FILE_GRACE_ATTEMPTS: u32 = 20;

pub enum Outcome {
    Bound,
    Failed,
    Exited,
    Idle(Option<Progress>),
}

pub fn launch(
    script: &Path,
    name: &str,
    base_dir: &Path,
    build_file: &Path,
    pid_file: &Path,
    socket_file: &Path,
    fail_file: &Path,
    progress_file: &Path,
    config: &BuildConfig,
    download: bool,
) {
    crate::debug!("launch: start name={} script={}", name, script.display());
    let java = match crate::java::find_java(config.java_min, config.java_pref, config.bundle, download, name) {
        Some(path) => { crate::debug!("launch: java found at {}", path.display()); path },
        None => {
            crate::debug!("launch: java not found");
            crate::java::java_not_found_message(config.java_min, &script.to_string_lossy());
            crate::state::abort(fail_file);
            std::process::exit(1);
        }
    };

    // Re-invoke ourselves in wrapper mode so the daemon process appears under
    // the client's name (the launcher binary is this runner with the JAR
    // appended). The wrapper exec's java synchronously and forwards signals.
    let executable = std::env::current_exe().unwrap_or_else(|_| script.to_path_buf());
    let mut command = Command::new(&executable);
    command.arg(crate::WRAP_SENTINEL).arg(&java);
    for argument in build_java_arguments(script, name, progress_file, config) { command.arg(argument); }
    // Capture the JVM invocation time as late as possible — after the slow
    // argument-building work (zsh probe, $fpath capture) — so `uptime` in the
    // daemon measures from the moment java is actually spawned, not from when
    // launch() was entered.
    command.arg(format!("-Dethereal.startTime={}", crate::now_ms()));
    command.arg("-jar").arg(script);
    command.stdin(Stdio::null());
    command.stdout(Stdio::null());
    match std::fs::OpenOptions::new().create(true).append(true).open(base_dir.join("daemon.log")) {
        Ok(log) => { command.stderr(Stdio::from(log)); }
        Err(_)  => { command.stderr(Stdio::null()); }
    }

    detach(&mut command);
    // On Windows, `Command::spawn` calls `CreateProcessW` with
    // `bInheritHandles=TRUE`, which propagates *every* inheritable handle in
    // the parent — not just the configured stdio. The launcher's own
    // stdin/stdout/stderr were inheritable when our caller (e.g. PowerShell's
    // `Process` API) created them, so they would otherwise leak into the
    // daemon and stay open for as long as the daemon runs, blocking the
    // caller's `ReadToEndAsync` long after the launcher itself has exited.
    // Mark them non-inheritable just before the spawn; the launcher continues
    // to use them normally afterwards.
    mark_stdio_non_inheritable();
    // A progress file left by a bootstrap that died mid-download would otherwise
    // count as this daemon's first sign of life.
    let _ = std::fs::remove_file(progress_file);
    crate::debug!("launch: spawning daemon: {} (wrapper)", executable.display());

    let mut child = match command.spawn() {
        Ok(child) => child,
        Err(e) => {
            crate::debug!("launch: spawn failed: {}", e);
            crate::state::abort(fail_file);
            crate::state::report_failure(base_dir, name, &format!("it could not be spawned ({e})"));
            std::process::exit(1);
        }
    };
    crate::debug!("launch: spawned, child pid={}", child.id());

    let _ = std::fs::write(pid_file, format!("{}\n", child.id()));

    let start = Instant::now();
    let (outcome, shown) = await_startup(socket_file, fail_file, progress_file, name, Some(&mut child));
    let _ = std::fs::remove_file(progress_file);
    crate::debug!("launch: post-poll socket_ready={} fail_exists={}",
        crate::state::socket_ready(socket_file), fail_file.exists());

    match outcome {
        Outcome::Bound => (),

        // The daemon process exited before its socket appeared: it failed during startup.
        Outcome::Exited => {
            crate::debug!("launch: daemon exited during startup, aborting");
            crate::state::abort(fail_file);
            crate::state::report_failure(base_dir, name, "it exited during startup");
            crate::state::backout(fail_file, pid_file, name);
            std::process::exit(1);
        }

        Outcome::Failed | Outcome::Idle(_) => {
            crate::debug!("launch: socket never appeared, aborting");
            crate::state::abort(fail_file);
            crate::state::report_failure(base_dir, name, &idle_reason(&outcome));
            crate::state::backout(fail_file, pid_file, name);
            std::process::exit(1);
        }
    }

    if shown {
        let secs = start.elapsed().as_secs_f64();
        crate::xeq::done(name, &format!("Started in {secs:.1}s"));
    }

    // The daemon writes the build file (recording the launcher's size, mtime and hash)
    // shortly after binding the socket. We don't need it to connect, but the staleness
    // check in `check_state` on later invocations does.
    let _ = crate::state::await_file(build_file, BUILD_FILE_GRACE_ATTEMPTS);
    crate::debug!("launch: build_file ready={}", crate::state::file_has_content(build_file));

    crate::state::backout(fail_file, pid_file, name);
    crate::debug!("launch: returning");
}

// Waits for the daemon to bind its socket, restarting the idle window on every change to the
// progress file and echoing the bootstrap's position to the terminal. `child` is the daemon
// process when this launcher spawned it (its exit is then detected immediately rather than at
// the deadline); a launcher waiting on another launcher's daemon passes `None`. Returns the
// outcome and whether a status line was shown (so the caller can close it).
pub fn await_startup(
    socket_file: &Path,
    fail_file: &Path,
    progress_file: &Path,
    name: &str,
    mut child: Option<&mut Child>,
) -> (Outcome, bool) {
    let start = Instant::now();
    let mut watch = Watch::new(start);
    let mut shown = false;

    loop {
        if crate::state::socket_ready(socket_file) { return (Outcome::Bound, shown); }
        if fail_file.exists() { return (Outcome::Failed, shown); }

        // If the daemon process exits before its socket appears, stop immediately rather
        // than polling out the full window — or, worse, falling through to connect to a
        // socket that will never accept (which can block forever).
        if let Some(child) = child.as_deref_mut() {
            if matches!(child.try_wait(), Ok(Some(_))) && !crate::state::socket_ready(socket_file) {
                return (Outcome::Exited, shown);
            }
        }

        let now = Instant::now();
        let current = progress::read(progress_file);

        if watch.observe(current, now) {
            if let Some(progress) = current {
                crate::xeq::step(name, &progress::message(&progress));
                shown = true;
            }
        }

        if watch.expired(now, STARTUP_IDLE_LIMIT) {
            return (Outcome::Idle(watch.current()), shown);
        }

        if !shown && start.elapsed() >= Duration::from_secs(2) {
            crate::xeq::step(name, "Starting…");
            shown = true;
        }

        std::thread::sleep(STARTUP_POLL);
    }
}

pub fn idle_reason(outcome: &Outcome) -> String {
    match outcome {
        Outcome::Idle(Some(progress)) => progress::reason(progress, STARTUP_IDLE_LIMIT),
        _ => format!("it did not bind its socket within {}s", STARTUP_IDLE_LIMIT.as_secs()),
    }
}

fn build_java_arguments(script: &Path, name: &str, progress_file: &Path, config: &BuildConfig) -> Vec<String> {
    let jar_size = std::fs::metadata(script).map(|metadata| metadata.len()).unwrap_or(0);
    let user_name = std::env::var("USER").or_else(|_| std::env::var("USERNAME")).unwrap_or_default();
    let uid: u32 = {
        #[cfg(unix)] unsafe { libc::geteuid() as u32 }
        #[cfg(windows)] { 0 }
    };
    // zsh's `$fpath` is the canonical source for shell-installed completion
    // function paths, but probe for zsh on PATH first: without this, every
    // daemon launch pays the cost of a failed `Command::spawn("zsh")` on
    // Windows (no zsh) and minimal Linux images, masked by `unwrap_or_default`.
    let fpath = if crate::java::which("zsh").is_some() {
        capture_stdout("zsh", &["-c", "printf '%s\\n' $fpath"]).unwrap_or_default()
    } else {
        String::new()
    };
    let command_path = crate::java::which(name)
        .map(|path| path.to_string_lossy().into_owned())
        .unwrap_or_default();

    vec![
        format!("-Dbuild.id={}", config.build_id),
        format!("-Dethereal.name={}", name),
        format!("-Dethereal.user.id={}", uid),
        format!("-Dethereal.user.name={}", user_name),
        format!("-Dethereal.script={}", script.display()),
        "-Dethereal.payloadSize=0".to_string(),
        format!("-Dethereal.jarSize={}", jar_size),
        format!("-Dethereal.command={}", command_path),
        format!("-Dethereal.fpath={}", fpath.trim_end()),
        // Where a Burdock bootstrap reports its dependency downloads; see `progress.rs`.
        format!("-Dburdock.progress={}", progress_file.display()),
    ]
}

#[cfg(unix)]
fn detach(command: &mut Command) {
    use std::os::unix::process::CommandExt;
    unsafe {
        command.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }
}

#[cfg(unix)]
pub(crate) fn mark_stdio_non_inheritable() {}

#[cfg(windows)]
pub(crate) fn mark_stdio_non_inheritable() {
    use std::os::windows::io::AsRawHandle;
    use windows_sys::Win32::Foundation::{HANDLE, SetHandleInformation, HANDLE_FLAG_INHERIT};
    let handles: [HANDLE; 3] = [
        std::io::stdin().as_raw_handle() as HANDLE,
        std::io::stdout().as_raw_handle() as HANDLE,
        std::io::stderr().as_raw_handle() as HANDLE,
    ];
    for handle in handles {
        if !handle.is_null() {
            unsafe { SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0); }
        }
    }
}

#[cfg(windows)]
fn detach(command: &mut Command) {
    use std::os::windows::process::CommandExt;
    const DETACHED_PROCESS: u32 = 0x00000008;
    const CREATE_NEW_PROCESS_GROUP: u32 = 0x00000200;
    command.creation_flags(DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP);
}

fn capture_stdout(command: &str, args: &[&str]) -> Option<String> {
    let output = Command::new(command).args(args).output().ok()?;
    if !output.status.success() { return None; }
    Some(String::from_utf8_lossy(&output.stdout).to_string())
}
