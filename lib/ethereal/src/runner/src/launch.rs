use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Duration;

use crate::config::BuildConfig;

const STARTUP_POLL: Duration = Duration::from_millis(50);
const STARTUP_MAX_ATTEMPTS: u32 = 200;
const BUILD_FILE_GRACE_ATTEMPTS: u32 = 20;

pub fn launch(
    script: &Path,
    name: &str,
    base_dir: &Path,
    build_file: &Path,
    pid_file: &Path,
    socket_file: &Path,
    fail_file: &Path,
    config: &BuildConfig,
    download: bool,
) {
    let java = match crate::java::find_java(config.java_min, config.java_pref, config.bundle, download) {
        Some(path) => path,
        None => {
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
    for argument in build_java_arguments(script, name, config) { command.arg(argument); }
    command.arg("-jar").arg(script);
    command.stdin(Stdio::null());
    command.stdout(Stdio::null());
    match std::fs::OpenOptions::new().create(true).append(true).open(base_dir.join("daemon.log")) {
        Ok(log) => { command.stderr(Stdio::from(log)); }
        Err(_)  => { command.stderr(Stdio::null()); }
    }

    detach(&mut command);

    let child = match command.spawn() {
        Ok(child) => child,
        Err(_) => {
            crate::state::abort(fail_file);
            std::process::exit(1);
        }
    };

    let _ = std::fs::write(pid_file, format!("{}\n", child.id()));

    let mut attempts = 0;
    while !crate::state::socket_ready(socket_file)
        && !fail_file.exists()
        && attempts < STARTUP_MAX_ATTEMPTS
    {
        std::thread::sleep(STARTUP_POLL);
        attempts += 1;
    }

    if !crate::state::socket_ready(socket_file) {
        crate::state::abort(fail_file);
        crate::state::backout(fail_file, pid_file, name);
        std::process::exit(1);
    }

    // The daemon writes the build-id file shortly after binding the socket.
    // We don't need it to connect, but staleness checks on later invocations do.
    let _ = crate::state::await_file(build_file, BUILD_FILE_GRACE_ATTEMPTS);

    crate::state::backout(fail_file, pid_file, name);
}

fn build_java_arguments(script: &Path, name: &str, config: &BuildConfig) -> Vec<String> {
    let start_time = crate::now_ms();
    let jar_size = std::fs::metadata(script).map(|metadata| metadata.len()).unwrap_or(0);
    let user_name = std::env::var("USER").or_else(|_| std::env::var("USERNAME")).unwrap_or_default();
    let uid: u32 = {
        #[cfg(unix)] unsafe { libc::geteuid() as u32 }
        #[cfg(windows)] { 0 }
    };
    let fpath = capture_stdout("zsh", &["-c", "printf '%s\\n' $fpath"]).unwrap_or_default();
    let command_path = crate::java::which(name)
        .map(|path| path.to_string_lossy().into_owned())
        .unwrap_or_default();

    vec![
        format!("-Dbuild.id={}", config.build_id),
        format!("-Dethereal.startTime={}", start_time),
        format!("-Dethereal.name={}", name),
        format!("-Dethereal.user.id={}", uid),
        format!("-Dethereal.user.name={}", user_name),
        format!("-Dethereal.script={}", script.display()),
        "-Dethereal.payloadSize=0".to_string(),
        format!("-Dethereal.jarSize={}", jar_size),
        format!("-Dethereal.command={}", command_path),
        format!("-Dethereal.fpath={}", fpath.trim_end()),
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
