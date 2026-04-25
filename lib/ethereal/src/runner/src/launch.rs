use std::path::Path;
use std::process::{Command, Stdio};

use crate::config::BuildConfig;

pub fn launch(
    script: &Path,
    name: &str,
    base_dir: &Path,
    port_file: &Path,
    pid_file: &Path,
    fail_file: &Path,
    cfg: &BuildConfig,
    do_download: bool,
) {
    let java = match crate::java::find_java(cfg.java_min, cfg.java_pref, cfg.bundle, do_download) {
        Some(p) => p,
        None => {
            let invocation = script.to_string_lossy();
            crate::java::java_not_found_message(cfg.java_min, &invocation);
            crate::state::abort(fail_file);
            std::process::exit(1);
        }
    };

    let start_time = crate::now_ms();
    let jar_size = std::fs::metadata(script).map(|m| m.len()).unwrap_or(0);
    let user_name = std::env::var("USER").or_else(|_| std::env::var("USERNAME")).unwrap_or_default();
    let uid: u32 = {
        #[cfg(unix)] unsafe { libc::geteuid() as u32 }
        #[cfg(windows)] { 0 }
    };
    let fpath = run_capture("zsh", &["-c", "printf '%s\\n' $fpath"]).unwrap_or_default();
    let command_path = crate::java::which(name)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();

    let java_args = vec![
        format!("-Dbuild.id={}", cfg.build_id),
        format!("-Dethereal.startTime={}", start_time),
        format!("-Dethereal.name={}", name),
        format!("-Dethereal.user.id={}", uid),
        format!("-Dethereal.user.name={}", user_name),
        format!("-Dethereal.script={}", script.display()),
        "-Dethereal.payloadSize=0".to_string(),
        format!("-Dethereal.jarSize={}", jar_size),
        format!("-Dethereal.command={}", command_path),
        format!("-Dethereal.fpath={}", fpath.trim_end()),
    ];

    let log_file = base_dir.join("daemon.log");
    let log = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_file);

    let mut cmd = Command::new(&java);
    for a in &java_args { cmd.arg(a); }
    cmd.arg("-jar").arg(script);
    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::null());
    match log {
        Ok(f) => { cmd.stderr(Stdio::from(f)); }
        Err(_) => { cmd.stderr(Stdio::null()); }
    }

    detach(&mut cmd);

    let child = match cmd.spawn() {
        Ok(c) => c,
        Err(_) => {
            crate::state::abort(fail_file);
            std::process::exit(1);
        }
    };

    let _ = std::fs::write(pid_file, format!("{}\n", child.id()));

    let mut wait_count = 0;
    while !crate::state::file_has_content(port_file) && !fail_file.exists() && wait_count < 200 {
        crate::sleep_ms(50);
        wait_count += 1;
    }

    if !crate::state::file_has_content(port_file) {
        crate::state::abort(fail_file);
        crate::state::backout(fail_file, pid_file, name);
        std::process::exit(1);
    }

    crate::state::backout(fail_file, pid_file, name);
}

#[cfg(unix)]
fn detach(cmd: &mut Command) {
    use std::os::unix::process::CommandExt;
    unsafe {
        cmd.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }
}

#[cfg(windows)]
fn detach(cmd: &mut Command) {
    use std::os::windows::process::CommandExt;
    const DETACHED_PROCESS: u32 = 0x00000008;
    const CREATE_NEW_PROCESS_GROUP: u32 = 0x00000200;
    cmd.creation_flags(DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP);
}

fn run_capture(cmd: &str, args: &[&str]) -> Option<String> {
    let out = Command::new(cmd).args(args).output().ok()?;
    if !out.status.success() { return None; }
    Some(String::from_utf8_lossy(&out.stdout).to_string())
}
