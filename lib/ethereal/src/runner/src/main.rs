use std::env;
use std::ffi::OsString;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

mod config;
mod state;
mod java;
mod launch;
mod protocol;
mod signals;
mod tty;
mod uds;
mod update;
mod wrapper;

use protocol::ClientInfo;
use uds::UnixStream;

const FORWARD_BUFFER_SIZE: usize = 4096;
const TERMINATION_POLL: Duration = Duration::from_millis(50);
const STARTUP_FAILURE_EXIT_CODE: i32 = 2;
pub const WRAP_SENTINEL: &str = "{wrap-java}";

fn main() {
    // The runner re-invokes itself with this sentinel as argv[1] when launching
    // the daemon, so the JVM runs as a child of a process whose name matches the
    // client (since this binary IS the renamed launcher). Dispatch before any
    // other parsing — the wrapper has its own minimal argv contract.
    let raw: Vec<String> = env::args().collect();
    if raw.get(1).is_some_and(|arg| arg == WRAP_SENTINEL) {
        wrapper::run(&raw[2..]);
    }

    let (script, args, download) = parse_arguments();
    let name = script.file_name().map(|name| name.to_string_lossy().into_owned()).unwrap_or_default();
    let build_config = config::read_config();

    update::check_updates(&script, &args, &name);

    let base_dir = state::base_dir(&name);
    let _ = std::fs::create_dir_all(&base_dir);
    let build_file  = base_dir.join("build");
    let pid_file    = base_dir.join("pid");
    let socket_file = base_dir.join("socket");
    let fail_file   = base_dir.join("fail");

    // Non-interactive invocations (completions, admin) must not touch the TTY,
    // fork a stdin-forwarding thread, or install signal handlers — doing so
    // can steal input from the parent shell or trigger SIGTTOU when the
    // process runs in a background process group (e.g. under `< <(...)`).
    let interactive = !args.first().is_some_and(|arg| arg == "{completions}" || arg == "{admin}");

    state::backout(&fail_file, &pid_file, &name);
    state::check_state(&pid_file, &build_file, &socket_file, build_config.build_id);

    if !state::file_has_content(&pid_file) {
        let lock_path = base_dir.join("lock");
        match state::try_exclusive_lock(&lock_path) {
            Some(_lock) => {
                launch::launch(
                    &script, &name, &base_dir,
                    &build_file, &pid_file, &socket_file, &fail_file,
                    &build_config, download,
                );
            }
            None => {
                if !state::await_socket(&socket_file, 40) {
                    state::abort(&fail_file);
                    state::backout(&fail_file, &pid_file, &name);
                    std::process::exit(1);
                }
            }
        }
    }
    state::backout(&fail_file, &pid_file, &name);

    if !state::socket_alive(&socket_file) { std::process::exit(STARTUP_FAILURE_EXIT_CODE); }

    if !interactive {
        std::process::exit(run_non_interactive(&socket_file, &script, &args));
    }

    let saved_tty = tty::save_tty_state();
    tty::set_raw_mode();

    let info = ClientInfo::collect(&script, &args, tty::stdin_is_tty());
    let (main_socket, stderr_socket) = match connect_to_daemon(&socket_file, &info) {
        Ok(connections) => connections,
        Err(_) => {
            tty::restore_tty_state(&saved_tty);
            std::process::exit(STARTUP_FAILURE_EXIT_CODE);
        }
    };

    spawn_forwarder(
        std::io::stdin(),
        main_socket.try_clone().expect("clone main socket"),
        false,
    );
    let stdout_thread = spawn_forwarder(
        main_socket.try_clone().expect("clone main socket"),
        std::io::stdout(),
        true,
    );
    let stderr_thread = spawn_forwarder(stderr_socket, std::io::stderr(), true);

    let termination_flag = Arc::new(AtomicBool::new(false));
    signals::install(socket_file.clone(), info.pid, termination_flag.clone());

    // When SIGTERM is flagged, shut down the main socket so the stdout
    // forwarder unblocks immediately.
    let socket_for_shutdown = main_socket.try_clone().expect("clone main socket");
    let monitor_flag = termination_flag.clone();
    std::thread::spawn(move || {
        while !monitor_flag.load(Ordering::SeqCst) { std::thread::sleep(TERMINATION_POLL); }
        let _ = socket_for_shutdown.shutdown(std::net::Shutdown::Both);
    });

    let _ = stdout_thread.join();
    tty::restore_tty_state(&saved_tty);

    if termination_flag.load(Ordering::SeqCst) { std::process::exit(1); }
    let _ = stderr_thread.join();
    std::process::exit(protocol::terminate(&socket_file, info.pid));
}

fn parse_arguments() -> (PathBuf, Vec<String>, bool) {
    let raw: Vec<String> = env::args().collect();
    let executable = raw.first().cloned().unwrap_or_default();
    let mut download = false;
    let mut args: Vec<String> = Vec::with_capacity(raw.len().saturating_sub(1));
    for arg in raw.iter().skip(1) {
        if arg == "--download" { download = true; } else { args.push(arg.clone()); }
    }
    let script = std::fs::canonicalize(&executable)
        .or_else(|_| std::env::current_exe())
        .unwrap_or_else(|_| PathBuf::from(&executable));
    (script, args, download)
}

fn connect_to_daemon(socket_path: &Path, info: &ClientInfo)
-> io::Result<(UnixStream, UnixStream)> {
    let mut main_socket = UnixStream::connect(socket_path)?;
    protocol::send_init(&mut main_socket, info);
    let mut stderr_socket = UnixStream::connect(socket_path)?;
    protocol::send_stderr_request(&mut stderr_socket, info.pid);
    Ok((main_socket, stderr_socket))
}

fn spawn_forwarder(
    reader: impl Read + Send + 'static,
    writer: impl Write + Send + 'static,
    flush_each_chunk: bool,
) -> std::thread::JoinHandle<()> {
    std::thread::spawn(move || forward(reader, writer, flush_each_chunk))
}

fn forward(mut reader: impl Read, mut writer: impl Write, flush_each_chunk: bool) {
    let mut buffer = [0u8; FORWARD_BUFFER_SIZE];
    loop {
        match reader.read(&mut buffer) {
            Ok(0) | Err(_) => break,
            Ok(count) => {
                if writer.write_all(&buffer[..count]).is_err() { break; }
                if flush_each_chunk { let _ = writer.flush(); }
            }
        }
    }
    let _ = writer.flush();
}

fn run_non_interactive(socket_file: &Path, script: &Path, args: &[String]) -> i32 {
    debug_log(format!(
        "run_non_interactive socket={} args={:?}", socket_file.display(), args,
    ));

    let info = ClientInfo::collect(script, args, false);
    let (main_socket, stderr_socket) = match connect_to_daemon(socket_file, &info) {
        Ok(connections) => { debug_log("connected"); connections }
        Err(error) => {
            debug_log(format!("connect failed: {}", error));
            return STARTUP_FAILURE_EXIT_CODE;
        }
    };

    let stdout_thread = spawn_forwarder(main_socket, std::io::stdout(), true);
    let stderr_thread = spawn_forwarder(stderr_socket, std::io::stderr(), true);

    let _ = stdout_thread.join();
    debug_log("stdout joined");
    let _ = stderr_thread.join();
    debug_log("stderr joined; calling terminate");
    let exit_code = protocol::terminate(socket_file, info.pid);
    debug_log(format!("terminate returned {}", exit_code));
    exit_code
}

impl ClientInfo {
    pub fn collect(script: &Path, args: &[String], is_tty: bool) -> Self {
        let env: Vec<String> = env::vars_os().map(|(name, value)| {
            let mut entry = OsString::new();
            entry.push(&name);
            entry.push("=");
            entry.push(&value);
            entry.to_string_lossy().into_owned()
        }).collect();
        ClientInfo {
            pid: std::process::id(),
            user_id: user_info::uid(),
            user_name: user_info::username(),
            script: script.to_string_lossy().into_owned(),
            pwd: env::current_dir()
                .map(|path| path.to_string_lossy().into_owned())
                .unwrap_or_default(),
            args: args.to_vec(),
            env,
            is_tty,
        }
    }
}

mod user_info {
    #[cfg(unix)]
    pub fn uid() -> u32 { unsafe { libc::getuid() as u32 } }

    #[cfg(unix)]
    pub fn username() -> String {
        std::env::var("USER").or_else(|_| std::env::var("LOGNAME")).unwrap_or_default()
    }

    #[cfg(windows)]
    pub fn uid() -> u32 { 0 }

    #[cfg(windows)]
    pub fn username() -> String { std::env::var("USERNAME").unwrap_or_default() }
}

pub fn now_ms() -> u128 {
    SystemTime::now().duration_since(UNIX_EPOCH).map(|elapsed| elapsed.as_millis()).unwrap_or(0)
}

fn debug_log(message: impl AsRef<str>) {
    if std::env::var_os("ETHEREAL_DEBUG").is_none() { return; }
    let log_path = std::env::temp_dir().join("ethereal-launcher.log");
    if let Ok(mut log_file) = std::fs::OpenOptions::new().create(true).append(true).open(&log_path) {
        let _ = writeln!(log_file, "[{}] {}", std::process::id(), message.as_ref());
    }
}
