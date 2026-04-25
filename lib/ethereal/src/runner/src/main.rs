use std::env;
use std::ffi::OsString;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

mod config;
mod state;
mod java;
mod launch;
mod protocol;
mod signals;
mod tty;
mod update;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut download_flag = false;
    let mut filtered: Vec<String> = Vec::with_capacity(args.len());
    filtered.push(args[0].clone());
    for a in &args[1..] {
        if a == "--download" { download_flag = true; } else { filtered.push(a.clone()); }
    }

    let script = std::fs::canonicalize(&filtered[0])
        .or_else(|_| std::env::current_exe())
        .unwrap_or_else(|_| PathBuf::from(&filtered[0]));

    let name = script.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default();
    let cfg = config::read_config();

    update::check_updates(&script, &filtered[1..], &name);

    let base_dir = state::base_dir(&name);
    let _ = std::fs::create_dir_all(&base_dir);
    let port_file = base_dir.join("port");
    let pid_file = base_dir.join("pid");
    let fail_file = base_dir.join("fail");

    // Non-interactive invocations (completions, admin) must not touch the TTY,
    // fork a stdin-forwarding thread, or install signal handlers — doing so
    // can steal input from the parent shell or trigger SIGTTOU when the
    // process runs in a background process group (e.g. under `< <(...)`).
    let non_interactive = filtered.get(1).is_some_and(|a| a == "{completions}" || a == "{admin}");

    state::backout(&fail_file, &pid_file, &name);
    state::check_state(&pid_file, &port_file, cfg.build_id);

    if !state::file_has_content(&pid_file) {
        let _ = std::fs::create_dir_all(&base_dir);
        let lock_path = base_dir.join("lock");
        match state::try_exclusive_lock(&lock_path) {
            Some(_lock) => {
                launch::launch(&script, &name, &base_dir, &port_file, &pid_file, &fail_file, &cfg, download_flag);
            }
            None => {
                if !state::await_file(&port_file, 40) {
                    state::abort(&fail_file);
                    state::backout(&fail_file, &pid_file, &name);
                    std::process::exit(1);
                }
            }
        }
    }
    state::backout(&fail_file, &pid_file, &name);

    let port = match state::read_port(&port_file) {
        Some(p) => p,
        None => std::process::exit(2),
    };

    if non_interactive {
        run_non_interactive(port, &script, &filtered);
        return;
    }

    let tty_state = tty::save_tty_state();
    tty::set_raw_mode();

    let my_pid = std::process::id();
    let user_id = user_info::uid();
    let user_name = user_info::username();
    let is_tty = tty::stdin_is_tty();

    let pwd = env::current_dir().map(|p| p.to_string_lossy().into_owned()).unwrap_or_default();

    let mut main_conn = match TcpStream::connect(("127.0.0.1", port)) {
        Ok(s) => s,
        Err(_) => {
            tty::restore_tty_state(&tty_state);
            std::process::exit(2);
        }
    };

    let env_pairs: Vec<String> = env::vars_os().map(|(k, v)| {
        let mut s = OsString::new();
        s.push(&k); s.push("="); s.push(&v);
        s.to_string_lossy().into_owned()
    }).collect();

    protocol::send_init(
        &mut main_conn,
        is_tty,
        my_pid,
        user_id,
        &user_name,
        &script.to_string_lossy(),
        &pwd,
        &filtered[1..],
        &env_pairs,
    );

    // Open stderr channel (mandatory)
    let mut stderr_conn = match TcpStream::connect(("127.0.0.1", port)) {
        Ok(s) => s,
        Err(_) => {
            tty::restore_tty_state(&tty_state);
            std::process::exit(2);
        }
    };
    protocol::send_stderr_request(&mut stderr_conn, my_pid);

    // Forward stdin -> TCP
    let mut main_for_stdin = main_conn.try_clone().expect("clone socket");
    let stdin_thread = std::thread::spawn(move || {
        let mut stdin = std::io::stdin();
        let mut buf = [0u8; 4096];
        loop {
            match stdin.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => { if main_for_stdin.write_all(&buf[..n]).is_err() { break; } }
                Err(_) => break,
            }
        }
        let _ = main_for_stdin.flush();
    });

    // Forward TCP -> stdout
    let mut main_for_stdout = main_conn.try_clone().expect("clone socket");
    let stdout_thread = std::thread::spawn(move || {
        let mut stdout = std::io::stdout();
        let mut buf = [0u8; 4096];
        loop {
            match main_for_stdout.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => { if stdout.write_all(&buf[..n]).is_err() { break; } let _ = stdout.flush(); }
                Err(_) => break,
            }
        }
    });

    // Forward stderr TCP -> local stderr
    let mut stderr_for_pipe = stderr_conn.try_clone().expect("clone stderr socket");
    let stderr_thread = std::thread::spawn(move || {
        let mut stderr = std::io::stderr();
        let mut buf = [0u8; 4096];
        loop {
            match stderr_for_pipe.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => { if stderr.write_all(&buf[..n]).is_err() { break; } let _ = stderr.flush(); }
                Err(_) => break,
            }
        }
    });

    // Signal forwarding
    let term_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let term_flag_clone = term_flag.clone();
    signals::install(port, my_pid, term_flag_clone);

    // Monitor thread: when SIGTERM is flagged, shut down the main socket so
    // the stdout forwarder unblocks immediately.
    let main_for_shutdown = main_conn.try_clone().expect("clone socket");
    let term_flag_monitor = term_flag.clone();
    std::thread::spawn(move || {
        while !term_flag_monitor.load(std::sync::atomic::Ordering::SeqCst) {
            std::thread::sleep(Duration::from_millis(50));
        }
        let _ = main_for_shutdown.shutdown(std::net::Shutdown::Both);
    });

    // Wait for stdout stream to close (daemon done)
    let _ = stdout_thread.join();

    // Stop stdin thread (it may be blocked on read — best effort)
    let _ = stdin_thread; // detached

    tty::restore_tty_state(&tty_state);

    if term_flag.load(std::sync::atomic::Ordering::SeqCst) {
        // Terminated via SIGTERM
        let _ = stderr_thread;
        std::process::exit(1);
    } else {
        // Normal exit path: ask daemon for exit code
        let _ = stderr_thread.join();
        let code = protocol::terminate(port, my_pid);
        std::process::exit(code);
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
    pub fn username() -> String {
        std::env::var("USERNAME").unwrap_or_default()
    }
}

pub fn now_ms() -> u128 {
    SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_millis()).unwrap_or(0)
}

pub fn sleep_ms(ms: u64) { std::thread::sleep(Duration::from_millis(ms)); }

fn dbg(msg: &str) {
    if std::env::var_os("ETHEREAL_DEBUG").is_some() {
        if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open("/tmp/ethereal-launcher.log") {
            use std::io::Write as _;
            let _ = writeln!(f, "[{}] {}", std::process::id(), msg);
        }
    }
}

fn run_non_interactive(port: u16, script: &PathBuf, filtered: &[String]) {
    dbg(&format!("run_non_interactive port={} args={:?}", port, &filtered[1..]));
    let my_pid = std::process::id();
    let user_id = user_info::uid();
    let user_name = user_info::username();
    let pwd = env::current_dir().map(|p| p.to_string_lossy().into_owned()).unwrap_or_default();

    let mut main_conn = match TcpStream::connect(("127.0.0.1", port)) {
        Ok(s) => s,
        Err(e) => { dbg(&format!("main_conn connect fail: {}", e)); std::process::exit(2); }
    };
    dbg("main_conn connected");

    let env_pairs: Vec<String> = env::vars_os().map(|(k, v)| {
        let mut s = OsString::new();
        s.push(&k); s.push("="); s.push(&v);
        s.to_string_lossy().into_owned()
    }).collect();

    protocol::send_init(
        &mut main_conn,
        false,
        my_pid,
        user_id,
        &user_name,
        &script.to_string_lossy(),
        &pwd,
        &filtered[1..],
        &env_pairs,
    );
    dbg("send_init done");

    let mut stderr_conn = match TcpStream::connect(("127.0.0.1", port)) {
        Ok(s) => s,
        Err(e) => { dbg(&format!("stderr_conn connect fail: {}", e)); std::process::exit(2); }
    };
    protocol::send_stderr_request(&mut stderr_conn, my_pid);
    dbg("stderr_conn sent");

    let mut main_for_stdout = main_conn.try_clone().expect("clone socket");
    let stdout_thread = std::thread::spawn(move || {
        let mut stdout = std::io::stdout();
        let mut buf = [0u8; 4096];
        let mut total = 0usize;
        loop {
            match main_for_stdout.read(&mut buf) {
                Ok(0) => { dbg(&format!("stdout_thread EOF, total={}", total)); break; }
                Ok(n) => { total += n; if stdout.write_all(&buf[..n]).is_err() { break; } let _ = stdout.flush(); }
                Err(e) => { dbg(&format!("stdout_thread err: {}", e)); break; }
            }
        }
    });

    let mut stderr_for_pipe = stderr_conn.try_clone().expect("clone stderr socket");
    let stderr_thread = std::thread::spawn(move || {
        let mut stderr = std::io::stderr();
        let mut buf = [0u8; 4096];
        loop {
            match stderr_for_pipe.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => { if stderr.write_all(&buf[..n]).is_err() { break; } let _ = stderr.flush(); }
                Err(_) => break,
            }
        }
    });

    dbg("waiting for stdout_thread");
    let _ = stdout_thread.join();
    dbg("stdout_thread joined");
    let _ = stderr_thread.join();
    dbg("stderr_thread joined; calling terminate");
    let code = protocol::terminate(port, my_pid);
    dbg(&format!("terminate returned {}", code));
    std::process::exit(code);
}
