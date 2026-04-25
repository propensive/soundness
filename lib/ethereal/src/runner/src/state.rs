use std::fs::{self, File, OpenOptions};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

use crate::uds::UnixStream;

const POLL_INTERVAL: Duration = Duration::from_millis(100);

pub fn base_dir(name: &str) -> PathBuf {
    let runtime = std::env::var_os("XDG_RUNTIME_DIR").map(PathBuf::from);
    let state = std::env::var_os("XDG_STATE_HOME").map(PathBuf::from);
    let home = std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".local").join("state"));
    let base = runtime.or(state).or(home).unwrap_or_else(|| PathBuf::from("."));
    base.join(name)
}

pub fn data_home() -> PathBuf {
    if let Some(dir) = std::env::var_os("XDG_DATA_HOME") { return PathBuf::from(dir); }
    if let Some(home) = std::env::var_os("HOME") {
        return PathBuf::from(home).join(".local").join("share");
    }
    PathBuf::from(".")
}

pub fn file_has_content(path: &Path) -> bool {
    fs::metadata(path).map(|metadata| metadata.len() > 0).unwrap_or(false)
}

#[cfg(unix)]
pub fn socket_ready(path: &Path) -> bool {
    use std::os::unix::fs::FileTypeExt;
    fs::metadata(path).map(|metadata| metadata.file_type().is_socket()).unwrap_or(false)
}

#[cfg(windows)]
pub fn socket_ready(path: &Path) -> bool {
    // On Windows AF_UNIX appears as a regular file/reparse point; presence
    // is the strongest portable signal.
    fs::metadata(path).is_ok()
}

// A stale socket file from a dead daemon will pass `socket_ready` but reject
// connections. Probe it to confirm a daemon is actually listening.
pub fn socket_alive(path: &Path) -> bool {
    if !socket_ready(path) { return false; }
    UnixStream::connect(path).is_ok()
}

fn poll_until(path: &Path, max_attempts: u32, ready: impl Fn(&Path) -> bool) -> bool {
    let mut attempts = 0;
    while !ready(path) && attempts < max_attempts {
        std::thread::sleep(POLL_INTERVAL);
        attempts += 1;
    }
    ready(path)
}

pub fn await_file(path: &Path, max_attempts: u32) -> bool {
    poll_until(path, max_attempts, file_has_content)
}

pub fn await_socket(path: &Path, max_attempts: u32) -> bool {
    poll_until(path, max_attempts, socket_ready)
}

pub fn abort(fail_file: &Path) {
    let _ = File::create(fail_file);
}

pub fn backout(fail_file: &Path, pid_file: &Path, name: &str) {
    let metadata = match fs::metadata(fail_file) {
        Ok(metadata) => metadata,
        Err(_) => return,
    };
    let modified = metadata.modified().ok()
        .and_then(|time| time.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|elapsed| elapsed.as_secs())
        .unwrap_or(0);
    let now = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)
        .map(|elapsed| elapsed.as_secs())
        .unwrap_or(0);
    if now.saturating_sub(modified) >= 2 {
        let _ = fs::remove_file(fail_file);
    } else if !file_has_content(pid_file) {
        eprintln!("\nThe {} daemon process failed to start.", name);
        eprintln!("Remove the file {} before trying again.", fail_file.display());
        std::process::exit(1);
    }
}

pub fn read_pid(pid_file: &Path) -> Option<u32> {
    let mut content = String::new();
    File::open(pid_file).ok()?.read_to_string(&mut content).ok()?;
    content.trim().parse().ok()
}

pub fn read_build_id(build_file: &Path) -> Option<u64> {
    let mut content = String::new();
    File::open(build_file).ok()?.read_to_string(&mut content).ok()?;
    content.split_whitespace().next()?.parse().ok()
}

pub fn process_alive(pid: u32) -> bool {
    #[cfg(unix)]
    unsafe { libc::kill(pid as libc::pid_t, 0) == 0 }
    #[cfg(windows)]
    unsafe {
        use windows_sys::Win32::System::Threading::{OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION};
        use windows_sys::Win32::Foundation::CloseHandle;
        let handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid);
        if handle.is_null() { false } else { CloseHandle(handle); true }
    }
}

pub fn check_state(pid_file: &Path, build_file: &Path, socket_file: &Path, build_id: u64) {
    fn clear_daemon_files(build_file: &Path, socket_file: &Path) {
        let _ = fs::remove_file(build_file);
        let _ = fs::remove_file(socket_file);
    }

    let Some(pid) = read_pid(pid_file) else {
        if file_has_content(pid_file) { let _ = fs::remove_file(pid_file); }
        clear_daemon_files(build_file, socket_file);
        return;
    };

    // `process_alive` can be misled by PID reuse — a stale daemon's PID may have
    // been recycled by an unrelated process. Probing the socket is the
    // authoritative liveness check: a real daemon accepts connections.
    if !process_alive(pid) || (socket_ready(socket_file) && !socket_alive(socket_file)) {
        let _ = fs::remove_file(pid_file);
        clear_daemon_files(build_file, socket_file);
        return;
    }

    if !file_has_content(build_file) {
        // Daemon may be mid-startup; give it a moment to finish writing the build file.
        await_file(build_file, 40);
        return;
    }

    let Some(file_build_id) = read_build_id(build_file) else { return; };
    if file_build_id != build_id && build_id != 0 {
        let _ = fs::remove_file(pid_file);
        clear_daemon_files(build_file, socket_file);
        std::thread::sleep(POLL_INTERVAL);
    }
}

pub struct Lock {
    _file: File,
}

pub fn try_exclusive_lock(lock_path: &Path) -> Option<Lock> {
    let file = OpenOptions::new().create(true).write(true).truncate(false).open(lock_path).ok()?;
    #[cfg(unix)]
    unsafe {
        use std::os::unix::io::AsRawFd;
        if libc::flock(file.as_raw_fd(), libc::LOCK_EX | libc::LOCK_NB) == 0 {
            Some(Lock { _file: file })
        } else {
            None
        }
    }
    #[cfg(windows)]
    {
        use std::os::windows::io::AsRawHandle;
        use windows_sys::Win32::Storage::FileSystem::{
            LockFileEx, LOCKFILE_EXCLUSIVE_LOCK, LOCKFILE_FAIL_IMMEDIATELY,
        };
        let handle = file.as_raw_handle() as windows_sys::Win32::Foundation::HANDLE;
        let mut overlapped: windows_sys::Win32::System::IO::OVERLAPPED = unsafe { std::mem::zeroed() };
        let ok = unsafe {
            LockFileEx(
                handle,
                LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY,
                0, !0, !0, &mut overlapped,
            )
        };
        if ok != 0 { Some(Lock { _file: file }) } else { None }
    }
}
