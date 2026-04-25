use std::fs::{self, File, OpenOptions};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::Duration;

pub fn base_dir(name: &str) -> PathBuf {
    let home = std::env::var_os("HOME");
    let runtime = std::env::var_os("XDG_RUNTIME_DIR");
    let state = std::env::var_os("XDG_STATE_HOME");
    let base: PathBuf = match runtime {
        Some(r) => PathBuf::from(r),
        None => match state {
            Some(s) => PathBuf::from(s),
            None => match home {
                Some(h) => PathBuf::from(h).join(".local").join("state"),
                None => PathBuf::from("."),
            },
        },
    };
    base.join(name)
}

pub fn data_home() -> PathBuf {
    let home = std::env::var_os("HOME");
    let data = std::env::var_os("XDG_DATA_HOME");
    match data {
        Some(d) => PathBuf::from(d),
        None => match home {
            Some(h) => PathBuf::from(h).join(".local").join("share"),
            None => PathBuf::from("."),
        },
    }
}

pub fn file_has_content(p: &Path) -> bool {
    fs::metadata(p).map(|m| m.len() > 0).unwrap_or(false)
}

pub fn await_file(p: &Path, limit_100ms: u32) -> bool {
    let mut count = 0;
    while !file_has_content(p) && count < limit_100ms {
        std::thread::sleep(Duration::from_millis(100));
        count += 1;
    }
    file_has_content(p)
}

pub fn abort(fail_file: &Path) {
    let _ = File::create(fail_file);
}

pub fn backout(fail_file: &Path, pid_file: &Path, name: &str) {
    if let Ok(meta) = fs::metadata(fail_file) {
        let modified = meta.modified().ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_secs())
            .unwrap_or(0);
        let now = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs()).unwrap_or(0);
        if now.saturating_sub(modified) >= 2 {
            let _ = fs::remove_file(fail_file);
        } else if !file_has_content(pid_file) {
            eprintln!("\nThe {} daemon process failed to start.", name);
            eprintln!("Remove the file {} before trying again.", fail_file.display());
            std::process::exit(1);
        }
    }
}

pub fn read_pid(pid_file: &Path) -> Option<u32> {
    let mut s = String::new();
    File::open(pid_file).ok()?.read_to_string(&mut s).ok()?;
    s.trim().parse().ok()
}

pub fn read_port(port_file: &Path) -> Option<u16> {
    let mut s = String::new();
    File::open(port_file).ok()?.read_to_string(&mut s).ok()?;
    s.split_whitespace().next()?.parse().ok()
}

pub fn read_build_id(port_file: &Path) -> Option<u64> {
    let mut s = String::new();
    File::open(port_file).ok()?.read_to_string(&mut s).ok()?;
    let parts: Vec<&str> = s.split_whitespace().collect();
    parts.get(1)?.parse().ok()
}

pub fn process_alive(pid: u32) -> bool {
    #[cfg(unix)]
    unsafe { libc::kill(pid as libc::pid_t, 0) == 0 }
    #[cfg(windows)]
    unsafe {
        use windows_sys::Win32::System::Threading::{OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION};
        use windows_sys::Win32::Foundation::CloseHandle;
        let h = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid);
        if h.is_null() { false } else { CloseHandle(h); true }
    }
}

pub fn check_state(pid_file: &Path, port_file: &Path, build_id: u64) {
    if let Some(pid) = read_pid(pid_file) {
        if process_alive(pid) {
            if file_has_content(port_file) {
                if let Some(file_build) = read_build_id(port_file) {
                    if file_build != build_id && build_id != 0 {
                        let _ = fs::remove_file(pid_file);
                        let _ = fs::remove_file(port_file);
                        std::thread::sleep(Duration::from_millis(100));
                    }
                }
            } else if !await_file(port_file, 40) {
                // daemon may be mid-startup that failed
            }
        } else {
            let _ = fs::remove_file(pid_file);
            let _ = fs::remove_file(port_file);
        }
    } else if file_has_content(pid_file) {
        // corrupt pid file
        let _ = fs::remove_file(pid_file);
        let _ = fs::remove_file(port_file);
    } else {
        // pid file missing — any stale port file is from an abandoned daemon
        let _ = fs::remove_file(port_file);
    }
}

// --- Lock support ---

pub struct Lock {
    #[cfg(unix)]
    _file: File,
    #[cfg(windows)]
    _file: File,
}

pub fn try_exclusive_lock(lock_path: &Path) -> Option<Lock> {
    let file = OpenOptions::new().create(true).write(true).truncate(false).open(lock_path).ok()?;
    #[cfg(unix)]
    unsafe {
        use std::os::unix::io::AsRawFd;
        let fd = file.as_raw_fd();
        if libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) == 0 {
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
