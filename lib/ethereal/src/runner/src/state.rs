use std::fs::{self, File, OpenOptions};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

use crate::uds::UnixStream;

const POLL_INTERVAL: Duration = Duration::from_millis(100);

// Must match the Scala daemon's view of the base directory (see
// `ethereal_core.scala`, which uses `Directories.runtimeDir.or(Directories.stateHome)`),
// otherwise the launcher polls one socket path while the JVM binds another and
// startup silently times out.
pub fn base_dir(name: &str) -> PathBuf {
    #[cfg(windows)]
    let base = local_app_data().join("Temp");
    #[cfg(unix)]
    let base = {
        let runtime = std::env::var_os("XDG_RUNTIME_DIR").map(PathBuf::from);
        let state = std::env::var_os("XDG_STATE_HOME").map(PathBuf::from);
        let home = std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".local").join("state"));
        runtime.or(state).or(home).unwrap_or_else(|| PathBuf::from("."))
    };
    base.join(name)
}

pub fn data_home() -> PathBuf {
    #[cfg(windows)]
    { local_app_data() }
    #[cfg(unix)]
    {
        if let Some(dir) = std::env::var_os("XDG_DATA_HOME") { return PathBuf::from(dir); }
        if let Some(home) = std::env::var_os("HOME") {
            return PathBuf::from(home).join(".local").join("share");
        }
        PathBuf::from(".")
    }
}

#[cfg(windows)]
fn local_app_data() -> PathBuf {
    if let Some(dir) = std::env::var_os("LOCALAPPDATA") { return PathBuf::from(dir); }
    if let Some(profile) = std::env::var_os("USERPROFILE") {
        return PathBuf::from(profile).join("AppData").join("Local");
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

const LOG_TAIL_LINES: usize = 20;

// Every startup failure below used to exit silently: `backout`'s message can only fire while the
// pid file is empty, and `launch` writes the wrapper's pid into it before any of the polling
// failure paths run, so nothing was ever printed. Meanwhile the JVM's own account of what went
// wrong sat unread in `daemon.log`. That is how a JAR the JVM would not open (#1680) presented
// itself as a flake. An *empty* log is itself a diagnosis, so say so rather than staying quiet.
pub fn report_failure(base_dir: &Path, name: &str, reason: &str) {
    eprintln!("\nThe {name} daemon failed to start: {reason}.");
    eprintln!("Its state directory is {}", base_dir.display());
    let log = base_dir.join("daemon.log");

    match fs::read_to_string(&log) {
        Ok(text) if !text.trim().is_empty() => {
            eprintln!("The last output in {} was:", log.display());
            let lines: Vec<&str> = text.lines().collect();
            for line in lines.iter().skip(lines.len().saturating_sub(LOG_TAIL_LINES)) {
                eprintln!("  {line}");
            }
        }
        Ok(_) => eprintln!("{} is empty: the JVM died before it could say why.", log.display()),
        Err(_) => eprintln!("{} could not be read.", log.display()),
    }
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

// The daemon records the launcher it was started from as whitespace-separated fields:
// `<buildId> <size> <mtimeMillis> <sha256hex>`. The first field is kept for launchers
// that predate the content-based staleness check; this launcher ignores it. Later
// fields are absent when an old daemon wrote the file, or when the daemon was started
// without a launcher (plain `java -jar`) — either way there is nothing to compare.
pub struct BuildRecord {
    #[allow(dead_code)] // parsed to validate the format; only tests read it back
    pub build_id: u64,
    pub size: Option<u64>,
    pub mtime_ms: Option<u64>,
    pub sha256: Option<String>,
}

pub fn read_build_record(build_file: &Path) -> Option<BuildRecord> {
    let mut content = String::new();
    File::open(build_file).ok()?.read_to_string(&mut content).ok()?;
    let mut fields = content.split_whitespace();
    let build_id = fields.next()?.parse().ok()?;
    let size = fields.next().and_then(|field| field.parse().ok());
    let mtime_ms = fields.next().and_then(|field| field.parse().ok());
    let sha256 = fields.next().map(str::to_owned);
    Some(BuildRecord { build_id, size, mtime_ms, sha256 })
}

pub fn mtime_millis(metadata: &fs::Metadata) -> Option<u64> {
    metadata.modified().ok()?
        .duration_since(SystemTime::UNIX_EPOCH).ok()
        .map(|elapsed| elapsed.as_millis() as u64)
}

pub fn hash_file(path: &Path) -> Option<String> {
    use sha2::{Digest, Sha256};
    let mut file = File::open(path).ok()?;
    let mut hasher = Sha256::new();
    std::io::copy(&mut file, &mut hasher).ok()?;
    let digest = hasher.finalize();
    let mut hex = String::with_capacity(digest.len()*2);
    for byte in digest { hex.push_str(&format!("{byte:02x}")); }
    Some(hex)
}

// Tiered by cost: size (already statted), then mtime, then a full-content hash. The
// hash runs only for a metadata-only change such as `touch`, so the common paths never
// read the launcher's content. Any doubt — unreadable script, missing fields — resolves
// to "not stale", which is the pre-existing behaviour.
pub fn is_stale(
    record: &BuildRecord,
    current_size: Option<u64>,
    current_mtime_ms: Option<u64>,
    current_hash: impl FnOnce() -> Option<String>,
) -> bool {
    let (Some(recorded_size), Some(size)) = (record.size, current_size) else { return false };
    if recorded_size != size { return true; }
    match (record.mtime_ms, current_mtime_ms) {
        (Some(recorded), Some(current)) if recorded != current => (),
        _ => return false,
    }
    match (record.sha256.as_deref(), current_hash().as_deref()) {
        (Some(recorded), Some(current)) => recorded != current,
        _ => false,
    }
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

pub fn check_state(pid_file: &Path, build_file: &Path, socket_file: &Path, script: &Path) {
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

    // A build id is no substitute for the content check — most applications never set
    // one, and the placeholder that used to ship on the classpath made every build
    // "build 1", so a rebuilt launcher never displaced its stale daemon (#1836).
    let Some(record) = read_build_record(build_file) else { return; };
    let metadata = fs::metadata(script).ok();
    let size = metadata.as_ref().map(|metadata| metadata.len());
    let mtime = metadata.as_ref().and_then(mtime_millis);

    if is_stale(&record, size, mtime, || hash_file(script)) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn temp_file(name: &str, content: &str) -> PathBuf {
        let path = std::env::temp_dir().join(format!("ethereal-state-test-{}-{name}", std::process::id()));
        let mut file = File::create(&path).unwrap();
        file.write_all(content.as_bytes()).unwrap();
        path
    }

    fn record(size: Option<u64>, mtime_ms: Option<u64>, sha256: Option<&str>) -> BuildRecord {
        BuildRecord { build_id: 1, size, mtime_ms, sha256: sha256.map(str::to_owned) }
    }

    #[test]
    fn record_with_all_fields() {
        let path = temp_file("full", "7 1234 99999 abc123\n");
        let record = read_build_record(&path).expect("record");
        let _ = fs::remove_file(&path);
        assert_eq!(record.build_id, 7);
        assert_eq!(record.size, Some(1234));
        assert_eq!(record.mtime_ms, Some(99999));
        assert_eq!(record.sha256.as_deref(), Some("abc123"));
    }

    #[test]
    fn record_with_only_build_id() {
        let path = temp_file("old", "42");
        let record = read_build_record(&path).expect("record");
        let _ = fs::remove_file(&path);
        assert_eq!(record.build_id, 42);
        assert_eq!(record.size, None);
        assert_eq!(record.mtime_ms, None);
        assert_eq!(record.sha256, None);
    }

    #[test]
    fn record_garbage_and_empty() {
        let garbage = temp_file("garbage", "not-a-number 12");
        assert!(read_build_record(&garbage).is_none());
        let _ = fs::remove_file(&garbage);
        let empty = temp_file("empty", "");
        assert!(read_build_record(&empty).is_none());
        let _ = fs::remove_file(&empty);
    }

    #[test]
    fn size_mismatch_is_stale_without_hashing() {
        let stale = is_stale(&record(Some(100), Some(5), Some("aa")), Some(200), Some(5),
                             || panic!("hash must not run on a size mismatch"));
        assert!(stale);
    }

    #[test]
    fn size_and_mtime_match_is_fresh_without_hashing() {
        let stale = is_stale(&record(Some(100), Some(5), Some("aa")), Some(100), Some(5),
                             || panic!("hash must not run when size and mtime match"));
        assert!(!stale);
    }

    #[test]
    fn mtime_mismatch_with_matching_hash_is_fresh() {
        let stale = is_stale(&record(Some(100), Some(5), Some("aa")), Some(100), Some(6),
                             || Some("aa".to_owned()));
        assert!(!stale);
    }

    #[test]
    fn mtime_mismatch_with_differing_hash_is_stale() {
        let stale = is_stale(&record(Some(100), Some(5), Some("aa")), Some(100), Some(6),
                             || Some("bb".to_owned()));
        assert!(stale);
    }

    #[test]
    fn old_format_record_is_fresh() {
        assert!(!is_stale(&record(None, None, None), Some(100), Some(5), || None));
    }

    #[test]
    fn unreadable_script_is_fresh() {
        assert!(!is_stale(&record(Some(100), Some(5), Some("aa")), None, None, || None));
    }

    #[test]
    fn hash_file_hashes_content() {
        let path = temp_file("hash", "abc");
        let hash = hash_file(&path).expect("hash");
        let _ = fs::remove_file(&path);
        // SHA-256 of "abc", a fixed test vector.
        assert_eq!(hash, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    }
}
