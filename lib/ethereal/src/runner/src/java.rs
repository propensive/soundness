use std::path::{Path, PathBuf};
use std::process::Command;

pub fn find_java(minimum: u16, preferred: u16, bundle: &str, do_download: bool) -> Option<PathBuf> {
    // PATH
    if let Some(p) = which("java") {
        if check_version(&p, minimum) { return Some(p); }
    }
    // $XDG_DATA_HOME/java/{preferred}-{bundle}/bin/java
    let data_home = crate::state::data_home();
    let default = data_home.join("java").join(format!("{}-{}", preferred, bundle)).join("bin").join("java");
    if default.exists() && is_exec(&default) { return Some(default); }

    // update-alternatives --list java (Linux)
    if let Ok(out) = Command::new("update-alternatives").arg("--list").arg("java").output() {
        if out.status.success() {
            for line in String::from_utf8_lossy(&out.stdout).lines() {
                let p = PathBuf::from(line);
                if check_version(&p, minimum) { return Some(p); }
            }
        }
    }

    // /usr/libexec/java_home -v N (macOS)
    let vstr = format!("-v{}", minimum);
    if let Ok(out) = Command::new("/usr/libexec/java_home").arg(&vstr).output() {
        if out.status.success() {
            let s = String::from_utf8_lossy(&out.stdout).trim().to_string();
            if !s.is_empty() {
                let p = PathBuf::from(s).join("bin").join("java");
                if check_version(&p, minimum) { return Some(p); }
            }
        }
    }

    if do_download {
        if let Some(p) = download(preferred, bundle) { return Some(p); }
    }

    None
}

pub fn java_not_found_message(minimum: u16, invocation: &str) {
    eprintln!("\nJava {} or later is required, but no suitable version of Java was found.", minimum);
    eprintln!("You can install java with one of the following commands:\n");
    if which("pacman").is_some() { eprintln!("  > pacman -S jre-openjdk"); }
    if which("emerge").is_some() { eprintln!("  > emerge dev-java/openjdk-{}", minimum); }
    if which("yum").is_some()    { eprintln!("  > yum install java-{}-openjdk", minimum); }
    if which("dnf").is_some()    { eprintln!("  > dnf install java-{}-openjdk", minimum); }
    if which("apt").is_some()    { eprintln!("  > apt update && apt install openjdk-{}-jre", minimum); }
    if which("zypper").is_some() { eprintln!("  > zypper install openjdk-{}-headless", minimum); }
    if which("brew").is_some()   { eprintln!("  > brew update && brew install java"); }
    eprintln!("  > {} --download\n", invocation);
}

pub fn check_version(java: &Path, minimum: u16) -> bool {
    let out = match Command::new(java).arg("-version").output() {
        Ok(o) => o,
        Err(_) => return false,
    };
    let all = String::from_utf8_lossy(&out.stderr).to_string() + &String::from_utf8_lossy(&out.stdout);
    for line in all.lines() {
        if line.contains("version") {
            if let Some(start) = line.find('"') {
                let after = &line[start + 1..];
                if let Some(end) = after.find('"') {
                    let v = &after[..end];
                    let major = v.split('.').next().unwrap_or("0");
                    if let Ok(n) = major.parse::<u16>() {
                        return n >= minimum;
                    }
                }
            }
        }
    }
    false
}

fn is_exec(p: &Path) -> bool {
    #[cfg(unix)]
    unsafe {
        use std::ffi::CString;
        let c = CString::new(p.as_os_str().to_string_lossy().as_bytes()).unwrap_or_default();
        libc::access(c.as_ptr(), libc::X_OK) == 0
    }
    #[cfg(windows)]
    { p.exists() }
}

pub fn which(cmd: &str) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let candidate = dir.join(cmd);
        #[cfg(windows)]
        {
            let exe = candidate.with_extension("exe");
            if exe.exists() { return Some(exe); }
        }
        if is_exec(&candidate) { return Some(candidate); }
    }
    None
}

fn arch_label() -> &'static str {
    let ma = std::env::consts::ARCH;
    match ma {
        "x86_64" => "x64",
        "x86"    => "x86",
        "aarch64" => "aarch64",
        "arm64" => "aarch64",
        _ => ma,
    }
}

fn os_label() -> &'static str {
    match std::env::consts::OS {
        "linux"   => "linux",
        "macos"   => "mac",
        "windows" => "windows",
        "solaris" => "solaris",
        other => other,
    }
}

pub fn download(preferred: u16, bundle: &str) -> Option<PathBuf> {
    let data_home = crate::state::data_home();
    let java_home = data_home.join("java");
    let tmp_root = data_home.join("tmp");
    let _ = std::fs::create_dir_all(&tmp_root);
    let tmp = mktempd(&tmp_root)?;
    let url = format!(
        "https://api.adoptium.net/v3/binary/latest/{preferred}/ga/{os}/{arch}/{bundle}/hotspot/normal/eclipse",
        preferred = preferred, os = os_label(), arch = arch_label(), bundle = bundle,
    );

    let have_curl = which("curl").is_some();
    let have_wget = which("wget").is_some();
    if !have_curl && !have_wget {
        let _ = std::fs::remove_dir(&tmp);
        eprintln!("Could not download Java: neither curl nor wget is on the PATH.");
        return None;
    }

    eprintln!("Downloading Java {} from {}", preferred, url);
    let pipe_cmd = if have_curl {
        format!("curl -sL \"{}\" | tar xz -C \"{}\"", url, tmp.display())
    } else {
        format!("wget -q -O - \"{}\" | tar xz -C \"{}\"", url, tmp.display())
    };
    let status = Command::new("sh").arg("-c").arg(&pipe_cmd).status().ok()?;
    if !status.success() {
        eprintln!("The download failed.");
        let _ = std::fs::remove_dir_all(&tmp);
        return None;
    }
    eprintln!("Download complete");

    let release_path = find_release(&tmp)?;
    let dir = release_path.parent()?.to_path_buf();
    let full_version = read_java_version(&release_path)?;
    let target_dir = java_home.join(format!("{}-{}", full_version, bundle));
    let _ = std::fs::remove_dir_all(&target_dir);
    let _ = std::fs::create_dir_all(&java_home);
    std::fs::rename(&dir, &target_dir).ok()?;
    let _ = std::fs::remove_dir_all(&tmp);
    let link = java_home.join(format!("{}-{}", preferred, bundle));
    #[cfg(unix)] {
        let _ = std::fs::remove_file(&link);
        let _ = std::os::unix::fs::symlink(&target_dir, &link);
    }
    #[cfg(windows)] {
        let _ = std::fs::remove_dir(&link);
        let _ = std::os::windows::fs::symlink_dir(&target_dir, &link);
    }
    let exe = target_dir.join("bin").join(if cfg!(windows) { "java.exe" } else { "java" });
    Some(exe)
}

fn mktempd(root: &Path) -> Option<PathBuf> {
    for _ in 0..6 {
        let nonce: u64 = (crate::now_ms() as u64) ^ std::process::id() as u64;
        let dir = root.join(format!("eth-{}", nonce));
        if std::fs::create_dir(&dir).is_ok() { return Some(dir); }
    }
    None
}

fn find_release(root: &Path) -> Option<PathBuf> {
    for entry in walk(root, 6) {
        if entry.file_name().and_then(|n| n.to_str()) == Some("release") { return Some(entry); }
    }
    None
}

fn walk(root: &Path, depth: u32) -> Vec<PathBuf> {
    let mut out = Vec::new();
    if depth == 0 { return out; }
    if let Ok(rd) = std::fs::read_dir(root) {
        for e in rd.flatten() {
            let p = e.path();
            if p.is_dir() {
                out.extend(walk(&p, depth - 1));
            } else {
                out.push(p);
            }
        }
    }
    out
}

fn read_java_version(release: &Path) -> Option<String> {
    let content = std::fs::read_to_string(release).ok()?;
    for line in content.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("JAVA_VERSION=") {
            let rest = rest.trim_matches('"');
            return Some(rest.to_string());
        }
    }
    None
}
