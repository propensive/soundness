use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub fn find_java(minimum: u16, preferred: u16, bundle: &str, do_download: bool) -> Option<PathBuf> {
    if let Some(path) = which("java") {
        if check_version(&path, minimum) { return Some(path); }
    }

    let data_home = crate::state::data_home();
    let preferred_install = data_home.join("java").join(format!("{}-{}", preferred, bundle))
        .join("bin").join("java");
    if preferred_install.exists() && is_executable(&preferred_install) {
        return Some(preferred_install);
    }

    // update-alternatives --list java (Linux)
    if let Ok(output) = Command::new("update-alternatives").arg("--list").arg("java").output() {
        if output.status.success() {
            for line in String::from_utf8_lossy(&output.stdout).lines() {
                let candidate = PathBuf::from(line);
                if check_version(&candidate, minimum) { return Some(candidate); }
            }
        }
    }

    // /usr/libexec/java_home -v N (macOS)
    let version_arg = format!("-v{}", minimum);
    if let Ok(output) = Command::new("/usr/libexec/java_home").arg(&version_arg).output() {
        if output.status.success() {
            let java_home = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !java_home.is_empty() {
                let candidate = PathBuf::from(java_home).join("bin").join("java");
                if check_version(&candidate, minimum) { return Some(candidate); }
            }
        }
    }

    if do_download { return download(preferred, bundle); }
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
    let output = match Command::new(java).arg("-version").output() {
        Ok(output) => output,
        Err(_) => return false,
    };
    let combined = String::from_utf8_lossy(&output.stderr).to_string()
        + &String::from_utf8_lossy(&output.stdout);
    for line in combined.lines() {
        if !line.contains("version") { continue; }
        let Some(start) = line.find('"') else { continue; };
        let after_quote = &line[start + 1..];
        let Some(end) = after_quote.find('"') else { continue; };
        let version_text = &after_quote[..end];
        let major_text = version_text.split('.').next().unwrap_or("0");
        if let Ok(major) = major_text.parse::<u16>() {
            return major >= minimum;
        }
    }
    false
}

fn is_executable(path: &Path) -> bool {
    #[cfg(unix)]
    unsafe {
        use std::ffi::CString;
        let c_path = CString::new(path.as_os_str().to_string_lossy().as_bytes()).unwrap_or_default();
        libc::access(c_path.as_ptr(), libc::X_OK) == 0
    }
    #[cfg(windows)]
    { path.exists() }
}

pub fn which(command: &str) -> Option<PathBuf> {
    let path_var = std::env::var_os("PATH")?;
    for directory in std::env::split_paths(&path_var) {
        let candidate = directory.join(command);
        #[cfg(windows)]
        {
            let with_extension = candidate.with_extension("exe");
            if with_extension.exists() { return Some(with_extension); }
        }
        if is_executable(&candidate) { return Some(candidate); }
    }
    None
}

fn arch_label() -> &'static str {
    match std::env::consts::ARCH {
        "x86_64"  => "x64",
        "x86"     => "x86",
        "aarch64" | "arm64" => "aarch64",
        other     => other,
    }
}

fn os_label() -> &'static str {
    match std::env::consts::OS {
        "linux"   => "linux",
        "macos"   => "mac",
        "windows" => "windows",
        "solaris" => "solaris",
        other     => other,
    }
}

pub fn download(preferred: u16, bundle: &str) -> Option<PathBuf> {
    let data_home = crate::state::data_home();
    let java_home = data_home.join("java");
    let temp_root = data_home.join("tmp");
    let _ = std::fs::create_dir_all(&temp_root);
    let temp_dir = make_temp_dir(&temp_root)?;
    let url = format!(
        "https://api.adoptium.net/v3/binary/latest/{preferred}/ga/{os}/{arch}/{bundle}/hotspot/normal/eclipse",
        preferred = preferred, os = os_label(), arch = arch_label(), bundle = bundle,
    );

    let have_curl = which("curl").is_some();
    let have_wget = which("wget").is_some();
    if !have_curl && !have_wget {
        let _ = std::fs::remove_dir(&temp_dir);
        eprintln!("Could not download Java: neither curl nor wget is on the PATH.");
        return None;
    }

    eprintln!("Downloading Java {} from {}", preferred, url);
    // Pipe curl/wget directly into tar from Rust rather than via `sh -c`. The
    // shell pipeline form fails on Windows, where there is no `sh` on PATH but
    // `curl.exe` and `tar.exe` ship with the OS.
    let mut downloader = if have_curl {
        let mut command = Command::new("curl");
        command.args(["-sL", &url]);
        command
    } else {
        let mut command = Command::new("wget");
        command.args(["-q", "-O", "-", &url]);
        command
    };
    let mut down_child = downloader.stdout(Stdio::piped()).spawn().ok()?;
    let down_stdout = down_child.stdout.take()?;
    let tar_status = Command::new("tar")
        .arg("xz")
        .arg("-C")
        .arg(&temp_dir)
        .stdin(Stdio::from(down_stdout))
        .status();
    let down_status = down_child.wait();
    let success = matches!(&down_status, Ok(s) if s.success())
        && matches!(&tar_status, Ok(s) if s.success());
    if !success {
        eprintln!("The download failed.");
        let _ = std::fs::remove_dir_all(&temp_dir);
        return None;
    }
    eprintln!("Download complete");

    let release_file = find_release(&temp_dir)?;
    let release_dir = release_file.parent()?.to_path_buf();
    let full_version = read_java_version(&release_file)?;
    let target_dir = java_home.join(format!("{}-{}", full_version, bundle));
    let _ = std::fs::remove_dir_all(&target_dir);
    let _ = std::fs::create_dir_all(&java_home);
    std::fs::rename(&release_dir, &target_dir).ok()?;
    let _ = std::fs::remove_dir_all(&temp_dir);
    let symlink = java_home.join(format!("{}-{}", preferred, bundle));
    #[cfg(unix)] {
        let _ = std::fs::remove_file(&symlink);
        let _ = std::os::unix::fs::symlink(&target_dir, &symlink);
    }
    #[cfg(windows)] {
        let _ = std::fs::remove_dir(&symlink);
        let _ = std::os::windows::fs::symlink_dir(&target_dir, &symlink);
    }
    Some(target_dir.join("bin").join(if cfg!(windows) { "java.exe" } else { "java" }))
}

fn make_temp_dir(root: &Path) -> Option<PathBuf> {
    for _ in 0..6 {
        let nonce: u64 = (crate::now_ms() as u64) ^ std::process::id() as u64;
        let candidate = root.join(format!("eth-{}", nonce));
        if std::fs::create_dir(&candidate).is_ok() { return Some(candidate); }
    }
    None
}

fn find_release(root: &Path) -> Option<PathBuf> {
    walk(root, 6).into_iter().find(|entry| {
        entry.file_name().and_then(|name| name.to_str()) == Some("release")
    })
}

fn walk(root: &Path, depth: u32) -> Vec<PathBuf> {
    let mut result = Vec::new();
    if depth == 0 { return result; }
    if let Ok(entries) = std::fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                result.extend(walk(&path, depth - 1));
            } else {
                result.push(path);
            }
        }
    }
    result
}

fn read_java_version(release_file: &Path) -> Option<String> {
    let content = std::fs::read_to_string(release_file).ok()?;
    for line in content.lines() {
        if let Some(rest) = line.trim().strip_prefix("JAVA_VERSION=") {
            return Some(rest.trim_matches('"').to_string());
        }
    }
    None
}
