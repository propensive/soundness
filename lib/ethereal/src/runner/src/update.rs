use std::path::Path;

pub fn check_updates(script: &Path, args: &[String], name: &str) {
    let data_home = crate::state::data_home();
    let pending = data_home.join(name).join(".pending");
    crate::debug!("update: pending={}", pending.display());
    let metadata = match std::fs::metadata(&pending) {
        Ok(m) => m,
        Err(e) => { crate::debug!("update: pending metadata err: {}", e); return; },
    };
    crate::debug!("update: pending size={}", metadata.len());
    if metadata.len() == 0 { return; }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perm = metadata.permissions();
        perm.set_mode(0o755);
        let _ = std::fs::set_permissions(&pending, perm);
    }

    let old = data_home.join(format!("{}.old", name));
    crate::debug!("update: removing prior old={}", old.display());
    let _ = std::fs::remove_file(&old);
    crate::debug!("update: renaming script={} -> old={}", script.display(), old.display());
    if let Err(e) = std::fs::rename(script, &old) {
        crate::debug!("update: rename(script -> old) failed: {}", e);
        return;
    }
    crate::debug!("update: renaming pending={} -> script={}", pending.display(), script.display());
    if let Err(e) = std::fs::rename(&pending, script) {
        crate::debug!("update: rename(pending -> script) failed: {}", e);
        let _ = std::fs::rename(&old, script);
        return;
    }
    crate::debug!("update: swap complete; re-execing");

    #[cfg(unix)]
    unsafe {
        use std::ffi::CString;
        let script_c = CString::new(script.as_os_str().to_string_lossy().as_bytes()).unwrap();
        let mut argv: Vec<*const libc::c_char> = Vec::with_capacity(args.len() + 2);
        argv.push(script_c.as_ptr());
        let arg_cstrs: Vec<CString> = args.iter()
            .map(|a| CString::new(a.as_bytes()).unwrap())
            .collect();
        for a in &arg_cstrs { argv.push(a.as_ptr()); }
        argv.push(std::ptr::null());
        libc::execv(script_c.as_ptr(), argv.as_ptr());
    }

    #[cfg(windows)]
    {
        use std::process::Command;
        // Same handle-leak prevention as in `launch.rs::launch` — without
        // this, the re-exec'd new binary inherits the launcher's
        // stdin/stdout/stderr pipe handles and keeps them open even after
        // the launcher exits, so any caller reading our output (e.g.
        // PowerShell's `Process.StandardOutput.ReadToEndAsync`) blocks
        // indefinitely.
        crate::launch::mark_stdio_non_inheritable();
        let status = Command::new(script).args(args).status();
        match status {
            Ok(s) => std::process::exit(s.code().unwrap_or(1)),
            Err(_) => std::process::exit(1),
        }
    }
}
