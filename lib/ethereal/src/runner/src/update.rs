use std::path::Path;

pub fn check_updates(script: &Path, args: &[String], name: &str) {
    let data_home = crate::state::data_home();
    let pending = data_home.join(name).join(".pending");
    let metadata = match std::fs::metadata(&pending) {
        Ok(m) => m,
        Err(_) => return,
    };
    if metadata.len() == 0 { return; }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perm = metadata.permissions();
        perm.set_mode(0o755);
        let _ = std::fs::set_permissions(&pending, perm);
    }

    let old = data_home.join(format!("{}.old", name));
    let _ = std::fs::remove_file(&old);
    if std::fs::rename(script, &old).is_err() { return; }
    if std::fs::rename(&pending, script).is_err() {
        let _ = std::fs::rename(&old, script);
        return;
    }

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
        let status = Command::new(script).args(args).status();
        match status {
            Ok(s) => std::process::exit(s.code().unwrap_or(1)),
            Err(_) => std::process::exit(1),
        }
    }
}
