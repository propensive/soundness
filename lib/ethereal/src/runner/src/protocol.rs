use std::io::{Read, Write};
use std::net::TcpStream;

pub fn send_init(
    conn: &mut TcpStream,
    is_tty: bool,
    pid: u32,
    uid: u32,
    user: &str,
    script: &str,
    pwd: &str,
    args: &[String],
    env: &[String],
) {
    let _ = conn.write_all(b"i\n");
    let _ = conn.write_all(if is_tty { b"t\n" } else { b"p\n" });
    let _ = write!(conn, "{}\n{}\n{}\n{}\n", pid, uid, user, script);
    let _ = write!(conn, "{}\n", pwd);
    let _ = write!(conn, "{}\n", args.len());
    for a in args {
        let _ = conn.write_all(a.as_bytes());
        let _ = conn.write_all(&[0u8]);
    }
    let _ = conn.write_all(b"\n##\n");
    for e in env {
        let _ = conn.write_all(e.as_bytes());
        let _ = conn.write_all(&[0u8]);
    }
    let _ = conn.write_all(b"\n##\n");
    let _ = conn.flush();
}

pub fn send_stderr_request(conn: &mut TcpStream, pid: u32) {
    let _ = write!(conn, "e\n{}\n", pid);
    let _ = conn.flush();
}

pub fn send_signal(port: u16, pid: u32, name: &str) {
    if let Ok(mut s) = TcpStream::connect(("127.0.0.1", port)) {
        let _ = write!(s, "s\n{}\n{}\n", pid, name);
        let _ = s.flush();
    }
}

pub fn terminate(port: u16, pid: u32) -> i32 {
    let mut s = match TcpStream::connect(("127.0.0.1", port)) {
        Ok(s) => s,
        Err(_) => return 2,
    };
    let _ = write!(s, "x\n{}\n", pid);
    let _ = s.flush();
    let mut buf = String::new();
    if s.read_to_string(&mut buf).is_err() { return 1; }
    buf.trim().parse().unwrap_or(1)
}
