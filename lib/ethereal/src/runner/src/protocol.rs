use std::io::{Read, Write};
use std::path::Path;

use crate::uds::UnixStream;

pub struct ClientInfo {
    pub pid:       u32,
    pub user_id:   u32,
    pub user_name: String,
    pub script:    String,
    pub pwd:       String,
    pub args:      Vec<String>,
    pub env:       Vec<String>,
    pub is_tty:    bool,
}

pub fn send_init(connection: &mut UnixStream, info: &ClientInfo) {
    let _ = connection.write_all(b"i\n");
    let _ = connection.write_all(if info.is_tty { b"t\n" } else { b"p\n" });
    let _ = write!(
        connection,
        "{}\n{}\n{}\n{}\n{}\n{}\n",
        info.pid, info.user_id, info.user_name, info.script, info.pwd, info.args.len(),
    );
    write_null_terminated(connection, &info.args);
    let _ = connection.write_all(b"\n##\n");
    write_null_terminated(connection, &info.env);
    let _ = connection.write_all(b"\n##\n");
    let _ = connection.flush();
}

pub fn send_stderr_request(connection: &mut UnixStream, pid: u32) {
    let _ = write!(connection, "e\n{}\n", pid);
    let _ = connection.flush();
}

pub fn send_signal(socket_path: &Path, pid: u32, name: &str) {
    if let Ok(mut connection) = UnixStream::connect(socket_path) {
        let _ = write!(connection, "s\n{}\n{}\n", pid, name);
        let _ = connection.flush();
    }
}

pub fn terminate(socket_path: &Path, pid: u32) -> i32 {
    let mut connection = match UnixStream::connect(socket_path) {
        Ok(connection) => connection,
        Err(_) => return 2,
    };
    let _ = write!(connection, "x\n{}\n", pid);
    let _ = connection.flush();
    let mut response = String::new();
    if connection.read_to_string(&mut response).is_err() { return 1; }
    response.trim().parse().unwrap_or(1)
}

fn write_null_terminated(connection: &mut UnixStream, items: &[String]) {
    for item in items {
        let _ = connection.write_all(item.as_bytes());
        let _ = connection.write_all(&[0u8]);
    }
}
