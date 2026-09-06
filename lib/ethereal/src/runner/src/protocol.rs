use std::io::Write;
use std::path::Path;
use std::time::Duration;

use crate::bintel::{self, variant, Record, Reply};
use crate::uds::UnixStream;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SignalAck {
    Accept,
    Reject,
    Timeout,
}

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

// Every connection to the daemon opens with one BinTEL document of the `ethereal-launcher`
// schema (see `bintel.rs`); what follows depends on the message. After `init` the connection
// is the invocation's stdin and stdout; after `stderr` it delivers stderr; after `control` the
// daemon sends `mode` documents on it; `signal`, `verify` and `exit` are answered with one
// document each and closed.

pub fn init_document(info: &ClientInfo) -> Vec<u8> {
    let mut record = Record::new();
    record.scalar(0, &info.pid.to_string());
    record.scalar(1, &info.user_id.to_string());
    record.scalar(2, &info.user_name);
    record.scalar(3, &info.script);
    record.scalar(4, &info.pwd);
    if info.is_tty { record.flag(5); }
    for argument in &info.args { record.scalar(6, argument); }
    for variable in &info.env { record.scalar(7, variable); }
    bintel::document(variant::INIT, record)
}

pub fn send_init(connection: &mut UnixStream, info: &ClientInfo) {
    let _ = connection.write_all(&init_document(info));
    let _ = connection.flush();
}

fn pid_record(pid: u32) -> Record {
    let mut record = Record::new();
    record.scalar(0, &pid.to_string());
    record
}

pub fn send_stderr_request(connection: &mut UnixStream, pid: u32) {
    let _ = connection.write_all(&bintel::document(variant::STDERR, pid_record(pid)));
    let _ = connection.flush();
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Verdict {
    Fresh,
    Stale,
}

// Ask the resident daemon whether the launcher it was started from still has the
// content it remembers — sent when the file's mtime disagrees with the build file's
// record, i.e. after a `touch` or a same-size rebuild. The daemon hashes at most once
// per change and remembers the answer, which a stateless launcher cannot. The verdict
// document says fresh or stale (the daemon then shuts down; await its death and launch
// afresh); anything else — including a daemon too old to speak this protocol, which just
// closes the connection — means proceed as normal.
pub fn verify(socket_path: &Path) -> Verdict {
    let mut connection = match UnixStream::connect(socket_path) {
        Ok(connection) => connection,
        Err(_) => return Verdict::Fresh,
    };
    let _ = connection.write_all(&bintel::document(variant::VERIFY, Record::new()));
    let _ = connection.flush();
    match bintel::read_document(&mut connection).ok().and_then(|doc| bintel::parse_reply(&doc)) {
        Some(Reply::Verdict { fresh: false }) => Verdict::Stale,
        _ => Verdict::Fresh,
    }
}

// The control channel: a side-connection on which the daemon sends `mode` documents asking
// for the client's terminal to be put into canonical (cooked) mode or back into raw mode. The
// launcher is the only process that can change the client's tty mode, and it has already
// raw-moded the terminal by the time the daemon knows which command is running, so the
// request has to be pushed back here.
pub fn send_control_request(connection: &mut UnixStream, pid: u32) {
    let _ = connection.write_all(&bintel::document(variant::CONTROL, pid_record(pid)));
    let _ = connection.flush();
}

pub fn send_signal(socket_path: &Path, pid: u32, name: &str, timeout_ms: u64) -> SignalAck {
    let mut connection = match UnixStream::connect(socket_path) {
        Ok(connection) => connection,
        Err(_) => return SignalAck::Timeout,
    };
    let mut record = pid_record(pid);
    record.scalar(1, name);
    if connection.write_all(&bintel::document(variant::SIGNAL, record)).is_err() {
        return SignalAck::Timeout;
    }
    if connection.flush().is_err() { return SignalAck::Timeout; }
    let _ = connection.set_read_timeout(Some(Duration::from_millis(timeout_ms)));
    match bintel::read_document(&mut connection).ok().and_then(|doc| bintel::parse_reply(&doc)) {
        Some(Reply::SignalAck { accept: true })  => SignalAck::Accept,
        Some(Reply::SignalAck { accept: false }) => SignalAck::Reject,
        _                                        => SignalAck::Timeout,
    }
}

pub fn terminate(socket_path: &Path, pid: u32) -> i32 {
    let mut connection = match UnixStream::connect(socket_path) {
        Ok(connection) => connection,
        Err(_) => return 2,
    };
    let _ = connection.write_all(&bintel::document(variant::EXIT, pid_record(pid)));
    let _ = connection.flush();
    match bintel::read_document(&mut connection).ok().and_then(|doc| bintel::parse_reply(&doc)) {
        Some(Reply::ExitStatus { code }) => code,
        _ => 1,
    }
}
