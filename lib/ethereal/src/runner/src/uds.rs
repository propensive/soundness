// Cross-platform AF_UNIX client. Once `std::os::windows::net::UnixStream`
// stabilises, the Windows arm here can collapse to a `pub use std::os::...`.

#[cfg(unix)]
pub use std::os::unix::net::UnixStream;

#[cfg(windows)]
pub use uds_windows::UnixStream;
