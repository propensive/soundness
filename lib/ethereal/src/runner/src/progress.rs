use std::path::Path;
use std::time::{Duration, Instant};

// While a Burdock-repackaged application is fetching its externalized dependencies, its
// bootstrap reports where it has got to in a `progress` file in the daemon's state directory
// (the launcher names it through the `burdock.progress` system property). One line:
// `<completed> <total> <bytes>` — requirements verified so far, requirements in total, and bytes
// downloaded so far. The launcher's startup deadline is measured from the last *change* to this
// file rather than from the spawn, so a cold cache on a slow link takes as long as it takes,
// while a daemon that is making no progress at all still dies after the fixed limit (#1938).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Progress {
    pub completed: u64,
    pub total: u64,
    pub bytes: u64,
}

pub fn parse(text: &str) -> Option<Progress> {
    let mut fields = text.split_whitespace();
    let completed = fields.next()?.parse().ok()?;
    let total = fields.next()?.parse().ok()?;
    let bytes = fields.next()?.parse().ok()?;
    if fields.next().is_some() { return None; }
    Some(Progress { completed, total, bytes })
}

pub fn read(path: &Path) -> Option<Progress> {
    std::fs::read_to_string(path).ok().and_then(|text| parse(&text))
}

pub fn message(progress: &Progress) -> String {
    format!(
        "Fetching dependencies {}/{} ({})…",
        progress.completed, progress.total, size(progress.bytes),
    )
}

pub fn reason(progress: &Progress, idle: Duration) -> String {
    format!(
        "it was still fetching dependency {} of {} ({} downloaded) after {}s without progress",
        (progress.completed + 1).min(progress.total.max(1)), progress.total, size(progress.bytes),
        idle.as_secs(),
    )
}

fn size(bytes: u64) -> String {
    if bytes >= 1_000_000 { format!("{:.1} MB", bytes as f64 / 1_000_000.0) }
    else if bytes >= 1_000 { format!("{:.0} kB", bytes as f64 / 1_000.0) }
    else { format!("{bytes} B") }
}

// Tracks the last observed progress and when it last changed. The file's absence is itself a
// state: its disappearance after downloads complete counts as one change, so class loading gets
// a fresh idle window of its own.
pub struct Watch {
    last_seen: Option<Progress>,
    last_change: Instant,
}

impl Watch {
    pub fn new(now: Instant) -> Watch { Watch { last_seen: None, last_change: now } }

    pub fn observe(&mut self, current: Option<Progress>, now: Instant) -> bool {
        if current == self.last_seen { return false; }
        self.last_seen = current;
        self.last_change = now;
        true
    }

    pub fn current(&self) -> Option<Progress> { self.last_seen }

    pub fn expired(&self, now: Instant, idle_limit: Duration) -> bool {
        now.duration_since(self.last_change) >= idle_limit
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn progress(completed: u64, total: u64, bytes: u64) -> Progress {
        Progress { completed, total, bytes }
    }

    #[test]
    fn parses_a_full_line() {
        assert_eq!(parse("34 128 12345678\n"), Some(progress(34, 128, 12345678)));
    }

    #[test]
    fn rejects_blank_partial_garbage_and_extra_fields() {
        assert_eq!(parse(""), None);
        assert_eq!(parse("34 128"), None);
        assert_eq!(parse("a b c"), None);
        assert_eq!(parse("1 2 3 4"), None);
    }

    #[test]
    fn a_fresh_watch_expires_at_the_idle_limit() {
        let start = Instant::now();
        let watch = Watch::new(start);
        assert!(!watch.expired(start + Duration::from_secs(9), Duration::from_secs(10)));
        assert!(watch.expired(start + Duration::from_secs(10), Duration::from_secs(10)));
    }

    #[test]
    fn a_change_resets_the_idle_clock() {
        let start = Instant::now();
        let mut watch = Watch::new(start);
        assert!(watch.observe(Some(progress(0, 2, 0)), start + Duration::from_secs(1)));
        assert!(watch.observe(Some(progress(0, 2, 500)), start + Duration::from_secs(9)));
        assert!(!watch.expired(start + Duration::from_secs(18), Duration::from_secs(10)));
        assert!(watch.expired(start + Duration::from_secs(19), Duration::from_secs(10)));
    }

    #[test]
    fn identical_content_does_not_reset_the_clock() {
        let start = Instant::now();
        let mut watch = Watch::new(start);
        assert!(watch.observe(Some(progress(1, 2, 500)), start + Duration::from_secs(1)));
        assert!(!watch.observe(Some(progress(1, 2, 500)), start + Duration::from_secs(9)));
        assert!(watch.expired(start + Duration::from_secs(11), Duration::from_secs(10)));
    }

    #[test]
    fn disappearance_counts_as_one_change() {
        let start = Instant::now();
        let mut watch = Watch::new(start);
        assert!(watch.observe(Some(progress(2, 2, 900)), start + Duration::from_secs(1)));
        assert!(watch.observe(None, start + Duration::from_secs(5)));
        assert!(!watch.observe(None, start + Duration::from_secs(6)));
        assert!(!watch.expired(start + Duration::from_secs(14), Duration::from_secs(10)));
        assert!(watch.expired(start + Duration::from_secs(15), Duration::from_secs(10)));
        assert_eq!(watch.current(), None);
    }

    #[test]
    fn messages_name_the_position_and_size() {
        assert_eq!(message(&progress(34, 128, 12_345_678)), "Fetching dependencies 34/128 (12.3 MB)…");
        assert_eq!(message(&progress(0, 2, 512)), "Fetching dependencies 0/2 (512 B)…");
        assert_eq!(
            reason(&progress(34, 128, 12_345_678), Duration::from_secs(10)),
            "it was still fetching dependency 35 of 128 (12.3 MB downloaded) after 10s without progress",
        );
    }
}
