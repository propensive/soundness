//! The subset of BinTEL the launcher protocol needs — §4 varints, §6.1 framing and the §7.1
//! node forms — written against one fixed schema, `ethereal-launcher`, whose TEL text lives
//! in `ethereal.Launcher.scala` and is the contract between this runner and the daemon.
//!
//! The schema's keyword order is compiled in: the document root has a single `select Message`
//! member, so a message is the root node (child count 1) containing one variant node whose
//! keyword index is the variant's position in the select, followed by the variant record's
//! fields in declaration order. Fields are scalars (index, byte length, UTF-8 bytes) or flags
//! (index alone). The 33-byte schema signature travels in every frame; a document carrying
//! any other signature is rejected before a field is read, so a runner and a daemon built
//! against different contracts fail loudly rather than misread each other.
//!
//! No general TEL machinery is here — no schema parsing, no hashing, no BASE-256 — because
//! the runner is a size-optimised launcher and the contract is fixed at build time.

use std::io::{self, Read};

/// §6.1 field 1: the external-schema magic number, `βτελ` in BASE-256.
pub const MAGIC: [u8; 4] = [0xB2, 0xC4, 0xB5, 0xBB];

/// The §8 palimpsest signature of the `ethereal-launcher` schema (BLAKE3-256 of its base
/// component plus the cadence byte), pinned here and in the daemon's tests.
pub const SIGNATURE: [u8; 33] = [
    0x47, 0x01, 0xec, 0x19, 0xcd, 0x0f, 0xd3, 0xec, 0xfc, 0x0e, 0x1b, 0x8a, 0x65, 0x25, 0xb4,
    0xed, 0xc3, 0xa3, 0xb1, 0xde, 0xda, 0x37, 0x0f, 0x68, 0x19, 0x86, 0xdb, 0x9a, 0xa3, 0x9c,
    0x1d, 0xa6, 0x92,
];

/// Variant indices of `select Message`, in the schema's declaration order.
pub mod variant {
    pub const INIT: u64 = 0;
    pub const STDERR: u64 = 1;
    pub const CONTROL: u64 = 2;
    pub const SIGNAL: u64 = 3;
    pub const EXIT: u64 = 4;
    pub const VERIFY: u64 = 5;
    pub const SIGNAL_ACK: u64 = 6;
    pub const VERDICT: u64 = 7;
    pub const MODE: u64 = 8;
    pub const EXIT_STATUS: u64 = 9;
}

/// The daemon reads documents from a peer it did not choose; so does the runner. A reply
/// larger than this is not a reply.
const MAXIMUM_LENGTH: u64 = 1 << 20;

// ── §4 varints ────────────────────────────────────────────────────────────────

pub fn encode_varint(out: &mut Vec<u8>, mut n: u64) {
    while n >= 0x80 {
        out.push(((n & 0x7f) as u8) | 0x80);
        n >>= 7;
    }
    out.push(n as u8);
}

/// `(value, bytes consumed)`, or `None` for a truncated, over-wide or overlong encoding
/// (all B02 under §4).
pub fn decode_varint(bytes: &[u8]) -> Option<(u64, usize)> {
    let mut value: u64 = 0;
    let mut shift: u32 = 0;
    for (i, &b) in bytes.iter().enumerate() {
        let chunk = (b & 0x7f) as u64;
        if shift >= 64 || (shift == 63 && chunk > 1) { return None; }
        value |= chunk << shift;
        if b & 0x80 == 0 {
            if i > 0 && chunk == 0 { return None; }
            return Some((value, i + 1));
        }
        shift += 7;
    }
    None
}

// ── Encoding ──────────────────────────────────────────────────────────────────

/// The fields of one variant record, accumulated in declaration order (§7.2 canonical order
/// is member order, and every message here is written that way).
pub struct Record {
    count: u64,
    bytes: Vec<u8>,
}

impl Record {
    pub fn new() -> Record { Record { count: 0, bytes: Vec::new() } }

    pub fn scalar(&mut self, index: u64, text: &str) {
        encode_varint(&mut self.bytes, index);
        encode_varint(&mut self.bytes, text.len() as u64);
        self.bytes.extend_from_slice(text.as_bytes());
        self.count += 1;
    }

    pub fn flag(&mut self, index: u64) {
        encode_varint(&mut self.bytes, index);
        self.count += 1;
    }
}

/// A complete framed document (§6.1) carrying one `Message` of the given variant.
pub fn document(variant: u64, record: Record) -> Vec<u8> {
    let mut body = Vec::with_capacity(record.bytes.len() + 8);
    encode_varint(&mut body, 1);             // root: one child, the select member
    encode_varint(&mut body, variant);       // the variant's keyword index
    encode_varint(&mut body, record.count);  // the record's child count
    body.extend_from_slice(&record.bytes);

    let mut signature_length = Vec::new();
    encode_varint(&mut signature_length, SIGNATURE.len() as u64);
    let length = signature_length.len() + SIGNATURE.len() + body.len();

    let mut out = Vec::with_capacity(4 + 2 + length);
    out.extend_from_slice(&MAGIC);
    encode_varint(&mut out, length as u64);
    out.extend_from_slice(&signature_length);
    out.extend_from_slice(&SIGNATURE);
    out.extend_from_slice(&body);
    out
}

// ── Decoding ──────────────────────────────────────────────────────────────────

/// A reply from the daemon.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Reply {
    SignalAck { accept: bool },
    Verdict { fresh: bool },
    Mode { canonical: bool },
    ExitStatus { code: i32 },
}

/// Reads exactly one framed document from `reader` — the magic number, the length varint and
/// then the declared bytes — and returns it whole. Nothing beyond the document is consumed.
pub fn read_document(reader: &mut impl Read) -> io::Result<Vec<u8>> {
    let mut out = vec![0u8; 4];
    reader.read_exact(&mut out)?;
    if out[..4] != MAGIC {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "not a BinTEL document"));
    }
    let mut declared: u64 = 0;
    let mut shift = 0;
    loop {
        let mut byte = [0u8; 1];
        reader.read_exact(&mut byte)?;
        out.push(byte[0]);
        if shift > 63 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "document length too wide"));
        }
        declared |= ((byte[0] & 0x7f) as u64) << shift;
        shift += 7;
        if byte[0] & 0x80 == 0 { break; }
    }
    if declared > MAXIMUM_LENGTH {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "document too long"));
    }
    let start = out.len();
    out.resize(start + declared as usize, 0);
    reader.read_exact(&mut out[start..])?;
    Ok(out)
}

/// The kind of field at `index` in a reply variant's record, per the schema.
#[derive(Clone, Copy)]
enum Kind { Scalar, Flag }

fn field_kind(variant: u64, index: u64) -> Option<Kind> {
    match (variant, index) {
        (variant::SIGNAL_ACK, 0) | (variant::VERDICT, 0) | (variant::MODE, 0) => Some(Kind::Flag),
        (variant::EXIT_STATUS, 0) => Some(Kind::Scalar),
        _ => None,
    }
}

/// Decodes a framed reply document. `None` for anything that is not a well-formed document of
/// the launcher schema carrying one reply variant.
pub fn parse_reply(document: &[u8]) -> Option<Reply> {
    if document.len() < 4 || document[..4] != MAGIC { return None; }
    let mut cur = 4;
    let (declared, n) = decode_varint(&document[cur..])?;
    cur += n;
    if declared as usize != document.len() - cur { return None; }

    let (signature_length, n) = decode_varint(&document[cur..])?;
    cur += n;
    if signature_length as usize != SIGNATURE.len() { return None; }
    if document.len() < cur + SIGNATURE.len() || document[cur..cur + SIGNATURE.len()] != SIGNATURE {
        return None;
    }
    cur += SIGNATURE.len();

    let (root_count, n) = decode_varint(&document[cur..])?;
    cur += n;
    if root_count != 1 { return None; }
    let (variant, n) = decode_varint(&document[cur..])?;
    cur += n;
    let (field_count, n) = decode_varint(&document[cur..])?;
    cur += n;

    let mut flags: Vec<u64> = Vec::new();
    let mut scalars: Vec<(u64, Vec<u8>)> = Vec::new();
    for _ in 0..field_count {
        let (index, n) = decode_varint(&document[cur..])?;
        cur += n;
        match field_kind(variant, index)? {
            Kind::Flag => flags.push(index),
            Kind::Scalar => {
                let (length, n) = decode_varint(&document[cur..])?;
                cur += n;
                let end = cur.checked_add(length as usize)?;
                if end > document.len() { return None; }
                scalars.push((index, document[cur..end].to_vec()));
                cur = end;
            }
        }
    }
    // §6.1 field 2 / B16: the structure must end exactly where the declared length says.
    if cur != document.len() { return None; }

    let flag = |index: u64| flags.contains(&index);
    let text = |index: u64| -> Option<String> {
        scalars.iter().find(|(i, _)| *i == index)
            .and_then(|(_, bytes)| String::from_utf8(bytes.clone()).ok())
    };

    match variant {
        variant::SIGNAL_ACK => Some(Reply::SignalAck { accept: flag(0) }),
        variant::VERDICT => Some(Reply::Verdict { fresh: flag(0) }),
        variant::MODE => Some(Reply::Mode { canonical: flag(0) }),
        variant::EXIT_STATUS => Some(Reply::ExitStatus { code: text(0)?.trim().parse().ok()? }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn varints_round_trip_and_reject_overlong() {
        for n in [0u64, 1, 127, 128, 300, 16384, u64::MAX] {
            let mut out = Vec::new();
            encode_varint(&mut out, n);
            assert_eq!(decode_varint(&out), Some((n, out.len())));
        }
        assert_eq!(decode_varint(&[0x80, 0x00]), None);
        assert_eq!(decode_varint(&[0x80]), None);
    }

    #[test]
    fn exit_document_has_the_pinned_layout() {
        let mut record = Record::new();
        record.scalar(0, "42");
        let doc = document(variant::EXIT, record);
        // magic, length (1 + 33 + 7 = 41), signature length, signature, body.
        assert_eq!(&doc[..4], &MAGIC);
        assert_eq!(doc[4], 41);
        assert_eq!(doc[5], 33);
        assert_eq!(&doc[6..39], &SIGNATURE);
        assert_eq!(&doc[39..], &[0x01, 0x04, 0x01, 0x00, 0x02, b'4', b'2']);
    }

    // The frames the daemon's own tests pin (`ethereal_test.scala`, "Launcher protocol"),
    // produced by `Launcher.encode` on the Scala side: both implementations must agree
    // byte for byte.
    fn hex(bytes: &[u8]) -> String {
        bytes.iter().map(|b| format!("{:02x}", b)).collect()
    }

    #[test]
    fn frames_match_the_daemon_side() {
        let sig = "4701ec19cd0fd3ecfc0e1b8a6525b4edc3a3b1deda370f681986db9aa39c1da692";
        let mut record = Record::new();
        record.scalar(0, "42");
        assert_eq!(hex(&document(variant::EXIT, record)),
                   format!("b2c4b5bb2921{sig}01040100023432"));
        assert_eq!(hex(&document(variant::VERIFY, Record::new())),
                   format!("b2c4b5bb2521{sig}010500"));
        let mut record = Record::new();
        record.flag(0);
        assert_eq!(hex(&document(variant::MODE, record)),
                   format!("b2c4b5bb2621{sig}01080100"));
        let info = crate::protocol::ClientInfo {
            pid: 7, user_id: 501, user_name: "jon".into(), script: "/usr/bin/x".into(),
            pwd: "/tmp".into(), args: vec!["a".into(), "b c".into()],
            env: vec!["K=V".into()], is_tty: true,
        };
        assert_eq!(hex(&crate::protocol::init_document(&info)),
                   format!("b2c4b5bb5221{sig}010009000137010335303102036a6f6e030a2f7573722f62696e2f7804042f746d7005060161060362206307034b3d56"));
    }

    #[test]
    fn replies_parse() {
        let mut record = Record::new();
        record.scalar(0, "3");
        let doc = document(variant::EXIT_STATUS, record);
        assert_eq!(parse_reply(&doc), Some(Reply::ExitStatus { code: 3 }));

        let mut record = Record::new();
        record.flag(0);
        let doc = document(variant::MODE, record);
        assert_eq!(parse_reply(&doc), Some(Reply::Mode { canonical: true }));

        let doc = document(variant::VERDICT, Record::new());
        assert_eq!(parse_reply(&doc), Some(Reply::Verdict { fresh: false }));
    }

    #[test]
    fn read_document_consumes_exactly_one_document() {
        let mut record = Record::new();
        record.flag(0);
        let mut stream = document(variant::SIGNAL_ACK, record);
        let length = stream.len();
        stream.extend_from_slice(b"trailing");
        let mut cursor = std::io::Cursor::new(stream);
        let doc = read_document(&mut cursor).unwrap();
        assert_eq!(doc.len(), length);
        assert_eq!(cursor.position() as usize, length);
        assert_eq!(parse_reply(&doc), Some(Reply::SignalAck { accept: true }));
    }

    #[test]
    fn a_foreign_signature_is_rejected() {
        let mut record = Record::new();
        record.scalar(0, "3");
        let mut doc = document(variant::EXIT_STATUS, record);
        doc[6] ^= 0x01;
        assert_eq!(parse_reply(&doc), None);
    }
}
