// Marker-patched static data. The Scala builder scans for the magic
// `ETHRCFG\x02` and overwrites the bytes that follow.
//
// Layout (3764 bytes total):
//   [0..8]        magic: "ETHRCFG" + format version (2)
//   [8..16]       build_id    (u64 little-endian)
//   [16..18]      java_min    (u16 little-endian)
//   [18..20]      java_pref   (u16 little-endian)
//   [20]          bundle      (0 = jre, 1 = jdk)
//   [21]          flags       (bit 0 = downgrade_permitted, others reserved)
//   [22..32]      reserved    (0)
//   [32..1344]    ml_dsa_44 public key (1312 bytes)
//   [1344..3764]  ml_dsa_44 signature  (2420 bytes; zero in the running
//                 binary, set only after build-time patching by the signer.
//                 The verifier zeroes this region of the incoming .pending
//                 binary before recomputing the signature.)

pub const RECORD_LEN: usize        = 3764;
pub const MAGIC_LEN: usize         = 8;
pub const META_OFFSET: usize       = 8;
pub const META_LEN: usize          = 24;          // build_id..reserved
pub const PUBKEY_OFFSET: usize     = 32;
pub const PUBKEY_LEN: usize        = 1312;        // ML-DSA-44 |pk|
pub const SIGNATURE_OFFSET: usize  = 1344;
pub const SIGNATURE_LEN: usize     = 2420;        // ML-DSA-44 |sig|

pub const FLAG_DOWNGRADE_PERMITTED: u8 = 0x01;

pub const MAGIC: [u8; MAGIC_LEN] =
    [b'E', b'T', b'H', b'R', b'C', b'F', b'G', 2];

#[used]
#[unsafe(no_mangle)]
#[cfg_attr(target_os = "macos",   unsafe(link_section = "__DATA,__ethereal"))]
#[cfg_attr(target_os = "linux",   unsafe(link_section = ".ethereal"))]
#[cfg_attr(target_os = "windows", unsafe(link_section = ".rdata$ether"))]
pub static mut ETHEREAL_CONFIG: [u8; RECORD_LEN] = {
    let mut record = [0u8; RECORD_LEN];
    record[0] = b'E'; record[1] = b'T'; record[2] = b'H'; record[3] = b'R';
    record[4] = b'C'; record[5] = b'F'; record[6] = b'G'; record[7] = 2;
    // build_id (8..16): zero by default.
    // java_min (16..18) = 21.
    record[16] = 21;
    // java_pref (18..20) = 24.
    record[18] = 24;
    // bundle, flags, reserved, pubkey, sig: zero by default.
    record
};

pub struct BuildConfig {
    pub build_id:  u64,
    pub java_min:  u16,
    pub java_pref: u16,
    pub bundle:    &'static str,
    pub flags:     u8,
}

fn read_block() -> [u8; RECORD_LEN] {
    // Volatile read so the compiler cannot constant-fold the initial values
    // defined above — the bytes are patched at build time (or left as
    // defaults when the runner is shipped unpatched).
    unsafe { core::ptr::read_volatile(&raw const ETHEREAL_CONFIG) }
}

pub fn read_config() -> BuildConfig {
    let record = read_block();
    let build_id     = u64::from_le_bytes(record[8..16].try_into().unwrap());
    let java_min_raw = u16::from_le_bytes(record[16..18].try_into().unwrap());
    let java_pref_raw = u16::from_le_bytes(record[18..20].try_into().unwrap());
    let bundle = if record[20] == 0 { "jre" } else { "jdk" };
    let flags = record[21];
    BuildConfig {
        build_id,
        java_min:  if java_min_raw  == 0 { 21 } else { java_min_raw  },
        java_pref: if java_pref_raw == 0 { 24 } else { java_pref_raw },
        bundle,
        flags,
    }
}

// Snapshot of the running binary's ML-DSA-44 public key. Returned as an owned
// array because the caller may need to forward it across thread or FFI
// boundaries; it's only 1312 bytes.
pub fn public_key() -> [u8; PUBKEY_LEN] {
    let record = read_block();
    let mut out = [0u8; PUBKEY_LEN];
    out.copy_from_slice(&record[PUBKEY_OFFSET..PUBKEY_OFFSET + PUBKEY_LEN]);
    out
}

// True iff the baked-in public key is all zeros — the safe "no signing
// configured" state. A runner in this state rejects every upgrade.
pub fn public_key_is_unset() -> bool {
    let record = read_block();
    record[PUBKEY_OFFSET..PUBKEY_OFFSET + PUBKEY_LEN].iter().all(|&b| b == 0)
}
