// Marker-patched static data. The Scala builder scans for the magic
// `ETHRCFG\x01` and overwrites the 24 bytes that follow.
//
// Layout:
//   [0..8]   magic: "ETHRCFG" + format version (1)
//   [8..16]  build_id    (u64 little-endian)
//   [16..18] java_min    (u16 little-endian)
//   [18..20] java_pref   (u16 little-endian)
//   [20]     bundle      (0 = jre, 1 = jdk)
//   [21..32] reserved    (0)

const MAGIC: [u8; 8] = [b'E', b'T', b'H', b'R', b'C', b'F', b'G', 1];
const RECORD_LEN: usize = 32;

#[used]
#[unsafe(no_mangle)]
#[cfg_attr(target_os = "macos",   unsafe(link_section = "__DATA,__ethereal"))]
#[cfg_attr(target_os = "linux",   unsafe(link_section = ".ethereal"))]
#[cfg_attr(target_os = "windows", unsafe(link_section = ".rdata$ether"))]
pub static mut ETHEREAL_CONFIG: [u8; RECORD_LEN] = [
    b'E', b'T', b'H', b'R', b'C', b'F', b'G', 1,
    0, 0, 0, 0, 0, 0, 0, 0,
    21, 0,
    24, 0,
    0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

pub struct BuildConfig {
    pub build_id:  u64,
    pub java_min:  u16,
    pub java_pref: u16,
    pub bundle:    &'static str,
}

fn read_record_from_executable() -> Option<[u8; RECORD_LEN]> {
    let executable = std::env::current_exe().ok()?;
    let bytes = std::fs::read(&executable).ok()?;
    let offset = bytes.windows(MAGIC.len()).position(|window| window == MAGIC)?;
    let end = offset + RECORD_LEN;
    if end > bytes.len() { return None; }
    let mut record = [0u8; RECORD_LEN];
    record.copy_from_slice(&bytes[offset..end]);
    Some(record)
}

pub fn read_config() -> BuildConfig {
    let record = read_record_from_executable().unwrap_or([0u8; RECORD_LEN]);
    let build_id = u64::from_le_bytes(record[8..16].try_into().unwrap());
    let java_min_raw = u16::from_le_bytes(record[16..18].try_into().unwrap());
    let java_pref_raw = u16::from_le_bytes(record[18..20].try_into().unwrap());
    let bundle = if record[20] == 0 { "jre" } else { "jdk" };
    BuildConfig {
        build_id,
        java_min:  if java_min_raw  == 0 { 21 } else { java_min_raw  },
        java_pref: if java_pref_raw == 0 { 24 } else { java_pref_raw },
        bundle,
    }
}
