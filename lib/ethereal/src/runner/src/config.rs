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

#[used]
#[unsafe(no_mangle)]
#[cfg_attr(target_os = "macos",   unsafe(link_section = "__DATA,__ethereal"))]
#[cfg_attr(target_os = "linux",   unsafe(link_section = ".ethereal"))]
#[cfg_attr(target_os = "windows", unsafe(link_section = ".rdata$ether"))]
pub static mut ETHEREAL_CONFIG: [u8; 32] = [
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

const MAGIC: [u8; 8] = [b'E', b'T', b'H', b'R', b'C', b'F', b'G', 1];

fn read_from_file() -> Option<[u8; 32]> {
    use std::io::Read;
    let exe = std::env::current_exe().ok()?;
    let mut f = std::fs::File::open(&exe).ok()?;
    let mut bytes = Vec::new();
    f.read_to_end(&mut bytes).ok()?;
    let mut i = 0;
    while i + MAGIC.len() + 24 <= bytes.len() {
        if bytes[i..i + MAGIC.len()] == MAGIC {
            let mut raw = [0u8; 32];
            raw[..8].copy_from_slice(&MAGIC);
            raw[8..].copy_from_slice(&bytes[i + 8..i + 32]);
            return Some(raw);
        }
        i += 1;
    }
    None
}

pub fn read_config() -> BuildConfig {
    let raw = read_from_file().unwrap_or([0u8; 32]);
    let build_id  = u64::from_le_bytes([raw[8], raw[9], raw[10], raw[11], raw[12], raw[13], raw[14], raw[15]]);
    let java_min  = u16::from_le_bytes([raw[16], raw[17]]);
    let java_pref = u16::from_le_bytes([raw[18], raw[19]]);
    let bundle    = if raw[20] == 0 { "jre" } else { "jdk" };
    let java_min  = if java_min == 0  { 21 } else { java_min  };
    let java_pref = if java_pref == 0 { 24 } else { java_pref };
    BuildConfig { build_id, java_min, java_pref, bundle }
}
