// ethereal-sign: produce a signed `.pending` binary for ethereal self-upgrade.
//
// Usage:
//   ethereal-sign keygen --out <key-prefix>
//     Generates a fresh ML-DSA-44 keypair, writing <prefix>.pub (1312 B) and
//     <prefix>.seed (32 B).  The seed is the FIPS-204 signing key seed; keep
//     it offline.
//
//   ethereal-sign sign --key <seed-file> --in <binary> --out <signed>
//                      [--allow-downgrade]
//     Reads <binary> (a runner+JAR file produced by an ethereal build),
//     locates the ETHRCFG\x02 marker, sets the per-upgrade flags byte and
//     populates the ML-DSA-44 signature slot. Writes the result to <signed>,
//     which is byte-for-byte the file that should be delivered as `.pending`
//     to the target machine.

use std::convert::TryInto;
use std::path::PathBuf;

use ml_dsa::{B32, Keypair, MlDsa44, Signature, SigningKey, signature::Signer};
use rand::{TryRngCore, rngs::OsRng};

// Layout — keep in sync with lib/ethereal/src/runner/src/config.rs.
const MAGIC: [u8; 8]            = *b"ETHRCFG\x02";
const RECORD_LEN: usize         = 3764;
const SIGNATURE_OFFSET: usize   = 1344;
const SIGNATURE_LEN: usize      = 2420;
const PUBKEY_LEN: usize         = 1312;
const SEED_LEN: usize           = 32;

fn find_magic(binary: &[u8]) -> Option<usize> {
    binary.windows(MAGIC.len()).position(|w| w == MAGIC)
}

fn die(msg: impl AsRef<str>) -> ! {
    eprintln!("ethereal-sign: {}", msg.as_ref());
    std::process::exit(1);
}

fn keygen(out_prefix: PathBuf) {
    let mut seed_raw = [0u8; SEED_LEN];
    OsRng.try_fill_bytes(&mut seed_raw)
        .unwrap_or_else(|e| die(format!("could not read entropy from the OS RNG: {e}")));
    let seed_b32: B32 = seed_raw.into();
    let sk: SigningKey<MlDsa44> = SigningKey::from_seed(&seed_b32);
    let vk = sk.verifying_key();
    let seed_bytes: [u8; SEED_LEN] = seed_raw;
    let pk_bytes: [u8; PUBKEY_LEN] = vk.encode().into();

    let seed_path = out_prefix.with_extension("seed");
    let pub_path  = out_prefix.with_extension("pub");
    if let Err(e) = std::fs::write(&seed_path, seed_bytes) {
        die(format!("could not write seed to {}: {e}", seed_path.display()));
    }
    if let Err(e) = std::fs::write(&pub_path, pk_bytes) {
        die(format!("could not write public key to {}: {e}", pub_path.display()));
    }
    eprintln!("wrote {} ({} bytes) and {} ({} bytes)",
              seed_path.display(), SEED_LEN, pub_path.display(), PUBKEY_LEN);
}

fn sign(seed_path: PathBuf, in_path: PathBuf, out_path: PathBuf, allow_downgrade: bool) {
    let seed = std::fs::read(&seed_path)
        .unwrap_or_else(|e| die(format!("could not read seed file {}: {e}", seed_path.display())));
    if seed.len() != SEED_LEN {
        die(format!("seed at {} is {} bytes; expected {SEED_LEN}",
                    seed_path.display(), seed.len()));
    }
    let seed_arr: [u8; SEED_LEN] = seed.as_slice().try_into().unwrap();

    let mut bin = std::fs::read(&in_path)
        .unwrap_or_else(|e| die(format!("could not read input {}: {e}", in_path.display())));

    let magic_offset = find_magic(&bin)
        .unwrap_or_else(|| die("input binary does not contain the ETHRCFG\\x02 marker"));
    let block_end = magic_offset + RECORD_LEN;
    if bin.len() < block_end {
        die(format!("input binary is truncated within the ETHRCFG block at offset {magic_offset}"));
    }

    // Set the per-upgrade flags byte before signing — it's part of the
    // signed payload.
    bin[magic_offset + 21] = if allow_downgrade { 0x01 } else { 0x00 };

    // Zero the signature slot before signing. Verifier does the same.
    let sig_start = magic_offset + SIGNATURE_OFFSET;
    let sig_end   = sig_start + SIGNATURE_LEN;
    for b in &mut bin[sig_start..sig_end] { *b = 0; }

    let seed_b32: B32 = seed_arr.into();
    let sk: SigningKey<MlDsa44> = SigningKey::from_seed(&seed_b32);
    let sig: Signature<MlDsa44> = sk.sign(&bin);
    let sig_bytes: [u8; SIGNATURE_LEN] = sig.encode().into();

    bin[sig_start..sig_end].copy_from_slice(&sig_bytes);

    if let Err(e) = std::fs::write(&out_path, &bin) {
        die(format!("could not write {}: {e}", out_path.display()));
    }
    eprintln!("wrote signed binary to {} (allow_downgrade={allow_downgrade})",
              out_path.display());
}

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let mut iter = argv.iter().skip(1);
    let subcmd = iter.next().map(String::as_str).unwrap_or("");

    match subcmd {
        "keygen" => {
            let mut out: Option<PathBuf> = None;
            while let Some(arg) = iter.next() {
                match arg.as_str() {
                    "--out" => out = iter.next().map(PathBuf::from),
                    _ => die(format!("unknown argument: {arg}")),
                }
            }
            let out = out.unwrap_or_else(|| die("missing --out <prefix>"));
            keygen(out);
        }
        "sign" => {
            let mut seed: Option<PathBuf> = None;
            let mut input: Option<PathBuf> = None;
            let mut output: Option<PathBuf> = None;
            let mut allow_downgrade = false;
            while let Some(arg) = iter.next() {
                match arg.as_str() {
                    "--key" => seed = iter.next().map(PathBuf::from),
                    "--in"  => input = iter.next().map(PathBuf::from),
                    "--out" => output = iter.next().map(PathBuf::from),
                    "--allow-downgrade" => allow_downgrade = true,
                    _ => die(format!("unknown argument: {arg}")),
                }
            }
            let seed   = seed.unwrap_or_else(|| die("missing --key <seed-file>"));
            let input  = input.unwrap_or_else(|| die("missing --in <binary>"));
            let output = output.unwrap_or_else(|| die("missing --out <signed>"));
            sign(seed, input, output, allow_downgrade);
        }
        "" => die("usage: ethereal-sign <keygen|sign> [options]"),
        other => die(format!("unknown subcommand: {other}")),
    }
}
