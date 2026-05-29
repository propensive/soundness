use ml_dsa::{EncodedSignature, EncodedVerifyingKey, MlDsa44, Signature, VerifyingKey,
             signature::Verifier};

use crate::config::{
    MAGIC, MAGIC_LEN, PUBKEY_LEN, RECORD_LEN, SIGNATURE_LEN, SIGNATURE_OFFSET,
};

#[derive(Debug)]
pub enum VerifyError {
    MagicMissing,
    Truncated,
    BadPublicKey,
    BadSignature,
    SignatureMismatch,
    PublicKeyUnset,
}

#[derive(Debug)]
pub struct VerifiedBinary {
    pub build_id: u64,
    pub flags:    u8,
}

// Locate the ETHRCFG magic in `binary`. Searches forwards; only the first
// occurrence is honoured (the marker is rare enough in optimised binaries
// that this is unambiguous in practice).
fn find_magic(binary: &[u8]) -> Option<usize> {
    if binary.len() < MAGIC_LEN { return None; }
    binary.windows(MAGIC_LEN).position(|w| w == MAGIC)
}

// Verify a candidate upgrade binary against the runner's baked-in public key.
//
// Algorithm:
//   1. Locate the ETHRCFG magic in `pending`.
//   2. Extract the embedded signature bytes from that block.
//   3. Make a working copy of the binary with the signature slot zeroed.
//   4. Verify the signature over the zeroed copy using the running runner's
//      public key.
//
// Returns the embedded `build_id` and `flags` on success so the caller can
// enforce downgrade policy. The pending bytes themselves are unchanged and
// ready to be swapped into place.
pub fn verify_pending(
    pending: &[u8],
    pubkey:  &[u8; PUBKEY_LEN],
) -> Result<VerifiedBinary, VerifyError> {

    if pubkey.iter().all(|&b| b == 0) {
        return Err(VerifyError::PublicKeyUnset);
    }

    let magic_offset = find_magic(pending).ok_or(VerifyError::MagicMissing)?;
    let block_end = magic_offset + RECORD_LEN;
    if pending.len() < block_end { return Err(VerifyError::Truncated); }

    let block = &pending[magic_offset..block_end];
    let build_id = u64::from_le_bytes(block[8..16].try_into().unwrap());
    let flags    = block[21];

    let sig_start_in_file = magic_offset + SIGNATURE_OFFSET;
    let sig_end_in_file   = sig_start_in_file + SIGNATURE_LEN;
    let mut sig_bytes = [0u8; SIGNATURE_LEN];
    sig_bytes.copy_from_slice(&pending[sig_start_in_file..sig_end_in_file]);

    // Mask the signature region to zero before recomputing the signature.
    // Signing and verification must agree on this exact byte range.
    let mut zeroed = pending.to_vec();
    for b in &mut zeroed[sig_start_in_file..sig_end_in_file] { *b = 0; }

    let encoded_pk = EncodedVerifyingKey::<MlDsa44>::try_from(&pubkey[..])
        .map_err(|_| VerifyError::BadPublicKey)?;
    let vk = VerifyingKey::<MlDsa44>::decode(&encoded_pk);

    let encoded_sig = EncodedSignature::<MlDsa44>::try_from(&sig_bytes[..])
        .map_err(|_| VerifyError::BadSignature)?;
    let sig = Signature::<MlDsa44>::decode(&encoded_sig)
        .ok_or(VerifyError::BadSignature)?;

    vk.verify(&zeroed, &sig).map_err(|_| VerifyError::SignatureMismatch)?;

    Ok(VerifiedBinary { build_id, flags })
}

#[cfg(all(test, feature = "sign"))]
mod tests {
    use super::*;
    use ml_dsa::{B32, Keypair, SigningKey, signature::Signer};
    use rand::{TryRngCore, rngs::OsRng};

    fn fresh_keypair() -> (SigningKey<MlDsa44>, [u8; PUBKEY_LEN]) {
        let mut seed = [0u8; 32];
        OsRng.try_fill_bytes(&mut seed).unwrap();
        let seed_b32: B32 = seed.into();
        let sk: SigningKey<MlDsa44> = SigningKey::from_seed(&seed_b32);
        let pk: [u8; PUBKEY_LEN] = sk.verifying_key().encode().into();
        (sk, pk)
    }

    fn make_fake_binary(magic_offset: usize, build_id: u64, flags: u8,
                        pk: &[u8; PUBKEY_LEN]) -> Vec<u8> {
        let mut bin = vec![0xAAu8; magic_offset + RECORD_LEN + 1024];
        bin[magic_offset..magic_offset + MAGIC_LEN].copy_from_slice(&MAGIC);
        bin[magic_offset + 8 .. magic_offset + 16]
            .copy_from_slice(&build_id.to_le_bytes());
        bin[magic_offset + 21] = flags;
        bin[magic_offset + 32 .. magic_offset + 32 + PUBKEY_LEN].copy_from_slice(pk);
        bin
    }

    fn sign_in_place(bin: &mut [u8], sk: &SigningKey<MlDsa44>, magic_offset: usize) {
        let sig_start = magic_offset + SIGNATURE_OFFSET;
        let sig_end   = sig_start + SIGNATURE_LEN;
        for b in &mut bin[sig_start..sig_end] { *b = 0; }
        let sig = sk.sign(bin);
        let sig_bytes: [u8; SIGNATURE_LEN] = sig.encode().into();
        bin[sig_start..sig_end].copy_from_slice(&sig_bytes);
    }

    #[test]
    fn round_trip() {
        let (sk, pk) = fresh_keypair();
        let mut bin = make_fake_binary(0x100, 42, 0, &pk);
        sign_in_place(&mut bin, &sk, 0x100);

        let v = verify_pending(&bin, &pk).expect("verify");
        assert_eq!(v.build_id, 42);
        assert_eq!(v.flags, 0);
    }

    #[test]
    fn signed_with_downgrade_flag_round_trips() {
        let (sk, pk) = fresh_keypair();
        let mut bin = make_fake_binary(0x100, 7, 0x01, &pk);
        sign_in_place(&mut bin, &sk, 0x100);

        let v = verify_pending(&bin, &pk).expect("verify");
        assert_eq!(v.build_id, 7);
        assert_eq!(v.flags, 0x01);
    }

    #[test]
    fn tampered_byte_outside_sig_rejected() {
        let (sk, pk) = fresh_keypair();
        let mut bin = make_fake_binary(0x100, 42, 0, &pk);
        sign_in_place(&mut bin, &sk, 0x100);

        bin[0] ^= 0x01;       // flip a bit outside the signature slot
        assert!(matches!(verify_pending(&bin, &pk),
                         Err(VerifyError::SignatureMismatch)));
    }

    #[test]
    fn tampered_sig_rejected() {
        let (sk, pk) = fresh_keypair();
        let mut bin = make_fake_binary(0x100, 42, 0, &pk);
        sign_in_place(&mut bin, &sk, 0x100);

        let sig_start = 0x100 + SIGNATURE_OFFSET;
        bin[sig_start] ^= 0x01;
        assert!(matches!(verify_pending(&bin, &pk),
                         Err(VerifyError::SignatureMismatch)));
    }

    #[test]
    fn tampered_flags_byte_rejected() {
        // The flags byte is inside the signed region. Flipping it after
        // signing must invalidate the signature, so an attacker cannot
        // turn on DOWNGRADE_PERMITTED by editing the binary.
        let (sk, pk) = fresh_keypair();
        let mut bin = make_fake_binary(0x100, 42, 0, &pk);
        sign_in_place(&mut bin, &sk, 0x100);

        bin[0x100 + 21] = 0x01;
        assert!(matches!(verify_pending(&bin, &pk),
                         Err(VerifyError::SignatureMismatch)));
    }

    #[test]
    fn tampered_build_id_rejected() {
        // Same defence for build_id — flipping it after signing must
        // invalidate the signature, so an attacker cannot lift a low
        // build_id past the runner's downgrade gate.
        let (sk, pk) = fresh_keypair();
        let mut bin = make_fake_binary(0x100, 1, 0, &pk);
        sign_in_place(&mut bin, &sk, 0x100);

        bin[0x100 + 8 .. 0x100 + 16].copy_from_slice(&999u64.to_le_bytes());
        assert!(matches!(verify_pending(&bin, &pk),
                         Err(VerifyError::SignatureMismatch)));
    }

    #[test]
    fn wrong_key_rejected() {
        let (sk_signer, _) = fresh_keypair();
        let (_, pk_other)  = fresh_keypair();
        let pk_signer: [u8; PUBKEY_LEN] = sk_signer.verifying_key().encode().into();
        let mut bin = make_fake_binary(0x100, 42, 0, &pk_signer);
        sign_in_place(&mut bin, &sk_signer, 0x100);

        assert!(matches!(verify_pending(&bin, &pk_other),
                         Err(VerifyError::SignatureMismatch)));
    }

    #[test]
    fn missing_magic_rejected() {
        let (_, pk) = fresh_keypair();
        let junk = vec![0u8; 8 * 1024];
        assert!(matches!(verify_pending(&junk, &pk),
                         Err(VerifyError::MagicMissing)));
    }

    #[test]
    fn unsigned_binary_rejected() {
        // .pending file with all-zero signature slot but a real magic block.
        // An all-zero signature may decode to a malformed-but-parseable
        // value (BadSignature) or to a parseable-but-invalid one
        // (SignatureMismatch) depending on which check fires first; either
        // way the file does not get applied.
        let (_, pk) = fresh_keypair();
        let bin = make_fake_binary(0x100, 42, 0, &pk);
        // No sign_in_place call — sig slot stays zero.
        match verify_pending(&bin, &pk) {
            Err(VerifyError::BadSignature) | Err(VerifyError::SignatureMismatch) => (),
            other => panic!("unsigned binary should be rejected, got {:?}", other),
        }
    }

    #[test]
    fn unset_runner_public_key_rejects_everything() {
        let (sk, pk) = fresh_keypair();
        let mut bin = make_fake_binary(0x100, 42, 0, &pk);
        sign_in_place(&mut bin, &sk, 0x100);

        // The runner is configured with an all-zero pubkey (no signing
        // configured): every upgrade is rejected upfront.
        let zeros = [0u8; PUBKEY_LEN];
        assert!(matches!(verify_pending(&bin, &zeros),
                         Err(VerifyError::PublicKeyUnset)));
    }
}
