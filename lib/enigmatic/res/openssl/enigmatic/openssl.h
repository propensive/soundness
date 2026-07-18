// The OpenSSL libcrypto surface the `OpensslCrypto` provider navigates: randomness
// (RAND_bytes), one-shot HMAC, and the EVP symmetric-cipher API. Parsed by xenophile's
// `CHeaderDialect` at compile time; each call is materialized as a Panama downcall on the
// JVM and a direct C call on Scala Native.
int RAND_bytes(unsigned char* buf, int num);

const EVP_MD* EVP_sha1(void);
const EVP_MD* EVP_md5(void);
const EVP_MD* EVP_sha256(void);
const EVP_MD* EVP_sha384(void);
const EVP_MD* EVP_sha512(void);
unsigned char* HMAC(const EVP_MD* md, const void* key, int key_len, const unsigned char* d,
    size_t n, unsigned char* out, unsigned int* out_len);

const EVP_CIPHER* EVP_get_cipherbyname(const char* name);
EVP_CIPHER_CTX* EVP_CIPHER_CTX_new(void);
void EVP_CIPHER_CTX_free(EVP_CIPHER_CTX* ctx);
int EVP_CIPHER_CTX_set_padding(EVP_CIPHER_CTX* ctx, int pad);
int EVP_EncryptInit_ex(EVP_CIPHER_CTX* ctx, const EVP_CIPHER* type, void* impl,
    const unsigned char* key, const unsigned char* iv);
int EVP_EncryptUpdate(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl,
    const unsigned char* in, int inl);
int EVP_EncryptFinal_ex(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl);
int EVP_DecryptInit_ex(EVP_CIPHER_CTX* ctx, const EVP_CIPHER* type, void* impl,
    const unsigned char* key, const unsigned char* iv);
int EVP_DecryptUpdate(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl,
    const unsigned char* in, int inl);
int EVP_DecryptFinal_ex(EVP_CIPHER_CTX* ctx, unsigned char* out, int* outl);
