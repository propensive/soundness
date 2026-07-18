// A tiny slice of libc, for the Scala Native FFI smoke test: nullary functions
// returning a C primitive (what xenophile's NativeInvoke v1 materializes).
int rand(void);
int getpid(void);
int abs(int n);
double pow(double base, double exp);
