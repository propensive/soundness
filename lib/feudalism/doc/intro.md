A _mutex_ is a data structure designed for safe reading and writing to a
mutable variable in a concurrent environment. Specifically, a mutex variable
may be mutated (that is, its old value read and transformed into a new value)
so long as no other threads are reading or writing to the mutex at the same
time. However, any number of threads may read the mutex variable concurrently.

Feudalism implements a generic `Mutex` type which guarantees these constraints.

