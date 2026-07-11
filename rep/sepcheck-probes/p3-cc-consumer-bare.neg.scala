// P3 cascade (go/no-go): a consumer compiled with ONLY captureChecking (no separation
// import — the repo-wide default) holding a bare reference to the kernel's Mutable type.
// Expectation: the read-only discipline is CC-level and fires anyway — i.e. re-parenting
// zephyrine types cascades to ALL consumers regardless of per-module sepcheck gating.
// If this probe unexpectedly COMPILES, the cascade is softer than planned and Phases 3-4
// get cheaper.
//LIB: p3-kernel.lib.scala
//EXPECT: update method|read-only
import language.experimental.captureChecking

import sepprobe.Kernel

def consume(k: Kernel): Unit = k.bump()
