# A declared classifier meet

`meet.scala` defines a classifier trait that derives from two unrelated classifiers —
`Durable extends caps.SharedCapability` and `caps.SharedUnscoped` — and classifies a log sink
with it. On 3.9.1-dev-p19 every class in the hierarchy is rejected:

```
trait DurableUnscoped inherits two unrelated classifier traits: trait SharedUnscoped and trait Durable
```

although `CaptureOps.classifier` (the `leastClassifier` fold over the base classes) already picks
`DurableUnscoped` as the least classifier: the fold collapses to `Nothing` only when no base
class derives from both. The check in `Setup.checkClassifiedInheritance` is purely pairwise; the
fix accepts an unrelated pair when another classifier among the base classes derives from both.

    rep/compile.sh classifier-meet          # the pinned toolchain
    SOUNDNESS_SCALA_HOME=<release dir> rep/compile.sh classifier-meet

Needed by Soundness's task model: a task body may capture only `Durable` capabilities, and the
log sinks, network access and transport observers it must be able to use are also level-exempt
(`SharedUnscoped`). Without a meet they cannot be both, and a durable holder (`rudiments.Loop`)
cannot retain a level-exempt one.
