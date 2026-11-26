# A configuration file which may be wrong

Decoding a configuration raises a `Yaml.Error` when a key is missing or mistyped. The program
which reads it decides what that means: no configuration, the defaults, or its caller's
problem. The decoder is the same in each.

```scala fuel
compile
alternative No configuration
  highlight safely  a failure becomes Unset
  code
      case class Stage(width: Int, traps: Int)

      def stage(yaml: Yaml): Optional[Stage] = safely(yaml.as[Stage])
alternative The defaults
  highlight recover  a failure becomes a value
  code
      case class Stage(width: Int, traps: Int)

      def stage(yaml: Yaml): Stage =
        recover:
          case error: Yaml.Error => Stage(43, 1)
        . protect(yaml.as[Stage])
alternative Another error
  highlight mitigate  a failure becomes the caller's kind of failure
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class StageError()(using Diagnostics) extends Error(m"the stage is not set")
      case class Stage(width: Int, traps: Int)

      def stage(yaml: Yaml): Stage raises StageError =
        mitigate:
          case error: Yaml.Error => StageError()
        . protect(yaml.as[Stage])
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) and
[recovering](../../soundness/modules/errors.md#recovering) have the full set.
