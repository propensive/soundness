# An upgrade which may not have happened

The launcher records what it did with a candidate upgrade, and `Upgrade.outcome` reads the
record back. Usually there is none, so the outcome is optional, and a report of it is written
with `let`, which does nothing where there is nothing to report.

```scala fuel
compile
frame
  label The outcome, if there is one
  highlight Optional[Upgrade.Outcome]  usually absent
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem

      def outcome(): Optional[Upgrade.Outcome] = Upgrade.outcome
frame
  label A message about it, if there is one
  highlight let  runs only where there is an outcome
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem

      def report(): Optional[Message] = Upgrade.outcome.let: outcome =>
        m"${outcome.result} (build ${outcome.candidate.show})"
frame
  label Acknowledged, so that it is reported once
  highlight Upgrade.acknowledge()  clears the record
  code
      import environments.javaBaseEnvironment, systems.javaBaseSystem

      def report(): Optional[Message] = Upgrade.outcome.let: outcome =>
        Upgrade.acknowledge()
        m"${outcome.result} (build ${outcome.candidate.show})"
##
```

The example is compiled, and is not run, since it reads the state of an installed program.
[Staging an upgrade](../../soundness/modules/daemons.md#staging-an-upgrade) lists the results,
and [optionality](../../philosophy/optionality.md) describes `let`.
