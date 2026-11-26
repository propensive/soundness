# A retry without a loop

Work which fails for a passing reason is tried again by `retry`, and the schedule it is
retried on is not written at the call. It is a value in scope, chosen by an import, so the
block says only which failures are worth another attempt.

```scala fuel
compile
alternative Ten times, backing off
  highlight exponentialTenTimesTenacity
  code
      import retryTenacities.exponentialTenTimesTenacity
      import threads.virtualThreads, strategies.throwUnsafely

      def knock(): Optional[Text] = Unset

      def porter(): Text = supervise:
        retry: (surrender, persevere) ?=>
          knock().or(persevere())
alternative Five times, at once
  highlight fixedNoDelayFiveTimesTenacity
  code
      import retryTenacities.fixedNoDelayFiveTimesTenacity
      import threads.virtualThreads, strategies.throwUnsafely

      def knock(): Optional[Text] = Unset

      def porter(): Text = supervise:
        retry: (surrender, persevere) ?=>
          knock().or(persevere())
##
```

The example is compiled, and is not run, since it would wait between its attempts.
[Retrying](../../soundness/modules/concurrency.md#retrying) lists the schedules, and says what
happens when the attempts run out.
