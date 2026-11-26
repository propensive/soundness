# When the attempts run out

`retry` tries a block again on a schedule, and raises a `Tenacity.Error` when the schedule is
exhausted. What that means is for the code around it to say, and each answer gives the method
a different type.

```scala fuel
compile
alternative An absent value
  highlight safely  five failures are an absent value
  code
      import retryTenacities.fixedNoDelayFiveTimesTenacity

      def knock(): Optional[Text] = Unset

      def porter()(using Monitor): Optional[Text] = safely:
        retry: (surrender, persevere) ?=>
          knock().or(persevere())
alternative A replacement
  highlight recover  five failures are another answer
  code
      import retryTenacities.fixedNoDelayFiveTimesTenacity

      def knock(): Optional[Text] = Unset

      def porter()(using Monitor): Text =
        recover:
          case error: Tenacity.Error => t"nobody came"
        . protect:
            retry: (surrender, persevere) ?=>
              knock().or(persevere())
alternative The caller's decision
  highlight raises  five failures are the caller's to handle
  code
      import retryTenacities.fixedNoDelayFiveTimesTenacity

      def knock(): Optional[Text] = Unset

      def porter()(using Monitor): Text raises Tenacity.Error =
        retry: (surrender, persevere) ?=>
          knock().or(persevere())
##
```

The example is compiled, and is not run.
[Retrying](../../soundness/modules/concurrency.md#retrying) describes the schedules, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the answers.
