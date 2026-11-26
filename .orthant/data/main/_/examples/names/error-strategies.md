# What an invalid name does

Validation raises a `Name.Error`, and does not decide what becomes of it. The code which
validates chooses, and the same `Name[Tag](text)` serves each choice.

```scala fuel
compile
alternative An absent value
  highlight safely  an invalid name is Unset
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotStart["0"] & MustNotContain["."] = !!

      def tag(text: Text): Optional[Name[Tag]] = safely(Name[Tag](text))
alternative A replacement
  highlight recover  an invalid name is replaced
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotStart["0"] & MustNotContain["."] = !!

      def tag(text: Text): Name[Tag] =
        recover:
          case error: Name.Error => n"untagged"
        . protect(Name[Tag](text))
alternative The caller's decision
  highlight raises..Name.Error  an invalid name is the caller's
  code
      sealed trait Tag

      inline given Tag is Nominative under MustNotStart["0"] & MustNotContain["."] = !!

      def tag(text: Text): Name[Tag] raises Name.Error = Name[Tag](text)
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) has the full set.
