# A tag which may not be a version

`as[Semver]` raises a `Semver.Error` when text is not a semantic version, and leaves the
consequence to its caller. The same expression gives an error to pass on, a version to fall
back to, or no version at all.

```scala fuel
compile
alternative Declared
  highlight raises..Semver.Error  the caller must answer for it
  code
      def release(tag: Text): Semver raises Semver.Error =
        tag.as[Semver]
alternative Replaced
  highlight recover  an unreadable tag counts as no release yet
  code
      def release(tag: Text): Semver =
        recover:
          case Semver.Error(_, _) => v"0.0.0"
        . protect(tag.as[Semver])
alternative Absent
  highlight safely  a failure becomes `Unset`
  code
      def release(tag: Text): Optional[Semver] =
        safely(tag.as[Semver])
##
```

[Versions](../../soundness/modules/versioning.md#versions) shows the error, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the full set of
answers.
