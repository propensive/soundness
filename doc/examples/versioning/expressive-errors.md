# Why text is not a version

When text is not a semantic version, the `Semver.Error` which is raised holds the reason as a
value of an enumeration. Code which handles the error matches on the reason, and has no
message to search.

```scala fuel
compile
frame
  label Any failure
  code
      def verdict(tag: Text): Text =
        recover:
          case Semver.Error(_, _) => t"not a version"
        . protect(t"version ${tag.as[Semver]}")
frame
  label One failure in particular
  highlight Semver.Error.Reason.LeadingZero  a case of an enumeration
  code
      def verdict(tag: Text): Text =
        recover:
          case Semver.Error(_, reason) => reason match
            case Semver.Error.Reason.LeadingZero => t"remove the zero"
            case _                               => t"not a version"
        . protect(t"version ${tag.as[Semver]}")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(verdict(t"1.4.2"))
Out.println(verdict(t"1.04.2"))
Out.println(verdict(t"first folio"))
```

[Versions](../../soundness/modules/versioning.md#versions) shows the error as a message, and
[expressive errors](../../philosophy/expressive-errors.md) why it is also data.
