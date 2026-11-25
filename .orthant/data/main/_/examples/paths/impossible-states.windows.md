# A name which Windows reserves

Windows keeps certain names for its devices, and a file cannot be called `aux` or `con`. The
rules of the platform are checked as the code compiles, where the name is written.

```scala fuel
compile
frame
  label A device name
  errors show
  code
      val notes = (Drive('C') / "Illyria" / "aux").on[Windows]
frame
  label Any other name
  highlight "auxiliary"  not a device
  code
      val notes = (Drive('C') / "Illyria" / "auxiliary").on[Windows]
frame
  label The same name is fine on Linux
  highlight Linux  a platform with no such rule
  code
      val notes = (% / "illyria" / "aux").on[Linux]
##
```

[Platforms](../../soundness/modules/paths.md#platforms) describes `Local`, whose rules are
the strictest of all.
