# An entry which may not be there

Looking up an entry by its path can fail, and the type of `entry` says so. A method which
calls it must declare the error, or decide what an absent entry means.

```scala fuel
compile
frame
  label The failure is not accounted for
  errors show
  code
      def prologue(plays: Zipfile, name: Path on Zip): Zip.Entry = plays.entry(name)
frame
  label It is declared
  highlight raises..Zip.Error  the caller decides
  code
      def prologue(plays: Zipfile, name: Path on Zip): Zip.Entry raises Zip.Error =
        plays.entry(name)
frame
  label Or it becomes an absent value
  highlight safely  `Unset` if it is not there
  code
      def prologue(plays: Zipfile, name: Path on Zip): Optional[Zip.Entry] =
        safely(plays.entry(name))
##
```

[ZIP](../../soundness/modules/archives.md#zip) lists the reasons a `Zip.Error` can give.
