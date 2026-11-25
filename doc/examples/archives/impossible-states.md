# An entry which cannot leave its archive

The classic attack on an archive is an entry named `../../etc/passwd`. The path of a tar entry
is not text. It is built one name at a time, relative to the root of the archive, and a name
which climbs out of the archive is not one the compiler accepts.

```scala fuel
compile
frame
  label A path within the archive
  highlight ?  the root of the archive
  code
      import codepages.utf8Codepage

      val scene = Tar.Entry(? / "act1" / "scene1.txt", t"Illyria".in[Data])
frame
  label A path which climbs out of it
  errors show
  code
      import codepages.utf8Codepage

      val scene = Tar.Entry(? / ".." / "etc" / "passwd", t"Illyria".in[Data])
frame
  label A separator, hidden in a name
  errors show
  code
      import codepages.utf8Codepage

      val scene = Tar.Entry(? / "act1/../../etc", t"Illyria".in[Data])
##
```

[Tar](../../soundness/modules/archives.md#tar) describes entries, and
[paths](../../soundness/modules/paths.md) the checks which every kind of path makes on its
names.
