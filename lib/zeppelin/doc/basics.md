### Creating a ZIP file

A ZIP file can be constructed from an existing file by passing it to the `ZipFile` constructor.
Provided there is an appropriate `GenericPathReader` and `GenericPathMaker` (from
[Anticipation](https://github.com/propensive/anticipation)) in scope, any file representation (such
as `java.io.File`) may be used. For example,
```scala
import zeppelin.*
import diuretic.*
val file: java.io.File = java.io.File("/home/work/data.zip")
val zip = ZipFile(file)
```
or,
```scala
import zeppelin.*
import anticipation.fileApi.galileiApi
val file: galilei.DiskPath = Unix / p"home" / p"work" / p"data.zip"
val zip = ZipFile(file)
```

`ZipFile` provides several methods for working with the file.

#### Reading entries from a ZIP file

To read every entry from a `ZipFile`, call `ZipFile#entries()`. This will return a `LazyList[ZipEntry]`, a stream
of `ZipEntry`s in the order they are stored within the file, each one consisting of a `Relative` path (relative to
the root of the ZIP file) and a method to get its contents.

`ZipEntry`s support [Turbulence](https://github.com/propensive/turbulence/)'s `Readable` interface, so they can be
read as any type for which an `Aggregable` instance exists.

#### Appending files to an existing ZIP file

To add additional entries to a `ZipFile`, use `ZipFile#append`, which takes a
`LazyList` of `ZipEntry`s to append.

This method includes two additional parameters: a `prefix`, a `Bytes`
(`IArray[Byte]`) value to be inserted in raw form at the start of the ZIP file,
typically used to make the ZIP file executable; and a `timestamp` value for
specifying the timestamp of each entry appended to the ZIP file. If no
`timestamp` is specified, the current time will be used.



