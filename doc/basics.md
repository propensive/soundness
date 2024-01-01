Surveillance watches directories for changes to their contents, through an extension method, `watch()`, on a
type representing a directory or a `Seq` of such types. For [Galilei](https://github.com/propensive/galilei)'s
`Directory` type, this works straight away. Other libraries which provide directory-like types can integrate with
Surveillance just by defining two simple [typeclass instances](#defining-typeclass-instances).

### Watching

A simple setup for watching a directory looks like this:
```scala
import galilei.*
import serpentine.*
import surveillance.*

val dir = (Unix / p"home" / p"work" / p"updates").directory()
val watcher = dir.watch()
```

Constructing a new `Watcher` on a directory will register that directory with the filesystem's filewatching service
and start a new thread to respond to updates.

The most important method of a `Watcher` is its `stream` method, which will return a `LazyList[WatchEvent]`



