All terms and types are provided in the `octogenarian` package:
```scala
import octogenarian.*
```

### Types

Octogenarian introduces various types representing entities relevant to Git.

A `GitRepo` is an immutable value representing a Git repository, defined only by its filesystem location: its
Git directory, and optionally, its working tree. The contents of the repository are not represented by a
`GitRepo`, and the API presumes that the repository may be changed by other processes.

The opaque types, `Tag`, `Branch` and `CommitHash` represent tags, branches and commit hashes within a Git
repository, and all are subtypes of `Refspec`. Instances of these types may be returned by various Git
operations, and may be used as parameters to some operations.

`Commit` describes the details of a commit, including its hash, tree, parent(s), author, committer, signature
and message.

`SshUrl` is used to specify a SSH URL pointing to a remote Git repository.

Errors are represented by `GitError`s, for operational problems, and `GitRefErrors`, for reference validation
failures.

Finally, `GitProcess` is a mutable type representing a long-running Git process, and can be used to capture
progress updates as well as the final result of an operation.

### Getting a `GitRepo`

A `GitRepo` can be created from an existing repository on the filesystem, initializing a new one, or by cloning
a remote URL.

If the repository is already on disk, constructing it is as simple as invoking the factory, `GitRepo(path)`,
where `path` is some representation of the path on the filesystem, as long as an appropriate
[Anticipation](https://github.com/propensive/anticipation/) `GenericPath` typeclass is available. If using
[Galilei](https://github.com/propensive/galilei/) `Path`s, then it is.

A new, empty repository can be created with `Git.init(path)`, specifying a nonexistent path on the filesystem
to create it. The optional `bare` parameter, defaulting to `false`, makes it possible to create a bare Git
repository, i.e. without a working tree.

A repository can also be cloned from an existing repository, either remotely or locally. The `Git.clone` method
will clone a repository from a URL, a local path or a `SshUrl`, using any Anticipation-aware types, for example:
```scala
import nettlesome.*
import galilei.*

val repo = Git.clone(
  url"https://github.com/propensive/octogenarian",
  % / p"home" / p"work" / p"octogenarian",
  branch = Branch(t"main")
)
```

A further variant of `clone` exists which allows a single `Commit` value to be specified, `cloneCommit`.

### Operations on a `GitRepo`

Various operations are available on an instance of a `GitRepo`. These generally follow the naming of the Git
command-line subcommands, but a single `git` subcommand is sometimes split into more than one method:
 - `checkout`, checks out a `Tag`, `Branch` or `CommitHash`
 - `pull`, pulls from the default remote
 - `switch`, changes the current branch
 - `fetch`, fetches from the specified remote
 - `commit`, commits the currently-staged changes with the specified message
 - `branches`, lists the branches
 - `makeBranch`, creates a new branch
 - `tags`, lists the repository tags
 - `log`, reads changes to the repository as a stream of `Commit`s
 - `pushTags`, pushes tags to the remote

Implementations of other commands remain incomplete, and should be added in due course.

### Asynchronous operations

Certain operations, notably those involving fetching, will run asynchronously, and their progress can be
tracked. Asynchronous operations will return a `GitProcess` instance. Calling its `complete` method will return
its result synchronously, as soon as it finishes. But updates on its progress can be obtained as a
`LazyList[Progress]` with its `progress` member.

