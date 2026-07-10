## Git

### About

Soundness drives [Git](https://git-scm.com/) as typed operations. A repository is a value on which
methods like `commit`, `checkout`, `merge` and `log` are called; a branch, a tag and a commit hash
are distinct types rather than interchangeable strings; and results come back as values — a commit
with its author and message, a list of branches — rather than text to parse. Operations that reach a
remote run asynchronously, reporting their progress as they go.

Underneath, the operations run the `git` command, so anything Git can do is available; what Soundness
adds is that the commands are methods, the references are typed, and the output is structured.

### On Git

Git's real interface is its command line, built for a person at a shell. Driving it from a program
means assembling command strings, and a mistyped subcommand or a malformed ref is a runtime failure;
the output is text in a format that must be parsed by hand; and nothing distinguishes a branch name
from a tag name from a commit hash until Git rejects one.

Soundness wraps the command line in a typed API. Each operation is a method that names its
requirements and the errors it can raise; a `GitBranch`, `GitTag` and `GitHash` are separate types,
so one cannot be passed where another is meant; and the output of `log`, `status` or `diff` is parsed
into values. Everything comes from the `soundness` package, with the `git` command located and the
capabilities the operations need in scope:

```scala
import soundness.*
import gitCommands.environmentDefaultGitCommand
import workingDirectories.defaultWorkingDirectory
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
```

### Opening or creating a repository

An existing repository opens with `GitRepo.at`, and a new one is created with `Git.init`, which
returns a `Worktree` — a repository together with a working tree:

```scala
val worktree = Git.init(directory, initialBranch = GitBranch(t"main"))
```

`Git.initBare` creates a bare repository, one with no working tree, returning a `GitRepo`.

### Making a commit

Staging and committing are methods on the worktree; the resulting commit's hash is read back by
resolving `HEAD`:

```scala
worktree.add(worktree.path/t"notes.txt")
worktree.commit(t"Add notes")
val hash = worktree.repo.revParse(Refspec.head())
```

### Inspecting history

`log` yields the commits as a stream, newest first, each a `Commit` with its hash, author, parents and
message:

```scala
worktree.repo.log().map(_.message.head).to(List)
// List(t"Add notes", …)
```

### Branches and tags

Branches and tags are created, listed and switched between with typed references. Merging takes the
reference to merge and a fast-forward policy:

```scala
worktree.makeBranch(GitBranch(t"feature"))
worktree.checkout(GitBranch(t"main"))
worktree.merge(GitBranch(t"feature"), ff = FastForward.Never, message = t"Merge feature")
```

### Cloning, pulling and pushing

An operation that reaches a remote — cloning, fetching, pulling, pushing — requires the `Online`
capability and runs asynchronously, returning a process whose progress can be observed and whose
result is taken with `complete`:

```scala
val cloned = Git.clone(source, target).complete()
cloned.repo.log().to(List)
```

### References

A `GitBranch`, `GitTag` and `GitHash` name the three kinds of reference, and a `Refspec` is any of
them or a relative expression such as `Refspec.head()` for `HEAD`. Because each is its own type, an
operation that expects a branch will not accept a tag, and a hash carries the guarantee that it is a
well-formed forty-character identifier.
