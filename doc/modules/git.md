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

Reference names follow Git's own rules — no leading dot, no `..`, no control characters, no
trailing `.lock`, and the rest — checked as [names](names.md) on their own plane, so an invalid
branch name is rejected where it is written rather than by Git at the point of use.

### Diffs

`diff` reports changes as structured values rather than as patch text to re-parse. Each `FileDiff`
carries the paths on both sides, the kind of change — added, modified, deleted, renamed, copied —
and its hunks, each hunk a list of edits:

```scala
val files = worktree.diff().to(List)
files.head.changeKind      // ChangeKind.Modified
files.head.hunks.flatMap(_.edits)
```

The same parser reads a patch from anywhere — a file, an email, a code-review comment — so a tool
that inspects or applies patches works with values, including the awkward cases: renames with
similarity indices, binary files, mode changes, and files with no trailing newline.

### Merging, cherry-picking and reverting

`merge` takes the reference to merge and a fast-forward policy, so whether a merge commit is
created is stated rather than inherited from configuration:

```scala
worktree.merge(GitBranch(t"feature"), ff = FastForward.Only)
worktree.merge(GitBranch(t"feature"), ff = FastForward.Never, message = t"Merge feature")
```

`cherryPick` and `revert` apply and undo a single commit's changes. A conflict is a typed failure
naming the paths that conflict, rather than a non-zero exit status and a message to parse.

### Remotes, notes and the reflog

Remotes are added, listed and removed as values, each reporting its name and URL. Notes attach
arbitrary text to a commit without altering it, in a chosen namespace, which is how build
attestations, review state and other out-of-band metadata travel with a repository:

```scala
worktree.repo.addRemote(t"origin", t"git@example.com:foo/bar.git")

worktree.repo.notes.add(hash, t"a note body")
worktree.repo.notes.show(hash)   // Unset where there is no note
```

`reflog` gives the local history of where a reference has pointed, newest first — the record that
makes a mistaken reset recoverable.
