## Git

### About

Soundness drives [Git](https://git-scm.com/) as typed operations on a repository opened for a
scope. A branch, a tag and a commit hash are distinct types rather than interchangeable strings;
results come back as values rather than text to parse; and the API is honest about the one thing
every Git library has to be honest about: the repository is on disk, and other processes write it.

Underneath, the operations are carried out by a backend. The command-line backend runs the `git`
command, so anything Git can do is available; the native backend reads and writes the repository's
files itself, and needs no `git` at all. Both implement the same `Git.Backend` typeclass, and the
choice is an import: `gitBackends.commandlineGit` or `gitBackends.soundnessGit`.

### On Git

Git's real interface is its command line, built for a person at a shell. Driving it from a program
means assembling command strings, parsing text output by hand, and treating a branch name, a tag
name and a commit hash alike until Git rejects one. Worse, every program that drives Git races with
every other: a ref read now may have moved by the time it is written.

Repository operations that report their failures in their types, cannot be run without the
capabilities they need, and cannot pretend a reading is still current, are [honest
signatures](../philosophy/honest-signatures.md) for version control.

Everything comes from the `soundness` package, with a backend selected and the capabilities the
operations need in scope:

```scala
import soundness.*

import codepages.utf8Codepage
import filesystemOptions.createNonexistentParents
import gitBackends.commandlineGit
import gitCommands.searchpathGitCommand
import internetAccess.online
import logging.silentLogging
import pathInterfaces.pathOnLinux
import strategies.throwUnsafely
import temporaryDirectories.javaBaseTemporaryDirectory
import workingDirectories.javaBaseWorkingDirectory
import environments.javaBaseEnvironment
import filesystemBackends.javaBaseFilesystem

val directory = temporaryDirectory[Path on Linux] / "git" / Uuid().show
directory.create[Directory]()
```

### Opening a repository

A repository is created with `Git.init`, and opened, for the duration of a block, as one of two
forms. `Git.Repo` is the object database and its refs, which a bare repository has; `Git.Worktree`
is a checked-out tree, with an index and working files, over its repository. The mode names the
grants the handle carries: a handle opened with `Read` alone cannot be used to write, which is a
compile-time error rather than a runtime one.

```scala
Git.init(directory, initialBranch = Git.Branch(t"main"))

directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.head().value
```

A `Git.Worktree.Handle` is also a `Git.Repo.Handle`, so every repository operation is available on
it, and it is a directory handle: paths formed from it, such as `worktree / "notes.txt"`, belong to
this handle alone, cannot express `..`, and may be read and written with the filesystem operations
under the same grants.

### Observing a repository that others change

Every read that resolves a ref, HEAD, the index or the working files returns a `Stale` value: what
it holds was true at the moment of the read, and may have changed since. `head()` reads where HEAD
is; the `Stale[Git.Head]` it returns is the proof of what was seen.

```scala
directory.open[Git.Worktree](): worktree ?=>
  val head: Stale[Git.Head] = worktree.head()
  head.value match
    case Git.Head.Attached(branch, tip) => t"on $branch"
    case Git.Head.Detached(at)          => t"detached at $at"
```

Every write that moves a ref takes the `Stale` value it is conditional on, and fails, with
`Git.Error(Moved(ref, expected, actual))`, if the ref no longer holds it. This is Git's own
compare-and-swap, `git update-ref ref new old`, made unavoidable: a commit cannot overwrite work
another process committed between the read and the write.

### Making a commit

Staging and committing are methods on the worktree. `commit` takes the reading of HEAD it is
conditional on and returns the new commit's hash:

```scala
directory.open[Git.Worktree](Read & Write): worktree ?=>
  val head = worktree.head()
  (worktree / "notes.txt").overwrite(t"Remember the milk")
  worktree.add(worktree / "notes.txt")
  worktree.commit(t"Add notes", head)
```

The commit object is assembled by Soundness and stored, rather than made by `git commit`, so
signing (`signer = …`) never leaves the process, and the same code serves both backends. A commit
whose tree is unchanged raises `NothingToCommit` unless `allowEmpty = true`; `amend` rewrites the tip.

### Inspecting history

`log` yields the commits as a list, newest first, each a `Commit` with its hash, author, parents and
message. It is `Stale`: the branch it follows may move.

```scala
directory.open[Git.Worktree](): worktree ?=>
  worktree.log().value.map(_.message)   // List(t"Add notes")
```

Objects are immutable and named by their content, so a read keyed by a hash is a plain value:
`commitAt`, `treeAt`, `blobAt` and `show` need no `Stale`.

### Branches and tags

Branches and tags are created, listed and switched between with typed references. A branch is
created at a commit; switching takes the reading of HEAD it moves from.

```scala
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
  worktree.switch(Git.Branch(t"main"), worktree.head())
  worktree.branches().value.keys
```

Deleting a branch or a tag takes the reading of its tip, so a branch that moved since it was
listed is not deleted by mistake. A lightweight tag is `makeTag`; `annotatedTag` writes a tag object
with a message, and optionally a signature.

### Cloning, fetching and pushing

An operation that reaches a remote requires the `Online` capability. It blocks until it is done,
and reports its progress through a `Git.Activity`: a shared tracker of atomic counters, updated as
objects arrive, which a user interface may poll from another thread with `report()`.

```scala
val target = directory / "clone"
Git.clone(directory, target)

target.open[Git.Worktree](): cloned ?=>
  cloned.log().value.size   // 1
```

`push` takes refspecs and a force policy. `Push.Force.Lease` is the remote-side compare-and-swap,
`--force-with-lease`: it carries the readings of the remote's refs the push is conditional on.

### Choosing a backend

`gitBackends.commandlineGit` runs the `git` command found by `gitCommands.searchpathGitCommand`,
in the working directory and environment in scope, logging each invocation. It is the choice when
git's own behaviour is wanted in full: hooks, filters, credential helpers, submodules.

`gitBackends.soundnessGit` is the native backend: objects (loose and packed), refs, the index,
configuration, notes, linked worktrees, merging, rebasing and the stash are all read and written
directly, in git's own formats, so a repository is always one `git` itself understands, and an
operation one side stops on a conflict the other can continue. It reaches repositories on the
same filesystem by path; remote ones need a transport, which the `http` and `ssh` components
supply: `Native(List(SmartHttp(), SshCommand()))` speaks git's wire protocol over whichever
`Http.Backend` is in scope and over a spawned `ssh`.

```scala fuel
typecheck
##
import gitTransports.{smartHttp, sshCommand}
import httpBackends.javaNetHttp

given Git.Backend = Native(List(SmartHttp(), SshCommand()))
```

Signing is independent of the backend. A `Git.Signer` makes the signature a commit or tag
carries, and a `Git.Verifier` checks one: `SshKeygen.Signer(key)` and `SshKeygen.Verifier(
allowedSigners)` drive `ssh-keygen -Y` as `gpg.format = ssh` has git do, and `Gpg.Signer()` and
`Gpg.Verifier()` drive `gpg`. A tag signed through either is one `git tag -v` accepts.

Whichever backend is chosen, `Exclusive` arbitrates only between handles in this JVM. Another
process is never waited for, which is why every ref-moving operation is a compare-and-swap on the
`Stale` reading it was given.

### References

A `Git.Branch`, `Git.Tag` and `Git.Hash` name the three kinds of reference, and a `Git.Revision` is
any of them or a relative expression such as `Git.Revision.head(1)` for `HEAD~1`. Because each is
its own type, an operation that expects a branch will not accept a tag, and a hash carries the
guarantee that it is a well-formed forty-character identifier.

A `Git.Ref` is a fully-qualified reference path, `refs/heads/main`, modelled as a path on the
`Git.Refs` plane. Reference names follow Git's own rules — no leading dot, no `..`, no trailing
`.lock`, and the rest — which are checked when a name is parsed from text, with `Git.Branch.parse`,
`Git.Tag.parse` or `Git.Refs.heads`, and reported as a `Git.RefError`.

### Diffs

`diff` reports changes as structured values rather than as patch text to re-parse. Each `FileDiff`
carries the paths on both sides, the kind of change — added, modified, deleted, renamed, copied —
and its hunks, each hunk a list of edits.

```scala
directory.open[Git.Worktree](Read & Write): worktree ?=>
  (worktree / "notes.txt").overwrite(t"Remember the milk and the eggs")
  val files = worktree.diff().value
  files.head.let(_.changeKind)   // ChangeKind.Modified
```

`diffStaged` compares the index with HEAD, `diffAgainst` the working files with a revision, and
`diffBetween` two commits. The same parser, `Patch.parse`, reads a patch from anywhere.

### Merging, cherry-picking, reverting and rebasing

`merge` takes the revision to merge, the reading of HEAD, and a fast-forward policy, so whether a
merge commit is created is stated rather than inherited from configuration. Its outcome is a value:
`FastForwarded`, `Merged`, `UpToDate`, or `Stopped`.

```scala
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.merge(Git.Branch(t"feature"), worktree.head(), ff = FastForward.Only)
```

A conflict is not an error. The operation stops, and the `Stopped` outcome carries a
`Git.Operation` naming the paths in conflict; once they are resolved and added, `continue()`
finishes it, `skip()` passes over the commit, and `abandon()` restores the state the operation
began from. `cherryPick`, `revert` and `rebase` have the same shape, and `stash` saves and
restores uncommitted work.

### Remotes, configuration, notes and the reflog

Remotes are added, listed and removed as values. Configuration is read and written through a lent
handle, typed by the value's codec: `repo.config { config ?=> config.get[Text](key) }`. Notes
attach text to a commit without altering it, in a chosen namespace; a note write is conditional
on a reading of the notes ref, which is how build attestations and other out-of-band metadata
travel with a repository without one writer clobbering another.

```scala
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.addRemote(t"origin", t"git@example.com:foo/bar.git")
  val hash = worktree.head().value.commit.or(Git.Hash.zero)
  val notes = worktree.ref(Git.Refs.defaultNotes)
  worktree.addNote(hash, t"a note body", notes)
  worktree.note(hash).value   // Unset where there is no note
```

`reflog` gives the local history of where a reference has pointed, newest first — the record that
makes a mistaken reset recoverable.
