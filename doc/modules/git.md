## Git

### About

[Git](https://git-scm.com/) is driven through typed operations on a repository which is opened
for a scope. A branch, a tag and a commit hash are distinct types, not interchangeable strings.
Results come back as values, not as text to parse. And the API is honest about the one thing
every Git library must be honest about: the repository is on disk, and other processes write to
it.

A backend carries out the operations. The command-line backend runs the `git` command, so
anything Git can do is available. The native backend reads and writes the files of the
repository itself, and needs no `git` at all. Both implement the same `Git.Backend`
[typeclass], and the choice is an import: `gitBackends.commandlineGit` or
`gitBackends.soundnessGit`.

### On Git

Git's real interface is its command line, built for a person at a shell. Driving it from a
program means assembling command strings, parsing text output by hand, and treating a branch
name, a tag name and a commit hash alike until Git rejects one. Worse, every program which
drives Git races with every other. A ref read now may have moved by the time it is written.

Repository operations here report their failures in their types, cannot run without the
[capabilities][capability] they need, and cannot pretend that a reading is still current. These
are [honest signatures](../philosophy/honest-signatures.md) for version control.

Everything comes from the `soundness` package. The other imports select a backend, bring the
capabilities which the operations need into scope, and let the samples print their results. The
samples work in a fresh repository under the temporary directory:

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
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val directory = temporaryDirectory[Path on Linux] / "git" / Uuid().show
directory.create[Directory](CreateFlag.Parents)
```

### Opening a repository

A repository is created with `Git.init`, and is opened, for the duration of a block, as one of
two forms. A `Git.Repo` is the object
database and its refs, which is what a bare repository has. A `Git.Worktree` is a checked-out
tree, with an index and working files, over its repository.

The mode names the grants which the handle carries. A handle opened with `Read` alone cannot be
used to write, and that is a compile error, not a runtime one:

```scala fuel
run
##
Git.init(directory, initialBranch = Git.Branch(t"main"))

directory.open[Git.Worktree](Read & Write): worktree ?=>
  Out.println(worktree.head().value.inspect)
```

The repository has no commits yet, so `HEAD` is attached to a branch with no tip.

A `Git.Worktree.Handle` is also a `Git.Repo.Handle`, so every repository operation is available
on it. It is a directory handle too. A path formed from it, such as `worktree / "notes.txt"`,
belongs to this handle alone, cannot express `..`, and may be read and written with the
[filesystem](filesystem.md) operations under the same grants.

### Observing a repository that others change

Every read which resolves a ref, `HEAD`, the index or the working files returns a `Stale` value.
What it holds was true at the moment of the read, and may have changed since. `head()` reads
where `HEAD` is, and the `Stale[Git.Head]` it returns is the proof of what was seen:

```scala fuel
run
##
directory.open[Git.Worktree](): worktree ?=>
  val head: Stale[Git.Head] = worktree.head()

  head.value match
    case Git.Head.Attached(branch, tip) => Out.println(t"on $branch")
    case Git.Head.Detached(at)          => Out.println(t"detached at $at")
```

Every write which moves a ref takes the `Stale` value it is conditional on. It fails with
`Git.Error(Moved(ref, expected, actual))` if the ref no longer holds that value. This is Git's
own compare-and-swap, `git update-ref ref new old`, made unavoidable. A commit cannot overwrite
work which another process committed between the read and the write.

### Making a commit

Staging and committing are methods on the worktree. `commit` takes the reading of `HEAD` it is
conditional on, and returns the hash of the new commit:

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  val head = worktree.head()
  (worktree / "notes.txt").overwrite(t"Remember the milk")
  worktree.add(worktree / "notes.txt")
  val hash = worktree.commit(t"Add notes", head)
  Out.println(hash.show.length.show)
```

The commit object is assembled here and stored, and is not made by `git commit`. So a signature
(`signer = …`) never leaves the process, and the same code serves both backends. A commit whose
tree is unchanged raises `NothingToCommit`, unless `allowEmpty = true`. `amend` rewrites the
tip.

### Inspecting history

`log` gives the commits as a list, newest first. Each is a `Commit` with its hash, author,
parents and message. The list is `Stale`, since the branch it follows may move:

```scala fuel
run
##
directory.open[Git.Worktree](): worktree ?=>
  worktree.log().value.each(commit => Out.println(commit.message))
```

Objects are [immutable] and named by their content, so a read keyed by a hash is a plain value:

```tabular
column  A read of
column  Such as
column  Returns
row  a ref, HEAD, the index or the working files  `head`, `ref`, `resolve`, `branches`, `tags`, `log`, `note`, `remotes`, `stashes`, `diff`  a `Stale` value
row  an object, by its hash  `show`, `commitAt`, `treeAt`, `blobAt`, `diffBetween`  a plain value
```

### Branches and tags

Branches and tags are created, listed and switched between with typed references. A branch is
created at a commit, and switching takes the reading of `HEAD` it moves from:

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
  worktree.switch(Git.Branch(t"main"), worktree.head())
  worktree.branches().value.keys.each(branch => Out.println(branch.show))
```

Deleting a branch or a tag takes the reading of its tip, so a branch which moved since it was
listed is not deleted by mistake. `makeTag` makes a lightweight tag. `annotatedTag` writes a tag
object with a message, and optionally a signature.

### Cloning, fetching and pushing

An operation which reaches a remote needs the `Online` capability. It blocks until it is done,
and reports its progress through a `Git.Activity`: a shared tracker of atomic counters, updated
as objects arrive, which a user interface may poll from another thread with `report()`.

```scala
import dysasymptotics.linearSize

val target = directory / "clone"
Git.clone(directory, target)
```

```scala fuel
run
##
target.open[Git.Worktree](): cloned ?=>
  Out.println(cloned.log().value.size.show)
```

`push` takes refspecs and a force policy. `Push.Force.Lease` is the compare-and-swap on the
remote side, `--force-with-lease`. It carries the readings of the remote's refs which the push
is conditional on.

### Choosing a backend

The two backends differ in how they do the work, and so in what each is for:

```tabular
column  Backend
column  Works by
column  Reaches remotes
column  The choice for
row  `gitBackends.commandlineGit`  running the `git` command  as `git` does  git's own behaviour in full: hooks, filters, credential helpers, submodules
row  `gitBackends.soundnessGit`  reading and writing git's formats directly  through a transport: `SmartHttp`, `SshCommand`  no dependency on `git`
```

`gitBackends.commandlineGit` runs the `git` command which `gitCommands.searchpathGitCommand`
finds, in the working directory and environment in scope, and logs each invocation.

`gitBackends.soundnessGit` is the native backend. Objects, loose and packed, refs, the index,
configuration, notes, linked worktrees, merging, rebasing and the stash are all read and
written directly, in Git's own formats. A repository is therefore always one which `git` itself
understands, and an operation which one side stops on a conflict, the other can continue. It
reaches a repository on the same filesystem by path. A remote one needs a transport, which the
`http` and `ssh` components supply. `octogenarian.Native(List(SmartHttp(), SshCommand()))`
speaks Git's wire protocol over whichever `Http.Backend` is in scope, and over a spawned `ssh`.
`Native` is not exported to `soundness`, where the name is xenophile's, so it is written
qualified:

```scala fuel
typecheck
##
import gitTransports.{smartHttp, sshCommand}
import httpBackends.javaNetHttp

given Git.Backend = octogenarian.Native(List(SmartHttp(), SshCommand()))
```

Signing is independent of the backend. A `Git.Signer` makes the signature which a commit or tag
carries, and a `Git.Verifier` checks one:

```tabular
column  Scheme
column  Signer
column  Verifier
column  Drives
row  SSH  `SshKeygen.Signer(key)`  `SshKeygen.Verifier(allowedSigners)`  `ssh-keygen -Y`, as `gpg.format = ssh` has git do
row  OpenPGP  `Gpg.Signer()`  `Gpg.Verifier()`  `gpg`
```

A tag signed through either is one which `git tag -v` accepts.

Whichever backend is chosen, `Exclusive` arbitrates only between handles in this JVM. Another
process is never waited for, which is why every operation which moves a ref is a compare-and-swap
on the `Stale` reading it was given.

### References

Each kind of reference is its own type:

```tabular
column  Type
column  Names
row  `Git.Branch`  a branch
row  `Git.Tag`  a tag
row  `Git.Hash`  a commit, by a well-formed forty-character identifier
row  `Git.Revision`  any of these, or a relative expression: `Git.Revision.head(1)` is `HEAD~1`
row  `Git.Ref`  a fully-qualified reference path, such as `refs/heads/main`
```

So an operation which expects a branch will not accept a tag:

```scala fuel
compile
errors show
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.switch(Git.Tag(t"v1"), worktree.head())
```

A `Git.Ref` is modeled as a [path](paths.md) on the `Git.Refs` plane. Reference names follow
Git's own rules: no leading dot, no `..`, no trailing `lock` suffix, and the rest. The rules are
checked when a name is parsed from text, with `Git.Branch.parse`, `Git.Tag.parse` or
`Git.Refs.heads`, and a bad name is reported as a `Git.RefError`:

```scala fuel
run
errors show
##
Git.Branch.parse(t"feature..old")
```

### Diffs

`diff` reports changes as structured values, not as patch text to parse again. Each `FileDiff`
carries the paths on both sides, the kind of change, which is added, modified, deleted, renamed
or copied, and its hunks. Each hunk is a list of edits:

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  (worktree / "notes.txt").overwrite(t"Remember the milk and the eggs")
  val files = worktree.diff().value
  Out.println(files.head.let(_.changeKind).inspect)
```

Each diff method compares two sides:

```tabular
column  Method
column  Left
column  Right
row  `diff()`  the index  the working files
row  `diffStaged()`  HEAD  the index
row  `diffAgainst(revision)`  the revision  the working files
row  `diffBetween(left, right)`  a commit  a commit
```

The same parser, `Patch.parse`, reads a patch from anywhere.

### Merging, cherry-picking, reverting and rebasing

`merge` takes the revision to merge, the reading of `HEAD`, and a fast-forward policy. Whether a
merge commit is created is stated, and not inherited from configuration. The outcome is a
value:

```tabular
column  Outcome
column  Meaning
row  `FastForwarded(head)`  the branch moved forward to `head`, with no merge commit
row  `Merged(head)`  a merge commit, `head`, was created
row  `UpToDate`  there was nothing to merge
row  `Stopped(operation)`  the merge stopped on a conflict
```

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  Out.println(worktree.merge(Git.Branch(t"feature"), worktree.head(), ff = FastForward.Only).inspect)
```

A conflict is not an error. The operation stops, and the `Stopped` outcome carries a
`Git.Operation` which names the paths in conflict. Once they are resolved and added,
`continue()` finishes the operation, `skip()` passes over the commit, and `abandon()` restores
the state the operation began from. `cherryPick`, `revert` and `rebase` have the same shape, and
`stash` saves and restores uncommitted work.

### Remotes, configuration, notes and the reflog

Remotes are added, listed and removed as values. Configuration is read and written through a
lent handle, typed by the codec of the value: `repo.config { config ?=> config.get[Text](key) }`.

Notes attach text to a commit without altering it, in a chosen namespace. A note write is
conditional on a reading of the notes ref. That is how build attestations and other out-of-band
metadata travel with a repository, without one writer clobbering another:

```scala fuel
run
##
directory.open[Git.Worktree](Read & Write): worktree ?=>
  worktree.addRemote(t"origin", t"git@example.com:globe/hamlet.git")
  val hash = worktree.head().value.commit.or(Git.Hash.zero)
  val notes = worktree.ref(Git.Refs.defaultNotes)
  worktree.addNote(hash, t"a note body", notes)
  Out.println(worktree.note(hash).value.inspect)
```

`note` gives `Unset` where there is no note. `reflog` gives the local history of where a
reference has pointed, newest first. That record is what makes a mistaken reset recoverable.

### Related

- [Filesystem](filesystem.md) describes the directory handles and grants which a worktree
  shares.
- [Paths](paths.md) describes the planes which `Git.Ref` is a path on.
- [Diffing](diffing.md) computes and applies diffs outside a repository.
- [Processes](processes.md) describes how the command-line backend runs `git`.
- [SSH](ssh.md) and the [HTTP client](http-client.md) carry the native backend's transports.
- [Cryptography](cryptography.md) describes the keys which sign commits and tags.
- [Trees](trees.md) draws a commit history in lanes.
