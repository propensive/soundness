                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package octogenarian

import soundness.*

import octogenarian.{content, namespace, target}

import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import workingDirectories.javaBaseWorkingDirectory
import environments.javaBaseEnvironment
import logging.silentLogging
import internetAccess.online

import strategies.throwUnsafely
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics

import filesystemOptions.overwritePreexisting
import filesystemOptions.createNonexistentParents

import gitBackends.soundnessGit

import filesystemBackends.javaBaseFilesystem
import denominative.dysasymptotics.linearSize

// The native backend, checked against repositories the real `git` builds: every read must agree
// with git's own answer, and every write must be one git accepts.
object NativeTests extends Suite(m"Octogenarian native backend"):
  def run(): Unit =

    val scratch: Path on Linux =
      java.nio.file.Path.of(temporaryDirectory[Path on Linux].encode.s).nn.toRealPath().nn.toString.tt
      . as[Path on Linux]

    def freshDir(): Path on Linux =
      val dir = scratch / Uuid().show
      dir.create[Directory]()
      dir

    // A repository made entirely by git, with a stable identity.
    def gitRepo(): Path on Linux =
      val dir = freshDir()
      sh"git -C $dir init -q -b main".exec[Exit]()
      sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
      sh"git -C $dir config user.name Octogenarian".exec[Exit]()
      sh"git -C $dir config commit.gpgsign false".exec[Exit]()
      dir

    def gitCommit(dir: Path on Linux, name: Text, content: Text, message: Text): Git.Hash =
      val file = dir / name
      if !file.existent() then file.create[File]()
      file.open[File](Write) { handle ?=> handle.write(Chain(content.in[Data])) }
      sh"git -C $dir add $name".exec[Exit]()
      sh"git -C $dir commit -q -m $message".exec[Exit]()
      Git.Hash.unsafe(sh"git -C $dir rev-parse HEAD".exec[Text]().trim)

    // git's output without its trailing newline; leading spaces (as in porcelain status) are kept.
    def gitOut(dir: Path on Linux, arguments: Text*): Text =
      val output = Command((t"git" :: t"-C" :: dir.encode :: arguments.to(List))*).exec[Text]()
      if output.ends(t"\n") then output.skip(1, Rtl) else output

    suite(m"Opening"):

      test(m"a repository git made opens as a worktree with the right directories"):
        val dir = gitRepo()
        dir.open[Git.Worktree]() { worktree ?=> (worktree.gitDir == dir / ".git", worktree.stem == dir) }
      .assert(_ == ((true, true)))

      test(m"a directory which is not a repository cannot be opened"):
        val dir = freshDir()
        capture[Git.Error](dir.open[Git.Repo]() { repo ?=> () }).reason == Git.Error.Reason.NoRepository(dir.encode)
      .assert(_ == true)

      test(m"a natively-initialized repository is one git recognises"):
        val dir = freshDir()
        Git.init(dir, initialBranch = Git.Branch(t"main"))
        gitOut(dir, t"rev-parse", t"--is-inside-work-tree")
      .assert(_ == t"true")

      test(m"a natively-initialized bare repository is bare to git"):
        val dir = freshDir()
        Git.init(dir, bare = true)
        gitOut(dir, t"rev-parse", t"--is-bare-repository")
      .assert(_ == t"true")

    suite(m"Reading refs and history"):

      test(m"HEAD of a fresh repository is an unborn main"):
        val dir = gitRepo()
        dir.open[Git.Worktree]() { worktree ?=> worktree.head().value }
      .assert(_ == Git.Head.Attached(Git.Branch(t"main"), Unset))

      test(m"HEAD resolves to the commit git made"):
        val dir = gitRepo()
        val hash = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Worktree]() { worktree ?=> worktree.head().value == Git.Head.Attached(Git.Branch(t"main"), hash) }
      .assert(_ == true)

      test(m"the log matches git log"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"b", t"b\n", t"second")
        gitCommit(dir, t"c", t"c\n", t"third")
        val expected = gitOut(dir, t"log", t"--format=%H").cut(t"\n")
        dir.open[Git.Worktree]() { worktree ?=> worktree.log().value.map(_.commit.text) == expected }
      .assert(_ == true)

      test(m"a parsed commit carries git's author and message"):
        val dir = gitRepo()
        val hash = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Worktree](): worktree ?=>
          val commit = worktree.commitAt(hash)
          (commit.author.identity, commit.message)
      .assert(_ == ((Git.Identity(t"Octogenarian", t"octogenarian@test.local"), t"first")))

      test(m"objects in a packed repository are read through the pack"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"a", t"a\nb\n", t"second")
        gitCommit(dir, t"a", t"a\nb\nc\n", t"third")
        sh"git -C $dir repack -adf -q".exec[Exit]()
        sh"git -C $dir prune-packed -q".exec[Exit]()
        val expected = gitOut(dir, t"log", t"--format=%H").cut(t"\n")
        dir.open[Git.Worktree]() { worktree ?=> worktree.log().value.map(_.commit.text) == expected }
      .assert(_ == true)

      test(m"a delta-compressed blob reads back whole"):
        val dir = gitRepo()
        val big = List.range(0, 200).map { n => t"line $n" }.join(t"\n")
        gitCommit(dir, t"a", big, t"first")
        gitCommit(dir, t"a", t"$big\nmore", t"second")
        sh"git -C $dir repack -adf -q --window=50 --depth=50".exec[Exit]()
        sh"git -C $dir prune-packed -q".exec[Exit]()
        val blob = Git.Hash.unsafe(gitOut(dir, t"rev-parse", t"HEAD:a"))
        dir.open[Git.Worktree]() { worktree ?=> Objects.text(worktree.blobAt(blob)) }
      .assert(_ == t"${List.range(0, 200).map { n => t"line $n" }.join(t"\n")}\nmore")

      test(m"HEAD~1 resolves as git resolves it"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"b", t"b\n", t"second")
        dir.open[Git.Worktree]() { worktree ?=> worktree.resolve(Git.Revision.head(1)).value == first }
      .assert(_ == true)

      test(m"an abbreviated hash resolves"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Worktree]() { worktree ?=> worktree.resolve(Git.Revision.unsafe(first.text.keep(8))).value == first }
      .assert(_ == true)

      test(m"a tag resolves and peels to its commit"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        sh"git -C $dir tag -a v1 -m release".exec[Exit]()
        dir.open[Git.Worktree]() { worktree ?=> worktree.tags().value == Map(Git.Tag(t"v1") -> first) }
      .assert(_ == true)

      test(m"packed refs are listed with loose ones"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        sh"git -C $dir branch packed".exec[Exit]()
        sh"git -C $dir pack-refs --all".exec[Exit]()
        sh"git -C $dir branch loose".exec[Exit]()
        dir.open[Git.Worktree]() { worktree ?=> worktree.branches().value.keys.to[Set] }
      .assert(_ == Set(Git.Branch(t"main"), Git.Branch(t"packed"), Git.Branch(t"loose")))

      test(m"the reflog agrees with git"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"b", t"b\n", t"second")
        val expected = gitOut(dir, t"reflog", t"show", t"--format=%H").cut(t"\n")
        dir.open[Git.Worktree]() { worktree ?=> worktree.reflog().value.map(_.hash.text) == expected }
      .assert(_ == true)

      test(m"merge-base agrees with git"):
        val dir = gitRepo()
        val base = gitCommit(dir, t"a", t"a\n", t"base")
        sh"git -C $dir checkout -q -b feature".exec[Exit]()
        val feature = gitCommit(dir, t"b", t"b\n", t"feature")
        sh"git -C $dir checkout -q main".exec[Exit]()
        val main = gitCommit(dir, t"c", t"c\n", t"main")
        dir.open[Git.Worktree]() { worktree ?=> (worktree.mergeBase(feature, main) == base, worktree.isAncestor(base, main), worktree.isAncestor(feature, main)) }
      .assert(_ == ((true, true, false)))

    suite(m"Writing refs"):

      test(m"a natively created branch is seen by git"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.makeBranch(Git.Branch(t"feature"), first) }
        gitOut(dir, t"rev-parse", t"refs/heads/feature") == first.text
      .assert(_ == true)

      test(m"a compare-and-swap against a moved ref fails with Moved"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Repo](Read & Write): repo ?=>
          val ref = Git.Refs.heads(t"other")
          repo.updateRef(ref, first, repo.ref(ref))
          val seen = repo.ref(ref)
          val second = gitCommit(dir, t"b", t"b\n", t"second")
          sh"git -C $dir update-ref refs/heads/other ${second.text}".exec[Exit]()
          capture[Git.Error](repo.updateRef(ref, Unset, seen)).reason
          == Git.Error.Reason.Moved(t"refs/heads/other", first, second)
      .assert(_ == true)

      test(m"a ref update writes a reflog line git reads"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.makeBranch(Git.Branch(t"feature"), first) }
        gitOut(dir, t"reflog", t"show", t"--format=%gs", t"refs/heads/feature") == t"branch: Created from ${first.text}"
      .assert(_ == true)

      test(m"deleting a packed ref removes it from packed-refs"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        sh"git -C $dir branch packed".exec[Exit]()
        sh"git -C $dir pack-refs --all".exec[Exit]()
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.deleteBranch(Git.Branch(t"packed"), Stale(first)) }
        gitOut(dir, t"branch", t"--list", t"packed")
      .assert(_ == t"")

      test(m"a natively written lightweight tag is seen by git"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.makeTag(Git.Tag(t"v1"), first) }
        gitOut(dir, t"rev-parse", t"v1") == first.text
      .assert(_ == true)

      test(m"a natively written annotated tag passes git fsck and shows its message"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.annotatedTag(Git.Tag(t"v1"), first, t"Release one") }
        (gitOut(dir, t"fsck", t"--strict"), gitOut(dir, t"tag", t"-l", t"-n1", t"v1").contains(t"Release one"))
      .assert(_ == ((t"", true)))

      test(m"a natively written object has the hash git computes"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        val hash = dir.open[Git.Repo](Read & Write) { repo ?=> repo.writeObject(Git.Object.Kind.Blob, t"hello\n".in[Data]) }
        gitOut(dir, t"cat-file", t"-p", hash.text)
      .assert(_ == t"hello")

      test(m"renaming the checked-out branch moves HEAD with it"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.renameBranch(Git.Branch(t"main"), Git.Branch(t"trunk"), Stale(first)) }
        gitOut(dir, t"symbolic-ref", t"HEAD")
      .assert(_ == t"refs/heads/trunk")

    suite(m"Notes"):

      test(m"a note git wrote is read natively"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        sh"git -C $dir notes add -m annotation ${first.text}".exec[Exit]()
        dir.open[Git.Repo]() { repo ?=> repo.note(first).value }
      .assert(_ == t"annotation")

      test(m"a note written natively is read by git"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.addNote(first, t"native note", repo.ref(Git.Refs.defaultNotes)) }
        (gitOut(dir, t"notes", t"show", first.text), gitOut(dir, t"fsck", t"--strict"))
      .assert(_ == ((t"native note", t"")))

      test(m"notes in a fanned-out tree are listed"):
        val dir = gitRepo()
        val hashes = List.range(0, 3).map { n => gitCommit(dir, t"f$n", t"$n\n", t"commit $n") }
        hashes.each { hash => sh"git -C $dir notes add -m note ${hash.text}".exec[Exit]() }
        dir.open[Git.Repo](): repo ?=>
          val keys = repo.notes().value.keys
          hashes.forall(keys.has(_))
      .assert(_ == true)

    suite(m"Configuration"):

      test(m"git's configuration is read natively"):
        val dir = gitRepo()
        dir.open[Git.Repo]() { repo ?=> repo.config { config ?=> config.get[Text](Git.Config.Key.parse(t"user.name")).value } }
      .assert(_ == t"Octogenarian")

      test(m"a natively written setting is read by git"):
        val dir = gitRepo()
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.configure { config ?=> config.set(Git.Config.Key.parse(t"octogenarian.answer"), 42) } }
        gitOut(dir, t"config", t"octogenarian.answer")
      .assert(_ == t"42")

      test(m"a quoted value with a comment character round-trips"):
        val dir = gitRepo()
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.configure { config ?=> config.set(Git.Config.Key.parse(t"octogenarian.note"), t"a # b") } }
        (gitOut(dir, t"config", t"octogenarian.note"), dir.open[Git.Repo]() { repo ?=> repo.config { config ?=> config.get[Text](Git.Config.Key.parse(t"octogenarian.note")).value } })
      .assert(_ == ((t"a # b", t"a # b")))

      test(m"multiple values accumulate and unset-all clears them"):
        val dir = gitRepo()
        val key = Git.Config.Key.parse(t"octogenarian.many")
        dir.open[Git.Repo](Read & Write): repo ?=>
          repo.configure: config ?=>
            config.add(key, t"one")
            config.add(key, t"two")
        val listed = gitOut(dir, t"config", t"--get-all", t"octogenarian.many").cut(t"\n")
        dir.open[Git.Repo](Read & Write) { repo ?=> repo.configure { config ?=> config.unset(key, all = true) } }
        (listed, gitOut(dir, t"config", t"--get-all", t"octogenarian.many"))
      .assert(_ == ((List(t"one", t"two"), t"")))

      test(m"remotes are read from the configuration"):
        val dir = gitRepo()
        sh"git -C $dir remote add origin git@example.com:foo/bar.git".exec[Exit]()
        dir.open[Git.Repo]() { repo ?=> repo.remotes().value }
      .assert(_ == List(Remote(t"origin", t"git@example.com:foo/bar.git")))

    suite(m"Working tree"):

      def write(dir: Path on Linux, name: Text, content: Text): Unit =
        val file = dir / name
        if !file.existent() then file.create[File]()
        file.open[File](Write) { handle ?=> handle.write(Chain(content.in[Data])) }

      test(m"status agrees with git for added, modified, deleted and untracked files"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"b", t"b\n", t"second")
        write(dir, t"a", t"changed\n")
        sh"rm $dir/b".exec[Exit]()
        write(dir, t"c", t"c\n")
        sh"git -C $dir add c".exec[Exit]()
        write(dir, t"d", t"d\n")
        val expected = gitOut(dir, t"status", t"--porcelain=v1", t"--untracked-files=all").cut(t"\n")

        dir.open[Git.Worktree](): worktree ?=>
          def code(status: Optional[Git.Status]): Text = status match
            case Git.Status.Updated   => t"M"
            case Git.Status.Added     => t"A"
            case Git.Status.Deleted   => t"D"
            case Git.Status.Untracked => t"?"
            case _                    => t" "

          def render(line: Git.PathStatus[worktree.Plane]): Text =
            t"${code(line.index)}${code(line.worktree)} ${Operations.relative(line.path)}"

          worktree.status().value.map(render(_)) == expected
      .assert(_ == true)

      test(m"a native add and commit is a commit git fsck accepts and git log shows"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"b", t"b\n")
        dir.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.add(Operations.planePath(worktree, t"b"))
          worktree.commit(t"second", worktree.head())
        (gitOut(dir, t"fsck", t"--strict"), gitOut(dir, t"log", t"--format=%s").cut(t"\n"), gitOut(dir, t"status", t"--porcelain"))
      .assert(_ == ((t"", List(t"second", t"first"), t"")))

      test(m"a native index is one git reads back identically"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        (dir / "sub").create[Directory]()
        write(dir / "sub", t"b", t"b\n")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.add(Operations.planePath(worktree, t"sub/b")) }
        gitOut(dir, t"ls-files", t"--stage").cut(t"\n").map(_.cut(t" ").prim.or(t""))
      .assert(_ == List(t"100644", t"100644"))

      test(m"rm removes the file and its index entry"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.rm(Operations.planePath(worktree, t"a"))() }
        (gitOut(dir, t"status", t"--porcelain"), (dir / "a").existent())
      .assert(_ == ((t"D  a", false)))

      test(m"mv renames the file and git sees a rename"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.mv(Operations.planePath(worktree, t"a"), Operations.planePath(worktree, t"b")) }
        gitOut(dir, t"status", t"--porcelain")
      .assert(_ == t"R  a -> b")

      test(m"restore discards a working change"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"a", t"changed\n")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.restore(Operations.planePath(worktree, t"a"))() }
        gitOut(dir, t"status", t"--porcelain")
      .assert(_ == t"")

      test(m"restore with staged unstages and keeps the file"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"a", t"changed\n")
        sh"git -C $dir add a".exec[Exit]()
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.restore(Operations.planePath(worktree, t"a"))(staged = true) }
        gitOut(dir, t"status", t"--porcelain")
      .assert(_ == t" M a")

      test(m"clean removes untracked files and reports them"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"junk", t"x\n")
        val removed = dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.clean().map(Operations.relative(_)) }
        (removed, (dir / "junk").existent())
      .assert(_ == ((List(t"junk"), false)))

      test(m"reset --hard restores the tree as git would"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"b", t"b\n", t"second")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.reset(ResetMode.Hard, Git.Revision.head(1), worktree.head()) }
        (gitOut(dir, t"status", t"--porcelain"), (dir / "b").existent(), gitOut(dir, t"log", t"--format=%s"))
      .assert(_ == ((t"", false, t"first")))

      test(m"reset --mixed keeps the file but unstages it"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"b", t"b\n", t"second")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.reset(ResetMode.Mixed, Git.Revision.head(1), worktree.head()) }
        gitOut(dir, t"status", t"--porcelain")
      .assert(_ == t"?? b")

      test(m"switch creates a branch and checkout of main returns"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        dir.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
          write(dir, t"b", t"b\n")
          worktree.add(Operations.planePath(worktree, t"b"))
          worktree.commit(t"feature", worktree.head())
          worktree.switch(Git.Branch(t"main"), worktree.head())
        (gitOut(dir, t"symbolic-ref", t"HEAD"), (dir / "b").existent(), gitOut(dir, t"status", t"--porcelain"))
      .assert(_ == ((t"refs/heads/main", false, t"")))

      test(m"checkout of a commit detaches HEAD and refuses to clobber changes"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        gitCommit(dir, t"a", t"a2\n", t"second")
        write(dir, t"a", t"dirty\n")
        val refused = dir.open[Git.Worktree](Read & Write): worktree ?=>
          capture[Git.Error](worktree.checkout(first, worktree.head())).reason
        sh"git -C $dir checkout -q -- a".exec[Exit]()
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.checkout(first, worktree.head()) }
        (refused, gitOut(dir, t"rev-parse", t"HEAD") == first.text, gitOut(dir, t"symbolic-ref", t"-q", t"HEAD"))
      .assert(_ == ((Git.Error.Reason.Dirty(List(t"a")), true, t"")))

      test(m"diff reports hunks git would print"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"one\ntwo\nthree\nfour\nfive\n", t"first")
        write(dir, t"a", t"one\ntwo\nTHREE\nfour\nfive\n")
        dir.open[Git.Worktree](): worktree ?=>
          val file = worktree.diff().value.prim.or(abort(Git.Error(Git.Error.Reason.Unexpected)))
          val hunk = file.hunks.prim.or(abort(Git.Error(Git.Error.Reason.Unexpected)))
          val changes = hunk.edits.filter(!_.isInstanceOf[Par[?]]).fold(0) { (n, _) => n + 1 }
          (file.changeKind, hunk.oldStart, hunk.oldLines, hunk.newStart, hunk.newLines, changes)
      .assert(_ == ((ChangeKind.Modified, 1, 5, 1, 5, 2)))

      test(m"diffBetween two commits matches the command-line parse of git diff"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"one\ntwo\nthree\n", t"first")
        val second = gitCommit(dir, t"a", t"one\n2\nthree\nfour\n", t"second")
        val expected = Patch.parse(gitOut(dir, t"diff", t"--no-color", first.text, second.text).cut(t"\n"))
        dir.open[Git.Worktree]() { worktree ?=> worktree.diffBetween(first, second) == expected }
      .assert(_ == true)

    suite(m"Merging and replaying"):

      def write(dir: Path on Linux, name: Text, content: Text): Unit =
        val file = dir / name
        if !file.existent() then file.create[File]()
        file.open[File](Write) { handle ?=> handle.write(Chain(content.in[Data])) }

      def read(dir: Path on Linux, name: Text): Text = Command(t"cat", (dir / name).encode).exec[Text]()

      // `main` and `feature` each with a commit beyond a common base; HEAD on `main`.
      def diverged(dir: Path on Linux, base: Text, ours: Text, theirs: Text): Git.Hash =
        gitCommit(dir, t"a", base, t"base")
        sh"git -C $dir switch -q -c feature".exec[Exit]()
        val feature = gitCommit(dir, t"a", theirs, t"feature")
        sh"git -C $dir switch -q main".exec[Exit]()
        gitCommit(dir, t"a", ours, t"main work")
        feature

      def fsck(dir: Path on Linux): Text = gitOut(dir, t"fsck", t"--strict", t"--no-dangling")

      test(m"merge fast-forwards to the branch tip and leaves a clean tree"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"base")
        sh"git -C $dir switch -q -c feature".exec[Exit]()
        val feature = gitCommit(dir, t"b", t"b\n", t"feature")
        sh"git -C $dir switch -q main".exec[Exit]()
        val outcome = dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.merge(Git.Branch(t"feature"), worktree.head()) }
        (outcome == Git.Merge.Outcome.FastForwarded(feature), gitOut(dir, t"rev-parse", t"HEAD") == feature.text, gitOut(dir, t"status", t"--porcelain"))
      .assert(_ == ((true, true, t"")))

      test(m"a non-overlapping merge makes a commit git accepts with the content git would make"):
        val dir = gitRepo()
        diverged(dir, t"one\ntwo\nthree\nfour\nfive\n", t"ONE\ntwo\nthree\nfour\nfive\n", t"one\ntwo\nthree\nfour\nFIVE\n")
        val outcome = dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.merge(Git.Branch(t"feature"), worktree.head()) }
        val parents = gitOut(dir, t"log", t"-1", t"--format=%P").cut(t" ").size

        ( outcome.isInstanceOf[Git.Merge.Outcome.Merged], parents, gitOut(dir, t"show", t"HEAD:a"),
          gitOut(dir, t"log", t"-1", t"--format=%s"), fsck(dir), gitOut(dir, t"status", t"--porcelain") )
      .assert(_ == ((true, 2, t"ONE\ntwo\nthree\nfour\nFIVE", t"Merge branch 'feature'", t"", t"")))

      test(m"a conflicting merge leaves the file and the index stages git would leave"):
        val dir = gitRepo()
        diverged(dir, t"base\n", t"ours\n", t"theirs\n")
        val twin = freshDir()
        sh"rm -rf $twin".exec[Exit]()
        sh"cp -R $dir $twin".exec[Exit]()
        sh"git -C $twin merge feature".exec[Exit]()
        val outcome = dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.merge(Git.Branch(t"feature"), worktree.head()) }

        ( outcome.isInstanceOf[Git.Merge.Outcome.Stopped], read(dir, t"a") == read(twin, t"a"),
          gitOut(dir, t"ls-files", t"--stage") == gitOut(twin, t"ls-files", t"--stage"),
          gitOut(dir, t"status", t"--porcelain"), gitOut(dir, t"rev-parse", t"MERGE_HEAD") == gitOut(twin, t"rev-parse", t"MERGE_HEAD") )
      .assert(_ == ((true, true, true, t"UU a", true)))

      test(m"git itself can commit a merge the native backend stopped"):
        val dir = gitRepo()
        diverged(dir, t"base\n", t"ours\n", t"theirs\n")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.merge(Git.Branch(t"feature"), worktree.head()) }
        write(dir, t"a", t"resolved\n")
        sh"git -C $dir add a".exec[Exit]()
        sh"git -C $dir -c core.editor=true commit -q --no-edit".exec[Exit]()
        (gitOut(dir, t"log", t"-1", t"--format=%P").cut(t" ").size, gitOut(dir, t"log", t"-1", t"--format=%s"))
      .assert(_ == ((2, t"Merge branch 'feature'")))

      test(m"a merge stopped by git is continued natively"):
        val dir = gitRepo()
        diverged(dir, t"base\n", t"ours\n", t"theirs\n")
        sh"git -C $dir merge feature".exec[Exit]()
        write(dir, t"a", t"resolved\n")
        val outcome = dir.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.add(Operations.planePath(worktree, t"a"))
          worktree.continue()

        ( outcome.isInstanceOf[Git.Sequence.Outcome.Completed], gitOut(dir, t"log", t"-1", t"--format=%P").cut(t" ").size,
          (dir / ".git" / "MERGE_HEAD").existent(), fsck(dir) )
      .assert(_ == ((true, 2, false, t"")))

      test(m"abandoning a stopped merge restores the tree git sees as clean"):
        val dir = gitRepo()
        diverged(dir, t"base\n", t"ours\n", t"theirs\n")
        val main = gitOut(dir, t"rev-parse", t"HEAD")
        dir.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.merge(Git.Branch(t"feature"), worktree.head())
          worktree.abandon()
        (gitOut(dir, t"status", t"--porcelain"), gitOut(dir, t"rev-parse", t"HEAD") == main, read(dir, t"a"))
      .assert(_ == ((t"", true, t"ours\n")))

      test(m"cherryPick replays a commit keeping its author date"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"base")
        sh"git -C $dir switch -q -c feature".exec[Exit]()
        val feature = gitCommit(dir, t"b", t"b\n", t"feature")
        sh"git -C $dir switch -q main".exec[Exit]()
        gitCommit(dir, t"c", t"c\n", t"main work")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.cherryPick(List(feature), worktree.head()) }

        ( gitOut(dir, t"log", t"--format=%s"), gitOut(dir, t"log", t"-1", t"--format=%at") == gitOut(dir, t"log", t"-1", t"--format=%at", feature.text),
          read(dir, t"b"), fsck(dir) )
      .assert(_ == ((t"feature\nmain work\nbase", true, t"b\n", t"")))

      test(m"revert makes the inverse commit with git's message"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"base")
        val toRevert = gitCommit(dir, t"b", t"b\n", t"add b")
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.revert(List(toRevert), worktree.head()) }
        (gitOut(dir, t"log", t"-1", t"--format=%s"), (dir / "b").existent(), gitOut(dir, t"status", t"--porcelain"))
      .assert(_ == ((t"Revert \"add b\"", false, t"")))

      test(m"rebase replays the branch onto main and reattaches HEAD"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"base")
        sh"git -C $dir switch -q -c feature".exec[Exit]()
        gitCommit(dir, t"b", t"b\n", t"feature")
        sh"git -C $dir switch -q main".exec[Exit]()
        gitCommit(dir, t"c", t"c\n", t"main work")
        sh"git -C $dir switch -q feature".exec[Exit]()
        val outcome = dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.rebase(Git.Branch(t"main"), worktree.head()) }

        ( outcome.isInstanceOf[Git.Sequence.Outcome.Completed], gitOut(dir, t"log", t"--format=%s"),
          gitOut(dir, t"symbolic-ref", t"HEAD"), (dir / ".git" / "rebase-merge").existent(), fsck(dir) )
      .assert(_ == ((true, t"feature\nmain work\nbase", t"refs/heads/feature", false, t"")))

      test(m"git can continue a rebase the native backend stopped"):
        val dir = gitRepo()
        diverged(dir, t"base\n", t"ours\n", t"theirs\n")
        sh"git -C $dir switch -q feature".exec[Exit]()
        val outcome = dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.rebase(Git.Branch(t"main"), worktree.head()) }
        write(dir, t"a", t"resolved\n")
        sh"git -C $dir add a".exec[Exit]()
        val exit = sh"git -C $dir -c core.editor=true rebase --continue".exec[Exit]()

        val result = ( outcome.isInstanceOf[Git.Sequence.Outcome.Stopped], exit == Exit.Ok, gitOut(dir, t"log", t"--format=%s"),
          gitOut(dir, t"symbolic-ref", t"HEAD") )
        result
      .assert(_ == ((true, true, t"feature\nmain work\nbase", t"refs/heads/feature")))

      test(m"a conflicting rebase is skipped natively"):
        val dir = gitRepo()
        diverged(dir, t"base\n", t"ours\n", t"theirs\n")
        sh"git -C $dir switch -q feature".exec[Exit]()
        val outcome = dir.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.rebase(Git.Branch(t"main"), worktree.head())
          worktree.skip()
        (outcome.isInstanceOf[Git.Sequence.Outcome.Completed], gitOut(dir, t"log", t"--format=%s"), gitOut(dir, t"symbolic-ref", t"HEAD"))
      .assert(_ == ((true, t"main work\nbase", t"refs/heads/feature")))

      test(m"a rebase git stopped is abandoned natively"):
        val dir = gitRepo()
        val feature = diverged(dir, t"base\n", t"ours\n", t"theirs\n")
        sh"git -C $dir switch -q feature".exec[Exit]()
        sh"git -C $dir rebase main".exec[Exit]()
        dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.abandon() }
        (gitOut(dir, t"rev-parse", t"HEAD") == feature.text, gitOut(dir, t"symbolic-ref", t"HEAD"), gitOut(dir, t"status", t"--porcelain"))
      .assert(_ == ((true, t"refs/heads/feature", t"")))

    suite(m"Stash"):

      def write(dir: Path on Linux, name: Text, content: Text): Unit =
        val file = dir / name
        if !file.existent() then file.create[File]()
        file.open[File](Write) { handle ?=> handle.write(Chain(content.in[Data])) }

      def read(dir: Path on Linux, name: Text): Text = Command(t"cat", (dir / name).encode).exec[Text]()

      test(m"a native stash is one git lists and pops"):
        val dir = gitRepo()
        val first = gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"a", t"changed\n")
        val stashed = dir.open[Git.Worktree](Read & Write) { worktree ?=> worktree.stash() }
        val listed = gitOut(dir, t"stash", t"list")
        val clean = gitOut(dir, t"status", t"--porcelain")
        sh"git -C $dir stash pop -q".exec[Exit]()
        val result = (stashed.present, listed == t"stash@{0}: WIP on main: ${first.text.keep(7)} first", clean, read(dir, t"a"))
        result
      .assert(_ == ((true, true, t"", t"changed\n")))

      test(m"a stash git made is popped natively"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"a", t"changed\n")
        sh"git -C $dir stash -q".exec[Exit]()
        dir.open[Git.Worktree](Read & Write): worktree ?=>
          val entry = worktree.stashes().let(_.prim.or(abort(Git.Error(Git.Error.Reason.NoSuchStash))))
          worktree.popStash(entry)
        (gitOut(dir, t"status", t"--porcelain"), gitOut(dir, t"stash", t"list"), read(dir, t"a"))
      .assert(_ == ((t" M a", t"", t"changed\n")))

      test(m"a stash with untracked files restores them on popping"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"d", t"d\n")
        val gone = dir.open[Git.Worktree](Read & Write): worktree ?=>
          worktree.stash(includeUntracked = true)
          !(dir / "d").existent()
        val listed = gitOut(dir, t"stash", t"list")
        dir.open[Git.Worktree](Read & Write): worktree ?=>
          val entry = worktree.stashes().let(_.prim.or(abort(Git.Error(Git.Error.Reason.NoSuchStash))))
          worktree.popStash(entry)
        (gone, listed.starts(t"stash@{0}: WIP on main"), gitOut(dir, t"status", t"--porcelain"))
      .assert(_ == ((true, true, t"?? d")))

      test(m"dropping a stash natively leaves no stash ref for git"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"a", t"changed\n")
        sh"git -C $dir stash -q".exec[Exit]()
        dir.open[Git.Worktree](Read & Write): worktree ?=>
          val entry = worktree.stashes().let(_.prim.or(abort(Git.Error(Git.Error.Reason.NoSuchStash))))
          worktree.dropStash(entry)
        (gitOut(dir, t"stash", t"list"), sh"git -C $dir rev-parse -q --verify refs/stash".exec[Exit]() == Exit.Ok)
      .assert(_ == ((t"", false)))

      test(m"dropping the older of two stashes keeps the newer at the top"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        write(dir, t"a", t"one\n")
        sh"git -C $dir stash -q -m older".exec[Exit]()
        write(dir, t"a", t"two\n")
        sh"git -C $dir stash -q -m newer".exec[Exit]()
        dir.open[Git.Worktree](Read & Write): worktree ?=>
          val entry = worktree.stashes().let(_.reverse.prim.or(abort(Git.Error(Git.Error.Reason.NoSuchStash))))
          worktree.dropStash(entry)
        gitOut(dir, t"stash", t"list")
      .assert(_ == t"stash@{0}: On main: newer")

    suite(m"Linked worktrees"):

      test(m"git's linked worktrees are listed natively"):
        val dir = gitRepo()
        gitCommit(dir, t"a", t"a\n", t"first")
        val other = freshDir()
        sh"rm -rf $other".exec[Exit]()
        sh"git -C $dir worktree add -q --detach $other".exec[Exit]()
        dir.open[Git.Repo]() { repo ?=> repo.worktrees().value.map(_.path).to[Set] == Set(dir, other) }
      .assert(_ == true)
