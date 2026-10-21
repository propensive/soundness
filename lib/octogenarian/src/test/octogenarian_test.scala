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

import scala.caps

import soundness.*

// `content`, `namespace` and `target` are package-level extensions on `NoteRef`; brought in by
// name so that they win on specificity against turbulence's generic `read` from `soundness.*`.
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

import gitCommands.searchpathGitCommand

import denominative.dysasymptotics.linearSize

object Tests extends Suite("git", m"Octogenarian Tests"):
  def run(): Unit =

    // ----- Shared helpers -------------------------------------------------

    // The real path: git reports real paths, and the system temporary directory is a symlink on
    // macOS.
    val scratch: Path on Linux =
      java.nio.file.Path.of(temporaryDirectory[Path on Linux].encode.s).nn.toRealPath().nn.toString.tt
      . as[Path on Linux]

    def freshDir(): Path on Linux =
      val name: Text = Uuid().show
      val dir = scratch / name
      dir.create[Directory]()
      dir

    // Initializes a repository with isolated configuration: a stable identity, `main` as the
    // initial branch, and signing disabled, so the tests do not depend on the developer's or the
    // CI runner's global git settings.
    // Which backend a repository suite runs under. A pure chooser, not the backend itself: a
    // backend is a capability, and a test lambda which captured one could not be a `Test`; the
    // given below makes one afresh wherever a test asks for it, as the imported givens do.
    trait Choice:
      def backend(): Git.Backend^{caps.any}

    object commandline extends Choice:
      def backend(): Git.Backend^{caps.any} = gitBackends.commandlineGit

    object native extends Choice:
      def backend(): Git.Backend^{caps.any} = gitBackends.soundnessGit

    given chosen: (choice: Choice) => (Git.Backend^{caps.any}) = choice.backend()

    def freshRepo()(using Choice): Path on Linux =
      val dir = freshDir()
      Git.init(dir, initialBranch = Git.Branch(t"main"))
      sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
      sh"git -C $dir config user.name Octogenarian".exec[Exit]()
      sh"git -C $dir config commit.gpgsign false".exec[Exit]()
      sh"git -C $dir config tag.gpgsign false".exec[Exit]()
      dir

    def withWorktree[result]
      ( block: ((Git.Worktree.Handle & Granting[Grant.Read & Grant.Write])^) ?=> result )
      ( using Choice )
    :   result =

      freshRepo().open[Git.Worktree](Read & Write)(block)

    def write(name: Text, content: Text)
      ( using worktree: (Git.Worktree.Handle & Granting[Grant.Read & Grant.Write])^ )
    :   Unit =

      val path = worktree.stem / name
      if !path.existent() then path.create[File]()
      path.open[File](Write): handle ?=>
        handle.write(Chain(content.in[Data]))

    def commitFile(name: Text, content: Text, message: Text)
      ( using worktree: (Git.Worktree.Handle & Granting[Grant.Read & Grant.Write])^ )
    :   Git.Hash =

      write(name, content)
      worktree.add(Operations.planePath(worktree, name))
      worktree.commit(message, worktree.head())

    def planePath(name: Text)(using worktree: (Git.Worktree.Handle & Granting[Grant.Read & Grant.Write])^)
    :   Path on worktree.Plane =

      Operations.planePath(worktree, name)

    def patchFrom(lines: Text*): List[FileDiff] =
      Patch.parse(lines.to(List))

    def messages(using worktree: (Git.Worktree.Handle & Granting[Grant.Read & Grant.Write])^): List[Text] =
      worktree.log().value.map(_.message)

    // ----- Stale ------------------------------------------------------------

    suite(m"Stale values"):

      test(m"a derived stale value is as stale as its source"):
        Stale(1).let(_ + 1)
      .assert(_ == Stale(2))

      test(m"two stale readings of the same value are equal"):
        Stale(t"a") == Stale(t"a")
      .assert(_ == true)

      test(m"a stale value inspects with its content"):
        Stale(Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd")).inspect
      .assert(_ == t"Stale(value:Hash(5c1d2b7e3a4f60918273645566778899aabbccdd))")

    // ----- Native rendering --------------------------------------------------

    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings.
    suite(m"Native-rendering coverage"):

      test(m"octogenarian's ref types inspect natively"):
        Inspectable.fallbacks
         ( Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd").inspect,
           Git.Branch.unsafe(t"main").inspect,
           Git.Tag.unsafe(t"v1.0").inspect,
           Git.Revision.head(2).inspect )
      .assert(_ == Nil)

      test(m"a hash inspects with all forty digits"):
        Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd").inspect
      .assert(_ == t"Hash(5c1d2b7e3a4f60918273645566778899aabbccdd)")

      test(m"a branch, a tag and a revision each name their kind"):
        List
         ( Git.Branch.unsafe(t"main").inspect,
           Git.Tag.unsafe(t"v1.0").inspect,
           Git.Revision.head(2).inspect )
      .assert(_ == List(t"Branch(main)", t"Tag(v1.0)", t"Revision(HEAD~2)"))

    // ----- Reference validation (unit tests, no git) ------------------------

    suite(m"Reference validation"):

      test(m"a valid branch name parses unchanged"):
        Git.Revision.parse(t"feature/foo")
      .assert(_ == t"feature/foo")

      test(m"a name with internal hyphens parses"):
        Git.Revision.parse(t"long-running-feature")
      .assert(_ == t"long-running-feature")

      test(m"a multi-segment ref parses"):
        Git.Revision.parse(t"refs/heads/main")
      .assert(_ == t"refs/heads/main")

      test(m"reject a ref with a leading dot"):
        safely(Git.Revision.parse(t".secret"))
      .assert(_.absent)

      test(m"reject a ref with a trailing dot"):
        safely(Git.Revision.parse(t"trailing."))
      .assert(_.absent)

      test(m"reject a ref ending in .lock"):
        safely(Git.Revision.parse(t"feature.lock"))
      .assert(_.absent)

      test(m"reject a ref containing @{"):
        safely(Git.Revision.parse(t"name@{0}"))
      .assert(_.absent)

      test(m"reject a ref containing .."):
        safely(Git.Revision.parse(t"foo..bar"))
      .assert(_.absent)

      test(m"reject a ref with an empty segment"):
        safely(Git.Revision.parse(t"foo//bar"))
      .assert(_.absent)

      test(m"reject a ref containing a space"):
        safely(Git.Revision.parse(t"two words"))
      .assert(_.absent)

      test(m"reject a ref containing colon"):
        safely(Git.Revision.parse(t"a:b"))
      .assert(_.absent)

      test(m"reject a ref containing tilde"):
        safely(Git.Revision.parse(t"a~1"))
      .assert(_.absent)

      test(m"reject a ref containing caret"):
        safely(Git.Revision.parse(t"a^1"))
      .assert(_.absent)

      test(m"reject a ref containing question mark"):
        safely(Git.Revision.parse(t"a?b"))
      .assert(_.absent)

      test(m"reject a ref containing star"):
        safely(Git.Revision.parse(t"a*b"))
      .assert(_.absent)

      test(m"reject a ref containing left bracket"):
        safely(Git.Revision.parse(t"a[b"))
      .assert(_.absent)

      test(m"Git.Hash accepts a valid 40-char lowercase hex string"):
        Git.Hash(t"0123456789abcdef0123456789abcdef01234567").show
      .assert(_ == t"0123456789abcdef0123456789abcdef01234567")

      test(m"Git.Hash rejects a string that is too short"):
        safely(Git.Hash(t"0123456789abcdef"))
      .assert(_.absent)

      test(m"Git.Hash rejects uppercase hex"):
        safely(Git.Hash(t"0123456789ABCDEF0123456789ABCDEF01234567"))
      .assert(_.absent)

      test(m"Git.Hash rejects non-hex characters"):
        safely(Git.Hash(t"0123456789abcdef0123456789abcdef0123456g"))
      .assert(_.absent)

      test(m"a fetch refspec parses into source, destination and force"):
        Git.Refspec.parse(t"+refs/heads/*:refs/remotes/origin/*")
      .assert(_ == Git.Refspec(t"refs/heads/*", t"refs/remotes/origin/*", true))

      test(m"a deletion refspec has no source"):
        Git.Refspec.parse(t":refs/heads/gone")
      .assert(_ == Git.Refspec(Unset, t"refs/heads/gone", false))

      test(m"a refspec renders back to its text"):
        Git.Refspec.parse(t"+main:main").text
      .assert(_ == t"+main:main")

    // ----- Timestamps and identities ----------------------------------------

    suite(m"Timestamps and signatures"):

      test(m"a git timestamp renders its offset with four digits"):
        Git.Timestamp(1700000000L, 90).text
      .assert(_ == t"1700000000 +0130")

      test(m"a negative offset renders with a minus sign"):
        Git.Timestamp(1700000000L, -300).text
      .assert(_ == t"1700000000 -0500")

      test(m"a timestamp round-trips through parsing"):
        Git.Timestamp.parse(t"1700000000", t"-0500")
      .assert(_ == Git.Timestamp(1700000000L, -300))

      test(m"an author line parses into identity and timestamp"):
        Git.Signature.parse(t"Jon Pretty <jon@example.com> 1700000000 +0000")
      .assert(_ == Git.Signature(Git.Identity(t"Jon Pretty", t"jon@example.com"), Git.Timestamp(1700000000L, 0)))

    // ----- Patch parser ----------------------------------------------------

    suite(m"Patch parser"):

      test(m"empty input yields no FileDiffs"):
        patchFrom().size
      .assert(_ == 0)

      test(m"a single Modified file yields one FileDiff"):
        patchFrom
         ( t"diff --git a/foo.txt b/foo.txt",
           t"index 1234567..89abcde 100644",
           t"--- a/foo.txt",
           t"+++ b/foo.txt",
           t"@@ -1,3 +1,3 @@",
           t" one",
           t"-two",
           t"+two-changed",
           t" three" ).size
      .assert(_ == 1)

      test(m"a Modified file has ChangeKind.Modified"):
        patchFrom
         ( t"diff --git a/foo.txt b/foo.txt",
           t"--- a/foo.txt",
           t"+++ b/foo.txt",
           t"@@ -1 +1 @@",
           t"-old",
           t"+new" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Modified)

      test(m"a new file has ChangeKind.Added"):
        patchFrom
         ( t"diff --git a/new.txt b/new.txt",
           t"new file mode 100644",
           t"index 0000000..89abcde",
           t"--- /dev/null",
           t"+++ b/new.txt",
           t"@@ -0,0 +1 @@",
           t"+content" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Added)

      test(m"a deleted file has ChangeKind.Deleted"):
        patchFrom
         ( t"diff --git a/old.txt b/old.txt",
           t"deleted file mode 100644",
           t"index 1234567..0000000",
           t"--- a/old.txt",
           t"+++ /dev/null",
           t"@@ -1 +0,0 @@",
           t"-content" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Deleted)

      test(m"a renamed file has ChangeKind.Renamed"):
        patchFrom
         ( t"diff --git a/old.txt b/new.txt",
           t"similarity index 100%",
           t"rename from old.txt",
           t"rename to new.txt" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Renamed)

      test(m"a renamed file records both old and new paths"):
        val file = patchFrom
         ( t"diff --git a/old.txt b/new.txt",
           t"similarity index 100%",
           t"rename from old.txt",
           t"rename to new.txt" ).stdlib.head
        (file.oldPath, file.newPath)
      .assert(_ == (t"old.txt", t"new.txt"))

      test(m"a copied file has ChangeKind.Copied"):
        patchFrom
         ( t"diff --git a/source b/dest",
           t"similarity index 80%",
           t"copy from source",
           t"copy to dest" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Copied)

      test(m"multiple files yield multiple FileDiffs"):
        patchFrom
         ( t"diff --git a/a b/a",
           t"--- a/a",
           t"+++ b/a",
           t"@@ -1 +1 @@",
           t"-1",
           t"+1a",
           t"diff --git a/b b/b",
           t"--- a/b",
           t"+++ b/b",
           t"@@ -1 +1 @@",
           t"-2",
           t"+2b" ).size
      .assert(_ == 2)

      test(m"multiple hunks in one file are kept separate"):
        patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -1 +1 @@",
           t"-a",
           t"+a1",
           t"@@ -10 +10 @@",
           t"-b",
           t"+b1" ).stdlib.head.hunks.size
      .assert(_ == 2)

      test(m"hunk records line range from @@ header"):
        val hunk = patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -5,3 +7,4 @@",
           t" ctx5",
           t"-old6",
           t"+new7",
           t"+new8",
           t" ctx9" ).stdlib.head.hunks.stdlib.head
        (hunk.oldStart, hunk.oldLines, hunk.newStart, hunk.newLines)
      .assert(_ == (5, 3, 7, 4))

      test(m"a single-line hunk omits the count field"):
        val hunk = patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -1 +1 @@",
           t"-x",
           t"+y" ).stdlib.head.hunks.stdlib.head
        (hunk.oldStart, hunk.oldLines, hunk.newStart, hunk.newLines)
      .assert(_ == (1, 1, 1, 1))

      test(m"hunk edits get accurate left/right line numbers"):
        val edits = patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -5,3 +7,4 @@",
           t" ctx5",
           t"-old6",
           t"+new7",
           t"+new8",
           t" ctx9" ).stdlib.head.hunks.stdlib.head.edits

        edits == List
         ( Par(5, 7, t"ctx5"),
           Del(6, t"old6"),
           Ins(8, t"new7"),
           Ins(9, t"new8"),
           Par(7, 10, t"ctx9") )
      .assert(_ == true)

      test(m"section header after @@ is preserved"):
        patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -1 +1 @@ def someFunction()",
           t"-old",
           t"+new" ).stdlib.head.hunks.stdlib.head.section
      .assert(_ == t"def someFunction()")

      test(m"`\\ No newline at end of file` is ignored"):
        patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -1 +1 @@",
           t"-old",
           t"\\ No newline at end of file",
           t"+new",
           t"\\ No newline at end of file" ).stdlib.head.hunks.stdlib.head.edits.size
      .assert(_ == 2)

      test(m"a binary diff yields a FileDiff with no hunks"):
        val file = patchFrom
         ( t"diff --git a/blob b/blob",
           t"index 1234567..89abcde",
           t"Binary files a/blob and b/blob differ" ).stdlib.head
        file.hunks
      .assert(_.nil)

      test(m"asDiff flattens hunks into a Dissonance Diff"):
        val file = patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -1 +1 @@",
           t"-old",
           t"+new",
           t"@@ -10 +10 @@",
           t"-other",
           t"+also" ).stdlib.head
        Patch.asDiff(file).edits.length
      .assert(_ == 4)

      test(m"a header with no --- / +++ block still records paths from `diff --git`"):
        val file = patchFrom
         ( t"diff --git a/foo b/foo",
           t"old mode 100644",
           t"new mode 100755" ).stdlib.head
        (file.oldPath, file.newPath)
      .assert(_ == (t"foo", t"foo"))

    // Compile-time checks on the handle types; the backend is immaterial, so they are not run per
    // backend (and `demilitarize` needs its given in the file's scope, not a parameter's).
    suite(m"Grant gating"):
      given Choice = native

      test(m"a write operation without the Write grant does not compile"):
        demilitarize:
          val dir = freshRepo()
          dir.open[Git.Worktree]() { worktree ?=> worktree.commit(t"nope", worktree.head()) }
        . map(_.message)
      .assert(_.nonEmpty)

      test(m"the handle cannot be the block's result"):
        demilitarize:
          val dir = freshRepo()
          def attempt(): Git.Repo.Handle^ = dir.open[Git.Repo]() { repo ?=> repo }
        . map(_.message.tt)
      .assert(_.exists(_.contains(t"outlives its scope")))

    // Every suite that touches a repository runs under both backends, on identical fixtures: the
    // native one must agree with the one that drives `git`.
    def repositorySuites[topic <: Label]()(using Choice, Testable of topic): Unit =
      // ----- Opening repositories -------------------------------------------

      suite(m"Opening repositories"):

        test(m"Git.init creates a .git directory under the worktree"):
          val dir = freshRepo()
          (dir / ".git").existent()
        .assert(_ == true)

        test(m"a worktree handle's stem is the directory it was opened on"):
          val dir = freshRepo()
          dir.open[Git.Worktree]() { worktree ?=> worktree.stem == dir }
        .assert(_ == true)

        test(m"a worktree handle's git directory is .git under the stem"):
          val dir = freshRepo()
          dir.open[Git.Worktree]() { worktree ?=> worktree.gitDir == dir / ".git" }
        .assert(_ == true)

        test(m"a bare repository opens as a Git.Repo with itself as the git directory"):
          val dir = freshDir()
          Git.init(dir, bare = true)
          dir.open[Git.Repo]() { repo ?=> repo.gitDir == dir }
        .assert(_ == true)

        test(m"a bare repository leaves no .git subdirectory"):
          val dir = freshDir()
          Git.init(dir, bare = true)
          !(dir / ".git").existent()
        .assert(_ == true)

        test(m"a bare repository cannot be opened as a worktree"):
          val dir = freshDir()
          Git.init(dir, bare = true)
          capture[Git.Error](dir.open[Git.Worktree]() { worktree ?=> () }).reason
        .assert(_ == Git.Error.Reason.NoWorktree)

        test(m"a directory which is not a repository cannot be opened"):
          val dir = freshDir()
          capture[Git.Error](dir.open[Git.Repo]() { repo ?=> () }).reason == Git.Error.Reason.NoRepository(dir.encode)
        .assert(_ == true)

        test(m"a .git directory opens as the worktree containing it"):
          val dir = freshRepo()
          (dir / ".git").open[Git.Worktree]() { worktree ?=> worktree.stem == dir }
        .assert(_ == true)


        test(m"a stale value leaves the block, since it is pure data"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.head()
          . value.current
        .assert(_ == Git.Branch(t"main"))

      // ----- HEAD and the log ---------------------------------------------------

      suite(m"HEAD and the log"):

        test(m"a fresh repository's HEAD is attached to an unborn main"):
          withWorktree: worktree ?=>
            worktree.head().value
        .assert(_ == Git.Head.Attached(Git.Branch(t"main"), Unset))

        test(m"a fresh repo has an empty log"):
          withWorktree(messages)
        .assert(_.nil)

        test(m"the first commit gives HEAD a tip"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.head().value == Git.Head.Attached(Git.Branch(t"main"), hash)
        .assert(_ == true)

        test(m"log returns commits newest-first"):
          withWorktree:
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            commitFile(t"c", t"c\n", t"third")
            messages
        .assert(_ == List(t"third", t"second", t"first"))

        test(m"each parsed Commit has the correct number of parents"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"root")
            commitFile(t"b", t"b\n", t"second")
            worktree.log().value.map(_.parents.size)
        .assert(_ == List(1, 0))

        test(m"a parsed commit carries the configured author"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.commitAt(hash).author.identity
        .assert(_ == Git.Identity(t"Octogenarian", t"octogenarian@test.local"))

        test(m"commit returns the hash HEAD resolves to"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.resolve(Git.Revision.head()).value == hash
        .assert(_ == true)

        test(m"HEAD~1 resolves to the previous commit"):
          withWorktree: worktree ?=>
            val first  = commitFile(t"a", t"a\n", t"first")
            val second = commitFile(t"b", t"b\n", t"second")
            first != second && worktree.resolve(Git.Revision.head(1)).value == first
        .assert(_ == true)

        test(m"a tag resolves to the tagged commit"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.makeTag(Git.Tag(t"v1"), hash)
            worktree.resolve(Git.Tag(t"v1")).value == hash
        .assert(_ == true)

        test(m"a branch resolves to its tip"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.resolve(Git.Branch(t"main")).value == hash
        .assert(_ == true)

        test(m"an unknown revision resolves to nothing"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.resolve(Git.Revision.unsafe(t"nonexistent")).value
        .assert(_.absent)

        test(m"a commit object can be read back by hash"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.show(hash).kind
        .assert(_ == Git.Object.Kind.Commit)

        test(m"a tree lists its entries"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.treeAt(worktree.commitAt(hash).tree).map(_.name)
        .assert(_ == List(t"a"))

        test(m"a blob's content is read back"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val entry = worktree.treeAt(worktree.commitAt(hash).tree).prim.or(abort(Git.Error(Git.Error.Reason.Unexpected)))
            String(worktree.blobAt(entry.hash).asInstanceOf[scala.Array[Byte]], "UTF-8").nn.tt
        .assert(_ == t"a\n")

      // ----- compare-and-swap ----------------------------------------------------

      suite(m"Compare-and-swap"):

        test(m"a commit against a stale HEAD fails with Moved"):
          withWorktree: worktree ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            val head = worktree.head()
            sh"git -C ${worktree.stem} commit --allow-empty -q -m external".exec[Exit]()
            val external = worktree.resolve(Git.Revision.head()).value
            write(t"b", t"b\n")
            worktree.add(planePath(t"b"))
            capture[Git.Error](worktree.commit(t"mine", head)).reason match
              case Git.Error.Reason.Moved(_, expected, actual) => (expected, actual) == ((first, external))
              case _                                           => false
        .assert(_ == true)

        test(m"a commit against the current HEAD succeeds despite a dirty tree"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"untracked", t"x\n")
            val head = worktree.head()
            write(t"b", t"b\n")
            worktree.add(planePath(t"b"))
            worktree.commit(t"second", head)
            messages
        .assert(_ == List(t"second", t"first"))

        test(m"updateRef creates a ref expected to be absent"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val ref = Git.Refs.heads(t"other")
            worktree.updateRef(ref, hash, worktree.ref(ref))
            worktree.ref(ref).value == hash
        .assert(_ == true)

        test(m"updateRef against a moved ref fails with Moved"):
          withWorktree: worktree ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            val ref = Git.Refs.heads(t"other")
            worktree.updateRef(ref, first, worktree.ref(ref))
            val seen = worktree.ref(ref)
            val second = commitFile(t"b", t"b\n", t"second")
            sh"git -C ${worktree.stem} update-ref refs/heads/other ${second.text}".exec[Exit]()
            capture[Git.Error](worktree.updateRef(ref, Unset, seen)).reason
            == Git.Error.Reason.Moved(t"refs/heads/other", first, second)
        .assert(_ == true)

        test(m"deleting a branch at a stale tip fails with Moved"):
          withWorktree: worktree ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            worktree.makeBranch(Git.Branch(t"feature"), first)
            val tips = worktree.branches()
            sh"git -C ${worktree.stem} update-ref refs/heads/feature ${first.text}".exec[Exit]()
            val second = commitFile(t"b", t"b\n", t"second")
            sh"git -C ${worktree.stem} update-ref refs/heads/feature ${second.text}".exec[Exit]()
            val at = tips.let(_.at(Git.Branch(t"feature")).or(first))
            capture[Git.Error](worktree.deleteBranch(Git.Branch(t"feature"), at, force = true)).reason match
              case Git.Error.Reason.Moved(_, _, _) => true
              case _                               => false
        .assert(_ == true)

        test(m"a note write against a stale notes ref fails with Moved"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val seen = worktree.ref(Git.Refs.defaultNotes)
            sh"git -C ${worktree.stem} notes add -m external ${hash.text}".exec[Exit]()
            capture[Git.Error](worktree.addNote(hash, t"mine", seen)).reason match
              case Git.Error.Reason.Moved(_, _, _) => true
              case _                               => false
        .assert(_ == true)

        test(m"a reset against a moved HEAD fails with Moved"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            val head = worktree.head()
            sh"git -C ${worktree.stem} commit --allow-empty -q -m external".exec[Exit]()
            capture[Git.Error](worktree.reset(ResetMode.Soft, Git.Revision.head(1), head)).reason match
              case Git.Error.Reason.Moved(t"HEAD", _, _) => true
              case _                                     => false
        .assert(_ == true)

      // ----- status ---------------------------------------------------------

      suite(m"status"):

        test(m"a clean repo has empty status"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.status().value
        .assert(_.nil)

        test(m"an untracked file shows as Untracked"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"new.txt", t"new\n")
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"new.txt") && e.index == Git.Status.Untracked
        .assert(_ == true)

        test(m"an added file shows as Added"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"new.txt", t"new\n")
            worktree.add(planePath(t"new.txt"))
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"new.txt") && e.index == Git.Status.Added
        .assert(_ == true)

        test(m"a modified-but-not-staged file shows as Updated in the worktree column"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"a", t"a-changed\n")
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"a") && e.worktree == Git.Status.Updated
        .assert(_ == true)

        test(m"a deleted-but-not-staged file shows as Deleted"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            sh"rm ${worktree.stem}/a".exec[Exit]()
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"a") && e.worktree == Git.Status.Deleted
        .assert(_ == true)

        test(m"status with ignored = true shows ignored files"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t".gitignore", t"ignored.txt\n", t"add gitignore")
            write(t"ignored.txt", t"x\n")

            worktree.status(ignored = true).value.stdlib.exists: e =>
              e.path == planePath(t"ignored.txt") && e.index == Git.Status.Ignored
        .assert(_ == true)

      // ----- add / commit / unstage / mv / rm / restore / clean ----------------

      suite(m"Index and working files"):

        test(m"add + commit puts a file under version control"):
          withWorktree:
            commitFile(t"hello.txt", t"hello\n", t"initial")
            messages.size
        .assert(_ == 1)

        test(m"committing nothing fails with NothingToCommit"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            capture[Git.Error](worktree.commit(t"empty", worktree.head())).reason
        .assert(_ == Git.Error.Reason.NothingToCommit)

        test(m"an empty commit is allowed when asked for"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.commit(t"empty", worktree.head(), allowEmpty = true)
            messages
        .assert(_ == List(t"empty", t"first"))

        test(m"amend rewrites the tip's message and keeps its tree"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            val tip = commitFile(t"b", t"b\n", t"secnod")
            val amended = worktree.amend(t"second", worktree.head())
            (messages, worktree.commitAt(amended).tree == worktree.commitAt(tip).tree, worktree.commitAt(amended).parents == worktree.commitAt(tip).parents)
        .assert(_ == ((List(t"second", t"first"), true, true)))

        test(m"unstage leaves the file on disk and removes it from the index"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"new.txt", t"new\n")
            worktree.add(planePath(t"new.txt"))
            worktree.unstage(planePath(t"new.txt"))
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"new.txt") && e.index == Git.Status.Untracked
        .assert(_ == true)

        test(m"mv renames a tracked file"):
          withWorktree: worktree ?=>
            commitFile(t"old.txt", t"hello\n", t"first")
            worktree.mv(planePath(t"old.txt"), planePath(t"new.txt"))
            !(worktree.stem / "old.txt").existent() && (worktree.stem / "new.txt").existent()
        .assert(_ == true)

        test(m"mv stages the rename for the next commit"):
          withWorktree: worktree ?=>
            commitFile(t"old.txt", t"hello\n", t"first")
            worktree.mv(planePath(t"old.txt"), planePath(t"new.txt"))
            worktree.commit(t"rename", worktree.head())
            messages.size
        .assert(_ == 2)

        test(m"rm removes a tracked file from the index and the disk"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.rm(planePath(t"a"))()
            !(worktree.stem / "a").existent() && worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"a") && e.index == Git.Status.Deleted
        .assert(_ == true)

        test(m"rm with cached keeps the file on disk"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.rm(planePath(t"a"))(cached = true)
            (worktree.stem / "a").existent() && worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"a") && e.index == Git.Status.Deleted
        .assert(_ == true)

        test(m"rm of an untracked path fails with Untracked"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"loose", t"x\n")
            capture[Git.Error](worktree.rm(planePath(t"loose"))()).reason
        .assert(_ == Git.Error.Reason.Untracked(t"loose"))

        test(m"restore discards a working-file change"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"a", t"changed\n")
            worktree.restore(planePath(t"a"))()
            worktree.status().value
        .assert(_.nil)

        test(m"restore with staged unstages a change and keeps it on disk"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"a", t"changed\n")
            worktree.add(planePath(t"a"))
            worktree.restore(planePath(t"a"))(staged = true)
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"a") && e.index == Unset && e.worktree == Git.Status.Updated
        .assert(_ == true)

        test(m"clean removes untracked files and reports them"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"junk", t"x\n")
            val removed = worktree.clean().map(Operations.relative(_))
            (removed, (worktree.stem / "junk").existent(), (worktree.stem / "a").existent())
        .assert(_ == ((List(t"junk"), false, true)))

        test(m"tracked lists the files in the index"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            worktree.tracked().value.map(Operations.relative(_))
        .assert(_ == List(t"a", t"b"))

      // ----- reset modes ----------------------------------------------------

      suite(m"reset modes"):

        test(m"reset --soft moves HEAD but keeps the working tree"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            worktree.reset(ResetMode.Soft, Git.Revision.head(1), worktree.head())
            messages.size == 1 && (worktree.stem / "b").existent()
        .assert(_ == true)

        test(m"reset --soft leaves changes staged"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            worktree.reset(ResetMode.Soft, Git.Revision.head(1), worktree.head())
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"b") && e.index == Git.Status.Added
        .assert(_ == true)

        test(m"reset --mixed moves HEAD and unstages, but keeps the working tree"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            worktree.reset(ResetMode.Mixed, Git.Revision.head(1), worktree.head())
            worktree.status().value.stdlib.exists: e =>
              e.path == planePath(t"b") && e.index == Git.Status.Untracked
        .assert(_ == true)

        test(m"reset --hard discards both index and working tree"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            worktree.reset(ResetMode.Hard, Git.Revision.head(1), worktree.head())
            !(worktree.stem / "b").existent() && worktree.status().value.nil
        .assert(_ == true)

      // ----- branches and tags ----------------------------------------------

      suite(m"branches and tags"):

        test(m"a fresh repo with one commit has just main"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.branches().value.keys.to[List].map(_.show)
        .assert(_ == List(t"main"))

        test(m"makeBranch adds a new branch at the given commit"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.makeBranch(Git.Branch(t"feature"), hash)
            worktree.branches().value.keys.to[Set]
        .assert(_ == Set(Git.Branch(t"main"), Git.Branch(t"feature")))

        test(m"makeBranch of an existing branch fails with Moved from nothing"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            capture[Git.Error](worktree.makeBranch(Git.Branch(t"main"), hash)).reason
            == Git.Error.Reason.Moved(t"refs/heads/main", Unset, hash)
        .assert(_ == true)

        test(m"switch with create moves HEAD to a new branch"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            worktree.currentBranch().value
        .assert(_ == Git.Branch(t"feature"))

        test(m"switch moves HEAD to the named branch"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            worktree.switch(Git.Branch(t"main"), worktree.head())
            worktree.currentBranch().value
        .assert(_ == Git.Branch(t"main"))

        test(m"checkout of a commit detaches HEAD"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.checkout(hash, worktree.head()).value == Git.Head.Detached(hash)
        .assert(_ == true)

        test(m"deleteBranch removes a merged branch"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.makeBranch(Git.Branch(t"feature"), hash)
            worktree.deleteBranch(Git.Branch(t"feature"), Stale(hash))
            worktree.branches().value.keys.to[List].map(_.show)
        .assert(_ == List(t"main"))

        test(m"deleteBranch refuses an unmerged branch"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            val tip = commitFile(t"b", t"b\n", t"feature work")
            worktree.switch(Git.Branch(t"main"), worktree.head())
            capture[Git.Error](worktree.deleteBranch(Git.Branch(t"feature"), Stale(tip))).reason
            == Git.Error.Reason.Unmerged(Git.Branch(t"feature"), tip)
        .assert(_ == true)

        test(m"deleteBranch with force removes an unmerged branch"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            val tip = commitFile(t"b", t"b\n", t"feature work")
            worktree.switch(Git.Branch(t"main"), worktree.head())
            worktree.deleteBranch(Git.Branch(t"feature"), Stale(tip), force = true)
            worktree.branches().value.keys.to[List].map(_.show)
        .assert(_ == List(t"main"))

        test(m"renameBranch updates the branch name"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.makeBranch(Git.Branch(t"oldname"), hash)
            worktree.renameBranch(Git.Branch(t"oldname"), Git.Branch(t"newname"), Stale(hash))
            val names = worktree.branches().value.keys.to[List].map(_.show).to[Set]
            names.has(t"newname") && !names.has(t"oldname")
        .assert(_ == true)

        test(m"renaming the checked-out branch keeps HEAD attached to it"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.renameBranch(Git.Branch(t"main"), Git.Branch(t"trunk"), Stale(hash))
            worktree.currentBranch().value
        .assert(_ == Git.Branch(t"trunk"))

        test(m"tags() is empty for a tagless repo"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.tags().value
        .assert(_ == Map())

        test(m"makeTag creates a lightweight tag at a commit"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.makeTag(Git.Tag(t"v1"), hash)
            worktree.tag(Git.Tag(t"v1")).value == Git.Tag.Target.Lightweight(hash)
        .assert(_ == true)

        test(m"an annotated tag carries its message"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val annotation = worktree.annotatedTag(Git.Tag(t"v1"), hash, t"Release one")
            val target = worktree.tag(Git.Tag(t"v1")).value
            (worktree.annotationAt(annotation).message, target == Git.Tag.Target.Annotated(annotation, hash))
        .assert(_ == ((t"Release one", true)))

        test(m"deleteTag removes a tag"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.makeTag(Git.Tag(t"v1"), hash)
            worktree.deleteTag(Git.Tag(t"v1"), Stale(hash))
            worktree.tags().value
        .assert(_ == Map())

        test(m"multiple tags are reported with their commits"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.makeTag(Git.Tag(t"v3"), hash)
            worktree.makeTag(Git.Tag(t"v1"), hash)
            worktree.makeTag(Git.Tag(t"v2"), hash)
            worktree.tags().value.keys.to[List].map(_.show).to[Set]
        .assert(_ == Set(t"v1", t"v2", t"v3"))

      // ----- reflog ---------------------------------------------------------

      suite(m"reflog"):

        test(m"a fresh repo has no reflog entries"):
          withWorktree: worktree ?=>
            worktree.reflog().value
        .assert(_.nil)

        test(m"reflog returns one entry per commit (newest first)"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            worktree.reflog().value.size
        .assert(_ == 2)

        test(m"a reset adds a `reset:` entry to the reflog"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            commitFile(t"b", t"b\n", t"second")
            worktree.reset(ResetMode.Soft, Git.Revision.head(1), worktree.head())
            worktree.reflog().value.stdlib.head.message.starts(t"reset:")
        .assert(_ == true)

        test(m"reflog entries carry the commit hash"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.reflog().value.stdlib.head.hash == hash
        .assert(_ == true)

      // ----- diff (integration) ---------------------------------------------

      suite(m"diff (integration)"):

        test(m"diff() reports working-tree changes"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"first\n", t"first")
            write(t"a", t"first\nsecond\n")

            val files = worktree.diff().value
            files.size == 1
              && files.stdlib.head.changeKind == ChangeKind.Modified
              && files.stdlib.head.hunks.bind(_.edits).stdlib.exists:
                  case Ins(_, t"second") => true
                  case _                 => false
        .assert(_ == true)

        test(m"diffStaged reports staged changes"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"b", t"b\n")
            worktree.add(planePath(t"b"))
            val staged = worktree.diffStaged().value
            staged.size == 1
              && staged.stdlib.head.changeKind == ChangeKind.Added
              && staged.stdlib.head.newPath == t"b"
        .assert(_ == true)

        test(m"diff() ignores already-committed changes"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.diff().value
        .assert(_.nil)

        test(m"diffAgainst compares working tree to a revision"):
          withWorktree: worktree ?=>
            val first = commitFile(t"a", t"v1\n", t"v1")
            commitFile(t"a", t"v2\n", t"v2")
            worktree.diffAgainst(first).value.size
        .assert(_ == 1)

        test(m"diffBetween shows changes between two commits"):
          withWorktree: worktree ?=>
            val first  = commitFile(t"a", t"v1\n", t"v1")
            val second = commitFile(t"a", t"v2\n", t"v2")
            worktree.diffBetween(first, second).size
        .assert(_ == 1)

        test(m"diff records a Deleted file with old path and no new path"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            sh"rm ${worktree.stem}/a".exec[Exit]()
            worktree.add(planePath(t"a"))
            val file = worktree.diffStaged().value.stdlib.head
            file.changeKind == ChangeKind.Deleted
              && file.oldPath == t"a"
              && file.newPath == Unset
        .assert(_ == true)

      // ----- merge / cherry-pick / revert / rebase -----------------------------

      suite(m"merge / cherry-pick / revert / rebase"):

        def diverged()(using worktree: (Git.Worktree.Handle & Granting[Grant.Read & Grant.Write])^): Git.Hash =
          commitFile(t"a", t"a\n", t"base")
          worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
          val feature = commitFile(t"b", t"b\n", t"feature")
          worktree.switch(Git.Branch(t"main"), worktree.head())
          feature

        test(m"merge fast-forwards onto a branch ahead of HEAD"):
          withWorktree: worktree ?=>
            val feature = diverged()
            val outcome = worktree.merge(Git.Branch(t"feature"), worktree.head(), ff = FastForward.Only)
            (outcome == Git.Merge.Outcome.FastForwarded(feature), messages.size)
        .assert(_ == ((true, 2)))

        test(m"merge with FastForward.Auto fast-forwards a clean lineage"):
          withWorktree: worktree ?=>
            diverged()
            worktree.merge(Git.Branch(t"feature"), worktree.head())
            worktree.log().value.map(_.parents.size)
        .assert(_ == List(1, 0))

        test(m"merge with FastForward.Never creates a merge commit"):
          withWorktree: worktree ?=>
            diverged()
            val outcome = worktree.merge(Git.Branch(t"feature"), worktree.head(), ff = FastForward.Never, message = t"merge")
            (worktree.log().value.stdlib.head.parents.size, outcome.isInstanceOf[Git.Merge.Outcome.Merged])
        .assert(_ == ((2, true)))

        test(m"merging an already-merged branch is UpToDate"):
          withWorktree: worktree ?=>
            diverged()
            worktree.merge(Git.Branch(t"feature"), worktree.head())
            worktree.merge(Git.Branch(t"feature"), worktree.head())
        .assert(_ == Git.Merge.Outcome.UpToDate)

        test(m"merge with FastForward.Only refuses a non-fast-forward"):
          withWorktree: worktree ?=>
            diverged()
            commitFile(t"c", t"c\n", t"main work")
            capture[Git.Error](worktree.merge(Git.Branch(t"feature"), worktree.head(), ff = FastForward.Only)).reason
        .assert(_ == Git.Error.Reason.NotFastForward)

        test(m"a conflicting merge stops, naming the conflicting path"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"base\n", t"base")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            commitFile(t"a", t"theirs\n", t"feature")
            worktree.switch(Git.Branch(t"main"), worktree.head())
            commitFile(t"a", t"ours\n", t"main work")
            worktree.merge(Git.Branch(t"feature"), worktree.head()) match
              case Git.Merge.Outcome.Stopped(Git.Operation.Merging(_, conflicting)) => conflicting
              case other                                                           => Nil
        .assert(_ == List(t"a"))

        test(m"a stopped merge is abandoned back to the sighted HEAD"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"base\n", t"base")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            commitFile(t"a", t"theirs\n", t"feature")
            worktree.switch(Git.Branch(t"main"), worktree.head())
            val main = commitFile(t"a", t"ours\n", t"main work")
            worktree.merge(Git.Branch(t"feature"), worktree.head())
            val restored = worktree.abandon()
            (restored.value.commit == main, worktree.operation().value.absent)
        .assert(_ == ((true, true)))

        test(m"a stopped merge continues once the conflict is resolved"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"base\n", t"base")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            commitFile(t"a", t"theirs\n", t"feature")
            worktree.switch(Git.Branch(t"main"), worktree.head())
            commitFile(t"a", t"ours\n", t"main work")
            worktree.merge(Git.Branch(t"feature"), worktree.head())
            write(t"a", t"resolved\n")
            worktree.add(planePath(t"a"))
            worktree.continue() match
              case Git.Sequence.Outcome.Completed(_) => worktree.log().value.stdlib.head.parents.size
              case _                                 => 0
        .assert(_ == 2)

        test(m"cherryPick replays a commit on the current branch"):
          withWorktree: worktree ?=>
            val feature = diverged()
            worktree.cherryPick(List(feature), worktree.head())
            (worktree.stem / "b").existent()
        .assert(_ == true)

        test(m"cherryPick advances HEAD by one commit"):
          withWorktree: worktree ?=>
            val feature = diverged()
            val before = messages.size
            worktree.cherryPick(List(feature), worktree.head())
            messages.size - before
        .assert(_ == 1)

        test(m"revert produces a new commit that undoes the original"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"base")
            val toRevert = commitFile(t"b", t"b\n", t"add b")
            worktree.revert(List(toRevert), worktree.head())
            messages.size == 3 && !(worktree.stem / "b").existent()
        .assert(_ == true)

        test(m"revert with noCommit leaves the inverse staged"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"base")
            val toRevert = commitFile(t"b", t"b\n", t"add b")
            worktree.revert(List(toRevert), worktree.head(), noCommit = true)
            messages.size == 2
              && worktree.status().value.stdlib.exists: e =>
                  e.path == planePath(t"b") && e.index == Git.Status.Deleted
        .assert(_ == true)

        test(m"rebase replays the branch onto the new base"):
          withWorktree: worktree ?=>
            diverged()
            commitFile(t"c", t"c\n", t"main work")
            worktree.switch(Git.Branch(t"feature"), worktree.head())
            val outcome = worktree.rebase(Git.Branch(t"main"), worktree.head())
            (outcome.isInstanceOf[Git.Sequence.Outcome.Completed], messages)
        .assert(_ == ((true, List(t"feature", t"main work", t"base"))))

        test(m"a conflicting rebase stops and can be skipped"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"base\n", t"base")
            worktree.switch(Git.Branch(t"feature"), worktree.head(), create = true)
            commitFile(t"a", t"theirs\n", t"feature")
            worktree.switch(Git.Branch(t"main"), worktree.head())
            commitFile(t"a", t"ours\n", t"main work")
            worktree.switch(Git.Branch(t"feature"), worktree.head())
            val stopped = worktree.rebase(Git.Branch(t"main"), worktree.head()).isInstanceOf[Git.Sequence.Outcome.Stopped]
            val skipped = worktree.skip().isInstanceOf[Git.Sequence.Outcome.Completed]
            (stopped, skipped, messages)
        .assert(_ == ((true, true, List(t"main work", t"base"))))

        test(m"continue with nothing in progress fails with NoOperationInProgress"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            capture[Git.Error](worktree.continue()).reason
        .assert(_ == Git.Error.Reason.NoOperationInProgress)

      // ----- stash ---------------------------------------------------------------

      suite(m"stash"):

        test(m"stash with no changes saves nothing"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.stash()
        .assert(_.absent)

        test(m"stash saves a change and cleans the tree"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"a", t"changed\n")
            val stashed = worktree.stash(message = t"wip")
            (stashed.present, worktree.status().value.nil, worktree.stashes().value.size)
        .assert(_ == ((true, true, 1)))

        test(m"popStash restores the change and drops the entry"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"a", t"changed\n")
            worktree.stash()
            val entry = worktree.stashes().let(_.prim.or(abort(Git.Error(Git.Error.Reason.NoSuchStash))))
            worktree.popStash(entry)
            (worktree.status().value.size, worktree.stashes().value.size)
        .assert(_ == ((1, 0)))

        test(m"dropStash discards an entry"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"a", t"changed\n")
            worktree.stash()
            val entry = worktree.stashes().let(_.prim.or(abort(Git.Error(Git.Error.Reason.NoSuchStash))))
            worktree.dropStash(entry)
            worktree.stashes().value
        .assert(_.nil)

        test(m"dropping a stale stash entry fails with NoSuchStash"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            write(t"a", t"changed\n")
            worktree.stash()
            val entry = worktree.stashes().let(_.prim.or(abort(Git.Error(Git.Error.Reason.NoSuchStash))))
            sh"git -C ${worktree.stem} stash drop -q".exec[Exit]()
            capture[Git.Error](worktree.dropStash(entry)).reason
        .assert(_ == Git.Error.Reason.NoSuchStash)

      // ----- config ------------------------------------------------------------

      suite(m"config"):

        test(m"a configured value reads back typed"):
          withWorktree: worktree ?=>
            worktree.config { config ?=> config.get[Text](Git.Config.Key.parse(t"user.name")).value }
        .assert(_ == t"Octogenarian")

        test(m"an unset key reads back as nothing"):
          withWorktree: worktree ?=>
            worktree.config { config ?=> config.get[Text](Git.Config.Key.parse(t"octogenarian.missing")).value }
        .assert(_.absent)

        test(m"set then get round-trips an integer"):
          withWorktree: worktree ?=>
            val key = Git.Config.Key.parse(t"octogenarian.answer")
            worktree.configure { config ?=> config.set(key, 42) }
            worktree.config { config ?=> config.get[Int](key).value }
        .assert(_ == 42)

        test(m"add accumulates multiple values under one key"):
          withWorktree: worktree ?=>
            val key = Git.Config.Key.parse(t"octogenarian.many")
            worktree.configure: config ?=>
              config.add(key, t"one")
              config.add(key, t"two")
            worktree.config { config ?=> config.all[Text](key).value }
        .assert(_ == List(t"one", t"two"))

        test(m"unset removes a key"):
          withWorktree: worktree ?=>
            val key = Git.Config.Key.parse(t"octogenarian.gone")
            worktree.configure: config ?=>
              config.set(key, t"here")
              config.unset(key)
            worktree.config { config ?=> config.get[Text](key).value }
        .assert(_.absent)

        test(m"a subsection key keeps its case"):
          Git.Config.Key.parse(t"Remote.Origin.URL")
        .assert(_ == Git.Config.Key(t"remote", t"Origin", t"url"))

        test(m"a key with no section fails to parse"):
          safely(Git.Config.Key.parse(t"nosection"))
        .assert(_.absent)

        test(m"a configuration file opens on its path"):
          val file = freshDir() / "config"
          file.create[File]()
          file.open[Git.Config](Read & Write): config ?=>
            config.set(Git.Config.Key.parse(t"alpha.beta"), t"gamma")
            config.get[Text](Git.Config.Key.parse(t"alpha.beta")).value
        .assert(_ == t"gamma")

      // ----- worktree management --------------------------------------------

      suite(m"worktree management"):

        test(m"a fresh repo lists exactly one worktree"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.worktrees().value.size
        .assert(_ == 1)

        test(m"addWorktree creates a second worktree sharing the object DB"):
          withWorktree: primary ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            val secondaryPath = freshDir()
            sh"rm -rf $secondaryPath".exec[Exit]()
            val created = primary.addWorktree(secondaryPath, first, detach = true)
            val secondaryLog = created.open[Git.Worktree]() { secondary ?=> secondary.log().value.size }
            (messages.size, secondaryLog, primary.worktrees().value.size)
        .assert(_ == ((1, 1, 2)))

        test(m"a secondary worktree resolves the same hash as the primary"):
          withWorktree: primary ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            val secondaryPath = freshDir()
            sh"rm -rf $secondaryPath".exec[Exit]()
            val created = primary.addWorktree(secondaryPath, first, detach = true)
            created.open[Git.Worktree]() { secondary ?=> secondary.resolve(Git.Revision.head()).value == first }
        .assert(_ == true)

        test(m"removeWorktree shrinks the listing back to one"):
          withWorktree: primary ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            val secondaryPath = freshDir()
            sh"rm -rf $secondaryPath".exec[Exit]()
            val created = primary.addWorktree(secondaryPath, first, detach = true)
            val entry = primary.worktrees().let(_.reap { case entry if entry.path == created => entry })
            entry.value.let(e => primary.removeWorktree(Stale(e)))
            primary.worktrees().value.size
        .assert(_ == 1)

        test(m"pruneWorktrees succeeds on a clean repo"):
          withWorktree: primary ?=>
            commitFile(t"a", t"a\n", t"first")
            primary.pruneWorktrees()
            primary.worktrees().value.size
        .assert(_ == 1)

        test(m"lock and unlock a secondary worktree"):
          withWorktree: primary ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            val secondaryPath = freshDir()
            sh"rm -rf $secondaryPath".exec[Exit]()
            val created = primary.addWorktree(secondaryPath, first, detach = true)
            def entry = primary.worktrees().let(_.reap { case entry if entry.path == created => entry })
            entry.value.let(e => primary.lockWorktree(Stale(e), reason = t"running CI"))
            val locked = entry.value.let(_.locked)
            entry.value.let(e => primary.unlockWorktree(Stale(e)))
            entry.value.let(e => primary.removeWorktree(Stale(e)))
            (locked, primary.worktrees().value.size)
        .assert(_ == ((t"running CI", 1)))

      // ----- remote management ----------------------------------------------

      suite(m"remote management"):

        test(m"a fresh repo has no remotes"):
          withWorktree: worktree ?=>
            worktree.remotes().value
        .assert(_.nil)

        test(m"addRemote then remotes() round-trip"):
          withWorktree: worktree ?=>
            worktree.addRemote(t"origin", t"git@example.com:foo/bar.git")
            worktree.remotes().value
        .assert(_ == List(Remote(t"origin", t"git@example.com:foo/bar.git")))

        test(m"adding a remote twice fails with RemoteExists"):
          withWorktree: worktree ?=>
            worktree.addRemote(t"origin", t"git@example.com:foo/bar.git")
            capture[Git.Error](worktree.addRemote(t"origin", t"git@example.com:foo/bar.git")).reason
        .assert(_ == Git.Error.Reason.RemoteExists(t"origin"))

        test(m"removeRemote drops the remote"):
          withWorktree: worktree ?=>
            worktree.addRemote(t"origin", t"git@example.com:foo/bar.git")
            worktree.removeRemote(t"origin")
            worktree.remotes().value
        .assert(_.nil)

        test(m"removing an unknown remote fails with NoSuchRemote"):
          withWorktree: worktree ?=>
            capture[Git.Error](worktree.removeRemote(t"origin")).reason
        .assert(_ == Git.Error.Reason.NoSuchRemote(t"origin"))

        test(m"renameRemote changes the name"):
          withWorktree: worktree ?=>
            worktree.addRemote(t"origin", t"git@example.com:foo/bar.git")
            worktree.renameRemote(t"origin", t"upstream")
            worktree.remotes().value.map(_.name)
        .assert(_ == List(t"upstream"))

        test(m"setRemoteUrl with push sets the push URL"):
          withWorktree: worktree ?=>
            worktree.addRemote(t"origin", t"git@example.com:foo/bar.git")
            worktree.setRemoteUrl(t"origin", t"git@push.example.com:foo/bar.git", push = true)
            worktree.remote(t"origin").value.let(_.pushUrl)
        .assert(_ == t"git@push.example.com:foo/bar.git")

        test(m"two remotes are listed and addressable separately"):
          withWorktree: worktree ?=>
            worktree.addRemote(t"origin", t"git@a.example:foo.git")
            worktree.addRemote(t"upstream", t"git@b.example:bar.git")
            worktree.remotes().value.map(_.name).to[Set]
        .assert(_ == Set(t"origin", t"upstream"))

      // ----- clone / fetch / push (local transport) -----------------------------

      suite(m"clone, fetch and push"):

        def bareMirror(): Path on Linux =
          val bareDir = freshDir()
          Git.init(bareDir, initialBranch = Git.Branch(t"main"), bare = true)
          bareDir

        test(m"clone of a local bare repo produces a working copy"):
          val bareDir = bareMirror()
          withWorktree: source ?=>
            commitFile(t"a", t"a\n", t"first")
            source.addRemote(t"mirror", bareDir.encode)
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main"))))

          val targetPath = freshDir()
          sh"rm -rf $targetPath".exec[Exit]()
          Git.clone(bareDir, targetPath)
          targetPath.open[Git.Worktree]() { cloned ?=> cloned.log().value.size }
        .assert(_ == 1)

        test(m"push reports the ref it created"):
          val bareDir = bareMirror()
          withWorktree: source ?=>
            commitFile(t"a", t"a\n", t"first")
            source.addRemote(t"mirror", bareDir.encode)
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main")))).value.keys.to[List]
        .assert(_ == List(Git.Refs.heads(t"main")))

        test(m"a non-fast-forward push is rejected"):
          val bareDir = bareMirror()
          withWorktree: source ?=>
            commitFile(t"a", t"a\n", t"first")
            source.addRemote(t"mirror", bareDir.encode)
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main"))))
            source.amend(t"rewritten", source.head())
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main")))).value.values.to[List]
        .assert(_.exists(_.isInstanceOf[Git.Push.Outcome.Rejected]))

        test(m"a push with a stale lease fails with LeaseFailed"):
          val bareDir = bareMirror()
          withWorktree: source ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            source.addRemote(t"mirror", bareDir.encode)
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main"))))
            val seen = source.remoteRefs(t"mirror").let(_.at(Git.Refs.heads(t"main")))
            bareDir.open[Git.Repo](Read & Write): mirror ?=>
              val second = commitFile(t"b", t"b\n", t"second")
              mirror.forceRef(Git.Refs.heads(t"main"), Git.Hash.unsafe(t"0000000000000000000000000000000000000000"))
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main"))), force = Git.Push.Force.Lease(Map(Git.Refs.heads(t"main") -> seen)))
            . value.values.to[List].exists(_.isInstanceOf[Git.Push.Outcome.Rejected])
        .assert(_ == true)

        test(m"fetch brings a remote branch into refs/remotes"):
          val bareDir = bareMirror()
          withWorktree: source ?=>
            val first = commitFile(t"a", t"a\n", t"first")
            source.addRemote(t"mirror", bareDir.encode)
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main"))))

          val targetPath = freshRepo()
          targetPath.open[Git.Worktree](Read & Write): target ?=>
            target.addRemote(t"origin", bareDir.encode)
            target.fetch(t"origin")
            target.ref(Git.Refs.remotes(t"origin", t"main")).value.present
        .assert(_ == true)

        test(m"pull fast-forwards a tracking branch"):
          val bareDir = bareMirror()
          val sourceDir = freshRepo()
          val targetPath = freshDir()
          sh"rm -rf $targetPath".exec[Exit]()

          sourceDir.open[Git.Worktree](Read & Write): source ?=>
            commitFile(t"a", t"a\n", t"first")
            source.addRemote(t"mirror", bareDir.encode)
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main"))))

          Git.clone(bareDir, targetPath)

          sourceDir.open[Git.Worktree](Read & Write): source ?=>
            commitFile(t"b", t"b\n", t"second")
            source.push(t"mirror", List(Git.Refspec(Git.Refs.heads(t"main"))))

          targetPath.open[Git.Worktree](Read & Write): cloned ?=>
            cloned.pull(t"origin", cloned.head(), mode = Git.Pull.Mode.FastForwardOnly)
            cloned.log().value.size
        .assert(_ == 2)

      // ----- git notes ------------------------------------------------------

      suite(m"git notes"):

        def notesRef(using worktree: (Git.Worktree.Handle & Granting[Grant.Read & Grant.Write])^)
        :   Stale[Optional[Git.Hash]] =
          worktree.ref(Git.Refs.defaultNotes)

        test(m"add then read a note in the default namespace"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.addNote(hash, t"a note body", notesRef)
            worktree.note(hash).value
        .assert(_ == t"a note body")

        test(m"a commit with no note reads as nothing"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.note(hash).value
        .assert(_.absent)

        test(m"add then read in a custom namespace"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val ref  = Git.Refs.notes(t"ci-attestation")
            worktree.addNote(hash, t"signed envelope", worktree.ref(ref), ref = ref)
            worktree.note(hash, ref = ref).value
        .assert(_ == t"signed envelope")

        test(m"custom namespace and default namespace are independent"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val alt = Git.Refs.notes(t"alt")
            worktree.addNote(hash, t"default note", notesRef)
            worktree.addNote(hash, t"custom note", worktree.ref(alt), ref = alt)
            (worktree.note(hash).value, worktree.note(hash, alt).value)
        .assert(_ == ((t"default note", t"custom note")))

        test(m"add without force on an existing note fails with NoteExists"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.addNote(hash, t"first body", notesRef)
            capture[Git.Error](worktree.addNote(hash, t"second body", notesRef)).reason
            == Git.Error.Reason.NoteExists(hash)
        .assert(_ == true)

        test(m"add with force overwrites an existing note"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.addNote(hash, t"first body", notesRef)
            worktree.addNote(hash, t"second body", notesRef, force = true)
            worktree.note(hash).value
        .assert(_ == t"second body")

        test(m"append concatenates to an existing note"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.addNote(hash, t"line one", notesRef)
            worktree.appendNote(hash, t"line two", notesRef)
            val body = worktree.note(hash).value
            body.let(b => b.contains(t"line one") && b.contains(t"line two")).or(false)
        .assert(_ == true)

        test(m"remove deletes a note"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            worktree.addNote(hash, t"a body", notesRef)
            worktree.removeNote(hash, notesRef)
            worktree.note(hash).value
        .assert(_.absent)

        test(m"notes on an empty namespace is empty"):
          withWorktree: worktree ?=>
            commitFile(t"a", t"a\n", t"first")
            worktree.notes().value
        .assert(_ == Map())

        test(m"notes yields one entry per annotated commit"):
          withWorktree: worktree ?=>
            val first  = commitFile(t"a", t"a\n", t"first")
            val second = commitFile(t"b", t"b\n", t"second")
            worktree.addNote(first, t"note one", notesRef)
            worktree.addNote(second, t"note two", notesRef)
            worktree.notes().value.keys.to[Set] == Set(first, second)
        .assert(_ == true)

        test(m"copy duplicates a note onto another commit"):
          withWorktree: worktree ?=>
            val first  = commitFile(t"a", t"a\n", t"first")
            val second = commitFile(t"b", t"b\n", t"second")
            worktree.addNote(first, t"shared body", notesRef)
            worktree.copyNote(first, second, notesRef)
            worktree.note(second).value
        .assert(_ == t"shared body")

        test(m"a note write returns the new notes commit, which the ref then holds"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val commit = worktree.addNote(hash, t"body", notesRef)
            worktree.ref(Git.Refs.defaultNotes).value == commit
        .assert(_ == true)

      // ----- commit-rooted note access -------------------------------------

      suite(m"commit-rooted note access"):

        test(m"commit / namespace builds a NoteRef whose namespace is refs/notes/<namespace>"):
          val hash = Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd")
          (hash / t"foo").namespace.encode
        .assert(_ == t"refs/notes/foo")

        test(m"chaining / extends the namespace path"):
          val hash = Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd")
          (hash / t"foo" / t"bar").namespace.encode
        .assert(_ == t"refs/notes/foo/bar")

        test(m"a NoteRef's target round-trips through the path root"):
          val hash = Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd")
          (hash / t"foo").target == hash
        .assert(_ == true)

        test(m"(commit / namespace).content[Text] returns the note body"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            val ref = Git.Refs.notes(t"greeting")
            worktree.addNote(hash, t"hello", worktree.ref(ref), ref = ref)
            (hash / t"greeting").content[Text].value
        .assert(_ == t"hello")

        test(m"content aborts NoteNotFound when no note exists"):
          withWorktree: worktree ?=>
            val hash = commitFile(t"a", t"a\n", t"first")
            capture[Git.Error]((hash / t"missing").content[Text]).reason
        .assert(_ == Git.Error.Reason.NoteNotFound)

        test(m"namespace validation rejects an invalid segment"):
          val hash = Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd")
          safely((hash / t"foo" / t"bad..segment").namespace).let(_.encode)
        .assert(_.absent)


    suite(m"command-line backend"):
      given Choice = commandline
      repositorySuites()

    suite(m"native backend"):
      given Choice = native
      repositorySuites()

    // ----- Git.Refs (Serpentine ref paths) ---------------------------------

    suite(m"Git.Refs (Serpentine ref paths)"):

      test(m"a branch ref path encodes as refs/heads/<name>"):
        (Git.Refs / t"heads" / t"main").encode
      .assert(_ == t"refs/heads/main")

      test(m"a notes ref path encodes as refs/notes/<namespace>"):
        Git.Refs.notes(t"ci-attestation").encode
      .assert(_ == t"refs/notes/ci-attestation")

      test(m"Git.Refs.heads(name) matches manual construction"):
        Git.Refs.heads(t"main").encode
      .assert(_ == t"refs/heads/main")

      test(m"Git.Refs.tags(name) encodes as refs/tags/<name>"):
        Git.Refs.tags(t"v1").encode
      .assert(_ == t"refs/tags/v1")

      test(m"the default notes ref is refs/notes/commits"):
        Git.Refs.defaultNotes.encode
      .assert(_ == t"refs/notes/commits")

      test(m"a fully-qualified name parses back to a ref path"):
        Git.Refs.parse(t"refs/remotes/origin/main").encode
      .assert(_ == t"refs/remotes/origin/main")

      test(m"a name outside refs/ does not parse"):
        safely(Git.Refs.parse(t"HEAD"))
      .assert(_.absent)

      test(m"Git.Refs.heads rejects a segment containing .."):
        safely(Git.Refs.heads(t"foo..bar")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects a segment ending in .lock"):
        safely(Git.Refs.heads(t"main.lock")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects a segment containing a space"):
        safely(Git.Refs.heads(t"two words")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects a segment containing a colon"):
        safely(Git.Refs.heads(t"a:b")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects an empty segment"):
        safely(Git.Refs.heads(t"")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.notes rejects a segment containing @{"):
        safely(Git.Refs.notes(t"name@{0}")).let(_.encode)
      .assert(_.absent)

      test(m"a Git.Refs path is usable as a revision via implicit conversion"):
        val ref: Git.Revision = Git.Refs.heads(t"main")
        ref.show
      .assert(_ == t"refs/heads/main")

    // ----- the reason table ------------------------------------------------------

    suite(m"stderr reasons"):

      test(m"a moved ref is read with both hashes"):
        Commandline.reason(t"error: cannot lock ref 'refs/heads/main': is at 1111111111111111111111111111111111111111 but expected 2222222222222222222222222222222222222222")
      .assert(_ == Git.Error.Reason.Moved(t"refs/heads/main", Git.Hash.unsafe(t"2222222222222222222222222222222222222222"), Git.Hash.unsafe(t"1111111111111111111111111111111111111111")))

      test(m"an existing ref is RefExists"):
        Commandline.reason(t"fatal: cannot lock ref 'refs/tags/v1': reference already exists")
      .assert(_ == Git.Error.Reason.RefExists(t"refs/tags/v1"))

      test(m"a non-repository is NoRepository"):
        Commandline.reason(t"fatal: not a git repository (or any of the parent directories): .git")
      .assert(_ == Git.Error.Reason.NoRepository(t""))

      test(m"overwritten local changes list their paths"):
        Commandline.reason(t"error: Your local changes to the following files would be overwritten by checkout:\n\ta\n\tb\nPlease commit your changes or stash them before you switch branches.")
      .assert(_ == Git.Error.Reason.Dirty(List(t"a", t"b")))

      test(m"an unrecognised message is Unexpected"):
        Commandline.reason(t"something nobody anticipated")
      .assert(_ == Git.Error.Reason.Unexpected)

      test(m"an existing note names its target"):
        Commandline.reason(t"error: Cannot add notes. Found existing notes for object 5c1d2b7e3a4f60918273645566778899aabbccdd. Use '-f' to overwrite existing notes")
      .assert(_ == Git.Error.Reason.NoteExists(Git.Hash.unsafe(t"5c1d2b7e3a4f60918273645566778899aabbccdd")))

    // ----- Activity --------------------------------------------------------------

    suite(m"Activity"):

      test(m"a fresh activity reports Idle with nothing done"):
        Git.Activity().report().value
      .assert(_ == Git.Activity.Report(Git.Activity.Phase.Idle, false, 0L, Unset, 0L))

      test(m"progress updates are visible in the next report"):
        val activity = Git.Activity()
        activity.begin(Git.Activity.Phase.Receiving)
        activity.progress(3L, 12L)
        activity.report().value
      .assert(_ == Git.Activity.Report(Git.Activity.Phase.Receiving, false, 3L, 12L, 0L))

      test(m"a report's completion is the fraction done"):
        val activity = Git.Activity()
        activity.begin(Git.Activity.Phase.Resolving)
        activity.progress(1L, 4L)
        activity.report().value.complete
      .assert(_ == 0.25)

      test(m"finishing marks the activity complete"):
        val activity = Git.Activity()
        activity.finish()
        (activity.complete, activity.report().value.phase)
      .assert(_ == ((true, Git.Activity.Phase.Done)))
