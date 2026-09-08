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

// `/` is now Path's own `def /` (Method on the Path class) so no
// Octogenarian-side extension import is needed.  `read`/`namespace`/
// `target` are still package-level extensions on `NoteRef`; bring them in
// explicitly so they win on specificity against Turbulence's generic `read`
// brought in via `soundness.*`.
import octogenarian.{content, namespace, target}

import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import workingDirectories.javaBaseWorkingDirectory
import logging.silentLogging
import internetAccess.online

import strategies.throwUnsafely
import charEncoders.utf8Encoder
import errorDiagnostics.stackTracesDiagnostics

import filesystemOptions.dereferenceSymlinks
import filesystemOptions.overwritePreexisting
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively

import gitCommands.searchpathGitCommand

import filesystemBackends.javaBaseFilesystem
import denominative.dysasymptotics.linearSize

object Tests extends Suite(m"Octogenarian Tests"):
  def run(): Unit =

    // ----- Shared helpers -------------------------------------------------

    def freshDir(): Path on Linux =
      val name: Text = Uuid().show
      val dir = temporaryDirectory[Path on Linux] / name
      dir.create[Directory]()
      dir

    // Initialize a fresh worktree with isolated config: a stable identity, a
    // forced `main` initial branch, and signing disabled so the tests don't
    // depend on the developer's or the CI runner's global git settings (in
    // particular `init.defaultBranch`, which on some runners is still
    // `master`).
    def freshWorktree(): Worktree =
      val dir = freshDir()
      val worktree = Git.init(dir, initialBranch = Git.Branch("main"))
      sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
      sh"git -C $dir config user.name Octogenarian".exec[Exit]()
      sh"git -C $dir config commit.gpgsign false".exec[Exit]()
      sh"git -C $dir config tag.gpgsign false".exec[Exit]()
      worktree

    def writeFile(path: Path on Linux, content: Text): Unit =
      if !path.existent() then path.create[File]()
      path.open[File](Write): handle ?=>
        handle.write(Chain(content.in[Data]))

    def commitFile(worktree: Worktree, name: Text, content: Text, message: Text): Git.Hash =
      writeFile(worktree.path / name, content)
      worktree.add(worktree.path / name)
      worktree.commit(message)
      worktree.repo.revParse(Refspec.head())

    def patchFrom(lines: Text*): List[FileDiff] =
      Patch.parse(lines.iterator)

    // ----- Refspec validation (unit tests, no git) ------------------------

    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings.
    suite(m"Native-rendering coverage"):

      test(m"octogenarian's ref types inspect natively"):
        Inspectable.fallbacks
         ( Git.Hash.unsafe("5c1d2b7e3a4f60918273645566778899aabbccdd").inspect,
           Git.Branch.unsafe("main").inspect,
           Git.Tag.unsafe("v1.0").inspect,
           Refspec.head(2).inspect )
      .assert(_ == Nil)

      test(m"a hash inspects with all forty digits"):
        Git.Hash.unsafe("5c1d2b7e3a4f60918273645566778899aabbccdd").inspect
      .assert(_ == "Hash(5c1d2b7e3a4f60918273645566778899aabbccdd)")

      test(m"a branch, a tag and a raw refspec each name their kind"):
        List
         ( Git.Branch.unsafe(t"main").inspect,
           Git.Tag.unsafe(t"v1.0").inspect,
           Refspec.head(2).inspect )
      .assert(_ == List(t"Branch(main)", t"Tag(v1.0)", t"Refspec(HEAD~2)"))

    suite(m"Refspec validation"):

      test(m"a valid branch name parses unchanged"):
        Refspec.parse("feature/foo")
      .assert(_ == "feature/foo")

      test(m"a name with internal hyphens parses"):
        Refspec.parse("long-running-feature")
      .assert(_ == "long-running-feature")

      test(m"a multi-segment ref parses"):
        Refspec.parse("refs/heads/main")
      .assert(_ == "refs/heads/main")

      test(m"reject a ref with a leading dot"):
        safely(Refspec.parse(".secret"))
      .assert(_.absent)

      test(m"reject a ref with a trailing dot"):
        safely(Refspec.parse("trailing."))
      .assert(_.absent)

      test(m"reject a ref ending in .lock"):
        safely(Refspec.parse("feature.lock"))
      .assert(_.absent)

      test(m"reject a ref containing @{"):
        safely(Refspec.parse("name@{0}"))
      .assert(_.absent)

      test(m"reject a ref containing .."):
        safely(Refspec.parse("foo..bar"))
      .assert(_.absent)

      test(m"reject a ref with an empty segment"):
        safely(Refspec.parse("foo//bar"))
      .assert(_.absent)

      test(m"reject a ref containing a space"):
        safely(Refspec.parse("two words"))
      .assert(_.absent)

      test(m"reject a ref containing colon"):
        safely(Refspec.parse("a:b"))
      .assert(_.absent)

      test(m"reject a ref containing tilde"):
        safely(Refspec.parse("a~1"))
      .assert(_.absent)

      test(m"reject a ref containing caret"):
        safely(Refspec.parse("a^1"))
      .assert(_.absent)

      test(m"reject a ref containing question mark"):
        safely(Refspec.parse("a?b"))
      .assert(_.absent)

      test(m"reject a ref containing star"):
        safely(Refspec.parse("a*b"))
      .assert(_.absent)

      test(m"reject a ref containing left bracket"):
        safely(Refspec.parse("a[b"))
      .assert(_.absent)

      test(m"Git.Hash accepts a valid 40-char lowercase hex string"):
        Git.Hash("0123456789abcdef0123456789abcdef01234567").show
      .assert(_ == "0123456789abcdef0123456789abcdef01234567")

      test(m"Git.Hash rejects a string that is too short"):
        safely(Git.Hash("abc123"))
      .assert(_.absent)

      test(m"Git.Hash rejects a string with uppercase hex"):
        safely(Git.Hash("0123456789ABCDEF0123456789abcdef01234567"))
      .assert(_.absent)

      test(m"Git.Hash rejects a string with non-hex characters"):
        safely(Git.Hash("zzzzz6789abcdef0123456789abcdef01234567"))
      .assert(_.absent)

      test(m"Refspec.head defaults to HEAD~0"):
        Refspec.head().show
      .assert(_ == "HEAD~0")

      test(m"Refspec.head with offset N gives HEAD~N"):
        Refspec.head(3).show
      .assert(_ == "HEAD~3")

    // ----- Patch parser unit tests (no git) -------------------------------

    suite(m"Patch parser"):

      test(m"empty input yields no FileDiffs"):
        patchFrom().size
      .assert(_ == 0)

      test(m"a single Modified file yields one FileDiff"):
        patchFrom
         ( "diff --git a/foo.txt b/foo.txt",
           "index 1234567..89abcde 100644",
           "--- a/foo.txt",
           "+++ b/foo.txt",
           "@@ -1,3 +1,3 @@",
           " one",
           "-two",
           "+two-changed",
           " three" ).size
      .assert(_ == 1)

      test(m"a Modified file has ChangeKind.Modified"):
        patchFrom
         ( "diff --git a/foo.txt b/foo.txt",
           "--- a/foo.txt",
           "+++ b/foo.txt",
           "@@ -1 +1 @@",
           "-old",
           "+new" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Modified)

      test(m"a new file has ChangeKind.Added"):
        patchFrom
         ( "diff --git a/new.txt b/new.txt",
           "new file mode 100644",
           "index 0000000..89abcde",
           "--- /dev/null",
           "+++ b/new.txt",
           "@@ -0,0 +1 @@",
           "+content" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Added)

      test(m"a deleted file has ChangeKind.Deleted"):
        patchFrom
         ( "diff --git a/old.txt b/old.txt",
           "deleted file mode 100644",
           "index 1234567..0000000",
           "--- a/old.txt",
           "+++ /dev/null",
           "@@ -1 +0,0 @@",
           "-content" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Deleted)

      test(m"a renamed file has ChangeKind.Renamed"):
        patchFrom
         ( "diff --git a/old.txt b/new.txt",
           "similarity index 100%",
           "rename from old.txt",
           "rename to new.txt" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Renamed)

      test(m"a renamed file records both old and new paths"):
        val file = patchFrom
         ( "diff --git a/old.txt b/new.txt",
           "similarity index 100%",
           "rename from old.txt",
           "rename to new.txt" ).stdlib.head
        (file.oldPath, file.newPath)
      .assert(_ == ("old.txt", "new.txt"))

      test(m"a copied file has ChangeKind.Copied"):
        patchFrom
         ( "diff --git a/source b/dest",
           "similarity index 80%",
           "copy from source",
           "copy to dest" ).stdlib.head.changeKind
      .assert(_ == ChangeKind.Copied)

      test(m"multiple files yield multiple FileDiffs"):
        patchFrom
         ( "diff --git a/a b/a",
           "--- a/a",
           "+++ b/a",
           "@@ -1 +1 @@",
           "-1",
           "+1a",
           "diff --git a/b b/b",
           "--- a/b",
           "+++ b/b",
           "@@ -1 +1 @@",
           "-2",
           "+2b" ).size
      .assert(_ == 2)

      test(m"multiple hunks in one file are kept separate"):
        patchFrom
         ( "diff --git a/foo b/foo",
           "--- a/foo",
           "+++ b/foo",
           "@@ -1 +1 @@",
           "-a",
           "+a1",
           "@@ -10 +10 @@",
           "-b",
           "+b1" ).stdlib.head.hunks.size
      .assert(_ == 2)

      test(m"hunk records line range from @@ header"):
        val hunk = patchFrom
         ( "diff --git a/foo b/foo",
           "--- a/foo",
           "+++ b/foo",
           "@@ -5,3 +7,4 @@",
           " ctx5",
           "-old6",
           "+new7",
           "+new8",
           " ctx9" ).stdlib.head.hunks.stdlib.head
        (hunk.oldStart, hunk.oldLines, hunk.newStart, hunk.newLines)
      .assert(_ == (5, 3, 7, 4))

      test(m"a single-line hunk omits the count field"):
        val hunk = patchFrom
         ( "diff --git a/foo b/foo",
           "--- a/foo",
           "+++ b/foo",
           "@@ -1 +1 @@",
           "-x",
           "+y" ).stdlib.head.hunks.stdlib.head
        (hunk.oldStart, hunk.oldLines, hunk.newStart, hunk.newLines)
      .assert(_ == (1, 1, 1, 1))

      test(m"hunk edits get accurate left/right line numbers"):
        val edits = patchFrom
         ( "diff --git a/foo b/foo",
           "--- a/foo",
           "+++ b/foo",
           "@@ -5,3 +7,4 @@",
           " ctx5",
           "-old6",
           "+new7",
           "+new8",
           " ctx9" ).stdlib.head.hunks.stdlib.head.edits

        edits == List
         ( Par(5, 7, t"ctx5"),
           Del(6, t"old6"),
           Ins(8, t"new7"),
           Ins(9, t"new8"),
           Par(7, 10, t"ctx9") )
      .assert(_ == true)

      test(m"section header after @@ is preserved"):
        patchFrom
         ( "diff --git a/foo b/foo",
           "--- a/foo",
           "+++ b/foo",
           "@@ -1 +1 @@ def someFunction()",
           "-old",
           "+new" ).stdlib.head.hunks.stdlib.head.section
      .assert(_ == "def someFunction()")

      test(m"`\\ No newline at end of file` is ignored"):
        patchFrom
         ( "diff --git a/foo b/foo",
           "--- a/foo",
           "+++ b/foo",
           "@@ -1 +1 @@",
           "-old",
           "\\ No newline at end of file",
           "+new",
           "\\ No newline at end of file" ).stdlib.head.hunks.stdlib.head.edits.size
      .assert(_ == 2)

      test(m"a binary diff yields a FileDiff with no hunks"):
        val file = patchFrom
         ( "diff --git a/blob b/blob",
           "index 1234567..89abcde",
           "Binary files a/blob and b/blob differ" ).stdlib.head
        file.hunks
      .assert(_.nil)

      test(m"asDiff flattens hunks into a Dissonance Diff"):
        val file = patchFrom
         ( "diff --git a/foo b/foo",
           "--- a/foo",
           "+++ b/foo",
           "@@ -1 +1 @@",
           "-old",
           "+new",
           "@@ -10 +10 @@",
           "-other",
           "+also" ).stdlib.head
        Patch.asDiff(file).edits.length
      .assert(_ == 4)

      test(m"a header with no --- / +++ block still records paths from `diff --git`"):
        val file = patchFrom
         ( "diff --git a/foo b/foo",
           "old mode 100644",
           "new mode 100755" ).stdlib.head
        (file.oldPath, file.newPath)
      .assert(_ == ("foo", "foo"))

    // ----- Repo + Worktree factories --------------------------------------

    suite(m"Repo + Worktree factories"):

      test(m"Git.init returns a Worktree at the requested path"):
        val dir = freshDir()
        Git.init(dir).path == dir
      .assert(_ == true)

      test(m"Git.init creates a .git directory under the worktree"):
        val dir = freshDir()
        Git.init(dir).repo.gitDir.existent()
      .assert(_ == true)

      test(m"Git.init exposes its worktree via .repo"):
        val dir = freshDir()
        val w = Git.init(dir)
        w.repo.gitDir == dir/".git"
      .assert(_ == true)

      test(m"Git.initBare returns a Git.Repo at the requested path"):
        val dir = freshDir()
        Git.initBare(dir).gitDir == dir
      .assert(_ == true)

      test(m"Git.initBare leaves no .git subdirectory"):
        val dir = freshDir()
        Git.initBare(dir)
        !(dir/".git").existent()
      .assert(_ == true)

      test(m"Git.initBare creates a HEAD file at the gitDir root"):
        val dir = freshDir()
        Git.initBare(dir)
        (dir/"HEAD").existent()
      .assert(_ == true)

    // ----- log / revParse -------------------------------------------------

    suite(m"log + revParse"):

      test(m"a fresh repo has an empty log"):
        val worktree = freshWorktree()
        worktree.repo.log()
      .assert(_.nil)

      test(m"log returns commits newest-first"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        commitFile(worktree, "b", "b\n", "second")
        commitFile(worktree, "c", "c\n", "third")
        worktree.repo.log().map(_.message.stdlib.head)
      .assert(_ == List(t"third", t"second", t"first"))

      test(m"each parsed Commit has the correct number of parents"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "root")
        commitFile(worktree, "b", "b\n", "second")
        worktree.repo.log().map(_.parent.size)
      .assert(_ == List(1, 0))

      test(m"revParse(HEAD) matches the committed hash"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.revParse(Refspec.head()) == hash
      .assert(_ == true)

      test(m"revParse(HEAD~1) resolves to the previous commit"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, "a", "a\n", "first")
        val second = commitFile(worktree, "b", "b\n", "second")
        first != second && worktree.repo.revParse(Refspec.head(1)) == first
      .assert(_ == true)

      test(m"revParse on a tag resolves to the tagged commit"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.tag(Git.Tag("v1"))
        worktree.repo.revParse(Git.Tag("v1")) == hash
      .assert(_ == true)

      test(m"revParse on a branch resolves to the branch's tip"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.revParse(Git.Branch("main")) == hash
      .assert(_ == true)

    // ----- status ---------------------------------------------------------

    suite(m"status"):

      test(m"a clean repo has empty status"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.status()
      .assert(_.nil)

      test(m"an untracked file shows as Untracked"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        writeFile(worktree.path / t"new.txt", "new\n")
        worktree.status().stdlib.exists: e =>
          e.path1 == "new.txt" && e.status1 == Git.Status.Untracked
      .assert(_ == true)

      test(m"an added file shows as Added"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        writeFile(worktree.path / t"new.txt", "new\n")
        worktree.add(worktree.path / t"new.txt")
        worktree.status().stdlib.exists: e =>
          e.path1 == "new.txt" && e.status1 == Git.Status.Added
      .assert(_ == true)

      test(m"a modified-but-not-staged file shows as Updated in slot 2"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        writeFile(worktree.path / t"a", "a-changed\n")
        worktree.status().stdlib.exists: e =>
          e.path1 == "a" && e.status2 == Git.Status.Updated
      .assert(_ == true)

      test(m"a deleted-but-not-staged file shows as Deleted"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        sh"rm ${worktree.path}/a".exec[Exit]()
        worktree.status().stdlib.exists: e =>
          e.path1 == "a" && e.status2 == Git.Status.Deleted
      .assert(_ == true)

      test(m"status() with ignored = true shows ignored files"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        writeFile(worktree.path / t".gitignore", "ignored.txt\n")
        worktree.add(worktree.path / t".gitignore")
        worktree.commit("add gitignore")
        writeFile(worktree.path / t"ignored.txt", "x\n")

        worktree.status(ignored = true).stdlib.exists: e =>
          e.path1 == "ignored.txt" && e.status1 == Git.Status.Ignored
      .assert(_ == true)

    // ----- add / commit / unstage / mv ------------------------------------

    suite(m"add / commit / unstage / mv"):

      test(m"add + commit puts a file under version control"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"hello.txt", "hello\n")
        worktree.add(worktree.path / t"hello.txt")
        worktree.commit("initial")
        worktree.repo.log().size
      .assert(_ == 1)

      test(m"unstage leaves the file on disk and removes it from the index"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        writeFile(worktree.path / t"new.txt", "new\n")
        worktree.add(worktree.path / t"new.txt")
        worktree.unstage(worktree.path / t"new.txt")
        worktree.status().stdlib.exists: e =>
          e.path1 == "new.txt" && e.status1 == Git.Status.Untracked
      .assert(_ == true)

      test(m"mv renames a tracked file"):
        val worktree = freshWorktree()
        commitFile(worktree, "old.txt", "hello\n", "first")
        worktree.mv(worktree.path / t"old.txt", worktree.path / t"new.txt")
        !(worktree.path / t"old.txt").existent() && (worktree.path / t"new.txt").existent()
      .assert(_ == true)

      test(m"mv stages the rename for the next commit"):
        val worktree = freshWorktree()
        commitFile(worktree, "old.txt", "hello\n", "first")
        worktree.mv(worktree.path / t"old.txt", worktree.path / t"new.txt")
        worktree.commit("rename")
        worktree.repo.log().size
      .assert(_ == 2)

    // ----- reset modes ----------------------------------------------------

    suite(m"reset modes"):

      test(m"reset --soft moves HEAD but keeps the working tree"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        commitFile(worktree, "b", "b\n", "second")
        worktree.reset(ResetMode.Soft, Refspec.head(1))
        worktree.repo.log().size == 1 && (worktree.path / t"b").existent()
      .assert(_ == true)

      test(m"reset --soft leaves changes staged"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        commitFile(worktree, "b", "b\n", "second")
        worktree.reset(ResetMode.Soft, Refspec.head(1))
        // After --soft, file b is staged as Added.
        worktree.status().stdlib.exists: e =>
          e.path1 == "b" && e.status1 == Git.Status.Added
      .assert(_ == true)

      test(m"reset --mixed moves HEAD and unstages, but keeps the working tree"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        commitFile(worktree, "b", "b\n", "second")
        worktree.reset(ResetMode.Mixed, Refspec.head(1))
        // b.txt still on disk, but no longer staged (Untracked).
        worktree.status().stdlib.exists: e =>
          e.path1 == "b" && e.status1 == Git.Status.Untracked
      .assert(_ == true)

      test(m"reset --hard discards both index and working tree"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        commitFile(worktree, "b", "b\n", "second")
        worktree.reset(ResetMode.Hard, Refspec.head(1))
        // b.txt is gone entirely.
        !(worktree.path / t"b").existent() && worktree.status().nil
      .assert(_ == true)

    // ----- branches and tags ----------------------------------------------

    suite(m"branches and tags"):

      test(m"a fresh repo with one commit has just main"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.branches().map(_.show)
      .assert(_ == List(t"main"))

      test(m"makeBranch adds a new branch"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.makeBranch(Git.Branch("feature"))
        worktree.branches().map(_.show).to[Set]
      .assert(_ == Set("main", "feature"))

      test(m"branch() returns the currently checked-out branch"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.makeBranch(Git.Branch("feature"))
        worktree.branch().show
      .assert(_ == "feature")

      test(m"switch moves HEAD to the named branch"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.makeBranch(Git.Branch("feature"))
        worktree.switch(Git.Branch("main"))
        worktree.branch().show
      .assert(_ == "main")

      test(m"deleteBranch removes a branch from the listing"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.makeBranch(Git.Branch("feature"))
        worktree.checkout(Git.Branch("main"))
        worktree.repo.deleteBranch(Git.Branch("feature"))
        worktree.branches().map(_.show)
      .assert(_ == List(t"main"))

      test(m"deleteBranch with force removes an unmerged branch"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.makeBranch(Git.Branch("feature"))
        commitFile(worktree, "b", "b\n", "feature work")
        worktree.checkout(Git.Branch("main"))
        // Feature is ahead of main, so unforced delete would refuse.
        worktree.repo.deleteBranch(Git.Branch("feature"), force = true)
        worktree.branches().map(_.show)
      .assert(_ == List(t"main"))

      test(m"renameBranch updates the branch name"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.makeBranch(Git.Branch("oldname"))
        worktree.repo.renameBranch(Git.Branch("oldname"), Git.Branch("newname"))
        val names = worktree.branches().map(_.show).to[Set]
        names.has("newname") && !names.has("oldname")
      .assert(_ == true)

      test(m"tags() returns an empty list for a tagless repo"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.repo.tags()
      .assert(_.nil)

      test(m"tag(name) creates a tag at HEAD"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.repo.tag(Git.Tag("v1"))
        worktree.repo.tags().map(_.show)
      .assert(_ == List(t"v1"))

      test(m"deleteTag removes a tag"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.repo.tag(Git.Tag("v1"))
        worktree.repo.deleteTag(Git.Tag("v1"))
        worktree.repo.tags()
      .assert(_.nil)

      test(m"multiple tags are reported in lexicographic order"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.repo.tag(Git.Tag("v3"))
        worktree.repo.tag(Git.Tag("v1"))
        worktree.repo.tag(Git.Tag("v2"))
        worktree.repo.tags().map(_.show)
      .assert(_ == List(t"v1", t"v2", t"v3"))

    // ----- reflog ---------------------------------------------------------

    suite(m"reflog"):

      test(m"a fresh repo has no reflog entries"):
        val worktree = freshWorktree()
        worktree.repo.reflog()
      .assert(_.nil)

      test(m"reflog returns one entry per commit (newest first)"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        commitFile(worktree, "b", "b\n", "second")
        worktree.repo.reflog().size
      .assert(_ == 2)

      test(m"a reset adds a `reset:` entry to the reflog"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        commitFile(worktree, "b", "b\n", "second")
        worktree.reset(ResetMode.Soft, Refspec.head(1))
        worktree.repo.reflog().stdlib.head.message.starts("reset:")
      .assert(_ == true)

      test(m"reflog entries carry the commit hash"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.reflog().stdlib.head.hash == hash
      .assert(_ == true)

    // ----- diff (integration) ---------------------------------------------

    suite(m"diff (integration)"):

      test(m"diff() reports working-tree changes"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "first\n", "first")
        writeFile(worktree.path / t"a", "first\nsecond\n")

        val files = worktree.diff()
        files.size == 1
          && files.stdlib.head.changeKind == ChangeKind.Modified
          && files.stdlib.head.hunks.bind(_.edits).stdlib.exists:
              case Ins(_, "second") => true
              case _                 => false
      .assert(_ == true)

      test(m"diff(staged = true) reports staged changes"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        writeFile(worktree.path / t"b", "b\n")
        worktree.add(worktree.path / t"b")
        val staged = worktree.diff(staged = true)
        staged.size == 1
          && staged.stdlib.head.changeKind == ChangeKind.Added
          && staged.stdlib.head.newPath == "b"
      .assert(_ == true)

      test(m"diff() ignores already-committed changes"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.diff()
      .assert(_.nil)

      test(m"diff(ref) compares working tree to a ref"):
        val worktree = freshWorktree()
        val first = commitFile(worktree, "a", "v1\n", "v1")
        commitFile(worktree, "a", "v2\n", "v2")
        worktree.diff(first).size
      .assert(_ == 1)

      test(m"Git.Repo.diff(refA, refB) shows changes between two commits"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, "a", "v1\n", "v1")
        val second = commitFile(worktree, "a", "v2\n", "v2")
        worktree.repo.diff(first, second).size
      .assert(_ == 1)

      test(m"diff records a Deleted file with old path and no new path"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        sh"rm ${worktree.path}/a".exec[Exit]()
        worktree.add(worktree.path / t"a")
        val file = worktree.diff(staged = true).stdlib.head
        file.changeKind == ChangeKind.Deleted
          && file.oldPath == "a"
          && file.newPath == Unset
      .assert(_ == true)

    // ----- merge / cherry-pick / revert -----------------------------------

    suite(m"merge / cherry-pick / revert"):

      test(m"merge fast-forwards onto a branch ahead of HEAD"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        worktree.makeBranch(Git.Branch("feature"))
        commitFile(worktree, "b", "b\n", "feature")
        worktree.checkout(Git.Branch("main"))
        worktree.merge(Git.Branch("feature"), ff = FastForward.Only)
        worktree.repo.log().size
      .assert(_ == 2)

      test(m"merge with FastForward.Auto fast-forwards a clean lineage"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        worktree.makeBranch(Git.Branch("feature"))
        commitFile(worktree, "b", "b\n", "feature")
        worktree.checkout(Git.Branch("main"))
        worktree.merge(Git.Branch("feature"))
        worktree.repo.log().map(_.parent.size)
      .assert(_ == List(1, 0))

      test(m"merge with FastForward.Never creates a merge commit"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        worktree.makeBranch(Git.Branch("feature"))
        commitFile(worktree, "b", "b\n", "feature")
        worktree.checkout(Git.Branch("main"))
        worktree.merge(Git.Branch("feature"), ff = FastForward.Never, message = "merge")
        worktree.repo.log().stdlib.head.parent.size
      .assert(_ == 2)

      test(m"merge with FastForward.Only refuses a non-fast-forward"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        worktree.makeBranch(Git.Branch("feature"))
        commitFile(worktree, "b", "b\n", "feature")
        worktree.checkout(Git.Branch("main"))
        commitFile(worktree, "c", "c\n", "main work")  // diverge
        capture[Git.Error](worktree.merge(Git.Branch("feature"), ff = FastForward.Only)).reason
      .assert(_ == Git.Error.Reason.MergeFailed)

      test(m"cherryPick replays a commit on the current branch"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        worktree.makeBranch(Git.Branch("feature"))
        val featureHash = commitFile(worktree, "b", "b\n", "feature")
        worktree.checkout(Git.Branch("main"))
        worktree.cherryPick(featureHash)
        (worktree.path / t"b").existent()
      .assert(_ == true)

      test(m"cherryPick advances HEAD by one commit"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        worktree.makeBranch(Git.Branch("feature"))
        val source = commitFile(worktree, "b", "b\n", "feature")
        worktree.checkout(Git.Branch("main"))

        val before = worktree.repo.log().size
        worktree.cherryPick(source)
        val after = worktree.repo.log().size
        after - before
      .assert(_ == 1)

      test(m"revert produces a new commit that undoes the original"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        val toRevert = commitFile(worktree, "b", "b\n", "add b")
        worktree.revert(toRevert)
        worktree.repo.log().size == 3 && !(worktree.path / t"b").existent()
      .assert(_ == true)

      test(m"revert with noCommit leaves the inverse staged"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "base")
        val toRevert = commitFile(worktree, "b", "b\n", "add b")
        worktree.revert(toRevert, noCommit = true)
        // History unchanged (still 2), but b is staged for deletion.
        worktree.repo.log().size == 2
          && worktree.status().stdlib.exists: e =>
              e.path1 == "b" && e.status1 == Git.Status.Deleted
      .assert(_ == true)

    // ----- worktree management --------------------------------------------

    suite(m"worktree management"):

      test(m"a fresh repo lists exactly one worktree"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.repo.worktrees().size
      .assert(_ == 1)

      test(m"addWorktree creates a second worktree sharing the object DB"):
        val primary = freshWorktree()
        commitFile(primary, "a", "a\n", "first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()  // git worktree add wants a fresh path
        val secondary = primary.repo.addWorktree(secondaryPath, Git.Branch("main"), detach = true)
        primary.repo.log().size == 1
          && secondary.repo.log().size == 1
          && primary.repo.worktrees().size == 2
      .assert(_ == true)

      test(m"a secondary worktree shares the object DB with the primary"):
        val primary = freshWorktree()
        val first = commitFile(primary, "a", "a\n", "first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()
        val secondary = primary.repo.addWorktree(secondaryPath, first, detach = true)
        // Same object DB, so the same hash resolves identically from both.
        secondary.repo.revParse(Refspec.head()) == first
      .assert(_ == true)

      test(m"removeWorktree shrinks the listing back to one"):
        val primary = freshWorktree()
        commitFile(primary, "a", "a\n", "first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()
        val secondary = primary.repo.addWorktree(secondaryPath, Git.Branch("main"), detach = true)
        secondary.remove()
        primary.repo.worktrees().size
      .assert(_ == 1)

      test(m"pruneWorktrees succeeds on a clean repo"):
        val primary = freshWorktree()
        commitFile(primary, "a", "a\n", "first")
        primary.repo.pruneWorktrees()
        primary.repo.worktrees().size
      .assert(_ == 1)

      test(m"lock and unlock a secondary worktree"):
        val primary = freshWorktree()
        commitFile(primary, "a", "a\n", "first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()
        val secondary = primary.repo.addWorktree(secondaryPath, Git.Branch("main"), detach = true)
        secondary.lock(reason = "running CI")
        secondary.unlock()
        // After unlock, removeWorktree should succeed without --force.
        secondary.remove()
        primary.repo.worktrees().size
      .assert(_ == 1)

    // ----- remote management ----------------------------------------------

    suite(m"remote management"):

      test(m"a fresh repo has no remotes"):
        val worktree = freshWorktree()
        worktree.repo.remotes()
      .assert(_.nil)

      test(m"addRemote then remotes() round-trip"):
        val worktree = freshWorktree()
        worktree.repo.addRemote("origin", "git@example.com:foo/bar.git")
        worktree.repo.remotes().stdlib.exists: r =>
          r.name == "origin" && r.fetchUrl == "git@example.com:foo/bar.git"
      .assert(_ == true)

      test(m"addRemote populates pushUrl when fetch and push URLs match"):
        val worktree = freshWorktree()
        worktree.repo.addRemote("origin", "git@example.com:foo/bar.git")
        worktree.repo.remotes().stdlib.head.pushUrl == "git@example.com:foo/bar.git"
      .assert(_ == true)

      test(m"removeRemote drops the remote"):
        val worktree = freshWorktree()
        worktree.repo.addRemote("origin", "git@example.com:foo/bar.git")
        worktree.repo.removeRemote("origin")
        worktree.repo.remotes()
      .assert(_.nil)

      test(m"two remotes are listed and addressable separately"):
        val worktree = freshWorktree()
        worktree.repo.addRemote("origin", "git@a.example:foo.git")
        worktree.repo.addRemote("upstream", "git@b.example:bar.git")
        worktree.repo.remotes().map(_.name).to[Set]
      .assert(_ == Set("origin", "upstream"))

    // ----- clone (integration) --------------------------------------------

    suite(m"clone"):

      test(m"clone of a local bare repo produces a working copy"):
        val source = freshWorktree()
        commitFile(source, "a", "a\n", "first")

        // Create a bare mirror with main as its initial branch so its HEAD
        // resolves correctly after the source pushes.
        val bareDir = freshDir()
        Git.initBare(bareDir, initialBranch = Git.Branch("main"))
        source.repo.addRemote("mirror", bareDir.encode)
        sh"git -C ${source.path} push mirror main".exec[Exit]()

        val targetPath = freshDir()
        sh"rm -rf $targetPath".exec[Exit]()
        val cloned = Git.clone(bareDir, targetPath).complete()
        cloned.repo.log().size
      .assert(_ == 1)

    // ----- Git.Refs (Serpentine ref paths) ---------------------------------

    suite(m"Git.Refs (Serpentine ref paths)"):

      test(m"a branch ref path encodes as refs/heads/<name>"):
        (Git.Refs / t"heads" / t"main").encode
      .assert(_ == "refs/heads/main")

      test(m"a notes ref path encodes as refs/notes/<namespace>"):
        Git.Refs.notes("ci-attestation").encode
      .assert(_ == "refs/notes/ci-attestation")

      test(m"Git.Refs.heads(name) matches manual construction"):
        Git.Refs.heads("main").encode
      .assert(_ == "refs/heads/main")

      test(m"Git.Refs.tags(name) encodes as refs/tags/<name>"):
        Git.Refs.tags("v1").encode
      .assert(_ == "refs/tags/v1")

      test(m"the default notes ref is refs/notes/commits"):
        Git.Refs.defaultNotes.encode
      .assert(_ == "refs/notes/commits")

      test(m"Git.Refs.heads rejects a segment containing .."):
        safely(Git.Refs.heads("foo..bar")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects a segment ending in .lock"):
        safely(Git.Refs.heads("main.lock")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects a segment containing a space"):
        safely(Git.Refs.heads("two words")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects a segment containing a colon"):
        safely(Git.Refs.heads("a:b")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.heads rejects an empty segment"):
        safely(Git.Refs.heads("")).let(_.encode)
      .assert(_.absent)

      test(m"Git.Refs.notes rejects a segment containing @{"):
        safely(Git.Refs.notes("name@{0}")).let(_.encode)
      .assert(_.absent)

      test(m"a Git.Refs path is usable as a Refspec via implicit conversion"):
        val ref: Refspec = Git.Refs.heads("main")
        ref.show
      .assert(_ == "refs/heads/main")

    // ----- git notes ------------------------------------------------------

    suite(m"git notes"):

      test(m"add then show a note in the default namespace"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "a note body")
        worktree.repo.notes.show(hash)
      .assert(_ == "a note body")

      test(m"show on a commit with no note returns Unset"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.show(hash)
      .assert(_.absent)

      test(m"add then show in a custom namespace"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        val ref  = Git.Refs.notes("ci-attestation")
        worktree.repo.notes.add(hash, "signed envelope", ref = ref)
        worktree.repo.notes.show(hash, ref = ref)
      .assert(_ == "signed envelope")

      test(m"custom namespace and default namespace are independent"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "default note")
        worktree.repo.notes.add(hash, "custom note", ref = Git.Refs.notes("alt"))
        (worktree.repo.notes.show(hash), worktree.repo.notes.show(hash, Git.Refs.notes("alt")))
      .assert(_ == (("default note", "custom note")))

      test(m"add without force on an existing note aborts NotesFailed"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "first body")
        capture[Git.Error](worktree.repo.notes.add(hash, "second body")).reason
      .assert(_ == Git.Error.Reason.NotesFailed)

      test(m"add with force overwrites an existing note"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "first body")
        worktree.repo.notes.add(hash, "second body", force = true)
        worktree.repo.notes.show(hash)
      .assert(_ == "second body")

      test(m"append concatenates to an existing note"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "line one")
        worktree.repo.notes.append(hash, "line two")
        val body = worktree.repo.notes.show(hash)
        body.let(b => b.contains("line one") && b.contains("line two")).or(false)
      .assert(_ == true)

      test(m"remove deletes a note"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "a body")
        worktree.repo.notes.remove(hash)
        worktree.repo.notes.show(hash)
      .assert(_.absent)

      test(m"list on an empty namespace returns an empty stream"):
        val worktree = freshWorktree()
        commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.list()
      .assert(_.nil)

      test(m"list yields one entry per annotated commit"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, "a", "a\n", "first")
        val second = commitFile(worktree, "b", "b\n", "second")
        worktree.repo.notes.add(first, "note one")
        worktree.repo.notes.add(second, "note two")
        worktree.repo.notes.list().map(_._2).to[Set] == Set(first, second)
      .assert(_ == true)

      test(m"copy duplicates a note onto another commit"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, "a", "a\n", "first")
        val second = commitFile(worktree, "b", "b\n", "second")
        worktree.repo.notes.add(first, "shared body")
        worktree.repo.notes.copy(first, second)
        worktree.repo.notes.show(second)
      .assert(_ == "shared body")

      test(m"a Path on Git.Refs is usable as a Refspec for revParse"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "body", ref = Git.Refs.notes("custom"))
        // refs/notes/custom now exists; revParse via the path resolves to a hash.
        val noteRefHash = worktree.repo.revParse(Git.Refs.notes("custom"))
        noteRefHash.show.length
      .assert(_ == 40)

    // ----- commit-rooted note access -------------------------------------

    suite(m"commit-rooted note access"):

      test(m"commit / namespace builds a NoteRef whose namespace is refs/notes/<namespace>"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        (hash / t"foo").namespace.encode
      .assert(_ == "refs/notes/foo")

      test(m"chaining / extends the namespace path"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        (hash / t"foo" / t"bar").namespace.encode
      .assert(_ == "refs/notes/foo/bar")

      test(m"a NoteRef's target round-trips through the path root"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        (hash / t"foo").target == hash
      .assert(_ == true)

      test(m"(commit / namespace).content[Text] returns the note body"):
        val worktree = freshWorktree()
        given Git.Repo = worktree.repo
        val hash = commitFile(worktree, "a", "a\n", "first")
        worktree.repo.notes.add(hash, "hello", ref = Git.Refs.notes("greeting"))
        (hash / t"greeting").content[Text]
      .assert(_ == "hello")

      test(m"content aborts NoteNotFound when no note exists"):
        val worktree = freshWorktree()
        given Git.Repo = worktree.repo
        val hash = commitFile(worktree, "a", "a\n", "first")
        capture[Git.Error]((hash / t"missing").content[Text]).reason
      .assert(_ == Git.Error.Reason.NoteNotFound)

      test(m"namespace validation rejects an invalid segment"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, "a", "a\n", "first")
        // Construction is unchecked; validation runs when the namespace path
        // is materialised for use against git.
        safely((hash / t"foo" / t"bad..segment").namespace).let(_.encode)
      .assert(_.absent)
