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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import workingDirectories.defaultWorkingDirectory
import logging.silentLogging
import internetAccess.online

import strategies.throwUnsafely
import charEncoders.utf8Encoder
import errorDiagnostics.stackTracesDiagnostics

import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled

import gitCommands.environmentDefaultGitCommand

object Tests extends Suite(m"Octogenarian Tests"):
  def run(): Unit =

    // ----- Shared helpers -------------------------------------------------

    def freshDir(): Path on Linux =
      val dir = temporaryDirectory[Path on Linux] / Uuid().show
      dir.create[Directory]()
      dir

    // Initialize a fresh worktree with isolated config: a stable identity, a
    // forced `main` initial branch, and signing disabled so the tests don't
    // depend on the developer's or the CI runner's global git settings (in
    // particular `init.defaultBranch`, which on some runners is still
    // `master`).
    def freshWorktree(): Worktree =
      val dir = freshDir()
      val worktree = Git.init(dir, initialBranch = GitBranch(t"main"))
      sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
      sh"git -C $dir config user.name Octogenarian".exec[Exit]()
      sh"git -C $dir config commit.gpgsign false".exec[Exit]()
      sh"git -C $dir config tag.gpgsign false".exec[Exit]()
      worktree

    def writeFile(path: Path on Linux, content: Text): Unit =
      if !path.exists() then path.create[File]()
      path.open: handle =>
        Stream(content.data).writeTo(handle)

    def commitFile(worktree: Worktree, name: Text, content: Text, message: Text): GitHash =
      writeFile(worktree.path / name, content)
      worktree.add(worktree.path / name)
      worktree.commit(message)
      worktree.repo.revParse(Refspec.head())

    def patchFrom(lines: Text*): List[FileDiff] =
      Patch.parse(lines.to(LazyList)).to(List)

    // ----- Refspec validation (unit tests, no git) ------------------------

    suite(m"Refspec validation"):

      test(m"a valid branch name parses unchanged"):
        Refspec.parse(t"feature/foo")
      .assert(_ == t"feature/foo")

      test(m"a name with internal hyphens parses"):
        Refspec.parse(t"long-running-feature")
      .assert(_ == t"long-running-feature")

      test(m"a multi-segment ref parses"):
        Refspec.parse(t"refs/heads/main")
      .assert(_ == t"refs/heads/main")

      test(m"reject a ref with a leading dot"):
        safely(Refspec.parse(t".secret"))
      .assert(_.absent)

      test(m"reject a ref with a trailing dot"):
        safely(Refspec.parse(t"trailing."))
      .assert(_.absent)

      test(m"reject a ref ending in .lock"):
        safely(Refspec.parse(t"feature.lock"))
      .assert(_.absent)

      test(m"reject a ref containing @{"):
        safely(Refspec.parse(t"name@{0}"))
      .assert(_.absent)

      test(m"reject a ref containing .."):
        safely(Refspec.parse(t"foo..bar"))
      .assert(_.absent)

      test(m"reject a ref with an empty segment"):
        safely(Refspec.parse(t"foo//bar"))
      .assert(_.absent)

      test(m"reject a ref containing a space"):
        safely(Refspec.parse(t"two words"))
      .assert(_.absent)

      test(m"reject a ref containing colon"):
        safely(Refspec.parse(t"a:b"))
      .assert(_.absent)

      test(m"reject a ref containing tilde"):
        safely(Refspec.parse(t"a~1"))
      .assert(_.absent)

      test(m"reject a ref containing caret"):
        safely(Refspec.parse(t"a^1"))
      .assert(_.absent)

      test(m"reject a ref containing question mark"):
        safely(Refspec.parse(t"a?b"))
      .assert(_.absent)

      test(m"reject a ref containing star"):
        safely(Refspec.parse(t"a*b"))
      .assert(_.absent)

      test(m"reject a ref containing left bracket"):
        safely(Refspec.parse(t"a[b"))
      .assert(_.absent)

      test(m"GitHash accepts a valid 40-char lowercase hex string"):
        GitHash(t"0123456789abcdef0123456789abcdef01234567").show
      .assert(_ == t"0123456789abcdef0123456789abcdef01234567")

      test(m"GitHash rejects a string that is too short"):
        safely(GitHash(t"abc123"))
      .assert(_.absent)

      test(m"GitHash rejects a string with uppercase hex"):
        safely(GitHash(t"0123456789ABCDEF0123456789abcdef01234567"))
      .assert(_.absent)

      test(m"GitHash rejects a string with non-hex characters"):
        safely(GitHash(t"zzzzz6789abcdef0123456789abcdef01234567"))
      .assert(_.absent)

      test(m"Refspec.head defaults to HEAD~0"):
        Refspec.head().show
      .assert(_ == t"HEAD~0")

      test(m"Refspec.head with offset N gives HEAD~N"):
        Refspec.head(3).show
      .assert(_ == t"HEAD~3")

    // ----- Patch parser unit tests (no git) -------------------------------

    suite(m"Patch parser"):

      test(m"empty input yields no FileDiffs"):
        patchFrom().length
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
           t" three" ).length
      .assert(_ == 1)

      test(m"a Modified file has ChangeKind.Modified"):
        patchFrom
         ( t"diff --git a/foo.txt b/foo.txt",
           t"--- a/foo.txt",
           t"+++ b/foo.txt",
           t"@@ -1 +1 @@",
           t"-old",
           t"+new" ).head.changeKind
      .assert(_ == ChangeKind.Modified)

      test(m"a new file has ChangeKind.Added"):
        patchFrom
         ( t"diff --git a/new.txt b/new.txt",
           t"new file mode 100644",
           t"index 0000000..89abcde",
           t"--- /dev/null",
           t"+++ b/new.txt",
           t"@@ -0,0 +1 @@",
           t"+content" ).head.changeKind
      .assert(_ == ChangeKind.Added)

      test(m"a deleted file has ChangeKind.Deleted"):
        patchFrom
         ( t"diff --git a/old.txt b/old.txt",
           t"deleted file mode 100644",
           t"index 1234567..0000000",
           t"--- a/old.txt",
           t"+++ /dev/null",
           t"@@ -1 +0,0 @@",
           t"-content" ).head.changeKind
      .assert(_ == ChangeKind.Deleted)

      test(m"a renamed file has ChangeKind.Renamed"):
        patchFrom
         ( t"diff --git a/old.txt b/new.txt",
           t"similarity index 100%",
           t"rename from old.txt",
           t"rename to new.txt" ).head.changeKind
      .assert(_ == ChangeKind.Renamed)

      test(m"a renamed file records both old and new paths"):
        val file = patchFrom
         ( t"diff --git a/old.txt b/new.txt",
           t"similarity index 100%",
           t"rename from old.txt",
           t"rename to new.txt" ).head
        (file.oldPath, file.newPath)
      .assert(_ == (t"old.txt", t"new.txt"))

      test(m"a copied file has ChangeKind.Copied"):
        patchFrom
         ( t"diff --git a/source b/dest",
           t"similarity index 80%",
           t"copy from source",
           t"copy to dest" ).head.changeKind
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
           t"+2b" ).length
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
           t"+b1" ).head.hunks.length
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
           t" ctx9" ).head.hunks.head
        (hunk.oldStart, hunk.oldLines, hunk.newStart, hunk.newLines)
      .assert(_ == (5, 3, 7, 4))

      test(m"a single-line hunk omits the count field"):
        val hunk = patchFrom
         ( t"diff --git a/foo b/foo",
           t"--- a/foo",
           t"+++ b/foo",
           t"@@ -1 +1 @@",
           t"-x",
           t"+y" ).head.hunks.head
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
           t" ctx9" ).head.hunks.head.edits

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
           t"+new" ).head.hunks.head.section
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
           t"\\ No newline at end of file" ).head.hunks.head.edits.length
      .assert(_ == 2)

      test(m"a binary diff yields a FileDiff with no hunks"):
        val file = patchFrom
         ( t"diff --git a/blob b/blob",
           t"index 1234567..89abcde",
           t"Binary files a/blob and b/blob differ" ).head
        file.hunks
      .assert(_.isEmpty)

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
           t"+also" ).head
        Patch.asDiff(file).edits.length
      .assert(_ == 4)

      test(m"a header with no --- / +++ block still records paths from `diff --git`"):
        val file = patchFrom
         ( t"diff --git a/foo b/foo",
           t"old mode 100644",
           t"new mode 100755" ).head
        (file.oldPath, file.newPath)
      .assert(_ == (t"foo", t"foo"))

    // ----- Repo + Worktree factories --------------------------------------

    suite(m"Repo + Worktree factories"):

      test(m"Git.init returns a Worktree at the requested path"):
        val dir = freshDir()
        Git.init(dir).path == dir
      .assert(_ == true)

      test(m"Git.init creates a .git directory under the worktree"):
        val dir = freshDir()
        Git.init(dir).repo.gitDir.exists()
      .assert(_ == true)

      test(m"Git.init exposes its worktree via .repo"):
        val dir = freshDir()
        val w = Git.init(dir)
        w.repo.gitDir == dir/".git"
      .assert(_ == true)

      test(m"Git.initBare returns a GitRepo at the requested path"):
        val dir = freshDir()
        Git.initBare(dir).gitDir == dir
      .assert(_ == true)

      test(m"Git.initBare leaves no .git subdirectory"):
        val dir = freshDir()
        Git.initBare(dir)
        !(dir/".git").exists()
      .assert(_ == true)

      test(m"Git.initBare creates a HEAD file at the gitDir root"):
        val dir = freshDir()
        Git.initBare(dir)
        (dir/"HEAD").exists()
      .assert(_ == true)

    // ----- log / revParse -------------------------------------------------

    suite(m"log + revParse"):

      test(m"a fresh repo has an empty log"):
        val worktree = freshWorktree()
        worktree.repo.log()
      .assert(_.isEmpty)

      test(m"log returns commits newest-first"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        commitFile(worktree, t"b", t"b\n", t"second")
        commitFile(worktree, t"c", t"c\n", t"third")
        worktree.repo.log().to(List).map(_.message.head)
      .assert(_ == List(t"third", t"second", t"first"))

      test(m"each parsed Commit has the correct number of parents"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"root")
        commitFile(worktree, t"b", t"b\n", t"second")
        worktree.repo.log().to(List).map(_.parent.length)
      .assert(_ == List(1, 0))

      test(m"revParse(HEAD) matches the committed hash"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.revParse(Refspec.head()) == hash
      .assert(_ == true)

      test(m"revParse(HEAD~1) resolves to the previous commit"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, t"a", t"a\n", t"first")
        val second = commitFile(worktree, t"b", t"b\n", t"second")
        first != second && worktree.repo.revParse(Refspec.head(1)) == first
      .assert(_ == true)

      test(m"revParse on a tag resolves to the tagged commit"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.tag(GitTag(t"v1"))
        worktree.repo.revParse(GitTag(t"v1")) == hash
      .assert(_ == true)

      test(m"revParse on a branch resolves to the branch's tip"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.revParse(GitBranch(t"main")) == hash
      .assert(_ == true)

    // ----- status ---------------------------------------------------------

    suite(m"status"):

      test(m"a clean repo has empty status"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.status()
      .assert(_.isEmpty)

      test(m"an untracked file shows as Untracked"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        writeFile(worktree.path / t"new.txt", t"new\n")
        worktree.status().exists: e =>
          e.path1 == t"new.txt" && e.status1 == GitStatus.Untracked
      .assert(_ == true)

      test(m"an added file shows as Added"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        writeFile(worktree.path / t"new.txt", t"new\n")
        worktree.add(worktree.path / t"new.txt")
        worktree.status().exists: e =>
          e.path1 == t"new.txt" && e.status1 == GitStatus.Added
      .assert(_ == true)

      test(m"a modified-but-not-staged file shows as Updated in slot 2"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        writeFile(worktree.path / t"a", t"a-changed\n")
        worktree.status().exists: e =>
          e.path1 == t"a" && e.status2 == GitStatus.Updated
      .assert(_ == true)

      test(m"a deleted-but-not-staged file shows as Deleted"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        sh"rm ${worktree.path}/a".exec[Exit]()
        worktree.status().exists: e =>
          e.path1 == t"a" && e.status2 == GitStatus.Deleted
      .assert(_ == true)

      test(m"status() with ignored = true shows ignored files"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        writeFile(worktree.path / t".gitignore", t"ignored.txt\n")
        worktree.add(worktree.path / t".gitignore")
        worktree.commit(t"add gitignore")
        writeFile(worktree.path / t"ignored.txt", t"x\n")

        worktree.status(ignored = true).exists: e =>
          e.path1 == t"ignored.txt" && e.status1 == GitStatus.Ignored
      .assert(_ == true)

    // ----- add / commit / unstage / mv ------------------------------------

    suite(m"add / commit / unstage / mv"):

      test(m"add + commit puts a file under version control"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"hello.txt", t"hello\n")
        worktree.add(worktree.path / t"hello.txt")
        worktree.commit(t"initial")
        worktree.repo.log().to(List).length
      .assert(_ == 1)

      test(m"unstage leaves the file on disk and removes it from the index"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        writeFile(worktree.path / t"new.txt", t"new\n")
        worktree.add(worktree.path / t"new.txt")
        worktree.unstage(worktree.path / t"new.txt")
        worktree.status().exists: e =>
          e.path1 == t"new.txt" && e.status1 == GitStatus.Untracked
      .assert(_ == true)

      test(m"mv renames a tracked file"):
        val worktree = freshWorktree()
        commitFile(worktree, t"old.txt", t"hello\n", t"first")
        worktree.mv(worktree.path / t"old.txt", worktree.path / t"new.txt")
        !(worktree.path / t"old.txt").exists() && (worktree.path / t"new.txt").exists()
      .assert(_ == true)

      test(m"mv stages the rename for the next commit"):
        val worktree = freshWorktree()
        commitFile(worktree, t"old.txt", t"hello\n", t"first")
        worktree.mv(worktree.path / t"old.txt", worktree.path / t"new.txt")
        worktree.commit(t"rename")
        worktree.repo.log().to(List).length
      .assert(_ == 2)

    // ----- reset modes ----------------------------------------------------

    suite(m"reset modes"):

      test(m"reset --soft moves HEAD but keeps the working tree"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        commitFile(worktree, t"b", t"b\n", t"second")
        worktree.reset(ResetMode.Soft, Refspec.head(1))
        worktree.repo.log().to(List).length == 1 && (worktree.path / t"b").exists()
      .assert(_ == true)

      test(m"reset --soft leaves changes staged"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        commitFile(worktree, t"b", t"b\n", t"second")
        worktree.reset(ResetMode.Soft, Refspec.head(1))
        // After --soft, file b is staged as Added.
        worktree.status().exists: e =>
          e.path1 == t"b" && e.status1 == GitStatus.Added
      .assert(_ == true)

      test(m"reset --mixed moves HEAD and unstages, but keeps the working tree"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        commitFile(worktree, t"b", t"b\n", t"second")
        worktree.reset(ResetMode.Mixed, Refspec.head(1))
        // b.txt still on disk, but no longer staged (Untracked).
        worktree.status().exists: e =>
          e.path1 == t"b" && e.status1 == GitStatus.Untracked
      .assert(_ == true)

      test(m"reset --hard discards both index and working tree"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        commitFile(worktree, t"b", t"b\n", t"second")
        worktree.reset(ResetMode.Hard, Refspec.head(1))
        // b.txt is gone entirely.
        !(worktree.path / t"b").exists() && worktree.status().isEmpty
      .assert(_ == true)

    // ----- branches and tags ----------------------------------------------

    suite(m"branches and tags"):

      test(m"a fresh repo with one commit has just main"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.branches().map(_.show)
      .assert(_ == List(t"main"))

      test(m"makeBranch adds a new branch"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.makeBranch(GitBranch(t"feature"))
        worktree.branches().map(_.show).to(Set)
      .assert(_ == Set(t"main", t"feature"))

      test(m"branch() returns the currently checked-out branch"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.makeBranch(GitBranch(t"feature"))
        worktree.branch().show
      .assert(_ == t"feature")

      test(m"switch moves HEAD to the named branch"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.makeBranch(GitBranch(t"feature"))
        worktree.switch(GitBranch(t"main"))
        worktree.branch().show
      .assert(_ == t"main")

      test(m"deleteBranch removes a branch from the listing"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.makeBranch(GitBranch(t"feature"))
        worktree.checkout(GitBranch(t"main"))
        worktree.repo.deleteBranch(GitBranch(t"feature"))
        worktree.branches().map(_.show)
      .assert(_ == List(t"main"))

      test(m"deleteBranch with force removes an unmerged branch"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.makeBranch(GitBranch(t"feature"))
        commitFile(worktree, t"b", t"b\n", t"feature work")
        worktree.checkout(GitBranch(t"main"))
        // Feature is ahead of main, so unforced delete would refuse.
        worktree.repo.deleteBranch(GitBranch(t"feature"), force = true)
        worktree.branches().map(_.show)
      .assert(_ == List(t"main"))

      test(m"renameBranch updates the branch name"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.makeBranch(GitBranch(t"oldname"))
        worktree.repo.renameBranch(GitBranch(t"oldname"), GitBranch(t"newname"))
        val names = worktree.branches().map(_.show).to(Set)
        names.has(t"newname") && !names.has(t"oldname")
      .assert(_ == true)

      test(m"tags() returns an empty list for a tagless repo"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.tags()
      .assert(_.isEmpty)

      test(m"tag(name) creates a tag at HEAD"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.tag(GitTag(t"v1"))
        worktree.repo.tags().map(_.show)
      .assert(_ == List(t"v1"))

      test(m"deleteTag removes a tag"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.tag(GitTag(t"v1"))
        worktree.repo.deleteTag(GitTag(t"v1"))
        worktree.repo.tags()
      .assert(_.isEmpty)

      test(m"multiple tags are reported in lexicographic order"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.tag(GitTag(t"v3"))
        worktree.repo.tag(GitTag(t"v1"))
        worktree.repo.tag(GitTag(t"v2"))
        worktree.repo.tags().map(_.show)
      .assert(_ == List(t"v1", t"v2", t"v3"))

    // ----- reflog ---------------------------------------------------------

    suite(m"reflog"):

      test(m"a fresh repo has no reflog entries"):
        val worktree = freshWorktree()
        worktree.repo.reflog().to(List)
      .assert(_.isEmpty)

      test(m"reflog returns one entry per commit (newest first)"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        commitFile(worktree, t"b", t"b\n", t"second")
        worktree.repo.reflog().to(List).length
      .assert(_ == 2)

      test(m"a reset adds a `reset:` entry to the reflog"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        commitFile(worktree, t"b", t"b\n", t"second")
        worktree.reset(ResetMode.Soft, Refspec.head(1))
        worktree.repo.reflog().to(List).head.message.starts(t"reset:")
      .assert(_ == true)

      test(m"reflog entries carry the commit hash"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.reflog().to(List).head.hash == hash
      .assert(_ == true)

    // ----- diff (integration) ---------------------------------------------

    suite(m"diff (integration)"):

      test(m"diff() reports working-tree changes"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"first\n", t"first")
        writeFile(worktree.path / t"a", t"first\nsecond\n")

        val files = worktree.diff().to(List)
        files.length == 1
          && files.head.changeKind == ChangeKind.Modified
          && files.head.hunks.flatMap(_.edits).exists:
              case Ins(_, t"second") => true
              case _                 => false
      .assert(_ == true)

      test(m"diff(staged = true) reports staged changes"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        writeFile(worktree.path / t"b", t"b\n")
        worktree.add(worktree.path / t"b")
        val staged = worktree.diff(staged = true).to(List)
        staged.length == 1
          && staged.head.changeKind == ChangeKind.Added
          && staged.head.newPath == t"b"
      .assert(_ == true)

      test(m"diff() ignores already-committed changes"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.diff()
      .assert(_.isEmpty)

      test(m"diff(ref) compares working tree to a ref"):
        val worktree = freshWorktree()
        val first = commitFile(worktree, t"a", t"v1\n", t"v1")
        commitFile(worktree, t"a", t"v2\n", t"v2")
        worktree.diff(first).to(List).length
      .assert(_ == 1)

      test(m"GitRepo.diff(refA, refB) shows changes between two commits"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, t"a", t"v1\n", t"v1")
        val second = commitFile(worktree, t"a", t"v2\n", t"v2")
        worktree.repo.diff(first, second).to(List).length
      .assert(_ == 1)

      test(m"diff records a Deleted file with old path and no new path"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        sh"rm ${worktree.path}/a".exec[Exit]()
        worktree.add(worktree.path / t"a")
        val file = worktree.diff(staged = true).to(List).head
        file.changeKind == ChangeKind.Deleted
          && file.oldPath == t"a"
          && file.newPath == Unset
      .assert(_ == true)

    // ----- merge / cherry-pick / revert -----------------------------------

    suite(m"merge / cherry-pick / revert"):

      test(m"merge fast-forwards onto a branch ahead of HEAD"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        worktree.makeBranch(GitBranch(t"feature"))
        commitFile(worktree, t"b", t"b\n", t"feature")
        worktree.checkout(GitBranch(t"main"))
        worktree.merge(GitBranch(t"feature"), ff = FastForward.Only)
        worktree.repo.log().to(List).length
      .assert(_ == 2)

      test(m"merge with FastForward.Auto fast-forwards a clean lineage"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        worktree.makeBranch(GitBranch(t"feature"))
        commitFile(worktree, t"b", t"b\n", t"feature")
        worktree.checkout(GitBranch(t"main"))
        worktree.merge(GitBranch(t"feature"))
        worktree.repo.log().to(List).map(_.parent.length)
      .assert(_ == List(1, 0))

      test(m"merge with FastForward.Never creates a merge commit"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        worktree.makeBranch(GitBranch(t"feature"))
        commitFile(worktree, t"b", t"b\n", t"feature")
        worktree.checkout(GitBranch(t"main"))
        worktree.merge(GitBranch(t"feature"), ff = FastForward.Never, message = t"merge")
        worktree.repo.log().to(List).head.parent.length
      .assert(_ == 2)

      test(m"merge with FastForward.Only refuses a non-fast-forward"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        worktree.makeBranch(GitBranch(t"feature"))
        commitFile(worktree, t"b", t"b\n", t"feature")
        worktree.checkout(GitBranch(t"main"))
        commitFile(worktree, t"c", t"c\n", t"main work")  // diverge
        capture[GitError](worktree.merge(GitBranch(t"feature"), ff = FastForward.Only)).reason
      .assert(_ == GitError.Reason.MergeFailed)

      test(m"cherryPick replays a commit on the current branch"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        worktree.makeBranch(GitBranch(t"feature"))
        val featureHash = commitFile(worktree, t"b", t"b\n", t"feature")
        worktree.checkout(GitBranch(t"main"))
        worktree.cherryPick(featureHash)
        (worktree.path / t"b").exists()
      .assert(_ == true)

      test(m"cherryPick advances HEAD by one commit"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        worktree.makeBranch(GitBranch(t"feature"))
        val source = commitFile(worktree, t"b", t"b\n", t"feature")
        worktree.checkout(GitBranch(t"main"))

        val before = worktree.repo.log().to(List).length
        worktree.cherryPick(source)
        val after = worktree.repo.log().to(List).length
        after - before
      .assert(_ == 1)

      test(m"revert produces a new commit that undoes the original"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        val toRevert = commitFile(worktree, t"b", t"b\n", t"add b")
        worktree.revert(toRevert)
        worktree.repo.log().to(List).length == 3 && !(worktree.path / t"b").exists()
      .assert(_ == true)

      test(m"revert with noCommit leaves the inverse staged"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"base")
        val toRevert = commitFile(worktree, t"b", t"b\n", t"add b")
        worktree.revert(toRevert, noCommit = true)
        // History unchanged (still 2), but b is staged for deletion.
        worktree.repo.log().to(List).length == 2
          && worktree.status().exists: e =>
              e.path1 == t"b" && e.status1 == GitStatus.Deleted
      .assert(_ == true)

    // ----- worktree management --------------------------------------------

    suite(m"worktree management"):

      test(m"a fresh repo lists exactly one worktree"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.worktrees().length
      .assert(_ == 1)

      test(m"addWorktree creates a second worktree sharing the object DB"):
        val primary = freshWorktree()
        commitFile(primary, t"a", t"a\n", t"first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()  // git worktree add wants a fresh path
        val secondary = primary.repo.addWorktree(secondaryPath, GitBranch(t"main"), detach = true)
        primary.repo.log().to(List).length == 1
          && secondary.repo.log().to(List).length == 1
          && primary.repo.worktrees().length == 2
      .assert(_ == true)

      test(m"a secondary worktree shares the object DB with the primary"):
        val primary = freshWorktree()
        val first = commitFile(primary, t"a", t"a\n", t"first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()
        val secondary = primary.repo.addWorktree(secondaryPath, first, detach = true)
        // Same object DB, so the same hash resolves identically from both.
        secondary.repo.revParse(Refspec.head()) == first
      .assert(_ == true)

      test(m"removeWorktree shrinks the listing back to one"):
        val primary = freshWorktree()
        commitFile(primary, t"a", t"a\n", t"first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()
        val secondary = primary.repo.addWorktree(secondaryPath, GitBranch(t"main"), detach = true)
        secondary.remove()
        primary.repo.worktrees().length
      .assert(_ == 1)

      test(m"pruneWorktrees succeeds on a clean repo"):
        val primary = freshWorktree()
        commitFile(primary, t"a", t"a\n", t"first")
        primary.repo.pruneWorktrees()
        primary.repo.worktrees().length
      .assert(_ == 1)

      test(m"lock and unlock a secondary worktree"):
        val primary = freshWorktree()
        commitFile(primary, t"a", t"a\n", t"first")
        val secondaryPath = freshDir()
        sh"rm -rf $secondaryPath".exec[Exit]()
        val secondary = primary.repo.addWorktree(secondaryPath, GitBranch(t"main"), detach = true)
        secondary.lock(reason = t"running CI")
        secondary.unlock()
        // After unlock, removeWorktree should succeed without --force.
        secondary.remove()
        primary.repo.worktrees().length
      .assert(_ == 1)

    // ----- remote management ----------------------------------------------

    suite(m"remote management"):

      test(m"a fresh repo has no remotes"):
        val worktree = freshWorktree()
        worktree.repo.remotes()
      .assert(_.isEmpty)

      test(m"addRemote then remotes() round-trip"):
        val worktree = freshWorktree()
        worktree.repo.addRemote(t"origin", t"git@example.com:foo/bar.git")
        worktree.repo.remotes().exists: r =>
          r.name == t"origin" && r.fetchUrl == t"git@example.com:foo/bar.git"
      .assert(_ == true)

      test(m"addRemote populates pushUrl when fetch and push URLs match"):
        val worktree = freshWorktree()
        worktree.repo.addRemote(t"origin", t"git@example.com:foo/bar.git")
        worktree.repo.remotes().head.pushUrl == t"git@example.com:foo/bar.git"
      .assert(_ == true)

      test(m"removeRemote drops the remote"):
        val worktree = freshWorktree()
        worktree.repo.addRemote(t"origin", t"git@example.com:foo/bar.git")
        worktree.repo.removeRemote(t"origin")
        worktree.repo.remotes()
      .assert(_.isEmpty)

      test(m"two remotes are listed and addressable separately"):
        val worktree = freshWorktree()
        worktree.repo.addRemote(t"origin", t"git@a.example:foo.git")
        worktree.repo.addRemote(t"upstream", t"git@b.example:bar.git")
        worktree.repo.remotes().map(_.name).to(Set)
      .assert(_ == Set(t"origin", t"upstream"))

    // ----- clone (integration) --------------------------------------------

    suite(m"clone"):

      test(m"clone of a local bare repo produces a working copy"):
        val source = freshWorktree()
        commitFile(source, t"a", t"a\n", t"first")

        // Create a bare mirror with main as its initial branch so its HEAD
        // resolves correctly after the source pushes.
        val bareDir = freshDir()
        Git.initBare(bareDir, initialBranch = GitBranch(t"main"))
        source.repo.addRemote(t"mirror", bareDir.encode)
        sh"git -C ${source.path} push mirror main".exec[Exit]()

        val targetPath = freshDir()
        sh"rm -rf $targetPath".exec[Exit]()
        val cloned = Git.clone(bareDir, targetPath).complete()
        cloned.repo.log().to(List).length
      .assert(_ == 1)

    // ----- GitRefs (Serpentine ref paths) ---------------------------------

    suite(m"GitRefs (Serpentine ref paths)"):

      test(m"a branch ref path encodes as refs/heads/<name>"):
        (GitRefs / t"heads" / t"main").encode
      .assert(_ == t"refs/heads/main")

      test(m"a notes ref path encodes as refs/notes/<namespace>"):
        GitRefs.notes(t"ci-attestation").encode
      .assert(_ == t"refs/notes/ci-attestation")

      test(m"GitRefs.heads(name) matches manual construction"):
        GitRefs.heads(t"main").encode
      .assert(_ == t"refs/heads/main")

      test(m"GitRefs.tags(name) encodes as refs/tags/<name>"):
        GitRefs.tags(t"v1").encode
      .assert(_ == t"refs/tags/v1")

      test(m"the default notes ref is refs/notes/commits"):
        GitRefs.defaultNotes.encode
      .assert(_ == t"refs/notes/commits")

      test(m"GitRefs.heads rejects a segment containing .."):
        safely(GitRefs.heads(t"foo..bar")).let(_.encode)
      .assert(_.absent)

      test(m"GitRefs.heads rejects a segment ending in .lock"):
        safely(GitRefs.heads(t"main.lock")).let(_.encode)
      .assert(_.absent)

      test(m"GitRefs.heads rejects a segment containing a space"):
        safely(GitRefs.heads(t"two words")).let(_.encode)
      .assert(_.absent)

      test(m"GitRefs.heads rejects a segment containing a colon"):
        safely(GitRefs.heads(t"a:b")).let(_.encode)
      .assert(_.absent)

      test(m"GitRefs.heads rejects an empty segment"):
        safely(GitRefs.heads(t"")).let(_.encode)
      .assert(_.absent)

      test(m"GitRefs.notes rejects a segment containing @{"):
        safely(GitRefs.notes(t"name@{0}")).let(_.encode)
      .assert(_.absent)

      test(m"a GitRefs path is usable as a Refspec via implicit conversion"):
        val ref: Refspec = GitRefs.heads(t"main")
        ref.show
      .assert(_ == t"refs/heads/main")

    // ----- git notes ------------------------------------------------------

    suite(m"git notes"):

      test(m"add then show a note in the default namespace"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"a note body")
        worktree.repo.notes.show(hash)
      .assert(_ == t"a note body")

      test(m"show on a commit with no note returns Unset"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.show(hash)
      .assert(_.absent)

      test(m"add then show in a custom namespace"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        val ref  = GitRefs.notes(t"ci-attestation")
        worktree.repo.notes.add(hash, t"signed envelope", ref = ref)
        worktree.repo.notes.show(hash, ref = ref)
      .assert(_ == t"signed envelope")

      test(m"custom namespace and default namespace are independent"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"default note")
        worktree.repo.notes.add(hash, t"custom note", ref = GitRefs.notes(t"alt"))
        (worktree.repo.notes.show(hash), worktree.repo.notes.show(hash, GitRefs.notes(t"alt")))
      .assert(_ == ((t"default note", t"custom note")))

      test(m"add without force on an existing note aborts NotesFailed"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"first body")
        capture[GitError](worktree.repo.notes.add(hash, t"second body")).reason
      .assert(_ == GitError.Reason.NotesFailed)

      test(m"add with force overwrites an existing note"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"first body")
        worktree.repo.notes.add(hash, t"second body", force = true)
        worktree.repo.notes.show(hash)
      .assert(_ == t"second body")

      test(m"append concatenates to an existing note"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"line one")
        worktree.repo.notes.append(hash, t"line two")
        val body = worktree.repo.notes.show(hash)
        body.let(b => b.contains(t"line one") && b.contains(t"line two")).or(false)
      .assert(_ == true)

      test(m"remove deletes a note"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"a body")
        worktree.repo.notes.remove(hash)
        worktree.repo.notes.show(hash)
      .assert(_.absent)

      test(m"list on an empty namespace returns an empty stream"):
        val worktree = freshWorktree()
        commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.list().to(List)
      .assert(_.isEmpty)

      test(m"list yields one entry per annotated commit"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, t"a", t"a\n", t"first")
        val second = commitFile(worktree, t"b", t"b\n", t"second")
        worktree.repo.notes.add(first, t"note one")
        worktree.repo.notes.add(second, t"note two")
        worktree.repo.notes.list().to(List).map(_._2).to(Set) == Set(first, second)
      .assert(_ == true)

      test(m"copy duplicates a note onto another commit"):
        val worktree = freshWorktree()
        val first  = commitFile(worktree, t"a", t"a\n", t"first")
        val second = commitFile(worktree, t"b", t"b\n", t"second")
        worktree.repo.notes.add(first, t"shared body")
        worktree.repo.notes.copy(first, second)
        worktree.repo.notes.show(second)
      .assert(_ == t"shared body")

      test(m"a Path on GitRefs is usable as a Refspec for revParse"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"body", ref = GitRefs.notes(t"custom"))
        // refs/notes/custom now exists; revParse via the path resolves to a hash.
        val noteRefHash = worktree.repo.revParse(GitRefs.notes(t"custom"))
        noteRefHash.show.length
      .assert(_ == 40)

    // ----- commit-rooted note access -------------------------------------

    suite(m"commit-rooted note access"):

      test(m"commit / namespace builds a NoteRef whose namespace is refs/notes/<namespace>"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        (hash / t"foo").namespace.encode
      .assert(_ == t"refs/notes/foo")

      test(m"chaining / extends the namespace path"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        (hash / t"foo" / t"bar").namespace.encode
      .assert(_ == t"refs/notes/foo/bar")

      test(m"a NoteRef's target round-trips through the path root"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        (hash / t"foo").target == hash
      .assert(_ == true)

      test(m"(commit / namespace).content[Text] returns the note body"):
        val worktree = freshWorktree()
        given GitRepo = worktree.repo
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        worktree.repo.notes.add(hash, t"hello", ref = GitRefs.notes(t"greeting"))
        (hash / t"greeting").content[Text]
      .assert(_ == t"hello")

      test(m"content aborts NoteNotFound when no note exists"):
        val worktree = freshWorktree()
        given GitRepo = worktree.repo
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        capture[GitError]((hash / t"missing").content[Text]).reason
      .assert(_ == GitError.Reason.NoteNotFound)

      test(m"namespace validation rejects an invalid segment"):
        val worktree = freshWorktree()
        val hash = commitFile(worktree, t"a", t"a\n", t"first")
        // Construction is unchecked; validation runs when the namespace path
        // is materialised for use against git.
        safely((hash / t"foo" / t"bad..segment").namespace).let(_.encode)
      .assert(_.absent)
