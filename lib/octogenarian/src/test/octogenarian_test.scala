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

import systems.java
import temporaryDirectories.system
import workingDirectories.default
import logging.silent

import strategies.throwUnsafely
import charEncoders.utf8

import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled

import gitCommands.environmentDefault

object Tests extends Suite(m"Octogenarian Tests"):
  def run(): Unit =
    suite(m"Repo + worktree split"):

      def freshDir(): Path on Linux =
        val dir = temporaryDirectory[Path on Linux] / Uuid().show
        dir.create[Directory]()
        dir

      // Initialize a fresh worktree with isolated config: a stable identity and
      // signing disabled so the tests don't depend on the developer's global
      // git settings.
      def freshWorktree(): Worktree =
        val dir = freshDir()
        val worktree = Git.init(dir)
        sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
        sh"git -C $dir config user.name Octogenarian".exec[Exit]()
        sh"git -C $dir config commit.gpgsign false".exec[Exit]()
        sh"git -C $dir config tag.gpgsign false".exec[Exit]()
        worktree

      def writeFile(path: Path on Linux, content: Text): Unit =
        if !path.exists() then path.create[File]()
        path.open: handle =>
          Stream(content.data).writeTo(handle)

      test(m"Git.init returns a Worktree at the requested path"):
        val dir = freshDir()
        Git.init(dir).path == dir
      .assert(_ == true)

      test(m"Git.init creates a .git directory"):
        val dir = freshDir()
        Git.init(dir).repo.gitDir.exists()
      .assert(_ == true)

      test(m"Git.initBare returns a GitRepo at the requested path"):
        val dir = freshDir()
        Git.initBare(dir).gitDir == dir
      .assert(_ == true)

      test(m"Init + add + commit + log round-trip yields one commit"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"hello.txt", t"hello\n")
        worktree.add(worktree.path / t"hello.txt")
        worktree.commit(t"initial")
        worktree.repo.log().to(List).length
      .assert(_ == 1)

      test(m"Tags created via the repo API are visible in tags()"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"a\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"a")
        worktree.repo.tag(GitTag(t"v1"))
        worktree.repo.tags().map(_.show)
      .assert(_ == List(t"v1"))

    suite(m"reset / unstage / mv"):

      def freshDir(): Path on Linux =
        val dir = temporaryDirectory[Path on Linux] / Uuid().show
        dir.create[Directory]()
        dir

      def freshWorktree(): Worktree =
        val dir = freshDir()
        val worktree = Git.init(dir)
        sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
        sh"git -C $dir config user.name Octogenarian".exec[Exit]()
        sh"git -C $dir config commit.gpgsign false".exec[Exit]()
        sh"git -C $dir config tag.gpgsign false".exec[Exit]()
        worktree

      def writeFile(path: Path on Linux, content: Text): Unit =
        if !path.exists() then path.create[File]()
        path.open: handle =>
          Stream(content.data).writeTo(handle)

      test(m"reset --soft moves HEAD but keeps the working tree"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"a\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"first")

        writeFile(worktree.path / t"b.txt", t"b\n")
        worktree.add(worktree.path / t"b.txt")
        worktree.commit(t"second")

        worktree.reset(ResetMode.Soft, Refspec.head(1))

        val log = worktree.repo.log().to(List)
        // log shrunk to 1 commit, but b.txt still on disk
        log.length == 1 && (worktree.path / t"b.txt").exists()
      .assert(_ == true)

      test(m"unstage removes a file from the index but leaves it on disk"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"a\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"first")

        writeFile(worktree.path / t"new.txt", t"new\n")
        worktree.add(worktree.path / t"new.txt")
        worktree.unstage(worktree.path / t"new.txt")

        // After unstage the file should still exist, but git status shows it
        // as untracked (`??`) rather than added (`A`).
        worktree.status().exists: entry =>
          entry.path1 == t"new.txt" && entry.status1 == GitStatus.Untracked
      .assert(_ == true)

      test(m"mv renames a tracked file"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"old.txt", t"hello\n")
        worktree.add(worktree.path / t"old.txt")
        worktree.commit(t"first")

        worktree.mv(worktree.path / t"old.txt", worktree.path / t"new.txt")

        !(worktree.path / t"old.txt").exists() && (worktree.path / t"new.txt").exists()
      .assert(_ == true)

    suite(m"reflog"):

      def freshDir(): Path on Linux =
        val dir = temporaryDirectory[Path on Linux] / Uuid().show
        dir.create[Directory]()
        dir

      def freshWorktree(): Worktree =
        val dir = freshDir()
        val worktree = Git.init(dir)
        sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
        sh"git -C $dir config user.name Octogenarian".exec[Exit]()
        sh"git -C $dir config commit.gpgsign false".exec[Exit]()
        sh"git -C $dir config tag.gpgsign false".exec[Exit]()
        worktree

      def writeFile(path: Path on Linux, content: Text): Unit =
        if !path.exists() then path.create[File]()
        path.open: handle =>
          Stream(content.data).writeTo(handle)

      test(m"reflog shows one entry per commit and reset"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"a\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"first")

        writeFile(worktree.path / t"b.txt", t"b\n")
        worktree.add(worktree.path / t"b.txt")
        worktree.commit(t"second")

        worktree.reset(ResetMode.Soft, Refspec.head(1))

        // After two commits and a reset to HEAD~1 we expect three reflog
        // entries: commit, commit, reset (newest first).
        val entries = worktree.repo.reflog().to(List)
        entries.length == 3 && entries.head.message.starts(t"reset:")
      .assert(_ == true)

    suite(m"diff / patch parser"):

      def freshDir(): Path on Linux =
        val dir = temporaryDirectory[Path on Linux] / Uuid().show
        dir.create[Directory]()
        dir

      def freshWorktree(): Worktree =
        val dir = freshDir()
        val worktree = Git.init(dir)
        sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
        sh"git -C $dir config user.name Octogenarian".exec[Exit]()
        sh"git -C $dir config commit.gpgsign false".exec[Exit]()
        sh"git -C $dir config tag.gpgsign false".exec[Exit]()
        worktree

      def writeFile(path: Path on Linux, content: Text): Unit =
        if !path.exists() then path.create[File]()
        path.open: handle =>
          Stream(content.data).writeTo(handle)

      test(m"diff() shows working-tree changes against the index"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"first\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"initial")

        writeFile(worktree.path / t"a.txt", t"first\nsecond\n")

        val files = worktree.diff().to(List)
        files.length == 1
          && files.head.changeKind == ChangeKind.Modified
          && files.head.newPath == t"a.txt"
          && files.head.hunks.flatMap(_.edits).exists:
              case Ins(_, t"second") => true
              case _                 => false
      .assert(_ == true)

      test(m"diff() detects an added file"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"a\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"initial")

        writeFile(worktree.path / t"b.txt", t"b\n")
        worktree.add(worktree.path / t"b.txt")

        val staged = worktree.diff(staged = true).to(List)
        staged.length == 1
          && staged.head.changeKind == ChangeKind.Added
          && staged.head.newPath == t"b.txt"
      .assert(_ == true)

      test(m"diff(ref) compares working tree to a ref"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"v1\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"v1")
        val firstHash = worktree.repo.revParse(Refspec.head())

        writeFile(worktree.path / t"a.txt", t"v2\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"v2")

        val files = worktree.diff(firstHash).to(List)
        files.length == 1 && files.head.hunks.nonEmpty
      .assert(_ == true)

    suite(m"worktree management"):

      def freshDir(): Path on Linux =
        val dir = temporaryDirectory[Path on Linux] / Uuid().show
        dir.create[Directory]()
        dir

      def freshWorktree(): Worktree =
        val dir = freshDir()
        val worktree = Git.init(dir)
        sh"git -C $dir config user.email octogenarian@test.local".exec[Exit]()
        sh"git -C $dir config user.name Octogenarian".exec[Exit]()
        sh"git -C $dir config commit.gpgsign false".exec[Exit]()
        sh"git -C $dir config tag.gpgsign false".exec[Exit]()
        worktree

      def writeFile(path: Path on Linux, content: Text): Unit =
        if !path.exists() then path.create[File]()
        path.open: handle =>
          Stream(content.data).writeTo(handle)

      test(m"worktrees() lists the primary worktree of a fresh repo"):
        val worktree = freshWorktree()
        writeFile(worktree.path / t"a.txt", t"a\n")
        worktree.add(worktree.path / t"a.txt")
        worktree.commit(t"first")

        worktree.repo.worktrees().length
      .assert(_ == 1)

      test(m"addWorktree creates a second worktree sharing the object DB"):
        val primary = freshWorktree()
        writeFile(primary.path / t"a.txt", t"a\n")
        primary.add(primary.path / t"a.txt")
        primary.commit(t"first")

        val secondaryPath = temporaryDirectory[Path on Linux] / Uuid().show
        val secondary = primary.repo.addWorktree(secondaryPath, GitBranch(t"main"), detach = true)

        // Both worktrees see the same single commit because they share the
        // primary's object database.
        primary.repo.log().to(List).length == 1
          && secondary.repo.log().to(List).length == 1
          && primary.repo.worktrees().length == 2
      .assert(_ == true)

      test(m"removeWorktree deletes a secondary and shrinks the listing"):
        val primary = freshWorktree()
        writeFile(primary.path / t"a.txt", t"a\n")
        primary.add(primary.path / t"a.txt")
        primary.commit(t"first")

        val secondaryPath = temporaryDirectory[Path on Linux] / Uuid().show
        val secondary = primary.repo.addWorktree(secondaryPath, GitBranch(t"main"), detach = true)
        secondary.remove()

        primary.repo.worktrees().length
      .assert(_ == 1)
