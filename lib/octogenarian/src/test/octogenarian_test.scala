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
        path.create[File]()
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
        path.create[File]()
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
