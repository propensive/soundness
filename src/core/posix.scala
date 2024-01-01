/*
    Guillotine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package guillotine

import rudiments.*
import anticipation.*

import language.experimental.captureChecking

trait PosixCommandOutputs:
  erased given alias: CommandOutput["alias", Text] = ###
  erased given ar: CommandOutput["ar", Text] = ###
  erased given at: CommandOutput["at", Text] = ###
  erased given awk: CommandOutput["awk", Text] = ###
  erased given basename: CommandOutput["basename", Text] = ###
  erased given batch: CommandOutput["batch", Text] = ###
  erased given bc: CommandOutput["bc", Text] = ###
  erased given cat: CommandOutput["cat", LazyList[Text]] = ###
  erased given chgrp: CommandOutput["chgrp", ExitStatus] = ###
  erased given chmod: CommandOutput["chmod", ExitStatus] = ###
  erased given chown: CommandOutput["chown", ExitStatus] = ###
  erased given cksum: CommandOutput["cksum", Text] = ###
  erased given cmp: CommandOutput["cmp", Text] = ###
  erased given comm: CommandOutput["comm", Text] = ###
  erased given command: CommandOutput["command", Text] = ###
  erased given cp: CommandOutput["cp", ExitStatus] = ###
  erased given crontab: CommandOutput["crontab", Text] = ###
  erased given csplit: CommandOutput["csplit", Text] = ###
  erased given cut: CommandOutput["cut", Text] = ###
  erased given date: CommandOutput["date", Text] = ###
  erased given dd: CommandOutput["dd", Text] = ###
  erased given df: CommandOutput["df", Text] = ###
  erased given diff: CommandOutput["diff", Text] = ###
  erased given dirname: CommandOutput["dirname", Text] = ###
  erased given du: CommandOutput["du", Text] = ###
  erased given echo: CommandOutput["echo", Text] = ###
  erased given ed: CommandOutput["ed", Text] = ###
  erased given env: CommandOutput["env", Text] = ###
  erased given expand: CommandOutput["expand", Text] = ###
  erased given expr: CommandOutput["expr", Text] = ###
  erased given file: CommandOutput["file", Text] = ###
  erased given find: CommandOutput["find", Text] = ###
  erased given fold: CommandOutput["fold", Text] = ###
  erased given gencat: CommandOutput["gencat", Text] = ###
  erased given getconf: CommandOutput["getconf", Text] = ###
  erased given getopts: CommandOutput["getopts", Text] = ###
  erased given grep: CommandOutput["grep", LazyList[Text]] = ###
  erased given hash: CommandOutput["hash", Text] = ###
  erased given head: CommandOutput["head", LazyList[Text]] = ###
  erased given iconv: CommandOutput["iconv", Text] = ###
  erased given id: CommandOutput["id", Text] = ###
  erased given join: CommandOutput["join", Text] = ###
  erased given kill: CommandOutput["kill", ExitStatus] = ###
  erased given ln: CommandOutput["ln", ExitStatus] = ###
  erased given locale: CommandOutput["locale", Text] = ###
  erased given localedef: CommandOutput["localedef", Text] = ###
  erased given logger: CommandOutput["logger", Text] = ###
  erased given logname: CommandOutput["logname", Text] = ###
  erased given lp: CommandOutput["lp", Text] = ###
  erased given ls: CommandOutput["ls", LazyList[Text]] = ###
  erased given m4: CommandOutput["m4", Text] = ###
  erased given mailx: CommandOutput["mailx", Text] = ###
  erased given man: CommandOutput["man", LazyList[Text]] = ###
  erased given mesg: CommandOutput["mesg", Text] = ###
  erased given mkdir: CommandOutput["mkdir", ExitStatus] = ###
  erased given mkfifo: CommandOutput["mkfifo", ExitStatus] = ###
  erased given mv: CommandOutput["mv", ExitStatus] = ###
  erased given newgrp: CommandOutput["newgrp", ExitStatus] = ###
  erased given od: CommandOutput["od", Text] = ###
  erased given paste: CommandOutput["paste", Text] = ###
  erased given patch: CommandOutput["patch", Text] = ###
  erased given patchchk: CommandOutput["patchchk", Text] = ###
  erased given pax: CommandOutput["pax", Text] = ###
  erased given pr: CommandOutput["pr", Text] = ###
  erased given printf: CommandOutput["printf", Text] = ###
  erased given ps: CommandOutput["ps", Text] = ###
  erased given pwd: CommandOutput["pwd", Text] = ###
  erased given read: CommandOutput["read", Text] = ###
  erased given renice: CommandOutput["renice", Text] = ###
  erased given rm: CommandOutput["rm", ExitStatus] = ###
  erased given rmdir: CommandOutput["rmdir", ExitStatus] = ###
  erased given sed: CommandOutput["sed", LazyList[Text]] = ###
  erased given sleep: CommandOutput["sleep", ExitStatus] = ###
  erased given sort: CommandOutput["sort", LazyList[Text]] = ###
  erased given split: CommandOutput["split", Text] = ###
  erased given strings: CommandOutput["strings", LazyList[Text]] = ###
  erased given stty: CommandOutput["stty", Text] = ###
  erased given tabs: CommandOutput["tabs", Text] = ###
  erased given tail: CommandOutput["tail", LazyList[Text]] = ###
  erased given tee: CommandOutput["tee", Text] = ###
  erased given test: CommandOutput["test", Text] = ###
  erased given touch: CommandOutput["touch", ExitStatus] = ###
  erased given tput: CommandOutput["tput", Text] = ###
  erased given tr: CommandOutput["tr", Text] = ###
  erased given tsort: CommandOutput["tsort", Text] = ###
  erased given tty: CommandOutput["tty", Text] = ###
  erased given umask: CommandOutput["umask", Text] = ###
  erased given unalias: CommandOutput["unalias", Text] = ###
  erased given uname: CommandOutput["uname", Text] = ###
  erased given unexpand: CommandOutput["unexpand", Text] = ###
  erased given uniq: CommandOutput["uniq", LazyList[Text]] = ###
  erased given uudecode: CommandOutput["uudecode", Text] = ###
  erased given uuencode: CommandOutput["uuencode", Text] = ###
  erased given waitCommand: CommandOutput["wait", Text] = ###
  erased given wc: CommandOutput["wc", Text] = ###
  erased given which[PathType: SpecificPath]: CommandOutput["which", PathType] = ###
  erased given who: CommandOutput["who", Text] = ###
  erased given write: CommandOutput["write", Text] = ###
