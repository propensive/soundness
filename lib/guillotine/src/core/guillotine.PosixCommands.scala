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
package guillotine

import anticipation.*
import prepositional.*
import rudiments.*

trait PosixCommands:
  erased given alias: ("alias" is Intelligible to Text) = Intelligible.Evidence["alias", Text]()
  erased given ar: ("ar" is Intelligible to Text) = Intelligible.Evidence["ar", Text]()
  erased given at: ("at" is Intelligible to Text) = Intelligible.Evidence["at", Text]()
  erased given awk: ("awk" is Intelligible to Text) = Intelligible.Evidence["awk", Text]()

  erased given basename: ("basename" is Intelligible to Text) =
    Intelligible.Evidence["basename", Text]()

  erased given batch: ("batch" is Intelligible to Text) = Intelligible.Evidence["batch", Text]()
  erased given bc: ("bc" is Intelligible to Text) = Intelligible.Evidence["bc", Text]()

  erased given cat: ("cat" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["cat", Iterator[Text]]()

  erased given chgrp: ("chgrp" is Intelligible to Exit) = Intelligible.Evidence["chgrp", Exit]()
  erased given chmod: ("chmod" is Intelligible to Exit) = Intelligible.Evidence["chmod", Exit]()
  erased given chown: ("chown" is Intelligible to Exit) = Intelligible.Evidence["chown", Exit]()
  erased given cksum: ("cksum" is Intelligible to Text) = Intelligible.Evidence["cksum", Text]()
  erased given cmp: ("cmp" is Intelligible to Text) = Intelligible.Evidence["cmp", Text]()
  erased given comm: ("comm" is Intelligible to Text) = Intelligible.Evidence["comm", Text]()

  erased given command: ("command" is Intelligible to Text) =
    Intelligible.Evidence["command", Text]()

  erased given cp: ("cp" is Intelligible to Exit) = Intelligible.Evidence["cp", Exit]()

  erased given crontab: ("crontab" is Intelligible to Text) =
    Intelligible.Evidence["crontab", Text]()

  erased given csplit: ("csplit" is Intelligible to Text) = Intelligible.Evidence["csplit", Text]()
  erased given cut: ("cut" is Intelligible to Text) = Intelligible.Evidence["cut", Text]()
  erased given date: ("date" is Intelligible to Text) = Intelligible.Evidence["date", Text]()
  erased given dd: ("dd" is Intelligible to Text) = Intelligible.Evidence["dd", Text]()
  erased given df: ("df" is Intelligible to Text) = Intelligible.Evidence["df", Text]()
  erased given diff: ("diff" is Intelligible to Text) = Intelligible.Evidence["diff", Text]()

  erased given dirname: ("dirname" is Intelligible to Text) =
    Intelligible.Evidence["dirname", Text]()

  erased given du: ("du" is Intelligible to Text) = Intelligible.Evidence["du", Text]()
  erased given echo: ("echo" is Intelligible to Text) = Intelligible.Evidence["echo", Text]()
  erased given ed: ("ed" is Intelligible to Text) = Intelligible.Evidence["ed", Text]()
  erased given env: ("env" is Intelligible to Text) = Intelligible.Evidence["env", Text]()
  erased given expand: ("expand" is Intelligible to Text) = Intelligible.Evidence["expand", Text]()
  erased given expr: ("expr" is Intelligible to Text) = Intelligible.Evidence["expr", Text]()
  erased given file: ("file" is Intelligible to Text) = Intelligible.Evidence["file", Text]()
  erased given find: ("find" is Intelligible to Text) = Intelligible.Evidence["find", Text]()
  erased given fold: ("fold" is Intelligible to Text) = Intelligible.Evidence["fold", Text]()
  erased given gencat: ("gencat" is Intelligible to Text) = Intelligible.Evidence["gencat", Text]()

  erased given getconf: ("getconf" is Intelligible to Text) =
    Intelligible.Evidence["getconf", Text]()

  erased given getopts: ("getopts" is Intelligible to Text) =
    Intelligible.Evidence["getopts", Text]()

  erased given grep: ("grep" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["grep", Iterator[Text]]()

  erased given hash: ("hash" is Intelligible to Text) = Intelligible.Evidence["hash", Text]()

  erased given head: ("head" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["head", Iterator[Text]]()

  erased given iconv: ("iconv" is Intelligible to Text) = Intelligible.Evidence["iconv", Text]()
  erased given id: ("id" is Intelligible to Text) = Intelligible.Evidence["id", Text]()
  erased given join: ("join" is Intelligible to Text) = Intelligible.Evidence["join", Text]()
  erased given kill: ("kill" is Intelligible to Exit) = Intelligible.Evidence["kill", Exit]()
  erased given ln: ("ln" is Intelligible to Exit) = Intelligible.Evidence["ln", Exit]()
  erased given locale: ("locale" is Intelligible to Text) = Intelligible.Evidence["locale", Text]()

  erased given localedef: ("localedef" is Intelligible to Text) =
    Intelligible.Evidence["localedef", Text]()

  erased given logger: ("logger" is Intelligible to Text) = Intelligible.Evidence["logger", Text]()

  erased given logname: ("logname" is Intelligible to Text) =
    Intelligible.Evidence["logname", Text]()

  erased given lp: ("lp" is Intelligible to Text) = Intelligible.Evidence["lp", Text]()

  erased given ls: ("ls" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["ls", Iterator[Text]]()

  erased given m4: ("m4" is Intelligible to Text) = Intelligible.Evidence["m4", Text]()
  erased given mailx: ("mailx" is Intelligible to Text) = Intelligible.Evidence["mailx", Text]()

  erased given man: ("man" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["man", Iterator[Text]]()

  erased given mesg: ("mesg" is Intelligible to Text) = Intelligible.Evidence["mesg", Text]()
  erased given mkdir: ("mkdir" is Intelligible to Exit) = Intelligible.Evidence["mkdir", Exit]()
  erased given mkfifo: ("mkfifo" is Intelligible to Exit) = Intelligible.Evidence["mkfifo", Exit]()
  erased given mknod: ("mknod" is Intelligible to Exit) = Intelligible.Evidence["mknod", Exit]()
  erased given mv: ("mv" is Intelligible to Exit) = Intelligible.Evidence["mv", Exit]()
  erased given newgrp: ("newgrp" is Intelligible to Exit) = Intelligible.Evidence["newgrp", Exit]()
  erased given od: ("od" is Intelligible to Text) = Intelligible.Evidence["od", Text]()
  erased given paste: ("paste" is Intelligible to Text) = Intelligible.Evidence["paste", Text]()
  erased given patch: ("patch" is Intelligible to Text) = Intelligible.Evidence["patch", Text]()

  erased given patchchk: ("patchchk" is Intelligible to Text) =
    Intelligible.Evidence["patchchk", Text]()

  erased given pax: ("pax" is Intelligible to Text) = Intelligible.Evidence["pax", Text]()
  erased given pr: ("pr" is Intelligible to Text) = Intelligible.Evidence["pr", Text]()
  erased given printf: ("printf" is Intelligible to Text) = Intelligible.Evidence["printf", Text]()
  erased given ps: ("ps" is Intelligible to Text) = Intelligible.Evidence["ps", Text]()
  erased given pwd: ("pwd" is Intelligible to Text) = Intelligible.Evidence["pwd", Text]()
  erased given read: ("read" is Intelligible to Text) = Intelligible.Evidence["read", Text]()
  erased given renice: ("renice" is Intelligible to Text) = Intelligible.Evidence["renice", Text]()
  erased given rm: ("rm" is Intelligible to Exit) = Intelligible.Evidence["rm", Exit]()
  erased given rmdir: ("rmdir" is Intelligible to Exit) = Intelligible.Evidence["rmdir", Exit]()

  erased given sed: ("sed" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["sed", Iterator[Text]]()

  erased given sleep: ("sleep" is Intelligible to Exit) = Intelligible.Evidence["sleep", Exit]()

  erased given sort: ("sort" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["sort", Iterator[Text]]()

  erased given split: ("split" is Intelligible to Text) = Intelligible.Evidence["split", Text]()

  erased given strings: ("strings" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["strings", Iterator[Text]]()

  erased given stty: ("stty" is Intelligible to Text) = Intelligible.Evidence["stty", Text]()
  erased given tabs: ("tabs" is Intelligible to Text) = Intelligible.Evidence["tabs", Text]()

  erased given tail: ("tail" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["tail", Iterator[Text]]()

  erased given tee: ("tee" is Intelligible to Text) = Intelligible.Evidence["tee", Text]()
  erased given test: ("test" is Intelligible to Text) = Intelligible.Evidence["test", Text]()
  erased given touch: ("touch" is Intelligible to Exit) = Intelligible.Evidence["touch", Exit]()
  erased given tput: ("tput" is Intelligible to Text) = Intelligible.Evidence["tput", Text]()
  erased given tr: ("tr" is Intelligible to Text) = Intelligible.Evidence["tr", Text]()
  erased given tsort: ("tsort" is Intelligible to Text) = Intelligible.Evidence["tsort", Text]()
  erased given tty: ("tty" is Intelligible to Text) = Intelligible.Evidence["tty", Text]()
  erased given umask: ("umask" is Intelligible to Text) = Intelligible.Evidence["umask", Text]()

  erased given unalias: ("unalias" is Intelligible to Text) =
    Intelligible.Evidence["unalias", Text]()

  erased given uname: ("uname" is Intelligible to Text) = Intelligible.Evidence["uname", Text]()

  erased given unexpand: ("unexpand" is Intelligible to Text) =
    Intelligible.Evidence["unexpand", Text]()

  erased given uniq: ("uniq" is Intelligible to Iterator[Text]) =
    Intelligible.Evidence["uniq", Iterator[Text]]()

  erased given uudecode: ("uudecode" is Intelligible to Text) =
    Intelligible.Evidence["uudecode", Text]()

  erased given uuencode: ("uuencode" is Intelligible to Text) =
    Intelligible.Evidence["uuencode", Text]()

  erased given waitCommand: ("wait" is Intelligible to Text) = Intelligible.Evidence["wait", Text]()
  erased given wc: ("wc" is Intelligible to Text) = Intelligible.Evidence["wc", Text]()

  inline given which: [path] => (erased instantiable: path is Instantiable across Paths from Text)
  =>  ( "which" is Intelligible to path ) =

    Intelligible.Evidence["which", path]()

  erased given who: ("who" is Intelligible to Text) = Intelligible.Evidence["who", Text]()
  erased given write: ("write" is Intelligible to Text) = Intelligible.Evidence["write", Text]()
