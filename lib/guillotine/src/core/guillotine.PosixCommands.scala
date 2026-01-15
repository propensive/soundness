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
┃    Soundness, version 0.51.0.                                                                    ┃
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
import proscenium.*
import rudiments.*

trait PosixCommands:
  erased given alias: ("alias" is Intelligible to Text) = !!
  erased given ar: ("ar" is Intelligible to Text) = !!
  erased given at: ("at" is Intelligible to Text) = !!
  erased given awk: ("awk" is Intelligible to Text) = !!
  erased given basename: ("basename" is Intelligible to Text) = !!
  erased given batch: ("batch" is Intelligible to Text) = !!
  erased given bc: ("bc" is Intelligible to Text) = !!
  erased given cat: ("cat" is Intelligible to Stream[Text]) = !!
  erased given chgrp: ("chgrp" is Intelligible to Exit) = !!
  erased given chmod: ("chmod" is Intelligible to Exit) = !!
  erased given chown: ("chown" is Intelligible to Exit) = !!
  erased given cksum: ("cksum" is Intelligible to Text) = !!
  erased given cmp: ("cmp" is Intelligible to Text) = !!
  erased given comm: ("comm" is Intelligible to Text) = !!
  erased given command: ("command" is Intelligible to Text) = !!
  erased given cp: ("cp" is Intelligible to Exit) = !!
  erased given crontab: ("crontab" is Intelligible to Text) = !!
  erased given csplit: ("csplit" is Intelligible to Text) = !!
  erased given cut: ("cut" is Intelligible to Text) = !!
  erased given date: ("date" is Intelligible to Text) = !!
  erased given dd: ("dd" is Intelligible to Text) = !!
  erased given df: ("df" is Intelligible to Text) = !!
  erased given diff: ("diff" is Intelligible to Text) = !!
  erased given dirname: ("dirname" is Intelligible to Text) = !!
  erased given du: ("du" is Intelligible to Text) = !!
  erased given echo: ("echo" is Intelligible to Text) = !!
  erased given ed: ("ed" is Intelligible to Text) = !!
  erased given env: ("env" is Intelligible to Text) = !!
  erased given expand: ("expand" is Intelligible to Text) = !!
  erased given expr: ("expr" is Intelligible to Text) = !!
  erased given file: ("file" is Intelligible to Text) = !!
  erased given find: ("find" is Intelligible to Text) = !!
  erased given fold: ("fold" is Intelligible to Text) = !!
  erased given gencat: ("gencat" is Intelligible to Text) = !!
  erased given getconf: ("getconf" is Intelligible to Text) = !!
  erased given getopts: ("getopts" is Intelligible to Text) = !!
  erased given grep: ("grep" is Intelligible to Stream[Text]) = !!
  erased given hash: ("hash" is Intelligible to Text) = !!
  erased given head: ("head" is Intelligible to Stream[Text]) = !!
  erased given iconv: ("iconv" is Intelligible to Text) = !!
  erased given id: ("id" is Intelligible to Text) = !!
  erased given join: ("join" is Intelligible to Text) = !!
  erased given kill: ("kill" is Intelligible to Exit) = !!
  erased given ln: ("ln" is Intelligible to Exit) = !!
  erased given locale: ("locale" is Intelligible to Text) = !!
  erased given localedef: ("localedef" is Intelligible to Text) = !!
  erased given logger: ("logger" is Intelligible to Text) = !!
  erased given logname: ("logname" is Intelligible to Text) = !!
  erased given lp: ("lp" is Intelligible to Text) = !!
  erased given ls: ("ls" is Intelligible to Stream[Text]) = !!
  erased given m4: ("m4" is Intelligible to Text) = !!
  erased given mailx: ("mailx" is Intelligible to Text) = !!
  erased given man: ("man" is Intelligible to Stream[Text]) = !!
  erased given mesg: ("mesg" is Intelligible to Text) = !!
  erased given mkdir: ("mkdir" is Intelligible to Exit) = !!
  erased given mkfifo: ("mkfifo" is Intelligible to Exit) = !!
  erased given mv: ("mv" is Intelligible to Exit) = !!
  erased given newgrp: ("newgrp" is Intelligible to Exit) = !!
  erased given od: ("od" is Intelligible to Text) = !!
  erased given paste: ("paste" is Intelligible to Text) = !!
  erased given patch: ("patch" is Intelligible to Text) = !!
  erased given patchchk: ("patchchk" is Intelligible to Text) = !!
  erased given pax: ("pax" is Intelligible to Text) = !!
  erased given pr: ("pr" is Intelligible to Text) = !!
  erased given printf: ("printf" is Intelligible to Text) = !!
  erased given ps: ("ps" is Intelligible to Text) = !!
  erased given pwd: ("pwd" is Intelligible to Text) = !!
  erased given read: ("read" is Intelligible to Text) = !!
  erased given renice: ("renice" is Intelligible to Text) = !!
  erased given rm: ("rm" is Intelligible to Exit) = !!
  erased given rmdir: ("rmdir" is Intelligible to Exit) = !!
  erased given sed: ("sed" is Intelligible to Stream[Text]) = !!
  erased given sleep: ("sleep" is Intelligible to Exit) = !!
  erased given sort: ("sort" is Intelligible to Stream[Text]) = !!
  erased given split: ("split" is Intelligible to Text) = !!
  erased given strings: ("strings" is Intelligible to Stream[Text]) = !!
  erased given stty: ("stty" is Intelligible to Text) = !!
  erased given tabs: ("tabs" is Intelligible to Text) = !!
  erased given tail: ("tail" is Intelligible to Stream[Text]) = !!
  erased given tee: ("tee" is Intelligible to Text) = !!
  erased given test: ("test" is Intelligible to Text) = !!
  erased given touch: ("touch" is Intelligible to Exit) = !!
  erased given tput: ("tput" is Intelligible to Text) = !!
  erased given tr: ("tr" is Intelligible to Text) = !!
  erased given tsort: ("tsort" is Intelligible to Text) = !!
  erased given tty: ("tty" is Intelligible to Text) = !!
  erased given umask: ("umask" is Intelligible to Text) = !!
  erased given unalias: ("unalias" is Intelligible to Text) = !!
  erased given uname: ("uname" is Intelligible to Text) = !!
  erased given unexpand: ("unexpand" is Intelligible to Text) = !!
  erased given uniq: ("uniq" is Intelligible to Stream[Text]) = !!
  erased given uudecode: ("uudecode" is Intelligible to Text) = !!
  erased given uuencode: ("uuencode" is Intelligible to Text) = !!
  erased given waitCommand: ("wait" is Intelligible to Text) = !!
  erased given wc: ("wc" is Intelligible to Text) = !!

  erased given which: [path] => (erased path is Instantiable across Paths from Text)
               => ("which" is Intelligible to path) = !!

  erased given who: ("who" is Intelligible to Text) = !!
  erased given write: ("write" is Intelligible to Text) = !!
