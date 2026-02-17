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
package guillotine

import anticipation.*
import prepositional.*
import proscenium.*
import rudiments.*

trait PosixCommands:
  inline given alias: ("alias" is Intelligible to Text) = !!
  inline given ar: ("ar" is Intelligible to Text) = !!
  inline given at: ("at" is Intelligible to Text) = !!
  inline given awk: ("awk" is Intelligible to Text) = !!
  inline given basename: ("basename" is Intelligible to Text) = !!
  inline given batch: ("batch" is Intelligible to Text) = !!
  inline given bc: ("bc" is Intelligible to Text) = !!
  inline given cat: ("cat" is Intelligible to Stream[Text]) = !!
  inline given chgrp: ("chgrp" is Intelligible to Exit) = !!
  inline given chmod: ("chmod" is Intelligible to Exit) = !!
  inline given chown: ("chown" is Intelligible to Exit) = !!
  inline given cksum: ("cksum" is Intelligible to Text) = !!
  inline given cmp: ("cmp" is Intelligible to Text) = !!
  inline given comm: ("comm" is Intelligible to Text) = !!
  inline given command: ("command" is Intelligible to Text) = !!
  inline given cp: ("cp" is Intelligible to Exit) = !!
  inline given crontab: ("crontab" is Intelligible to Text) = !!
  inline given csplit: ("csplit" is Intelligible to Text) = !!
  inline given cut: ("cut" is Intelligible to Text) = !!
  inline given date: ("date" is Intelligible to Text) = !!
  inline given dd: ("dd" is Intelligible to Text) = !!
  inline given df: ("df" is Intelligible to Text) = !!
  inline given diff: ("diff" is Intelligible to Text) = !!
  inline given dirname: ("dirname" is Intelligible to Text) = !!
  inline given du: ("du" is Intelligible to Text) = !!
  inline given echo: ("echo" is Intelligible to Text) = !!
  inline given ed: ("ed" is Intelligible to Text) = !!
  inline given env: ("env" is Intelligible to Text) = !!
  inline given expand: ("expand" is Intelligible to Text) = !!
  inline given expr: ("expr" is Intelligible to Text) = !!
  inline given file: ("file" is Intelligible to Text) = !!
  inline given find: ("find" is Intelligible to Text) = !!
  inline given fold: ("fold" is Intelligible to Text) = !!
  inline given gencat: ("gencat" is Intelligible to Text) = !!
  inline given getconf: ("getconf" is Intelligible to Text) = !!
  inline given getopts: ("getopts" is Intelligible to Text) = !!
  inline given grep: ("grep" is Intelligible to Stream[Text]) = !!
  inline given hash: ("hash" is Intelligible to Text) = !!
  inline given head: ("head" is Intelligible to Stream[Text]) = !!
  inline given iconv: ("iconv" is Intelligible to Text) = !!
  inline given id: ("id" is Intelligible to Text) = !!
  inline given join: ("join" is Intelligible to Text) = !!
  inline given kill: ("kill" is Intelligible to Exit) = !!
  inline given ln: ("ln" is Intelligible to Exit) = !!
  inline given locale: ("locale" is Intelligible to Text) = !!
  inline given localedef: ("localedef" is Intelligible to Text) = !!
  inline given logger: ("logger" is Intelligible to Text) = !!
  inline given logname: ("logname" is Intelligible to Text) = !!
  inline given lp: ("lp" is Intelligible to Text) = !!
  inline given ls: ("ls" is Intelligible to Stream[Text]) = !!
  inline given m4: ("m4" is Intelligible to Text) = !!
  inline given mailx: ("mailx" is Intelligible to Text) = !!
  inline given man: ("man" is Intelligible to Stream[Text]) = !!
  inline given mesg: ("mesg" is Intelligible to Text) = !!
  inline given mkdir: ("mkdir" is Intelligible to Exit) = !!
  inline given mkfifo: ("mkfifo" is Intelligible to Exit) = !!
  inline given mv: ("mv" is Intelligible to Exit) = !!
  inline given newgrp: ("newgrp" is Intelligible to Exit) = !!
  inline given od: ("od" is Intelligible to Text) = !!
  inline given paste: ("paste" is Intelligible to Text) = !!
  inline given patch: ("patch" is Intelligible to Text) = !!
  inline given patchchk: ("patchchk" is Intelligible to Text) = !!
  inline given pax: ("pax" is Intelligible to Text) = !!
  inline given pr: ("pr" is Intelligible to Text) = !!
  inline given printf: ("printf" is Intelligible to Text) = !!
  inline given ps: ("ps" is Intelligible to Text) = !!
  inline given pwd: ("pwd" is Intelligible to Text) = !!
  inline given read: ("read" is Intelligible to Text) = !!
  inline given renice: ("renice" is Intelligible to Text) = !!
  inline given rm: ("rm" is Intelligible to Exit) = !!
  inline given rmdir: ("rmdir" is Intelligible to Exit) = !!
  inline given sed: ("sed" is Intelligible to Stream[Text]) = !!
  inline given sleep: ("sleep" is Intelligible to Exit) = !!
  inline given sort: ("sort" is Intelligible to Stream[Text]) = !!
  inline given split: ("split" is Intelligible to Text) = !!
  inline given strings: ("strings" is Intelligible to Stream[Text]) = !!
  inline given stty: ("stty" is Intelligible to Text) = !!
  inline given tabs: ("tabs" is Intelligible to Text) = !!
  inline given tail: ("tail" is Intelligible to Stream[Text]) = !!
  inline given tee: ("tee" is Intelligible to Text) = !!
  inline given test: ("test" is Intelligible to Text) = !!
  inline given touch: ("touch" is Intelligible to Exit) = !!
  inline given tput: ("tput" is Intelligible to Text) = !!
  inline given tr: ("tr" is Intelligible to Text) = !!
  inline given tsort: ("tsort" is Intelligible to Text) = !!
  inline given tty: ("tty" is Intelligible to Text) = !!
  inline given umask: ("umask" is Intelligible to Text) = !!
  inline given unalias: ("unalias" is Intelligible to Text) = !!
  inline given uname: ("uname" is Intelligible to Text) = !!
  inline given unexpand: ("unexpand" is Intelligible to Text) = !!
  inline given uniq: ("uniq" is Intelligible to Stream[Text]) = !!
  inline given uudecode: ("uudecode" is Intelligible to Text) = !!
  inline given uuencode: ("uuencode" is Intelligible to Text) = !!
  inline given waitCommand: ("wait" is Intelligible to Text) = !!
  inline given wc: ("wc" is Intelligible to Text) = !!

  inline given which: [path] => (erased path is Instantiable across Paths from Text)
  =>  ( "which" is Intelligible to path ) =

      !!

  inline given who: ("who" is Intelligible to Text) = !!
  inline given write: ("write" is Intelligible to Text) = !!
