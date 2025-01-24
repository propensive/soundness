/*
    Guillotine, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import language.experimental.captureChecking

import anticipation.*
import prepositional.*
import rudiments.*

trait PosixCommands:
  erased given alias: ("alias" is Intelligible into Text) = ###
  erased given ar: ("ar" is Intelligible into Text) = ###
  erased given at: ("at" is Intelligible into Text) = ###
  erased given awk: ("awk" is Intelligible into Text) = ###
  erased given basename: ("basename" is Intelligible into Text) = ###
  erased given batch: ("batch" is Intelligible into Text) = ###
  erased given bc: ("bc" is Intelligible into Text) = ###
  erased given cat: ("cat" is Intelligible into Stream[Text]) = ###
  erased given chgrp: ("chgrp" is Intelligible into Exit) = ###
  erased given chmod: ("chmod" is Intelligible into Exit) = ###
  erased given chown: ("chown" is Intelligible into Exit) = ###
  erased given cksum: ("cksum" is Intelligible into Text) = ###
  erased given cmp: ("cmp" is Intelligible into Text) = ###
  erased given comm: ("comm" is Intelligible into Text) = ###
  erased given command: ("command" is Intelligible into Text) = ###
  erased given cp: ("cp" is Intelligible into Exit) = ###
  erased given crontab: ("crontab" is Intelligible into Text) = ###
  erased given csplit: ("csplit" is Intelligible into Text) = ###
  erased given cut: ("cut" is Intelligible into Text) = ###
  erased given date: ("date" is Intelligible into Text) = ###
  erased given dd: ("dd" is Intelligible into Text) = ###
  erased given df: ("df" is Intelligible into Text) = ###
  erased given diff: ("diff" is Intelligible into Text) = ###
  erased given dirname: ("dirname" is Intelligible into Text) = ###
  erased given du: ("du" is Intelligible into Text) = ###
  erased given echo: ("echo" is Intelligible into Text) = ###
  erased given ed: ("ed" is Intelligible into Text) = ###
  erased given env: ("env" is Intelligible into Text) = ###
  erased given expand: ("expand" is Intelligible into Text) = ###
  erased given expr: ("expr" is Intelligible into Text) = ###
  erased given file: ("file" is Intelligible into Text) = ###
  erased given find: ("find" is Intelligible into Text) = ###
  erased given fold: ("fold" is Intelligible into Text) = ###
  erased given gencat: ("gencat" is Intelligible into Text) = ###
  erased given getconf: ("getconf" is Intelligible into Text) = ###
  erased given getopts: ("getopts" is Intelligible into Text) = ###
  erased given grep: ("grep" is Intelligible into Stream[Text]) = ###
  erased given hash: ("hash" is Intelligible into Text) = ###
  erased given head: ("head" is Intelligible into Stream[Text]) = ###
  erased given iconv: ("iconv" is Intelligible into Text) = ###
  erased given id: ("id" is Intelligible into Text) = ###
  erased given join: ("join" is Intelligible into Text) = ###
  erased given kill: ("kill" is Intelligible into Exit) = ###
  erased given ln: ("ln" is Intelligible into Exit) = ###
  erased given locale: ("locale" is Intelligible into Text) = ###
  erased given localedef: ("localedef" is Intelligible into Text) = ###
  erased given logger: ("logger" is Intelligible into Text) = ###
  erased given logname: ("logname" is Intelligible into Text) = ###
  erased given lp: ("lp" is Intelligible into Text) = ###
  erased given ls: ("ls" is Intelligible into Stream[Text]) = ###
  erased given m4: ("m4" is Intelligible into Text) = ###
  erased given mailx: ("mailx" is Intelligible into Text) = ###
  erased given man: ("man" is Intelligible into Stream[Text]) = ###
  erased given mesg: ("mesg" is Intelligible into Text) = ###
  erased given mkdir: ("mkdir" is Intelligible into Exit) = ###
  erased given mkfifo: ("mkfifo" is Intelligible into Exit) = ###
  erased given mv: ("mv" is Intelligible into Exit) = ###
  erased given newgrp: ("newgrp" is Intelligible into Exit) = ###
  erased given od: ("od" is Intelligible into Text) = ###
  erased given paste: ("paste" is Intelligible into Text) = ###
  erased given patch: ("patch" is Intelligible into Text) = ###
  erased given patchchk: ("patchchk" is Intelligible into Text) = ###
  erased given pax: ("pax" is Intelligible into Text) = ###
  erased given pr: ("pr" is Intelligible into Text) = ###
  erased given printf: ("printf" is Intelligible into Text) = ###
  erased given ps: ("ps" is Intelligible into Text) = ###
  erased given pwd: ("pwd" is Intelligible into Text) = ###
  erased given read: ("read" is Intelligible into Text) = ###
  erased given renice: ("renice" is Intelligible into Text) = ###
  erased given rm: ("rm" is Intelligible into Exit) = ###
  erased given rmdir: ("rmdir" is Intelligible into Exit) = ###
  erased given sed: ("sed" is Intelligible into Stream[Text]) = ###
  erased given sleep: ("sleep" is Intelligible into Exit) = ###
  erased given sort: ("sort" is Intelligible into Stream[Text]) = ###
  erased given split: ("split" is Intelligible into Text) = ###
  erased given strings: ("strings" is Intelligible into Stream[Text]) = ###
  erased given stty: ("stty" is Intelligible into Text) = ###
  erased given tabs: ("tabs" is Intelligible into Text) = ###
  erased given tail: ("tail" is Intelligible into Stream[Text]) = ###
  erased given tee: ("tee" is Intelligible into Text) = ###
  erased given test: ("test" is Intelligible into Text) = ###
  erased given touch: ("touch" is Intelligible into Exit) = ###
  erased given tput: ("tput" is Intelligible into Text) = ###
  erased given tr: ("tr" is Intelligible into Text) = ###
  erased given tsort: ("tsort" is Intelligible into Text) = ###
  erased given tty: ("tty" is Intelligible into Text) = ###
  erased given umask: ("umask" is Intelligible into Text) = ###
  erased given unalias: ("unalias" is Intelligible into Text) = ###
  erased given uname: ("uname" is Intelligible into Text) = ###
  erased given unexpand: ("unexpand" is Intelligible into Text) = ###
  erased given uniq: ("uniq" is Intelligible into Stream[Text]) = ###
  erased given uudecode: ("uudecode" is Intelligible into Text) = ###
  erased given uuencode: ("uuencode" is Intelligible into Text) = ###
  erased given waitCommand: ("wait" is Intelligible into Text) = ###
  erased given wc: ("wc" is Intelligible into Text) = ###

  erased given which: [PathType] => (erased PathType is SpecificPath)
  =>    ("which" is Intelligible into PathType) = ###

  erased given who: ("who" is Intelligible into Text) = ###
  erased given write: ("write" is Intelligible into Text) = ###
