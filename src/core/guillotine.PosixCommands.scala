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
  erased given ("alias" is Intelligible into Text) as alias = ###
  erased given ("ar" is Intelligible into Text) as ar = ###
  erased given ("at" is Intelligible into Text) as at = ###
  erased given ("awk" is Intelligible into Text) as awk = ###
  erased given ("basename" is Intelligible into Text) as basename = ###
  erased given ("batch" is Intelligible into Text) as batch = ###
  erased given ("bc" is Intelligible into Text) as bc = ###
  erased given ("cat" is Intelligible into LazyList[Text]) as cat = ###
  erased given ("chgrp" is Intelligible into Exit) as chgrp = ###
  erased given ("chmod" is Intelligible into Exit) as chmod = ###
  erased given ("chown" is Intelligible into Exit) as chown = ###
  erased given ("cksum" is Intelligible into Text) as cksum = ###
  erased given ("cmp" is Intelligible into Text) as cmp = ###
  erased given ("comm" is Intelligible into Text) as comm = ###
  erased given ("command" is Intelligible into Text) as command = ###
  erased given ("cp" is Intelligible into Exit) as cp = ###
  erased given ("crontab" is Intelligible into Text) as crontab = ###
  erased given ("csplit" is Intelligible into Text) as csplit = ###
  erased given ("cut" is Intelligible into Text) as cut = ###
  erased given ("date" is Intelligible into Text) as date = ###
  erased given ("dd" is Intelligible into Text) as dd = ###
  erased given ("df" is Intelligible into Text) as df = ###
  erased given ("diff" is Intelligible into Text) as diff = ###
  erased given ("dirname" is Intelligible into Text) as dirname = ###
  erased given ("du" is Intelligible into Text) as du = ###
  erased given ("echo" is Intelligible into Text) as echo = ###
  erased given ("ed" is Intelligible into Text) as ed = ###
  erased given ("env" is Intelligible into Text) as env = ###
  erased given ("expand" is Intelligible into Text) as expand = ###
  erased given ("expr" is Intelligible into Text) as expr = ###
  erased given ("file" is Intelligible into Text) as file = ###
  erased given ("find" is Intelligible into Text) as find = ###
  erased given ("fold" is Intelligible into Text) as fold = ###
  erased given ("gencat" is Intelligible into Text) as gencat = ###
  erased given ("getconf" is Intelligible into Text) as getconf = ###
  erased given ("getopts" is Intelligible into Text) as getopts = ###
  erased given ("grep" is Intelligible into LazyList[Text]) as grep = ###
  erased given ("hash" is Intelligible into Text) as hash = ###
  erased given ("head" is Intelligible into LazyList[Text]) as head = ###
  erased given ("iconv" is Intelligible into Text) as iconv = ###
  erased given ("id" is Intelligible into Text) as id = ###
  erased given ("join" is Intelligible into Text) as join = ###
  erased given ("kill" is Intelligible into Exit) as kill = ###
  erased given ("ln" is Intelligible into Exit) as ln = ###
  erased given ("locale" is Intelligible into Text) as locale = ###
  erased given ("localedef" is Intelligible into Text) as localedef = ###
  erased given ("logger" is Intelligible into Text) as logger = ###
  erased given ("logname" is Intelligible into Text) as logname = ###
  erased given ("lp" is Intelligible into Text) as lp = ###
  erased given ("ls" is Intelligible into LazyList[Text]) as ls = ###
  erased given ("m4" is Intelligible into Text) as m4 = ###
  erased given ("mailx" is Intelligible into Text) as mailx = ###
  erased given ("man" is Intelligible into LazyList[Text]) as man = ###
  erased given ("mesg" is Intelligible into Text) as mesg = ###
  erased given ("mkdir" is Intelligible into Exit) as mkdir = ###
  erased given ("mkfifo" is Intelligible into Exit) as mkfifo = ###
  erased given ("mv" is Intelligible into Exit) as mv = ###
  erased given ("newgrp" is Intelligible into Exit) as newgrp = ###
  erased given ("od" is Intelligible into Text) as od = ###
  erased given ("paste" is Intelligible into Text) as paste = ###
  erased given ("patch" is Intelligible into Text) as patch = ###
  erased given ("patchchk" is Intelligible into Text) as patchchk = ###
  erased given ("pax" is Intelligible into Text) as pax = ###
  erased given ("pr" is Intelligible into Text) as pr = ###
  erased given ("printf" is Intelligible into Text) as printf = ###
  erased given ("ps" is Intelligible into Text) as ps = ###
  erased given ("pwd" is Intelligible into Text) as pwd = ###
  erased given ("read" is Intelligible into Text) as read = ###
  erased given ("renice" is Intelligible into Text) as renice = ###
  erased given ("rm" is Intelligible into Exit) as rm = ###
  erased given ("rmdir" is Intelligible into Exit) as rmdir = ###
  erased given ("sed" is Intelligible into LazyList[Text]) as sed = ###
  erased given ("sleep" is Intelligible into Exit) as sleep = ###
  erased given ("sort" is Intelligible into LazyList[Text]) as sort = ###
  erased given ("split" is Intelligible into Text) as split = ###
  erased given ("strings" is Intelligible into LazyList[Text]) as strings = ###
  erased given ("stty" is Intelligible into Text) as stty = ###
  erased given ("tabs" is Intelligible into Text) as tabs = ###
  erased given ("tail" is Intelligible into LazyList[Text]) as tail = ###
  erased given ("tee" is Intelligible into Text) as tee = ###
  erased given ("test" is Intelligible into Text) as test = ###
  erased given ("touch" is Intelligible into Exit) as touch = ###
  erased given ("tput" is Intelligible into Text) as tput = ###
  erased given ("tr" is Intelligible into Text) as tr = ###
  erased given ("tsort" is Intelligible into Text) as tsort = ###
  erased given ("tty" is Intelligible into Text) as tty = ###
  erased given ("umask" is Intelligible into Text) as umask = ###
  erased given ("unalias" is Intelligible into Text) as unalias = ###
  erased given ("uname" is Intelligible into Text) as uname = ###
  erased given ("unexpand" is Intelligible into Text) as unexpand = ###
  erased given ("uniq" is Intelligible into LazyList[Text]) as uniq = ###
  erased given ("uudecode" is Intelligible into Text) as uudecode = ###
  erased given ("uuencode" is Intelligible into Text) as uuencode = ###
  erased given ("wait" is Intelligible into Text) as waitCommand = ###
  erased given ("wc" is Intelligible into Text) as wc = ###

  erased given [PathType](using erased PathType is SpecificPath)
      => ("which" is Intelligible into PathType) as which = ###

  erased given ("who" is Intelligible into Text) as who = ###
  erased given ("write" is Intelligible into Text) as write = ###
