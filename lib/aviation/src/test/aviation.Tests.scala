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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package aviation

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

object Tests extends Suite(m"Aviation Tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      test(m"Parse a canonical date"):
        Date.parse(t"2011-12-13")
      .check(_ == 2011-Dec-13)

      test(m"Parse a date with a single-digit month"):
        Date.parse(t"2011-09-13")
      .assert(_ == 2011-Sep-13)

      test(m"Parse a date in the distant past"):
        Date.parse(t"59-09-13")
      .assert(_ == 59-Sep-13)

      test(m"Month cannot be higher than 12"):
        capture(Date.parse(t"59-13-13"))
      .assert(_ == DateError(t"59-13-13"))

      test(m"Month cannot be less than 1"):
        capture(Date.parse(t"59-0-13"))
      .assert(_ == DateError(t"59-0-13"))

      test(m"Day cannot be over 31"):
        capture(Date.parse(t"59-11-32"))
      .assert(_ == DateError(t"59-11-32"))

      test(m"Day must exist in month"):
        capture(Date.parse(t"59-11-31"))
      .assert(_ == DateError(t"59-11-31"))


    suite(m"Leap Seconds tests"):
      test(m"There are 10 leap seconds for any year before 1972"):
        LeapSeconds.during(1900, false)
      .assert(_ == 10)

      test(m"There are 10 leap seconds in the first half of 1972"):
        LeapSeconds.during(1972, false)
      .assert(_ == 10)

      test(m"There are 11 leap seconds in the second half of 1972"):
        LeapSeconds.during(1972, true)
      .assert(_ == 11)

      test(m"There are 12 leap seconds in the first half of 1973"):
        LeapSeconds.during(1973, false)
      .assert(_ == 12)

      test(m"There are 12 leap seconds in the second half of 1973"):
        LeapSeconds.during(1973, true)
      .assert(_ == 12)

      test(m"There are 13 leap seconds in the first half of 1974"):
        LeapSeconds.during(1974, false)
      .assert(_ == 13)

      test(m"There are 13 leap seconds in the second half of 1974"):
        LeapSeconds.during(1974, true)
      .assert(_ == 13)

      test(m"There are 19 leap seconds in the first half of 1980"):
        LeapSeconds.during(1980, false).tap(println)
      .assert(_ == 19)

      test(m"There are 19 leap seconds in the first half of 1981"):
        LeapSeconds.during(1981, false)
      .assert(_ == 19)

      test(m"There are 20 leap seconds in the second half of 1981"):
        LeapSeconds.during(1981, true)
      .assert(_ == 20)

      test(m"There are 19 leap seconds in the first half of 1981"):
        LeapSeconds.during(1981, false)
      .assert(_ == 19)

      test(m"There are 20 leap seconds in the second half of 1981"):
        LeapSeconds.during(1981, true)
      .assert(_ == 20)

      test(m"There are 20 leap seconds in the first half of 1982"):
        LeapSeconds.during(1982, false)
      .assert(_ == 20)

      test(m"There are 21 leap seconds in the second half of 1982"):
        LeapSeconds.during(1982, true)
      .assert(_ == 21)

      test(m"There are 24 leap seconds in the second half of 1988"):
        LeapSeconds.during(1988, true)
      .assert(_ == 24)

      test(m"There are 32 leap seconds in the first half of 2000"):
        LeapSeconds.during(2000, false)
      .assert(_ == 32)

      test(m"There are 37 leap seconds in the first half of 2017"):
        LeapSeconds.during(2017, false)
      .assert(_ == 37)

      test(m"There are 37 leap seconds in the first half of 2100"):
        LeapSeconds.during(2100, false)
      .assert(_ == 37)

    suite(m"Gregorian Calendar Tests"):
      test(m"2000 is a leap year"):
        calendars.gregorian.leapYear(2000)
      .assert(_ == true)

      test(m"1800, 1900, 2100, 2200 are not leap years"):
        List(1800, 1900, 2100, 2200).map(calendars.gregorian.leapYear)
      .assert(_.all(!_))

      test(m"Years not divisble by 4 are never leap years"):
        List(1985, 2001, 2023, 1843).map(calendars.gregorian.leapYear)
      .assert(_.all(!_))

      test(m"Check recent Julian Day"):
        (2022-Dec-16).julianDay
      .assert(_ == 2459930)

      test(m"Check Julian Day in 1950"):
        (1950-Mar-10).julianDay
      .assert(_ == 2433351)

      test(m"Check Julian Day in 1650"):
        (1650-Mar-10).julianDay
      .assert(_ == 2323779)

      test(m"Check Julian Day in Year 1600"):
        (1600-Jan-1).julianDay
      .assert(_ == 2305449)

      test(m"Check Julian Day in Year 1"):
        (1-Jan-1).julianDay
      .assert(_ == 1721426)

      test(m"Get zeroth day of year"):
        (2010-Jan-1).julianDay -> (calendars.gregorian.zerothDayOfYear(2010).julianDay + 1)
      .assert(_ == _)

      test(m"Get days in non-leap-year"):
        calendars.gregorian.daysInYear(1995)
      .assert(_ == 365)

      test(m"Get days in leap-year"):
        calendars.gregorian.daysInYear(1996)
      .assert(_ == 366)

      test(m"Get Year from Date"):
        given calendar: Calendar = calendars.gregorian
        val date = 2016-Jul-11
        calendar.getYear(date)
      .assert(_ == 2016)

      test(m"Check Gregorian date"):
        given calendar: Calendar = calendars.gregorian
        2016-Apr-11
      .assert(_ == 2016-Apr-11)

      test(m"Get Gregorian date"):
        given calendar: Calendar = calendars.gregorian
        val date = 2016-Jul-11
        (date.year, date.month, date.day)
      .assert(_ == (2016, Jul, 11))

      test(m"Add two periods"):
        val period = 1.day + 2.months
        val period2 = 3.days + 1.year
        period + period2
      .assert(_ == 4.days + 2.months + 1.year)

      test(m"Simplify a period"):
        (8.months + 6.months).simplify
      .assert(_ == 1.year + 2.months)

      test(m"Hours do not simplify"):
        (1.day + 25.hours).simplify
      .assert(_ == 25.hours + 1.day)

      test(m"Minutes simplify"):
        123.minutes.simplify
      .assert(_ == 2.hours + 3.minutes)

      test(m"Seconds simplify"):
        123.seconds.simplify
      .assert(_ == 2.minutes + 3.seconds)

      test(m"Cascading simplification"):
        (1.hour + 59.minutes + 59.seconds + 2.seconds).simplify
      .assert(_ == 2.hours + 1.second)

      test(m"Simple multiplication"):
        (1.hour + 5.minutes)*100
      .assert(_ == 100.hours + 500.minutes)

      test(m"Simplified multiplication"):
        ((1.hour + 5.seconds)*100).simplify
      .assert(_ == 100.hours + 8.minutes + 20.seconds)

      test(m"Specify times"):
        2.01.am
      .assert(_ == Clockface(2, 1, 0))

      test(m"Specify times 2"):
        2.59.am
      .assert(_ == Clockface(2, 59, 0))

      test(m"Specify times 3"):
        11.40.am
      .assert(_ == Clockface(11, 40, 0))

      test(m"Specify times 4"):
        7.25.pm
      .assert(_ == Clockface(19, 25, 0))

      import calendars.gregorian
      test(m"Specify datetime"):
        2018-Aug-11 at 5.25.pm
      .assert(_ == Timestamp(Date(2018, Aug, 11), Clockface(17, 25, 0)))

      test(m"Add two months to a date"):
        2014-Nov-20 + 2.months
      .assert(_ == 2015-Jan-20)

      test(m"Add two days to a date"):
        2014-Nov-20 + 2.days
      .assert(_ == 2014-Nov-22)

      test(m"Add one year to a date"):
        2014-Nov-20 + 1.year
      .assert(_ == 2015-Nov-20)

      test(m"Add two years to a date"):
        2014-Nov-20 + 2.years
      .assert(_ == 2016-Nov-20)

      test(m"Add three years to a date"):
        2014-Nov-20 + 3.years
      .assert(_ == 2017-Nov-20)

      // test(m"Read TZDB file"):
      //   Tzdb.parseFile(t"europe")
      // .assert(_ == List())
