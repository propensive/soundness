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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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
        LeapSeconds.during(Year(1900), false)
      .assert(_ == 10)

      test(m"There are 10 leap seconds in the first half of 1972"):
        LeapSeconds.during(Year(1972), false)
      .assert(_ == 10)

      test(m"There are 11 leap seconds in the second half of 1972"):
        LeapSeconds.during(Year(1972), true)
      .assert(_ == 11)

      test(m"There are 12 leap seconds in the first half of 1973"):
        LeapSeconds.during(Year(1973), false)
      .assert(_ == 12)

      test(m"There are 12 leap seconds in the second half of 1973"):
        LeapSeconds.during(Year(1973), true)
      .assert(_ == 12)

      test(m"There are 13 leap seconds in the first half of 1974"):
        LeapSeconds.during(Year(1974), false)
      .assert(_ == 13)

      test(m"There are 13 leap seconds in the second half of 1974"):
        LeapSeconds.during(Year(1974), true)
      .assert(_ == 13)

      test(m"There are 19 leap seconds in the first half of 1980"):
        LeapSeconds.during(Year(1980), false)
      .assert(_ == 19)

      test(m"There are 19 leap seconds in the first half of 1981"):
        LeapSeconds.during(Year(1981), false)
      .assert(_ == 19)

      test(m"There are 20 leap seconds in the second half of 1981"):
        LeapSeconds.during(Year(1981), true)
      .assert(_ == 20)

      test(m"There are 19 leap seconds in the first half of 1981"):
        LeapSeconds.during(Year(1981), false)
      .assert(_ == 19)

      test(m"There are 20 leap seconds in the second half of 1981"):
        LeapSeconds.during(Year(1981), true)
      .assert(_ == 20)

      test(m"There are 20 leap seconds in the first half of 1982"):
        LeapSeconds.during(Year(1982), false)
      .assert(_ == 20)

      test(m"There are 21 leap seconds in the second half of 1982"):
        LeapSeconds.during(Year(1982), true)
      .assert(_ == 21)

      test(m"There are 24 leap seconds in the second half of 1988"):
        LeapSeconds.during(Year(1988), true)
      .assert(_ == 24)

      test(m"There are 32 leap seconds in the first half of 2000"):
        LeapSeconds.during(Year(2000), false)
      .assert(_ == 32)

      test(m"There are 37 leap seconds in the first half of 2017"):
        LeapSeconds.during(Year(2017), false)
      .assert(_ == 37)

      test(m"There are 37 leap seconds in the first half of 2100"):
        LeapSeconds.during(Year(2100), false)
      .assert(_ == 37)

    suite(m"Gregorian Calendar Tests"):
      test(m"2000 is a leap year"):
        calendars.gregorian.leapYear(Year(2000))
      .assert(_ == true)

      test(m"1800, 1900, 2100, 2200 are not leap years"):
        List(Year(1800), Year(1900), Year(2100), Year(2200)).map(calendars.gregorian.leapYear)
      .assert(_.all(!_))

      test(m"Years not divisble by 4 are never leap years"):
        List(Year(1985), Year(200), Year(202), Year(1843)).map(calendars.gregorian.leapYear)
      .assert(_.all(!_))

      test(m"Check recent Julian Day"):
        (2022-Dec-16).jdn
      .assert(_ == 2459930)

      test(m"Check Julian Day in 1950"):
        (1950-Mar-10).jdn
      .assert(_ == 2433351)

      test(m"Check Julian Day in 1650"):
        (1650-Mar-10).jdn
      .assert(_ == 2323779)

      test(m"Check Julian Day in Year 1582"):
        (1582-Oct-15).jdn
      .assert(_ == 2299161)

      test(m"Check Julian Day in Year 1600"):
        (1600-Jan-1).jdn
      .assert(_ == 2305448)

      test(m"Check Julian Day in Year 1"):
        (1-Jan-1).jdn
      .assert(_ == 1721426)

      test(m"Get zeroth day of year"):
        (2010-Jan-1).jdn -> (calendars.gregorian.zerothDayOfYear(Year(2010)).jdn + 1)
      .assert(_ == _)

      test(m"Get days in non-leap-year"):
        calendars.gregorian.daysInYear(Year(1995))
      .assert(_ == 365)

      test(m"Get days in leap-year"):
        calendars.gregorian.daysInYear(Year(1996))
      .assert(_ == 366)

      test(m"Get Year from Date"):
        given calendar: Calendar = calendars.gregorian
        val date = 2016-Jul-11
        calendar.annual(date)
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
        val period = 1.days + 2.months
        val period2 = 3.days + 1.years
        period + period2
      .assert(_ == 4.days + 2.months + 1.years)

      test(m"Simplify a period"):
        (8.months + 6.months).simplify
      .assert(_ == 1.years + 2.months)

      test(m"Hours do not simplify"):
        (1.days + 25.hours).simplify
      .assert(_ == 25.hours + 1.days)

      test(m"Minutes simplify"):
        123.minutes.simplify
      .assert(_ == 2.hours + 3.minutes)

      test(m"Seconds simplify"):
        123.seconds.simplify
      .assert(_ == 2.minutes + 3.seconds)

      test(m"Cascading simplification"):
        (1.hours + 59.minutes + 59.seconds + 2.seconds).simplify
      .assert(_ == 2.hours + 1.seconds)

      test(m"Simple multiplication"):
        (1.hours + 5.minutes)*100
      .assert(_ == 100.hours + 500.minutes)

      test(m"Simplified multiplication"):
        ((1.hours + 5.seconds)*100).simplify
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
      .assert(_ == Timestamp(Date(Year(2018), Aug, Day(11)), Clockface(17, 25, 0)))

      test(m"Add two months to a date"):
        2014-Nov-20 + 2.months
      .assert(_ == 2015-Jan-20)

      test(m"Add two days to a date"):
        2014-Nov-20 + 2.days
      .assert(_ == 2014-Nov-22)

      test(m"Add one year to a date"):
        2014-Nov-20 + 1.years
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

    suite(m"Decoding instants"):
      suite(m"ISO 8601"):
        import timestampDecoders.iso8601
        test(m"with Z suffix (UTC)"):
          t"1994-11-06T08:49:37Z".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with positive timezone offset"):
          t"1994-11-06T09:49:37+01:00".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with negative timezone offset"):
          t"1994-11-06T03:49:37-05:00".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with fractional seconds (.123)"):
          t"2020-02-29T12:34:56.123Z".decode[Instant]
        . assert(_ == Instant(1582979696123L))

        test(m"with nanosecond precision (.123456789)"):
          t"2020-02-29T12:34:56.123456789Z".decode[Instant]
        . assert(_ == Instant(1582979696123L))

        test(m"date-only format (midnight UTC)"):
          t"2020-12-31".decode[Instant]
        . assert(_ == Instant(1609372800000L))

        // test(m"ISO 8601 leap second accepted as next second"):
        //   t"2016-12-31T23:59:60Z".decode[Instant]
        // . assert(_ == Instant(1483228800000L))

        test(m"ISO 8601 with timezone offset and fractional seconds"):
          t"2023-03-25T10:15:30.456+02:00".decode[Instant]
        . assert(_ == Instant(1679732130456L))

      suite(m"RFC 1123"):
        import timestampDecoders.rfc1123
        test(m"basic with GMT timezone"):
          t"Sun, 06 Nov 1994 08:49:37 GMT".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with unusual day of week (consistency check)"):
          t"Tue, 01 Jan 2019 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1546300800000L))
