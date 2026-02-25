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
package aviation

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

import autopsies.contrastExpectations

object Tests extends Suite(m"Aviation Tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      test(m"Parse a canonical date"):
        t"2011-12-13".decode[Date]
      .check(_ == 2011-Dec-13)

      test(m"Parse a date with a single-digit month"):
        t"2011-09-13".decode[Date]
      . assert(_ == 2011-Sep-13)

      test(m"Parse a date in the distant past"):
        t"59-09-13".decode[Date]
      . assert(_ == 59-Sep-13)

      test(m"Month cannot be higher than 12"):
        capture(t"59-13-13".decode[Date])
      . assert(_ == TimeError(_.Unknown(t"13", t"month")))

      test(m"Month cannot be less than 1"):
        capture(t"59-0-13".decode[Date])
      . assert(_ == TimeError(_.Unknown(t"0", t"month")))

      test(m"Day cannot be over 31"):
        capture(t"59-11-32".decode[Date])
      . assert(_ == TimeError(_.Invalid(59, 11, 32, calendars.gregorian)))

      test(m"Day must exist in month"):
        capture(t"59-11-31".decode[Date])
      . assert(_ == TimeError(_.Invalid(59, 11, 31, calendars.gregorian)))


    suite(m"Leap Seconds tests"):
      test(m"There are 10 leap seconds for any year before 1972"):
        LeapSeconds.during(Year(1900), false)
      . assert(_ == 10)

      test(m"There are 10 leap seconds in the first half of 1972"):
        LeapSeconds.during(Year(1972), false)
      . assert(_ == 10)

      test(m"There are 11 leap seconds in the second half of 1972"):
        LeapSeconds.during(Year(1972), true)
      . assert(_ == 11)

      test(m"There are 12 leap seconds in the first half of 1973"):
        LeapSeconds.during(Year(1973), false)
      . assert(_ == 12)

      test(m"There are 12 leap seconds in the second half of 1973"):
        LeapSeconds.during(Year(1973), true)
      . assert(_ == 12)

      test(m"There are 13 leap seconds in the first half of 1974"):
        LeapSeconds.during(Year(1974), false)
      . assert(_ == 13)

      test(m"There are 13 leap seconds in the second half of 1974"):
        LeapSeconds.during(Year(1974), true)
      . assert(_ == 13)

      test(m"There are 19 leap seconds in the first half of 1980"):
        LeapSeconds.during(Year(1980), false)
      . assert(_ == 19)

      test(m"There are 19 leap seconds in the first half of 1981"):
        LeapSeconds.during(Year(1981), false)
      . assert(_ == 19)

      test(m"There are 20 leap seconds in the second half of 1981"):
        LeapSeconds.during(Year(1981), true)
      . assert(_ == 20)

      test(m"There are 20 leap seconds in the first half of 1982"):
        LeapSeconds.during(Year(1982), false)
      . assert(_ == 20)

      test(m"There are 21 leap seconds in the second half of 1982"):
        LeapSeconds.during(Year(1982), true)
      . assert(_ == 21)

      test(m"There are 24 leap seconds in the second half of 1988"):
        LeapSeconds.during(Year(1988), true)
      . assert(_ == 24)

      test(m"There are 32 leap seconds in the first half of 2000"):
        LeapSeconds.during(Year(2000), false)
      . assert(_ == 32)

      test(m"There are 37 leap seconds in the first half of 2017"):
        LeapSeconds.during(Year(2017), false)
      . assert(_ == 37)

      test(m"There are 37 leap seconds in the first half of 2100"):
        LeapSeconds.during(Year(2100), false)
      . assert(_ == 37)

    suite(m"Gregorian Calendar Tests"):
      test(m"2000 is a leap year"):
        calendars.gregorian.leapYear(Year(2000))
      . assert(_ == true)

      test(m"1800, 1900, 2100, 2200 are not leap years"):
        List(Year(1800), Year(1900), Year(2100), Year(2200)).map(calendars.gregorian.leapYear)
      . assert(_.all(!_))

      test(m"Years not divisble by 4 are never leap years"):
        List(Year(1985), Year(200), Year(202), Year(1843)).map(calendars.gregorian.leapYear)
      . assert(_.all(!_))

      test(m"Check recent Julian Day"):
        (2022-Dec-16).jdn
      . assert(_ == 2459930)

      test(m"Check Julian Day in 1950"):
        (1950-Mar-10).jdn
      . assert(_ == 2433351)

      test(m"Check Julian Day in 1650"):
        (1650-Mar-10).jdn
      . assert(_ == 2323779)

      test(m"Check Julian Day in Year 1582"):
        (1582-Oct-15).jdn
      . assert(_ == 2299161)

      test(m"Check Julian Day in Year 1600"):
        (1600-Jan-1).jdn
      . assert(_ == 2305448)

      test(m"Check Julian Day in Year 1"):
        (1-Jan-1).jdn
      . assert(_ == 1721426)

      test(m"Get zeroth day of year"):
        (2010-Jan-1).jdn -> (calendars.gregorian.zerothDayOfYear(Year(2010)).jdn + 1)
      . assert(_ == _)

      test(m"Get days in non-leap-year"):
        calendars.gregorian.daysInYear(Year(1995))
      . assert(_ == 365)

      test(m"Get days in leap-year"):
        calendars.gregorian.daysInYear(Year(1996))
      . assert(_ == 366)

      test(m"Get Year from Date"):
        given calendar: Calendar = calendars.gregorian
        val date = 2016-Jul-11
        calendar.annual(date)
      . assert(_ == 2016)

      test(m"Check Gregorian date"):
        given calendar: Calendar = calendars.gregorian
        2016-Apr-11
      . assert(_ == 2016-Apr-11)

      test(m"Subtract dates"):
        2018-Nov-19 - (2017-Sep-1)
      . assert(_ == Quanta[Mono[Days[1]]](444))

      test(m"Subtract days from a date"):
        2018-Nov-19 - Quanta[Mono[Days[1]]](2)
      . assert(_ == 2018-Nov-17)

      test(m"Add days to a date"):
        2018-Nov-19 + Quanta[Mono[Days[1]]](2)
      . assert(_ == 2018-Nov-21)

      test(m"Add days to a date, order reversed"):
        Quanta[Mono[Days[1]]](2) + (2018-Nov-19)
      . assert(_ == 2018-Nov-21)

      test(m"Get Gregorian date"):
        given calendar: Calendar = calendars.gregorian
        val date = 2016-Jul-11
        (date.year, date.month, date.day)
      . assert(_ == (2016, Jul, 11))

      test(m"Add two periods"):
        val period = 1.days + 2.months
        val period2 = 3.days + 1.years
        period + period2
      . assert(_ == 4.days + 2.months + 1.years)

      test(m"Simplify a period"):
        (8.months + 6.months).simplify
      . assert(_ == 1.years + 2.months)

      test(m"Hours do not simplify"):
        (1.days + 25.hours).simplify
      . assert(_ == 25.hours + 1.days)

      test(m"Minutes simplify"):
        123.minutes.simplify
      . assert(_ == 2.hours + 3.minutes)

      test(m"Seconds simplify"):
        123.seconds.simplify
      . assert(_ == 2.minutes + 3.seconds)

      test(m"Cascading simplification"):
        (1.hours + 59.minutes + 59.seconds + 2.seconds).simplify
      . assert(_ == 2.hours + 1.seconds)

      test(m"Simple multiplication"):
        (1.hours + 5.minutes)*100
      . assert(_ == 100.hours + 500.minutes)

      test(m"Simplified multiplication"):
        ((1.hours + 5.seconds)*100).simplify
      . assert(_ == 100.hours + 8.minutes + 20.seconds)

      test(m"Specify times"):
        2.01.am
      . assert(_ == Clockface(2, 1, 0))

      test(m"Specify times 2"):
        2.59.am
      . assert(_ == Clockface(2, 59, 0))

      test(m"Specify times 3"):
        11.40.am
      . assert(_ == Clockface(11, 40, 0))

      test(m"Specify times 4"):
        7.25.pm
      . assert(_ == Clockface(19, 25, 0))

      test(m"Time with too many minutes is invalid"):
        demilitarize(7.88.am)
      . assert(_.nonEmpty)

      test(m"Time with too few minute digits is invalid"):
        demilitarize(7.8.am)
      . assert(_.nonEmpty)

      test(m"Time with too many minute digits is invalid"):
        demilitarize(7.585.am)
      . assert(_.nonEmpty)

      test(m"Specify afternoon time with hour `12`"):
        12.25.pm
      . assert(_ == Clockface(12, 25, 0))

      test(m"Specify morning time with hour `12`"):
        12.25.am
      . assert(_ == Clockface(0, 25, 0))

      import calendars.gregorian

      test(m"Specify datetime"):
        2018-Aug-11 at 5.25.pm
      . assert(_ == Timestamp(Date(Year(2018), Aug, Day(11)), Clockface(17, 25, 0)))

      test(m"Add two months to a date"):
        2014-Nov-20 + 2.months
      . assert(_ == 2015-Jan-20)

      test(m"Add two days to a date"):
        2014-Nov-20 + 2.days
      . assert(_ == 2014-Nov-22)

      test(m"Add one year to a date"):
        2014-Nov-20 + 1.years
      . assert(_ == 2015-Nov-20)

      test(m"Add two years to a date"):
        2014-Nov-20 + 2.years
      . assert(_ == 2016-Nov-20)

      test(m"Add three years to a date"):
        2014-Nov-20 + 3.years
      . assert(_ == 2017-Nov-20)

      // test(m"Read TZDB file"):
      //   Tzdb.parseFile(t"europe")
      // .assert(_ == List())

    suite(m"Decoding instants"):
      suite(m"ISO 8601"):
        import instantDecodables.iso8601
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

        test(m"Month-only format"):
          t"2012-11".decode[Instant]
        . assert(_ == Instant(1351728000000L))

        test(m"ISO 8601 with timezone offset and fractional seconds"):
          t"2023-03-25T10:15:30.456+02:00".decode[Instant]
        . assert(_ == Instant(1679732130456L))

        test(m"Calendar date, full time, Z"):
          t"2023-05-28T14:30:59Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Calendar date, basic format, full time, Z"):
          t"20230528T143059Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Calendar date, full time, offset +00:00"):
          t"2023-05-28T14:30:59+00:00".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Calendar date, full time, offset +02:00"):
          t"2023-05-28T14:30:59+02:00".decode[Instant]
        . assert(_ == Instant(1685277059000L))

        test(m"Calendar date, full time, offset -05:00"):
          t"2023-05-28T14:30:59-05:00".decode[Instant]
        . assert(_ == Instant(1685302259000L))

        test(m"Calendar date, fractional seconds, Z"):
          t"2023-05-28T14:30:59.123Z".decode[Instant]
        . assert(_ == Instant(1685284259123L))

        test(m"Calendar date, comma as decimal separator, Z"):
          t"2023-05-28T14:30:59,123Z".decode[Instant]
        . assert(_ == Instant(1685284259123L))

        test(m"Calendar date, hours and minutes, Z"):
          t"2023-05-28T14:30Z".decode[Instant]
        . assert(_ == Instant(1685284200000L))

        test(m"Calendar date, hour only, Z"):
          t"2023-05-28T14Z".decode[Instant]
        . assert(_ == Instant(1685282400000L))

        test(m"Week date, Sunday of week 21, full time, Z"):
          t"2023-W21-7T14:30:59Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Week date, Monday of week 21, full time, Z"):
          t"2023-W21-1T14:30:59Z".decode[Instant]
        . assert(_ == Instant(1684765859000L))

        test(m"Week date, basic format, Sunday of week 21, full time, Z"):
          t"2023W217T143059Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Start of UNIX epoch"):
          t"1970-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(0L))

        test(m"Leap day 2020"):
          t"2020-02-29T12:00:00Z".decode[Instant]
        . assert(_ == Instant(1582977600000L))

        test(m"End of 1999"):
          t"1999-12-31T23:59:59Z".decode[Instant]
        . assert(_ == Instant(946684799000L))

        test(m"Start of 2000"):
          t"2000-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(946684800000L))

        test(m"Far future 2199"):
          t"2199-12-31T23:59:59Z".decode[Instant]
        . assert(_ == Instant(7258118399000L))

        test(m"Far past 1800"):
          t"1800-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(-5364662400000L))

        test(m"Middle of the day"):
          t"2023-06-15T12:00:00Z".decode[Instant]
        . assert(_ == Instant(1686830400000L))

        test(m"New Year 2023"):
          t"2023-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(1672531200000L))

        test(m"DST change irrelevant (UTC)"):
          t"2021-03-28T01:30:00Z".decode[Instant]
        . assert(_ == Instant(1616895000000L))

        test(m"Millisecond rounding test"):
          t"2022-11-15T23:59:59Z".decode[Instant]
        . assert(_ == Instant(1668556799000L))

      suite(m"RFC 1123"):
        import instantDecodables.rfc1123
        test(m"basic with GMT timezone"):
          t"Sun, 06 Nov 1994 08:49:37 GMT".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with unusual day of week (consistency check)"):
          t"Tue, 01 Jan 2019 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1546300800000L))

        test(m"Standard RFC1123 date with full weekday and month names"):
          t"Sun, 06 Nov 1994 08:49:37 GMT".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"Leap day in a leap year"):
          t"Mon, 29 Feb 2016 12:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1456747200000L))

        test(m"Epoch start date"):
          t"Thu, 01 Jan 1970 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(0L))

        test(m"Last second before Y2K"):
          t"Fri, 31 Dec 1999 23:59:59 GMT".decode[Instant]
        . assert(_ == Instant(946684799000L))

        test(m"First second of Y2K"):
          t"Sat, 01 Jan 2000 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(946684800000L))

        test(m"Date with single-digit day"):
          capture(t"Wed, 3 Jul 2002 17:45:00 GMT".decode[Instant])
        . assert(_ == TimeError(_.Format(t"Wed, 3 Jul 2002 17:45:00 GMT", Rfc1123, Prim + 6)(Rfc1123.Issue.Digit)))

        test(m"Date with single-digit hour, minute, and second"):
          t"Tue, 02 Mar 2021 07:08:09 GMT".decode[Instant]
        . assert(_ == Instant(1614668889000L))

        test(m"Date with correct day of week (Monday)"):
          t"Mon, 15 Aug 2022 18:30:00 GMT".decode[Instant]
        . assert(_ == Instant(1660588200000L))

        test(m"Incorrect weekday (should be Sunday, not Monday)"):
          t"Mon, 31 Dec 2006 23:59:59 GMT".decode[Instant]
        . assert(_ == Instant(1167609599000L))

        test(m"Date around DST end (should be in UTC, so no DST effect)"):
          t"Sun, 01 Nov 2020 01:30:00 GMT".decode[Instant]
        . assert(_ == Instant(1604194200000L))

        test(m"Far future date (2100)"):
          t"Fri, 01 Jan 2100 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(4102444800000L))

        test(m"Far past date (1900)"):
          t"Mon, 01 Jan 1900 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(-2208988800000L))

        test(m"Time with 2-digit hour 23"):
          t"Wed, 08 Sep 2021 23:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1631142000000L))

        test(m"Midday exactly (12:00:00)"):
          t"Sat, 25 Dec 2021 12:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1640433600000L))

        test(m"New Year's Eve (2022)"):
          t"Sat, 31 Dec 2022 23:59:59 GMT".decode[Instant]
        . assert(_ == Instant(1672531199000L))

        test(m"Short month name test (Jan)"):
          t"Sun, 01 Jan 2023 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1672531200000L))

        test(m"Minimum valid time (00:00:00)"):
          t"Sun, 10 Oct 2021 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1633824000000L))

      suite(m"Working days tests"):
        suite(m"Counting days")
          test(m"Count the Saturdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Sat)
          . assert(_ == 5)

          test(m"Count the Sundays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Sun)
          . assert(_ == 5)

          test(m"Count the Mondays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Mon)
          . assert(_ == 5)

          test(m"Count the Tuesdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Tue)
          . assert(_ == 4)

          test(m"Count the Wednesdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Wed)
          . assert(_ == 4)

          test(m"Count the Thursdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Thu)
          . assert(_ == 4)

          test(m"Count the Fridays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Fri)
          . assert(_ == 4)

          test(m"Count the Saturdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Sat)
          . assert(_ == 4)

          test(m"Count the Sundays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Sun)
          . assert(_ == 4 )

          test(m"Count the Mondays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Mon)
          . assert(_ == 4)

          test(m"Count the Tuesdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Tue)
          . assert(_ == 5)

          test(m"Count the Wednesdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Wed)
          . assert(_ == 5)

          test(m"Count the Thursdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Thu)
          . assert(_ == 4)

          test(m"Count the Fridays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Fri)
          . assert(_ == 4)

          test(m"Count the Saturdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Sat)
          . assert(_ == 5)

          test(m"Count the Sundays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Sun)
          . assert(_ == 4)

          test(m"Count the Mondays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Mon)
          . assert(_ == 4)

          test(m"Count the Tuesdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Tue)
          . assert(_ == 4)

          test(m"Count the Wednesdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Wed)
          . assert(_ == 4)

          test(m"Count the Thursdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Thu)
          . assert(_ == 5)

          test(m"Count the Fridays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Fri)
          . assert(_ == 5)

        suite(m"UK holidays example"):
          given Holidays = Holidays(List
            ( Holiday(2025-Jan-1, t"New Year's Day"),
              Holiday(2025-Apr-21, t"Good Friday"),
              Holiday(2025-May-5, t"Early May Bank Holiday"),
              Holiday(2025-May-26, t"Spring Bank Holiday"),
              Holiday(2025-Aug-25, t"Summer Bank Holiday"),
              Holiday(2025-Dec-25, t"Christmas Day"),
              Holiday(2025-Dec-26, t"Boxing Day")) )

          test(m"Check the next working day after Monday is Tuesday"):
            import hebdomads.european
            2025-Aug-18 + WorkingDays(1)
          . assert(_ == 2025-Aug-19)

          test(m"Check two working days after a Monday is Wednesday"):
            import hebdomads.european
            2025-Aug-18 + WorkingDays(2)
          . assert(_ == 2025-Aug-20)

          test(m"Check four working days after a Monday is Friday"):
            import hebdomads.european
            2025-Aug-18 + WorkingDays(4)
          . assert(_ == 2025-Aug-22)

          test(m"Check five working days after a Monday is Monday"):
            import hebdomads.european
            2025-Aug-11 + WorkingDays(5)
          . assert(_ == 2025-Aug-18)

          test(m"Check one working days after a Friday is Monday"):
            import hebdomads.european
            2025-Apr-11 + WorkingDays(1)
          . assert(_ == 2025-Apr-14)

          test(m"Check the working day after Christmas Eve"):
            import hebdomads.european
            2025-Dec-24 + WorkingDays(1)
          . assert(_ == 2025-Dec-29)

          test(m"Check the working day after Maundy Thursday is Easter Monday"):
            import hebdomads.european
            2025-Apr-18 + WorkingDays(1)
          . assert(_ == 2025-Apr-22)

          test(m"Working day after Good Friday is Easter Tuesday"):
            import hebdomads.european
            2025-Apr-19 + WorkingDays(1)
          . assert(_ == 2025-Apr-23)

          test(m"Check there are 253 working days in a year"):
            import hebdomads.european
            2025-Jan-3 + WorkingDays(253)
          . assert(_ == 2026-Jan-1)
