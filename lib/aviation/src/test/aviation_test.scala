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
import abstractables.instantIsAbstractable

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

    suite(m"Hebdomad implementations"):
      suite(m"European hebdomad"):
        import hebdomads.european

        test(m"starts on Monday"):
          summon[Hebdomad].start
        . assert(_ == Mon)

        test(m"Saturday is a weekend day"):
          Sat.weekend
        . assert(_ == true)

        test(m"Sunday is a weekend day"):
          Sun.weekend
        . assert(_ == true)

        test(m"Friday is not a weekend day"):
          Fri.weekend
        . assert(_ == false)

        test(m"Monday is the first weekday (ordinal 0)"):
          Mon.number.n0
        . assert(_ == 0)

        test(m"Sunday is the seventh weekday (ordinal 6)"):
          Sun.number.n0
        . assert(_ == 6)

      suite(m"North American hebdomad"):
        import hebdomads.northAmerican

        test(m"starts on Sunday"):
          summon[Hebdomad].start
        . assert(_ == Sun)

        test(m"Saturday is a weekend day"):
          Sat.weekend
        . assert(_ == true)

        test(m"Sunday is the first weekday (ordinal 0)"):
          Sun.number.n0
        . assert(_ == 0)

        test(m"Monday is the second weekday (ordinal 1)"):
          Mon.number.n0
        . assert(_ == 1)

      suite(m"Jewish hebdomad"):
        import hebdomads.jewish

        test(m"starts on Sunday"):
          summon[Hebdomad].start
        . assert(_ == Sun)

        test(m"Friday is a weekend day"):
          Fri.weekend
        . assert(_ == true)

        test(m"Saturday is a weekend day"):
          Sat.weekend
        . assert(_ == true)

        test(m"Sunday is not a weekend day"):
          Sun.weekend
        . assert(_ == false)

    suite(m"Year operations"):
      test(m"Year addition"):
        Year(2024) + 5
      . assert(_ == Year(2029))

      test(m"Year subtraction"):
        Year(2024) - 100
      . assert(_ == Year(1924))

      test(m"Year ordering: less than"):
        Year(1999) < Year(2000)
      . assert(_ == true)

      test(m"Year ordering: greater than"):
        Year(2024) > Year(2023)
      . assert(_ == true)

      test(m"Year ordering: equal"):
        Year(2024) == Year(2024)
      . assert(_ == true)

      test(m"Year decode from text"):
        t"2024".decode[Year]
      . assert(_ == Year(2024))

    suite(m"Month operations"):
      test(m"Month from text: Jan"):
        Month(t"Jan")
      . assert(_ == Jan)

      test(m"Month from text: Dec"):
        Month(t"Dec")
      . assert(_ == Dec)

      test(m"Month from invalid text"):
        capture(Month(t"foo"))
      . assert(_ == TimeError(_.Unknown(t"foo", t"month")))

      test(m"Month from invalid number 0"):
        capture(Month(0))
      . assert(_ == TimeError(_.Unknown(t"0", t"month")))

      test(m"Month from invalid number 13"):
        capture(Month(13))
      . assert(_ == TimeError(_.Unknown(t"13", t"month")))

      test(m"Month.unapply on text matches"):
        t"jan" match
          case Month(m) => m
          case _        => Jan
      . assert(_ == Jan)

      test(m"Month.unapply on int matches"):
        7 match
          case Month(m) => m
          case _        => Jan
      . assert(_ == Jul)

      test(m"Month.unapply rejects invalid int"):
        13 match
          case Month(m) => true
          case _        => false
      . assert(_ == false)

      test(m"Jan.numerical"):
        Jan.numerical
      . assert(_ == 1)

      test(m"Dec.numerical"):
        Dec.numerical
      . assert(_ == 12)

      test(m"Mar.offset(false) == 59"):
        Mar.offset(false)
      . assert(_ == 59)

      test(m"Mar.offset(true) == 60"):
        Mar.offset(true)
      . assert(_ == 60)

      test(m"Jan.offset(true) == 0"):
        Jan.offset(true)
      . assert(_ == 0)

      test(m"Feb.offset(true) == 31"):
        Feb.offset(true)
      . assert(_ == 31)

      test(m"Month.all has 12 entries"):
        Month.all.length
      . assert(_ == 12)

    suite(m"Chronology defaults"):
      test(m"standardTime ambiguousTimes default is Dilate"):
        Chronology.standardTime.ambiguousTimes
      . assert(_ == Chronology.AmbiguousTimes.Dilate)

      test(m"standardTime monthArithmetic default is Scale"):
        Chronology.standardTime.monthArithmetic
      . assert(_ == Chronology.MonthArithmetic.Scale)

      test(m"standardTime leapDayArithmetic default is PreferFeb28"):
        Chronology.standardTime.leapDayArithmetic
      . assert(_ == Chronology.LeapDayArithmetic.PreferFeb28)

      test(m"already simplified span stays the same"):
        (1.years + 2.months + 3.days).simplify
      . assert(_ == 1.years + 2.months + 3.days)

    suite(m"AM/PM literal corners"):
      test(m"12.00.am is midnight"):
        12.00.am
      . assert(_ == Clockface(0, 0, 0))

      test(m"12.00.pm is noon"):
        12.00.pm
      . assert(_ == Clockface(12, 0, 0))

      test(m"0.00.am is midnight"):
        0.00.am
      . assert(_ == Clockface(0, 0, 0))

      test(m"13.00.am is a compile error"):
        demilitarize(13.00.am)
      . assert(_.nonEmpty)

      test(m"hour above 12 is a compile error"):
        demilitarize(15.30.pm)
      . assert(_.nonEmpty)

    suite(m"Anniversary"):
      import calendars.gregorian

      test(m"Mar - 14 constructs an Anniversary"):
        val ann: Anniversary = Mar - 14
        (ann.month, ann.day())
      . assert(_ == (Mar, 14))

      test(m"Anniversary applied to a year produces a Date"):
        given Anniversary.NonexistentLeapDay = calendars.nonexistentLeapDays.roundDown
        (Mar - 14)(Year(2024))
      . assert(_ == 2024-Mar-14)

      test(m"Feb 29 in non-leap year roundDown gives Feb 28"):
        given Anniversary.NonexistentLeapDay = calendars.nonexistentLeapDays.roundDown
        (Feb - 29)(Year(2023))
      . assert(_ == 2023-Feb-28)

      test(m"Feb 29 in non-leap year roundUp gives Mar 1"):
        given Anniversary.NonexistentLeapDay = calendars.nonexistentLeapDays.roundUp
        (Feb - 29)(Year(2023))
      . assert(_ == 2023-Mar-1)

      test(m"Feb 29 in non-leap year raiseErrors raises TimeError"):
        given Anniversary.NonexistentLeapDay = calendars.nonexistentLeapDays.raiseErrors
        capture((Feb - 29)(Year(2023)))
      . matches:
          case TimeError(_) =>

      test(m"Feb 29 in a leap year is unaffected by rounding strategy"):
        given Anniversary.NonexistentLeapDay = calendars.nonexistentLeapDays.roundDown
        (Feb - 29)(Year(2024))
      . assert(_ == 2024-Feb-29)

      test(m"date.anniversary extracts month and day"):
        val ann = (2025-Mar-14).anniversary
        (ann.month, ann.day())
      . assert(_ == (Mar, 14))

      test(m"Anniversary Showable, big-endian dot separator"):
        import dateFormats.{endianness, separators}, endianness.bigEndian, separators.dot
        import dateFormats.months.englishShort
        (Mar - 14).show
      . assert(_ == t"Mar.14")

      test(m"Anniversary Showable, little-endian dot separator"):
        import dateFormats.{endianness, separators}, endianness.littleEndian, separators.dot
        import dateFormats.months.englishShort
        (Mar - 14).show
      . assert(_ == t"14.Mar")

    suite(m"Monthstamp"):
      import calendars.gregorian

      test(m"2024 - Jan constructs a Monthstamp"):
        val ms: Monthstamp = 2024 - Jan
        (ms.year(), ms.month)
      . assert(_ == (2024, Jan))

      test(m"Monthstamp - day produces a Date"):
        (2024 - Jan) - 15
      . assert(_ == 2024-Jan-15)

      test(m"Monthstamp - day for end of month"):
        (2024 - Feb) - 29
      . assert(_ == 2024-Feb-29)

      test(m"date.monthstamp extracts year and month"):
        val ms = (2025-Mar-14).monthstamp
        (ms.year(), ms.month)
      . assert(_ == (2025, Mar))

      test(m"Monthstamp Showable, big-endian dot separator"):
        import dateFormats.{endianness, separators, years}
        import endianness.bigEndian, separators.dot, years.full
        import dateFormats.months.englishShort
        (2024 - Mar).show
      . assert(_ == t"Mar.2024")

      test(m"Monthstamp Showable, little-endian"):
        import dateFormats.{endianness, separators, years}
        import endianness.littleEndian, separators.dot, years.full
        import dateFormats.months.englishShort
        (2024 - Mar).show
      . assert(_ == t"2024.Mar")

    suite(m"Holidays methods"):
      val holidays = Holidays(List
        ( Holiday(2025-Jan-1, t"New Year's Day"),
          Holiday(2025-Apr-21, t"Good Friday"),
          Holiday(2025-Dec-25, t"Christmas Day"),
          Holiday(2025-Dec-26, t"Boxing Day") ))

      test(m"holiday returns the matching Holiday"):
        holidays.holiday(2025-Dec-25).let(_.name).or(t"")
      . assert(_ == t"Christmas Day")

      test(m"holiday on a non-holiday returns Unset"):
        holidays.holiday(2025-Mar-15).absent
      . assert(_ == true)

      test(m"workDay is false on a holiday"):
        holidays.workDay(2025-Jan-1)
      . assert(_ == false)

      test(m"workDay is true on a normal day"):
        holidays.workDay(2025-Mar-15)
      . assert(_ == true)

      test(m"between returns holidays in [start, end)"):
        holidays.between(2025-Apr-1, 2025-Dec-25).map(_.date)
      . assert(_ == List(2025-Apr-21))

      test(m"between excludes the end date"):
        holidays.between(2025-Dec-25, 2025-Dec-26).map(_.date)
      . assert(_ == List(2025-Dec-25))

      test(m"between with empty range"):
        holidays.between(2025-Mar-1, 2025-Apr-1)
      . assert(_ == List())

      test(m"between returns sorted results"):
        holidays.between(2025-Jan-1, 2025-Dec-31).map(_.date)
      . assert(_ == List(2025-Jan-1, 2025-Apr-21, 2025-Dec-25, 2025-Dec-26))

    suite(m"Date Showable formats"):
      val date = 2025-Apr-7

      test(m"european format"):
        import dateFormats.european
        date.show
      . assert(_ == t"07.04.2025")

      test(m"american format"):
        import dateFormats.american
        date.show
      . assert(_ == t"04/07/2025")

      test(m"unitedKingdom format"):
        import dateFormats.unitedKingdom
        date.show
      . assert(_ == t"07/04/2025")

      test(m"southEastAsia format"):
        import dateFormats.southEastAsia
        date.show
      . assert(_ == t"07-04-2025")

      test(m"iso8601 format"):
        import dateFormats.iso8601
        date.show
      . assert(_ == t"2025-04-07")

      test(m"two-digit year variant"):
        import dateFormats.endianness.bigEndian
        import dateFormats.numerics.fixedWidth
        import dateFormats.separators.hyphen
        import dateFormats.years.twoDigits
        date.show
      . assert(_ == t"25-04-07")

      test(m"variable width single-digit month"):
        import dateFormats.endianness.bigEndian
        import dateFormats.numerics.variableWidth
        import dateFormats.separators.hyphen
        import dateFormats.years.full
        date.show
      . assert(_ == t"2025-4-7")

    suite(m"Clockface Showable formats"):
      val time = Clockface(14, 30, 59)
      val noon = Clockface(12, 0, 0)
      val midnight = Clockface(0, 0, 0)

      test(m"military format"):
        import timeFormats.military
        time.show
      . assert(_ == t"1430")

      test(m"civilian format at 14:30"):
        import timeFormats.civilian
        time.show
      . assert(_ == t"02:30 PM")

      test(m"civilian format at noon"):
        import timeFormats.civilian
        noon.show
      . assert(_ == t"12:00 PM")

      test(m"civilian format at midnight"):
        import timeFormats.civilian
        midnight.show
      . assert(_ == t"12:00 AM")

      test(m"associatedPress format"):
        import timeFormats.associatedPress
        time.show
      . assert(_ == t"2:30 p.m.")

      test(m"french format"):
        import timeFormats.french
        time.show
      . assert(_ == t"14h30")

      test(m"iso8601 time format"):
        import timeFormats.iso8601
        time.show
      . assert(_ == t"14:30:59")

      test(m"ledger format"):
        import timeFormats.ledger
        time.show
      . assert(_ == t"14.30")

      test(m"railway format"):
        import timeFormats.railway
        time.show
      . assert(_ == t"14:30")

    suite(m"Weekday name formatters"):
      import dateFormats.weekdays

      test(m"english full names: Mon"):
        weekdays.english.name(Mon)
      . assert(_ == t"Monday")

      test(m"english full names: Sun"):
        weekdays.english.name(Sun)
      . assert(_ == t"Sunday")

      test(m"englishShort: Mon"):
        weekdays.englishShort.name(Mon)
      . assert(_ == t"Mon")

      test(m"englishShort: Sun"):
        weekdays.englishShort.name(Sun)
      . assert(_ == t"Sun")

      test(m"oneLetterAmbiguous: Mon"):
        weekdays.oneLetterAmbiguous.name(Mon)
      . assert(_ == t"M")

      test(m"oneLetterAmbiguous: Tue"):
        weekdays.oneLetterAmbiguous.name(Tue)
      . assert(_ == t"T")

      test(m"shortestUnambiguous: Tue"):
        weekdays.shortestUnambiguous.name(Tue)
      . assert(_ == t"Tu")

      test(m"shortestUnambiguous: Thu"):
        weekdays.shortestUnambiguous.name(Thu)
      . assert(_ == t"Th")

      test(m"twoLetter: Mon"):
        weekdays.twoLetter.name(Mon)
      . assert(_ == t"Mo")

      test(m"twoLetter: Sat"):
        weekdays.twoLetter.name(Sat)
      . assert(_ == t"Sa")

    suite(m"Month name formatters"):
      import dateFormats.months

      test(m"english full names: Jan"):
        months.english.name(Jan)
      . assert(_ == t"January")

      test(m"english full names: Sep"):
        months.english.name(Sep)
      . assert(_ == t"September")

      test(m"englishShort: Jan"):
        months.englishShort.name(Jan)
      . assert(_ == t"Jan")

      test(m"englishShort: Sep"):
        months.englishShort.name(Sep)
      . assert(_ == t"Sep")

      test(m"oneLetterAmbiguous: Jan"):
        months.oneLetterAmbiguous.name(Jan)
      . assert(_ == t"J")

      test(m"oneLetterAmbiguous: Mar"):
        months.oneLetterAmbiguous.name(Mar)
      . assert(_ == t"M")

      test(m"numeric: Jan"):
        months.numeric.name(Jan)
      . assert(_ == t"1")

      test(m"numeric: Dec"):
        months.numeric.name(Dec)
      . assert(_ == t"12")

      test(m"twoDigit: Jan"):
        months.twoDigit.name(Jan)
      . assert(_ == t"01")

      test(m"twoDigit: Dec"):
        months.twoDigit.name(Dec)
      . assert(_ == t"12")

    suite(m"Meridiem formatters"):
      import timeFormats.meridiems

      test(m"upper Am"):
        meridiems.upper.text(Meridiem.Am)
      . assert(_ == t"AM")

      test(m"upper Pm"):
        meridiems.upper.text(Meridiem.Pm)
      . assert(_ == t"PM")

      test(m"lower Am"):
        meridiems.lower.text(Meridiem.Am)
      . assert(_ == t"am")

      test(m"lower Pm"):
        meridiems.lower.text(Meridiem.Pm)
      . assert(_ == t"pm")

      test(m"upperPunctuated Am"):
        meridiems.upperPunctuated.text(Meridiem.Am)
      . assert(_ == t"A.M.")

      test(m"lowerPunctuated Pm"):
        meridiems.lowerPunctuated.text(Meridiem.Pm)
      . assert(_ == t"p.m.")

    suite(m"Date arithmetic edge cases"):
      import calendars.gregorian

      test(m"Subtract a date from itself yields zero"):
        2024-Jul-15 - (2024-Jul-15)
      . assert(_ == Quanta[Mono[Days[1]]](0))

      test(m"Cross-year date difference"):
        2025-Jan-1 - (2024-Jan-1)
      . assert(_ == Quanta[Mono[Days[1]]](366))

      test(m"Date less-than"):
        (2024-Jan-1) < (2024-Jan-2)
      . assert(_ == true)

      test(m"Date greater-than"):
        (2024-Jan-2) > (2024-Jan-1)
      . assert(_ == true)

      test(m"Date less-than-or-equal (equal)"):
        (2024-Jan-1) <= (2024-Jan-1)
      . assert(_ == true)

      test(m"Date greater-than-or-equal (equal)"):
        (2024-Jan-1) >= (2024-Jan-1)
      . assert(_ == true)

      test(m"Adding a negative year"):
        2024-Mar-15 + (-1).years
      . assert(_ == 2023-Mar-15)

      test(m"Subtracting one year via negative Timespan addition"):
        2024-Mar-15 + (-1).years
      . assert(_ == 2023-Mar-15)

      test(m"Date.addDays positive"):
        (2024-Jan-1).addDays(31)
      . assert(_ == 2024-Feb-1)

      test(m"Date.addDays negative"):
        (2024-Feb-1).addDays(-31)
      . assert(_ == 2024-Jan-1)

      test(m"yearDay for Jan 1"):
        (2024-Jan-1).yearDay
      . assert(_ == 1)

      test(m"yearDay for Dec 31 in leap year"):
        (2024-Dec-31).yearDay
      . assert(_ == 366)

      test(m"yearDay for Mar 1 in leap year"):
        (2024-Mar-1).yearDay
      . assert(_ == 61)

      test(m"yearDay for Mar 1 in non-leap year"):
        (2023-Mar-1).yearDay
      . assert(_ == 60)

      test(m"weekday of 2024-Jan-1 is Monday"):
        (2024-Jan-1).weekday
      . assert(_ == Mon)

      test(m"weekday of 2025-Mar-15 is Saturday"):
        (2025-Mar-15).weekday
      . assert(_ == Sat)

      test(m"date is on weekend"):
        import hebdomads.european
        (2025-Mar-15).weekend
      . assert(_ == true)

      test(m"date is not on weekend"):
        import hebdomads.european
        (2025-Mar-17).weekend
      . assert(_ == false)

      test(m"Adding 1 year to Feb 29 in a leap year currently panics"):
        try
          val _ = 2024-Feb-29 + 1.years
          t"no error"
        catch case error: Throwable => t"threw ${error.getClass.getSimpleName.nn}"
      . assert(_.starts(t"threw"))

      test(m"Adding 1 month to Jan 31 currently panics"):
        try
          val _ = 2024-Jan-31 + 1.months
          t"no error"
        catch case error: Throwable => t"threw ${error.getClass.getSimpleName.nn}"
      . assert(_.starts(t"threw"))

    suite(m"WorkingDays edge cases"):
      given Holidays = Holidays(List
        ( Holiday(2025-Jan-1, t"New Year's Day"),
          Holiday(2025-Dec-25, t"Christmas Day"),
          Holiday(2025-Dec-26, t"Boxing Day") ))

      test(m"WorkingDays(0) on a Saturday bumps to Monday"):
        import hebdomads.european
        2025-Mar-15 + WorkingDays(0)
      . assert(_ == 2025-Mar-17)

      test(m"WorkingDays(0) on a Sunday bumps to Monday"):
        import hebdomads.european
        2025-Mar-16 + WorkingDays(0)
      . assert(_ == 2025-Mar-17)

      test(m"WorkingDays(0) on a holiday bumps to next working day"):
        import hebdomads.european
        2025-Jan-1 + WorkingDays(0)
      . assert(_ == 2025-Jan-2)

      test(m"WorkingDays(0) on a normal weekday is the same day"):
        import hebdomads.european
        2025-Mar-17 + WorkingDays(0)
      . assert(_ == 2025-Mar-17)

      test(m"WorkingDays across consecutive holidays"):
        import hebdomads.european
        2025-Dec-24 + WorkingDays(2)
      . assert(_ == 2025-Dec-30)

    suite(m"ISO 8601 parse error cases"):
      import instantDecodables.iso8601

      test(m"Non-digit at year start raises"):
        capture(t"abcd-01-01".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Truncated year raises"):
        capture(t"20".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Trailing junk after seconds raises"):
        capture(t"2024-01-01T12:00:00garbage".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Bad week-date letter raises"):
        capture(t"2024-X21-1".decode[Instant])
      . matches:
          case _: TimeError =>

    suite(m"RFC 1123 parse error cases"):
      import instantDecodables.rfc1123

      test(m"Lowercase day name raises"):
        capture(t"sun, 06 Nov 1994 08:49:37 GMT".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Wrong month abbreviation raises"):
        capture(t"Sun, 06 Jux 1994 08:49:37 GMT".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Missing trailing GMT raises"):
        capture(t"Sun, 06 Nov 1994 08:49:37".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Truncated input raises"):
        capture(t"Sun, ".decode[Instant])
      . matches:
          case _: TimeError =>

    suite(m"Instant arithmetic"):
      test(m"Instant + Hour quantity"):
        Instant(0L) + 1*Hour
      . assert(_ == Instant(3600000L))

      test(m"Instant + Minute quantity"):
        Instant(0L) + 30*Minute
      . assert(_ == Instant(1800000L))

      test(m"Instant + Second quantity"):
        Instant(0L) + 5*Second
      . assert(_ == Instant(5000L))

      test(m"Instant - Instant gives Duration in seconds"):
        val d: Quantity[Seconds[1]] = Instant(3600000L) - Instant(0L)
        d.value
      . assert(_ == 3600.0)

      test(m"Instant - Quantity gives Instant"):
        Instant(3600000L) - 1*Hour
      . assert(_ == Instant(0L))

      test(m"Instant ordering: less than"):
        Instant(0L) < Instant(1L)
      . assert(_ == true)

      test(m"Instant ordering: greater than or equal"):
        Instant(1L) >= Instant(1L)
      . assert(_ == true)

      test(m"Instant.Min less than Instant.Max"):
        Instant.Min < Instant.Max
      . assert(_ == true)

      test(m"Instant.long round-trip"):
        Instant(12345L).long
      . assert(_ == 12345L)

    suite(m"Duration construction"):
      test(m"Duration(1000L) is one second"):
        Duration(1000L).value
      . assert(_ == 1.0)

      test(m"Duration(0L) is zero"):
        Duration(0L).value
      . assert(_ == 0.0)

      test(m"Duration round-trip via 60_000 ms is 60 s"):
        Duration(60_000L).value
      . assert(_ == 60.0)

    suite(m"Timespan"):
      test(m"1.years sets only the years field"):
        val ts = 1.years
        (ts.years, ts.months, ts.days, ts.hours, ts.minutes, ts.seconds)
      . assert(_ == (1, 0, 0, 0, 0, 0))

      test(m"3.days sets only the days field"):
        val ts = 3.days
        (ts.years, ts.months, ts.days, ts.hours, ts.minutes, ts.seconds)
      . assert(_ == (0, 0, 3, 0, 0, 0))

      test(m"4.hours sets only the hours field"):
        val ts = 4.hours
        (ts.years, ts.months, ts.days, ts.hours, ts.minutes, ts.seconds)
      . assert(_ == (0, 0, 0, 4, 0, 0))

      test(m"6.seconds sets only the seconds field"):
        val ts = 6.seconds
        (ts.years, ts.months, ts.days, ts.hours, ts.minutes, ts.seconds)
      . assert(_ == (0, 0, 0, 0, 0, 6))

      test(m"Timespan subtraction"):
        (2.years + 3.months) - (1.years + 1.months)
      . assert(_ == 1.years + 2.months)

      test(m"Adding Timespans field-by-field"):
        2.hours + 30.minutes + 15.seconds
      . assert(_ == Timespan(0, 0, 0, 2, 30, 15))

      test(m"Timespan multiplication preserves all fields"):
        ((1.years + 2.months + 3.days)*5).simplify
      . assert(_ == 5.years + 10.months + 15.days)

      test(m"Timespan round-trip via Long nanoseconds drops Y/M/D fields"):
        val original = 1.years + 2.months + 3.days + 4.hours + 5.minutes + 6.seconds
        val nanos = original.generic
        val roundTripped = summon[Timespan is Instantiable across Durations from Long].apply(nanos)
        (roundTripped.years, roundTripped.months, roundTripped.days)
      . assert(_ == (0, 0, 0))

    suite(m"Clock"):
      test(m"Clock.fixed returns the same instant"):
        val clock = Clock.fixed(Instant(12345L))
        (clock(), clock())
      . assert(_ == (Instant(12345L), Instant(12345L)))

      test(m"now() uses the implicit clock"):
        given Clock = Clock.fixed(Instant(99999L))
        now()
      . assert(_ == Instant(99999L))

      test(m"today() with fixed clock and UTC"):
        given Clock = Clock.fixed(Instant(0L))
        given Timezone = tz"UTC"
        given calendar: RomanCalendar = calendars.gregorian
        today()
      . assert(_ == 1970-Jan-1)

      test(m"Clock.offset reflects an offset"):
        val before = java.lang.System.currentTimeMillis
        val clock = Clock.offset(60*Second)
        val result: Instant = clock()
        val after = java.lang.System.currentTimeMillis
        result.long >= (before + 60_000L) && result.long <= (after + 60_001L)
      . assert(_ == true)

    suite(m"Period operations"):
      test(m"instant ~ instant constructs a Period"):
        val period = Instant(0L) ~ Instant(1000L)
        (period.start.long, period.finish.long)
      . assert(_ == (0L, 1000L))

      test(m"Period.duration returns the time difference"):
        val period = Instant(0L) ~ Instant(3_600_000L)
        period.duration.value
      . assert(_ == 3600.0)

      test(m"Period.duration is zero for zero-length period"):
        val period = Instant(1000L) ~ Instant(1000L)
        period.duration.value
      . assert(_ == 0.0)

      test(m"Period constructed via Period(start, finish)"):
        val duration: Duration = 3600*Second
        val period = Period(Instant(0L), Instant(0L) + duration)
        (period.start.long, period.finish.long)
      . assert(_ == (0L, 3_600_000L))

      test(m"Period.intersect of identical periods"):
        val a = Instant(0L) ~ Instant(1000L)
        a.intersect(a).let { p => (p.start.long, p.finish.long) }
      . assert(_ == (0L, 1000L))

      test(m"Period.intersect of disjoint periods is Unset"):
        val a = Instant(0L) ~ Instant(1000L)
        val b = Instant(2000L) ~ Instant(3000L)
        a.intersect(b)
      . assert(_ == Unset)

      test(m"Period.intersect with partial overlap"):
        val a = Instant(0L) ~ Instant(2000L)
        val b = Instant(1000L) ~ Instant(3000L)
        a.intersect(b).let { p => (p.start.long, p.finish.long) }
      . assert(_ == (1000L, 2000L))

      test(m"Period.intersect when one fully contains the other"):
        val a = Instant(0L) ~ Instant(3000L)
        val b = Instant(1000L) ~ Instant(2000L)
        a.intersect(b).let { p => (p.start.long, p.finish.long) }
      . assert(_ == (1000L, 2000L))

      test(m"Period.intersect at exact boundary returns Unset"):
        val a = Instant(0L) ~ Instant(1000L)
        val b = Instant(1000L) ~ Instant(2000L)
        a.intersect(b)
      . assert(_ == Unset)

      test(m"Period.union of overlapping periods is one period"):
        val a = Instant(0L) ~ Instant(2000L)
        val b = Instant(1000L) ~ Instant(3000L)
        a.union(b).map { p => (p.start.long, p.finish.long) }
      . assert(_ == Set((0L, 3000L)))

      test(m"Period.union of disjoint periods is two periods"):
        val a = Instant(0L) ~ Instant(1000L)
        val b = Instant(2000L) ~ Instant(3000L)
        a.union(b).size
      . assert(_ == 2)

    suite(m"Moment and Timezone"):
      import calendars.gregorian

      test(m"Instant 0 in UTC has the correct date and time fields"):
        val moment = Instant(0L).in(tz"UTC")
        (moment.date, moment.time)
      . assert(_ == (1970-Jan-1, Clockface(0, 0, 0)))

      test(m"Moment.instant round-trips an instant in UTC"):
        val original = Instant(1_700_000_000_000L)
        original.in(tz"UTC").instant
      . assert(_ == Instant(1_700_000_000_000L))

      test(m"Moment.timestamp drops the timezone"):
        Instant(0L).in(tz"UTC").timestamp
      . assert(_ == Timestamp(1970-Jan-1, Clockface(0, 0, 0)))

      test(m"Same instant in London during winter is GMT (offset 0)"):
        val winter = t"2024-01-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601)
        winter.in(tz"Europe/London").time.hour
      . assert(_ == 12)

      test(m"Same instant in London during summer is BST (offset +1)"):
        val summer = t"2024-07-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601)
        summer.in(tz"Europe/London").time.hour
      . assert(_ == 13)

      test(m"New York winter offset (UTC-5)"):
        val winter = t"2024-01-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601)
        winter.in(tz"America/New_York").time.hour
      . assert(_ == 7)

      test(m"New York summer offset (UTC-4)"):
        val summer = t"2024-07-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601)
        summer.in(tz"America/New_York").time.hour
      . assert(_ == 8)

      test(m"Timestamp.instant round-trips through London"):
        given Timezone = tz"Europe/London"
        val ts = Timestamp(2024-Jan-15, Clockface(12, 0, 0))
        ts.instant.in(tz"Europe/London").time.hour
      . assert(_ == 12)

      test(m"Timezone(t\"NotARealZone\") raises TimezoneError"):
        capture(Timezone(t"NotARealZone"))
      . matches:
          case _: TimezoneError =>

      test(m"tz\"NotARealZone\" is a compile error"):
        demilitarize(tz"NotARealZone")
      . assert(_.nonEmpty)

    suite(m"TaiInstant and leap-second conversion"):
      test(m"Instant 0 (UNIX epoch) has 10-second TAI offset"):
        LeapSeconds.tai(0L) - 0L
      . assert(_ == 10_000L)

      test(m"Just before Jan 1 1973 (after June 1972 leap) has +11 offset"):
        val justBefore = 94694399999L
        LeapSeconds.tai(justBefore) - justBefore
      . assert(_ == 11_000L)

      test(m"Just after Jan 1 1973 (after Dec 1972 leap) has +12 offset"):
        val justAfter = 94694400001L
        LeapSeconds.tai(justAfter) - justAfter
      . assert(_ == 12_000L)

      test(m"Instant in 2017 has +37 offset"):
        val later = t"2017-06-15T00:00:00Z".decode[Instant](using instantDecodables.iso8601)
        LeapSeconds.tai(later.long) - later.long
      . assert(_ == 37_000L)

    suite(m"Horology sexagesimal"):
      val horology = Horology.sexagesimal

      test(m"addPrimary adds hours"):
        horology.addPrimary(Clockface(0, 0, 0), Base24(5))
      . assert(_ == Clockface(5, 0, 0))

      test(m"addSecondary adds minutes within an hour"):
        horology.addSecondary(Clockface(0, 30, 0), Base60(15))
      . assert(_ == Clockface(0, 45, 0))

      test(m"addSecondary carries minutes into hours"):
        horology.addSecondary(Clockface(0, 30, 0), Base60(45))
      . assert(_ == Clockface(1, 15, 0))

      test(m"addSecondary day-overflow wraps via Base24 modulo"):
        horology.addSecondary(Clockface(23, 30, 0), Base60(45))
      . assert(_ == Clockface(0, 15, 0))

      test(m"addTertiary adds seconds within a minute"):
        horology.addTertiary(Clockface(0, 0, 30), Base60(20))
      . assert(_ == Clockface(0, 0, 50))

      test(m"addTertiary carries seconds into minutes"):
        horology.addTertiary(Clockface(0, 0, 45), Base60(30))
      . assert(_ == Clockface(0, 1, 15))

      test(m"addTertiary cascading carry into hours"):
        horology.addTertiary(Clockface(0, 59, 59), Base60(2))
      . assert(_ == Clockface(1, 0, 1))

    suite(m"Julian calendar"):
      test(m"Year 1900 is a leap year in the Julian calendar"):
        calendars.julian.leapYear(Year(1900))
      . assert(_ == true)

      test(m"Year 2000 is a leap year in the Julian calendar"):
        calendars.julian.leapYear(Year(2000))
      . assert(_ == true)

      test(m"Year 1901 is not a leap year"):
        calendars.julian.leapYear(Year(1901))
      . assert(_ == false)

      test(m"Year 1900 has 366 days under the Julian calendar"):
        calendars.julian.daysInYear(Year(1900))
      . assert(_ == 366)

    suite(m"Timestamp"):
      import calendars.gregorian

      test(m"Timestamp Showable produces 'time, date'"):
        import dateFormats.iso8601
        import timeFormats.iso8601
        Timestamp(2024-Jan-15, Clockface(14, 30, 59)).show
      . assert(_ == t"14:30:59, 2024-01-15")

      test(m"Timestamp.year accessor"):
        Timestamp(2024-Mar-15, Clockface(0, 0, 0)).year
      . assert(_ == Year(2024))

      test(m"Timestamp.month accessor"):
        Timestamp(2024-Mar-15, Clockface(0, 0, 0)).month
      . assert(_ == Mar)

      test(m"Timestamp.day accessor"):
        Timestamp(2024-Mar-15, Clockface(0, 0, 0)).day()
      . assert(_ == 15)

      test(m"Timestamp.hour/minute/second accessors"):
        val ts = Timestamp(2024-Mar-15, Clockface(14, 30, 59))
        (ts.hour, ts.minute, ts.second)
      . assert(_ == (14, 30, 59))

      test(m"Timestamp.monthstamp"):
        val ms = Timestamp(2024-Mar-15, Clockface(0, 0, 0)).monthstamp
        (ms.year(), ms.month)
      . assert(_ == (2024, Mar))

      test(m"Timestamp.decode 'YYYY-MM-DDTHH:MM:SS'"):
        t"2024-01-15T14:30:59".decode[Timestamp]
      . assert(_ == Timestamp(2024-Jan-15, Clockface(14, 30, 59)))

      test(m"Timestamp.decode 'YYYY-MM-DD HH:MM:SS'"):
        t"2024-01-15 14:30:59".decode[Timestamp]
      . assert(_ == Timestamp(2024-Jan-15, Clockface(14, 30, 59)))

      test(m"Timestamp.decode rejects malformed input"):
        capture(t"not-a-timestamp".decode[Timestamp])
      . matches:
          case _: TimestampError =>

      test(m"Timestamp.decode rejects invalid month"):
        capture(t"2024-13-15T14:30:59".decode[Timestamp])
      . matches:
          case _: TimestampError =>

    suite(m"TZDB parser"):
      given TimeEvent is Loggable = Log.silent[TimeEvent]

      test(m"parseFile on a non-existent file raises NoTzdbFile"):
        capture(Tzdb.parseFile(t"this-does-not-exist"))
      . matches:
          case _: TzdbError =>

      test(m"parses a single Rule line"):
        val lines = Stream(t"Rule\tUS\t2007\tmax\t-\tMar\tSun>=8\t2:00\t1:00\tD")
        Tzdb.parse(t"inline", lines).headOption
      . matches:
          case Some(_: Tzdb.Entry.Rule) =>

      test(m"parses a single Link line"):
        val lines = Stream(t"Link\tEurope/London\tEurope/Belfast")
        Tzdb.parse(t"inline", lines).headOption
      . matches:
          case Some(_: Tzdb.Entry.Link) =>

      test(m"parses a leap line with normal-time"):
        val lines = Stream(t"Leap\t1972\tJun\t30\t23:59:59\t+\tS")
        Tzdb.parse(t"inline", lines).headOption
      . matches:
          case Some(_: Tzdb.Entry.Leap) =>

      test(m"leap line with 60-second time raises TzdbError"):
        val lines = Stream(t"Leap\t1972\tJun\t30\t23:59:60\t+\tS")
        capture(Tzdb.parse(t"inline", lines))
      . matches:
          case _: TzdbError =>


      test(m"unparseable Rule raises UnexpectedRule"):
        val lines = Stream(t"Rule\tonly")
        capture(Tzdb.parse(t"inline", lines))
      . matches:
          case _: TzdbError =>

      test(m"unparseable Link raises UnexpectedLink"):
        val lines = Stream(t"Link\tonly")
        capture(Tzdb.parse(t"inline", lines))
      . matches:
          case _: TzdbError =>
