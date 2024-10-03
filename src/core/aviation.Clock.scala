package aviation

abstract class Clock():
  def apply(): Instant

object Clock:
  given Clock as current:
    def apply(): Instant = Instant.of(System.currentTimeMillis)

  def fixed(instant: Instant): Clock = new Clock():
    def apply(): Instant = instant

  def offset(diff: Duration): Clock = new Clock():
    def apply(): Instant = Instant.of(System.currentTimeMillis) + diff
