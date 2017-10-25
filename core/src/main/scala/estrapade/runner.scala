package estrapade

/** the runner for tests definitions, which determines if, when and how the tests and assertions on
 *  them are executed
 *
 *  Implementing a custom test runner involves specifying three methods `skip`, `record` and
 *  `report`.  */
trait Runner {
  import Test._

  /** the type of the report to be returned upon calling `report` */
  type Return
  
  /** should the specified test, identified by a prefix of its hash, be skipped? */
  def skip(test: String): Boolean

  /** capture the outcome of running the test, as a side-effect */
  def record(definition: Definition[_], outcome: Outcome, duration: Long): Unit
  
  /** return or generate a report providing details about the recorded test runs */
  def report(): Return
}

