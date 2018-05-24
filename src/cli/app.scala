package probation

abstract class TestApp() {

  /** the code containing the tests to be run */
  def tests(): Unit
  
  /** the instance of the [[Run]] to be available to tests defined when implementing the [[tests]]
    * method
    *
    * Note that this value is initialized explicitly when the [[main]] method is executed, as the
    * instance requires the command-line arguments as parameters to its creation. Prior to running
    * the [[main]] method, this value will be `null`. */
  implicit protected var runner: Runner = _
  
  /** the entry-point of the application, implemented to run a typical test suite
   *
   *  This implementation initializes a new [[CliRunner]], and sets the implicit `runner` variable
   *  (which is available anywhere in the class body) to its value. It then runs the user-defined
   *  `test()` method, and prints out a report to stdout.
   *
   *  Parameters should be provided in POSIX-style, in short (`-s`) or long (`--long`) form, with
   *  any values to parameters specified in a subsequent argument (in the command-line sense of
   *  "argument"), e.g. `-c 80` or `--columns 80`; immediately following a short parameter, e.g.
   *  `-c80`; or, following an equals-sign (`=`) as part of a long parameter, e.g.
   *  `--columns=80`. */
  def main(args: Array[String]): Unit = {
    import CliRunner.Config

    /* parses the command-line arguments to specify the parameters for the creation of a [[Config]]
     * instance */
    def parse(args: List[String], config: Config = Config()): Config = (args.flatMap { arg =>
      
      // Transform combined forms of arguments and values into separate arguments
      if(arg.startsWith("--")) arg.split("=", 2).to[List]
      else if(arg.startsWith("-") && arg.length > 2) List(arg.substring(0, 2), arg.substring(2))
      else List(arg)
    }) match {
      case Nil =>
        config
      case ("-h" | "--help") :: _ =>
        println("""Usage: probation [OPTIONS]
                  |Runs the application, and prints a test report to the console.
                  |
                  |Mandatory arguments to long options are mandatory for short options too.
                  |  -c, --columns=COLS     print output for a terminal width of COLS
                  |  -l, --lazy             only compute tests when their values are used
                  |  -n, --no-color         do not print ANSI color output
                  |  -o, --test-only=TESTS  only run tests with the specified comma-separated hashes
                  |  -q, --quiet            do not produce any output
                  |  -t, --tags=TAGS        only run tests with the specified comma-separated tags
                  |""".stripMargin)
        sys.exit(0)
      case ("-c" | "--columns") :: cols :: rest =>
        parse(rest, config.copy(columns = cols.toInt))
      case ("-d" | "--directory") :: dir :: rest =>
        parse(rest, config.copy(outputDir = Some(dir)))
      case ("-n" | "--no-color") :: rest =>
        parse(rest, config.copy(color = false))
      case ("-q" | "--quiet") :: rest =>
        parse(rest, config.copy(quiet = true))
      case ("-l" | "--lazy") :: rest =>
        parse(rest, config.copy(runLazily = true))
      case ("-o" | "--test-only") :: tests :: rest =>
        parse(rest, config.copy(testOnly = Some(tests.split(",").to[Set])))
      case other :: _ =>
        println(s"Unknown argument: $other")
        sys.exit(1)
    }

    runner = new CliRunner(parse(args.to[List]))
    tests()
    Test.report()
    ()
  }
}

