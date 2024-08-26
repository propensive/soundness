package soundness

export exoskeleton.{Argument, Arguments, Cli, CliInterpreter, Flag, FlagInterpreter,
    FlagParameters, PosixCliInterpreter, PosixParameters, Shell, SimpleParameterInterpreter,
    Subcommand, Suggestions, Switch, arguments}

package parameterInterpretation:
  export exoskeleton.parameterInterpretation.{simple, posix}
