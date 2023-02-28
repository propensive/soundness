package larceny

def captureCompileErrors(code: Any): List[CompileError] = code match
  case xs: List[?] => xs.collect:
    case error: CompileError => error

case class CompileError(text: String, badCode: String, offset: Int)