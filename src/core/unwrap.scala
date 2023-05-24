package rudiments

extension (string: String)
  def unwrap: Text =
    val buf: StringBuilder = StringBuilder()
    
    def recur(lines: List[String], break: Boolean): Text = lines match
      case Nil =>
        Text(buf.toString)
      case line :: tail =>
        if line.forall(_.isWhitespace) then recur(tail, true) else
          buf.append(if !break then " " else "\n")
          buf.append(line.trim.nn.replaceAll("\\s+", " "))
          recur(tail, false)
    
    recur(string.split("\n").nn.map(_.nn).to(List), false)