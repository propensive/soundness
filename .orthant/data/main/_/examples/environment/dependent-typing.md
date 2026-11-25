# A variable which knows its type

A variable is read as a member of `Environment`, named in camel case. The name decides the
type: `COLUMNS` is known to hold a number, so `Environment.columns` is an `Int`, decoded from
the text of the variable.

```scala fuel
compile
frame
  label A variable which holds a number
  code
      import strategies.throwUnsafely

      given globe: Environment = name => if name == t"COLUMNS" then t"80" else Unset

      val columns = Environment.columns
frame
  label It is a number
  highlight Int  decoded from the text
  code
      import strategies.throwUnsafely

      given globe: Environment = name => if name == t"COLUMNS" then t"80" else Unset

      val columns = Environment.columns
      val margin: Int = (columns - 66)/2
frame
  label It is not text
  errors show
  code
      import strategies.throwUnsafely

      given globe: Environment = name => if name == t"COLUMNS" then t"80" else Unset

      val columns = Environment.columns
      val margin: Text = columns
##
```

[Environment variables](../../soundness/modules/environment.md#environment-variables) says
how a name is translated.
