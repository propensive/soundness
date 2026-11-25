# An agent's refusal has a reason

Every fault an agent reports arrives as an `Acp.Error` whose `reason` is one of a fixed set. A
method which talks to the agent declares the error, and a caller which knows what to do about
one reason says so, and leaves the rest declared.

```scala fuel
compile
frame
  label The fault is not accounted for
  errors show
  code
      def begin()(using Acp.Connection): Text =
        Acp.connection.newSession(t"/globe").sessionId
frame
  label It is declared
  highlight raises..Error  passed on to the caller
  code
      def begin()(using Acp.Connection): Text raises Acp.Error =
        Acp.connection.newSession(t"/globe").sessionId
frame
  label And an agent which wants authentication is given it
  highlight Acp.Error.Reason.AuthRequired  the one reason handled here
  highlight abort(error)  every other reason is passed on
  code
      def begin()(using Acp.Connection): Text raises Acp.Error =
        recover:
          case error: Acp.Error => error.reason match
            case Acp.Error.Reason.AuthRequired =>
              Acp.connection.authenticate(t"api-key")
              Acp.connection.newSession(t"/globe").sessionId
            case other => abort(error)
        . protect(Acp.connection.newSession(t"/globe").sessionId)
##
```

The example is compiled, and is not run, since it speaks to an agent in another process.
[Errors](../../soundness/modules/acp.md#errors) lists the reasons, and
[sessions](../../soundness/modules/acp.md#sessions) describes `authenticate`.
