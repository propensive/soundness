# A permission the agent may not have offered

An agent which asks permission offers a list of options, and the list need not hold the kind a
policy would choose. The first option of a kind is therefore an `Optional`, and `lay` gives
both answers at once: the option's id if there is one, and `Cancelled` if there is not.

```scala fuel
compile
frame
  label An option to allow once is taken for granted
  errors show
  code
      def policy()(using Acp.Registry): Unit = Acp.permission:
        val once = Acp.options.filter(_.kind == Acp.PermissionOptionKind.AllowOnce)
        Acp.Selected(once.head.optionId)
frame
  label The request is cancelled where it was not offered
  highlight lay(Acp.Cancelled)  the answer when no such option exists
  highlight Acp.Selected(option.optionId)  and when one does
  code
      def policy()(using Acp.Registry): Unit = Acp.permission:
        val once = Acp.options.filter(_.kind == Acp.PermissionOptionKind.AllowOnce)
        once.head.lay(Acp.Cancelled): option => Acp.Selected(option.optionId)
##
```

The example is compiled, and is not run, since the handler answers an agent in another process.
[Permissions](../../soundness/modules/acp.md#permissions) has the kinds of option, and
[optionality](../../philosophy/optionality.md) the meaning of `lay`.
