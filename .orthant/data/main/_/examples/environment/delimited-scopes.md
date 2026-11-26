# A variable set for one block

`variables` runs a block under an environment which has one variable more. The variable is set
for the code in the block, and for nothing else: when the block ends, the environment is the
one there was before.

```scala fuel
compile
frame
  label Inside the block
  highlight variables  sets PROMPTER_CUE for the block
  code
      import environments.emptyEnvironment, strategies.throwUnsafely

      val inside: Text =
        variables(prompterCue = t"Enter Viola"):
          Environment.prompterCue[Text]
frame
  label Outside it
  highlight safely(  unset again
  code
      import environments.emptyEnvironment, strategies.throwUnsafely

      val inside: Text =
        variables(prompterCue = t"Enter Viola"):
          Environment.prompterCue[Text]

      val outside: Optional[Text] = safely(Environment.prompterCue[Text])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(inside)
Out.println(outside.inspect)
```

[Overriding for a block](../../soundness/modules/environment.md#overriding-for-a-block)
explains what a subprocess inherits, and
[delimited scopes](../../philosophy/delimited-scopes.md) the shape.
