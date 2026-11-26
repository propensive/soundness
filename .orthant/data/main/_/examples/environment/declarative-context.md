# Which environment a program sees

The environment a variable is read from is chosen by an import or a given. The real one, an
empty one for a test, and one written for the purpose are the same to the code which reads
them.

```scala fuel
compile
alternative The real environment
  highlight javaBaseEnvironment  the JVM's own
  code
      import environments.javaBaseEnvironment

      val shell: Optional[Text] = safely(Environment.shell[Text])
alternative No environment at all
  highlight emptyEnvironment  every variable is unset
  code
      import environments.emptyEnvironment

      val shell: Optional[Text] = safely(Environment.shell[Text])
alternative One written for a test
  highlight given..Environment  a function from a name
  code
      given globe: Environment = name => if name == t"SHELL" then t"/bin/zsh" else Unset

      val shell: Optional[Text] = safely(Environment.shell[Text])
##
```

The example is compiled, and is not run, since the first tab reads the machine it runs on.
[Overriding for a block](../../soundness/modules/environment.md#overriding-for-a-block) has
the reasons to substitute an environment, and
[declarative context](../../philosophy/declarative-context.md) the principle.
