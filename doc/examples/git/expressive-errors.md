# A refusal which gives its reason

Git has a dozen rules for the name of a reference. A name which breaks one is refused with a
`Git.RefError`, which holds the name and which rule it broke as values, and says both in its
message.

```scala fuel
compile
frame
  label A branch, from a name
  highlight parse  checks the name against Git's rules
  code
      import strategies.throwUnsafely

      def branch(name: Text): Git.Branch = Git.Branch.parse(name)
##
```

```scala fuel
run
errors show
##
branch(t"second..quarto")
```

[References](../../soundness/modules/git.md#references) has the rules, and
[expressive errors](../../philosophy/expressive-errors.md) the reasons for an error which
explains itself.
