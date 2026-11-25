# The options are ordinary JSON

The options for a ceremony are a `Json` value, with nothing of WebAuthn in its type. Whatever
reads, shows or sends JSON works on them, and a member is reached by its name.

```scala fuel
compile
frame
  label The options for a registration
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, strategies.throwUnsafely
      import dynamicAccess.dynamicJson

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))
      val burbage = WebAuthn.User(t"account-1567".in[Data], t"burbage", t"Richard Burbage")

      val options: Json = WebAuthn.registration(globe, burbage, WebAuthn.Challenge())
frame
  label A member of them
  highlight options.user.displayName.as[Text]  a path into the JSON
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, strategies.throwUnsafely
      import dynamicAccess.dynamicJson

      val globe =
        WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))
      val burbage = WebAuthn.User(t"account-1567".in[Data], t"burbage", t"Richard Burbage")

      val options: Json = WebAuthn.registration(globe, burbage, WebAuthn.Challenge())
      val player: Text = options.user.displayName.as[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(player)
```

[Registration](../../soundness/modules/webauthn.md#registration) says what a page does with
the options, and [JSON](../../soundness/modules/json.md) what else can be done with them.
