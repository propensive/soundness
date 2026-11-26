# A credential, named by its scheme

An API's specification names its security schemes, and says what each calls for. A credential
has both in its type: `"stage_door" is Credential to Authorization` is the OAuth token for
the scheme of that name, and cannot be given a mere key.

```scala fuel
compile
frame
  label A key, where the scheme calls for a token
  errors show
  code
      given door: ("stage_door" is Credential to Authorization) = Credential(t"5p34r")
frame
  label A key, for a scheme which takes one
  highlight "prompt_key"..Text
  code
      given key: ("prompt_key" is Credential to Text) = Credential(t"5p34r")
frame
  label And a token, for the scheme which takes that
  highlight "stage_door"..Authorization
  code
      given key: ("prompt_key" is Credential to Text) = Credential(t"5p34r")

      def door(token: Authorization): ("stage_door" is Credential to Authorization) =
        Credential(token)
##
```

[Credentials for a typed client](../../soundness/modules/oauth.md#credentials-for-a-typed-client)
says how a client finds them, and [infix types](../../philosophy/infix-types.md) explains the
words.
