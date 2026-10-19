                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package enigmatic

import soundness.*

// Keys generated once with `ssh-keygen -N ''` and `openssl genpkey`/`rsa`/`ecparam`/`pkey`, in
// every format a key file is found in. None protects anything.
object KeyFixtures:
  val sshEd25519: Text = t"""
    |-----BEGIN OPENSSH PRIVATE KEY-----
    |b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
    |QyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUgAAAJB+g2KdfoNi
    |nQAAAAtzc2gtZWQyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUg
    |AAAEDTzzy05nIC9r9aKtXYNJMgY1F/g5ZiPNyksGUvj8yR6xGH6R8uL9Aww7+jnLISnQu3
    |kiglJKYyOcKlvCJ7v+xSAAAADHRlc3RAZXhhbXBsZQE=
    |-----END OPENSSH PRIVATE KEY-----
    """.s.stripMargin.tt

  val sshEd25519Public: Text =
    t"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS test@example"

  val sshEcdsa256: Text = t"""
    |-----BEGIN OPENSSH PRIVATE KEY-----
    |b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAaAAAABNlY2RzYS
    |1zaGEyLW5pc3RwMjU2AAAACG5pc3RwMjU2AAAAQQRdOMFpco5p2jnlKq/NwqYMYoUzwzBg
    |nwjuPtBEjVOPzeeUTT7lwUD95OfwDZjmq6MFWVOzdy+uAeprl8TIqovgAAAAqBUqwMcVKs
    |DHAAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBF04wWlyjmnaOeUq
    |r83CpgxihTPDMGCfCO4+0ESNU4/N55RNPuXBQP3k5/ANmOarowVZU7N3L64B6muXxMiqi+
    |AAAAAhAPFyK43r4tQUj2J34FUWk9h7IHn3mfZ6B74bJbeV+P5xAAAADHRlc3RAZXhhbXBs
    |ZQECAw==
    |-----END OPENSSH PRIVATE KEY-----
    """.s.stripMargin.tt

  val sshEcdsa256Public: Text =
    t"ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBF04wWlyjmnaOeUqr83CpgxihTPDMGCfCO4+0ESNU4/N55RNPuXBQP3k5/ANmOarowVZU7N3L64B6muXxMiqi+A= test@example"

  val sshEcdsa521: Text = t"""
    |-----BEGIN OPENSSH PRIVATE KEY-----
    |b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAArAAAABNlY2RzYS
    |1zaGEyLW5pc3RwNTIxAAAACG5pc3RwNTIxAAAAhQQBY++klzV6dCbFzuKK1Q0pytfqBYvR
    |vnQII/ffezcIKq1+evO3FRCc0/DMJHK6KX1jfP6NWUORsWhNgCXpN1INsg4AmpLFF+rEks
    |/ZwgQqzvaEQSFfJ4c6/je8SQTpwWMzUbimxQneSEv9z5ZFZi8+u0xtwnyFpoa7DSv1KkKs
    |GLrRYCMAAAEQ3mbgT95m4E8AAAATZWNkc2Etc2hhMi1uaXN0cDUyMQAAAAhuaXN0cDUyMQ
    |AAAIUEAWPvpJc1enQmxc7iitUNKcrX6gWL0b50CCP333s3CCqtfnrztxUQnNPwzCRyuil9
    |Y3z+jVlDkbFoTYAl6TdSDbIOAJqSxRfqxJLP2cIEKs72hEEhXyeHOv43vEkE6cFjM1G4ps
    |UJ3khL/c+WRWYvPrtMbcJ8haaGuw0r9SpCrBi60WAjAAAAQgG6JP9+zEw0ypOIcYloTwBj
    |slFa06Q5nscAPfcq0WjJ4rEFEBpCSojHRuK38z7lA8/IkNIW/scSDvpU9em2raBmeQAAAA
    |x0ZXN0QGV4YW1wbGUBAgMEBQY=
    |-----END OPENSSH PRIVATE KEY-----
    """.s.stripMargin.tt

  val sshRsa2048: Text = t"""
    |-----BEGIN OPENSSH PRIVATE KEY-----
    |b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAABFwAAAAdzc2gtcn
    |NhAAAAAwEAAQAAAQEA3M9Dd1q1WXYAvZVzMYIC4LNTsTB0mY085Aj7CNuvLIglRi1Cuitm
    |ks3Y69EVdNyV3bIFnYmm1qpZDHFt6Fp+EfSb6DzE6MGd3G1CGSkBl5D5PUREsUXAMRz4CG
    |shC0TqSWq7fTM1bzyEd3FXBuQqkNdJ6CBVsKmgg/5uSHZvmS3I9VZSfTcx5tbR0cjGniys
    |QUiLERgD5Iq/Ya28Q/ktan0rQmhmPrsK0AgW+CqGzOHMIr6E3uoSJIrx+CR7B7taGbqMyc
    |xobieQ2+peC51yQ+S/cHautPp6TZiAVaLoC7UEc67ylU6+7NlhOOo6Y//C21B8O+6GCPa6
    |/yXVvrAshwAAA8gjOhoYIzoaGAAAAAdzc2gtcnNhAAABAQDcz0N3WrVZdgC9lXMxggLgs1
    |OxMHSZjTzkCPsI268siCVGLUK6K2aSzdjr0RV03JXdsgWdiabWqlkMcW3oWn4R9JvoPMTo
    |wZ3cbUIZKQGXkPk9RESxRcAxHPgIayELROpJart9MzVvPIR3cVcG5CqQ10noIFWwqaCD/m
    |5Idm+ZLcj1VlJ9NzHm1tHRyMaeLKxBSIsRGAPkir9hrbxD+S1qfStCaGY+uwrQCBb4KobM
    |4cwivoTe6hIkivH4JHsHu1oZuozJzGhuJ5Db6l4LnXJD5L9wdq60+npNmIBVougLtQRzrv
    |KVTr7s2WE46jpj/8LbUHw77oYI9rr/JdW+sCyHAAAAAwEAAQAAAQBPFSxkougwkkLRo6UO
    |JjJBp/hhjak1FZCpW04NbiDfIiO3bp5nbwPdsDvwuYGlyRsju46nTZWjbH9YJYfnoAFnDO
    |SJ2LntYurW94qk2DrpvdKGTt0LYJ/k9yseWMAM+TGv+boMrTqGDHalEMLKhQ15sqGVsX7/
    |NsF2hLma2lA9261ZjFKhXkIKCVIwg/1ylav3aRDsTVd8XIx35YZKQUGO1g2QiQ7JIao+8I
    |dr/Y7L7jETatir0YObGdPkmJBU737FYAPklZzSMk+WiB65nJTREHvGmDgPOMke2jnveba0
    |poGwiQUTnQ8ao946K1+Y3bn0XO5Tv78AgczewHRAn3sBAAAAgFiLYh7UkcApmeeOkBQhjp
    |TKfLF3QR8pt08w6WZjZGimjJTdiWI367trbOWE8lY9On/uKjnPmY4EhI4CYsKaP1bFt3M6
    |i4BOiDybZcIJuylJHCzMBlslY2DJxjuUfeWrLnqII7XKsjadHdv6hXu4uZTAbxyzyHF0q6
    |cHfD4xvKKSAAAAgQD+kvytCOuifLxsnqf8tDHMOE5d3pyIm9e2o+PueVNYKyjKrJuylnmS
    |e/uXKCTVT0Ft62TFW4330FU9/icFzrNZFWwQj8262hbynk5MdZUOBBkBYDjG3FV7BsMvYW
    |N27ckBjAiLNsOca1f+HLU0seSS+wCsvrtauJ5dy3jVrsr8fwAAAIEA3gvdQ+5D2ER14KR9
    |TIwP1OYFbbSuvrY+t4w0+F+X6w9B6z7S0R/2OKCKEvV8lrAXaeP7NWgGme36NSDE3UFo0H
    |N64hZ3a7Rbotsas9uJiwPh8GsdowKERgrw94BWD2lWvV/xNJ/1umqc3MMavWdyaL4IlTJb
    |n+GGdlMdsr6I6/kAAAAMdGVzdEBleGFtcGxlAQIDBAUGBw==
    |-----END OPENSSH PRIVATE KEY-----
    """.s.stripMargin.tt

  val sshRsa2048Public: Text =
    t"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDcz0N3WrVZdgC9lXMxggLgs1OxMHSZjTzkCPsI268siCVGLUK6K2aSzdjr0RV03JXdsgWdiabWqlkMcW3oWn4R9JvoPMTowZ3cbUIZKQGXkPk9RESxRcAxHPgIayELROpJart9MzVvPIR3cVcG5CqQ10noIFWwqaCD/m5Idm+ZLcj1VlJ9NzHm1tHRyMaeLKxBSIsRGAPkir9hrbxD+S1qfStCaGY+uwrQCBb4KobM4cwivoTe6hIkivH4JHsHu1oZuozJzGhuJ5Db6l4LnXJD5L9wdq60+npNmIBVougLtQRzrvKVTr7s2WE46jpj/8LbUHw77oYI9rr/JdW+sCyH test@example"

  // `ssh-keygen -N secret`: aes256-ctr under bcrypt.
  val sshEd25519Encrypted: Text = t"""
    |-----BEGIN OPENSSH PRIVATE KEY-----
    |b3BlbnNzaC1rZXktdjEAAAAACmFlczI1Ni1jdHIAAAAGYmNyeXB0AAAAGAAAABB7M60jAx
    |3pApNMe7iSljWgAAAAGAAAAAEAAAAzAAAAC3NzaC1lZDI1NTE5AAAAIDHetHTASxBWMIbi
    |izrWLnL2e66UzjBpDgrJOYAHJS/oAAAAkLY1Qnul+4ZWFNsq5A8imKIfO6xH9FwX/aIaZD
    |ndlWzB/y9m7kV7czJa4nhgTNJX15MEtqsLTTnNKFnd9nglxlAV3QCujShsxe2FyS3rG8cG
    |rQCh0uY5LZknBX2uY/Tv0aII0TJm32WVvm6T6zoomP2CQ5ibkhLHaG0EbrndAOVXmPA9R+
    |WZ7zYGT7RdCoXwVA==
    |-----END OPENSSH PRIVATE KEY-----
    """.s.stripMargin.tt

  // `openssl genpkey -algorithm ed25519`: a version-1 PrivateKeyInfo, which carries no public key.
  val pkcs8Ed25519: Text = t"""
    |-----BEGIN PRIVATE KEY-----
    |MC4CAQAwBQYDK2VwBCIEIIbKsY12j+HLwwg0B5LNDBoHI57RAIUbOGAi8wueiz80
    |-----END PRIVATE KEY-----
    """.s.stripMargin.tt

  val spkiEd25519: Text = t"""
    |-----BEGIN PUBLIC KEY-----
    |MCowBQYDK2VwAyEAUpHmVwbrIn7PHWmc2hsBmIZmwqszygRlp09gjCt40io=
    |-----END PUBLIC KEY-----
    """.s.stripMargin.tt

  val pkcs8Rsa2048: Text = t"""
    |-----BEGIN PRIVATE KEY-----
    |MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCbyR/syPHpt/pc
    |lJUuGi34A9FWyUo1zxpRLbR7EvmPsaEmkWc4uPU4VUM/28ZDetD6qbf+yLIqyQgZ
    |xbscW+trBa6mz2R4qI3Ao+tRTc2Bt0cLuH80YGVRO+dft69OIRFBlu0P9v4uAMFS
    |mYo7fNfxAwjs1R08cqW9yttRdJFxCIcE7J5P8zrHZYRH51vIGBZt65YF8XxK8nfn
    |LGqR5GHgKKP8Qp9NM6/Tq6lNFJveDI+zNa+Kft/MYgUpy2nW4FgkHB6nvljP+JvR
    |t9V+RlWGDIp3aH+syNTG2GyiHylkUKq8a+PTiaX4mxjDrkbNtf5PMsITILwd1xyN
    |oNTpimg/AgMBAAECggEAFqUlSV+qXp16PLPyfXmbvgYcC4AGFA1PV4aLyioncb5R
    |3Q/cz89/AdnPBTzWtJ9pOk6qatRUpciEL27hNI2eCngQe7oA8lMaC+sn/TEQku8H
    |2knO39e1UpohF3ZVesduwTiFrEDskNZWQ8IC6IPMxgF5bifKcKt/sTeIKu6FZ8cp
    |2vu9BjYGYGg279bReoMsd++liXUCvwSij8Q5GclIOX4lS5Hs1PsjsRkTaV4cNkyY
    |2tT6C1lBK+waaLWZqLgY3eHBNXBo2i2r6RxrNERzV9cmALGDW+ZmurJ8+Sil2Tb+
    |Q3b/RNl4c4lDIKn88SdR3oPa0ARoY9vWWKCJ/dDOWQKBgQDV8jY8mjKdHRdcfSSz
    |r3tA0uiu9IiI41h6pC8ekaiBw8HAt6hLbuiOvCybYtxyUqGZQd/6NbHUTC6WiX+T
    |zk2TlveAF7wkGClAWLCCRotAfIOamPtSBCTyfrKkJ7dyrreV5z52pI7D16ZIkvPa
    |Prn6NcN/Rc/zgOqL2PO5uMK8aQKBgQC6aEWLUsDNN8JbHexyt/mxSNUZbJLFsabR
    |mL690ZmXuQSNy2OQdGdM4y6VrQRSwYDwrjSQhU+hf3DrtXU//wpHz2g6WaEN6XFU
    |D9PlcFw4OAY/S2THgXCUvSTrKp4hUN76IthbCQsNa4WA2f6R06ZcDqOqsbYwbWhV
    |hS6rGKCKZwKBgQCBhc2ICuIxc/c8qxaZIdfAeu8bXDd8iAZ82QEdxJj7VoPqJEXi
    |xRZNs2Zd/A/xtkzW96adV/x8xlI+GKf/15nnjDHyfmwMfgQbTJYviH2tYLRb19nu
    |4Uts8387GkBZUaUbP4/1S9KShe/JaA3xRqBY9kuQtbcbeCSG3aVP/qcGYQKBgQCp
    |hgShBRxERc5icR9E4R2aZiMu4PfGBJ8yhFgLZt/pBKEoluywRbxgR0dpZZTF8ibi
    |O3ugZyyZMb4vDrJYBgiCcBRY5IQ8k622BkLtqTalBgZgb59ydLw++JjnqbVvlQ2D
    |rEbY26uKg9tA6H/EfRgRJlsf9Eg9WHKHO/zKKzZtRwKBgFcjoOZRtPG+rLHOQCSr
    |IZScRrN/TldvfNQ9bQVY13L9U8haZVNJhxPDngu8y2PhJWJWR4UwlnGzna0624gR
    |TphRDFhEQPWgyzGKThobFt3cY9WRrWiGwRbjz5x8sRPrBQO7OLMwQqQrLKOjZq9W
    |/r5sRtW7w1i1GE4lC2bjFQmY
    |-----END PRIVATE KEY-----
    """.s.stripMargin.tt

  // `openssl rsa -traditional` of the key above.
  val pkcs1Rsa2048: Text = t"""
    |-----BEGIN RSA PRIVATE KEY-----
    |MIIEpAIBAAKCAQEAm8kf7Mjx6bf6XJSVLhot+APRVslKNc8aUS20exL5j7GhJpFn
    |OLj1OFVDP9vGQ3rQ+qm3/siyKskIGcW7HFvrawWups9keKiNwKPrUU3NgbdHC7h/
    |NGBlUTvnX7evTiERQZbtD/b+LgDBUpmKO3zX8QMI7NUdPHKlvcrbUXSRcQiHBOye
    |T/M6x2WER+dbyBgWbeuWBfF8SvJ35yxqkeRh4Cij/EKfTTOv06upTRSb3gyPszWv
    |in7fzGIFKctp1uBYJBwep75Yz/ib0bfVfkZVhgyKd2h/rMjUxthsoh8pZFCqvGvj
    |04ml+JsYw65GzbX+TzLCEyC8HdccjaDU6YpoPwIDAQABAoIBABalJUlfql6dejyz
    |8n15m74GHAuABhQNT1eGi8oqJ3G+Ud0P3M/PfwHZzwU81rSfaTpOqmrUVKXIhC9u
    |4TSNngp4EHu6APJTGgvrJ/0xEJLvB9pJzt/XtVKaIRd2VXrHbsE4haxA7JDWVkPC
    |AuiDzMYBeW4nynCrf7E3iCruhWfHKdr7vQY2BmBoNu/W0XqDLHfvpYl1Ar8Eoo/E
    |ORnJSDl+JUuR7NT7I7EZE2leHDZMmNrU+gtZQSvsGmi1mai4GN3hwTVwaNotq+kc
    |azREc1fXJgCxg1vmZrqyfPkopdk2/kN2/0TZeHOJQyCp/PEnUd6D2tAEaGPb1lig
    |if3QzlkCgYEA1fI2PJoynR0XXH0ks697QNLorvSIiONYeqQvHpGogcPBwLeoS27o
    |jrwsm2LcclKhmUHf+jWx1Ewulol/k85Nk5b3gBe8JBgpQFiwgkaLQHyDmpj7UgQk
    |8n6ypCe3cq63lec+dqSOw9emSJLz2j65+jXDf0XP84Dqi9jzubjCvGkCgYEAumhF
    |i1LAzTfCWx3scrf5sUjVGWySxbGm0Zi+vdGZl7kEjctjkHRnTOMula0EUsGA8K40
    |kIVPoX9w67V1P/8KR89oOlmhDelxVA/T5XBcODgGP0tkx4FwlL0k6yqeIVDe+iLY
    |WwkLDWuFgNn+kdOmXA6jqrG2MG1oVYUuqxigimcCgYEAgYXNiAriMXP3PKsWmSHX
    |wHrvG1w3fIgGfNkBHcSY+1aD6iRF4sUWTbNmXfwP8bZM1vemnVf8fMZSPhin/9eZ
    |54wx8n5sDH4EG0yWL4h9rWC0W9fZ7uFLbPN/OxpAWVGlGz+P9UvSkoXvyWgN8Uag
    |WPZLkLW3G3gkht2lT/6nBmECgYEAqYYEoQUcREXOYnEfROEdmmYjLuD3xgSfMoRY
    |C2bf6QShKJbssEW8YEdHaWWUxfIm4jt7oGcsmTG+Lw6yWAYIgnAUWOSEPJOttgZC
    |7ak2pQYGYG+fcnS8PviY56m1b5UNg6xG2NurioPbQOh/xH0YESZbH/RIPVhyhzv8
    |yis2bUcCgYBXI6DmUbTxvqyxzkAkqyGUnEazf05Xb3zUPW0FWNdy/VPIWmVTSYcT
    |w54LvMtj4SViVkeFMJZxs52tOtuIEU6YUQxYRED1oMsxik4aGxbd3GPVka1ohsEW
    |48+cfLET6wUDuzizMEKkKyyjo2avVv6+bEbVu8NYtRhOJQtm4xUJmA==
    |-----END RSA PRIVATE KEY-----
    """.s.stripMargin.tt

  val spkiRsa2048: Text = t"""
    |-----BEGIN PUBLIC KEY-----
    |MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAm8kf7Mjx6bf6XJSVLhot
    |+APRVslKNc8aUS20exL5j7GhJpFnOLj1OFVDP9vGQ3rQ+qm3/siyKskIGcW7HFvr
    |awWups9keKiNwKPrUU3NgbdHC7h/NGBlUTvnX7evTiERQZbtD/b+LgDBUpmKO3zX
    |8QMI7NUdPHKlvcrbUXSRcQiHBOyeT/M6x2WER+dbyBgWbeuWBfF8SvJ35yxqkeRh
    |4Cij/EKfTTOv06upTRSb3gyPszWvin7fzGIFKctp1uBYJBwep75Yz/ib0bfVfkZV
    |hgyKd2h/rMjUxthsoh8pZFCqvGvj04ml+JsYw65GzbX+TzLCEyC8HdccjaDU6Ypo
    |PwIDAQAB
    |-----END PUBLIC KEY-----
    """.s.stripMargin.tt

  // `openssl ecparam -genkey -name prime256v1`: SEC1, with the curve in its `[0]` field.
  val sec1P256: Text = t"""
    |-----BEGIN EC PRIVATE KEY-----
    |MHcCAQEEIFbaNalDIdzV7OvDG3IgMPYSvmb+vpAyzgUJChDUoQN3oAoGCCqGSM49
    |AwEHoUQDQgAEMi3VjQn5vrKKJwpIcqk8+keD5+AJfHq230zgcb3PdFwPKLv40SG/
    |zSBSeDTkft5sVOYN5pxhOlXEMP+EnfEjCw==
    |-----END EC PRIVATE KEY-----
    """.s.stripMargin.tt

  val spkiP256: Text = t"""
    |-----BEGIN PUBLIC KEY-----
    |MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEMi3VjQn5vrKKJwpIcqk8+keD5+AJ
    |fHq230zgcb3PdFwPKLv40SG/zSBSeDTkft5sVOYN5pxhOlXEMP+EnfEjCw==
    |-----END PUBLIC KEY-----
    """.s.stripMargin.tt

  val pkcs8P384: Text = t"""
    |-----BEGIN PRIVATE KEY-----
    |MIG2AgEAMBAGByqGSM49AgEGBSuBBAAiBIGeMIGbAgEBBDDbR+xMFKGVsbgYoV6J
    |fx+fywCWIOO6OsFTLrciLxHfU8yn6mfqgX8EWbiXrBbbeWuhZANiAAQ61GbvDoS3
    |S0UBzszc3NbQdx8dJvZRUVMKjXcMNOEVOgb3zJPNA/Ex4hgo8fIfViDszrTIq/4Z
    |jksi5SXCAFYFvFrGfvBD5aVEssRCjdwQ29ejIbOP5jorszGIMaCL2xA=
    |-----END PRIVATE KEY-----
    """.s.stripMargin.tt

  // `openssl genpkey -aes-256-cbc`: PBES2.
  val pkcs8Rsa2048Encrypted: Text = t"""
    |-----BEGIN ENCRYPTED PRIVATE KEY-----
    |MIIFNTBfBgkqhkiG9w0BBQ0wUjAxBgkqhkiG9w0BBQwwJAQQEvnLQYDAkAf3042X
    |DP+quwICCAAwDAYIKoZIhvcNAgkFADAdBglghkgBZQMEASoEEJj3OPedwfZ73hP1
    |Ku79+c4EggTQY3f/vypvFstV8UiCDOJietD2+qcczIe03mD1CUpwYAz0CR02XnGb
    |6dKL+X8247He09Pi51hEwj4/9YW3/iw7B9k3wjcKtANYXWB5aHa+fRrOl3G+B5OF
    |-----END ENCRYPTED PRIVATE KEY-----
    """.s.stripMargin.tt
