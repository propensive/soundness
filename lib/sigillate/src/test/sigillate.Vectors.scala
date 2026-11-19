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
package sigillate

import soundness.*

// The examples of RFC 7515, RFC 7516, RFC 7517, RFC 7638 and RFC 8037, taken from the RFCs' text
// with the line breaks they insert "for display purposes only" removed.
object Vectors:

  // RFC 7515 §A.1: the HMAC key
  val hs256Key: Text =
    List
      ( t"""{"kty":"oct","k":"AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS""",
        t"""4hcgUuTwjAzZr1Z9CAow"}""" )
    . join
  val hs256Jws: Text =
    List
      ( t"""eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODA""",
        t"""sDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_w""",
        t"""W1gFWFOEjXk""" )
    . join

  // RFC 7515 §A.2: the RSA key
  val rs256Key: Text =
    List
      ( t"""{"kty":"RSA","n":"ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddxHmfHQp-V""",
        t"""aw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMsD1W_YpRPEwOWvG6b32690r2jZ47soMZo9w""",
        t"""Gzjb_7OMg0LOL-bSf63kpaSHSXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdVMT""",
        t"""BQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8NAco9_h9iaGNj8q2ethFkMLs91kz""",
        t"""k2PAcDTW9gb54h4FRWyuXpoQ","e":"AQAB","d":"Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpX""",
        t"""yWPEAeP88vLNO97IjlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0BkTGoVEn2b""",
        t"""KVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0""",
        t"""ZHHzQlBjudU2QvXt4ehNYTCBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLhBOne""",
        t"""ufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ","p":"4BzEEOtIpmVdVEZNCqS7baC4crd0p""",
        t"""qnRH_5IB3jw3bcxGn6QLvnEtfdUdiYrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcm""",
        t"""nPGBY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc","q":"uQPEfgmVtjL0Uyyx88G""",
        t"""ZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxaewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62w""",
        t"""Dmpe_HlB-TnBA-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc","dp":"BwKfV3Ak""",
        t"""q5_MFZDFZCnW-wzl-CCo83WoZvnLQwCTeDv8uzluRSnm71I3QCLdhrqE2e9YkxvuxdBfpT_PI7Yz-FOKnu1R""",
        t"""6HsJeDCjn12Sk3vmAktV2zb34MCdy7cpdTh_YVr7tss2u6vneTwrA86rZtu5Mbr1C1XsmvkxHQAdYo0","dq""",
        t"""":"h_96-mK1R_7glhsum81dZxjTnYynPbZpHziZjeeHcXYsXaaMwkOlODsWa7I9xXDoRwbKgB719rrmI2oKr""",
        t"""6N3Do9U0ajaHF-NKJnwgjMd2w9cjz3_-kyNlxAr2v4IKhGNpmM5iIgOS1VZnOZ68m6_pbLBSp3nssTdlqvd0""",
        t"""tIiTHU","qi":"IYd7DHOhrWvxkwPQsRM2tOgrjbcrfvtQJipd-DlcxyVuuM9sQLdgjVk2oy26F0EmpScGLq""",
        t"""2MowX7fhd_QJQ3ydy5cY7YIBi87w93IKLEdfnbJtoOPLUW0ITrJReOgo1cq9SbsxYawBgfp_gh6A5603k2-Z""",
        t"""QwVK0JKSHuLFkuQ3U"}""" )
    . join
  val rs256Jws: Text =
    List
      ( t"""eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGF""",
        t"""tcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu""",
        t"""3XOiZj5RZmh7AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4BAynRFdi""",
        t"""uB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K0GarZRmB_eSN9383LcOLn6_dO--x""",
        t"""i12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqvhJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5A""",
        t"""XLIhWkWywlVmtVrBp0igcN_IoypGlUPQGe77Rw""" )
    . join

  // RFC 7515 §A.3: the P-256 key
  val es256Key: Text =
    List
      ( t"""{"kty":"EC","crv":"P-256","x":"f83OJ3D2xF1Bg8vub9tLe1gHMzV76e8Tus9uPHvRVEU","y":"x_F""",
        t"""EzRu9m36HLN_tue659LNpXW6pCyStikYjKIWI5a0","d":"jpsQnnGQmL-YBIffH1136cspYG6-0iY7X1fCE""",
        t"""9-E9LI"}""" )
    . join
  val es256Jws: Text =
    List
      ( t"""eyJhbGciOiJFUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGF""",
        t"""tcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw""",
        t"""5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q""" )
    . join

  // RFC 7515 §A.4: the P-521 key
  val es512Key: Text =
    List
      ( t"""{"kty":"EC","crv":"P-521","x":"AekpBQ8ST8a8VcfVOTNl353vSrDCLLJXmPk06wTjxrrjcBpXp5EOn""",
        t"""YG_NjFZ6OvLFV1jSfS9tsz4qUxcWceqwQGk","y":"ADSmRA43Z1DSNx_RvcLI87cdL07l6jQyyBXMoxVg_l""",
        t"""2Th-x3S1WDhjDly79ajL4Kkd0AZMaZmh9ubmf63e3kyMj2","d":"AY5pb7A0UFiB3RELSD64fTLOSV_jazd""",
        t"""F7fLYyuTw8lOfRhWg6Y6rUrPAxerEzgdRhajnu0ferB0d53vM9mE15j2C"}""" )
    . join
  val es512Jws: Text =
    List
      ( t"""eyJhbGciOiJFUzUxMiJ9.UGF5bG9hZA.AdwMgeerwtHoh-l192l60hp9wAHZFVJbLfD_UxMi70cwnZOYaRI1""",
        t"""bKPWROc-mZZqwqT2SI-KGDKB34XO0aw_7XdtAG8GaSwFKdCAPZgoXD2YBJZCPEX3xKpRwcdOO8KpEHwJjyqO""",
        t"""gzDO7iKvU8vcnwNrmxYbSW9ERBXukOXolLzeO_Jn""" )
    . join

  // RFC 7515 §A.5
  val unsecuredJws: Text =
    List
      ( t"""eyJhbGciOiJub25lIn0.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFt""",
        t"""cGxlLmNvbS9pc19yb290Ijp0cnVlfQ.""" )
    . join

  // RFC 7515 §A.6.4
  val generalJws: Text =
    List
      ( t"""{"payload":"eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNv""",
        t"""bS9pc19yb290Ijp0cnVlfQ","signatures":[{"protected":"eyJhbGciOiJSUzI1NiJ9","header":{""",
        t""""kid":"2010-12-29"},"signature":"cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJb""",
        t"""u3XOiZj5RZmh7AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4BAynRFd""",
        t"""iuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K0GarZRmB_eSN9383LcOLn6_dO--""",
        t"""xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqvhJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5""",
        t"""AXLIhWkWywlVmtVrBp0igcN_IoypGlUPQGe77Rw"},{"protected":"eyJhbGciOiJFUzI1NiJ9","heade""",
        t"""r":{"kid":"e9bc097a-ce51-4036-9562-d2ade882db0d"},"signature":"DtEhU3ljbEg8L38VWAfUA""",
        t"""qOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q"}]}""" )
    . join

  // RFC 7515 §A.7
  val flattenedJws: Text =
    List
      ( t"""{"payload":"eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNv""",
        t"""bS9pc19yb290Ijp0cnVlfQ","protected":"eyJhbGciOiJFUzI1NiJ9","header":{"kid":"e9bc097a""",
        t"""-ce51-4036-9562-d2ade882db0d"},"signature":"DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4Gawxae""",
        t"""pmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q"}""" )
    . join

  // RFC 7516 §A.1: RSA-OAEP with A256GCM
  val rsaOaepKey: Text =
    List
      ( t"""{"kty":"RSA","n":"oahUIoWw0K0usKNuOR6H4wkf4oBUXHTxRvgb48E-BVvxkeDNjbC4he8rUWcJoZmds2""",
        t"""h7M70imEVhRU5djINXtqllXI4DFqcI1DgjT9LewND8MW2Krf3Spsk_ZkoFnilakGygTwpZ3uesH-PFABNIUY""",
        t"""pOiN15dsQRkgr0vEhxN92i2asbOenSZeyaxziK72UwxrrKoExv6kc5twXTq4h-QChLOln0_mtUZwfsRaMStP""",
        t"""s6mS6XrgxnxbWhojf663tuEQueGC-FCMfra36C9knDFGzKsNa7LZK2djYgyD3JR_MB_4NUJW_TqOQtwHYbxe""",
        t"""voJArm-L5StowjzGy-_bq6Gw","e":"AQAB","d":"kLdtIj6GbDks_ApCSTYQtelcNttlKiOyPzMrXHeI-y""",
        t"""k1F7-kpDxY4-WY5NWV5KntaEeXS1j82E375xxhWMHXyvjYecPT9fpwR_M9gV8n9Hrh2anTpTD93Dt62ypW3y""",
        t"""DsJzBnTnrYu1iwWRgBKrEYY46qAZIrA2xAwnm2X7uGR1hghkqDp0Vqj3kbSCz1XyfCs6_LehBwtxHIyh8Rip""",
        t"""y40p24moOAbgxVw3rxT_vlt3UVe4WO3JkJOzlpUf-KTVI2Ptgm-dARxTEtE-id-4OJr0h-K-VFs3VSndVTIz""",
        t"""nSxfyrj8ILL6MG_Uv8YAu7VILSB3lOW085-4qE3DzgrTjgyQ","p":"1r52Xk46c-LsfB5P442p7atdPUrxQ""",
        t"""Sy4mti_tZI3Mgf2EuFVbUoDBvaRQ-SWxkbkmoEzL7JXroSBjSrK3YIQgYdMgyAEPTPjXv_hI2_1eTSPVZfzL""",
        t"""0lffNn03IXqWF5MDFuoUYE0hzb2vhrlN_rKrbfDIwUbTrjjgieRbwC6Cl0","q":"wLb35x7hmQWZsWJmB_v""",
        t"""le87ihgZ19S8lBEROLIsZG4ayZVe9Hi9gDVCOBmUDdaDYVTSNx_8Fyw1YYa9XGrGnDew00J28cRUoeBB_jKI""",
        t"""1oma0Orv1T9aXIWxKwd4gvxFImOWr3QRL9KEBRzk2RatUBnmDZJTIAfwTs0g68UZHvtc","dp":"ZK-YwE7d""",
        t"""iUh0qR1tR7w8WHtolDx3MZ_OTowiFvgfeQ3SiresXjm9gZ5KLhMXvo-uz-KUJWDxS5pFQ_M0evdo1dKiRTjV""",
        t"""w_x4NyqyXPM5nULPkcpU827rnpZzAJKpdhWAgqrXGKAECQH0Xt4taznjnd_zVpAmZZq60WPMBMfKcuE","dq""",
        t"""":"Dq0gfgJ1DdFGXiLvQEZnuKEN0UUmsJBxkjydc3j4ZYdBiMRAy86x0vHCjywcMlYYg4yoC4YZa9hNVcsjq""",
        t"""A3FeiL19rk8g6Qn29Tt0cj8qqyFpz9vNDBUfCAiJVeESOjJDZPYHdHY8v1b-o-Z2X5tvLx-TCekf7oxyeKDU""",
        t"""qKWjis","qi":"VIMpMYbPf47dT1w_zDUXfPimsSegnMOA1zTaX7aGk_8urY6R8-ZW1FxU7AlWAyLWybqq6t""",
        t"""16VFd7hQd0y6flUK4SlOydB61gwanOsXGOAOv82cHq0E3eL4HrtZkUuKvnPrMnsUUFlfUdybVzxyjz9JF_Xy""",
        t"""aY14ardLSjf4L_FNY"}""" )
    . join
  val rsaOaepJwe: Text =
    List
      ( t"""eyJhbGciOiJSU0EtT0FFUCIsImVuYyI6IkEyNTZHQ00ifQ.OKOawDo13gRp2ojaHV7LFpZcgV7T6DVZKTyKO""",
        t"""MTYUmKoTCVJRgckCL9kiMT03JGeipsEdY3mx_etLbbWSrFr05kLzcSr4qKAq7YN7e9jwQRb23nfa6c9d-Stn""",
        t"""ImGyFDbSv04uVuxIp5Zms1gNxKKK2Da14B8S4rzVRltdYwam_lDp5XnZAYpQdb76FdIKLaVmqgfwX7XWRxv2""",
        t"""322i-vDxRfqNzo_tETKzpVLzfiwQyeyPGLBIO56YJ7eObdv0je81860ppamavo35UgoRdbYaBcoh9QcfylQr""",
        t"""66oc6vFWXRcZ_ZT2LawVCWTIy3brGPi6UklfCpIMfIjf7iGdXKHzg.48V1_ALb6US04U3b.5eym8TW_c8SuK""",
        t"""0ltJ3rpYIzOeDQz7TALvtu6UG9oMo4vpzs9tX_EFShS8iB7j6jiSdiwkIr3ajwQzaBtQD_A.XFBoMYUZodet""",
        t"""ZdvTiFvSkQ""" )
    . join

  // RFC 7516 §A.2: RSA1_5 with A128CBC-HS256
  val rsa15Key: Text =
    List
      ( t"""{"kty":"RSA","n":"sXchDaQebHnPiGvyDOAT4saGEUetSyo9MKLOoWFsueri23bOdgWp4Dy1WlUzewbgBH""",
        t"""od5pcM9H95GQRV3JDXboIRROSBigeC5yjU1hGzHHyXss8UDprecbAYxknTcQkhslANGRUZmdTOQ5qTRsLAt6""",
        t"""BTYuyvVRdhS8exSZEy_c4gs_7svlJJQ4H9_NxsiIoLwAEk7-Q3UXERGYw_75IDrGA84-lA_-Ct4eTlXHBIY2""",
        t"""EaV7t7LjJaynVJCpkv4LKjTTAumiGUIuQhrNhZLuF_RJLqHpM2kgWFLU7-VTdL1VbC2tejvcI2BlMkEpk1Bz""",
        t"""BZI0KQB0GaDWFLN-aEAw3vRw","e":"AQAB","d":"VFCWOqXr8nvZNyaaJLXdnNPXZKRaWCjkU5Q2egQQpT""",
        t"""BMwhprMzWzpR8Sxq1OPThh_J6MUD8Z35wky9b8eEO0pwNS8xlh1lOFRRBoNqDIKVOku0aZb-rynq8cxjDTLZ""",
        t"""Q6Fz7jSjR1Klop-YKaUHc9GsEofQqYruPhzSA-QgajZGPbE_0ZaVDJHfyd7UUBUKunFMScbflYAAOYJqVIVw""",
        t"""aYR5zWEEceUjNnTNo_CVSj-VvXLO5VZfCUAVLgW4dpf1SrtZjSt34YLsRarSb127reG_DUwg9Ch-KyvjT1Sk""",
        t"""HgUWRVGcyly7uvVGRSDwsXypdrNinPA4jlhoNdizK2zF2CWQ","p":"9gY2w6I6S6L0juEKsbeDAwpd9WMfg""",
        t"""qFoeA9vEyEUuk4kLwBKcoe1x4HG68ik918hdDSE9vDQSccA3xXHOAFOPJ8R9EeIAbTi1VwBYnbTp87X-xcPW""",
        t"""lEPkrdoUKW60tgs1aNd_Nnc9LEVVPMS390zbFxt8TN_biaBgelNgbC95sM","q":"uKlCKvKv_ZJMVcdIs5v""",
        t"""VSU_6cPtYI1ljWytExV_skstvRSNi9r66jdd9-yBhVfuG4shsp2j7rGnIio901RBeHo6TPKWVVykPu1iYhQX""",
        t"""w1jIABfw-MVsN-3bQ76WLdt2SDxsHs7q7zPyUyHXmps7ycZ5c72wGkUwNOjYelmkiNS0","dp":"w0kZbV63""",
        t"""cVRvVX6yk3C8cMxo2qCM4Y8nsq1lmMSYhG4EcL6FWbX5h9yuvngs4iLEFk6eALoUS4vIWEwcL4txw9LsWH_z""",
        t"""KI-hwoReoP77cOdSL4AVcraHawlkpyd2TWjE5evgbhWtOxnZee3cXJBkAi64Ik6jZxbvk-RR3pEhnCs","dq""",
        t"""":"o_8V14SezckO6CNLKs_btPdFiO9_kC1DsuUTd2LAfIIVeMZ7jn1Gus_Ff7B7IVx3p5KuBGOVF8L-qifLb""",
        t"""6nQnLysgHDh132NDioZkhH7mI7hPG-PYE_odApKdnqECHWw0J-F0JWnUd6D2B_1TvF9mXA2Qx-iGYn8OVV1B""",
        t"""smp6qU","qi":"eNho5yRBEBxhGBtQRww9QirZsB66TrfFReG_CcteI1aCneT0ELGhYlRlCtUkTRclIfuEPm""",
        t"""NsNDPbLoLqqCVznFbvdB7x-Tl-m0l_eFTj2KiqwGqE9PZB9nNTwMVvH3VRRSLWACvPnSiwP8N5Usy-WRXS-V""",
        t"""7TbpxIhvepTfE0NNo"}""" )
    . join
  val rsa15Jwe: Text =
    List
      ( t"""eyJhbGciOiJSU0ExXzUiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0.UGhIOguC7IuEvf_NPVaXsGMoLOmwvc1G""",
        t"""yqlIKOK1nN94nHPoltGRhWhw7Zx0-kFm1NJn8LE9XShH59_i8J0PH5ZZyNfGy2xGdULU7sHNF6Gp2vPLgNZ_""",
        t"""_deLKxGHZ7PcHALUzoOegEI-8E66jX2E4zyJKx-YxzZIItRzC5hlRirb6Y5Cl_p-ko3YvkkysZIFNPccxRU7""",
        t"""qve1WYPxqbb2Yw8kZqa2rMWI5ng8OtvzlV7elprCbuPhcCdZ6XDP0_F8rkXds2vE4X-ncOIM8hAYHHi29NX0""",
        t"""mcKiRaD0-D-ljQTP-cFPgwCp6X-nZZd9OHBv-B3oWh2TbqmScqXMR4gp_A.AxY8DCtDaGlsbGljb3RoZQ.KD""",
        t"""lTtXchhZTGufMYmOYGS4HffxPSUrfmqCHXaI9wOGY.9hH0vgRfYgPnAHOd8stkvw""" )
    . join

  // RFC 7516 §A.3: A128KW with A128CBC-HS256
  val aesKwKey: Text = t"""{"kty":"oct","k":"GawgguFyGrWKav7AX4VKUg"}"""
  val aesKwJwe: Text =
    List
      ( t"""eyJhbGciOiJBMTI4S1ciLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0.6KB707dM9YTIgHtLvtgWQ8mKwboJW3of""",
        t"""9locizkDTHzBC2IlrT1oOQ.AxY8DCtDaGlsbGljb3RoZQ.KDlTtXchhZTGufMYmOYGS4HffxPSUrfmqCHXaI""",
        t"""9wOGY.U0m_YmjN04DJvceFICbCVQ""" )
    . join

  // RFC 7516 §A.4.7
  val generalJwe: Text =
    List
      ( t"""{"protected":"eyJlbmMiOiJBMTI4Q0JDLUhTMjU2In0","unprotected":{"jku":"https://server.""",
        t"""example.com/keys.jwks"},"recipients":[{"header":{"alg":"RSA1_5","kid":"2011-04-29"},""",
        t""""encrypted_key":"UGhIOguC7IuEvf_NPVaXsGMoLOmwvc1GyqlIKOK1nN94nHPoltGRhWhw7Zx0-kFm1NJ""",
        t"""n8LE9XShH59_i8J0PH5ZZyNfGy2xGdULU7sHNF6Gp2vPLgNZ__deLKxGHZ7PcHALUzoOegEI-8E66jX2E4zy""",
        t"""JKx-YxzZIItRzC5hlRirb6Y5Cl_p-ko3YvkkysZIFNPccxRU7qve1WYPxqbb2Yw8kZqa2rMWI5ng8OtvzlV7""",
        t"""elprCbuPhcCdZ6XDP0_F8rkXds2vE4X-ncOIM8hAYHHi29NX0mcKiRaD0-D-ljQTP-cFPgwCp6X-nZZd9OHB""",
        t"""v-B3oWh2TbqmScqXMR4gp_A"},{"header":{"alg":"A128KW","kid":"7"},"encrypted_key":"6KB7""",
        t"""07dM9YTIgHtLvtgWQ8mKwboJW3of9locizkDTHzBC2IlrT1oOQ"}],"iv":"AxY8DCtDaGlsbGljb3RoZQ",""",
        t""""ciphertext":"KDlTtXchhZTGufMYmOYGS4HffxPSUrfmqCHXaI9wOGY","tag":"Mz-VPPyU4RlcuYv1Iw""",
        t"""Ivzw"}""" )
    . join

  // RFC 7516 §A.5
  val flattenedJwe: Text =
    List
      ( t"""{"protected":"eyJlbmMiOiJBMTI4Q0JDLUhTMjU2In0","unprotected":{"jku":"https://server.""",
        t"""example.com/keys.jwks"},"header":{"alg":"A128KW","kid":"7"},"encrypted_key":"6KB707d""",
        t"""M9YTIgHtLvtgWQ8mKwboJW3of9locizkDTHzBC2IlrT1oOQ","iv":"AxY8DCtDaGlsbGljb3RoZQ","ciph""",
        t"""ertext":"KDlTtXchhZTGufMYmOYGS4HffxPSUrfmqCHXaI9wOGY","tag":"Mz-VPPyU4RlcuYv1IwIvzw"""",
        t"""}""" )
    . join

  // RFC 7517 §A.1
  val publicKeys: Text =
    List
      ( t"""{"keys":[{"kty":"EC","crv":"P-256","x":"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4"""",
        t""","y":"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM","use":"enc","kid":"1"},{"kty":"RS""",
        t"""A","n": "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFx""",
        t"""uhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h""",
        t"""4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbI""",
        t"""SD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEg""",
        t"""U8awapJzKnqDKgw","e":"AQAB","alg":"RS256","kid":"2011-04-29"}]}""" )
    . join

  // RFC 7517 §A.2
  val privateKeys: Text =
    List
      ( t"""{"keys":[{"kty":"EC","crv":"P-256","x":"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4"""",
        t""","y":"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM","d":"870MB6gfuTJ4HtUnUvYMyJpr5eUZ""",
        t"""NP4Bk43bVdj3eAE","use":"enc","kid":"1"},{"kty":"RSA","n":"0vx7agoebGcQSuuPiLJXZptN9n""",
        t"""ndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3ok""",
        t"""njhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAt""",
        t"""aSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4""",
        t"""lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw","e":"AQAB","d":"X4""",
        t"""cTteJY_gn4FYPsXB8rdXix5vwsg1FLN5E3EaG6RJoVH-HLLKD9M7dx5oo7GURknchnrRweUkC7hT5fJLM0Wb""",
        t"""FAKNLWY2vv7B6NqXSzUvxT0_YSfqijwp3RTzlBaCxWp4doFk5N2o8Gy_nHNKroADIkJ46pRUohsXywbReAdY""",
        t"""aMwFs9tv8d_cPVY3i07a3t8MN6TNwm0dSawm9v47UiCl3Sk5ZiG7xojPLu4sbg1U2jx4IBTNBznbJSzFHK66""",
        t"""jT8bgkuqsk0GjskDJk19Z4qwjwbsnn4j2WBii3RL-Us2lGVkY8fkFzme1z0HbIkfz0Y6mqnOYtqc0X4jfcKo""",
        t"""AC8Q","p":"83i-7IvMGXoMXCskv73TKr8637FiO7Z27zv8oj6pbWUQyLPQBQxtPVnwD20R-60eTDmD2ujnM""",
        t"""t5PoqMrm8RfmNhVWDtjjMmCMjOpSXicFHj7XOuVIYQyqVWlWEh6dN36GVZYk93N8Bc9vY41xy8B9RzzOGVQz""",
        t"""XvNEvn7O0nVbfs","q":"3dfOR9cuYq-0S-mkFLzgItgMEfFzB2q3hWehMuG0oCuqnb3vobLyumqjVZQO1dI""",
        t"""rdwgTnCdpYzBcOfW5r370AFXjiWft_NGEiovonizhKpo9VVS78TzFgxkIdrecRezsZ-1kYd_s1qDbxtkDEgf""",
        t"""AITAG9LUnADun4vIcb6yelxk","dp":"G4sPXkc6Ya9y8oJW9_ILj4xuppu0lzi_H7VTkS8xj5SdX3coE0oi""",
        t"""mYwxIi2emTAue0UOa5dpgFGyBJ4c8tQ2VF402XRugKDTP8akYhFo5tAA77Qe_NmtuYZc3C3m3I24G2GvR5sS""",
        t"""DxUyAN2zq8Lfn9EUms6rY3Ob8YeiKkTiBj0","dq":"s9lAH9fggBsoFR8Oac2R_E2gw282rT2kGOAhvIllE""",
        t"""TE1efrA6huUUvMfBcMpn8lqeW6vzznYY5SSQF7pMdC_agI3nG8Ibp1BUb0JUiraRNqUfLhcQb_d9GF4Dh7e7""",
        t"""4WbRsobRonujTYN1xCaP6TO61jvWrX-L18txXw494Q_cgk","qi":"GyM_p6JrXySiz1toFgKbWV-JdI3jQ4""",
        t"""ypu9rbMWx3rQJBfmt0FoYzgUIZEVFEcOqwemRN81zoDAaa-Bk0KWNGDjJHZDdDmFhW3AN7lI-puxk_mHZGJ1""",
        t"""1rxyR8O55XLSe3SPmRfKwZI6yU24ZxvQKFYItdldUKGzO6Ia6zTKhAVRU","alg":"RS256","kid":"2011""",
        t"""-04-29"}]}""" )
    . join

  // RFC 7517 §A.3
  val symmetricKeys: Text =
    List
      ( t"""{"keys":[{"kty":"oct","alg":"A128KW","k":"GawgguFyGrWKav7AX4VKUg"},{"kty":"oct","k":""",
        t""""AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9C""",
        t"""Aow","kid":"HMAC key used in JWS spec Appendix A.1 example"}]}""" )
    . join

  // RFC 7517 §C.9: an RSA key encrypted under a password
  val passwordJwe: Text =
    List
      ( t"""eyJhbGciOiJQQkVTMi1IUzI1NitBMTI4S1ciLCJwMnMiOiIyV0NUY0paMVJ2ZF9DSnVKcmlwUTF3IiwicDJj""",
        t"""Ijo0MDk2LCJlbmMiOiJBMTI4Q0JDLUhTMjU2IiwiY3R5IjoiandrK2pzb24ifQ.TrqXOwuNUfDV9VPTNbyGv""",
        t"""EJ9JMjefAVn-TR1uIxR9p6hsRQh9Tk7BA.Ye9j1qs22DmRSAddIh-VnA.AwhB8lxrlKjFn02LGWEqg27H4Tg""",
        t"""9fyZAbFv3p5ZicHpj64QyHC44qqlZ3JEmnZTgQowIqZJ13jbyHB8LgePiqUJ1hf6M2HPLgzw8L-mEeQ0jvDU""",
        t"""TrE07NtOerBk8bwBQyZ6g0kQ3DEOIglfYxV8-FJvNBYwbqN1Bck6d_i7OtjSHV-8DIrp-3JcRIe05YKy3Oi3""",
        t"""4Z_GOiAc1EK21B11c_AE11PII_wvvtRiUiG8YofQXakWd1_O98Kap-UgmyWPfreUJ3lJPnbD4Ve95owEfMGL""",
        t"""OPflo2MnjaTDCwQokoJ_xplQ2vNPz8iguLcHBoKllyQFJL2mOWBwqhBo9Oj-O800as5mmLsvQMTflIrIEbbT""",
        t"""MzHMBZ8EFW9fWwwFu0DWQJGkMNhmBZQ-3lvqTc-M6-gWA6D8PDhONfP2Oib2HGizwG1iEaX8GRyUpfLuljCL""",
        t"""Ie1DkGOewhKuKkZh04DKNM5Nbugf2atmU9OP0Ldx5peCUtRG1gMVl7Qup5ZXHTjgPDr5b2N731UooCGAUqHd""",
        t"""gGhg0JVJ_ObCTdjsH4CF1SJsdUhrXvYx3HJh2Xd7CwJRzU_3Y1GxYU6-s3GFPbirfqqEipJDBTHpcoCmyrwY""",
        t"""jYHFgnlqBZRotRrS95g8F95bRXqsaDY7UgQGwBQBwy665d0zpvTasvfXf_c0MWAl-neFaKOW_Px6g4EUDjG1""",
        t"""GWSXV9cLStLw_0ovdApDIFLHYHePyagyHjouQUuGiq7BsYwYrwaF06tgB8hV8omLNfMEmDPJaZUzMuHw6tBD""",
        t"""wGkzD-tS_ub9hxrpJ4UsOWnt5rGUyoN2N_c1-TQlXxm5oto14MxnoAyBQBpwIEgSH3Y4ZhwKBhHPjSo0cdwu""",
        t"""NdYbGPpb-YUvF-2NZzODiQ1OvWQBRHSbPWYz_xbGkgD504LRtqRwCO7CC_CyyURi1sEssPVsMJRX_U4LFEOc""",
        t"""82TiDdqjKOjRUfKK5rqLi8nBE9soQ0DSaOoFQZiGrBrqxDsNYiAYAmxxkos-i3nX4qtByVx85sCE5U_0MqG7""",
        t"""COxZWMOPEFrDaepUV-cOyrvoUIng8i8ljKBKxETY2BgPegKBYCxsAUcAkKamSCC9AiBxA0UOHyhTqtlvMksO""",
        t"""7AEhNC2-YzPyx1FkhMoS4LLe6E_pFsMlmjA6P1NSge9C5G5tETYXGAn6b1xZbHtmwrPScro9LWhVmAaA7_bx""",
        t"""YObnFUxgWtK4vzzQBjZJ36UTk4OTB-JvKWgfVWCFsaw5WCHj6Oo4jpO7d2yN7WMfAj2hTEabz9wumQ0TMhBd""",
        t"""uZ-QON3pYObSy7TSC1vVme0NJrwF_cJRehKTFmdlXGVldPxZCplr7ZQqRQhF8JP-l4mEQVnCaWGn9ONHlemc""",
        t"""zGOS-A-wwtnmwjIB1V_vgJRf4FdpV-4hUk4-QLpu3-1lWFxrtZKcggq3tWTduRo5_QebQbUUT_VSCgsFcOmy""",
        t"""WKoj56lbxthN19hq1XGWbLGfrrR6MWh23vk01zn8FVwi7uFwEnRYSafsnWLa1Z5TpBj9GvAdl2H9NHwzpB5N""",
        t"""qHpZNkQ3NMDj13Fn8fzO0JB83Etbm_tnFQfcb13X3bJ15Cz-Ww1MGhvIpGGnMBT_ADp9xSIyAM9dQ1yeVXk-""",
        t"""AIgWBUlN5uyWSGyCxp0cJwx7HxM38z0UIeBu-MytL-eqndM7LxytsVzCbjOTSVRmhYEMIzUAnS1gs7uMQAGR""",
        t"""dgRIElTJESGMjb_4bZq9s6Ve1LKkSi0_QDsrABaLe55UY0zF4ZSfOV5PMyPtocwV_dcNPlxLgNAD1BFX_Z9k""",
        t"""AdMZQW6fAmsfFle0zAoMe4l9pMESH0JB4sJGdCKtQXj1cXNydDYozF7l8H00BV_Er7zd6VtIw0MxwkFCTats""",
        t"""v_R-GsBCH218RgVPsfYhwVuT8R4HarpzsDBufC4r8_c8fc9Z278sQ081jFjOja6L2x0N_ImzFNXU6xwO-Ska""",
        t"""-QeuvYZ3X_L31ZOX4Llp-7QSfgDoHnOxFv1Xws-D5mDHD3zxOup2b2TppdKTZb9eW2vxUVviM8OI9atBfPKM""",
        t"""GAOv9omA-6vv5IxUH0-lWMiHLQ_g8vnswp-Jav0c4t6URVUzujNOoNd_CBGGVnHiJTCHl88LQxsqLHHIu4Fz""",
        t"""-U2SGnlxGTj0-ihit2ELGRv4vO8E1BosTmf0cx3qgG0Pq0eOLBDIHsrdZ_CCAiTc0HVkMbyq1M6qEhM-q5P6""",
        t"""y1QCIrwg.0HFmhOzsQ98nNWJjIHkR7A""" )
    . join

  // RFC 7638 §3.1
  val thumbprintKey: Text =
    List
      ( t"""{"kty": "RSA","n": "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86z""",
        t"""wu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt""",
        t"""7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajr""",
        t"""n1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44""",
        t"""-csFCur-kEgU8awapJzKnqDKgw","e": "AQAB","alg": "RS256","kid": "2011-04-29"}""" )
    . join

  // RFC 8037 §A.1
  val ed25519Key: Text =
    List
      ( t"""{"kty":"OKP","crv":"Ed25519","d":"nWGxne_9WmC6hEr0kuwsxERJxWl7MmkZcDusAxyuf2A","x":"""",
        t"""11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo"}""" )
    . join

  // RFC 8037 §A.4
  val ed25519Jws: Text =
    List
      ( t"""eyJhbGciOiJFZERTQSJ9.RXhhbXBsZSBvZiBFZDI1NTE5IHNpZ25pbmc.hgyY0il_MGCjP0JzlnLWG1PPOt7""",
        t"""-09PGcvMg3AIbQR6dWbhijcNR4ki4iylGjg5BhVsPt9g7sVvpAr_MuM0KAg""" )
    . join
