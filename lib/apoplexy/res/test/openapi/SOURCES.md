# OpenAPI corpus

Real-world OpenAPI documents, checked in unmodified so that the suites run offline. Each row
gives the upstream file, the commit it was taken from (or the fetch date for a live endpoint),
its licence and its size. The `local/` directory holds the hand-written fixtures the client
tests use.

| Path | Upstream | Commit / fetched | Licence | Bytes |
|---|---|---|---|---|
| `oai/v3.0/*.{json,yaml}` | https://github.com/OAI/learn.openapis.org (`examples/v3.0/`) | `bbb743ed3b7c5ed76b6e6ba9b302af38f3956c44` | CC-BY-4.0 (OpenAPI Initiative) | 4 KB – 9 KB each |
| `oai/v3.1/*.{json,yaml}` | https://github.com/OAI/learn.openapis.org (`examples/v3.1/`) | `bbb743ed3b7c5ed76b6e6ba9b302af38f3956c44` | CC-BY-4.0 (OpenAPI Initiative) | 0.4 KB – 7 KB each |
| `swagger/petstore3.json` | https://petstore3.swagger.io/api/v3/openapi.json (https://github.com/swagger-api/swagger-petstore) | fetched 2026-09-27 | Apache-2.0 | 17,106 |
| `redocly/museum.yaml` | https://github.com/Redocly/museum-openapi-example (`openapi.yaml`) | `2770b2b2e59832d245c7b0eb0badf6568d7efb53` | MIT | 23,137 |
| `twilio/accounts_v1.{json,yaml}` | https://github.com/twilio/twilio-oai (`spec/json/twilio_accounts_v1.json`, `spec/yaml/twilio_accounts_v1.yaml`) | `5aa7f31977ce5812f7b7bc1f46a38555ebaa2888` | MIT | 89,195 / 67,944 |
| `twilio/lookups_v2.json` | https://github.com/twilio/twilio-oai (`spec/json/twilio_lookups_v2.json`) | `5aa7f31977ce5812f7b7bc1f46a38555ebaa2888` | MIT | 111,570 |
| `kubernetes/version.json` | https://github.com/kubernetes/kubernetes (`api/openapi-spec/v3/version_openapi.json`) | `6c1c7702cf2052245ef10e699d45f071af306f59` | Apache-2.0 | 3,124 |
| `kubernetes/rbac.json` | https://github.com/kubernetes/kubernetes (`api/openapi-spec/v3/apis__rbac.authorization.k8s.io__v1_openapi.json`) | `6c1c7702cf2052245ef10e699d45f071af306f59` | Apache-2.0 | 404,130 |
| `discord/openapi.json` | https://github.com/discord/discord-api-spec (`specs/openapi.json`) | `bb8eb1ec745a3dfc1e06c40b6e641c9738393df6` | MIT | 1,192,037 |
| `box/openapi.json` | https://github.com/box/box-openapi (`openapi/openapi.json`) | `933ec4c5d9f4dbdaaa823efa3a083cf177aecbde` | Apache-2.0 | 1,780,704 |
| `stripe/spec3.json` | https://github.com/stripe/openapi (`openapi/spec3.json`) | `18fa2cc768024b47789dc6fa243acd48478acac2` | MIT | 8,316,935 |

The online suite (`CorpusOnlineTests`, run only with `SOUNDNESS_CI_ONLINE=1`) fetches the
descriptions too large to check in, pinned to a commit: GitHub's REST API
(https://github.com/github/rest-api-description, MIT), OpenAI's
(https://github.com/openai/openai-openapi, MIT) and Cloudflare's
(https://github.com/cloudflare/api-schemas, BSD-3-Clause).
