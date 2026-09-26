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

package jacinta

import soundness.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely

// Real-world JSON Schema documents, as published, each bound to a `Json.Provider` for the
// provider tests. They live in this file, compiled before the tests, so that the `record`
// macro can evaluate each provider while the test call sites are being compiled. Sources:
// json-schema.org's examples, the GeoJSON schema, the JSON Resume schema, and SchemaStore.


// address.schema.json (728 bytes)
object AddressSchema extends Json.Provider(t"""{
 "$$id": "https://example.com/address.schema.json",
 "$$schema": "https://json-schema.org/draft/2020-12/schema",
 "description": "An address similar to http://microformats.org/wiki/h-card",
 "type": "object",
 "properties": {
  "post-office-box": {
   "type": "string"
  },
  "extended-address": {
   "type": "string"
  },
  "street-address": {
   "type": "string"
  },
  "locality": {
   "type": "string"
  },
  "region": {
   "type": "string"
  },
  "postal-code": {
   "type": "string"
  },
  "country-name": {
   "type": "string"
  }
 },
 "required": [
  "locality",
  "region",
  "country-name"
 ],
 "dependentRequired": {
  "post-office-box": [
   "street-address"
  ],
  "extended-address": [
   "street-address"
  ]
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// geographical-location.schema.json (461 bytes)
object GeographicalLocationSchema extends Json.Provider(t"""{
 "$$id": "https://example.com/geographical-location.schema.json",
 "$$schema": "https://json-schema.org/draft/2020-12/schema",
 "title": "Longitude and Latitude Values",
 "description": "A geographical coordinate.",
 "required": [
  "latitude",
  "longitude"
 ],
 "type": "object",
 "properties": {
  "latitude": {
   "type": "number",
   "minimum": -90,
   "maximum": 90
  },
  "longitude": {
   "type": "number",
   "minimum": -180,
   "maximum": 180
  }
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// calendar.schema.json (915 bytes)
object CalendarSchema extends Json.Provider(t"""{
 "$$id": "https://example.com/calendar.schema.json",
 "$$schema": "https://json-schema.org/draft/2020-12/schema",
 "description": "A representation of an event",
 "type": "object",
 "required": [
  "dtstart",
  "summary"
 ],
 "properties": {
  "dtstart": {
   "type": "string",
   "description": "Event starting time"
  },
  "dtend": {
   "type": "string",
   "description": "Event ending time"
  },
  "summary": {
   "type": "string"
  },
  "location": {
   "type": "string"
  },
  "url": {
   "type": "string"
  },
  "duration": {
   "type": "string",
   "description": "Event duration"
  },
  "rdate": {
   "type": "string",
   "description": "Recurrence date"
  },
  "rrule": {
   "type": "string",
   "description": "Recurrence rule"
  },
  "category": {
   "type": "string"
  },
  "description": {
   "type": "string"
  },
  "geo": {
   "$$ref": "https://example.com/geographical-location.schema.json"
  }
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// card.schema.json (1653 bytes)
object CardSchema extends Json.Provider(t"""{
 "$$id": "https://example.com/card.schema.json",
 "$$schema": "https://json-schema.org/draft/2020-12/schema",
 "description": "A representation of a person, company, organization, or place",
 "type": "object",
 "required": [
  "familyName",
  "givenName"
 ],
 "properties": {
  "fn": {
   "description": "Formatted Name",
   "type": "string"
  },
  "familyName": {
   "type": "string"
  },
  "givenName": {
   "type": "string"
  },
  "additionalName": {
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "honorificPrefix": {
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "honorificSuffix": {
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "nickname": {
   "type": "string"
  },
  "url": {
   "type": "string"
  },
  "email": {
   "type": "object",
   "properties": {
    "type": {
     "type": "string"
    },
    "value": {
     "type": "string"
    }
   }
  },
  "tel": {
   "type": "object",
   "properties": {
    "type": {
     "type": "string"
    },
    "value": {
     "type": "string"
    }
   }
  },
  "adr": {
   "$$ref": "https://example.com/address.schema.json"
  },
  "geo": {
   "$$ref": "https://example.com/geographical-location.schema.json"
  },
  "tz": {
   "type": "string"
  },
  "photo": {
   "type": "string"
  },
  "logo": {
   "type": "string"
  },
  "sound": {
   "type": "string"
  },
  "bday": {
   "type": "string"
  },
  "title": {
   "type": "string"
  },
  "role": {
   "type": "string"
  },
  "org": {
   "type": "object",
   "properties": {
    "organizationName": {
     "type": "string"
    },
    "organizationUnit": {
     "type": "string"
    }
   }
  }
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// Point.json (482 bytes)
object GeoJsonPointSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://geojson.org/schema/Point.json",
 "title": "GeoJSON Point",
 "type": "object",
 "required": [
  "type",
  "coordinates"
 ],
 "properties": {
  "type": {
   "type": "string",
   "enum": [
    "Point"
   ]
  },
  "coordinates": {
   "type": "array",
   "minItems": 2,
   "items": {
    "type": "number"
   }
  },
  "bbox": {
   "type": "array",
   "minItems": 4,
   "items": {
    "type": "number"
   }
  }
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// Feature.json (9581 bytes)
object GeoJsonFeatureSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://geojson.org/schema/Feature.json",
 "title": "GeoJSON Feature",
 "type": "object",
 "required": [
  "type",
  "properties",
  "geometry"
 ],
 "properties": {
  "type": {
   "type": "string",
   "enum": [
    "Feature"
   ]
  },
  "id": {
   "oneOf": [
    {
     "type": "number"
    },
    {
     "type": "string"
    }
   ]
  },
  "properties": {
   "oneOf": [
    {
     "type": "null"
    },
    {
     "type": "object"
    }
   ]
  },
  "geometry": {
   "oneOf": [
    {
     "type": "null"
    },
    {
     "title": "GeoJSON Point",
     "type": "object",
     "required": [
      "type",
      "coordinates"
     ],
     "properties": {
      "type": {
       "type": "string",
       "enum": [
        "Point"
       ]
      },
      "coordinates": {
       "type": "array",
       "minItems": 2,
       "items": {
        "type": "number"
       }
      },
      "bbox": {
       "type": "array",
       "minItems": 4,
       "items": {
        "type": "number"
       }
      }
     }
    },
    {
     "title": "GeoJSON LineString",
     "type": "object",
     "required": [
      "type",
      "coordinates"
     ],
     "properties": {
      "type": {
       "type": "string",
       "enum": [
        "LineString"
       ]
      },
      "coordinates": {
       "type": "array",
       "minItems": 2,
       "items": {
        "type": "array",
        "minItems": 2,
        "items": {
         "type": "number"
        }
       }
      },
      "bbox": {
       "type": "array",
       "minItems": 4,
       "items": {
        "type": "number"
       }
      }
     }
    },
    {
     "title": "GeoJSON Polygon",
     "type": "object",
     "required": [
      "type",
      "coordinates"
     ],
     "properties": {
      "type": {
       "type": "string",
       "enum": [
        "Polygon"
       ]
      },
      "coordinates": {
       "type": "array",
       "items": {
        "type": "array",
        "minItems": 4,
        "items": {
         "type": "array",
         "minItems": 2,
         "items": {
          "type": "number"
         }
        }
       }
      },
      "bbox": {
       "type": "array",
       "minItems": 4,
       "items": {
        "type": "number"
       }
      }
     }
    },
    {
     "title": "GeoJSON MultiPoint",
     "type": "object",
     "required": [
      "type",
      "coordinates"
     ],
     "properties": {
      "type": {
       "type": "string",
       "enum": [
        "MultiPoint"
       ]
      },
      "coordinates": {
       "type": "array",
       "items": {
        "type": "array",
        "minItems": 2,
        "items": {
         "type": "number"
        }
       }
      },
      "bbox": {
       "type": "array",
       "minItems": 4,
       "items": {
        "type": "number"
       }
      }
     }
    },
    {
     "title": "GeoJSON MultiLineString",
     "type": "object",
     "required": [
      "type",
      "coordinates"
     ],
     "properties": {
      "type": {
       "type": "string",
       "enum": [
        "MultiLineString"
       ]
      },
      "coordinates": {
       "type": "array",
       "items": {
        "type": "array",
        "minItems": 2,
        "items": {
         "type": "array",
         "minItems": 2,
         "items": {
          "type": "number"
         }
        }
       }
      },
      "bbox": {
       "type": "array",
       "minItems": 4,
       "items": {
        "type": "number"
       }
      }
     }
    },
    {
     "title": "GeoJSON MultiPolygon",
     "type": "object",
     "required": [
      "type",
      "coordinates"
     ],
     "properties": {
      "type": {
       "type": "string",
       "enum": [
        "MultiPolygon"
       ]
      },
      "coordinates": {
       "type": "array",
       "items": {
        "type": "array",
        "items": {
         "type": "array",
         "minItems": 4,
         "items": {
          "type": "array",
          "minItems": 2,
          "items": {
           "type": "number"
          }
         }
        }
       }
      },
      "bbox": {
       "type": "array",
       "minItems": 4,
       "items": {
        "type": "number"
       }
      }
     }
    },
    {
     "title": "GeoJSON GeometryCollection",
     "type": "object",
     "required": [
      "type",
      "geometries"
     ],
     "properties": {
      "type": {
       "type": "string",
       "enum": [
        "GeometryCollection"
       ]
      },
      "geometries": {
       "type": "array",
       "items": {
        "oneOf": [
         {
          "title": "GeoJSON Point",
          "type": "object",
          "required": [
           "type",
           "coordinates"
          ],
          "properties": {
           "type": {
            "type": "string",
            "enum": [
             "Point"
            ]
           },
           "coordinates": {
            "type": "array",
            "minItems": 2,
            "items": {
             "type": "number"
            }
           },
           "bbox": {
            "type": "array",
            "minItems": 4,
            "items": {
             "type": "number"
            }
           }
          }
         },
         {
          "title": "GeoJSON LineString",
          "type": "object",
          "required": [
           "type",
           "coordinates"
          ],
          "properties": {
           "type": {
            "type": "string",
            "enum": [
             "LineString"
            ]
           },
           "coordinates": {
            "type": "array",
            "minItems": 2,
            "items": {
             "type": "array",
             "minItems": 2,
             "items": {
              "type": "number"
             }
            }
           },
           "bbox": {
            "type": "array",
            "minItems": 4,
            "items": {
             "type": "number"
            }
           }
          }
         },
         {
          "title": "GeoJSON Polygon",
          "type": "object",
          "required": [
           "type",
           "coordinates"
          ],
          "properties": {
           "type": {
            "type": "string",
            "enum": [
             "Polygon"
            ]
           },
           "coordinates": {
            "type": "array",
            "items": {
             "type": "array",
             "minItems": 4,
             "items": {
              "type": "array",
              "minItems": 2,
              "items": {
               "type": "number"
              }
             }
            }
           },
           "bbox": {
            "type": "array",
            "minItems": 4,
            "items": {
             "type": "number"
            }
           }
          }
         },
         {
          "title": "GeoJSON MultiPoint",
          "type": "object",
          "required": [
           "type",
           "coordinates"
          ],
          "properties": {
           "type": {
            "type": "string",
            "enum": [
             "MultiPoint"
            ]
           },
           "coordinates": {
            "type": "array",
            "items": {
             "type": "array",
             "minItems": 2,
             "items": {
              "type": "number"
             }
            }
           },
           "bbox": {
            "type": "array",
            "minItems": 4,
            "items": {
             "type": "number"
            }
           }
          }
         },
         {
          "title": "GeoJSON MultiLineString",
          "type": "object",
          "required": [
           "type",
           "coordinates"
          ],
          "properties": {
           "type": {
            "type": "string",
            "enum": [
             "MultiLineString"
            ]
           },
           "coordinates": {
            "type": "array",
            "items": {
             "type": "array",
             "minItems": 2,
             "items": {
              "type": "array",
              "minItems": 2,
              "items": {
               "type": "number"
              }
             }
            }
           },
           "bbox": {
            "type": "array",
            "minItems": 4,
            "items": {
             "type": "number"
            }
           }
          }
         },
         {
          "title": "GeoJSON MultiPolygon",
          "type": "object",
          "required": [
           "type",
           "coordinates"
          ],
          "properties": {
           "type": {
            "type": "string",
            "enum": [
             "MultiPolygon"
            ]
           },
           "coordinates": {
            "type": "array",
            "items": {
             "type": "array",
             "items": {
              "type": "array",
              "minItems": 4,
              "items": {
               "type": "array",
               "minItems": 2,
               "items": {
                "type": "number"
               }
              }
             }
            }
           },
           "bbox": {
            "type": "array",
            "minItems": 4,
            "items": {
             "type": "number"
            }
           }
          }
         }
        ]
       }
      },
      "bbox": {
       "type": "array",
       "minItems": 4,
       "items": {
        "type": "number"
       }
      }
     }
    }
   ]
  },
  "bbox": {
   "type": "array",
   "minItems": 4,
   "items": {
    "type": "number"
   }
  }
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// lerna.json (4078 bytes)
object LernaSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/lerna",
 "description": "Lerna is a tool used in JavaScript monorepo projects. The lerna.json file is\\nused to configure lerna to to best fit your project.",
 "properties": {
  "version": {
   "description": "The current version of the repository (or independent).",
   "type": "string"
  },
  "npmClient": {
   "description": "Specify which client to run commands with (change to \\"yarn\\" to run commands with yarn. Defaults to \\"npm\\".",
   "type": "string"
  },
  "npmClientArgs": {
   "description": "Array of strings that will be passed as arguments to the npmClient.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "useWorkspaces": {
   "description": "Enable workspaces integration when using Yarn.",
   "type": "boolean"
  },
  "workspaces": {
   "description": "Array of globs to use a workspace locations.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "packages": {
   "description": "Array of globs to use a package locations.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "ignoreChanges": {
   "description": "Array of globs of files to ignore when detecting changed packages.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "command": {
   "description": "Options for the CLI commands.",
   "type": "object",
   "properties": {
    "publish": {
     "description": "Options for the publish command.",
     "type": "object",
     "properties": {
      "ignoreChanges": {
       "description": "An array of globs that won't be included in \\"lerna changed/publish\\". Use this to prevent publishing of a new version unnecessarily for changes, such as fixing a README.md typo.",
       "type": [
        "string",
        "array"
       ],
       "items": {
        "type": "string"
       }
      },
      "message": {
       "description": "A custom commit message when performing version updates for publication. See https://github.com/lerna/lerna/tree/master/commands/version#--message-msg for more information.",
       "type": "string"
      }
     }
    },
    "bootstrap": {
     "description": "Options for the bootstrap command.",
     "type": "object",
     "properties": {
      "ignore": {
       "description": "An array of globs that won't be bootstrapped when running \\"lerna bootstrap\\" command.",
       "type": [
        "string",
        "array"
       ],
       "items": {
        "type": "string"
       }
      },
      "npmClientArgs": {
       "description": "Array of strings that will be passed as arguments directly to \\"npm install\\" during the \\"lerna bootstrap\\" command.",
       "type": "array",
       "items": {
        "type": "string"
       }
      }
     }
    },
    "init": {
     "description": "Options for the init command.",
     "type": "object",
     "properties": {
      "exact": {
       "description": "Use lerna 1.x behavior of \\"exact\\" comparison. It will enforce the exact match for all subsequent executions.",
       "type": "boolean"
      }
     }
    },
    "run": {
     "description": "Options for the run command.",
     "type": "object",
     "properties": {
      "npmClient": {
       "description": "Which npm client should be used when running package scripts.",
       "type": "string"
      }
     }
    },
    "version": {
     "description": "Options for the version command.",
     "type": "object",
     "properties": {
      "allowBranch": {
       "description": "A whitelist of globs that match git branches where \\"lerna version\\" is enabled.",
       "type": [
        "string",
        "array"
       ],
       "items": {
        "type": "string"
       }
      },
      "message": {
       "description": "A custom commit message when performing version updates for publication. See https://github.com/lerna/lerna/tree/master/commands/version#--message-msg for more information.",
       "type": "string"
      }
     }
    }
   }
  }
 },
 "title": "A JSON schema for lerna.json files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// nycrc.json (1793 bytes)
object NycrcSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/nycrc.json",
 "properties": {
  "extends": {
   "description": "Name of configuration to extend from.",
   "type": "string"
  },
  "all": {
   "description": "Whether or not to instrument all files (not just the ones touched by your test suite).",
   "type": "boolean",
   "default": false
  },
  "check-coverage": {
   "description": "Check whether coverage is within thresholds, fail if not",
   "type": "boolean",
   "default": false
  },
  "extension": {
   "description": "List of extensions that nyc should attempt to handle in addition to .js",
   "type": "array",
   "items": {
    "type": "string"
   },
   "default": [
    ".js",
    ".cjs",
    ".mjs",
    ".ts",
    ".tsx",
    ".jsx"
   ]
  },
  "include": {
   "description": "List of files to include for coverage.",
   "type": "array",
   "items": {
    "type": "string"
   },
   "default": [
    "**"
   ]
  },
  "exclude": {
   "description": "List of files to exclude for coverage.",
   "type": "array",
   "items": {
    "type": "string"
   },
   "default": [
    "coverage/**"
   ]
  },
  "reporter": {
   "description": "The names of custom reporter to show coverage results.",
   "type": "array",
   "items": {
    "type": "string"
   },
   "default": [
    "text"
   ]
  },
  "report-dir": {
   "description": "Where to put the coverage report files.",
   "type": "string",
   "default": "./coverage"
  },
  "skip-full": {
   "description": "Don't show files with 100% statement, branch, and function coverage",
   "type": "boolean",
   "default": false
  },
  "temp-dir": {
   "description": "Directory to output raw coverage information to.",
   "type": "string",
   "default": "./.nyc_output"
  }
 },
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// vsconfig.json (835 bytes)
object VsconfigSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/vsconfig.json",
 "properties": {
  "version": {
   "description": "The version of the component configuration file format.",
   "type": "string",
   "pattern": "^(\\\\d+\\\\.)?(\\\\d+\\\\.)?(\\\\d+\\\\.)?(\\\\d+)$$"
  },
  "components": {
   "type": "array",
   "description": "An array of Visual Studio component names.",
   "items": {
    "type": "string",
    "minLength": 1
   }
  },
  "extensions": {
   "type": "array",
   "description": "An array of Visual Studio extensions. These can be URLs to marketplace extensions or paths to private VSIX files.",
   "items": {
    "type": "string",
    "minLength": 1
   }
  }
 },
 "required": [
  "components"
 ],
 "title": "JSON schema for Visual Studio component configuration files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// global.json (3605 bytes)
object GlobalJsonSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/global.json",
 "additionalProperties": true,
 "properties": {
  "sdk": {
   "type": "object",
   "description": "Specifies information about the .NET SDK to select.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#sdk",
   "properties": {
    "version": {
     "type": "string",
     "pattern": "^(?<major>0|[1-9]\\\\d*)\\\\.(?<minor>0|[1-9]\\\\d*)\\\\.(?<patch>0|[1-9]\\\\d*)(?:-(?<prerelease>(?:0|[1-9]\\\\d*|\\\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\\\.(?:0|[1-9]\\\\d*|\\\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\\\+(?<buildmetadata>[0-9a-zA-Z-]+(?:\\\\.[0-9a-zA-Z-]+)*))?$$",
     "description": "The version of the .NET SDK to use. A full version number is required; wildcards and version ranges aren't supported.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#version"
    },
    "allowPrerelease": {
     "type": "boolean",
     "description": "Whether the SDK resolver should consider prerelease versions when selecting the SDK version to use.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#allowprerelease"
    },
    "rollForward": {
     "type": "string",
     "enum": [
      "patch",
      "feature",
      "minor",
      "major",
      "latestPatch",
      "latestFeature",
      "latestMinor",
      "latestMajor",
      "disable"
     ],
     "description": "The roll-forward policy to use when selecting an SDK version. A version must also be specified unless this is set to 'latestMajor'. When omitted, the effective policy is 'patch' if a version is specified and 'latestMajor' otherwise.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#rollforward"
    },
    "paths": {
     "type": "array",
     "description": "The locations to consider when searching for a compatible .NET SDK. Paths can be absolute, relative to global.json, or the special value '$$host$$'. Available since .NET 10 SDK.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#paths",
     "items": {
      "type": "string"
     }
    },
    "errorMessage": {
     "type": "string",
     "description": "A custom error message to display when the SDK resolver can't find a compatible .NET SDK. Available since .NET 10 SDK.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#errormessage"
    }
   },
   "dependencies": {
    "rollForward": {
     "anyOf": [
      {
       "properties": {
        "version": {}
       },
       "required": [
        "version"
       ]
      },
      {
       "properties": {
        "rollForward": {
         "enum": [
          "latestMajor"
         ]
        }
       }
      }
     ]
    }
   }
  },
  "msbuild-sdks": {
   "type": "object",
   "description": "Controls project SDK versions in one place rather than in each individual project. Each property name is a project SDK name and its value is the version to use.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#msbuild-sdks",
   "additionalProperties": {
    "type": "string"
   }
  },
  "test": {
   "type": "object",
   "description": "Specifies information about tests.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#test",
   "properties": {
    "runner": {
     "type": "string",
     "enum": [
      "Microsoft.Testing.Platform",
      "VSTest"
     ],
     "default": "VSTest",
     "description": "The test runner that the 'dotnet test' command uses to discover and run tests. Available since .NET 10 SDK.\\nhttps://learn.microsoft.com/dotnet/core/tools/global-json#runner"
    }
   }
  }
 },
 "title": "JSON schema for the .NET global configuration file",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// nodemon.json (4988 bytes)
object NodemonSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/nodemon.json",
 "definitions": {
  "pathPattern": {
   "anyOf": [
    {
     "type": "string"
    },
    {
     "type": "object",
     "properties": {
      "re": {
       "type": "string"
      }
     },
     "additionalProperties": false,
     "required": [
      "re"
     ],
     "description": "Regular expression"
    }
   ]
  },
  "terminationSignals": {
   "anyOf": [
    {
     "const": "SIGTERM"
    },
    {
     "const": "SIGINT"
    },
    {
     "const": "SIGQUIT"
    },
    {
     "const": "SIGKILL"
    },
    {
     "const": "SIGHUP"
    }
   ]
  },
  "variables": {
   "anyOf": [
    {
     "const": "{{pwd}}",
     "description": "The current directory"
    },
    {
     "const": "{{filename}}",
     "description": "The filename you pass to nodemon"
    }
   ]
  }
 },
 "dependencies": {
  "pollingInterval": {
   "properties": {
    "legacyWatch": {}
   },
   "required": [
    "legacyWatch"
   ]
  },
  "nodeArgs": {
   "properties": {
    "exec": {
     "type": "string"
    }
   },
   "required": [
    "exec"
   ]
  }
 },
 "properties": {
  "colours": {
   "default": true,
   "description": "set to false to disable color output",
   "type": "boolean"
  },
  "cwd": {
   "description": "change into <dir> before running the script",
   "type": "string"
  },
  "delay": {
   "default": 0,
   "description": "debounce restart for a number of milliseconds",
   "type": "number"
  },
  "dump": {
   "default": false,
   "description": "print full debug configuration",
   "type": "boolean"
  },
  "exec": {
   "description": "execute script with \\"app\\", ie. -x \\"python -v\\".  May use variables.",
   "examples": [
    "{{pwd}}/index.js --some-arg",
    "{{filename}}"
   ],
   "oneOf": [
    {
     "type": "string"
    },
    {
     "type": "array",
     "items": {
      "type": "string"
     }
    }
   ]
  },
  "execMap": {
   "description": "The global config file is useful for setting up default executables",
   "type": "object"
  },
  "exitcrash": {
   "description": "Exit nodemon after crash",
   "type": "boolean"
  },
  "ext": {
   "default": "*",
   "description": "extensions to look for, ie. \\"js,jade,hbs\\"",
   "type": "string"
  },
  "ignore": {
   "description": "Ignore directory or file.  One entry per ignored value.  Wildcards are allowed.",
   "oneOf": [
    {
     "$$ref": "#/definitions/pathPattern"
    },
    {
     "type": "array",
     "items": {
      "$$ref": "#/definitions/pathPattern",
      "description": "Path or pattern of file or directory to ignore.  Can also use regular expressions wrapped in an object with a single property named \\"re\\".",
      "examples": [
       ".gitignore",
       ".vscode",
       "__tests__/*",
       "__*__/*.js",
       "*.test.js"
      ]
     }
    }
   ]
  },
  "ignoreRoot": {
   "description": "root paths to ignore",
   "items": {
    "type": "string"
   },
   "type": "array"
  },
  "legacyWatch": {
   "default": false,
   "description": "use polling to watch for changes (typically needed when watching over a network/Docker)",
   "type": "boolean"
  },
  "noUpdateNotifier": {
   "default": false,
   "description": "opt-out of update version check",
   "type": "boolean"
  },
  "nodeArgs": {
   "description": "arguments to pass to node if exec is \\"node\\"",
   "type": "array"
  },
  "pollingInterval": {
   "default": 100,
   "description": "combined with legacyWatch, milliseconds to poll for (default 100)",
   "type": "number"
  },
  "quiet": {
   "default": false,
   "description": "minimise nodemon messages to start/stop only",
   "type": "boolean"
  },
  "runOnChangeOnly": {
   "default": false,
   "description": "execute script on change only, not startup",
   "type": "boolean"
  },
  "signal": {
   "$$ref": "#/definitions/terminationSignals",
   "description": "use specified kill signal instead of default (ex. SIGTERM)",
   "type": "string"
  },
  "spawn": {
   "default": false,
   "description": "force nodemon to use spawn (over fork) [node only]",
   "type": "boolean"
  },
  "stdin": {
   "default": true,
   "description": "set to false to have nodemon pass stdin directly to child process",
   "type": "boolean"
  },
  "verbose": {
   "default": false,
   "description": "show detail on what is causing restarts",
   "type": "boolean"
  },
  "watch": {
   "description": "Watch directory or file.  One entry per watched value.  Wildcards are allowed.",
   "oneOf": [
    {
     "$$ref": "#/definitions/pathPattern"
    },
    {
     "type": "array",
     "items": {
      "$$ref": "#/definitions/pathPattern",
      "description": "Path or pattern of file or directory to watch.  Can also use regular expressions wrapped in an object with a single property named \\"re\\".",
      "examples": [
       "src/index.js",
       "src",
       "src/*.js",
       "*.js"
      ]
     }
    }
   ]
  }
 },
 "title": "JSON Schema for Nodemon Config",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// bowerrc.json (4254 bytes)
object BowerrcSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/bowerrc.json",
 "additionalProperties": true,
 "properties": {
  "analytics": {
   "type": "boolean",
   "default": true
  },
  "cwd": {
   "type": "string",
   "description": "The directory from which bower should run. All relative paths will be calculated according to this setting."
  },
  "directory": {
   "type": "string",
   "description": "The directory from which bower should run. All relative paths will be calculated according to this setting.",
   "default": "bower_components"
  },
  "json": {
   "type": "string",
   "description": "A file path to the Bower configuration file",
   "default": "bower.json"
  },
  "registry": {
   "anyOf": [
    {
     "type": "string",
     "format": "uri"
    },
    {
     "type": "object",
     "properties": {
      "search": {
       "anyOf": [
        {
         "type": "string",
         "format": "uri"
        },
        {
         "type": "array",
         "items": {
          "type": "string",
          "format": "uri"
         }
        }
       ],
       "description": "An array of URLs pointing to read-only Bower registries. A string means only one. When looking into the registry for an endpoint, Bower will query these registries by the specified order."
      },
      "register": {
       "type": "string",
       "description": "The URL to use when registering packages.",
       "format": "uri"
      },
      "publish": {
       "type": "string",
       "description": "The URL to use when publishing packages.",
       "format": "uri"
      }
     }
    }
   ],
   "description": "The registry config"
  },
  "proxy": {
   "type": "string",
   "description": "The proxy to use for http requests.",
   "format": "uri"
  },
  "https-proxy": {
   "type": "string",
   "description": "The proxy to use for https requests.",
   "format": "uri"
  },
  "user-agent": {
   "type": "string",
   "description": "Sets the User-Agent for each request made."
  },
  "timeout": {
   "type": "number",
   "description": "The timeout to be used when making requests in milliseconds.",
   "default": 60000
  },
  "strict-ssl": {
   "type": "boolean",
   "description": "Whether or not to do SSL key validation when making requests via https."
  },
  "ca": {
   "anyOf": [
    {
     "type": "object"
    },
    {
     "type": "string"
    }
   ],
   "description": "The CA certificates to be used, defaults to null. This is similar to the registry key, specifying each CA to use for each registry endpoint."
  },
  "color": {
   "type": "boolean",
   "description": "Enable or disable use of colors in the CLI output.",
   "default": true
  },
  "storage": {
   "type": "object",
   "description": "Where to store persistent data, such as cache, needed by bower.",
   "properties": {
    "packages": {
     "type": "string"
    },
    "registry": {
     "type": "string"
    },
    "links": {
     "type": "string"
    }
   }
  },
  "tmp": {
   "type": "string",
   "description": "Where to store temporary files and folders"
  },
  "interactive": {
   "type": "boolean",
   "description": "Makes bower interactive, prompting whenever necessary"
  },
  "resolvers": {
   "type": "array",
   "description": "Identifies pluggable resolvers to be used for locating and fetching packages",
   "items": {
    "type": "string"
   }
  },
  "shallowCloneHosts": {
   "type": "array",
   "description": "Whitelists hosts which are known to support shallow cloning",
   "items": {
    "type": "string"
   }
  },
  "scripts": {
   "description": "Contains custom hooks used to trigger other automated tools",
   "type": "object",
   "properties": {
    "preinstall": {
     "type": "string",
     "description": "A script to run before install"
    },
    "postinstall": {
     "type": "string",
     "description": "A script to run after install"
    },
    "preuninstall": {
     "type": "string",
     "description": "A script to run before uninstall"
    }
   }
  },
  "ignoredDependencies": {
   "type": "array",
   "description": "Bower will ignore these dependencies when resolving packages",
   "items": {
    "type": "string"
   }
  }
 },
 "title": "JSON schema for .bowerrc files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// lintstagedrc.schema.json (2998 bytes)
object LintStagedSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/lintstagedrc.schema.json",
 "anyOf": [
  {
   "$$ref": "#/definitions/advancedConfig"
  },
  {
   "$$ref": "#/definitions/basicConfig"
  }
 ],
 "definitions": {
  "$$schemaProperty": {
   "type": "string"
  },
  "linter": {
   "type": [
    "string",
    "array"
   ]
  },
  "lintersMap": {
   "description": "keys (String) are glob patterns, values (Array<String> | String) are commands to execute.",
   "type": "object",
   "additionalProperties": {
    "$$ref": "#/definitions/linter"
   }
  },
  "globOptions": {
   "description": "micromatch options to customize how glob patterns match files.",
   "type": "object",
   "properties": {
    "matchBase": {
     "type": "boolean",
     "default": true
    },
    "dot": {
     "type": "boolean",
     "default": true
    }
   },
   "additionalProperties": false
  },
  "advancedConfig": {
   "properties": {
    "$$schema": {
     "$$ref": "#/definitions/$$schemaProperty"
    },
    "concurrent": {
     "description": "Controls if linters are run simultaneously for each glob pattern.",
     "type": "boolean",
     "default": true
    },
    "chunkSize": {
     "description": "Max allowed chunk size based on number of files for glob pattern. This option is only applicable on Windows based systems to avoid command length limitations",
     "type": "number",
     "minimum": 1
    },
    "globOptions": {
     "$$ref": "#/definitions/globOptions",
     "description": "micromatch options to customize how glob patterns match files."
    },
    "linters": {
     "$$ref": "#/definitions/lintersMap",
     "description": "keys (String) are glob patterns, values (Array<String> | String) are commands to execute."
    },
    "ignore": {
     "description": "array of glob patterns to entirely ignore from any task.",
     "type": "array",
     "items": {
      "type": "string"
     },
     "default": "['**/docs/**/*.js']"
    },
    "subTaskConcurrency": {
     "description": "Controls concurrency for processing chunks generated for each linter. This option is only applicable on Windows. Execution is not concurrent by default.",
     "type": "integer",
     "minimum": 1,
     "default": 1
    },
    "renderer": {
     "enum": [
      "update",
      "verbose"
     ],
     "default": "update"
    },
    "relative": {
     "description": "If true it will give the relative path from your package.json directory to your linter arguments.",
     "type": "boolean",
     "default": false
    }
   },
   "additionalProperties": false
  },
  "basicConfig": {
   "properties": {
    "$$schema": {
     "$$ref": "#/definitions/$$schemaProperty"
    }
   },
   "propertyNames": {
    "not": {
     "enum": [
      "concurrent",
      "chunkSize",
      "globOptions",
      "linters",
      "ignore",
      "subTaskConcurrency",
      "renderer",
      "relative"
     ]
    }
   },
   "additionalProperties": {
    "$$ref": "#/definitions/linter"
   }
  }
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// commitlintrc.json (2257 bytes)
object CommitlintSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/commitlintrc.json",
 "definitions": {
  "rule": {
   "oneOf": [
    {
     "description": "A rule",
     "type": "array",
     "items": [
      {
       "description": "Level: 0 disables the rule. For 1 it will be considered a warning, for 2 an error",
       "type": "number",
       "enum": [
        0,
        1,
        2
       ]
      },
      {
       "description": "Applicable: always|never: never inverts the rule",
       "type": "string",
       "enum": [
        "always",
        "never"
       ]
      },
      {
       "description": "Value: the value for this rule"
      }
     ],
     "minItems": 1,
     "maxItems": 3,
     "additionalItems": false
    }
   ]
  }
 },
 "properties": {
  "extends": {
   "description": "Resolvable ids to commitlint configurations to extend",
   "oneOf": [
    {
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    {
     "type": "string"
    }
   ]
  },
  "parserPreset": {
   "description": "Resolvable id to conventional-changelog parser preset to import and use",
   "oneOf": [
    {
     "type": "string"
    },
    {
     "type": "object",
     "properties": {
      "name": {
       "type": "string"
      },
      "path": {
       "type": "string"
      },
      "parserOpts": {}
     },
     "additionalProperties": false
    }
   ]
  },
  "helpUrl": {
   "description": "Custom URL to show upon failure",
   "type": "string"
  },
  "formatter": {
   "description": "Resolvable id to package, from node_modules, which formats the output",
   "type": "string"
  },
  "rules": {
   "description": "Rules to check against",
   "type": "object",
   "propertyNames": {
    "type": "string"
   },
   "additionalProperties": {
    "$$ref": "#/definitions/rule"
   }
  },
  "plugins": {
   "description": "Resolvable ids of commitlint plugins from node_modules",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "ignores": {
   "description": "Additional commits to ignore, defined by ignore matchers",
   "type": "array",
   "items": {}
  },
  "defaultIgnores": {
   "description": "Whether commitlint uses the default ignore rules",
   "type": "boolean"
  }
 }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// schema.json (12633 bytes)
object ResumeSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "http://example.com/example.json",
 "additionalProperties": true,
 "definitions": {
  "iso8601": {
   "type": "string",
   "description": "Similar to the standard date type, but each section after the year is optional. e.g. 2014-06-29 or 2023-04",
   "pattern": "^([1-2][0-9]{3}-[0-1][0-9]-[0-3][0-9]|[1-2][0-9]{3}-[0-1][0-9]|[1-2][0-9]{3})$$"
  }
 },
 "properties": {
  "$$schema": {
   "type": "string",
   "description": "link to the version of the schema that can validate the resume",
   "format": "uri"
  },
  "basics": {
   "type": "object",
   "additionalProperties": true,
   "properties": {
    "name": {
     "type": "string"
    },
    "label": {
     "type": "string",
     "description": "e.g. Web Developer"
    },
    "image": {
     "type": "string",
     "description": "URL (as per RFC 3986) to a image in JPEG or PNG format"
    },
    "email": {
     "type": "string",
     "description": "e.g. thomas@gmail.com",
     "format": "email"
    },
    "phone": {
     "type": "string",
     "description": "Phone numbers are stored as strings so use any format you like, e.g. 712-117-2923"
    },
    "url": {
     "type": "string",
     "description": "URL (as per RFC 3986) to your website, e.g. personal homepage",
     "format": "uri"
    },
    "summary": {
     "type": "string",
     "description": "Write a short 2-3 sentence biography about yourself"
    },
    "location": {
     "type": "object",
     "additionalProperties": true,
     "properties": {
      "address": {
       "type": "string",
       "description": "To add multiple address lines, use \\n. For example, 1234 Glücklichkeit Straße\\nHinterhaus 5. Etage li."
      },
      "postalCode": {
       "type": "string"
      },
      "city": {
       "type": "string"
      },
      "countryCode": {
       "type": "string",
       "description": "code as per ISO-3166-1 ALPHA-2, e.g. US, AU, IN"
      },
      "region": {
       "type": "string",
       "description": "The general region where you live. Can be a US state, or a province, for instance."
      }
     }
    },
    "profiles": {
     "type": "array",
     "description": "Specify any number of social networks that you participate in",
     "additionalItems": false,
     "items": {
      "type": "object",
      "additionalProperties": true,
      "properties": {
       "network": {
        "type": "string",
        "description": "e.g. Facebook or Twitter"
       },
       "username": {
        "type": "string",
        "description": "e.g. neutralthoughts"
       },
       "url": {
        "type": "string",
        "description": "e.g. http://twitter.example.com/neutralthoughts",
        "format": "uri"
       }
      }
     }
    }
   }
  },
  "work": {
   "type": "array",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "name": {
      "type": "string",
      "description": "e.g. Facebook"
     },
     "location": {
      "type": "string",
      "description": "e.g. Menlo Park, CA"
     },
     "description": {
      "type": "string",
      "description": "e.g. Social Media Company"
     },
     "position": {
      "type": "string",
      "description": "e.g. Software Engineer"
     },
     "url": {
      "type": "string",
      "description": "e.g. http://facebook.example.com",
      "format": "uri"
     },
     "startDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "endDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "summary": {
      "type": "string",
      "description": "Give an overview of your responsibilities at the company"
     },
     "highlights": {
      "type": "array",
      "description": "Specify multiple accomplishments",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. Increased profits by 20% from 2011-2012 through viral advertising"
      }
     }
    }
   }
  },
  "volunteer": {
   "type": "array",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "organization": {
      "type": "string",
      "description": "e.g. Facebook"
     },
     "position": {
      "type": "string",
      "description": "e.g. Software Engineer"
     },
     "url": {
      "type": "string",
      "description": "e.g. http://facebook.example.com",
      "format": "uri"
     },
     "startDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "endDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "summary": {
      "type": "string",
      "description": "Give an overview of your responsibilities at the company"
     },
     "highlights": {
      "type": "array",
      "description": "Specify accomplishments and achievements",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. Increased profits by 20% from 2011-2012 through viral advertising"
      }
     }
    }
   }
  },
  "education": {
   "type": "array",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "institution": {
      "type": "string",
      "description": "e.g. Massachusetts Institute of Technology"
     },
     "url": {
      "type": "string",
      "description": "e.g. http://facebook.example.com",
      "format": "uri"
     },
     "area": {
      "type": "string",
      "description": "e.g. Arts"
     },
     "studyType": {
      "type": "string",
      "description": "e.g. Bachelor"
     },
     "startDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "endDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "score": {
      "type": "string",
      "description": "grade point average, e.g. 3.67/4.0"
     },
     "courses": {
      "type": "array",
      "description": "List notable courses/subjects",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. H1302 - Introduction to American history"
      }
     }
    }
   }
  },
  "awards": {
   "type": "array",
   "description": "Specify any awards you have received throughout your professional career",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "title": {
      "type": "string",
      "description": "e.g. One of the 100 greatest minds of the century"
     },
     "date": {
      "$$ref": "#/definitions/iso8601"
     },
     "awarder": {
      "type": "string",
      "description": "e.g. Time Magazine"
     },
     "summary": {
      "type": "string",
      "description": "e.g. Received for my work with Quantum Physics"
     }
    }
   }
  },
  "certificates": {
   "type": "array",
   "description": "Specify any certificates you have received throughout your professional career",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "name": {
      "type": "string",
      "description": "e.g. Certified Kubernetes Administrator"
     },
     "date": {
      "$$ref": "#/definitions/iso8601"
     },
     "url": {
      "type": "string",
      "description": "e.g. http://example.com",
      "format": "uri"
     },
     "issuer": {
      "type": "string",
      "description": "e.g. CNCF"
     }
    }
   }
  },
  "publications": {
   "type": "array",
   "description": "Specify your publications through your career",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "name": {
      "type": "string",
      "description": "e.g. The World Wide Web"
     },
     "publisher": {
      "type": "string",
      "description": "e.g. IEEE, Computer Magazine"
     },
     "releaseDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "url": {
      "type": "string",
      "description": "e.g. http://www.computer.org.example.com/csdl/mags/co/1996/10/rx069-abs.html",
      "format": "uri"
     },
     "summary": {
      "type": "string",
      "description": "Short summary of publication. e.g. Discussion of the World Wide Web, HTTP, HTML."
     }
    }
   }
  },
  "skills": {
   "type": "array",
   "description": "List out your professional skill-set",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "name": {
      "type": "string",
      "description": "e.g. Web Development"
     },
     "level": {
      "type": "string",
      "description": "e.g. Master"
     },
     "keywords": {
      "type": "array",
      "description": "List some keywords pertaining to this skill",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. HTML"
      }
     }
    }
   }
  },
  "languages": {
   "type": "array",
   "description": "List any other languages you speak",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "language": {
      "type": "string",
      "description": "e.g. English, Spanish"
     },
     "fluency": {
      "type": "string",
      "description": "e.g. Fluent, Beginner"
     }
    }
   }
  },
  "interests": {
   "type": "array",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "name": {
      "type": "string",
      "description": "e.g. Philosophy"
     },
     "keywords": {
      "type": "array",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. Friedrich Nietzsche"
      }
     }
    }
   }
  },
  "references": {
   "type": "array",
   "description": "List references you have received",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "name": {
      "type": "string",
      "description": "e.g. Timothy Cook"
     },
     "reference": {
      "type": "string",
      "description": "e.g. Joe blogs was a great employee, who turned up to work at least once a week. He exceeded my expectations when it came to doing nothing."
     }
    }
   }
  },
  "projects": {
   "type": "array",
   "description": "Specify career projects",
   "additionalItems": false,
   "items": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "name": {
      "type": "string",
      "description": "e.g. The World Wide Web"
     },
     "description": {
      "type": "string",
      "description": "Short summary of project. e.g. Collated works of 2017."
     },
     "highlights": {
      "type": "array",
      "description": "Specify multiple features",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. Directs you close but not quite there"
      }
     },
     "keywords": {
      "type": "array",
      "description": "Specify special elements involved",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. AngularJS"
      }
     },
     "startDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "endDate": {
      "$$ref": "#/definitions/iso8601"
     },
     "url": {
      "type": "string",
      "format": "uri",
      "description": "e.g. http://www.computer.org/csdl/mags/co/1996/10/rx069-abs.html"
     },
     "roles": {
      "type": "array",
      "description": "Specify your role on this project or in company",
      "additionalItems": false,
      "items": {
       "type": "string",
       "description": "e.g. Team Lead, Speaker, Writer"
      }
     },
     "entity": {
      "type": "string",
      "description": "Specify the relevant company/entity affiliations e.g. 'greenpeace', 'corporationXYZ'"
     },
     "type": {
      "type": "string",
      "description": " e.g. 'volunteering', 'presentation', 'talk', 'application', 'conference'"
     }
    }
   }
  },
  "meta": {
   "type": "object",
   "description": "The schema version and any other tooling configuration lives here",
   "additionalProperties": true,
   "properties": {
    "canonical": {
     "type": "string",
     "description": "URL (as per RFC 3986) to latest version of this document",
     "format": "uri"
    },
    "version": {
     "type": "string",
     "description": "A version field which follows semver - e.g. v1.0.0"
    },
    "lastModified": {
     "type": "string",
     "description": "Using ISO 8601 with YYYY-MM-DDThh:mm:ss"
    }
   }
  }
 },
 "title": "Resume Schema",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// launchsettings.json (6714 bytes)
object LaunchSettingsSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/launchsettings.json",
 "allowTrailingCommas": true,
 "definitions": {
  "profile": {
   "$$ref": "#/definitions/profileContent",
   "type": "object",
   "required": [
    "commandName"
   ]
  },
  "iisSetting": {
   "$$ref": "#/definitions/iisSettingContent",
   "type": "object"
  },
  "iisSettingContent": {
   "type": "object",
   "properties": {
    "windowsAuthentication": {
     "type": "boolean",
     "description": "Set to true to enable windows authentication for your site in IIS and IIS Express.",
     "default": false
    },
    "anonymousAuthentication": {
     "type": "boolean",
     "description": "Set to true to enable anonymous authentication for your site in IIS and IIS Express.",
     "default": true
    },
    "iisExpress": {
     "$$ref": "#/definitions/iisBindingContent",
     "type": "object",
     "description": "Site settings to use with IISExpress profiles."
    },
    "iis": {
     "$$ref": "#/definitions/iisBindingContent",
     "type": "object",
     "description": "Site settings to use with IIS profiles."
    }
   }
  },
  "iisBindingContent": {
   "type": "object",
   "properties": {
    "applicationUrl": {
     "type": "string",
     "format": "uri",
     "description": "The URL of the web site.",
     "default": ""
    },
    "sslPort": {
     "type": "integer",
     "maximum": 65535,
     "minimum": 0,
     "description": "The SSL port to use for the web site.",
     "default": 0
    }
   }
  },
  "profileContent": {
   "type": "object",
   "properties": {
    "commandName": {
     "type": "string",
     "description": "Identifies the debug target to run.",
     "enum": [
      "Executable",
      "Project",
      "IIS",
      "IISExpress",
      "DebugRoslynComponent",
      "Docker",
      "DockerCompose",
      "MsixPackage",
      "SdkContainer",
      "WSL",
      "WSL2"
     ],
     "default": "",
     "minLength": 1
    },
    "commandLineArgs": {
     "type": "string",
     "description": "The arguments to pass to the target being run.",
     "default": ""
    },
    "executablePath": {
     "type": "string",
     "description": "An absolute or relative path to the executable.",
     "default": ""
    },
    "workingDirectory": {
     "type": "string",
     "description": "Sets the working directory of the command."
    },
    "launchBrowser": {
     "type": "boolean",
     "description": "Set to true if the browser should be launched.",
     "default": false
    },
    "launchUrl": {
     "type": "string",
     "description": "The relative URL to launch in the browser."
    },
    "environmentVariables": {
     "type": "object",
     "description": "Set the environment variables as key/value pairs.",
     "additionalProperties": {
      "type": "string"
     }
    },
    "applicationUrl": {
     "type": "string",
     "description": "A semi-colon delimited list of URL(s) to configure for the web server."
    },
    "nativeDebugging": {
     "type": "boolean",
     "description": "Set to true to enable native code debugging.",
     "default": false
    },
    "externalUrlConfiguration": {
     "type": "boolean",
     "description": "Set to true to disable configuration of the site when running the Asp.Net Core Project profile.",
     "default": false
    },
    "use64Bit": {
     "type": "boolean",
     "description": "Set to true to run the 64 bit version of IIS Express, false to run the x86 version.",
     "default": true
    },
    "ancmHostingModel": {
     "enum": [
      "InProcess",
      "OutOfProcess"
     ],
     "description": "Specifies the hosting model to use when running ASP.NET core projects in IIS and IIS Express.",
     "default": false
    },
    "sqlDebugging": {
     "type": "boolean",
     "description": "Set to true to enable debugging of SQL scripts and stored procedures.",
     "default": false
    },
    "jsWebView2Debugging": {
     "type": "boolean",
     "description": "Set to true to enable the JavaScript debugger for Microsoft Edge (Chromium) based WebView2.",
     "default": false
    },
    "leaveRunningOnClose": {
     "type": "boolean",
     "description": "Set to true to leave the IIS application pool running when the project is closed.",
     "default": false
    },
    "remoteDebugEnabled": {
     "type": "boolean",
     "description": "Set to true to have the debugger attach to a process on a remote computer.",
     "default": false
    },
    "remoteDebugMachine": {
     "type": "string",
     "description": "The name and port number of the remote machine in name:port format."
    },
    "authenticationMode": {
     "enum": [
      "None",
      "Windows"
     ],
     "description": "The authentication scheme to use when connecting to the remote computer.",
     "default": "None"
    },
    "hotReloadEnabled": {
     "type": "boolean",
     "description": "Set to true to enable applying code changes to the running application.",
     "default": true
    },
    "publishAllPorts": {
     "type": "boolean",
     "description": "Publish all exposed ports to random ports in Docker (-P).",
     "default": true
    },
    "useSSL": {
     "type": "boolean",
     "description": "Set to true to bind the SSL port.",
     "default": true
    },
    "sslPort": {
     "type": "integer",
     "maximum": 65535,
     "minimum": 0,
     "description": "The SSL port to use for the web site.",
     "default": 0
    },
    "httpPort": {
     "type": "integer",
     "maximum": 65535,
     "minimum": 0,
     "description": "The HTTP port to use for the web site.",
     "default": 0
    },
    "dotnetRunMessages": {
     "type": "boolean",
     "description": "Set to true to display a message when the project is building.",
     "default": true
    },
    "inspectUri": {
     "type": "string",
     "description": "The url to enable debugging on a Blazor WebAssembly application.",
     "default": "{wsProtocol}://{url.hostname}:{url.port}/_framework/debug/ws-proxy?browser={browserInspectUri}"
    },
    "targetProject": {
     "type": "string",
     "description": "A relative or absolute path to the .NET project file on which Roslyn component should be executed. Relative to the current project's folder.",
     "default": ""
    }
   }
  }
 },
 "properties": {
  "profiles": {
   "type": "object",
   "description": "A list of debug profiles",
   "additionalProperties": {
    "$$ref": "#/definitions/profile"
   }
  },
  "iisSettings": {
   "$$ref": "#/definitions/iisSettingContent",
   "type": "object",
   "description": "IIS and IIS Express settings"
  }
 },
 "title": "JSON schema for the Visual Studio LaunchSettings.json file.",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// huskyrc.json (45427 bytes)
object HuskySchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/huskyrc.json",
 "additionalProperties": false,
 "definitions": {
  "hook": {
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  }
 },
 "description": "Husky can prevent bad `git commit`, `git push` and more 🐶 woof!",
 "properties": {
  "$$schema": {
   "type": "string"
  },
  "skipCI": {
   "title": "Skipping Git hooks installation.",
   "type": "boolean",
   "default": false
  },
  "hooks": {
   "title": "Git hooks.",
   "type": "object",
   "properties": {
    "applypatch-msg": {
     "$$comment": "https://git-scm.com/docs/githooks#_applypatch_msg",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-am. It takes a single parameter, the name of the file that holds the proposed commit log message. Exiting with a non-zero status causes git am to abort before applying the patch.\\n\\nThe hook is allowed to edit the message file in place, and can be used to normalize the message into some project standard format. It can also be used to refuse the commit after inspecting the message file.\\n\\nThe default applypatch-msg hook, when enabled, runs the commit-msg hook, if the latter is enabled.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-am\\">git-am</a>. It takes a single parameter, the name of the file that holds the proposed commit log message. Exiting with a non-zero status causes <code>git am</code> to abort before applying the patch.</p>\\n<p>The hook is allowed to edit the message file in place, and can be used to normalize the message into some project standard format. It can also be used to refuse the commit after inspecting the message file.</p>\\n<p>The default <em>applypatch-msg</em> hook, when enabled, runs the <em>commit-msg</em> hook, if the latter is enabled.</p>",
     "markdownDescription": "This hook is invoked by [git-am](https://git-scm.com/docs/git-am). It takes a single parameter, the name of the file that holds the proposed commit log message. Exiting with a non-zero status causes `git am` to abort before applying the patch.\\n\\nThe hook is allowed to edit the message file in place, and can be used to normalize the message into some project standard format. It can also be used to refuse the commit after inspecting the message file.\\n\\nThe default **applypatch-msg** hook, when enabled, runs the **commit-msg** hook, if the latter is enabled."
    },
    "pre-applypatch": {
     "$$comment": "https://git-scm.com/docs/githooks#_pre_applypatch",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-am. It takes no parameter, and is invoked after the patch is applied, but before a commit is made.\\n\\nIf it exits with non-zero status, then the working tree will not be committed after applying the patch.\\n\\nIt can be used to inspect the current working tree and refuse to make a commit if it does not pass certain test.\\n\\nThe default pre-applypatch hook, when enabled, runs the pre-commit hook, if the latter is enabled.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-am\\">git-am</a>. It takes no parameter, and is invoked after the patch is applied, but before a commit is made.</p>\\n<p>If it exits with non-zero status, then the working tree will not be committed after applying the patch.</p>\\n<p>It can be used to inspect the current working tree and refuse to make a commit if it does not pass certain test.</p>\\n<p>The default <em>pre-applypatch</em> hook, when enabled, runs the <em>pre-commit</em> hook, if the latter is enabled.</p>",
     "markdownDescription": "This hook is invoked by [git-am](https://git-scm.com/docs/git-am). It takes no parameter, and is invoked after the patch is applied, but before a commit is made.\\n\\nIf it exits with non-zero status, then the working tree will not be committed after applying the patch.\\n\\nIt can be used to inspect the current working tree and refuse to make a commit if it does not pass certain test.\\n\\nThe default **pre-applypatch** hook, when enabled, runs the **pre-commit** hook, if the latter is enabled."
    },
    "post-applypatch": {
     "$$comment": "https://git-scm.com/docs/githooks#_post_applypatch",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-am. It takes no parameter, and is invoked after the patch is applied and a commit is made.\\n\\nThis hook is meant primarily for notification, and cannot affect the outcome of git am.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-am\\">git-am</a>. It takes no parameter, and is invoked after the patch is applied and a commit is made.</p>\\n<p>This hook is meant primarily for notification, and cannot affect the outcome of <code>git am</code>.</p>",
     "markdownDescription": "This hook is invoked by [git-am](https://git-scm.com/docs/git-am). It takes no parameter, and is invoked after the patch is applied and a commit is made.\\n\\nThis hook is meant primarily for notification, and cannot affect the outcome of `git am`."
    },
    "pre-commit": {
     "$$comment": "https://git-scm.com/docs/githooks#_pre_commit",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-commit, and can be bypassed with the --no-verify option. It takes no parameters, and is invoked before obtaining the proposed commit log message and making a commit. Exiting with a non-zero status from this script causes the git commit command to abort before creating a commit.\\n\\nThe default pre-commit hook, when enabled, catches introduction of lines with trailing whitespaces and aborts the commit when such a line is found.\\n\\nAll the git commit hooks are invoked with the environment variable GIT_EDITOR=: if the command will not bring up an editor to modify the commit message.\\n\\nThe default pre-commit hook, when enabled—​and with the hooks.allownonascii config option unset or set to false—​prevents the use of non-ASCII filenames.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-commit\\">git-commit</a>, and can be bypassed with the <code>--no-verify</code> option. It takes no parameters, and is invoked before obtaining the proposed commit log message and making a commit. Exiting with a non-zero status from this script causes the <code>git commit</code> command to abort before creating a commit.</p>\\n<p>The default <em>pre-commit</em> hook, when enabled, catches introduction of lines with trailing whitespaces and aborts the commit when such a line is found.</p>\\n<p>All the <code>git commit</code> hooks are invoked with the environment variable <code>GIT_EDITOR=:</code> if the command will not bring up an editor to modify the commit message.</p>\\n<p>The default <em>pre-commit</em> hook, when enabled—​and with the <code>hooks.allownonascii</code> config option unset or set to false—​prevents the use of non-ASCII filenames.</p>",
     "markdownDescription": "This hook is invoked by [git-commit](https://git-scm.com/docs/git-commit), and can be bypassed with the `--no-verify` option. It takes no parameters, and is invoked before obtaining the proposed commit log message and making a commit. Exiting with a non-zero status from this script causes the `git commit` command to abort before creating a commit.\\n\\nThe default **pre-commit** hook, when enabled, catches introduction of lines with trailing whitespaces and aborts the commit when such a line is found.\\n\\nAll the `git commit` hooks are invoked with the environment variable `GIT_EDITOR=:` if the command will not bring up an editor to modify the commit message.\\n\\nThe default **pre-commit** hook, when enabled—​and with the `hooks.allownonascii` config option unset or set to false—​prevents the use of non-ASCII filenames."
    },
    "prepare-commit-msg": {
     "$$comment": "https://git-scm.com/docs/githooks#_prepare_commit_msg",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-commit right after preparing the default log message, and before the editor is started.\\n\\nIt takes one to three parameters. The first is the name of the file that contains the commit log message. The second is the source of the commit message, and can be: message (if a -m or -F option was given); template (if a -t option was given or the configuration option commit.template is set); merge (if the commit is a merge or a .git/MERGE_MSG file exists); squash (if a .git/SQUASH_MSG file exists); or commit, followed by a commit SHA-1 (if a -c, -C or --amend option was given).\\n\\nIf the exit status is non-zero, git commit will abort.\\n\\nThe purpose of the hook is to edit the message file in place, and it is not suppressed by the --no-verify option. A non-zero exit means a failure of the hook and aborts the commit. It should not be used as replacement for pre-commit hook.\\n\\nThe sample prepare-commit-msg hook that comes with Git removes the help message found in the commented portion of the commit template.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-commit\\">git-commit</a> right after preparing the default log message, and before the editor is started.</p>\\n<p>It takes one to three parameters. The first is the name of the file that contains the commit log message. The second is the source of the commit message, and can be: <code>message</code> (if a <code>-m</code> or <code>-F</code> option was given); <code>template</code> (if a <code>-t</code> option was given or the configuration option <code>commit.template</code> is set); <code>merge</code> (if the commit is a merge or a <code>.git/MERGE_MSG</code> file exists); <code>squash</code> (if a <code>.git/SQUASH_MSG</code> file exists); or <code>commit</code>, followed by a commit SHA-1 (if a <code>-c</code>, <code>-C</code> or <code>--amend</code> option was given).</p>\\n<p>If the exit status is non-zero, <code>git commit</code> will abort.</p>\\n<p>The purpose of the hook is to edit the message file in place, and it is not suppressed by the <code>--no-verify</code> option. A non-zero exit means a failure of the hook and aborts the commit. It should not be used as replacement for pre-commit hook.</p>\\n<p>The sample <code>prepare-commit-msg</code> hook that comes with Git removes the help message found in the commented portion of the commit template.</p>",
     "markdownDescription": "This hook is invoked by [git-commit](https://git-scm.com/docs/git-commit) right after preparing the default log message, and before the editor is started.\\n\\nIt takes one to three parameters. The first is the name of the file that contains the commit log message. The second is the source of the commit message, and can be: `message` (if a `-m` or `-F` option was given); `template` (if a `-t` option was given or the configuration option `commit.template` is set); `merge` (if the commit is a merge or a `.git/MERGE_MSG` file exists); `squash` (if a `.git/SQUASH_MSG` file exists); or `commit`, followed by a commit SHA-1 (if a `-c`, `-C` or `--amend` option was given).\\n\\nIf the exit status is non-zero, `git commit` will abort.\\n\\nThe purpose of the hook is to edit the message file in place, and it is not suppressed by the `--no-verify` option. A non-zero exit means a failure of the hook and aborts the commit. It should not be used as replacement for pre-commit hook.\\n\\nThe sample `prepare-commit-msg` hook that comes with Git removes the help message found in the commented portion of the commit template."
    },
    "commit-msg": {
     "$$comment": "https://git-scm.com/docs/githooks#_commit_msg",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-commit and git-merge, and can be bypassed with the --no-verify option. It takes a single parameter, the name of the file that holds the proposed commit log message. Exiting with a non-zero status causes the command to abort.\\n\\nThe hook is allowed to edit the message file in place, and can be used to normalize the message into some project standard format. It can also be used to refuse the commit after inspecting the message file.\\n\\nThe default commit-msg hook, when enabled, detects duplicate \\"Signed-off-by\\" lines, and aborts the commit if one is found.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-commit\\">git-commit</a> and <a href=\\"https://git-scm.com/docs/git-merge\\">git-merge</a>, and can be bypassed with the <code>--no-verify</code> option. It takes a single parameter, the name of the file that holds the proposed commit log message. Exiting with a non-zero status causes the command to abort.</p>\\n<p>The hook is allowed to edit the message file in place, and can be used to normalize the message into some project standard format. It can also be used to refuse the commit after inspecting the message file.</p>\\n<p>The default <em>commit-msg</em> hook, when enabled, detects duplicate \\"Signed-off-by\\" lines, and aborts the commit if one is found.</p>",
     "markdownDescription": "This hook is invoked by [git-commit](https://git-scm.com/docs/git-commit) and [git-merge](https://git-scm.com/docs/git-merge), and can be bypassed with the `--no-verify` option. It takes a single parameter, the name of the file that holds the proposed commit log message. Exiting with a non-zero status causes the command to abort.\\n\\nThe hook is allowed to edit the message file in place, and can be used to normalize the message into some project standard format. It can also be used to refuse the commit after inspecting the message file.\\n\\nThe default **commit-msg** hook, when enabled, detects duplicate \\"Signed-off-by\\" lines, and aborts the commit if one is found."
    },
    "post-commit": {
     "$$comment": "https://git-scm.com/docs/githooks#_post_commit",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-commit. It takes no parameters, and is invoked after a commit is made.\\n\\nThis hook is meant primarily for notification, and cannot affect the outcome of git commit.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-commit\\">git-commit</a>. It takes no parameters, and is invoked after a commit is made.</p>\\n<p>This hook is meant primarily for notification, and cannot affect the outcome of <code>git commit</code>.</p>",
     "markdownDescription": "This hook is invoked by [git-commit](https://git-scm.com/docs/git-commit). It takes no parameters, and is invoked after a commit is made.\\n\\nThis hook is meant primarily for notification, and cannot affect the outcome of `git commit`."
    },
    "pre-rebase": {
     "$$comment": "https://git-scm.com/docs/githooks#_pre_rebase",
     "$$ref": "#/definitions/hook",
     "description": "This hook is called by git-rebase and can be used to prevent a branch from getting rebased. The hook may be called with one or two parameters. The first parameter is the upstream from which the series was forked. The second parameter is the branch being rebased, and is not set when rebasing the current branch.",
     "x-intellij-html-description": "<p>This hook is called by <a href=\\"https://git-scm.com/docs/git-rebase\\">git-rebase</a> and can be used to prevent a branch from getting rebased. The hook may be called with one or two parameters. The first parameter is the upstream from which the series was forked. The second parameter is the branch being rebased, and is not set when rebasing the current branch.</p>",
     "markdownDescription": "This hook is called by [git-rebase](https://git-scm.com/docs/git-rebase) and can be used to prevent a branch from getting rebased. The hook may be called with one or two parameters. The first parameter is the upstream from which the series was forked. The second parameter is the branch being rebased, and is not set when rebasing the current branch."
    },
    "post-checkout": {
     "$$comment": "https://git-scm.com/docs/githooks#_post_checkout",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked when a git-checkout or git-switch is run after having updated the worktree. The hook is given three parameters: the ref of the previous HEAD, the ref of the new HEAD (which may or may not have changed), and a flag indicating whether the checkout was a branch checkout (changing branches, flag=1) or a file checkout (retrieving a file from the index, flag=0). This hook cannot affect the outcome of git switch or git checkout.\\n\\nIt is also run after git-clone, unless the --no-checkout (-n) option is used. The first parameter given to the hook is the null-ref, the second the ref of the new HEAD and the flag is always 1. Likewise for git worktree add unless --no-checkout is used.\\n\\nThis hook can be used to perform repository validity checks, auto-display differences from the previous HEAD if different, or set working dir metadata properties.",
     "x-intellij-html-description": "<p>This hook is invoked when a <a href=\\"https://git-scm.com/docs/git-checkout\\">git-checkout</a> or <a href=\\"https://git-scm.com/docs/git-switch\\">git-switch</a> is run after having updated the worktree. The hook is given three parameters: the ref of the previous HEAD, the ref of the new HEAD (which may or may not have changed), and a flag indicating whether the checkout was a branch checkout (changing branches, flag=1) or a file checkout (retrieving a file from the index, flag=0). This hook cannot affect the outcome of <code>git switch</code> or <code>git checkout</code>.</p>\\n<p>It is also run after <a href=\\"https://git-scm.com/docs/git-clone\\">git-clone</a>, unless the <code>--no-checkout</code> (<code>-n</code>) option is used. The first parameter given to the hook is the null-ref, the second the ref of the new HEAD and the flag is always 1. Likewise for <code>git worktree add</code> unless <code>--no-checkout</code> is used.</p>\\n<p>This hook can be used to perform repository validity checks, auto-display differences from the previous HEAD if different, or set working dir metadata properties.</p>",
     "markdownDescription": "This hook is invoked when a [git-checkout](https://git-scm.com/docs/git-checkout) or [git-switch](https://git-scm.com/docs/git-switch) is run after having updated the worktree. The hook is given three parameters: the ref of the previous HEAD, the ref of the new HEAD (which may or may not have changed), and a flag indicating whether the checkout was a branch checkout (changing branches, flag=1) or a file checkout (retrieving a file from the index, flag=0). This hook cannot affect the outcome of `git switch` or `git checkout`.\\n\\nIt is also run after [git-clone](https://git-scm.com/docs/git-clone), unless the `--no-checkout` (`-n`) option is used. The first parameter given to the hook is the null-ref, the second the ref of the new HEAD and the flag is always 1. Likewise for `git worktree add` unless `--no-checkout` is used.\\n\\nThis hook can be used to perform repository validity checks, auto-display differences from the previous HEAD if different, or set working dir metadata properties."
    },
    "post-merge": {
     "$$comment": "https://git-scm.com/docs/githooks#_post_merge",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-merge, which happens when a git pull is done on a local repository. The hook takes a single parameter, a status flag specifying whether or not the merge being done was a squash merge. This hook cannot affect the outcome of git merge and is not executed, if the merge failed due to conflicts.\\n\\nThis hook can be used in conjunction with a corresponding pre-commit hook to save and restore any form of metadata associated with the working tree (e.g.: permissions/ownership, ACLS, etc). See contrib/hooks/setgitperms.perl for an example of how to do this.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-merge\\">git-merge</a>, which happens when a <code>git pull</code> is done on a local repository. The hook takes a single parameter, a status flag specifying whether or not the merge being done was a squash merge. This hook cannot affect the outcome of <code>git merge</code> and is not executed, if the merge failed due to conflicts.</p>\\n<p>This hook can be used in conjunction with a corresponding pre-commit hook to save and restore any form of metadata associated with the working tree (e.g.: permissions/ownership, ACLS, etc). See contrib/hooks/setgitperms.perl for an example of how to do this.</p>",
     "markdownDescription": "This hook is invoked by [git-merge](https://git-scm.com/docs/git-merge), which happens when a `git pull` is done on a local repository. The hook takes a single parameter, a status flag specifying whether or not the merge being done was a squash merge. This hook cannot affect the outcome of `git merge` and is not executed, if the merge failed due to conflicts.\\n\\nThis hook can be used in conjunction with a corresponding pre-commit hook to save and restore any form of metadata associated with the working tree (e.g.: permissions/ownership, ACLS, etc). See contrib/hooks/setgitperms.perl for an example of how to do this."
    },
    "pre-push": {
     "$$comment": "https://git-scm.com/docs/githooks#_pre_push",
     "$$ref": "#/definitions/hook",
     "description": "This hook is called by git-push and can be used to prevent a push from taking place. The hook is called with two parameters which provide the name and location of the destination remote, if a named remote is not being used both values will be the same.\\n\\nInformation about what is to be pushed is provided on the hook's standard input with lines of the form:\\n\\n<local ref> SP <local sha1> SP <remote ref> SP <remote sha1> LF\\nFor instance, if the command git push origin master:foreign were run the hook would receive a line like the following:\\n\\nrefs/heads/master 67890 refs/heads/foreign 12345\\nalthough the full, 40-character SHA-1s would be supplied. If the foreign ref does not yet exist the <remote SHA-1> will be 40 0. If a ref is to be deleted, the <local ref> will be supplied as (delete) and the <local SHA-1> will be 40 0. If the local commit was specified by something other than a name which could be expanded (such as HEAD~, or a SHA-1) it will be supplied as it was originally given.\\n\\nIf this hook exits with a non-zero status, git push will abort without pushing anything. Information about why the push is rejected may be sent to the user by writing to standard error.",
     "x-intellij-html-description": "<p>This hook is called by <a href=\\"https://git-scm.com/docs/git-push\\">git-push</a> and can be used to prevent a push from taking place. The hook is called with two parameters which provide the name and location of the destination remote, if a named remote is not being used both values will be the same.</p>\\n<p>Information about what is to be pushed is provided on the hook's standard input with lines of the form:</p>\\n<pre>&lt;local ref&gt; SP &lt;local sha1&gt; SP &lt;remote ref&gt; SP &lt;remote sha1&gt; LF</pre>\\n<p>For instance, if the command <code>git push origin master:foreign</code> were run the hook would receive a line like the following:</p>\\n<pre>refs/heads/master 67890 refs/heads/foreign 12345</pre>\\n<p>although the full, 40-character SHA-1s would be supplied. If the foreign ref does not yet exist the <code>&lt;remote SHA-1&gt;</code> will be 40 <code>0</code>. If a ref is to be deleted, the <code>&lt;local ref&gt;</code> will be supplied as <code>(delete)</code> and the <code>&lt;local SHA-1&gt;</code> will be 40 <code>0</code>. If the local commit was specified by something other than a name which could be expanded (such as <code>HEAD~</code>, or a SHA-1) it will be supplied as it was originally given.</p>\\n<p>If this hook exits with a non-zero status, <code>git push</code> will abort without pushing anything. Information about why the push is rejected may be sent to the user by writing to standard error.</p>",
     "markdownDescription": "This hook is called by [git-push](https://git-scm.com/docs/git-push) and can be used to prevent a push from taking place. The hook is called with two parameters which provide the name and location of the destination remote, if a named remote is not being used both values will be the same.\\n\\nInformation about what is to be pushed is provided on the hook's standard input with lines of the form:\\n```\\n<local ref> SP <local sha1> SP <remote ref> SP <remote sha1> LF\\n```\\nFor instance, if the command `git push origin master:foreign` were run the hook would receive a line like the following:\\n```\\nrefs/heads/master 67890 refs/heads/foreign 12345\\n```\\nalthough the full, 40-character SHA-1s would be supplied. If the foreign ref does not yet exist the `<remote SHA-1>` will be 40 `0`. If a ref is to be deleted, the `<local ref>` will be supplied as `(delete)` and the `<local SHA-1>` will be 40 `0`. If the local commit was specified by something other than a name which could be expanded (such as `HEAD~`, or a SHA-1) it will be supplied as it was originally given.\\n\\nIf this hook exits with a non-zero status, `git push` will abort without pushing anything. Information about why the push is rejected may be sent to the user by writing to standard error."
    },
    "post-update": {
     "$$comment": "https://git-scm.com/docs/githooks#_post_update",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-receive-pack when it reacts to git push and updates reference(s) in its repository. It executes on the remote repository once after all the refs have been updated.\\n\\nIt takes a variable number of parameters, each of which is the name of ref that was actually updated.\\n\\nThis hook is meant primarily for notification, and cannot affect the outcome of git receive-pack.\\n\\nThe post-update hook can tell what are the heads that were pushed, but it does not know what their original and updated values are, so it is a poor place to do log old..new. The post-receive hook does get both original and updated values of the refs. You might consider it instead if you need them.\\n\\nWhen enabled, the default post-update hook runs git update-server-info to keep the information used by dumb transports (e.g., HTTP) up to date. If you are publishing a Git repository that is accessible via HTTP, you should probably enable this hook.\\n\\nBoth standard output and standard error output are forwarded to git send-pack on the other end, so you can simply echo messages for the user.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-receive-pack\\">git-receive-pack</a> when it reacts to <code>git push</code> and updates reference(s) in its repository. It executes on the remote repository once after all the refs have been updated.</p>\\n<p>It takes a variable number of parameters, each of which is the name of ref that was actually updated.</p>\\n<p>This hook is meant primarily for notification, and cannot affect the outcome of <code>git receive-pack</code>.</p>\\n<p>The <em>post-update</em> hook can tell what are the heads that were pushed, but it does not know what their original and updated values are, so it is a poor place to do log old..new. The <a href=\\"https://git-scm.com/docs/githooks#post-receive\\"><em>post-receive</em></a> hook does get both original and updated values of the refs. You might consider it instead if you need them.</p>\\n<p>When enabled, the default <em>post-update</em> hook runs <code>git update-server-info</code> to keep the information used by dumb transports (e.g., HTTP) up to date. If you are publishing a Git repository that is accessible via HTTP, you should probably enable this hook.</p>\\n<p>Both standard output and standard error output are forwarded to <code>git send-pack</code> on the other end, so you can simply <code>echo</code> messages for the user.</p>",
     "markdownDescription": "This hook is invoked by [git-receive-pack](https://git-scm.com/docs/git-receive-pack) when it reacts to `git push` and updates reference(s) in its repository. It executes on the remote repository once after all the refs have been updated.\\n\\nIt takes a variable number of parameters, each of which is the name of ref that was actually updated.\\n\\nThis hook is meant primarily for notification, and cannot affect the outcome of `git receive-pack`.\\n\\nThe **post-update** hook can tell what are the heads that were pushed, but it does not know what their original and updated values are, so it is a poor place to do log old..new. The **post-receive** hook does get both original and updated values of the refs. You might consider it instead if you need them.\\n\\nWhen enabled, the default **post-update** hook runs `git update-server-info` to keep the information used by dumb transports (e.g., HTTP) up to date. If you are publishing a Git repository that is accessible via HTTP, you should probably enable this hook.\\n\\nBoth standard output and standard error output are forwarded to `git send-pack` on the other end, so you can simply `echo` messages for the user."
    },
    "push-to-checkout": {
     "$$comment": "https://git-scm.com/docs/githooks#_push_to_checkout",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-receive-pack when it reacts to git push and updates reference(s) in its repository, and when the push tries to update the branch that is currently checked out and the receive.denyCurrentBranch configuration variable is set to updateInstead. Such a push by default is refused if the working tree and the index of the remote repository has any difference from the currently checked out commit; when both the working tree and the index match the current commit, they are updated to match the newly pushed tip of the branch. This hook is to be used to override the default behaviour.\\n\\nThe hook receives the commit with which the tip of the current branch is going to be updated. It can exit with a non-zero status to refuse the push (when it does so, it must not modify the index or the working tree). Or it can make any necessary changes to the working tree and to the index to bring them to the desired state when the tip of the current branch is updated to the new commit, and exit with a zero status.\\n\\nFor example, the hook can simply run git read-tree -u -m HEAD \\"$$1\\" in order to emulate git fetch that is run in the reverse direction with git push, as the two-tree form of git read-tree -u -m is essentially the same as git switch or git checkout that switches branches while keeping the local changes in the working tree that do not interfere with the difference between the branches.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-receive-pack\\">git-receive-pack</a> when it reacts to <code>git push</code> and updates reference(s) in its repository, and when the push tries to update the branch that is currently checked out and the <code>receive.denyCurrentBranch</code> configuration variable is set to <code>updateInstead</code>. Such a push by default is refused if the working tree and the index of the remote repository has any difference from the currently checked out commit; when both the working tree and the index match the current commit, they are updated to match the newly pushed tip of the branch. This hook is to be used to override the default behaviour.</p>\\n<p>The hook receives the commit with which the tip of the current branch is going to be updated. It can exit with a non-zero status to refuse the push (when it does so, it must not modify the index or the working tree). Or it can make any necessary changes to the working tree and to the index to bring them to the desired state when the tip of the current branch is updated to the new commit, and exit with a zero status.</p>\\n<p>For example, the hook can simply run <code>git read-tree -u -m HEAD \\"$$1\\"</code> in order to emulate <code>git fetch</code> that is run in the reverse direction with <code>git push</code>, as the two-tree form of <code>git read-tree -u -m</code> is essentially the same as <code>git switch</code> or <code>git checkout</code> that switches branches while keeping the local changes in the working tree that do not interfere with the difference between the branches.</p>",
     "markdownDescription": "This hook is invoked by [git-receive-pack](https://git-scm.com/docs/git-receive-pack) when it reacts to `git push` and updates reference(s) in its repository, and when the push tries to update the branch that is currently checked out and the `receive.denyCurrentBranch` configuration variable is set to `updateInstead`. Such a push by default is refused if the working tree and the index of the remote repository has any difference from the currently checked out commit; when both the working tree and the index match the current commit, they are updated to match the newly pushed tip of the branch. This hook is to be used to override the default behaviour.\\n\\nThe hook receives the commit with which the tip of the current branch is going to be updated. It can exit with a non-zero status to refuse the push (when it does so, it must not modify the index or the working tree). Or it can make any necessary changes to the working tree and to the index to bring them to the desired state when the tip of the current branch is updated to the new commit, and exit with a zero status.\\n\\nFor example, the hook can simply run `git read-tree -u -m HEAD \\"$$1\\"` in order to emulate `git fetch` that is run in the reverse direction with `git push`, as the two-tree form of `git read-tree -u -m` is essentially the same as `git switch` or `git checkout` that switches branches while keeping the local changes in the working tree that do not interfere with the difference between the branches."
    },
    "pre-auto-gc": {
     "$$comment": "https://git-scm.com/docs/githooks#_pre_auto_gc",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git gc --auto (see git-gc). It takes no parameter, and exiting with non-zero status from this script causes the git gc --auto to abort.",
     "x-intellij-html-description": "<p>This hook is invoked by <code>git gc --auto</code> (see <a href=\\"https://git-scm.com/docs/git-gc\\">git-gc</a>). It takes no parameter, and exiting with non-zero status from this script causes the <code>git gc --auto</code> to abort.</p>",
     "markdownDescription": "This hook is invoked by `git gc --auto` (see [git-gc](https://git-scm.com/docs/git-gc)). It takes no parameter, and exiting with non-zero status from this script causes the `git gc --auto` to abort."
    },
    "post-rewrite": {
     "$$comment": "https://git-scm.com/docs/githooks#_post_rewrite",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by commands that rewrite commits (git-commit when called with --amend and git-rebase; however, full-history (re)writing tools like git-fast-import or git-filter-repo typically do not call it!). Its first argument denotes the command it was invoked by: currently one of amend or rebase. Further command-dependent arguments may be passed in the future.\\n\\nThe hook receives a list of the rewritten commits on stdin, in the format\\n\\n<old-sha1> SP <new-sha1> [ SP <extra-info> ] LF\\nThe extra-info is again command-dependent. If it is empty, the preceding SP is also omitted. Currently, no commands pass any extra-info.\\n\\nThe hook always runs after the automatic note copying (see \\"notes.rewrite.<command>\\" in git-config) has happened, and thus has access to these notes.\\n\\nThe following command-specific comments apply:\\n\\nrebase\\nFor the squash and fixup operation, all commits that were squashed are listed as being rewritten to the squashed commit. This means that there will be several lines sharing the same new-sha1.\\n\\nThe commits are guaranteed to be listed in the order that they were processed by rebase.",
     "x-intellij-html-description": "<p>This hook is invoked by commands that rewrite commits (<a href=\\"https://git-scm.com/docs/git-commit\\">git-commit</a> when called with <code>--amend</code> and <a href=\\"https://git-scm.com/docs/git-rebase\\">git-rebase</a>; however, full-history (re)writing tools like <a href=\\"https://git-scm.com/docs/git-fast-import\\">git-fast-import</a> or <a href=\\"https://github.com/newren/git-filter-repo\\">git-filter-repo</a> typically do not call it!). Its first argument denotes the command it was invoked by: currently one of <code>amend</code> or <code>rebase</code>. Further command-dependent arguments may be passed in the future.</p>\\n<p>The hook receives a list of the rewritten commits on stdin, in the format</p>\\n<pre>&lt;old-sha1&gt; SP &lt;new-sha1&gt; [ SP &lt;extra-info&gt; ] LF</pre>\\n<p>The <em>extra-info</em> is again command-dependent. If it is empty, the preceding SP is also omitted. Currently, no commands pass any <em>extra-info</em>.</p>\\n<p>The hook always runs after the automatic note copying (see \\"notes.rewrite.&lt;command&gt;\\" in <a href=\\"https://git-scm.com/docs/git-config\\">git-config</a>) has happened, and thus has access to these notes.</p>\\n<p>The following command-specific comments apply:</p>\\n<dl>\\n    <dt>rebase</dt>\\n    <dd>\\n        <p>For the <em>squash</em> and <em>fixup</em> operation, all commits that were squashed are listed as being rewritten to the squashed commit. This means that there will be several lines sharing the same <em>new-sha1</em>.</p>\\n        <p>The commits are guaranteed to be listed in the order that they were processed by rebase.</p>\\n    </dd>\\n</dl>",
     "markdownDescription": "This hook is invoked by commands that rewrite commits ([git-commit](https://git-scm.com/docs/git-commit) when called with `--amend` and [git-rebase](https://git-scm.com/docs/git-rebase); however, full-history (re)writing tools like [git-fast-import](https://git-scm.com/docs/git-fast-import) or [git-filter-repo](https://github.com/newren/git-filter-repo) typically do not call it!). Its first argument denotes the command it was invoked by: currently one of `amend` or `rebase`. Further command-dependent arguments may be passed in the future.\\n\\nThe hook receives a list of the rewritten commits on stdin, in the format\\n```\\n<old-sha1> SP <new-sha1> [ SP <extra-info> ] LF\\n```\\nThe **extra-info** is again command-dependent. If it is empty, the preceding SP is also omitted. Currently, no commands pass any **extra-info**.\\n\\nThe hook always runs after the automatic note copying (see \\"notes.rewrite.\\\\<command\\\\>\\" in [git-config](https://git-scm.com/docs/git-config)) has happened, and thus has access to these notes.\\n\\nThe following command-specific comments apply:\\n\\n**rebase**  \\nFor the **squash** and **fixup** operation, all commits that were squashed are listed as being rewritten to the squashed commit. This means that there will be several lines sharing the same **new-sha1**.  \\nThe commits are guaranteed to be listed in the order that they were processed by rebase."
    },
    "sendemail-validate": {
     "$$comment": "https://git-scm.com/docs/githooks#_sendemail_validate",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-send-email. It takes a single parameter, the name of the file that holds the e-mail to be sent. Exiting with a non-zero status causes git send-email to abort before sending any e-mails.",
     "x-intellij-html-description": "<p>This hook is invoked by <a href=\\"https://git-scm.com/docs/git-send-email\\">git-send-email</a>. It takes a single parameter, the name of the file that holds the e-mail to be sent. Exiting with a non-zero status causes <code>git send-email</code> to abort before sending any e-mails.</p>",
     "markdownDescription": "This hook is invoked by [git-send-email](https://git-scm.com/docs/git-send-email). It takes a single parameter, the name of the file that holds the e-mail to be sent. Exiting with a non-zero status causes `git send-email` to abort before sending any e-mails."
    },
    "fsmonitor-watchman": {
     "$$comment": "https://git-scm.com/docs/githooks#_fsmonitor_watchman",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked when the configuration option core.fsmonitor is set to .git/hooks/fsmonitor-watchman. It takes two arguments, a version (currently 1) and the time in elapsed nanoseconds since midnight, January 1, 1970.\\n\\nThe hook should output to stdout the list of all files in the working directory that may have changed since the requested time. The logic should be inclusive so that it does not miss any potential changes. The paths should be relative to the root of the working directory and be separated by a single NUL.\\n\\nIt is OK to include files which have not actually changed. All changes including newly-created and deleted files should be included. When files are renamed, both the old and the new name should be included.\\n\\nGit will limit what files it checks for changes as well as which directories are checked for untracked files based on the path names given.\\n\\nAn optimized way to tell git \\"all files have changed\\" is to return the filename /.\\n\\nThe exit status determines whether git will use the data from the hook to limit its search. On error, it will fall back to verifying all files and folders.",
     "x-intellij-html-description": "<p>This hook is invoked when the configuration option <code>core.fsmonitor</code> is set to <code>.git/hooks/fsmonitor-watchman</code> or <code>.git/hooks/fsmonitor-watchmanv2</code> depending on the version of the hook to use.</p>\\n<p>Version 1 takes two arguments, a version (1) and the time in elapsed nanoseconds since midnight, January 1, 1970.</p>\\n<p>Version 2 takes two arguments, a version (2) and a token that is used for identifying changes since the token. For watchman this would be a clock id. This version must output to stdout the new token followed by a NUL before the list of files.</p>\\n<p>The hook should output to stdout the list of all files in the working directory that may have changed since the requested time. The logic should be inclusive so that it does not miss any potential changes. The paths should be relative to the root of the working directory and be separated by a single NUL.</p>\\n<p>It is OK to include files which have not actually changed. All changes including newly-created and deleted files should be included. When files are renamed, both the old and the new name should be included.</p>\\n<p>Git will limit what files it checks for changes as well as which directories are checked for untracked files based on the path names given.</p>\\n<p>An optimized way to tell git \\"all files have changed\\" is to return the filename <code>/</code>.</p>\\n<p>The exit status determines whether git will use the data from the hook to limit its search. On error, it will fall back to verifying all files and folders.</p>",
     "markdownDescription": "This hook is invoked when the configuration option `core.fsmonitor` is set to `.git/hooks/fsmonitor-watchman`. It takes two arguments, a version (currently 1) and the time in elapsed nanoseconds since midnight, January 1, 1970.\\n\\nThe hook should output to stdout the list of all files in the working directory that may have changed since the requested time. The logic should be inclusive so that it does not miss any potential changes. The paths should be relative to the root of the working directory and be separated by a single NUL.\\n\\nIt is OK to include files which have not actually changed. All changes including newly-created and deleted files should be included. When files are renamed, both the old and the new name should be included.\\n\\nGit will limit what files it checks for changes as well as which directories are checked for untracked files based on the path names given.\\n\\nAn optimized way to tell git \\"all files have changed\\" is to return the filename `/`.\\n\\nThe exit status determines whether git will use the data from the hook to limit its search. On error, it will fall back to verifying all files and folders."
    },
    "p4-pre-submit": {
     "$$comment": "https://git-scm.com/docs/githooks#_p4_pre_submit",
     "$$ref": "#/definitions/hook",
     "description": "This hook is invoked by git-p4 submit. It takes no parameters and nothing from standard input. Exiting with non-zero status from this script prevent git-p4 submit from launching. Run git-p4 submit --help for details.",
     "x-intellij-html-description": "<p>This hook is invoked by <code>git-p4 submit</code>. It takes no parameters and nothing from standard input. Exiting with non-zero status from this script prevent <code>git-p4 submit</code> from launching. Run <code>git-p4 submit --help</code> for details.</p>",
     "markdownDescription": "This hook is invoked by `git-p4 submit`. It takes no parameters and nothing from standard input. Exiting with non-zero status from this script prevent `git-p4 submit` from launching. Run `git-p4 submit --help` for details."
    }
   },
   "additionalProperties": false
  }
 },
 "required": [
  "hooks"
 ],
 "title": "Husky configuration.",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// dotnetcli.host.json (768 bytes)
object DotnetCliHostSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/dotnetcli.host.json",
 "definitions": {
  "symbolInfo": {
   "type": "object",
   "properties": {
    "isHidden": {
     "anyOf": [
      {
       "type": "boolean"
      },
      {
       "type": "string",
       "pattern": "^(?:true|false)$$"
      }
     ]
    },
    "longName": {
     "type": "string"
    },
    "shortName": {
     "type": "string"
    }
   }
  }
 },
 "properties": {
  "symbolInfo": {
   "type": "object",
   "additionalProperties": {
    "$$ref": "#/definitions/symbolInfo"
   }
  },
  "usageExamples": {
   "type": "array",
   "items": {
    "type": "string"
   }
  }
 },
 "title": "JSON schema for .NET CLI template host files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// jsdoc-1.0.0.json (9047 bytes)
object JsdocSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/jsdoc-1.0.0.json",
 "properties": {
  "plugins": {
   "type": "array",
   "title": "Configuring plugins",
   "description": "Enables plugins for JSDoc",
   "default": [],
   "examples": [
    [
     "plugins/markdown",
     "plugins/summarize"
    ]
   ]
  },
  "recurseDepth": {
   "$$comment": "Is used only if `jsdoc` command is invoked with `-r` flag",
   "type": "integer",
   "title": "Specifying recursion depth",
   "default": 10,
   "description": "Controls recursion depth for source files and tutorials"
  },
  "source": {
   "type": "object",
   "title": "Specifying input files",
   "description": "Determines the set of input files",
   "properties": {
    "include": {
     "$$comment": "`-r` flag for `jsdoc` command will recurse in subdirectories of paths listed",
     "examples": [
      [
       "myProject/a.js",
       "myProject/lib",
       "myProject/_private"
      ]
     ],
     "type": "array",
     "title": "Input files paths",
     "description": "An array of paths to input files"
    },
    "exclude": {
     "$$comment": "With JSDoc ^3.3.0 may include subdirectories of include",
     "examples": [
      [
       "myProject/lib/ignore.js"
      ]
     ],
     "type": "array",
     "title": "Input files exclusion paths",
     "description": "An array of paths to exclude from input"
    },
    "includePattern": {
     "$$comment": "By default, .js, .jsx, .jsdoc files are included",
     "type": "string",
     "title": "Inclusion RegExp",
     "default": ".+\\\\.js(doc|x)?$$",
     "description": "Forces input filenames to match regular expression"
    },
    "excludePattern": {
     "$$comment": "By default, underscored files and folders are excluded",
     "type": "string",
     "title": "Exclusion RegExp",
     "default": "(^|\\\\/|\\\\\\\\)_",
     "description": "Forces input filenames to match regular expression"
    }
   },
   "additionalProperties": false
  },
  "sourceType": {
   "type": "string",
   "enum": [
    "module",
    "script"
   ],
   "default": "module",
   "title": "Specifying source type",
   "description": "Determines how input files are parsed"
  },
  "opts": {
   "$$comment": "The command line options take precedence over config file",
   "type": "object",
   "title": "Incorporating CLI options",
   "description": "Determines flags that `jsdoc` command will be invoked with",
   "additionalProperties": false,
   "properties": {
    "access": {
     "$$comment": "Equivalent to `-a` flag",
     "default": "all",
     "description": "Only display symbols with the given `access` property",
     "enum": [
      "all",
      "private",
      "protected",
      "public",
      "undefined"
     ],
     "title": "Symbol access",
     "type": "string"
    },
    "debug": {
     "$$comment": "Equivalent to `--debug` flag",
     "description": "Log information that can help debug issues in JSDoc itself",
     "title": "Log debug info",
     "type": "boolean"
    },
    "destination": {
     "$$comment": "Equivalent to `-d` flag",
     "default": "./out/",
     "description": "The path to the output folder for the generated documentation",
     "title": "Output folder",
     "type": "string"
    },
    "encoding": {
     "$$comment": "Equivalent to `-e` flag",
     "default": "utf8",
     "description": "Assume this encoding when reading all source files",
     "title": "Input files encoding",
     "type": "string"
    },
    "package": {
     "$$comment": "Equivalent to `-p` flag",
     "description": "The `package.json` file that contains the project name, version, and other details",
     "title": "Package",
     "type": "string"
    },
    "pedantic": {
     "default": false,
     "description": "Treat errors as fatal errors, and treat warnings as errors",
     "title": "Pedantic",
     "type": "boolean"
    },
    "readme": {
     "$$comment": "Equivalent to `-R` flag",
     "description": "The README.md file to include in the generated documentation",
     "title": "README.md",
     "type": "string"
    },
    "recurse": {
     "$$comment": "Equivalent to `-r` flag",
     "default": false,
     "description": "Recurses to subdirectories when searching input files",
     "title": "Recurse to subdirectories",
     "type": "boolean"
    },
    "template": {
     "$$comment": "Equivalent to `-t` flag",
     "default": "templates/default",
     "description": "The path to the template to use for generating output",
     "title": "Output template",
     "type": "string"
    },
    "test": {
     "$$comment": "Equivalent to `-T` flag. Won't work if installed via NPM",
     "default": false,
     "description": "Run JSDoc's test suite, and print the results to the console",
     "title": "Run tests",
     "type": "boolean"
    },
    "tutorials": {
     "$$comment": "Equivalent to `-u` flag",
     "description": "Directory in which JSDoc should search for tutorials",
     "examples": [
      "path/to/tutorials",
      "./docs/tutorials"
     ],
     "title": "Tutorials path",
     "type": "string"
    }
   }
  },
  "tags": {
   "additionalProperties": false,
   "description": "Controls allowed JSDoc tags and their interpretation",
   "properties": {
    "allowUnknownTags": {
     "$$comment": "If set to `false`, emits a warning. If set to an array, whitelists tags",
     "default": true,
     "description": "Determines how to handle unrecognized tags",
     "items": {
      "title": "JSDoc tag",
      "type": "string"
     },
     "title": "Unknown tags",
     "type": [
      "boolean",
      "array"
     ],
     "uniqueItems": true
    },
    "dictionaries": {
     "description": "Controls which tags JSDoc recognizes and how they are interpreted",
     "items": {
      "$$comment": "^3.3.0 two dictionaries: JSDoc and Closure Compiler",
      "default": [
       "jsdoc",
       "closure"
      ],
      "enum": [
       "jsdoc",
       "closure"
      ],
      "title": "Dictionary",
      "type": "string"
     },
     "title": "JSDoc dictionaries",
     "type": "array"
    }
   },
   "title": "Configuring tags and tag dictionaries",
   "type": "object"
  },
  "templates": {
   "description": "Affects the appearance and content of generated documentation",
   "properties": {
    "cleverLinks": {
     "$$comment": "If `true`, text of @link tag that is a URL will be rendered in normal font, else in monospace",
     "default": false,
     "description": "Controls @link tag text rendering",
     "title": "@link URL",
     "type": "boolean"
    },
    "default": {
     "properties": {
      "includeDate": {
       "$$comment": "^3.3.0 can be set to `false` to omit current date",
       "default": true,
       "description": "Controls if current date is displayed in the footer of documentation",
       "title": "Showing the current date",
       "type": "boolean"
      },
      "layoutFile": {
       "$$comment": "^3.4.0 can be set to custom layout file",
       "default": "layout.tmpl",
       "description": "Path to layout file to use for documentation template",
       "title": "Overriding layout file",
       "type": "string"
      },
      "outputSourceFiles": {
       "$$comment": "^3.3.0 can be set to `false` to remove links to source files",
       "default": true,
       "description": "Disables pretty-printed source files",
       "title": "Generating pretty-printed source files",
       "type": "boolean"
      },
      "staticFiles": {
       "additionalProperties": false,
       "properties": {
        "exclude": {
         "description": "An array of paths that should not be copied to the output directory",
         "items": {
          "type": "string"
         },
         "type": "array"
        },
        "excludePattern": {
         "description": "A regular expression indicating which files to skip",
         "type": "string"
        },
        "include": {
         "description": "An array of paths whose contents should be copied to the output directory",
         "items": {
          "type": "string"
         },
         "type": "array"
        },
        "includePattern": {
         "description": "A regular expression indicating which files to copy",
         "type": "string"
        }
       },
       "title": "Copying static files",
       "type": "object"
      },
      "useLongnameInNav": {
       "$$comment": "^3.4.0 can be set to `true` to use longhands",
       "default": false,
       "description": "Controls if shortened or longhand version of a symbol will be shown in documentation",
       "title": "Showing longnames",
       "type": "boolean"
      }
     }
    },
    "monospaceLinks": {
     "$$comment": "If `true`, all link text of inline @link tag will be rendered in monospace font",
     "default": false,
     "description": "Controls @link tag text rendering",
     "title": "@link text",
     "type": "boolean"
    }
   },
   "title": "Configuring templates",
   "type": "object"
  }
 },
 "title": "JSON Schema for JSDoc configuration files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// web-manifest-combined.json (341 bytes)
object WebManifestCombinedSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/web-manifest-combined.json",
 "allOf": [
  {
   "$$ref": "web-manifest.json"
  },
  {
   "$$ref": "web-manifest-app-info.json"
  },
  {
   "$$ref": "web-manifest-share-target.json"
  }
 ],
 "title": "JSON schema for Web Application manifest files"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// prettierrc.json (10970 bytes)
object PrettierSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://www.schemastore.org/prettierrc.json",
 "definitions": {
  "optionsDefinition": {
   "type": "object",
   "properties": {
    "arrowParens": {
     "description": "Include parentheses around a sole arrow function parameter.",
     "default": "always",
     "oneOf": [
      {
       "const": "always",
       "description": "Always include parens. Example: `(x) => x`"
      },
      {
       "const": "avoid",
       "description": "Omit parens when possible. Example: `x => x`"
      }
     ]
    },
    "bracketSameLine": {
     "description": "Put > of opening tags on the last line instead of on a new line.",
     "default": false,
     "type": "boolean"
    },
    "bracketSpacing": {
     "description": "Print spaces between brackets.",
     "default": true,
     "type": "boolean"
    },
    "checkIgnorePragma": {
     "description": "Check whether the file's first docblock comment contains '@noprettier' or '@noformat' to determine if it should be formatted.",
     "default": false,
     "type": "boolean"
    },
    "cursorOffset": {
     "description": "Print (to stderr) where a cursor at the given position would move to after formatting.",
     "default": -1,
     "type": "integer"
    },
    "embeddedLanguageFormatting": {
     "description": "Control how Prettier formats quoted code embedded in the file.",
     "default": "auto",
     "oneOf": [
      {
       "const": "auto",
       "description": "Format embedded code if Prettier can automatically identify it."
      },
      {
       "const": "off",
       "description": "Never automatically format embedded code."
      }
     ]
    },
    "endOfLine": {
     "description": "Which end of line characters to apply.",
     "default": "lf",
     "oneOf": [
      {
       "const": "lf",
       "description": "Line Feed only (\\\\n), common on Linux and macOS as well as inside git repos"
      },
      {
       "const": "crlf",
       "description": "Carriage Return + Line Feed characters (\\\\r\\\\n), common on Windows"
      },
      {
       "const": "cr",
       "description": "Carriage Return character only (\\\\r), used very rarely"
      },
      {
       "const": "auto",
       "description": "Maintain existing\\n(mixed values within one file are normalised by looking at what's used after the first line)"
      }
     ]
    },
    "experimentalOperatorPosition": {
     "description": "Where to print operators when binary expressions wrap lines.",
     "default": "end",
     "oneOf": [
      {
       "const": "start",
       "description": "Print operators at the start of new lines."
      },
      {
       "const": "end",
       "description": "Print operators at the end of previous lines."
      }
     ]
    },
    "experimentalTernaries": {
     "description": "Use curious ternaries, with the question mark after the condition.",
     "default": false,
     "type": "boolean"
    },
    "filepath": {
     "description": "Specify the input filepath. This will be used to do parser inference.",
     "type": "string"
    },
    "htmlWhitespaceSensitivity": {
     "description": "How to handle whitespaces in HTML.",
     "default": "css",
     "oneOf": [
      {
       "const": "css",
       "description": "Respect the default value of CSS display property."
      },
      {
       "const": "strict",
       "description": "Whitespaces are considered sensitive."
      },
      {
       "const": "ignore",
       "description": "Whitespaces are considered insensitive."
      }
     ]
    },
    "insertPragma": {
     "description": "Insert @format pragma into file's first docblock comment.",
     "default": false,
     "type": "boolean"
    },
    "jsxSingleQuote": {
     "description": "Use single quotes in JSX.",
     "default": false,
     "type": "boolean"
    },
    "objectWrap": {
     "description": "How to wrap object literals.",
     "default": "preserve",
     "oneOf": [
      {
       "const": "preserve",
       "description": "Keep as multi-line, if there is a newline between the opening brace and first property."
      },
      {
       "const": "collapse",
       "description": "Fit to a single line when possible."
      }
     ]
    },
    "parser": {
     "description": "Which parser to use.",
     "anyOf": [
      {
       "const": "flow",
       "description": "Flow"
      },
      {
       "const": "babel",
       "description": "JavaScript"
      },
      {
       "const": "babel-flow",
       "description": "Flow"
      },
      {
       "const": "babel-ts",
       "description": "TypeScript"
      },
      {
       "const": "typescript",
       "description": "TypeScript"
      },
      {
       "const": "acorn",
       "description": "JavaScript"
      },
      {
       "const": "espree",
       "description": "JavaScript"
      },
      {
       "const": "meriyah",
       "description": "JavaScript"
      },
      {
       "const": "css",
       "description": "CSS"
      },
      {
       "const": "less",
       "description": "Less"
      },
      {
       "const": "scss",
       "description": "SCSS"
      },
      {
       "const": "json",
       "description": "JSON"
      },
      {
       "const": "json5",
       "description": "JSON5"
      },
      {
       "const": "jsonc",
       "description": "JSON with Comments"
      },
      {
       "const": "json-stringify",
       "description": "JSON.stringify"
      },
      {
       "const": "graphql",
       "description": "GraphQL"
      },
      {
       "const": "markdown",
       "description": "Markdown"
      },
      {
       "const": "mdx",
       "description": "MDX"
      },
      {
       "const": "vue",
       "description": "Vue"
      },
      {
       "const": "yaml",
       "description": "YAML"
      },
      {
       "const": "glimmer",
       "description": "Ember / Handlebars"
      },
      {
       "const": "html",
       "description": "HTML"
      },
      {
       "const": "angular",
       "description": "Angular"
      },
      {
       "const": "lwc",
       "description": "Lightning Web Components"
      },
      {
       "const": "mjml",
       "description": "MJML"
      },
      {
       "type": "string",
       "description": "Custom parser"
      }
     ]
    },
    "plugins": {
     "description": "Add a plugin. Multiple plugins can be passed as separate `--plugin`s.",
     "default": [],
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "printWidth": {
     "description": "The line length where Prettier will try wrap.",
     "default": 80,
     "type": "integer"
    },
    "proseWrap": {
     "description": "How to wrap prose.",
     "default": "preserve",
     "oneOf": [
      {
       "const": "always",
       "description": "Wrap prose if it exceeds the print width."
      },
      {
       "const": "never",
       "description": "Do not wrap prose."
      },
      {
       "const": "preserve",
       "description": "Wrap prose as-is."
      }
     ]
    },
    "quoteProps": {
     "description": "Change when properties in objects are quoted.",
     "default": "as-needed",
     "oneOf": [
      {
       "const": "as-needed",
       "description": "Only add quotes around object properties where required."
      },
      {
       "const": "consistent",
       "description": "If at least one property in an object requires quotes, quote all properties."
      },
      {
       "const": "preserve",
       "description": "Respect the input use of quotes in object properties."
      }
     ]
    },
    "rangeEnd": {
     "description": "Format code ending at a given character offset (exclusive).\\nThe range will extend forwards to the end of the selected statement.",
     "default": null,
     "type": "integer"
    },
    "rangeStart": {
     "description": "Format code starting at a given character offset.\\nThe range will extend backwards to the start of the first line containing the selected statement.",
     "default": 0,
     "type": "integer"
    },
    "requirePragma": {
     "description": "Require either '@prettier' or '@format' to be present in the file's first docblock comment in order for it to be formatted.",
     "default": false,
     "type": "boolean"
    },
    "semi": {
     "description": "Print semicolons.",
     "default": true,
     "type": "boolean"
    },
    "singleAttributePerLine": {
     "description": "Enforce single attribute per line in HTML, Vue and JSX.",
     "default": false,
     "type": "boolean"
    },
    "singleQuote": {
     "description": "Use single quotes instead of double quotes.",
     "default": false,
     "type": "boolean"
    },
    "tabWidth": {
     "description": "Number of spaces per indentation level.",
     "default": 2,
     "type": "integer"
    },
    "trailingComma": {
     "description": "Print trailing commas wherever possible when multi-line.",
     "default": "all",
     "oneOf": [
      {
       "const": "all",
       "description": "Trailing commas wherever possible (including function arguments)."
      },
      {
       "const": "es5",
       "description": "Trailing commas where valid in ES5 (objects, arrays, etc.)"
      },
      {
       "const": "none",
       "description": "No trailing commas."
      }
     ]
    },
    "useTabs": {
     "description": "Indent with tabs instead of spaces.",
     "default": false,
     "type": "boolean"
    },
    "vueIndentScriptAndStyle": {
     "description": "Indent script and style tags in Vue files.",
     "default": false,
     "type": "boolean"
    }
   }
  },
  "overridesDefinition": {
   "type": "object",
   "properties": {
    "overrides": {
     "type": "array",
     "description": "Provide a list of patterns to override prettier configuration.",
     "items": {
      "type": "object",
      "required": [
       "files"
      ],
      "properties": {
       "files": {
        "description": "Include these files in this override.",
        "oneOf": [
         {
          "type": "string"
         },
         {
          "type": "array",
          "items": {
           "type": "string"
          }
         }
        ]
       },
       "excludeFiles": {
        "description": "Exclude these files from this override.",
        "oneOf": [
         {
          "type": "string"
         },
         {
          "type": "array",
          "items": {
           "type": "string"
          }
         }
        ]
       },
       "options": {
        "$$ref": "#/definitions/optionsDefinition",
        "type": "object",
        "description": "The options to apply for this override."
       }
      },
      "additionalProperties": false
     }
    }
   }
  }
 },
 "oneOf": [
  {
   "type": "object",
   "allOf": [
    {
     "$$ref": "#/definitions/optionsDefinition"
    },
    {
     "$$ref": "#/definitions/overridesDefinition"
    }
   ]
  },
  {
   "type": "string"
  }
 ],
 "title": "Schema for .prettierrc"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// mocharc.json (2687 bytes)
object MochaSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/mocharc",
 "additionalProperties": true,
 "definitions": {
  "bool": {
   "type": "boolean"
  },
  "int": {
   "type": "integer",
   "minimum": 0
  },
  "string": {
   "type": "string"
  },
  "string-array": {
   "anyOf": [
    {
     "type": "string"
    },
    {
     "type": "array",
     "items": {
      "type": "string"
     }
    }
   ]
  }
 },
 "description": "A JSON schema describing a .mocharc.[json|yml|yaml] file",
 "properties": {
  "allow-uncaught": {
   "$$ref": "#/definitions/bool"
  },
  "async-only": {
   "$$ref": "#/definitions/bool"
  },
  "bail": {
   "$$ref": "#/definitions/bool"
  },
  "check-leaks": {
   "$$ref": "#/definitions/bool"
  },
  "delay": {
   "$$ref": "#/definitions/bool"
  },
  "exit": {
   "$$ref": "#/definitions/bool"
  },
  "forbid-only": {
   "$$ref": "#/definitions/bool"
  },
  "forbid-pending": {
   "$$ref": "#/definitions/bool"
  },
  "global": {
   "$$ref": "#/definitions/string-array"
  },
  "jobs": {
   "$$ref": "#/definitions/int"
  },
  "parallel": {
   "$$ref": "#/definitions/bool"
  },
  "retries": {
   "$$ref": "#/definitions/int"
  },
  "slow": {
   "$$ref": "#/definitions/int"
  },
  "timeout": {
   "$$ref": "#/definitions/int"
  },
  "ui": {
   "$$ref": "#/definitions/string"
  },
  "color": {
   "$$ref": "#/definitions/bool"
  },
  "diff": {
   "$$ref": "#/definitions/bool"
  },
  "full-trace": {
   "$$ref": "#/definitions/bool"
  },
  "growl": {
   "$$ref": "#/definitions/bool"
  },
  "inline-diffs": {
   "$$ref": "#/definitions/bool"
  },
  "reporter": {
   "$$ref": "#/definitions/string"
  },
  "reporter-option": {
   "$$ref": "#/definitions/string-array"
  },
  "config": {
   "$$ref": "#/definitions/string"
  },
  "package": {
   "$$ref": "#/definitions/string"
  },
  "extension": {
   "$$ref": "#/definitions/string-array"
  },
  "file": {
   "$$ref": "#/definitions/string-array"
  },
  "ignore": {
   "$$ref": "#/definitions/string-array"
  },
  "recursive": {
   "$$ref": "#/definitions/bool"
  },
  "require": {
   "$$ref": "#/definitions/string-array"
  },
  "sort": {
   "$$ref": "#/definitions/bool"
  },
  "watch": {
   "$$ref": "#/definitions/bool"
  },
  "watch-files": {
   "$$ref": "#/definitions/string-array"
  },
  "watch-ignore": {
   "$$ref": "#/definitions/string-array"
  },
  "fgrep": {
   "$$ref": "#/definitions/string"
  },
  "grep": {
   "$$ref": "#/definitions/string"
  },
  "invert": {
   "$$ref": "#/definitions/bool"
  },
  "spec": {
   "$$ref": "#/definitions/string-array"
  },
  "enable-source-maps": {
   "$$ref": "#/definitions/bool"
  }
 },
 "title": "Mocha JS Configuration File Schema",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// coffeelint.json (10655 bytes)
object CoffeelintSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/coffeelint.json",
 "additionalProperties": true,
 "definitions": {
  "base": {
   "type": "object",
   "properties": {
    "level": {
     "description": "Determines the error level",
     "type": "string",
     "enum": [
      "error",
      "warn",
      "ignore"
     ]
    }
   }
  }
 },
 "properties": {
  "arrow_spacing": {
   "$$ref": "#/definitions/base",
   "description": "This rule checks to see that there is spacing before and after the arrow operator that declares a function. [default level: ignore]",
   "type": "object"
  },
  "braces_spacing": {
   "$$ref": "#/definitions/base",
   "description": "This rule checks to see that there is the proper spacing inside curly braces. The spacing amount is specified by `spaces`. The spacing amount for empty objects is specified by `empty_object_spaces`. [default level: ignore]",
   "type": "object",
   "properties": {
    "empty_object_spaces": {
     "type": "integer",
     "enum": [
      0,
      1
     ]
    },
    "spaces": {
     "type": "integer",
     "enum": [
      0,
      1
     ]
    }
   }
  },
  "camel_case_classes": {
   "$$ref": "#/definitions/base",
   "description": "This rule mandates that all class names are CamelCased. Camel casing class names is a generally accepted way of distinguishing constructor functions - which require the `new` prefix to behave properly - from plain old functions. [default level: error]",
   "type": "object"
  },
  "coffeescript_error": {
   "$$ref": "#/definitions/base",
   "description": "[default level: error]",
   "type": "object"
  },
  "colon_assignment_spacing": {
   "$$ref": "#/definitions/base",
   "description": "This rule checks to see that there is spacing before and after the colon in a colon assignment (i.e., classes, objects). [default level: ignore]",
   "type": "object",
   "properties": {
    "spacing": {
     "type": "object",
     "properties": {
      "left": {
       "type": "integer",
       "enum": [
        0,
        1
       ]
      },
      "right": {
       "type": "integer",
       "enum": [
        0,
        1
       ]
      }
     }
    }
   }
  },
  "cyclomatic_complexity": {
   "$$ref": "#/definitions/base",
   "description": "Examine the complexity of your application. [default level: ignore]",
   "type": "object",
   "properties": {
    "value": {
     "type": "integer"
    }
   }
  },
  "duplicate_key": {
   "$$ref": "#/definitions/base",
   "description": "Prevents defining duplicate keys in object literals and classes. [default level: error]",
   "type": "object"
  },
  "empty_constructor_needs_parens": {
   "$$ref": "#/definitions/base",
   "description": "Requires constructors with no parameters to include the parens. [default level: ignore]",
   "type": "object"
  },
  "ensure_comprehensions": {
   "$$ref": "#/definitions/base",
   "description": "This rule makes sure that parentheses are around comprehensions. [default level: warn]",
   "type": "object"
  },
  "eol_last": {
   "$$ref": "#/definitions/base",
   "description": "Checks that the file ends with a single newline. [default level: ignore]",
   "type": "object"
  },
  "indentation": {
   "$$ref": "#/definitions/base",
   "description": "This rule imposes a standard number of spaces to be used for indentation. Since whitespace is significant in CoffeeScript, it's critical that a project chooses a standard indentation format and stays consistent. Other roads lead to darkness. [default level: error]",
   "type": "object",
   "properties": {
    "value": {
     "type": "integer"
    }
   }
  },
  "line_endings": {
   "$$ref": "#/definitions/base",
   "description": "This rule ensures your project uses only windows or unix line endings. [default level: ignore]",
   "type": "object",
   "properties": {
    "value": {
     "type": "string",
     "enum": [
      "unix",
      "windows"
     ]
    }
   }
  },
  "max_line_length": {
   "$$ref": "#/definitions/base",
   "description": "This rule imposes a maximum line length on your code. [default level: error]",
   "type": "object",
   "properties": {
    "value": {
     "type": "integer"
    },
    "limitComments": {
     "type": "boolean"
    }
   }
  },
  "missing_fat_arrows": {
   "$$ref": "#/definitions/base",
   "description": "Warns when you use `this` inside a function that wasn't defined with a fat arrow. This rule does not apply to methods defined in a class, since they have `this` bound to the class instance (or the class itself, for class methods). [default level: ignore]",
   "type": "object"
  },
  "newlines_after_classes": {
   "$$ref": "#/definitions/base",
   "description": "Checks the number of newlines between classes and other code. [default level: ignore]",
   "type": "object",
   "properties": {
    "value": {
     "type": "integer"
    }
   }
  },
  "no_backticks": {
   "$$ref": "#/definitions/base",
   "description": "Backticks allow snippets of JavaScript to be embedded in CoffeeScript. While some folks consider backticks useful in a few niche circumstances, they should be avoided because so none of JavaScript's 'bad parts', like with and eval, sneak into CoffeeScript. [default level: error]",
   "type": "object"
  },
  "no_debugger": {
   "$$ref": "#/definitions/base",
   "description": "This rule detects the `debugger` statement. [default level: warn]",
   "type": "object"
  },
  "no_empty_functions": {
   "$$ref": "#/definitions/base",
   "description": "Disallows declaring empty functions. The goal of this rule is that unintentional empty callbacks can be detected. [default level: ignore]",
   "type": "object"
  },
  "no_empty_param_list": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits empty parameter lists in function definitions. [default level: ignore]",
   "type": "object"
  },
  "no_implicit_braces": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits implicit braces when declaring object literals. Implicit braces can make code more difficult to understand, especially when used in combination with optional parenthesis. [default level: ignore]",
   "type": "object",
   "properties": {
    "strict": {
     "type": "boolean"
    }
   }
  },
  "no_implicit_parens": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits implicit parens on function calls. [default level: ignore]",
   "type": "object"
  },
  "no_interpolation_in_single_quotes": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits string interpolation in a single quoted string. [default level: ignore]",
   "type": "object"
  },
  "no_nested_string_interpolation": {
   "$$ref": "#/definitions/base",
   "description": "This rule warns about nested string interpolation, as it tends to make code harder to read and understand. [default level: warn]",
   "type": "object"
  },
  "no_plusplus": {
   "$$ref": "#/definitions/base",
   "description": "This rule forbids the increment and decrement arithmetic operators. Some people believe the `++` and `--` to be cryptic and the cause of bugs due to misunderstandings of their precedence rules. [default level: ignore]",
   "type": "object"
  },
  "no_private_function_fat_arrows": {
   "$$ref": "#/definitions/base",
   "description": "Warns when you use the fat arrow for a private function inside a class definition scope. It is not necessary and it does not do anything. [default level: warn]",
   "type": "object"
  },
  "no_stand_alone_at": {
   "$$ref": "#/definitions/base",
   "description": "This rule checks that no stand alone `@` are in use, they are discouraged. [default level: ignore]",
   "type": "object"
  },
  "no_tabs": {
   "$$ref": "#/definitions/base",
   "description": "This rule forbids tabs in indentation. Enough said. [default level: error]",
   "type": "object"
  },
  "no_this": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits `this`. Use `@` instead. [default level: ignore]",
   "type": "object"
  },
  "no_throwing_strings": {
   "$$ref": "#/definitions/base",
   "description": "This rule forbids throwing string literals or interpolations. While JavaScript (and CoffeeScript by extension) allow any expression to be thrown, it is best to only throw `Error` objects, because they contain valuable debugging information like the stack trace. [default level: error]",
   "type": "object"
  },
  "no_trailing_semicolons": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits trailing semicolons, since they are needless cruft in CoffeeScript. [default level: error]",
   "type": "object"
  },
  "no_trailing_whitespace": {
   "$$ref": "#/definitions/base",
   "description": "This rule forbids trailing whitespace in your code, since it is needless cruft. [default level: error]",
   "type": "object",
   "properties": {
    "allowed_in_comments": {
     "type": "boolean"
    },
    "allowed_in_empty_lines": {
     "type": "boolean"
    }
   }
  },
  "no_unnecessary_double_quotes": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits double quotes unless string interpolation is used or the string contains single quotes. [default level: ignore]",
   "type": "object"
  },
  "no_unnecessary_fat_arrows": {
   "$$ref": "#/definitions/base",
   "description": "Disallows defining functions with fat arrows when `this` is not used within the function.  [default level: warn]",
   "type": "object"
  },
  "non_empty_constructor_needs_parens": {
   "$$ref": "#/definitions/base",
   "description": "Requires constructors with parameters to include the parens. [default level: ignore]",
   "type": "object"
  },
  "prefer_english_operator": {
   "$$ref": "#/definitions/base",
   "description": "This rule prohibits `&&`, `||`, `==`, `!=` and `!`. Use `and`, `or`, `is`, `isnt`, and `not` instead. `!!` (for converting to a boolean) is ignored. [default level: ignore]",
   "type": "object"
  },
  "space_operators": {
   "$$ref": "#/definitions/base",
   "description": "This rule enforces that operators have space around them.  [default level: ignore]",
   "type": "object"
  },
  "spacing_after_comma": {
   "$$ref": "#/definitions/base",
   "description": "This rule checks to make sure you have a space after commas. [default level: ignore]",
   "type": "object"
  },
  "transform_messes_up_line_numbers": {
   "$$ref": "#/definitions/base",
   "description": "This rule detects when changes are made by transform function, and warns that line numbers are probably incorrect. [default level: warn]",
   "type": "object"
  }
 },
 "title": "JSON schema for coffeelint.json files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// codecov.json (13955 bytes)
object CodecovSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/codecov",
 "definitions": {
  "default": {
   "$$comment": "See https://docs.codecov.com/docs/commit-status#basic-configuration",
   "type": "object",
   "properties": {
    "target": {
     "anyOf": [
      {
       "type": "string",
       "pattern": "^(([0-9]+\\\\.?[0-9]*|\\\\.[0-9]+)%?|auto)$$"
      },
      {
       "type": "number"
      }
     ],
     "default": "auto"
    },
    "threshold": {
     "type": "string",
     "default": "0%",
     "pattern": "^([0-9]+\\\\.?[0-9]*|\\\\.[0-9]+)%?$$"
    },
    "base": {
     "type": "string",
     "default": "auto",
     "deprecated": true
    },
    "flags": {
     "type": "array",
     "default": []
    },
    "paths": {
     "anyOf": [
      {
       "type": "array"
      },
      {
       "type": "string"
      }
     ],
     "default": []
    },
    "branches": {
     "type": "array",
     "default": []
    },
    "if_not_found": {
     "type": "string",
     "enum": [
      "failure",
      "success"
     ],
     "default": "success"
    },
    "informational": {
     "type": "boolean",
     "default": false
    },
    "only_pulls": {
     "type": "boolean",
     "default": false
    },
    "if_ci_failed": {
     "type": "string",
     "enum": [
      "error",
      "success"
     ]
    },
    "flag_coverage_not_uploaded_behavior": {
     "type": "string",
     "enum": [
      "include",
      "exclude",
      "pass"
     ]
    }
   }
  },
  "flag": {
   "type": "object",
   "properties": {
    "joined": {
     "type": "boolean"
    },
    "required": {
     "type": "boolean"
    },
    "ignore": {
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "paths": {
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "assume": {
     "anyOf": [
      {
       "type": "boolean"
      },
      {
       "type": "array",
       "items": {
        "type": "string"
       }
      }
     ]
    }
   }
  },
  "layout": {
   "anyOf": [
    {},
    {
     "enum": [
      "header",
      "footer",
      "diff",
      "file",
      "files",
      "flag",
      "flags",
      "reach",
      "sunburst",
      "uncovered"
     ]
    }
   ]
  },
  "notification": {
   "type": "object",
   "properties": {
    "url": {
     "type": "string"
    },
    "branches": {
     "type": "string"
    },
    "threshold": {
     "type": "string"
    },
    "message": {
     "type": "string"
    },
    "flags": {
     "type": "string"
    },
    "base": {
     "enum": [
      "parent",
      "pr",
      "auto"
     ]
    },
    "only_pulls": {
     "type": "boolean"
    },
    "paths": {
     "type": "array",
     "items": {
      "type": "string"
     }
    }
   }
  }
 },
 "description": "Schema for codecov.yml files.",
 "properties": {
  "codecov": {
   "description": "See https://docs.codecov.io/docs/codecov-yaml for details",
   "type": "object",
   "properties": {
    "url": {
     "type": "string"
    },
    "slug": {
     "type": "string"
    },
    "bot": {
     "description": "Team bot. See https://docs.codecov.io/docs/team-bot for details",
     "type": "string"
    },
    "branch": {
     "type": "string"
    },
    "ci": {
     "description": "Detecting CI services. See https://docs.codecov.io/docs/detecting-ci-services for details.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "assume_all_flags": {
     "type": "boolean"
    },
    "strict_yaml_branch": {
     "type": "string"
    },
    "max_report_age": {
     "anyOf": [
      {
       "type": "string"
      },
      {
       "type": "integer"
      },
      {
       "type": "boolean"
      }
     ]
    },
    "disable_default_path_fixes": {
     "type": "boolean"
    },
    "require_ci_to_pass": {
     "type": "boolean"
    },
    "allow_pseudo_compare": {
     "type": "boolean"
    },
    "archive": {
     "type": "object",
     "properties": {
      "uploads": {
       "type": "boolean"
      }
     }
    },
    "notify": {
     "type": "object",
     "properties": {
      "after_n_builds": {
       "type": "integer"
      },
      "countdown": {
       "type": "integer"
      },
      "delay": {
       "type": "integer"
      },
      "wait_for_ci": {
       "type": "boolean"
      }
     }
    },
    "ui": {
     "type": "object",
     "properties": {
      "hide_density": {
       "anyOf": [
        {
         "type": "boolean"
        },
        {
         "type": "array",
         "items": {
          "type": "string"
         }
        }
       ]
      },
      "hide_complexity": {
       "anyOf": [
        {
         "type": "boolean"
        },
        {
         "type": "array",
         "items": {
          "type": "string"
         }
        }
       ]
      },
      "hide_contextual": {
       "type": "boolean"
      },
      "hide_sunburst": {
       "type": "boolean"
      },
      "hide_search": {
       "type": "boolean"
      }
     }
    }
   }
  },
  "coverage": {
   "description": "Coverage configuration. See https://docs.codecov.io/docs/coverage-configuration for details.",
   "type": "object",
   "properties": {
    "precision": {
     "type": "integer",
     "minimum": 0,
     "maximum": 5
    },
    "round": {
     "enum": [
      "down",
      "up",
      "nearest"
     ]
    },
    "range": {
     "type": "string"
    },
    "notify": {
     "description": "Notifications. See https://docs.codecov.io/docs/notifications for details.",
     "type": "object",
     "properties": {
      "irc": {
       "type": "object",
       "properties": {
        "url": {
         "type": "string"
        },
        "branches": {
         "type": "string"
        },
        "threshold": {
         "type": "string"
        },
        "message": {
         "type": "string"
        },
        "flags": {
         "type": "string"
        },
        "base": {
         "enum": [
          "parent",
          "pr",
          "auto"
         ]
        },
        "only_pulls": {
         "type": "boolean"
        },
        "paths": {
         "type": "array",
         "items": {
          "type": "string"
         }
        },
        "channel": {
         "type": "string"
        },
        "password": {
         "type": "string"
        },
        "nickserv_password": {
         "type": "string"
        },
        "notice": {
         "type": "boolean"
        }
       }
      },
      "slack": {
       "description": "Slack. See https://docs.codecov.io/docs/notifications#section-slack for details.",
       "type": "object",
       "properties": {
        "url": {
         "type": "string"
        },
        "branches": {
         "type": "string"
        },
        "threshold": {
         "type": "string"
        },
        "message": {
         "type": "string"
        },
        "flags": {
         "type": "string"
        },
        "base": {
         "enum": [
          "parent",
          "pr",
          "auto"
         ]
        },
        "only_pulls": {
         "type": "boolean"
        },
        "paths": {
         "type": "array",
         "items": {
          "type": "string"
         }
        },
        "attachments": {
         "$$ref": "#/definitions/layout"
        }
       }
      },
      "gitter": {
       "description": "Gitter. See https://docs.codecov.io/docs/notifications#section-gitter for details.",
       "type": "object",
       "properties": {
        "url": {
         "type": "string"
        },
        "branches": {
         "type": "string"
        },
        "threshold": {
         "type": "string"
        },
        "message": {
         "type": "string"
        },
        "flags": {
         "type": "string"
        },
        "base": {
         "enum": [
          "parent",
          "pr",
          "auto"
         ]
        },
        "only_pulls": {
         "type": "boolean"
        },
        "paths": {
         "type": "array",
         "items": {
          "type": "string"
         }
        }
       }
      },
      "hipchat": {
       "description": "Hipchat. See https://docs.codecov.io/docs/notifications#section-hipchat for details.",
       "type": "object",
       "properties": {
        "url": {
         "type": "string"
        },
        "branches": {
         "type": "string"
        },
        "threshold": {
         "type": "string"
        },
        "message": {
         "type": "string"
        },
        "flags": {
         "type": "string"
        },
        "base": {
         "enum": [
          "parent",
          "pr",
          "auto"
         ]
        },
        "only_pulls": {
         "type": "boolean"
        },
        "paths": {
         "type": "array",
         "items": {
          "type": "string"
         }
        },
        "card": {
         "type": "boolean"
        },
        "notify": {
         "type": "boolean"
        }
       }
      },
      "webhook": {
       "type": "object",
       "properties": {
        "url": {
         "type": "string"
        },
        "branches": {
         "type": "string"
        },
        "threshold": {
         "type": "string"
        },
        "message": {
         "type": "string"
        },
        "flags": {
         "type": "string"
        },
        "base": {
         "enum": [
          "parent",
          "pr",
          "auto"
         ]
        },
        "only_pulls": {
         "type": "boolean"
        },
        "paths": {
         "type": "array",
         "items": {
          "type": "string"
         }
        }
       }
      },
      "email": {
       "type": "object",
       "properties": {
        "url": {
         "type": "string"
        },
        "branches": {
         "type": "string"
        },
        "threshold": {
         "type": "string"
        },
        "message": {
         "type": "string"
        },
        "flags": {
         "type": "string"
        },
        "base": {
         "enum": [
          "parent",
          "pr",
          "auto"
         ]
        },
        "only_pulls": {
         "type": "boolean"
        },
        "paths": {
         "type": "array",
         "items": {
          "type": "string"
         }
        },
        "layout": {
         "$$ref": "#/definitions/layout"
        },
        "+to": {
         "type": "array",
         "items": {
          "type": "string"
         }
        }
       }
      }
     }
    },
    "status": {
     "description": "Commit status. See https://docs.codecov.io/docs/commit-status for details.",
     "anyOf": [
      {
       "type": "boolean"
      },
      {
       "type": "object",
       "additionalProperties": false,
       "properties": {
        "default_rules": {
         "type": "object"
        },
        "project": {
         "type": "object",
         "properties": {
          "default": {
           "anyOf": [
            {
             "type": "boolean"
            },
            {
             "$$ref": "#/definitions/default",
             "type": "object"
            }
           ]
          }
         },
         "additionalProperties": {
          "anyOf": [
           {
            "type": "boolean"
           },
           {
            "$$ref": "#/definitions/default",
            "type": "object"
           }
          ]
         }
        },
        "patch": {
         "anyOf": [
          {
           "$$ref": "#/definitions/default",
           "type": "object"
          },
          {
           "type": "string",
           "enum": [
            "off"
           ]
          },
          {
           "type": "boolean"
          }
         ]
        },
        "changes": {
         "$$ref": "#/definitions/default",
         "anyOf": [
          {
           "type": "boolean"
          },
          {
           "type": "object"
          }
         ]
        }
       }
      }
     ]
    }
   }
  },
  "ignore": {
   "description": "Ignoring paths. see https://docs.codecov.io/docs/ignoring-paths for details.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "fixes": {
   "description": "Fixing paths. See https://docs.codecov.io/docs/fixing-paths for details.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "flags": {
   "description": "Flags. See https://docs.codecov.io/docs/flags for details.",
   "oneOf": [
    {
     "type": "array",
     "items": {
      "$$ref": "#/definitions/flag"
     }
    },
    {
     "type": "object",
     "additionalProperties": {
      "$$ref": "#/definitions/flag"
     }
    }
   ]
  },
  "comment": {
   "description": "Pull request comments. See https://docs.codecov.io/docs/pull-request-comments for details.",
   "oneOf": [
    {
     "type": "object",
     "properties": {
      "layout": {
       "$$ref": "#/definitions/layout"
      },
      "require_changes": {
       "type": "boolean"
      },
      "require_base": {
       "type": "boolean"
      },
      "require_head": {
       "type": "boolean"
      },
      "branches": {
       "type": "array",
       "items": {
        "type": "string"
       }
      },
      "behavior": {
       "enum": [
        "default",
        "once",
        "new",
        "spammy"
       ]
      },
      "flags": {
       "type": "array",
       "items": {
        "$$ref": "#/definitions/flag"
       }
      },
      "paths": {
       "type": "array",
       "items": {
        "type": "string"
       }
      }
     }
    },
    {
     "const": false
    }
   ]
  },
  "github_checks": {
   "description": "GitHub Checks. See https://docs.codecov.com/docs/github-checks for details.",
   "anyOf": [
    {
     "type": "object",
     "properties": {
      "annotations": {
       "type": "boolean"
      }
     }
    },
    {
     "type": "boolean"
    },
    {
     "type": "string",
     "const": "off"
    }
   ]
  }
 },
 "title": "JSON schema for Codecov configuration files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// cloudbuild.json (29371 bytes)
object CloudBuildSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/cloudbuild",
 "$$comment": "See the Cloud Build config schema at https://cloud.google.com/build/docs/build-config-file-schema for the definitions.",
 "definitions": {
  "automapSubstitutions": {
   "description": "If set to true, automatically map all subsistutions and make them available as environment variables in a single step. If set to false, ignore substitutions for that step. Can be used for a build step or for an entire build.",
   "markdownDescription": "If set to true, automatically map all subsistutions  and make them available as environment variables in a single step. If set to false, ignore substitutions for that step. Can be used for a build step or for an entire build.",
   "type": "boolean",
   "examples": [
    true,
    false
   ]
  },
  "BuildStep": {
   "description": "A step in the build pipeline.",
   "type": "object",
   "additionalProperties": false,
   "properties": {
    "name": {
     "description": "Required. The name of the container image that will run this particular build step. If the image is available in the host's Docker daemon's cache, it will be run directly. If not, the host will attempt to pull the image first, using the builder service account's credentials if necessary.\\n\\nThe Docker daemon's cache will already have the latest versions of all of the officially supported build steps. The Docker daemon will also have cached many of the layers for some popular images, like \\"ubuntu\\", \\"debian\\", but they will be refreshed at the time you attempt to use them.\\n\\nIf you built an image in a previous build step, it will be stored in the host's Docker daemon's cache and is available to use as the name for a later build step.",
     "markdownDescription": "Required. The name of the container image that will run this particular build step. If the image is available in the host's Docker daemon's cache, it will be run directly. If not, the host will attempt to pull the image first, using the builder service account's credentials if necessary.\\n\\nThe Docker daemon's cache will already have the latest versions of all of the [officially supported build steps](https://github.com/GoogleCloudPlatform/cloud-builders). The Docker daemon will also have cached many of the layers for some popular images, like `ubuntu`, `debian`, but they will be refreshed at the time you attempt to use them.\\n\\nIf you built an image in a previous build step, it will be stored in the host's Docker daemon's cache and is available to use as the name for a later build step.",
     "type": "string"
    },
    "allowFailure": {
     "description": "In a build step, if you set the value of the allowFailure field to true, and the build step fails, then the build succeeds as long as all other build steps in that build succeed.",
     "markdownDescription": "In a build step, if you set the value of the `allowFailure` field to `true`, and the build step fails, then the build succeeds as long as all other build steps in that build succeed.",
     "type": "boolean"
    },
    "allowExitCodes": {
     "description": "Specify that a build step failure can be ignored when that step returns a particular exit code.",
     "type": "array",
     "items": {
      "type": "integer"
     },
     "examples": [
      1,
      2
     ]
    },
    "automapSubstitutions": {
     "$$ref": "#/definitions/automapSubstitutions"
    },
    "waitFor": {
     "description": "The ID(s) of the step(s) that this build step depends on. This build step will not start until all the build steps in waitFor have completed successfully. If waitFor is empty, this build step will start when all previous build steps in the list have completed successfully. If waitFor is set to '-', the step runs immediately when the build starts.",
     "markdownDescription": "The `id`(s) of the step(s) that this build step depends on. This build step will not start until all the build steps in `waitFor` have completed successfully. If `waitFor` is empty, this build step will start when all previous build steps in the list have completed successfully. If `waitFor` is set to `'-'`, the step runs immediately when the build starts.",
     "type": "array",
     "items": {
      "type": "string",
      "examples": [
       [
        "-"
       ],
       [
        "terraform-init",
        "terraform-apply"
       ]
      ]
     }
    },
    "env": {
     "description": "A list of environment variable definitions to be used when running a step. The elements are of the form \\"KEY=VALUE\\" for the environment variable \\"KEY\\" being given the value \\"VALUE\\".",
     "markdownDescription": "A list of environment variable definitions to be used when running a step. The elements are of the form `KEY=VALUE` for the environment variable `KEY` being given the value `VALUE`.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "entrypoint": {
     "description": "Entrypoint to be used instead of the build step image's default entrypoint. If unset, the image's default entrypoint is used.",
     "markdownDescription": "Entrypoint to be used instead of the build step image's default `entrypoint`. If unset, the image's default `entrypoint` is used.",
     "type": "string"
    },
    "script": {
     "description": "Specify a shell script to execute in the step. If you specify script in a build step, you cannot specify args or entrypoint in the same step.",
     "markdownDescription": "Specify a shell script to execute in the step. If you specify script in a build step, you cannot specify `args` or `entrypoint` in the same step.",
     "type": "string"
    },
    "volumes": {
     "description": "List of volumes to mount into the build step. Each volume is created as an empty volume prior to execution of the build step. Upon completion of the build, volumes and their contents are discarded. Using a named volume in only one step is not valid as it is indicative of a build request with an incorrect configuration.",
     "type": "array",
     "items": {
      "$$ref": "#/definitions/Volume"
     }
    },
    "args": {
     "description": "A list of arguments that will be presented to the step when it is started.\\n\\nIf the image used to run the step's container has an entrypoint, the args are used as arguments to that entrypoint. If the image does not define an entrypoint, the first element in args is used as the entrypoint, and the remainder will be used as arguments.",
     "markdownDescription": "A list of arguments that will be presented to the step when it is started.\\n\\nIf the image used to run the step's container has an `entrypoint`, the `args` are used as arguments to that `entrypoint`. If the image does not define an `entrypoint`, the first element in `args` is used as the `entrypoint`, and the remainder will be used as arguments.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "timeout": {
     "$$ref": "#/definitions/Timeout",
     "description": "Time limit for executing this build step. If not defined, the step has no time limit and will be allowed to continue to run until either it completes or the build itself times out."
    },
    "id": {
     "description": "Unique identifier for this build step, used in waitFor to reference this build step as a dependency.",
     "markdownDescription": "Unique identifier for this build step, used in `waitFor` to reference this build step as a dependency.",
     "type": "string"
    },
    "secretEnv": {
     "description": "A list of environment variables which are encrypted using a Cloud Key Management Service crypto key. These values must be specified in the build's Secret.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "dir": {
     "description": "Working directory to use when running this step's container.\\n\\nIf this value is a relative path, it is relative to the build's working directory. If this value is absolute, it may be outside the build's working directory, in which case the contents of the path may not be persisted across build step executions, unless a volume for that path is specified. If the build specifies a RepoSource with dir and a step with a dir, which specifies an absolute path, the RepoSource dir is ignored for the step's execution.",
     "markdownDescription": "Working directory to use when running this step's container.\\n\\nIf this value is a relative path, it is relative to the build's working directory. If this value is absolute, it may be outside the build's working directory, in which case the contents of the path may not be persisted across build step executions, unless a volume for that path is specified. If the build specifies a `RepoSource` with `dir` and a step with a `dir`, which specifies an absolute path, the `RepoSource` `dir` is ignored for the step's execution.",
     "type": "string"
    }
   }
  },
  "BuildOptions": {
   "description": "Optional arguments to enable specific features of builds.",
   "type": "object",
   "additionalProperties": false,
   "properties": {
    "automapSubstitutions": {
     "$$ref": "#/definitions/automapSubstitutions"
    },
    "machineType": {
     "description": "Compute Engine machine type on which to run the build.",
     "type": "string",
     "enum": [
      "E2_HIGHCPU_8",
      "E2_HIGHCPU_32",
      "E2_MEDIUM",
      "N1_HIGHCPU_8",
      "N1_HIGHCPU_32",
      "UNSPECIFIED"
     ],
     "enumDescriptions": [
      "e2 HighCPU: 8 vCPUs, 8GB RAM",
      "e2 HighCPU: 32 vCPUs, 32GB RAM",
      "e2 Medium: 1 vCPU, 4GB RAM",
      "n1 HighCPU: 8 vCPUs, 7.2GB RAM",
      "n1 HighCPU: 32 vCPUs, 28.8GB RAM",
      "e2 Standard: 2 vCPU, 8GB RAM"
     ],
     "default": "UNSPECIFIED"
    },
    "volumes": {
     "description": "Global list of volumes to mount for ALL build steps. Each volume is created as an empty volume prior to starting the build process. Upon completion of the build, volumes and their contents are discarded. Global volume names and paths cannot conflict with the volumes defined a build step. Using a global volume in a build with only one step is not valid as it is indicative of a build request with an incorrect configuration.",
     "type": "array",
     "items": {
      "$$ref": "#/definitions/Volume"
     }
    },
    "logStreamingOption": {
     "description": "Option to define build log streaming behavior to Google Cloud Storage.",
     "type": "string",
     "enum": [
      "STREAM_DEFAULT",
      "STREAM_ON",
      "STREAM_OFF"
     ],
     "enumDescriptions": [
      "Service may automatically determine build log streaming behavior.",
      "Build logs should be streamed to Google Cloud Storage.",
      "Build logs should not be streamed to Google Cloud Storage; they will be written when the build is completed."
     ]
    },
    "pool": {
     "description": "Set the value of this field to the resource name of the private pool to run the build.",
     "type": "object",
     "properties": {
      "name": {
       "description": "Required. The full resource name of the private pool of the form 'projects/$$PRIVATEPOOL_PROJECT_ID/locations/$$REGION/workerPools/$$PRIVATEPOOL_ID'.",
       "markdownDescription": "Required. The full resource name of the private pool of the form `projects/$$PRIVATEPOOL_PROJECT_ID/locations/$$REGION/workerPools/$$PRIVATEPOOL_ID`.",
       "type": "string"
      }
     },
     "additionalProperties": false,
     "required": [
      "name"
     ]
    },
    "env": {
     "description": "A list of global environment variable definitions that will exist for all build steps in this build.\\n\\nIf a variable is defined both globally and in a build step, the variable will use the build step value. The elements are of the form \\"KEY=VALUE\\" for the environment variable \\"KEY\\" being given the value \\"VALUE\\".",
     "markdownDescription": "A list of global environment variable definitions that will exist for all build steps in this build.\\n\\nIf a variable is defined both globally and in a build step, the variable will use the build step value. The elements are of the form `KEY=VALUE` for the environment variable `KEY` being given the value `VALUE`.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "logging": {
     "description": "Option to specify the logging mode, which determines where the logs are stored.",
     "type": "string",
     "enum": [
      "LOGGING_UNSPECIFIED",
      "LEGACY",
      "GCS_ONLY",
      "CLOUD_LOGGING_ONLY",
      "NONE"
     ],
     "enumDescriptions": [
      "The service determines the logging mode. The default is LEGACY. Do not rely on the default logging behavior as it may change in the future.",
      "Stackdriver logging and Cloud Storage logging are enabled.",
      "Only Cloud Storage logging is enabled.",
      "Only Cloud Logging is enabled. Note that logs for both the Cloud Console UI and Cloud SDK are based on Cloud Storage logs, so neither will provide logs if this option is chosen.",
      "Turn off all logging. No build logs will be captured."
     ]
    },
    "defaultLogsBucketBehavior": {
     "description": "Configure Cloud Build to create a default logs bucket within your own project in the same region as your build.",
     "type": "string",
     "enum": [
      "DEFAULT_LOGS_BUCKET_BEHAVIOR_UNSPECIFIED",
      "REGIONAL_USER_OWNED_BUCKET"
     ],
     "enumDescriptions": [
      "Unspecified",
      "Configure Cloud Build to use regionalized, user-owned logs."
     ]
    },
    "requestedVerifyOption": {
     "description": "Requested verifiability options.",
     "type": "string",
     "enum": [
      "NOT_VERIFIED",
      "VERIFIED"
     ],
     "enumDescriptions": [
      "Not a verifiable build. (default)",
      "Verified build."
     ]
    },
    "substitutionOption": {
     "description": "Option to specify behavior when there is an error in the substitution checks.",
     "type": "string",
     "enum": [
      "MUST_MATCH",
      "ALLOW_LOOSE"
     ],
     "enumDescriptions": [
      "Fails the build if error in substitutions checks, like missing a substitution in the template or in the map.",
      "Do not fail the build if error in substitutions checks."
     ]
    },
    "dynamicSubstitutions": {
     "description": "Use this option to explicitly enable or disable bash parameter expansion in substitutions.\\n\\nIf your build is invoked by a trigger, the dynamicSubstitutions field is always set to true and does not need to be specified in your build config file. If your build is invoked manually, you must set the dynamicSubstitutions field to true for bash parameter expansions to be interpreted when running your build.",
     "markdownDescription": "Use this option to explicitly enable or disable bash parameter expansion in substitutions.\\n\\nIf your build is invoked by a trigger, the `dynamicSubstitutions` field is always set to `true` and does not need to be specified in your build config file. If your build is invoked manually, you must set the `dynamicSubstitutions` field to `true` for bash parameter expansions to be interpreted when running your build.",
     "type": "boolean"
    },
    "diskSizeGb": {
     "description": "Requested disk size for the VM that runs the build.\\n\\nNote that this is *NOT* \\"disk free\\"; some of the space will be used by the operating system and build utilities. Also note that this is the minimum disk size that will be allocated for the build -- the build may run with a larger disk than requested. At present, the maximum disk size is 2000GB; builds that request more than the maximum are rejected with an error.",
     "oneOf": [
      {
       "type": "integer",
       "minimum": 1,
       "maximum": 2000
      },
      {
       "type": "string",
       "pattern": "^(?:[1-9]\\\\d{0,2}|1\\\\d{3}|2000)$$"
      }
     ],
     "examples": [
      30,
      50,
      "100",
      200,
      "300"
     ]
    },
    "secretEnv": {
     "description": "A list of global environment variables, which are encrypted using a Cloud Key Management Service crypto key. These values must be specified in the build's Secret. These variables will be available to all build steps in this build.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "sourceProvenanceHash": {
     "description": "Requested hash for SourceProvenance.",
     "type": "array",
     "items": {
      "enum": [
       "NONE",
       "SHA256",
       "MD5"
      ],
      "type": "string"
     },
     "enumDescriptions": [
      "No hash requested.",
      "Use a sha256 hash.",
      "Use a md5 hash."
     ]
    }
   }
  },
  "Secret": {
   "description": "Pairs a set of secret environment variables containing encrypted values with the Cloud KMS key to use to decrypt the value.",
   "type": "object",
   "properties": {
    "kmsKeyName": {
     "type": "string",
     "description": "Cloud KMS key name to use to decrypt these envs."
    },
    "secretEnv": {
     "type": "object",
     "additionalProperties": {
      "type": "string"
     },
     "description": "Map of environment variable name to its encrypted value. Secret environment variables must be unique across all of a build's secrets, and must be used by at least one build step. Values can be at most 64 KB in size. There can be at most 100 secret values across all of a build's secrets."
    }
   }
  },
  "Artifacts": {
   "description": "Artifacts produced by a build that should be uploaded upon successful completion of all build steps.",
   "type": "object",
   "properties": {
    "objects": {
     "$$ref": "#/definitions/ArtifactObjects",
     "description": "A list of objects to be uploaded to Cloud Storage upon successful completion of all build steps. Files in the workspace matching specified paths globs will be uploaded to the specified Cloud Storage location using the builder service account's credentials. The location and generation of the uploaded objects will be stored in the Build resource's results field. If any objects fail to be pushed, the build is marked FAILURE.",
     "markdownDescription": "A list of objects to be uploaded to Cloud Storage upon successful completion of all build steps. Files in the workspace matching specified paths globs will be uploaded to the specified Cloud Storage location using the builder service account's credentials. The location and generation of the uploaded objects will be stored in the Build resource's results field. If any objects fail to be pushed, the build is marked `FAILURE`."
    },
    "goModules": {
     "description": "Allows you to upload non-container Go modules to Go repositories in Artifact Registry.",
     "type": "array",
     "items": {
      "$$ref": "#/definitions/GoModules"
     }
    },
    "mavenArtifacts": {
     "description": "Allows you to upload non-container Java artifacts to Maven repositories in Artifact Registry.",
     "type": "array",
     "items": {
      "$$ref": "#/definitions/MavenArtifacts"
     }
    },
    "pythonPackages": {
     "description": "Allows you to upload Python packages to Artifact Registry.",
     "type": "array",
     "items": {
      "$$ref": "#/definitions/PythonPackages"
     }
    },
    "npmPackages": {
     "description": "Uploads your built NPM packages to supported repositories.",
     "type": "array",
     "items": {
      "$$ref": "#/definitions/NpmPackages"
     }
    }
   }
  },
  "ArtifactObjects": {
   "description": "Files in the workspace to upload to Cloud Storage upon successful completion of all build steps.",
   "type": "object",
   "properties": {
    "location": {
     "description": "Cloud Storage bucket and optional object path, in the form \\"gs://bucket/path/to/somewhere/\\". Files in the workspace matching any path pattern will be uploaded to Cloud Storage with this location as a prefix.",
     "markdownDescription": "Cloud Storage bucket and optional object path, in the form `gs://bucket/path/to/somewhere/`. See the [Bucket Name Requirements](https://cloud.google.com/storage/docs/bucket-naming#requirements). Files in the workspace matching any path pattern will be uploaded to Cloud Storage with this location as a prefix.",
     "type": "string"
    },
    "paths": {
     "description": "Path globs used to match files in the build's workspace.",
     "type": "array",
     "items": {
      "type": "string"
     }
    }
   }
  },
  "GoModules": {
   "description": "Allows you to upload non-container Go modules to Go repositories in Artifact Registry.",
   "type": "object",
   "additionalProperties": false,
   "properties": {
    "repositoryName": {
     "description": "The name of your Go repository in Artifact Registry.",
     "type": "string"
    },
    "repositoryLocation": {
     "description": "The location for your repository in Artifact Registry.",
     "type": "string"
    },
    "repositoryProject_id": {
     "description": "The ID of the Google Cloud project that contains your Artifact Registry Go repository.",
     "type": "string"
    },
    "sourcePath": {
     "description": "The path to the go.mod file in the build's workspace.",
     "type": "string"
    },
    "modulePath": {
     "description": "The local directory that contains the Go module to upload. It is recommended to use an absolute path for the value.",
     "type": "string"
    },
    "moduleVersion": {
     "description": "The version of the Go module.",
     "type": "string"
    }
   },
   "required": [
    "repositoryName",
    "repositoryLocation",
    "repositoryProject_id",
    "sourcePath",
    "modulePath",
    "moduleVersion"
   ]
  },
  "MavenArtifacts": {
   "description": "Allows you to upload non-container Java artifacts to Maven repositories in Artifact Registry.",
   "type": "object",
   "additionalProperties": false,
   "properties": {
    "repository": {
     "description": "Required. Name of the Artifact Registry repository to store Java artifacts.",
     "type": "string"
    },
    "path": {
     "description": "Required. The application file path.",
     "type": "string"
    },
    "artifactId": {
     "description": "Required. Name of your package file created from your build step",
     "type": "string"
    },
    "groupId": {
     "description": "Required. Uniquely identifies your project across all Maven projects, in the format com.mycompany.app.",
     "markdownDescription": "Required. Uniquely identifies your project across all Maven projects, in the format `com.mycompany.app`.",
     "type": "string"
    },
    "version": {
     "description": "Required. The version number for your application.",
     "type": "string"
    }
   },
   "required": [
    "repository",
    "path",
    "artifactId",
    "groupId",
    "version"
   ]
  },
  "PythonPackages": {
   "description": "Allows you to upload Python packages to Artifact Registry.",
   "type": "object",
   "additionalProperties": false,
   "properties": {
    "repository": {
     "description": "Required. Name of the Artifact Registry repository to store the Python package.",
     "type": "string"
    },
    "paths": {
     "description": "Required. The package file paths.",
     "type": "array",
     "items": {
      "type": "string"
     }
    }
   },
   "required": [
    "repository",
    "paths"
   ]
  },
  "NpmPackages": {
   "description": "Uploads your built NPM packages to supported repositories.",
   "type": "object",
   "additionalProperties": false,
   "properties": {
    "repository": {
     "description": "Required. Name of the Artifact Registry repository to store the NPM package.",
     "type": "string"
    },
    "packagePath": {
     "description": "Required. The path for the local directory containing the NPM package that you want to upload to Artifact Registry. Google recommends using an absolute path. Your packagePath value can be . to use the current working directory, but the field cannot be omitted or left empty. This directory must contain a package.json file.",
     "markdownDescription": "Required. The path for the local directory containing the NPM package that you want to upload to Artifact Registry. Google recommends using an absolute path. Your `packagePath` value can be `.` to use the current working directory, but the field cannot be omitted or left empty. This directory must contain a `package.json` file.",
     "type": "string"
    }
   },
   "required": [
    "repository",
    "packagePath"
   ]
  },
  "Volume": {
   "description": "Volume describes a Docker container volume which is mounted into build steps in order to persist files across build step execution.",
   "type": "object",
   "properties": {
    "name": {
     "description": "Name of the volume to mount. Volume names must be unique per build step and must be valid names for Docker volumes. Each named volume must be used by at least two build steps.",
     "type": "string"
    },
    "path": {
     "description": "Path at which to mount the volume. Paths must be absolute and cannot conflict with other volume paths on the same build step or with certain reserved volume paths.",
     "type": "string"
    }
   }
  },
  "Timeout": {
   "description": "Time limit for executing the build or particular build step. The timeout field of a build step specifies the amount of time the step is allowed to run, and the timeout field of a build specifies the amount of time the build is allowed to run.",
   "markdownDescription": "Time limit for executing the build or particular build step. The `timeout` field of a build step specifies the amount of time the step is allowed to run, and the `timeout` field of a build specifies the amount of time the build is allowed to run.",
   "type": "string",
   "pattern": "^\\\\d+(\\\\.\\\\d{0,9})?s$$",
   "examples": [
    "3.5s",
    "120s"
   ]
  }
 },
 "description": "A build resource in the Cloud Build API.",
 "properties": {
  "steps": {
   "type": "array",
   "items": {
    "$$ref": "#/definitions/BuildStep"
   },
   "description": "Required. The operations to be performed on the workspace."
  },
  "logsBucket": {
   "description": "Google Cloud Storage bucket where logs should be written. Logs file names will be of the format $${logs_bucket}/log-$${build_id}.txt.",
   "markdownDescription": "Google Cloud Storage bucket where logs should be written. See [Bucket Name Requirements](https://cloud.google.com/storage/docs/bucket-naming#requirements). Logs file names will be of the format `$${logs_bucket}/log-$${build_id}.txt`.",
   "type": "string"
  },
  "tags": {
   "type": "array",
   "items": {
    "type": "string"
   },
   "description": "Tags for organizing and filtering builds."
  },
  "substitutions": {
   "additionalProperties": {
    "type": "string"
   },
   "description": "Substitutions data for Build resource.",
   "type": "object"
  },
  "images": {
   "description": "A list of images to be pushed upon the successful completion of all build steps. The images are pushed using the builder service account's credentials. The digests of the pushed images will be stored in the Build resource's results field. If any of the images fail to be pushed, the build status is marked FAILURE.",
   "markdownDescription": "A list of images to be pushed upon the successful completion of all build steps. The images are pushed using the builder service account's credentials. The digests of the pushed images will be stored in the Build resource's results field. If any of the images fail to be pushed, the build status is marked `FAILURE`.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "options": {
   "$$ref": "#/definitions/BuildOptions",
   "description": "Special options for this build."
  },
  "artifacts": {
   "$$ref": "#/definitions/Artifacts",
   "description": "Artifacts produced by the build that should be uploaded upon successful completion of all build steps."
  },
  "timeout": {
   "$$ref": "#/definitions/Timeout",
   "description": "Amount of time that this build should be allowed to run, to second granularity. If this amount of time elapses, work on the build will cease and the build status will be TIMEOUT.",
   "markdownDescription": "Amount of time that this build should be allowed to run, to second granularity. If this amount of time elapses, work on the build will cease and the build status will be `TIMEOUT`.",
   "default": "600s"
  },
  "secrets": {
   "description": "Secrets to decrypt using Cloud Key Management Service.",
   "type": "array",
   "items": {
    "$$ref": "#/definitions/Secret"
   }
  },
  "serviceAccount": {
   "description": "Use this field to specify the IAM service account to use at build time.",
   "type": "string"
  },
  "queueTtl": {
   "description": "Specifies the amount of time a build can be queued. If a build is in the queue for longer than the value set in queueTtl, the build expires and the build status is set to EXPIRED.",
   "markdownDescription": "Specifies the amount of time a build can be queued. If a build is in the queue for longer than the value set in `queueTtl`, the build expires and the build status is set to `EXPIRED`.",
   "type": "string",
   "pattern": "^\\\\d+(\\\\.\\\\d{0,9})?s$$",
   "examples": [
    "3.5s",
    "120s"
   ],
   "default": "3600s"
  }
 },
 "required": [
  "steps"
 ],
 "title": "Google Cloud Build build config file",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// staticwebapp.config.json (18426 bytes)
object StaticWebAppSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/staticwebapp.config.json",
 "additionalProperties": false,
 "default": {
  "navigationFallback": {
   "rewrite": "/index.html"
  }
 },
 "definitions": {
  "route": {
   "type": "object",
   "required": [
    "route"
   ],
   "properties": {
    "route": {
     "type": "string",
     "description": "Request route pattern to match. May contain valid wildcards. See documentation: https://aka.ms/swa/config-schema"
    },
    "methods": {
     "type": "array",
     "description": "Request method(s) to match",
     "items": {
      "anyOf": [
       {
        "type": "string",
        "enum": [
         "GET",
         "HEAD",
         "POST",
         "PUT",
         "DELETE",
         "PATCH",
         "CONNECT",
         "OPTIONS",
         "TRACE"
        ]
       }
      ]
     }
    },
    "allowedRoles": {
     "type": "array",
     "description": "Roles that are allowed to access this route. If not empty, only role(s) listed are authorized to access the route. Roles are only used for authorization; they are not used to evaluate whether the route matches the request.",
     "items": {
      "anyOf": [
       {
        "type": "string",
        "examples": [
         "anonymous",
         "authenticated"
        ]
       }
      ]
     }
    },
    "headers": {
     "type": "object",
     "description": "Override any matching global headers",
     "additionalProperties": true
    },
    "redirect": {
     "type": "string",
     "description": "Redirect to a relative or absolute path, or an external URI. Default status code is 302, override with 301."
    },
    "statusCode": {
     "type": "integer",
     "description": "Status code override"
    },
    "rewrite": {
     "type": "string",
     "description": "A path to rewrite the request route to"
    }
   },
   "additionalProperties": false
  },
  "auth": {
   "type": "object",
   "required": [
    "identityProviders"
   ],
   "properties": {
    "rolesSource": {
     "type": "string",
     "description": "Route to API function for assigning roles. For example, \\"/api/GetRoles\\". See https://aka.ms/swa-roles-function"
    },
    "identityProviders": {
     "type": "object",
     "properties": {
      "azureActiveDirectory": {
       "type": "object",
       "required": [
        "registration"
       ],
       "properties": {
        "enabled": {
         "description": "<false> if the azureActiveDirectory provider is not enabled, <true> otherwise",
         "type": "boolean",
         "default": true
        },
        "registration": {
         "type": "object",
         "required": [
          "openIdIssuer",
          "clientSecretSettingName"
         ],
         "properties": {
          "openIdIssuer": {
           "type": "string",
           "description": "The endpoint for the OpenID configuration of the AAD tenant"
          },
          "clientIdSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Application (client) ID for the Azure AD app registration"
          },
          "clientSecretSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the client secret for the Azure AD app registration"
          },
          "clientSecretCertificateKeyVaultReference": {
           "type": "string",
           "description": "A Key Vault reference for the certificate used for certificate-based authentication"
          },
          "clientSecretCertificateThumbprint": {
           "type": "string",
           "description": "The thumbprint of the certificate used for certificate-based authentication"
          }
         },
         "additionalProperties": false
        },
        "login": {
         "type": "object",
         "description": "",
         "properties": {
          "loginParameters": {
           "type": "array",
           "items": {
            "type": "string"
           }
          }
         },
         "additionalProperties": false
        },
        "userDetailsClaim": {
         "type": "string",
         "description": "The name of the claim from which we should read user details"
        }
       },
       "additionalProperties": false
      },
      "apple": {
       "type": "object",
       "required": [
        "registration"
       ],
       "properties": {
        "enabled": {
         "description": "<false> if the apple provider is not enabled, <true> otherwise",
         "type": "boolean",
         "default": true
        },
        "registration": {
         "type": "object",
         "required": [
          "clientSecretSettingName"
         ],
         "properties": {
          "clientIdSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Client ID"
          },
          "clientSecretSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Client Secret"
          }
         },
         "additionalProperties": false
        },
        "login": {
         "type": "object",
         "description": "",
         "properties": {
          "scopes": {
           "type": "array",
           "items": {
            "type": "string"
           }
          }
         },
         "additionalProperties": false
        },
        "userDetailsClaim": {
         "type": "string",
         "description": "The name of the claim from which we should read user details"
        }
       },
       "additionalProperties": false
      },
      "facebook": {
       "type": "object",
       "required": [
        "registration"
       ],
       "properties": {
        "enabled": {
         "description": "<false> if the facebook provider is not enabled, <true> otherwise",
         "type": "boolean",
         "default": true
        },
        "registration": {
         "type": "object",
         "required": [
          "appSecretSettingName"
         ],
         "properties": {
          "appIdSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the App ID"
          },
          "appSecretSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the App Secret"
          }
         },
         "additionalProperties": false
        },
        "login": {
         "type": "object",
         "description": "",
         "properties": {
          "scopes": {
           "type": "array",
           "items": {
            "type": "string"
           }
          }
         },
         "additionalProperties": false
        },
        "userDetailsClaim": {
         "type": "string",
         "description": "The name of the claim from which we should read user details"
        }
       },
       "additionalProperties": false
      },
      "github": {
       "type": "object",
       "required": [
        "registration"
       ],
       "properties": {
        "enabled": {
         "description": "<false> if the gitHub provider is not enabled, <true> otherwise",
         "type": "boolean",
         "default": true
        },
        "registration": {
         "type": "object",
         "required": [
          "clientSecretSettingName"
         ],
         "properties": {
          "clientIdSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Client ID"
          },
          "clientSecretSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Client Secret"
          }
         },
         "additionalProperties": false
        },
        "login": {
         "type": "object",
         "description": "",
         "properties": {
          "scopes": {
           "type": "array",
           "items": {
            "type": "string"
           }
          }
         },
         "additionalProperties": false
        },
        "userDetailsClaim": {
         "type": "string",
         "description": "The name of the claim from which we should read user details"
        }
       },
       "additionalProperties": false
      },
      "google": {
       "type": "object",
       "required": [
        "registration"
       ],
       "properties": {
        "enabled": {
         "description": "<false> if the google provider is not enabled, <true> otherwise",
         "type": "boolean",
         "default": true
        },
        "registration": {
         "type": "object",
         "required": [
          "clientSecretSettingName"
         ],
         "properties": {
          "clientIdSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Client ID"
          },
          "clientSecretSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Client Secret"
          }
         },
         "additionalProperties": false
        },
        "login": {
         "type": "object",
         "description": "",
         "properties": {
          "scopes": {
           "type": "array",
           "items": {
            "type": "string"
           }
          }
         },
         "additionalProperties": false
        },
        "userDetailsClaim": {
         "type": "string",
         "description": "The name of the claim from which we should read user details"
        }
       },
       "additionalProperties": false
      },
      "twitter": {
       "type": "object",
       "required": [
        "registration"
       ],
       "properties": {
        "enabled": {
         "description": "<false> if the twitter provider is not enabled, <true> otherwise",
         "type": "boolean",
         "default": true
        },
        "registration": {
         "type": "object",
         "required": [
          "consumerSecretSettingName"
         ],
         "properties": {
          "consumerKeySettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Consumer Key"
          },
          "consumerSecretSettingName": {
           "type": "string",
           "description": "The name of the application setting containing the Consumer Secret"
          }
         },
         "additionalProperties": false
        },
        "userDetailsClaim": {
         "type": "string",
         "description": "The name of the claim from which we should read user details"
        }
       },
       "additionalProperties": false
      },
      "customOpenIdConnectProviders": {
       "type": "object",
       "patternProperties": {
        ".*": {
         "type": "object",
         "required": [
          "registration",
          "login"
         ],
         "properties": {
          "enabled": {
           "description": "<false> if the custom OpenID Connect provider is not enabled, <true> otherwise",
           "type": "boolean",
           "default": true
          },
          "registration": {
           "type": "object",
           "required": [
            "clientCredential",
            "openIdConnectConfiguration"
           ],
           "properties": {
            "clientIdSettingName": {
             "type": "string",
             "description": "The name of the application setting containing the Client ID"
            },
            "clientCredential": {
             "type": "object",
             "required": [
              "clientSecretSettingName"
             ],
             "properties": {
              "clientSecretSettingName": {
               "type": "string",
               "description": "The name of the application setting containing the Client Secret"
              }
             }
            },
            "openIdConnectConfiguration": {
             "type": "object",
             "properties": {
              "authorizationEndpoint": {
               "type": "string",
               "description": "The path to the authorization endpoint"
              },
              "tokenEndpoint": {
               "type": "string",
               "description": "The path to the token endpoint"
              },
              "issuer": {
               "type": "string",
               "description": "The path to the issuer endpoint"
              },
              "certificationUri": {
               "type": "string",
               "description": "The path to the jwks uri"
              },
              "wellKnownOpenIdConfiguration": {
               "type": "string",
               "description": "The path to the well known configuration endpoint"
              }
             }
            }
           },
           "additionalProperties": false
          },
          "login": {
           "type": "object",
           "description": "",
           "properties": {
            "nameClaimType": {
             "type": "string"
            },
            "scopes": {
             "type": "array",
             "items": {
              "type": "string"
             }
            },
            "loginParameterNames": {
             "type": "array",
             "items": {
              "type": "string"
             }
            }
           },
           "additionalProperties": false
          }
         },
         "additionalProperties": false
        }
       }
      }
     },
     "additionalProperties": false
    }
   },
   "additionalProperties": false
  }
 },
 "description": "Documentation: https://aka.ms/swa/config-schema",
 "properties": {
  "$$schema": {
   "type": "string",
   "default": "https://www.schemastore.org/staticwebapp.config.json",
   "description": "JSON schema"
  },
  "routes": {
   "type": "array",
   "description": "Route definitions to modify routing behavior",
   "default": [
    {
     "route": "/example",
     "rewrite": "/example.html"
    }
   ],
   "items": {
    "examples": [
     {
      "route": "/example",
      "rewrite": "/example.html"
     },
     {
      "route": "/login",
      "redirect": "/.auth/login/github"
     }
    ],
    "anyOf": [
     {
      "allOf": [
       {
        "$$ref": "#/definitions/route"
       }
      ]
     }
    ]
   }
  },
  "navigationFallback": {
   "type": "object",
   "description": "A default file to return if the request does not match a resource",
   "default": {
    "rewrite": "/index.html"
   },
   "required": [
    "rewrite"
   ],
   "properties": {
    "rewrite": {
     "type": "string",
     "description": "The default file to return if the request does not match a resource",
     "default": "/index.html"
    },
    "exclude": {
     "type": "array",
     "description": "Paths to exclude from the fallback route. May use valid wildcards. https://aka.ms/swa/config-schema",
     "examples": [
      [
       "*.{jpg,gif,png}",
       "assets/*"
      ]
     ]
    }
   },
   "additionalProperties": false
  },
  "responseOverrides": {
   "type": "object",
   "description": "Custom error pages or redirects",
   "examples": [
    {
     "404": {
      "rewrite": "/custom_404.html",
      "statusCode": 200
     }
    }
   ],
   "propertyNames": {
    "pattern": "^\\\\d+$$"
   },
   "patternProperties": {
    ".*": {
     "oneOf": [
      {
       "type": "object",
       "properties": {
        "redirect": {
         "type": "string",
         "description": "Redirect to a relative or absolute path, or an external URI. Default status code is 302, override with 301."
        },
        "statusCode": {
         "type": "integer",
         "description": "Status code"
        },
        "rewrite": {
         "type": "string",
         "description": "A path to rewrite the request route to"
        }
       }
      }
     ]
    }
   }
  },
  "mimeTypes": {
   "type": "object",
   "description": "Custom mime types configuration",
   "default": {},
   "examples": [
    {
     ".config": "application/xml"
    }
   ],
   "patternProperties": {
    "^\\\\..+$$": {
     "type": "string"
    }
   },
   "additionalProperties": false
  },
  "globalHeaders": {
   "type": "object",
   "description": "Default headers to set on all responses",
   "additionalProperties": true
  },
  "auth": {
   "$$ref": "#/definitions/auth"
  },
  "networking": {
   "type": "object",
   "description": "Networking configuration",
   "properties": {
    "allowedIpRanges": {
     "type": "array",
     "description": "Restrict access to one or more IPv4 ranges. Supports CIDR notation (e.g., \\"192.168.100.14/24\\")",
     "items": {
      "type": "string"
     },
     "examples": [
      [
       "10.0.0.0/24",
       "192.1.1.1/10"
      ]
     ]
    }
   },
   "additionalProperties": false
  },
  "forwardingGateway": {
   "type": "object",
   "description": "Forwarding gateway configuration",
   "properties": {
    "allowedForwardedHosts": {
     "type": "array",
     "description": "The value of `X-Forwarded-Host` to allow to be used when generating redirect URLs",
     "items": {
      "type": "string"
     },
     "examples": [
      [
       "example.org",
       "www.example.org",
       "staging.example.org"
      ]
     ]
    },
    "requiredHeaders": {
     "type": "object",
     "description": "HTTP header name/value pairs that are required for access",
     "examples": [
      {
       "X-Azure-FDID": "10dd26ef"
      }
     ],
     "additionalProperties": true
    }
   },
   "additionalProperties": false
  },
  "platform": {
   "type": "object",
   "description": "Platform configuration",
   "properties": {
    "apiRuntime": {
     "type": "string",
     "enum": [
      "dotnet:3.1",
      "dotnet:6.0",
      "dotnet-isolated:6.0",
      "dotnet-isolated:7.0",
      "dotnet-isolated:8.0",
      "dotnet-isolated:9.0",
      "node:12",
      "node:14",
      "node:16",
      "node:18",
      "node:20",
      "python:3.8",
      "python:3.9",
      "python:3.10"
     ],
     "description": "Language runtime for the managed functions API"
    }
   },
   "additionalProperties": false
  },
  "trailingSlash": {
   "type": "string",
   "enum": [
    "always",
    "never",
    "auto"
   ],
   "description": "Trailing slash configuration"
  }
 },
 "title": "Azure Static Web Apps configuration file",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// babelrc.json (6258 bytes)
object BabelSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/babelrc.json",
 "allOf": [
  {
   "$$ref": "#/definitions/Options"
  },
  {
   "properties": {
    "env": {
     "description": "This is an object of keys that represent different environments. For example, you may have: `{ env: { production: { /* specific options */ } } }` which will use those options when the environment variable BABEL_ENV is set to \\"production\\". If BABEL_ENV isn't set then NODE_ENV will be used, if it's not set then it defaults to \\"development\\"",
     "type": "object",
     "additionalProperties": {
      "$$ref": "#/definitions/Options"
     },
     "default": {}
    }
   }
  }
 ],
 "definitions": {
  "Options": {
   "type": "object",
   "properties": {
    "ast": {
     "description": "Include the AST in the returned object",
     "type": "boolean",
     "default": true
    },
    "auxiliaryCommentAfter": {
     "description": "Attach a comment after all non-user injected code.",
     "type": "string"
    },
    "auxiliaryCommentBefore": {
     "description": "Attach a comment before all non-user injected code.",
     "type": "string"
    },
    "code": {
     "description": "Enable code generation",
     "type": "boolean",
     "default": true
    },
    "comments": {
     "description": "Output comments in generated output.",
     "type": "boolean",
     "default": true
    },
    "compact": {
     "description": "Do not include superfluous whitespace characters and line terminators. When set to \\"auto\\" compact is set to true on input sizes of >500KB.",
     "anyOf": [
      {
       "const": [
        "auto"
       ],
       "type": "string"
      },
      {
       "type": "boolean"
      }
     ],
     "default": "auto"
    },
    "extends": {
     "description": "A path to a .babelrc file to extend",
     "type": "string"
    },
    "filename": {
     "description": "Filename for use in errors etc.",
     "type": "string",
     "default": "unknown"
    },
    "filenameRelative": {
     "description": "Filename relative to sourceRoot (defaults to \\"filename\\")",
     "type": "string"
    },
    "highlightCode": {
     "description": "ANSI highlight syntax error code frames",
     "type": "boolean"
    },
    "ignore": {
     "description": "Opposite of the \\"only\\" option",
     "anyOf": [
      {
       "type": "string"
      },
      {
       "items": {
        "type": "string"
       },
       "type": "array"
      }
     ]
    },
    "inputSourceMap": {
     "description": "If true, attempt to load an input sourcemap from the file itself. If an object is provided, it will be treated as the source map object itself.",
     "anyOf": [
      {
       "type": "boolean"
      },
      {
       "type": "object"
      }
     ],
     "default": true
    },
    "keepModuleIdExtensions": {
     "description": "Keep extensions in module ids",
     "type": "boolean",
     "default": false
    },
    "moduleId": {
     "description": "Specify a custom name for module ids.",
     "type": "string"
    },
    "moduleIds": {
     "description": "If truthy, insert an explicit id for modules. By default, all modules are anonymous. (Not available for common modules)",
     "type": "string",
     "default": false
    },
    "moduleRoot": {
     "description": "Optional prefix for the AMD module formatter that will be prepend to the filename on module definitions. (defaults to \\"sourceRoot\\")",
     "type": "string"
    },
    "only": {
     "description": "A glob, regex, or mixed array of both, matching paths to only compile. Can also be an array of arrays containing paths to explicitly match. When attempting to compile a non-matching file it's returned verbatim.",
     "anyOf": [
      {
       "type": "string"
      },
      {
       "items": {
        "type": "string"
       },
       "type": "array"
      }
     ]
    },
    "plugins": {
     "description": "List of plugins to load and use",
     "type": "array",
     "items": {
      "anyOf": [
       {
        "type": "string"
       },
       {
        "items": [
         {
          "description": "The name of the plugin.",
          "type": "string"
         },
         {
          "description": "The options of the plugin.",
          "type": "object"
         }
        ],
        "type": "array",
        "minItems": 2,
        "additionalItems": false
       }
      ]
     }
    },
    "presets": {
     "description": "List of presets (a set of plugins) to load and use",
     "type": "array",
     "items": {
      "anyOf": [
       {
        "type": "string"
       },
       {
        "type": "array",
        "items": [
         {
          "description": "The name of the preset.",
          "type": "string"
         },
         {
          "description": "The options of the preset.",
          "type": "object"
         }
        ],
        "minItems": 2,
        "additionalItems": false
       }
      ]
     }
    },
    "retainLines": {
     "default": false,
     "description": "Retain line numbers. This will lead to wacky code but is handy for scenarios where you can't use source maps. NOTE: This will obviously not retain the columns.",
     "type": "boolean"
    },
    "sourceFileName": {
     "description": "Set sources[0] on returned source map. (defaults to \\"filenameRelative\\")",
     "type": "string"
    },
    "sourceMaps": {
     "default": false,
     "description": "If truthy, adds a map property to returned output. If set to \\"inline\\", a comment with a sourceMappingURL directive is added to the bottom of the returned code. If set to \\"both\\" then a map property is returned as well as a source map comment appended.",
     "anyOf": [
      {
       "type": "string",
       "enum": [
        "both",
        "inline"
       ]
      },
      {
       "type": "boolean"
      }
     ]
    },
    "sourceMapTarget": {
     "description": "Set file on returned source map. (defaults to \\"filenameRelative\\")",
     "type": "string"
    },
    "sourceRoot": {
     "description": "The root from which all sources are relative. (defaults to \\"moduleRoot\\")",
     "type": "string"
    }
   }
  }
 },
 "title": "JSON schema for Babel 6+ configuration files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// openweather.roadrisk.json (1338 bytes)
object OpenWeatherRoadRiskSchema extends Json.Provider(t"""{
 "$$schema": "https://json-schema.org/draft/2019-09/schema",
 "$$id": "https://json.schemastore.org/openweather.roadrisk",
 "description": "API responses from the OpenWeather Road Risk API from https://openweathermap.org/api/road-risk",
 "items": {
  "type": "object",
  "additionalProperties": false,
  "required": [
   "dt",
   "coord",
   "weather",
   "alerts"
  ],
  "properties": {
   "dt": {
    "type": "integer"
   },
   "coord": {
    "type": "array",
    "items": {
     "type": "number"
    }
   },
   "weather": {
    "type": "object",
    "additionalProperties": false,
    "properties": {
     "temp": {
      "type": "number"
     },
     "wind_speed": {
      "type": "number"
     },
     "wind_deg": {
      "type": "number"
     },
     "precipitation_intensity": {
      "type": "number"
     },
     "dew_point": {
      "type": "number"
     }
    }
   },
   "alerts": {
    "type": "array",
    "items": {
     "type": "object",
     "additionalProperties": false,
     "required": [
      "sender_name",
      "event",
      "event_level"
     ],
     "properties": {
      "sender_name": {
       "type": "string"
      },
      "event": {
       "type": "string"
      },
      "event_level": {
       "type": "integer"
      }
     }
    }
   }
  }
 },
 "title": "OpenWeather Road Risk API",
 "type": "array"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// renovate.json (64 bytes)
object RenovateSchema extends Json.Provider(t"""{
 "$$ref": "https://docs.renovatebot.com/renovate-schema.json"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// package.json (42031 bytes)
object PackageJsonSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "title": "JSON schema for NPM package.json files",
 "definitions": {
  "person": {
   "description": "A person who has been involved in creating or maintaining this package.",
   "type": [
    "object",
    "string"
   ],
   "required": [
    "name"
   ],
   "properties": {
    "name": {
     "type": "string"
    },
    "url": {
     "type": "string",
     "format": "uri"
    },
    "email": {
     "type": "string",
     "format": "email"
    }
   }
  },
  "dependency": {
   "description": "Dependencies are specified with a simple hash of package name to version range. The version range is a string which has one or more space-separated descriptors. Dependencies can also be identified with a tarball or git URL.",
   "type": "object",
   "additionalProperties": {
    "type": "string"
   }
  },
  "devDependency": {
   "description": "Specifies dependencies that are required for the development and testing of the project. These dependencies are not needed in the production environment.",
   "type": "object",
   "additionalProperties": {
    "type": "string"
   }
  },
  "optionalDependency": {
   "description": "Specifies dependencies that are optional for your project. These dependencies are attempted to be installed during the npm install process, but if they fail to install, the installation process will not fail.",
   "type": "object",
   "additionalProperties": {
    "type": "string"
   }
  },
  "peerDependency": {
   "description": "Specifies dependencies that are required by the package but are expected to be provided by the consumer of the package.",
   "type": "object",
   "additionalProperties": {
    "type": "string"
   }
  },
  "peerDependencyMeta": {
   "description": "When a user installs your package, warnings are emitted if packages specified in \\"peerDependencies\\" are not already installed. The \\"peerDependenciesMeta\\" field serves to provide more information on how your peer dependencies are utilized. Most commonly, it allows peer dependencies to be marked as optional. Metadata for this field is specified with a simple hash of the package name to a metadata object.",
   "type": "object",
   "additionalProperties": {
    "type": "object",
    "additionalProperties": true,
    "properties": {
     "optional": {
      "description": "Specifies that this peer dependency is optional and should not be installed automatically.",
      "type": "boolean"
     }
    }
   }
  },
  "license": {
   "anyOf": [
    {
     "type": "string"
    },
    {
     "enum": [
      "AGPL-3.0-only",
      "Apache-2.0",
      "BSD-2-Clause",
      "BSD-3-Clause",
      "BSL-1.0",
      "CC0-1.0",
      "CDDL-1.0",
      "CDDL-1.1",
      "EPL-1.0",
      "EPL-2.0",
      "GPL-2.0-only",
      "GPL-3.0-only",
      "ISC",
      "LGPL-2.0-only",
      "LGPL-2.1-only",
      "LGPL-2.1-or-later",
      "LGPL-3.0-only",
      "LGPL-3.0-or-later",
      "MIT",
      "MPL-2.0",
      "MS-PL",
      "UNLICENSED"
     ]
    }
   ]
  },
  "scriptsInstallAfter": {
   "description": "Run AFTER the package is installed.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "scriptsPublishAfter": {
   "description": "Run AFTER the package is published.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "scriptsRestart": {
   "description": "Run by the 'npm restart' command. Note: 'npm restart' will run the stop and start scripts if no restart script is provided.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "scriptsStart": {
   "description": "Run by the 'npm start' command.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "scriptsStop": {
   "description": "Run by the 'npm stop' command.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "scriptsTest": {
   "description": "Run by the 'npm test' command.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "scriptsUninstallBefore": {
   "description": "Run BEFORE the package is uninstalled.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "scriptsVersionBefore": {
   "description": "Run BEFORE bump the package version.",
   "type": "string",
   "x-intellij-language-injection": "Shell Script"
  },
  "packageExportsEntryPath": {
   "type": [
    "string",
    "null"
   ],
   "description": "The module path that is resolved when this specifier is imported. Set to `null` to disallow importing this module.",
   "pattern": "^\\\\./"
  },
  "packageExportsEntryObject": {
   "type": "object",
   "description": "Used to specify conditional exports, note that Conditional exports are unsupported in older environments, so it's recommended to use the fallback array option if support for those environments is a concern.",
   "properties": {
    "require": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "description": "The module path that is resolved when this specifier is imported as a CommonJS module using the `require(...)` function."
    },
    "import": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "description": "The module path that is resolved when this specifier is imported as an ECMAScript module using an `import` declaration or the dynamic `import(...)` function."
    },
    "module-sync": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "$$comment": "https://nodejs.org/api/packages.html#conditional-exports#:~:text=%22module-sync%22",
     "description": "The same as `import`, but can be used with require(esm) in Node 20+. This requires the files to not use any top-level awaits."
    },
    "node": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "description": "The module path that is resolved when this environment is Node.js."
    },
    "default": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "description": "The module path that is resolved when no other export type matches."
    },
    "types": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "description": "The module path that is resolved for TypeScript types when this specifier is imported. Should be listed before other conditions. Additionally, versioned \\"types\\" condition in the form \\"types@{selector}\\" are supported."
    }
   },
   "patternProperties": {
    "^[^.0-9]+$$": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "description": "The module path that is resolved when this environment matches the property name."
    },
    "^types@.+$$": {
     "$$ref": "#/definitions/packageExportsEntryOrFallback",
     "description": "The module path that is resolved for TypeScript types when this specifier is imported. Should be listed before other conditions. Additionally, versioned \\"types\\" condition in the form \\"types@{selector}\\" are supported."
    }
   },
   "additionalProperties": false
  },
  "packageExportsEntry": {
   "oneOf": [
    {
     "$$ref": "#/definitions/packageExportsEntryPath"
    },
    {
     "$$ref": "#/definitions/packageExportsEntryObject"
    }
   ]
  },
  "packageExportsFallback": {
   "type": "array",
   "description": "Used to allow fallbacks in case this environment doesn't support the preceding entries.",
   "items": {
    "$$ref": "#/definitions/packageExportsEntry"
   }
  },
  "packageExportsEntryOrFallback": {
   "oneOf": [
    {
     "$$ref": "#/definitions/packageExportsEntry"
    },
    {
     "$$ref": "#/definitions/packageExportsFallback"
    }
   ]
  },
  "packageImportsEntryPath": {
   "type": [
    "string",
    "null"
   ],
   "description": "The module path that is resolved when this specifier is imported. Set to `null` to disallow importing this module."
  },
  "packageImportsEntryObject": {
   "type": "object",
   "description": "Used to specify conditional exports, note that Conditional exports are unsupported in older environments, so it's recommended to use the fallback array option if support for those environments is a concern.",
   "properties": {
    "require": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved when this specifier is imported as a CommonJS module using the `require(...)` function."
    },
    "import": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved when this specifier is imported as an ECMAScript module using an `import` declaration or the dynamic `import(...)` function."
    },
    "node": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved when this environment is Node.js."
    },
    "default": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved when no other export type matches."
    },
    "types": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved for TypeScript types when this specifier is imported. Should be listed before other conditions. Additionally, versioned \\"types\\" condition in the form \\"types@{selector}\\" are supported."
    }
   },
   "patternProperties": {
    "^[^.0-9]+$$": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved when this environment matches the property name."
    },
    "^types@.+$$": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved for TypeScript types when this specifier is imported. Should be listed before other conditions. Additionally, versioned \\"types\\" condition in the form \\"types@{selector}\\" are supported."
    }
   },
   "additionalProperties": false
  },
  "packageImportsEntry": {
   "oneOf": [
    {
     "$$ref": "#/definitions/packageImportsEntryPath"
    },
    {
     "$$ref": "#/definitions/packageImportsEntryObject"
    }
   ]
  },
  "packageImportsFallback": {
   "type": "array",
   "description": "Used to allow fallbacks in case this environment doesn't support the preceding entries.",
   "items": {
    "$$ref": "#/definitions/packageImportsEntry"
   }
  },
  "packageImportsEntryOrFallback": {
   "oneOf": [
    {
     "$$ref": "#/definitions/packageImportsEntry"
    },
    {
     "$$ref": "#/definitions/packageImportsFallback"
    }
   ]
  },
  "fundingUrl": {
   "type": "string",
   "format": "uri",
   "description": "URL to a website with details about how to fund the package."
  },
  "fundingWay": {
   "type": "object",
   "description": "Used to inform about ways to help fund development of the package.",
   "properties": {
    "url": {
     "$$ref": "#/definitions/fundingUrl"
    },
    "type": {
     "type": "string",
     "description": "The type of funding or the platform through which funding can be provided, e.g. patreon, opencollective, tidelift or github."
    }
   },
   "additionalProperties": false,
   "required": [
    "url"
   ]
  },
  "devEngineDependency": {
   "description": "Specifies requirements for development environment components such as operating systems, runtimes, or package managers. Used to ensure consistent development environments across the team.",
   "type": "object",
   "required": [
    "name"
   ],
   "properties": {
    "name": {
     "type": "string",
     "description": "The name of the dependency, with allowed values depending on the parent field"
    },
    "version": {
     "type": "string",
     "description": "The version range for the dependency"
    },
    "onFail": {
     "type": "string",
     "enum": [
      "ignore",
      "warn",
      "error",
      "download"
     ],
     "description": "What action to take if validation fails"
    }
   }
  },
  "runtimeEngineDependency": {
   "description": "Specifies a supported JavaScript runtime.",
   "type": "object",
   "required": [
    "name"
   ],
   "properties": {
    "name": {
     "type": "string",
     "description": "The runtime name"
    },
    "version": {
     "type": "string",
     "description": "The version range for the runtime"
    },
    "onFail": {
     "type": "string",
     "enum": [
      "ignore",
      "warn",
      "error",
      "download"
     ],
     "description": "What action to take if runtime validation fails"
    }
   }
  }
 },
 "type": "object",
 "patternProperties": {
  "^_": {
   "description": "Any property starting with _ is valid.",
   "tsType": "any"
  }
 },
 "properties": {
  "name": {
   "description": "The name of the package.",
   "type": "string",
   "maxLength": 214,
   "minLength": 1
  },
  "version": {
   "description": "Version must be parsable by node-semver, which is bundled with npm as a dependency.",
   "type": "string"
  },
  "description": {
   "description": "This helps people discover your package, as it's listed in 'npm search'.",
   "type": "string"
  },
  "keywords": {
   "description": "This helps people discover your package as it's listed in 'npm search'.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "homepage": {
   "description": "The url to the project homepage.",
   "type": "string"
  },
  "bugs": {
   "description": "The url to your project's issue tracker and / or the email address to which issues should be reported. These are helpful for people who encounter issues with your package.",
   "type": [
    "object",
    "string"
   ],
   "properties": {
    "url": {
     "type": "string",
     "description": "The url to your project's issue tracker.",
     "format": "uri"
    },
    "email": {
     "type": "string",
     "description": "The email address to which issues should be reported.",
     "format": "email"
    }
   }
  },
  "license": {
   "$$ref": "#/definitions/license",
   "description": "You should specify a license for your package so that people know how they are permitted to use it, and any restrictions you're placing on it."
  },
  "licenses": {
   "description": "DEPRECATED: Instead, use SPDX expressions, like this: { \\"license\\": \\"ISC\\" } or { \\"license\\": \\"(MIT OR Apache-2.0)\\" } see: 'https://docs.npmjs.com/files/package.json#license'.",
   "type": "array",
   "items": {
    "type": "object",
    "properties": {
     "type": {
      "$$ref": "#/definitions/license"
     },
     "url": {
      "type": "string",
      "format": "uri"
     }
    }
   }
  },
  "author": {
   "$$ref": "#/definitions/person"
  },
  "contributors": {
   "description": "A list of people who contributed to this package.",
   "type": "array",
   "items": {
    "$$ref": "#/definitions/person"
   }
  },
  "maintainers": {
   "description": "A list of people who maintains this package.",
   "type": "array",
   "items": {
    "$$ref": "#/definitions/person"
   }
  },
  "files": {
   "description": "The 'files' field is an array of files to include in your project. If you name a folder in the array, then it will also include the files inside that folder.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "main": {
   "description": "The main field is a module ID that is the primary entry point to your program.",
   "type": "string"
  },
  "exports": {
   "description": "The \\"exports\\" field is used to restrict external access to non-exported module files, also enables a module to import itself using \\"name\\".",
   "oneOf": [
    {
     "$$ref": "#/definitions/packageExportsEntryPath",
     "description": "The module path that is resolved when the module specifier matches \\"name\\", shadows the \\"main\\" field."
    },
    {
     "type": "object",
     "properties": {
      ".": {
       "$$ref": "#/definitions/packageExportsEntryOrFallback",
       "description": "The module path that is resolved when the module specifier matches \\"name\\", shadows the \\"main\\" field."
      }
     },
     "patternProperties": {
      "^\\\\./.+": {
       "$$ref": "#/definitions/packageExportsEntryOrFallback",
       "description": "The module path prefix that is resolved when the module specifier starts with \\"name/\\", set to \\"./*\\" to allow external modules to import any subpath."
      }
     },
     "additionalProperties": false
    },
    {
     "$$ref": "#/definitions/packageExportsEntryObject",
     "description": "The module path that is resolved when the module specifier matches \\"name\\", shadows the \\"main\\" field."
    },
    {
     "$$ref": "#/definitions/packageExportsFallback",
     "description": "The module path that is resolved when the module specifier matches \\"name\\", shadows the \\"main\\" field."
    }
   ]
  },
  "imports": {
   "description": "The \\"imports\\" field is used to create private mappings that only apply to import specifiers from within the package itself.",
   "type": "object",
   "patternProperties": {
    "^#.+$$": {
     "$$ref": "#/definitions/packageImportsEntryOrFallback",
     "description": "The module path that is resolved when this environment matches the property name."
    }
   },
   "additionalProperties": false
  },
  "bin": {
   "type": [
    "string",
    "object"
   ],
   "additionalProperties": {
    "type": "string"
   }
  },
  "type": {
   "description": "When set to \\"module\\", the type field allows a package to specify all .js files within are ES modules. If the \\"type\\" field is omitted or set to \\"commonjs\\", all .js files are treated as CommonJS.",
   "type": "string",
   "enum": [
    "commonjs",
    "module"
   ],
   "default": "commonjs"
  },
  "types": {
   "description": "Set the types property to point to your bundled declaration file.",
   "type": "string"
  },
  "typings": {
   "description": "Note that the \\"typings\\" field is synonymous with \\"types\\", and could be used as well.",
   "type": "string"
  },
  "typesVersions": {
   "description": "The \\"typesVersions\\" field is used since TypeScript 3.1 to support features that were only made available in newer TypeScript versions.",
   "type": "object",
   "additionalProperties": {
    "description": "Contains overrides for the TypeScript version that matches the version range matching the property key.",
    "type": "object",
    "properties": {
     "*": {
      "description": "Maps all file paths to the file paths specified in the array.",
      "type": "array",
      "items": {
       "type": "string",
       "pattern": "^[^*]*(?:\\\\*[^*]*)?$$"
      }
     }
    },
    "patternProperties": {
     "^[^*]+$$": {
      "description": "Maps the file path matching the property key to the file paths specified in the array.",
      "type": "array",
      "items": {
       "type": "string"
      }
     },
     "^[^*]*\\\\*[^*]*$$": {
      "description": "Maps file paths matching the pattern specified in property key to file paths specified in the array.",
      "type": "array",
      "items": {
       "type": "string",
       "pattern": "^[^*]*(?:\\\\*[^*]*)?$$"
      }
     }
    },
    "additionalProperties": false
   }
  },
  "man": {
   "type": [
    "array",
    "string"
   ],
   "description": "Specify either a single file or an array of filenames to put in place for the man program to find.",
   "items": {
    "type": "string"
   }
  },
  "directories": {
   "type": "object",
   "properties": {
    "bin": {
     "description": "If you specify a 'bin' directory, then all the files in that folder will be used as the 'bin' hash.",
     "type": "string"
    },
    "doc": {
     "description": "Put markdown files in here. Eventually, these will be displayed nicely, maybe, someday.",
     "type": "string"
    },
    "example": {
     "description": "Put example scripts in here. Someday, it might be exposed in some clever way.",
     "type": "string"
    },
    "lib": {
     "description": "Tell people where the bulk of your library is. Nothing special is done with the lib folder in any way, but it's useful meta info.",
     "type": "string"
    },
    "man": {
     "description": "A folder that is full of man pages. Sugar to generate a 'man' array by walking the folder.",
     "type": "string"
    },
    "test": {
     "type": "string"
    }
   }
  },
  "repository": {
   "description": "Specify the place where your code lives. This is helpful for people who want to contribute.",
   "type": [
    "object",
    "string"
   ],
   "properties": {
    "type": {
     "type": "string"
    },
    "url": {
     "type": "string"
    },
    "directory": {
     "type": "string"
    }
   }
  },
  "funding": {
   "oneOf": [
    {
     "$$ref": "#/definitions/fundingUrl"
    },
    {
     "$$ref": "#/definitions/fundingWay"
    },
    {
     "type": "array",
     "items": {
      "oneOf": [
       {
        "$$ref": "#/definitions/fundingUrl"
       },
       {
        "$$ref": "#/definitions/fundingWay"
       }
      ]
     },
     "minItems": 1,
     "uniqueItems": true
    }
   ]
  },
  "scripts": {
   "description": "The 'scripts' member is an object hash of script commands that are run at various times in the lifecycle of your package. The key is the lifecycle event, and the value is the command to run at that point.",
   "type": "object",
   "properties": {
    "lint": {
     "type": "string",
     "description": "Run code quality tools, e.g. ESLint, TSLint, etc."
    },
    "prepublish": {
     "type": "string",
     "description": "Run BEFORE the package is published (Also run on local npm install without any arguments)."
    },
    "prepare": {
     "type": "string",
     "description": "Runs BEFORE the package is packed, i.e. during \\"npm publish\\" and \\"npm pack\\", and on local \\"npm install\\" without any arguments. This is run AFTER \\"prepublish\\", but BEFORE \\"prepublishOnly\\"."
    },
    "prepublishOnly": {
     "type": "string",
     "description": "Run BEFORE the package is prepared and packed, ONLY on npm publish."
    },
    "prepack": {
     "type": "string",
     "description": "run BEFORE a tarball is packed (on npm pack, npm publish, and when installing git dependencies)."
    },
    "postpack": {
     "type": "string",
     "description": "Run AFTER the tarball has been generated and moved to its final destination."
    },
    "publish": {
     "type": "string",
     "description": "Publishes a package to the registry so that it can be installed by name. See https://docs.npmjs.com/cli/v8/commands/npm-publish"
    },
    "postpublish": {
     "$$ref": "#/definitions/scriptsPublishAfter"
    },
    "preinstall": {
     "type": "string",
     "description": "Run BEFORE the package is installed."
    },
    "install": {
     "$$ref": "#/definitions/scriptsInstallAfter"
    },
    "postinstall": {
     "$$ref": "#/definitions/scriptsInstallAfter"
    },
    "preuninstall": {
     "$$ref": "#/definitions/scriptsUninstallBefore"
    },
    "uninstall": {
     "$$ref": "#/definitions/scriptsUninstallBefore"
    },
    "postuninstall": {
     "type": "string",
     "description": "Run AFTER the package is uninstalled."
    },
    "preversion": {
     "$$ref": "#/definitions/scriptsVersionBefore"
    },
    "version": {
     "$$ref": "#/definitions/scriptsVersionBefore"
    },
    "postversion": {
     "type": "string",
     "description": "Run AFTER bump the package version."
    },
    "pretest": {
     "$$ref": "#/definitions/scriptsTest"
    },
    "test": {
     "$$ref": "#/definitions/scriptsTest"
    },
    "posttest": {
     "$$ref": "#/definitions/scriptsTest"
    },
    "prestop": {
     "$$ref": "#/definitions/scriptsStop"
    },
    "stop": {
     "$$ref": "#/definitions/scriptsStop"
    },
    "poststop": {
     "$$ref": "#/definitions/scriptsStop"
    },
    "prestart": {
     "$$ref": "#/definitions/scriptsStart"
    },
    "start": {
     "$$ref": "#/definitions/scriptsStart"
    },
    "poststart": {
     "$$ref": "#/definitions/scriptsStart"
    },
    "prerestart": {
     "$$ref": "#/definitions/scriptsRestart"
    },
    "restart": {
     "$$ref": "#/definitions/scriptsRestart"
    },
    "postrestart": {
     "$$ref": "#/definitions/scriptsRestart"
    },
    "serve": {
     "type": "string",
     "description": "Start dev server to serve application files"
    }
   },
   "additionalProperties": {
    "type": "string",
    "tsType": "string | undefined",
    "x-intellij-language-injection": "Shell Script"
   }
  },
  "config": {
   "description": "A 'config' hash can be used to set configuration parameters used in package scripts that persist across upgrades.",
   "type": "object",
   "additionalProperties": true
  },
  "dependencies": {
   "$$ref": "#/definitions/dependency"
  },
  "devDependencies": {
   "$$ref": "#/definitions/devDependency"
  },
  "optionalDependencies": {
   "$$ref": "#/definitions/optionalDependency"
  },
  "peerDependencies": {
   "$$ref": "#/definitions/peerDependency"
  },
  "peerDependenciesMeta": {
   "$$ref": "#/definitions/peerDependencyMeta"
  },
  "bundleDependencies": {
   "description": "Array of package names that will be bundled when publishing the package.",
   "oneOf": [
    {
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    {
     "type": "boolean"
    }
   ]
  },
  "bundledDependencies": {
   "description": "DEPRECATED: This field is honored, but \\"bundleDependencies\\" is the correct field name. Ignored if \\"bundleDependencies\\" is also present.",
   "oneOf": [
    {
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    {
     "type": "boolean"
    }
   ]
  },
  "resolutions": {
   "description": "Resolutions is used to support selective version resolutions using yarn, which lets you define custom package versions or ranges inside your dependencies. For npm, use overrides instead. See: https://yarnpkg.com/configuration/manifest#resolutions",
   "type": "object"
  },
  "overrides": {
   "description": "Overrides is used to support selective version overrides using npm, which lets you define custom package versions or ranges inside your dependencies. For yarn, use resolutions instead. See: https://docs.npmjs.com/cli/v9/configuring-npm/package-json#overrides",
   "type": "object"
  },
  "packageManager": {
   "description": "Defines which package manager is expected to be used when working on the current project. This field is currently experimental and needs to be opted-in; see https://nodejs.org/api/corepack.html",
   "type": "string",
   "oneOf": [
    {
     "pattern": "(npm|pnpm|yarn|bun|aube|nub|utoo)@\\\\d+\\\\.\\\\d+\\\\.\\\\d+(-.+)?"
    },
    {
     "const": "bun"
    }
   ]
  },
  "engines": {
   "type": "object",
   "properties": {
    "node": {
     "type": "string"
    },
    "runtime": {
     "oneOf": [
      {
       "$$ref": "#/definitions/runtimeEngineDependency"
      },
      {
       "type": "array",
       "items": {
        "$$ref": "#/definitions/runtimeEngineDependency"
       }
      }
     ],
     "description": "Specifies which JavaScript runtimes (like Node.js, Deno, Bun) are supported. Values should use WinterCG Runtime Keys (see https://runtime-keys.proposal.wintercg.org/)."
    }
   },
   "additionalProperties": {
    "type": "string"
   }
  },
  "volta": {
   "description": "Defines which tools and versions are expected to be used when Volta is installed.",
   "type": "object",
   "properties": {
    "extends": {
     "description": "The value of that entry should be a path to another JSON file which also has a \\"volta\\" section",
     "type": "string"
    }
   },
   "patternProperties": {
    "(node|npm|pnpm|yarn)": {
     "type": "string"
    }
   }
  },
  "engineStrict": {
   "type": "boolean"
  },
  "os": {
   "description": "Specify which operating systems your module will run on.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "cpu": {
   "description": "Specify that your code only runs on certain cpu architectures.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "devEngines": {
   "description": "Define the runtime and package manager for developing the current project.",
   "type": "object",
   "properties": {
    "os": {
     "oneOf": [
      {
       "$$ref": "#/definitions/devEngineDependency"
      },
      {
       "type": "array",
       "items": {
        "$$ref": "#/definitions/devEngineDependency"
       }
      }
     ],
     "description": "Specifies which operating systems are supported for development"
    },
    "cpu": {
     "oneOf": [
      {
       "$$ref": "#/definitions/devEngineDependency"
      },
      {
       "type": "array",
       "items": {
        "$$ref": "#/definitions/devEngineDependency"
       }
      }
     ],
     "description": "Specifies which CPU architectures are supported for development"
    },
    "libc": {
     "oneOf": [
      {
       "$$ref": "#/definitions/devEngineDependency"
      },
      {
       "type": "array",
       "items": {
        "$$ref": "#/definitions/devEngineDependency"
       }
      }
     ],
     "description": "Specifies which C standard libraries are supported for development"
    },
    "runtime": {
     "oneOf": [
      {
       "$$ref": "#/definitions/devEngineDependency"
      },
      {
       "type": "array",
       "items": {
        "$$ref": "#/definitions/devEngineDependency"
       }
      }
     ],
     "description": "Specifies which JavaScript runtimes (like Node.js, Deno, Bun) are supported for development. Values should use WinterCG Runtime Keys (see https://runtime-keys.proposal.wintercg.org/)"
    },
    "packageManager": {
     "oneOf": [
      {
       "$$ref": "#/definitions/devEngineDependency"
      },
      {
       "type": "array",
       "items": {
        "$$ref": "#/definitions/devEngineDependency"
       }
      }
     ],
     "description": "Specifies which package managers are supported for development"
    }
   }
  },
  "preferGlobal": {
   "type": "boolean",
   "description": "DEPRECATED: This option used to trigger an npm warning, but it will no longer warn. It is purely there for informational purposes. It is now recommended that you install any binaries as local devDependencies wherever possible."
  },
  "private": {
   "description": "If set to true, then npm will refuse to publish it.",
   "oneOf": [
    {
     "type": "boolean"
    },
    {
     "enum": [
      "false",
      "true"
     ]
    }
   ]
  },
  "publishConfig": {
   "description": "Values applied at publish time. npm: publish-time config values (tag, registry, access, provenance), see https://docs.npmjs.com/cli/v12/configuring-npm/package-json#publishconfig. pnpm: also overrides manifest fields (main, exports, types, etc.) before packing, plus pnpm-specific fields (directory, linkDirectory, executableFiles), see https://pnpm.io/package_json#publishconfig.",
   "type": "object",
   "properties": {
    "access": {
     "description": "Access level for scoped packages at publish time (defaults to \\"restricted\\"). Supported by npm, honored by pnpm.",
     "type": "string",
     "enum": [
      "public",
      "restricted"
     ]
    },
    "tag": {
     "description": "Distribution tag to publish under (defaults to \\"latest\\"). Supported by npm, honored by pnpm.",
     "type": "string"
    },
    "registry": {
     "description": "Registry to publish the package to. Supported by npm, honored by pnpm.",
     "type": "string",
     "format": "uri"
    },
    "provenance": {
     "description": "npm only: generate and publish a provenance attestation (requires a supported CI provider).",
     "type": "boolean"
    },
    "directory": {
     "description": "pnpm only: subdirectory (relative to this package.json) to publish from; it must contain its own package.json.",
     "type": "string"
    },
    "linkDirectory": {
     "description": "pnpm only: symlink the project from publishConfig.directory during local development (default: true).",
     "type": "boolean"
    },
    "executableFiles": {
     "description": "pnpm only: additional files to mark executable (+x) in the package archive.",
     "type": "array",
     "items": {
      "type": "string"
     }
    }
   },
   "additionalProperties": true
  },
  "dist": {
   "type": "object",
   "properties": {
    "shasum": {
     "type": "string"
    },
    "tarball": {
     "type": "string"
    }
   }
  },
  "readme": {
   "type": "string"
  },
  "module": {
   "description": "An ECMAScript module ID that is the primary entry point to your program.",
   "type": "string"
  },
  "esnext": {
   "description": "A module ID with untranspiled code that is the primary entry point to your program.",
   "type": [
    "string",
    "object"
   ],
   "properties": {
    "main": {
     "type": "string"
    },
    "browser": {
     "type": "string"
    }
   },
   "additionalProperties": {
    "type": "string"
   }
  },
  "workspaces": {
   "description": "Allows packages within a directory to depend on one another using direct linking of local files. Additionally, dependencies within a workspace are hoisted to the workspace root when possible to reduce duplication. Note: It's also a good idea to set \\"private\\" to true when using this feature.",
   "anyOf": [
    {
     "type": "array",
     "description": "Workspace package paths. Glob patterns are supported.",
     "items": {
      "type": "string"
     }
    },
    {
     "type": "object",
     "properties": {
      "packages": {
       "type": "array",
       "description": "Workspace package paths. Glob patterns are supported.",
       "items": {
        "type": "string"
       }
      },
      "nohoist": {
       "type": "array",
       "description": "Packages to block from hoisting to the workspace root. Currently only supported in Yarn only.",
       "items": {
        "type": "string"
       }
      }
     }
    }
   ]
  },
  "jspm": {
   "$$ref": "#"
  },
  "eslintConfig": {
   "$$ref": "eslintrc.json"
  },
  "prettier": {
   "$$ref": "https://www.schemastore.org/prettierrc.json"
  },
  "stylelint": {
   "$$ref": "stylelintrc.json"
  },
  "ava": {
   "$$ref": "ava.json"
  },
  "release": {
   "$$ref": "semantic-release.json"
  },
  "jscpd": {
   "$$ref": "jscpd.json"
  },
  "madge": {
   "$$ref": "madge.json"
  },
  "nodemonConfig": {
   "$$ref": "nodemon.json"
  },
  "quikrun": {
   "$$ref": "https://www.schemastore.org/quikrun.json"
  },
  "pnpm": {
   "description": "Defines pnpm specific configuration.",
   "type": "object",
   "properties": {
    "overrides": {
     "description": "Used to override any dependency in the dependency graph.",
     "type": "object"
    },
    "packageExtensions": {
     "description": "Used to extend the existing package definitions with additional information.",
     "type": "object",
     "patternProperties": {
      "^.+$$": {
       "type": "object",
       "properties": {
        "dependencies": {
         "$$ref": "#/definitions/dependency"
        },
        "optionalDependencies": {
         "$$ref": "#/definitions/optionalDependency"
        },
        "peerDependencies": {
         "$$ref": "#/definitions/peerDependency"
        },
        "peerDependenciesMeta": {
         "$$ref": "#/definitions/peerDependencyMeta"
        }
       },
       "additionalProperties": false
      }
     },
     "additionalProperties": false
    },
    "peerDependencyRules": {
     "type": "object",
     "properties": {
      "ignoreMissing": {
       "description": "pnpm will not print warnings about missing peer dependencies from this list.",
       "type": "array",
       "items": {
        "type": "string"
       }
      },
      "allowedVersions": {
       "description": "Unmet peer dependency warnings will not be printed for peer dependencies of the specified range.",
       "type": "object"
      },
      "allowAny": {
       "description": "Any peer dependency matching the pattern will be resolved from any version, regardless of the range specified in \\"peerDependencies\\".",
       "type": "array",
       "items": {
        "type": "string"
       }
      }
     },
     "additionalProperties": false
    },
    "neverBuiltDependencies": {
     "description": "A list of dependencies to run builds for.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "onlyBuiltDependencies": {
     "description": "A list of package names that are allowed to be executed during installation.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "onlyBuiltDependenciesFile": {
     "description": "Specifies a JSON file that lists the only packages permitted to run installation scripts during the pnpm install process.",
     "type": "string"
    },
    "ignoredBuiltDependencies": {
     "description": "A list of package names that should not be built during installation.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "allowedDeprecatedVersions": {
     "description": "A list of deprecated versions that the warnings are suppressed.",
     "type": "object"
    },
    "patchedDependencies": {
     "description": "A list of dependencies that are patched.",
     "type": "object"
    },
    "allowNonAppliedPatches": {
     "description": "When true, installation won't fail if some of the patches from the \\"patchedDependencies\\" field were not applied.",
     "type": "boolean"
    },
    "allowUnusedPatches": {
     "description": "When true, installation won't fail if some of the patches from the \\"patchedDependencies\\" field were not applied.",
     "type": "boolean"
    },
    "updateConfig": {
     "type": "object",
     "properties": {
      "ignoreDependencies": {
       "description": "A list of packages that should be ignored when running \\"pnpm outdated\\" or \\"pnpm update --latest\\".",
       "type": "array",
       "items": {
        "type": "string"
       }
      }
     },
     "additionalProperties": false
    },
    "configDependencies": {
     "type": "object",
     "description": "Configurational dependencies are installed before all the other types of dependencies (before 'dependencies', 'devDependencies', 'optionalDependencies')."
    },
    "auditConfig": {
     "type": "object",
     "properties": {
      "ignoreCves": {
       "description": "A list of CVE IDs that will be ignored by \\"pnpm audit\\".",
       "type": "array",
       "items": {
        "type": "string",
        "pattern": "^CVE-\\\\d{4}-\\\\d{4,7}$$"
       }
      },
      "ignoreGhsas": {
       "description": "A list of GHSA Codes that will be ignored by \\"pnpm audit\\".",
       "type": "array",
       "items": {
        "type": "string",
        "pattern": "^GHSA(-[23456789cfghjmpqrvwx]{4}){3}$$"
       }
      }
     },
     "additionalProperties": false
    },
    "requiredScripts": {
     "description": "A list of scripts that must exist in each project.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "supportedArchitectures": {
     "description": "Specifies architectures for which you'd like to install optional dependencies, even if they don't match the architecture of the system running the install.",
     "type": "object",
     "properties": {
      "os": {
       "type": "array",
       "items": {
        "type": "string"
       }
      },
      "cpu": {
       "type": "array",
       "items": {
        "type": "string"
       }
      },
      "libc": {
       "type": "array",
       "items": {
        "type": "string"
       }
      }
     },
     "additionalProperties": false
    },
    "ignoredOptionalDependencies": {
     "description": "A list of optional dependencies that the install should be skipped.",
     "type": "array",
     "items": {
      "type": "string"
     }
    },
    "executionEnv": {
     "type": "object",
     "properties": {
      "nodeVersion": {
       "description": "Specifies which exact Node.js version should be used for the project's runtime.",
       "type": "string"
      }
     },
     "additionalProperties": false
    }
   },
   "additionalProperties": false
  },
  "stackblitz": {
   "description": "Defines the StackBlitz configuration for the project.",
   "type": "object",
   "properties": {
    "installDependencies": {
     "description": "StackBlitz automatically installs npm dependencies when opening a project.",
     "type": "boolean"
    },
    "startCommand": {
     "description": "A terminal command to be executed when opening the project, after installing npm dependencies.",
     "type": [
      "string",
      "boolean"
     ]
    },
    "compileTrigger": {
     "description": "The compileTrigger option controls how file changes in the editor are written to the WebContainers in-memory filesystem. ",
     "oneOf": [
      {
       "type": "string",
       "enum": [
        "auto",
        "keystroke",
        "save"
       ]
      }
     ]
    },
    "env": {
     "description": "A map of default environment variables that will be set in each top-level shell process.",
     "type": "object"
    }
   },
   "additionalProperties": false
  },
  "allowScripts": {
   "description": "Records which dependencies are permitted to run install scripts (preinstall, install, postinstall, and prepare for non-registry sources). Maintained via the \\"npm approve-scripts\\" command, which enforces a default-deny policy: install scripts for any dependency without a matching entry are silently skipped. Keys are package identifiers — either name-only (e.g. \\"pkg\\") or pinned with a version (e.g. \\"pkg@1.2.3\\"); by default entries are pinned so approval is version-specific. A value of true allows the dependency's install scripts to run, while false explicitly denies them (existing false entries are never silently overridden).",
   "type": "object",
   "additionalProperties": {
    "type": "boolean"
   }
  },
  "sideEffects": {
   "description": "Provides hints to the Webpack compiler, denoting which files in your project are \\"pure\\" and therefore safe to prune if unused.",
   "oneOf": [
    {
     "type": "boolean"
    },
    {
     "type": "array",
     "items": {
      "type": "string"
     },
     "uniqueItems": true
    }
   ]
  }
 },
 "$$id": "https://json.schemastore.org/package.json"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}


// eslintrc.json (48585 bytes)
object EslintrcSchema extends Json.Provider(t"""{
 "$$schema": "http://json-schema.org/draft-07/schema#",
 "$$id": "https://json.schemastore.org/eslintrc.json",
 "definitions": {
  "stringOrStringArray": {
   "oneOf": [
    {
     "type": "string"
    },
    {
     "type": "array",
     "items": {
      "type": "string"
     }
    }
   ]
  },
  "rule": {
   "oneOf": [
    {
     "description": "ESLint rule\\n\\n0 - turns the rule off\\n1 - turn the rule on as a warning (doesn't affect exit code)\\n2 - turn the rule on as an error (exit code is 1 when triggered)\\n",
     "type": "integer",
     "minimum": 0,
     "maximum": 2
    },
    {
     "description": "ESLint rule\\n\\n\\"off\\" - turns the rule off\\n\\"warn\\" - turn the rule on as a warning (doesn't affect exit code)\\n\\"error\\" - turn the rule on as an error (exit code is 1 when triggered)\\n",
     "type": "string",
     "enum": [
      "off",
      "warn",
      "error"
     ]
    },
    {
     "type": "array"
    }
   ]
  },
  "possibleErrors": {
   "type": "object",
   "properties": {
    "comma-dangle": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow trailing commas"
    },
    "for-direction": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce \\"for\\" loop update clause moving the counter in the right direction"
    },
    "getter-return": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce return statements in getters"
    },
    "no-await-in-loop": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow await inside of loops"
    },
    "no-compare-neg-zero": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow comparing against -0"
    },
    "no-cond-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow assignment operators in conditional expressions"
    },
    "no-console": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of console"
    },
    "no-constant-condition": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow constant expressions in conditions"
    },
    "no-control-regex": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow control characters in regular expressions"
    },
    "no-debugger": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of debugger"
    },
    "no-dupe-args": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow duplicate arguments in function definitions"
    },
    "no-dupe-keys": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow duplicate keys in object literals"
    },
    "no-duplicate-case": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow duplicate case labels"
    },
    "no-empty": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow empty block statements"
    },
    "no-empty-character-class": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow empty character classes in regular expressions"
    },
    "no-ex-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow reassigning exceptions in catch clauses"
    },
    "no-extra-boolean-cast": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary boolean casts"
    },
    "no-extra-parens": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary parentheses"
    },
    "no-extra-semi": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary semicolons"
    },
    "no-func-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow reassigning function declarations"
    },
    "no-inner-declarations": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow function or var declarations in nested blocks"
    },
    "no-invalid-regexp": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow invalid regular expression strings in RegExp constructors"
    },
    "no-irregular-whitespace": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow irregular whitespace outside of strings and comments"
    },
    "no-negated-in-lhs": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow negating the left operand in in expressions (deprecated)"
    },
    "no-obj-calls": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow calling global object properties as functions"
    },
    "no-prototype-builtins": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow calling some Object.prototype methods directly on objects"
    },
    "no-regex-spaces": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow multiple spaces in regular expressions"
    },
    "no-sparse-arrays": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow sparse arrays"
    },
    "no-template-curly-in-string": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow template literal placeholder syntax in regular strings"
    },
    "no-unexpected-multiline": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow confusing multiline expressions"
    },
    "no-unreachable": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unreachable code after return, throw, continue, and break statements"
    },
    "no-unsafe-finally": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow control flow statements in finally blocks"
    },
    "no-unsafe-negation": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow negating the left operand of relational operators"
    },
    "use-isnan": {
     "$$ref": "#/definitions/rule",
     "description": "Require calls to isNaN() when checking for NaN"
    },
    "valid-jsdoc": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce valid JSDoc comments"
    },
    "valid-typeof": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce comparing typeof expressions against valid strings"
    }
   }
  },
  "bestPractices": {
   "type": "object",
   "properties": {
    "accessor-pairs": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce getter and setter pairs in objects"
    },
    "array-callback-return": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce return statements in callbacks of array methods"
    },
    "block-scoped-var": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce the use of variables within the scope they are defined"
    },
    "class-methods-use-this": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce that class methods utilize this"
    },
    "complexity": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum cyclomatic complexity allowed in a program"
    },
    "consistent-return": {
     "$$ref": "#/definitions/rule",
     "description": "Require return statements to either always or never specify values"
    },
    "curly": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent brace style for all control statements"
    },
    "default-case": {
     "$$ref": "#/definitions/rule",
     "description": "Require default cases in switch statements"
    },
    "dot-location": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent newlines before and after dots"
    },
    "dot-notation": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce dot notation whenever possible"
    },
    "eqeqeq": {
     "$$ref": "#/definitions/rule",
     "description": "Require the use of === and !=="
    },
    "guard-for-in": {
     "$$ref": "#/definitions/rule",
     "description": "Require for-in loops to include an if statement"
    },
    "no-alert": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of alert, confirm, and prompt"
    },
    "no-caller": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of arguments.caller or arguments.callee"
    },
    "no-case-declarations": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow lexical declarations in case clauses"
    },
    "no-div-regex": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow division operators explicitly at the beginning of regular expressions"
    },
    "no-else-return": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow else blocks after return statements in if statements"
    },
    "no-empty-function": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow empty functions"
    },
    "no-empty-pattern": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow empty destructuring patterns"
    },
    "no-eq-null": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow null comparisons without type-checking operators"
    },
    "no-eval": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of eval()"
    },
    "no-extend-native": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow extending native types"
    },
    "no-extra-bind": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary calls to .bind()"
    },
    "no-extra-label": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary labels"
    },
    "no-fallthrough": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow fallthrough of case statements"
    },
    "no-floating-decimal": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow leading or trailing decimal points in numeric literals"
    },
    "no-global-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow assignments to native objects or read-only global variables"
    },
    "no-implicit-coercion": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow shorthand type conversions"
    },
    "no-implicit-globals": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow var and named function declarations in the global scope"
    },
    "no-implied-eval": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of eval()-like methods"
    },
    "no-invalid-this": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow this keywords outside of classes or class-like objects"
    },
    "no-iterator": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of the __iterator__ property"
    },
    "no-labels": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow labeled statements"
    },
    "no-lone-blocks": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary nested blocks"
    },
    "no-loop-func": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow function declarations and expressions inside loop statements"
    },
    "no-magic-numbers": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow magic numbers"
    },
    "no-multi-spaces": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow multiple spaces"
    },
    "no-multi-str": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow multiline strings"
    },
    "no-native-reassign": {
     "$$ref": "#/definitions/rule"
    },
    "no-new": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow new operators outside of assignments or comparisons"
    },
    "no-new-func": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow new operators with the Function object"
    },
    "no-new-wrappers": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow new operators with the String, Number, and Boolean objects"
    },
    "no-octal": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow octal literals"
    },
    "no-octal-escape": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow octal escape sequences in string literals"
    },
    "no-param-reassign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow reassigning function parameters"
    },
    "no-proto": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of the __proto__ property"
    },
    "no-redeclare": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow var redeclaration"
    },
    "no-restricted-properties": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow certain properties on certain objects"
    },
    "no-return-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow assignment operators in return statements"
    },
    "no-return-await": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary return await"
    },
    "no-script-url": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow javascript: urls"
    },
    "no-self-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow assignments where both sides are exactly the same"
    },
    "no-self-compare": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow comparisons where both sides are exactly the same"
    },
    "no-sequences": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow comma operators"
    },
    "no-throw-literal": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow throwing literals as exceptions"
    },
    "no-unmodified-loop-condition": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unmodified loop conditions"
    },
    "no-unused-expressions": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unused expressions"
    },
    "no-unused-labels": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unused labels"
    },
    "no-useless-call": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary calls to .call() and .apply()"
    },
    "no-useless-concat": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary concatenation of literals or template literals"
    },
    "no-useless-escape": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary escape characters"
    },
    "no-useless-return": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow redundant return statements"
    },
    "no-void": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow void operators"
    },
    "no-warning-comments": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow specified warning terms in comments"
    },
    "no-with": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow with statements"
    },
    "prefer-promise-reject-errors": {
     "$$ref": "#/definitions/rule",
     "description": "Require using Error objects as Promise rejection reasons"
    },
    "radix": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce the consistent use of the radix argument when using parseInt()"
    },
    "require-await": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow async functions which have no await expression"
    },
    "vars-on-top": {
     "$$ref": "#/definitions/rule",
     "description": "Require var declarations be placed at the top of their containing scope"
    },
    "wrap-iife": {
     "$$ref": "#/definitions/rule",
     "description": "Require parentheses around immediate function invocations"
    },
    "yoda": {
     "$$ref": "#/definitions/rule",
     "description": "Require or Disallow \\"Yoda\\" conditions"
    }
   }
  },
  "strictMode": {
   "type": "object",
   "properties": {
    "strict": {
     "$$ref": "#/definitions/rule",
     "description": "require or disallow strict mode directives"
    }
   }
  },
  "variables": {
   "type": "object",
   "properties": {
    "init-declarations": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow initialization in var declarations"
    },
    "no-catch-shadow": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow catch clause parameters from shadowing variables in the outer scope"
    },
    "no-delete-var": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow deleting variables"
    },
    "no-label-var": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow labels that share a name with a variable"
    },
    "no-restricted-globals": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow specified global variables"
    },
    "no-shadow": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow var declarations from shadowing variables in the outer scope"
    },
    "no-shadow-restricted-names": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow identifiers from shadowing restricted names"
    },
    "no-undef": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of undeclared variables unless mentioned in /*global */ comments"
    },
    "no-undefined": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of undefined as an identifier"
    },
    "no-undef-init": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow initializing variables to undefined"
    },
    "no-unused-vars": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unused variables"
    },
    "no-use-before-define": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of variables before they are defined"
    }
   }
  },
  "nodeAndCommonJs": {
   "type": "object",
   "properties": {
    "callback-return": {
     "$$ref": "#/definitions/rule",
     "description": "Require return statements after callbacks"
    },
    "global-require": {
     "$$ref": "#/definitions/rule",
     "description": "Require require() calls to be placed at top-level module scope"
    },
    "handle-callback-err": {
     "$$ref": "#/definitions/rule",
     "description": "Require error handling in callbacks"
    },
    "no-buffer-constructor": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow use of the Buffer() constructor"
    },
    "no-mixed-requires": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow require calls to be mixed with regular var declarations"
    },
    "no-new-require": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow new operators with calls to require"
    },
    "no-path-concat": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow string concatenation with __dirname and __filename"
    },
    "no-process-env": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of process.env"
    },
    "no-process-exit": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the use of process.exit()"
    },
    "no-restricted-modules": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow specified modules when loaded by require"
    },
    "no-sync": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow synchronous methods"
    }
   }
  },
  "stylisticIssues": {
   "type": "object",
   "properties": {
    "array-bracket-newline": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce line breaks after opening and before closing array brackets"
    },
    "array-bracket-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing inside array brackets"
    },
    "array-element-newline": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce line breaks after each array element"
    },
    "block-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing inside single-line blocks"
    },
    "brace-style": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent brace style for blocks"
    },
    "camelcase": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce camelcase naming convention"
    },
    "capitalized-comments": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce or disallow capitalization of the first letter of a comment"
    },
    "comma-dangle": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow trailing commas"
    },
    "comma-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing before and after commas"
    },
    "comma-style": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent comma style"
    },
    "computed-property-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing inside computed property brackets"
    },
    "consistent-this": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent naming when capturing the current execution context"
    },
    "eol-last": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce at least one newline at the end of files"
    },
    "func-call-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow spacing between function identifiers and their invocations"
    },
    "func-name-matching": {
     "$$ref": "#/definitions/rule",
     "description": "Require function names to match the name of the variable or property to which they are assigned"
    },
    "func-names": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow named function expressions"
    },
    "func-style": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce the consistent use of either function declarations or expressions"
    },
    "function-call-argument-newline": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce line breaks between arguments of a function call"
    },
    "function-paren-newline": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent line breaks inside function parentheses"
    },
    "id-blacklist": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow specified identifiers"
    },
    "id-length": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce minimum and maximum identifier lengths"
    },
    "id-match": {
     "$$ref": "#/definitions/rule",
     "description": "Require identifiers to match a specified regular expression"
    },
    "implicit-arrow-linebreak": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce the location of arrow function bodies"
    },
    "indent": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent indentation"
    },
    "indent-legacy": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent indentation (legacy, deprecated)"
    },
    "jsx-quotes": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce the consistent use of either double or single quotes in JSX attributes"
    },
    "key-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing between keys and values in object literal properties"
    },
    "keyword-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing before and after keywords"
    },
    "line-comment-position": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce position of line comments"
    },
    "lines-between-class-members": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow an empty line between class members"
    },
    "linebreak-style": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent linebreak style"
    },
    "lines-around-comment": {
     "$$ref": "#/definitions/rule",
     "description": "Require empty lines around comments"
    },
    "lines-around-directive": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow newlines around directives"
    },
    "max-depth": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum depth that blocks can be nested"
    },
    "max-len": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum line length"
    },
    "max-lines": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum number of lines per file"
    },
    "max-nested-callbacks": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum depth that callbacks can be nested"
    },
    "max-params": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum number of parameters in function definitions"
    },
    "max-statements": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum number of statements allowed in function blocks"
    },
    "max-statements-per-line": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a maximum number of statements allowed per line"
    },
    "multiline-comment-style": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce a particular style for multiline comments"
    },
    "multiline-ternary": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce newlines between operands of ternary expressions"
    },
    "new-cap": {
     "$$ref": "#/definitions/rule",
     "description": "Require constructor function names to begin with a capital letter"
    },
    "newline-after-var": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow an empty line after var declarations"
    },
    "newline-before-return": {
     "$$ref": "#/definitions/rule",
     "description": "Require an empty line before return statements"
    },
    "newline-per-chained-call": {
     "$$ref": "#/definitions/rule",
     "description": "Require a newline after each call in a method chain"
    },
    "new-parens": {
     "$$ref": "#/definitions/rule",
     "description": "Require parentheses when invoking a constructor with no arguments"
    },
    "no-array-constructor": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow Array constructors"
    },
    "no-bitwise": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow bitwise operators"
    },
    "no-continue": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow continue statements"
    },
    "no-inline-comments": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow inline comments after code"
    },
    "no-lonely-if": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow if statements as the only statement in else blocks"
    },
    "no-mixed-operators": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow mixed binary operators"
    },
    "no-mixed-spaces-and-tabs": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow mixed spaces and tabs for indentation"
    },
    "no-multi-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow use of chained assignment expressions"
    },
    "no-multiple-empty-lines": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow multiple empty lines"
    },
    "no-negated-condition": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow negated conditions"
    },
    "no-nested-ternary": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow nested ternary expressions"
    },
    "no-new-object": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow Object constructors"
    },
    "no-plusplus": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow the unary operators ++ and --"
    },
    "no-restricted-syntax": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow specified syntax"
    },
    "no-spaced-func": {
     "$$ref": "#/definitions/rule"
    },
    "no-tabs": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow tabs in file"
    },
    "no-ternary": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow ternary operators"
    },
    "no-trailing-spaces": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow trailing whitespace at the end of lines"
    },
    "no-underscore-dangle": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow dangling underscores in identifiers"
    },
    "no-unneeded-ternary": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow ternary operators when simpler alternatives exist"
    },
    "no-whitespace-before-property": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow whitespace before properties"
    },
    "nonblock-statement-body-position": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce the location of single-line statements"
    },
    "object-curly-newline": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent line breaks inside braces"
    },
    "object-curly-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing inside braces"
    },
    "object-property-newline": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce placing object properties on separate lines"
    },
    "object-shorthand": {
     "$$ref": "#/definitions/rule"
    },
    "one-var": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce variables to be declared either together or separately in functions"
    },
    "one-var-declaration-per-line": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow newlines around var declarations"
    },
    "operator-assignment": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow assignment operator shorthand where possible"
    },
    "operator-linebreak": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent linebreak style for operators"
    },
    "padded-blocks": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow padding within blocks"
    },
    "padding-line-between-statements": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow padding lines between statements"
    },
    "quote-props": {
     "$$ref": "#/definitions/rule",
     "description": "Require quotes around object literal property names"
    },
    "quotes": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce the consistent use of either backticks, double, or single quotes"
    },
    "require-jsdoc": {
     "$$ref": "#/definitions/rule",
     "description": "Require JSDoc comments"
    },
    "semi": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow semicolons instead of ASI"
    },
    "semi-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing before and after semicolons"
    },
    "semi-style": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce location of semicolons"
    },
    "sort-keys": {
     "$$ref": "#/definitions/rule",
     "description": "Requires object keys to be sorted"
    },
    "sort-vars": {
     "$$ref": "#/definitions/rule",
     "description": "Require variables within the same declaration block to be sorted"
    },
    "space-before-blocks": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing before blocks"
    },
    "space-before-function-paren": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing before function definition opening parenthesis"
    },
    "spaced-comment": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing after the // or /* in a comment"
    },
    "space-infix-ops": {
     "$$ref": "#/definitions/rule",
     "description": "Require spacing around operators"
    },
    "space-in-parens": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing inside parentheses"
    },
    "space-unary-ops": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing before or after unary operators"
    },
    "switch-colon-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce spacing around colons of switch statements"
    },
    "template-tag-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow spacing between template tags and their literals"
    },
    "unicode-bom": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow Unicode byte order mark (BOM)"
    },
    "wrap-regex": {
     "$$ref": "#/definitions/rule",
     "description": "Require parenthesis around regex literals"
    }
   }
  },
  "ecmaScript6": {
   "type": "object",
   "properties": {
    "arrow-body-style": {
     "$$ref": "#/definitions/rule",
     "description": "Require braces around arrow function bodies"
    },
    "arrow-parens": {
     "$$ref": "#/definitions/rule",
     "description": "Require parentheses around arrow function arguments"
    },
    "arrow-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing before and after the arrow in arrow functions"
    },
    "constructor-super": {
     "$$ref": "#/definitions/rule",
     "description": "Require super() calls in constructors"
    },
    "generator-star-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce consistent spacing around * operators in generator functions"
    },
    "no-class-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow reassigning class members"
    },
    "no-confusing-arrow": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow arrow functions where they could be confused with comparisons"
    },
    "no-const-assign": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow reassigning const variables"
    },
    "no-dupe-class-members": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow duplicate class members"
    },
    "no-duplicate-imports": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow duplicate module imports"
    },
    "no-new-symbol": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow new operators with the Symbol object"
    },
    "no-restricted-imports": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow specified modules when loaded by import"
    },
    "no-this-before-super": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow this/super before calling super() in constructors"
    },
    "no-useless-computed-key": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary computed property keys in object literals"
    },
    "no-useless-constructor": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow unnecessary constructors"
    },
    "no-useless-rename": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow renaming import, export, and destructured assignments to the same name"
    },
    "no-var": {
     "$$ref": "#/definitions/rule",
     "description": "Require let or const instead of var"
    },
    "object-shorthand": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow method and property shorthand syntax for object literals"
    },
    "prefer-arrow-callback": {
     "$$ref": "#/definitions/rule",
     "description": "Require arrow functions as callbacks"
    },
    "prefer-const": {
     "$$ref": "#/definitions/rule",
     "description": "Require const declarations for variables that are never reassigned after declared"
    },
    "prefer-destructuring": {
     "$$ref": "#/definitions/rule",
     "description": "Require destructuring from arrays and/or objects"
    },
    "prefer-numeric-literals": {
     "$$ref": "#/definitions/rule",
     "description": "Disallow parseInt() in favor of binary, octal, and hexadecimal literals"
    },
    "prefer-reflect": {
     "$$ref": "#/definitions/rule",
     "description": "Require Reflect methods where applicable"
    },
    "prefer-rest-params": {
     "$$ref": "#/definitions/rule",
     "description": "Require rest parameters instead of arguments"
    },
    "prefer-spread": {
     "$$ref": "#/definitions/rule",
     "description": "Require spread operators instead of .apply()"
    },
    "prefer-template": {
     "$$ref": "#/definitions/rule",
     "description": "Require template literals instead of string concatenation"
    },
    "require-yield": {
     "$$ref": "#/definitions/rule",
     "description": "Require generator functions to contain yield"
    },
    "rest-spread-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce spacing between rest and spread operators and their expressions"
    },
    "sort-imports": {
     "$$ref": "#/definitions/rule",
     "description": "Enforce sorted import declarations within modules"
    },
    "symbol-description": {
     "$$ref": "#/definitions/rule",
     "description": "Require symbol descriptions"
    },
    "template-curly-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow spacing around embedded expressions of template strings"
    },
    "yield-star-spacing": {
     "$$ref": "#/definitions/rule",
     "description": "Require or disallow spacing around the * in yield* expressions"
    }
   }
  },
  "legacy": {
   "type": "object",
   "properties": {
    "max-depth": {
     "$$ref": "#/definitions/rule"
    },
    "max-len": {
     "$$ref": "#/definitions/rule"
    },
    "max-params": {
     "$$ref": "#/definitions/rule"
    },
    "max-statements": {
     "$$ref": "#/definitions/rule"
    },
    "no-bitwise": {
     "$$ref": "#/definitions/rule"
    },
    "no-plusplus": {
     "$$ref": "#/definitions/rule"
    }
   }
  }
 },
 "properties": {
  "ecmaFeatures": {
   "description": "By default, ESLint supports only ECMAScript 5 syntax. You can override that setting to enable support for ECMAScript 6 as well as JSX by using configuration settings.",
   "type": "object",
   "properties": {
    "arrowFunctions": {
     "type": "boolean"
    },
    "binaryLiterals": {
     "type": "boolean"
    },
    "blockBindings": {
     "type": "boolean"
    },
    "classes": {
     "type": "boolean"
    },
    "defaultParams": {
     "type": "boolean"
    },
    "destructuring": {
     "type": "boolean"
    },
    "experimentalObjectRestSpread": {
     "type": "boolean",
     "description": "Enables support for the experimental object rest/spread properties (IMPORTANT: This is an experimental feature that may change significantly in the future. It's recommended that you do not write rules relying on this functionality unless you are willing to incur maintenance cost when it changes.)"
    },
    "forOf": {
     "type": "boolean"
    },
    "generators": {
     "type": "boolean"
    },
    "globalReturn": {
     "type": "boolean",
     "description": "allow return statements in the global scope"
    },
    "impliedStrict": {
     "type": "boolean",
     "description": "enable global strict mode (if ecmaVersion is 5 or greater)"
    },
    "jsx": {
     "type": "boolean",
     "description": "enable JSX"
    },
    "modules": {
     "type": "boolean"
    },
    "objectLiteralComputedProperties": {
     "type": "boolean"
    },
    "objectLiteralDuplicateProperties": {
     "type": "boolean"
    },
    "objectLiteralShorthandMethods": {
     "type": "boolean"
    },
    "objectLiteralShorthandProperties": {
     "type": "boolean"
    },
    "octalLiterals": {
     "type": "boolean"
    },
    "regexUFlag": {
     "type": "boolean"
    },
    "regexYFlag": {
     "type": "boolean"
    },
    "restParams": {
     "type": "boolean"
    },
    "spread": {
     "type": "boolean"
    },
    "superInFunctions": {
     "type": "boolean"
    },
    "templateStrings": {
     "type": "boolean"
    },
    "unicodeCodePointEscapes": {
     "type": "boolean"
    }
   }
  },
  "env": {
   "description": "An environment defines global variables that are predefined.",
   "type": "object",
   "properties": {
    "amd": {
     "type": "boolean",
     "description": "defines require() and define() as global variables as per the amd spec"
    },
    "applescript": {
     "type": "boolean",
     "description": "AppleScript global variables"
    },
    "atomtest": {
     "type": "boolean",
     "description": "Atom test helper globals"
    },
    "browser": {
     "type": "boolean",
     "description": "browser global variables"
    },
    "commonjs": {
     "type": "boolean",
     "description": "CommonJS global variables and CommonJS scoping (use this for browser-only code that uses Browserify/WebPack)"
    },
    "shared-node-browser": {
     "type": "boolean",
     "description": "Globals common to both Node and Browser"
    },
    "embertest": {
     "type": "boolean",
     "description": "Ember test helper globals"
    },
    "es6": {
     "type": "boolean",
     "description": "enable all ECMAScript 6 features except for modules"
    },
    "greasemonkey": {
     "type": "boolean",
     "description": "GreaseMonkey globals"
    },
    "jasmine": {
     "type": "boolean",
     "description": "adds all of the Jasmine testing global variables for version 1.3 and 2.0"
    },
    "jest": {
     "type": "boolean",
     "description": "Jest global variables"
    },
    "jquery": {
     "type": "boolean",
     "description": "jQuery global variables"
    },
    "meteor": {
     "type": "boolean",
     "description": "Meteor global variables"
    },
    "mocha": {
     "type": "boolean",
     "description": "adds all of the Mocha test global variables"
    },
    "mongo": {
     "type": "boolean",
     "description": "MongoDB global variables"
    },
    "nashorn": {
     "type": "boolean",
     "description": "Java 8 Nashorn global variables"
    },
    "node": {
     "type": "boolean",
     "description": "Node.js global variables and Node.js scoping"
    },
    "phantomjs": {
     "type": "boolean",
     "description": "PhantomJS global variables"
    },
    "prototypejs": {
     "type": "boolean",
     "description": "Prototype.js global variables"
    },
    "protractor": {
     "type": "boolean",
     "description": "Protractor global variables"
    },
    "qunit": {
     "type": "boolean",
     "description": "QUnit global variables"
    },
    "serviceworker": {
     "type": "boolean",
     "description": "Service Worker global variables"
    },
    "shelljs": {
     "type": "boolean",
     "description": "ShellJS global variables"
    },
    "webextensions": {
     "type": "boolean",
     "description": "WebExtensions globals"
    },
    "worker": {
     "type": "boolean",
     "description": "web workers global variables"
    }
   }
  },
  "extends": {
   "$$ref": "#/definitions/stringOrStringArray",
   "description": "If you want to extend a specific configuration file, you can use the extends property and specify the path to the file. The path can be either relative or absolute."
  },
  "globals": {
   "description": "Set each global variable name equal to true to allow the variable to be overwritten or false to disallow overwriting.",
   "type": "object",
   "additionalProperties": {
    "oneOf": [
     {
      "type": "string",
      "enum": [
       "readonly",
       "writable",
       "off"
      ]
     },
     {
      "description": "The values false|\\"readable\\" and true|\\"writeable\\" are deprecated, they are equivalent to \\"readonly\\" and \\"writable\\", respectively.",
      "type": "boolean"
     }
    ]
   }
  },
  "noInlineConfig": {
   "description": "Prevent comments from changing config or rules",
   "type": "boolean"
  },
  "reportUnusedDisableDirectives": {
   "description": "Report unused eslint-disable comments",
   "type": "boolean"
  },
  "parser": {
   "type": "string"
  },
  "parserOptions": {
   "description": "The JavaScript language options to be supported",
   "type": "object",
   "properties": {
    "ecmaFeatures": {
     "$$ref": "#/properties/ecmaFeatures"
    },
    "ecmaVersion": {
     "enum": [
      3,
      5,
      6,
      2015,
      7,
      2016,
      8,
      2017,
      9,
      2018,
      10,
      2019,
      11,
      2020,
      12,
      2021,
      13,
      2022,
      14,
      2023,
      15,
      2024,
      "latest"
     ],
     "default": 5,
     "description": "Set to 3, 5 (default), 6, 7, 8, 9, 10, 11, 12, 13, 14, or 15 to specify the version of ECMAScript syntax you want to use. You can also set it to 2015 (same as 6), 2016 (same as 7), 2017 (same as 8), 2018 (same as 9), 2019 (same as 10), 2020 (same as 11), 2021 (same as 12), 2022 (same as 13), 2023 (same as 14), or 2024 (same as 15) to use the year-based naming. You can also set \\"latest\\" to use the most recently supported version."
    },
    "sourceType": {
     "enum": [
      "script",
      "module",
      "commonjs"
     ],
     "default": "script",
     "description": "set to \\"script\\" (default), \\"commonjs\\", or \\"module\\" if your code is in ECMAScript modules"
    }
   }
  },
  "plugins": {
   "description": "ESLint supports the use of third-party plugins. Before using the plugin, you have to install it using npm.",
   "type": "array",
   "items": {
    "type": "string"
   }
  },
  "root": {
   "description": "By default, ESLint will look for configuration files in all parent folders up to the root directory. This can be useful if you want all of your projects to follow a certain convention, but can sometimes lead to unexpected results. To limit ESLint to a specific project, set this to `true` in a configuration in the root of your project.",
   "type": "boolean"
  },
  "ignorePatterns": {
   "$$ref": "#/definitions/stringOrStringArray",
   "description": "Tell ESLint to ignore specific files and directories. Each value uses the same pattern as the `.eslintignore` file."
  },
  "rules": {
   "description": "ESLint comes with a large number of rules. You can modify which rules your project uses either using configuration comments or configuration files.",
   "type": "object",
   "allOf": [
    {
     "$$ref": "#/definitions/possibleErrors"
    },
    {
     "$$ref": "#/definitions/bestPractices"
    },
    {
     "$$ref": "#/definitions/strictMode"
    },
    {
     "$$ref": "#/definitions/variables"
    },
    {
     "$$ref": "#/definitions/nodeAndCommonJs"
    },
    {
     "$$ref": "#/definitions/stylisticIssues"
    },
    {
     "$$ref": "#/definitions/ecmaScript6"
    },
    {
     "$$ref": "#/definitions/legacy"
    },
    {
     "$$ref": "partial-eslint-plugins.json"
    }
   ]
  },
  "settings": {
   "description": "ESLint supports adding shared settings into configuration file. You can add settings object to ESLint configuration file and it will be supplied to every rule that will be executed. This may be useful if you are adding custom rules and want them to have access to the same information and be easily configurable.",
   "type": "object"
  },
  "overrides": {
   "type": "array",
   "description": "Allows to override configuration for files and folders, specified by glob patterns",
   "items": {
    "type": "object",
    "properties": {
     "files": {
      "description": "Glob pattern for files to apply 'overrides' configuration, relative to the directory of the config file",
      "oneOf": [
       {
        "type": "string"
       },
       {
        "minItems": 1,
        "type": "array",
        "items": {
         "type": "string"
        }
       }
      ]
     },
     "extends": {
      "$$ref": "#/definitions/stringOrStringArray",
      "description": "If you want to extend a specific configuration file, you can use the extends property and specify the path to the file. The path can be either relative or absolute."
     },
     "excludedFiles": {
      "$$ref": "#/definitions/stringOrStringArray",
      "description": "If a file matches any of the 'excludedFiles' glob patterns, the 'overrides' configuration won't apply"
     },
     "ecmaFeatures": {
      "$$ref": "#/properties/ecmaFeatures"
     },
     "env": {
      "$$ref": "#/properties/env"
     },
     "globals": {
      "$$ref": "#/properties/globals"
     },
     "parser": {
      "$$ref": "#/properties/parser"
     },
     "parserOptions": {
      "$$ref": "#/properties/parserOptions"
     },
     "plugins": {
      "$$ref": "#/properties/plugins"
     },
     "processor": {
      "description": "To specify a processor, specify the plugin name and processor name joined by a forward slash",
      "type": "string"
     },
     "rules": {
      "$$ref": "#/properties/rules"
     },
     "settings": {
      "$$ref": "#/properties/settings"
     },
     "overrides": {
      "$$ref": "#/properties/overrides"
     }
    },
    "additionalProperties": false,
    "required": [
     "files"
    ]
   }
  }
 },
 "title": "JSON schema for ESLint configuration files",
 "type": "object"
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}
