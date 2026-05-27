#!/usr/bin/env python3
"""One-time JSON → TEL conversion for stratiform's parser benchmarks.

Mirrors the eight JSON samples from `jacinta.Benchmarks`, converting each
to a TEL document plus an inferred TEL schema (in tel-schema source form
so the runtime `Tels.Reconstructor` can reload it).  Writes the resulting
pairs to `lib/stratiform/res/bench/stratiform/`.

JSON → TEL conventions
----------------------
- A JSON object's keys become child compounds, kebab-cased.
- A primitive value becomes the keyword's first inline atom; atom form
  follows §22.3 escalation (inline → source → literal).
- A JSON array nested under a key becomes a repeated compound — one
  entry per element, all sharing the key as their keyword.
- A top-level JSON array is flattened: each element becomes a top-level
  `item` compound.

Schema inference
----------------
Walk the converted TEL and produce a `tel-schema` document whose
`document` block lists one Field per unique top-level keyword.  Repeated
keys get `repeatable optional`; absent keys (in any element of an array
of objects) get `optional`.  Nested object structure becomes a fresh
Record per object shape.  Numeric, boolean and string values all map to
the built-in `String` scalar (the parser's behaviour is identical for
all atom-text values; specialised scalar types are a follow-up).
"""

import json
import re
import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUTDIR = ROOT / "lib" / "stratiform" / "res" / "bench" / "stratiform"


# ── JSON example generators (mirror jacinta.Benchmarks) ──

JSON_EXAMPLE_1 = """{"web-app": {
  "servlet": [
    {
      "servlet-name": "cofaxCDS",
      "servlet-class": "org.cofax.cds.CDSServlet",
      "init-param": {
        "configGlossary:installationAt": "Philadelphia, PA",
        "configGlossary:adminEmail": "ksm@pobox.com",
        "configGlossary:poweredBy": "Cofax",
        "configGlossary:poweredByIcon": "/images/cofax.gif",
        "configGlossary:staticPath": "/content/static",
        "templateProcessorClass": "org.cofax.WysiwygTemplate",
        "templateLoaderClass": "org.cofax.FilesTemplateLoader",
        "templatePath": "templates",
        "templateOverridePath": "",
        "defaultListTemplate": "listTemplate.htm",
        "defaultFileTemplate": "articleTemplate.htm",
        "useJSP": false,
        "jspListTemplate": "listTemplate.jsp",
        "jspFileTemplate": "articleTemplate.jsp",
        "cachePackageTagsTrack": 200,
        "cachePackageTagsStore": 200,
        "cachePackageTagsRefresh": 60,
        "cacheTemplatesTrack": 100,
        "cacheTemplatesStore": 50,
        "cacheTemplatesRefresh": 15,
        "cachePagesTrack": 200,
        "cachePagesStore": 100,
        "cachePagesRefresh": 10,
        "cachePagesDirtyRead": 10,
        "searchEngineListTemplate": "forSearchEnginesList.htm",
        "searchEngineFileTemplate": "forSearchEngines.htm",
        "searchEngineRobotsDb": "WEB-INF/robots.db",
        "useDataStore": true,
        "dataStoreClass": "org.cofax.SqlDataStore",
        "redirectionClass": "org.cofax.SqlRedirection",
        "dataStoreName": "cofax",
        "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
        "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
        "dataStoreUser": "sa",
        "dataStorePassword": "dataStoreTestQuery",
        "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
        "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
        "dataStoreInitConns": 10,
        "dataStoreMaxConns": 100,
        "dataStoreConnUsageLimit": 100,
        "dataStoreLogLevel": "debug",
        "maxUrlLength": 500}},
    {
      "servlet-name": "cofaxEmail",
      "servlet-class": "org.cofax.cds.EmailServlet",
      "init-param": {
      "mailHost": "mail1",
      "mailHostOverride": "mail2"}},
    {
      "servlet-name": "cofaxAdmin",
      "servlet-class": "org.cofax.cds.AdminServlet"},

    {
      "servlet-name": "fileServlet",
      "servlet-class": "org.cofax.cds.FileServlet"},
    {
      "servlet-name": "cofaxTools",
      "servlet-class": "org.cofax.cms.CofaxToolsServlet",
      "init-param": {
        "templatePath": "toolstemplates/",
        "log": 1,
        "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
        "logMaxSize": "",
        "dataLog": 1,
        "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
        "dataLogMaxSize": "",
        "removePageCache": "/content/admin/remove?cache=pages&id=",
        "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
        "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
        "lookInContext": 1,
        "adminGroupID": 4,
        "betaServer": true}}],
  "servlet-mapping": {
    "cofaxCDS": "/",
    "cofaxEmail": "/cofaxutil/aemail/*",
    "cofaxAdmin": "/admin/*",
    "fileServlet": "/static/*",
    "cofaxTools": "/tools/*"},

  "taglib": {
    "taglib-uri": "cofax.tld",
    "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}
"""

JSON_EXAMPLE_2 = """{"menu":{"id":"file","value":"File","popup":{"menuitem":[{"value":"New","onclick":"CreateNewDoc()"},
{"value":"Open","onclick":"OpenDoc()"},{"value":"Close","onclick":"CloseDoc()"}]}}}
"""

JSON_EXAMPLE_3 = """{"menu": {
  "header": "SVG Viewer",
    "items": [
        {"id": "Open"},
        {"id": "OpenNew", "label": "Open New"},
        null,
        {"id": "ZoomIn", "label": "Zoom In"},
        {"id": "ZoomOut", "label": "Zoom Out"},
        {"id": "OriginalView", "label": "Original View"},
        null,
        {"id": "Quality"},
        {"id": "Pause"},
        {"id": "Mute"},
        null,
        {"id": "Find", "label": "Find..."},
        {"id": "FindAgain", "label": "Find Again"},
        {"id": "Copy"},
        {"id": "CopyAgain", "label": "Copy Again"},
        {"id": "CopySVG", "label": "Copy SVG"},
        {"id": "ViewSVG", "label": "View SVG"},
        {"id": "ViewSource", "label": "View Source"},
        {"id": "SaveAs", "label": "Save As"},
        null,
        {"id": "Help"},
        {"id": "About", "label": "About Adobe CVG Viewer..."}
    ]
}}
"""


def gen_example_4():
    parts = ['{"users":[']
    for i in range(100):
        if i > 0:
            parts.append(",")
        active = "true" if (i & 1) == 0 else "false"
        role = "admin" if i % 10 == 0 else "user"
        parts.append(f'{{"id":{i},"username":"user{i}","email":"user{i}@example.com",')
        parts.append(f'"active":{active},"role":"{role}"}}')
    parts.append("]}")
    return "".join(parts)


def gen_example_5():
    parts = ['{"logs":[']
    levels = ["info", "debug", "warn", "error"]
    services = ["auth", "api", "db", "cache", "worker"]
    for i in range(500):
        if i > 0:
            parts.append(",")
        ts = 1700000000 + i
        level = levels[i & 3]
        service = services[i % 5]
        user_id = 1000 + (i % 50)
        parts.append(f'{{"timestamp":{ts},"level":"{level}","service":"{service}",')
        parts.append(f'"requestId":"req-{i}","userId":{user_id},"message":"event {i} processed"}}')
    parts.append("]}")
    return "".join(parts)


def gen_example_6():
    parts = ['{"transactions":[']
    for i in range(50):
        if i > 0:
            parts.append(",")
        nonce = i
        block_number = 18500000 + i
        gas_used = 21000 + i * 100
        value_wei = f"12345678901234567890{1000 + i}"
        value_eth = f"1234567890.12345678901{i % 10}"
        gas_price_wei = f"30000000000.0123456789{i % 10}"
        temperature = f"2.7345678901234567890{i % 10}e-3"
        parts.append(f'{{"from":"0xabcdef{i}","to":"0x123456{i}","value":{value_wei},')
        parts.append(f'"valueEth":{value_eth},"gasPriceWei":{gas_price_wei},"gasUsed":{gas_used},')
        parts.append(f'"blockNumber":{block_number},"nonce":{nonce},"temperatureDelta":{temperature}}}')
    parts.append("]}")
    return "".join(parts)


def gen_example_7():
    return "[" + ",".join(str(i * 37 + 1) for i in range(1000)) + "]"


def gen_example_8():
    return "[" + ",".join(f"{i*7+1}.{(i*13+1)%1000}" for i in range(1000)) + "]"


EXAMPLES = [
    ("example1", JSON_EXAMPLE_1),
    ("example2", JSON_EXAMPLE_2),
    ("example3", JSON_EXAMPLE_3),
    ("example4", gen_example_4()),
    ("example5", gen_example_5()),
    ("example6", gen_example_6()),
    ("example7", gen_example_7()),
    ("example8", gen_example_8()),
]


# ── JSON → TEL conversion ──

_KEBAB_NEEDS_HYPHEN = re.compile(r"[A-Z]")


def to_kebab(s):
    """Convert any JSON key to a TEL-safe kebab-case identifier.

    - camelCase → kebab-case (insert `-` before each interior uppercase)
    - any non-alphanumeric / non-hyphen char is replaced with `-`
    - collapsed runs of `-`, trimmed.
    """
    # camelCase split: insert `-` before uppercase letters then lowercase
    out = []
    for i, c in enumerate(s):
        if c.isupper() and i > 0 and (s[i - 1].islower() or s[i - 1].isdigit()):
            out.append("-")
        out.append(c.lower() if c.isupper() else c)
    intermediate = "".join(out)
    # Replace any non-[a-z0-9-] character with '-'
    cleaned = re.sub(r"[^a-z0-9-]+", "-", intermediate)
    # Collapse multiple hyphens and trim
    cleaned = re.sub(r"-+", "-", cleaned).strip("-")
    return cleaned or "value"


def atom_form(value):
    """Pick inline / source / literal per §22.3 escalation."""
    if "\n" not in value:
        # Inline candidate: no leading/trailing space, no `  ` run, no ` #`
        if (value
                and not value.startswith(" ")
                and not value.endswith(" ")
                and "  " not in value
                and " #" not in value):
            return ("inline", value)
    # Source candidate: no trailing-space line, no blank line
    lines = value.split("\n")
    has_trailing_space = any(line.endswith(" ") for line in lines)
    has_blank = "\n\n" in value
    if not has_trailing_space and not has_blank:
        return ("source", value)
    return ("literal", value)


def fmt_atom(form_value, indent_spaces):
    form, value = form_value
    if form == "inline":
        return " " + value
    indent = " " * indent_spaces
    if form == "source":
        # Each line indented at indent+4, joined with LF; trailing LF kept.
        body_indent = " " * (indent_spaces + 4)
        body = "\n".join(body_indent + line for line in value.split("\n"))
        return "\n" + body
    # literal
    body_indent = " " * (indent_spaces + 6)
    body = "\n".join(body_indent + line for line in value.rstrip("\n").split("\n"))
    return "\n" + body_indent + "---\n" + body + "\n---"


def value_to_atom(v):
    """Stringify a JSON primitive for use as a TEL atom value."""
    if v is True:
        return "true"
    if v is False:
        return "false"
    if v is None:
        return "null"
    if isinstance(v, (int, float)):
        return str(v)
    return str(v)


def is_primitive(v):
    return v is None or isinstance(v, (bool, int, float, str))


def emit_object(obj, indent):
    """Emit a JSON object as TEL compounds (one per key) at indent level."""
    lines = []
    pad = "  " * indent
    for key, value in obj.items():
        keyword = to_kebab(key)
        if value is None:
            lines.append(f"{pad}{keyword}")
        elif is_primitive(value):
            atom = atom_form(value_to_atom(value))
            lines.append(f"{pad}{keyword}{fmt_atom(atom, indent * 2)}")
        elif isinstance(value, list):
            # Each element becomes a repeated compound with `keyword`.
            for element in value:
                if element is None:
                    lines.append(f"{pad}{keyword}")
                elif is_primitive(element):
                    atom = atom_form(value_to_atom(element))
                    lines.append(f"{pad}{keyword}{fmt_atom(atom, indent * 2)}")
                elif isinstance(element, dict):
                    lines.append(f"{pad}{keyword}")
                    lines.extend(emit_object(element, indent + 1))
                elif isinstance(element, list):
                    lines.append(f"{pad}{keyword}")
                    lines.extend(emit_array_at(element, indent + 1))
        elif isinstance(value, dict):
            lines.append(f"{pad}{keyword}")
            lines.extend(emit_object(value, indent + 1))
    return lines


def emit_array_at(arr, indent):
    """A non-top-level array used as a value: emit each element as `item …`."""
    lines = []
    pad = "  " * indent
    for el in arr:
        if el is None:
            lines.append(f"{pad}item")
        elif is_primitive(el):
            atom = atom_form(value_to_atom(el))
            lines.append(f"{pad}item{fmt_atom(atom, indent * 2)}")
        elif isinstance(el, dict):
            lines.append(f"{pad}item")
            lines.extend(emit_object(el, indent + 1))
        elif isinstance(el, list):
            lines.append(f"{pad}item")
            lines.extend(emit_array_at(el, indent + 1))
    return lines


def json_to_tel(data):
    if isinstance(data, dict):
        body = emit_object(data, 0)
    elif isinstance(data, list):
        body = emit_array_at(data, 0)
    else:
        body = ["value" + fmt_atom(atom_form(value_to_atom(data)), 0)]
    return "\n".join(body) + "\n"


# ── Schema inference ──

class SchemaShape:
    """Accumulator for the structural shape observed at a given path."""

    def __init__(self):
        self.fields = {}        # keyword → SchemaShape
        self.is_scalar = False  # True if any value at this position was primitive
        self.is_array = False   # True if observed as an array element
        self.repeated = False   # True if multiple occurrences observed under one parent

    def merge_value(self, value, occurrence_counts):
        if isinstance(value, dict):
            for k, v in value.items():
                kw = to_kebab(k)
                occurrence_counts[kw] = occurrence_counts.get(kw, 0) + 1
                if kw not in self.fields:
                    self.fields[kw] = SchemaShape()
                self.fields[kw].observe(v)
        elif isinstance(value, list):
            self.is_array = True
            for el in value:
                self.observe_element(el)
        elif is_primitive(value):
            self.is_scalar = True

    def observe(self, value):
        """Observe a single value (under a key)."""
        if isinstance(value, dict):
            counts = {}
            self.merge_value(value, counts)
            for kw, n in counts.items():
                if n > 1:
                    self.fields[kw].repeated = True
        elif isinstance(value, list):
            self.is_array = True
            # Treat each array element as another observation under this key.
            counts = {}
            for el in value:
                if isinstance(el, dict):
                    self.merge_value(el, counts)
                # Track non-object array elements as primitive at this position.
                elif is_primitive(el):
                    self.is_scalar = True
        elif is_primitive(value):
            self.is_scalar = True

    def observe_element(self, value):
        """An array element under a top-level array: treat like observe()."""
        self.observe(value)


def infer_schema(data, name="bench"):
    """Build a TEL schema (in tel-schema source form) describing `data`."""
    root = SchemaShape()
    if isinstance(data, dict):
        counts = {}
        root.merge_value(data, counts)
        for kw, n in counts.items():
            if n > 1:
                root.fields[kw].repeated = True
    elif isinstance(data, list):
        # Top-level array — every element becomes an `item`.
        item_shape = SchemaShape()
        item_shape.repeated = True
        for el in data:
            item_shape.observe(el)
        root.fields["item"] = item_shape

    # Recursively emit Record definitions for every object-shaped position.
    record_lines = []
    record_names = []
    seen_records = {}

    def emit_record(shape, suggested):
        """Walk an object shape and emit a record. Returns the record's name."""
        # De-dup records with identical shape signatures so a 100-element
        # array of users doesn't generate 100 copies.
        sig = shape_signature(shape)
        if sig in seen_records:
            return seen_records[sig]
        # Capitalised PascalCase name from the suggested kebab base.
        name = pascal_from_kebab(suggested)
        if name in record_names:
            name = name + str(len(record_names))
        record_names.append(name)
        seen_records[sig] = name

        members = []
        for kw, child in shape.fields.items():
            type_name = "String"
            if child.fields:
                child_name = emit_record(child, kw)
                type_name = child_name
            modifiers = []
            modifiers.append("optional")  # Always allow absence — pragmatic for benches
            if child.repeated or child.is_array:
                modifiers.append("repeatable")
            modifiers_str = " " + " ".join(modifiers) if modifiers else ""
            members.append(f"  field {kw} {type_name}{modifiers_str}")

        record_lines.append(f"record {name}")
        record_lines.extend(members if members else ["  # (no fields observed)"])
        record_lines.append("")
        return name

    def shape_signature(shape):
        if not shape.fields:
            return "scalar"
        return "{" + ",".join(
            f"{kw}:{shape_signature(child)}" + ("*" if child.repeated or child.is_array else "")
            for kw, child in sorted(shape.fields.items())
        ) + "}"

    # Emit the document body.
    document_lines = ["document"]
    for kw, child in root.fields.items():
        type_name = "String"
        if child.fields:
            child_name = emit_record(child, kw)
            type_name = child_name
        modifiers = ["optional"]
        if child.repeated or child.is_array:
            modifiers.append("repeatable")
        document_lines.append(f"  field {kw} {type_name} {' '.join(modifiers)}")

    schema = []
    schema.append(f"name {to_kebab(name)}")
    schema.append("")
    schema.extend(record_lines)
    schema.extend(document_lines)
    schema.append("")
    return "\n".join(schema)


def pascal_from_kebab(s):
    return "".join(part.capitalize() for part in s.split("-") if part) or "Anon"


# ── Main ──


def main():
    OUTDIR.mkdir(parents=True, exist_ok=True)
    print(f"Writing TEL + schema files to {OUTDIR}")

    for name, json_text in EXAMPLES:
        data = json.loads(json_text)
        tel_text = json_to_tel(data)
        schema_text = infer_schema(data, name)
        tel_path = OUTDIR / f"{name}.tel"
        schema_path = OUTDIR / f"{name}.schema.tel"
        tel_path.write_text(tel_text)
        schema_path.write_text(schema_text)
        print(f"  {name}: TEL {len(tel_text):>6}B, schema {len(schema_text):>5}B")


if __name__ == "__main__":
    main()
